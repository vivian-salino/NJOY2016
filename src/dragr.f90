module dragm
   ! provides subroutine dragr for NJOY2012
   use locale
   use xsmm
   implicit none
   private
   type(xsm_file),pointer :: draglib
   public dragr
contains

   subroutine dragr
   !-----------------------------------------------------------------
   !
   !     Produce a draglib interface file from Njoy intermediate
   !   cross-section library.
   !
   !     The draglib format provide an efficient way to store multi-
   !   group isotopic nuclear data to be used in a lattice code.
   !   For example the draglib can then be used by the Dragon code.
   !
   !     Working from thermr and groupr output tape,this module
   !   produce the standard interface file on the xsm file. The xsm
   !   data base is used to organize the data in a hierarchical
   !   format. Therefore, it will be easy to convert back and forth
   !   between the binary direct access format (efficient during a
   !   calculation) and the ascii or binary format (useful for
   !   backup and exchange purposes).
   !
   ! Authors: Hasan Saygin (original version -- 1992)
   !          Alain Hebert (reprogrammed in 2003)
   !          Alain Hebert (adapted to Njoy2012 in 2014)
   !
   ! Copyright:
   !  Copyright (C) 2003 Ecole Polytechnique de Montreal
   !  This library is free software; you can redistribute it and/or
   !  modify it under the terms of the GNU Lesser General Public
   !  License as published by the Free Software Foundation; either
   !  version 2.1 of the License, or (at your option) any later
   !  version.
   !
   !---input specifications (free format)----
   !
   ! card 1 file units
   !   nendf     input endf unit
   !   npendf    input pendf unit
   !   ngendf    input gendf unit
   !   nfp       input endf unit for fission yield data
   !   ndcy      input endf unit for radioactive decay data
   !   nimpo     input draglib unit
   !   nexpo     output draglib unit
   !   pfflag    fission product flag: off/on = 0/1. pfflag=1 to
   !             avoid storing scattering matrices. (default=0)
   ! card 2 hollerith identification for the library
   ! (nimpo=0 only)
   !   labell    72 character identification for the library
   ! card 3 material data (one card per material)
   !   matno     integer material identifier
   !             (endf/b mat number)
   !   hmat      hollerith material identifier (up to 8 characters
   !             each). By default, an ascii identifier is
   !             constructed.
   !   nbesp     zero (no energy-dependent fission spectra) or
   !             number of energy-dependent fission spectra)
   ! card 4 material readme comment (up to 72 characters)
   ! card 5 energy mesh for energy-dependent fission spectra
   ! present if and only if nbesp.ne.0
   !   e1, e2, e3, etc. nbesp+1 increasing values of energies (eV)
   ! card 6 energy limits for dilution-dependent xs data. This data
   !        is important to avoid self-shielding in very low and
   !        very high energy group xs and to reduce the size of the
   !        draglib. (one card per material)
   !   eres0     lower limit of resolved resonance xs data (ev)
   !   eres1     upper limit of unresolved resonance xs data (ev)
   ! card 7 autolib energy limits. This data is used with advanced
   !        self-shielding models such as the Sanchez-Coste and
   !        Ribon extended models. It is highly recommended to use
   !        the same data for all resonant isotopes. (one card per
   !        material)
   ! present if and only if npendf.ne.0
   !   eaut0     lower limit of autolib resonance xs data (ev)
   !   eaut1     upper limit of autolib resonance xs data (ev)
   !   deli      elementary lethargy width (ev)
   !
   !            repeat cards 3 to 7 for all materials desired
   !            matno=0/ terminates dragr run.
   !
   ! card 8 burnup chain data (one or two cards per isotope)
   ! present if and only if nfp.ne.0 and ndcy.ne.0
   !  hich      hollerith isotope identifier (same as hmat below)
   !            hich must be constructed in a way compatible with
   !            subroutine dranam (ex: Am242m). If an isotope is
   !            missing in this burnup chain, an error message of
   !            the type 'isotope xxx should not be lumped' may be
   !            issued by subroutine dralum.
   !  hrch(1)   hollerith neutron-induced reaction identifier for
   !            first daughter
   !  en(1)     Q value in MeV for first daughter
   !  br(1)     branching ratio for an isomeric daughter (=0.0 if
   !            there is no isomeric daughter)
   !  hrch(2)   hollerith neutron-induced reaction identifier for
   !            second daughter
   !  en(2)     Q value in MeV for second daughter
   !  ...
   !
   !            repeat card 7 for all isotopes present in the
   !            burnup chain. hich='end'/ terminates the chain.
   !
   ! card 9 concatenate information
   !   hmat      hollerith material identifier (up to 8 characters
   !             each). By default, an ascii identifier is
   !             constructed. It corresponds to the unique isotope
   !             draglib previously computed that need to be concatenate.
   !   inew      flag to specify if the isotope is the first one. or
   !             if a concatenated drag lib exist
   !-----------------------------------------------------------------
   use mainio ! provides nsysi,contio,nsyso,nsyse
   use endf   ! provides endf routines and variables
   use util   ! provides timer,openz,repoz,error
   real(kr)::time,eres0,eres1,eaut0,eaut1,deli
   integer :: maxa,maxgr,maxesp
   parameter (maxa=2000,maxgr=400,maxesp=4)
   character text72*72,text12*12,text8*8
   integer ngen,matno,ng
   integer iesp(maxesp+1)
   character(len=4) labell(18)
   integer i,ig,igaut0,igaut1,igres0,igres1,iig,ipflag,iza,nb,nbesp,ndcy, &
   & nendf,nexpo,nfp,nimpo,npen,nw,ilong,ilong1,ityxsm,inew
   logical lsame
   real(kr) enext
   real(kr) ener(maxgr+1),eesp(maxesp+1)
   real eespi(maxesp+1)
   real,allocatable,dimension(:) :: gar1,gar2
   real(kr),allocatable,dimension(:) :: scr
   !
   call timer(time)
   write(nsyso,'(/'' dragr...produce a draglib format output file'',24x,f8.1, &
   & ''s'')') time
   write(nsyse,'(/'' dragr...'',60x,f8.1,''s'')') time
   !
   !**allocate xsm file containing the draglib
   allocate(draglib)
   !
   !**read users input/output unit numbers.
   nendf=0
   npen=0
   ngen=0
   nfp=0
   ndcy=0
   nimpo=0
   nexpo=0
   ipflag=0
   read(nsysi,*) nendf,npen,ngen,nfp,ndcy,nimpo,nexpo,ipflag
   allocate(scr(maxa))
   !
   !**open the input/output files
   write(nsyso,'(/ &
   &  '' input endf unit ............................. '',i10/ &
   &  '' input pendf unit ............................ '',i10/ &
   &  '' input gendf unit ............................ '',i10/ &
   &  '' input endf unit for fission yield data ...... '',i10/ &
   &  '' input endf unit for radioactive decay data .. '',i10/ &
   &  '' input draglib unit .......................... '',i10/ &
   &  '' output draglib unit ......................... '',i10/ &
   &  '' fission product flag ........................ '',i10)') &
   &  nendf,npen,ngen,nfp,ndcy,nimpo,nexpo,ipflag
   if(nendf.ne.0) call openz(nendf,0)
   if(npen.ne.0) call openz(npen,0)
   if(ngen.ne.0) call openz(ngen,0)
   if((nfp.ne.0).and.(ndcy.ne.0)) then
     call openz(nfp,0)
     call openz(ndcy,0)
   endif
   if(nimpo.eq.0) then
     ! ***create a new xsm file and write the signature
     call xsmop(draglib,'drag',0,1)
     text12='L_DRAGLIB'
     read(text12,'(3a4)') (labell(i),i=1,3)
     labell(1)=text12(1:4)
     labell(2)=text12(5:8)
     labell(3)=text12(9:12)
     call xsmput(draglib,'SIGNATURE',labell(1:3))
     text12='RELEASE_2003'
     labell(1)=text12(1:4)
     labell(2)=text12(5:8)
     labell(3)=text12(9:12)
     call xsmput(draglib,'VERSION',labell(1:3))
   else
     ! ***import the existing xsm file
     call openz(nimpo,0)
     call xsmop(draglib,'drag',0,1)
     if(nimpo.gt.0) then
       call xsmexp(draglib,nimpo,2,2,1)
     else
       call xsmexp(draglib,-nimpo,1,2,1)
     endif
     call closz(nimpo)
   endif
   if((nendf.eq.0).and.(npen.eq.0).and.(ngen.eq.0).and. &
     &(nfp.eq.0).and.(ndcy.eq.0)) go to 40
   call openz(nexpo,1)
   !
   !**read identification for the library
   if(nimpo.eq.0) then
     text72=' '
     read(nsysi,*) text72
     if(text72.ne.' ') then
       do i=1,18
         labell(i)=text72((i-1)*4+1:i*4)
       enddo
       write(nsyso,'('' identification for the library ....... ''/1x,a)') text72
       call xsmput(draglib,'README',labell(1:18))
     endif
   endif
   !
   !**material loop
   if(ngen.eq.0) go to 20
   do
     matno=0
     text8=' '
     read(nsysi,*) matno,text8,nbesp
     if(nbesp.gt.maxesp) call error('dragr','maxesp overflow',' ')
     if(matno.eq.0) then
       go to 10
     else if(text8.eq.' ') then
       ! automatic composition of the ascii material name.
       if(nendf.eq.0) call error('dragr','endf unit not provided',' ')
       call findf(matno,1,451,nendf)
       call contio(nendf,0,0,scr,nb,nw)
       iza=nint(c1h+0.1)
       call contio(nendf,0,0,scr,nb,nw)
       call dranam(10*iza+l2h,text8)
     endif
     write(nsyso,'('' --> processing material '',a, &
     &  ''  (endf identification ='',i8,'')'')') text8,matno
     !
     if(nendf.ne.0) then
       ! ***construct the "isotopic specification line" for the burnup data.
       call dradat(nendf,matno,text8)
     endif
     !
     ! ***energy group information for the library
     call drahd(ngen,matno,ng,maxgr,ener)
     text72=' '
     read(nsysi,*) text72
     do i=1,18
       labell(i)=text72((i-1)*4+1:i*4)
     enddo
     write(nsyso,'('' identification for the material ....... '',a)') text72
     !
     ! ***recover energy limits for energy-dependent fission spectra
     if(nbesp.ne.0) then
       read(nsysi,*) (eesp(i),i=1,nbesp+1)
       iig=nbesp+1
       do ig=1,ng+1
         if(iig.eq.0) call error('dragr','bad limits for energy-dep' &
         &  //'endent fission spectra(1)',' ')
         if(eesp(iig).ge.0.999d0*ener(ig)) then
           iesp(nbesp+2-iig)=ig-1
           iig=iig-1
         endif
       enddo
       if(iig.ne.0) call error('dragr','bad limits for energy-depen' &
       & //'dent fission spectra(2)',' ')
       do iig=1,nbesp+1
         eespi(iig)=eesp(nbesp+2-iig)
       enddo
       call xsmput(draglib,'CHI-ENERGY',eespi(1:nbesp+1))
       call xsmput(draglib,'CHI-LIMITS',iesp(1:nbesp+1))
     else
       nbesp=1
       iesp(1)=0
       iesp(2)=ng
     endif
     !
     ! ***step on material
     call xsmsix(draglib,text8,1)
     call xsmput(draglib,'README',labell(1:18))
     !
     ! ***recover energy limits for dilution-dependent xs data
     read(nsysi,*) eres0,eres1
     eres0=max(0.1d0,eres0)
     if(eres1.eq.0.0d0) eres1=ener(1)
     igres0=1
     igres1=ng
     do ig=1,ng
       if(eres1.le.1.001d0*ener(ig+1)) igres0=ig+1
     enddo
     do ig=ng+1,2,-1
       if(eres0.ge.0.999d0*ener(ig)) igres1=ig-1
     enddo
     write(nsyso,'(/ &
     &  '' first group of resolved resonance xs data.... '',i10/ &
     &  '' last group of unresolved resonance xs data... '',i10)') &
     &  igres0,igres1
     !
     ! ***generate draglib file for this material
     call dramat(ngen,matno,ng,igres0,igres1,ipflag,nbesp,iesp)
     !
     ! ***put autolib data in draglib file for this material
     if(npen.ne.0) then
       read(nsysi,*) eaut0,eaut1,deli
       igaut0=1
       igaut1=ng
       do ig=1,ng
         if(eaut1.le.1.001d0*ener(ig+1)) igaut0=ig+1
       enddo
       do ig=ng+1,2,-1
         if(eaut0.ge.0.999d0*ener(ig)) igaut1=ig-1
       enddo
       write(nsyso,'(/ &
       &    '' first group of autolib xs data.............. '',i10/ &
       &    '' last group of autolib xs data............... '',i10/ &
       &    '' elementary lethargy width................... '',1p,d10.4)') &
       &    igaut0,igaut1,deli
       call drauto(npen,matno,ng,ener,eaut0,eaut1,igaut0,igaut1,deli)
     endif
     call xsmsix(draglib,' ',2)
   enddo
   !
   ! **finished
   10 call closz(ngen)
   call closz(npen)
   call closz(nendf)
   !
   ! **compute depletion-related data
   20 if((nfp.ne.0).and.(ndcy.ne.0)) then
   call xsmsix(draglib,'DEPL-CHAIN',1)
   call dradep(nfp,ndcy)
   call xsmsix(draglib,' ',2)
   call closz(ndcy)
   call closz(nfp)
   endif
   !
   ! **concatenate two Draglib
   40 if((nendf.eq.0).and.(npen.eq.0).and.(ngen.eq.0).and. &
     &(nfp.eq.0).and.(ndcy.eq.0)) then
     read(nsysi,*) text8,inew
     call openz(nexpo,inew)
     ! ***import the existing xsm file to concatenate
     if (inew.eq.0) then
       call xsmlen(draglib,'ENERGY',ilong,ityxsm)
       allocate(gar1(ilong))
       call xsmget(draglib,'ENERGY',gar1)
     endif
     call xsmcl(draglib,2)
     deallocate(draglib)
     allocate(draglib)
     call xsmop(draglib,'drag',0,1)
     if(nexpo.gt.0) then
       call xsmexp(draglib,nexpo,2,2,1)
     else
       call xsmexp(draglib,-nexpo,1,2,1)
     endif
     call closz(nexpo)
     call openz(nexpo,1)
     if (inew.eq.0) then
       call xsmlen(draglib,'ENERGY',ilong1,ityxsm)
       allocate(gar2(ilong1))
       call xsmget(draglib,'ENERGY',gar2)
       if (ilong.ne.ilong1)  call error('dragr','number of energy' &
       & //' group do not match for concatenate',' ')
       lsame=.true.
       do ig=1,ilong-1
         lsame=lsame.and.(abs(gar1(ig)-gar2(ig)).le.1.0e-3*gar1(ig))
       enddo
       if(.not.lsame) then
         do ig=1,ilong-1
           write(nsyso,*) ig,gar1(ig),gar2(ig), &
           & abs(gar1(ig)-gar2(ig))/gar1(ig)*100, '%'
         enddo
         call error('dragr','inconsistent energy mesh',' ')
       endif
       deallocate(gar1,gar2)
     endif
     call openz(nimpo,0)
     if(nimpo.gt.0) then
       call xsmexp(draglib,nimpo,2,2,1)
     else
       call xsmexp(draglib,-nimpo,1,2,1)
     endif
     call closz(nimpo)
   endif
   call xsmlib(draglib)
   call xsmcl(draglib,1)
   !
   ! **export the xsm file
   call xsmop(draglib,'drag',2,1)
   if(nexpo.gt.0) then
     call xsmexp(draglib,nexpo,2,1,1)
   else
     call xsmexp(draglib,-nexpo,1,1,1)
   endif
   call xsmcl(draglib,2)
   !
   !**deallocate xsm file containing the draglib
   deallocate(draglib)
   !
   call closz(nexpo)
   call timer(time)
   write(nsyso,'(69x,f8.1,''s''/1x,7(''**********''),''*******'')') time
   deallocate(scr)
   return
   end subroutine dragr
   !
   subroutine dradat(nendf,matno,text8)
   !-----------------------------------------------------------------
   ! Construct the "isotopic specification line" for the burnup data.
   !-----------------------------------------------------------------
   use mainio ! provides nsysi,contio,nsyso,nsyse
   use endf   ! provides endf routines and variables
   use util   ! provides timer,openz,repoz,error
   integer :: maxa,maxedi,lz
   parameter(maxa=2000,maxedi=11,lz=6)
   integer nendf,matno
   character hline*80,text8*8
   integer ied,iof,iof2,iza,nb,nw
   logical lfind
   integer,save,dimension(maxedi) :: malist= &
   & (/ 16,17,18,28,37,102,103,104,105,107,108 /)
   character(len=8),save,dimension(maxedi) :: namedi= &
   & (/'n2n     ','n3n     ','nftot   ','nnp     ','n4n     ','ng      ', &
   &   'np      ','nd      ','nt      ','na      ','n2a     ' /)
   real(kr),allocatable,dimension(:) :: scr
   !
   allocate(scr(maxa))
   write(nsyso,'(/'' isotopic specification line for material'',i8, &
   & '':''/1x,80(''-''))') matno
   hline=' '
   iof=1
   call findf(matno,1,451,nendf)
   call contio(nendf,0,0,scr,nb,nw)
   iza=nint(c1h+0.1)
   call contio(nendf,0,0,scr,nb,nw)
   write(hline(iof:),'(a8,1x)') text8
   iof=iof+9
   do ied=1,maxedi
     call repoz(nendf)
     lfind=.false.
     do while (.not.lfind)
       if(nendf.lt.0) then
         read(-nendf,end=50) math,mfh,mth,nb,nw
       else if(nendf.gt.0) then
         read(nendf,'(66x,i4,i2,i3,i5)',end=50) math,mfh,mth,nsp
       endif
       lfind=(math.eq.matno).and.(mfh.eq.3).and.(mth.eq.malist(ied))
     enddo
     if(.not.lfind) go to 50
     if(iof+26.gt.80) then
       write(nsyso,'(1x,a80)') hline
       hline=' '
       iof=10
     endif
     iof2=index(namedi(ied),' ')
     write(hline(iof:),'(a,1x)') namedi(ied)(:iof2-1)
     iof=iof+iof2
     if(mth.eq.18) then
       ! ***fission -- recover info in mf=1, mt=458
       call repoz(nendf)
       lfind=.false.
       do while (.not.lfind)
         if(nendf.lt.0) then
           read(-nendf,end=30) math,mfh,mth,nb,nw
         else if(nendf.gt.0) then
           read(nendf,'(66x,i4,i2,i3,i5)',end=30) math,mfh,mth,nsp
         endif
         lfind=(math.eq.matno).and.(mfh.eq.1).and.(mth.eq.458)
       enddo
       if(lfind) go to 40
       30 write(hline(iof:),'(''???????????'',f6.3,1x)') 0.0
       iof=iof+18
       go to 50
       40 call listio(nendf,0,0,scr,nb,nw)
       ! ***remove energy from delayed neutrons, betas and gammas as
       ! they are taken into account separately from file 8.
       c1h=scr(lz+15)-scr(lz+5)-scr(lz+9)-scr(lz+11)
     else
       ! ***else -- recover the Q of the reaction (may be negative)
       call contio(nendf,0,0,scr,nb,nw)
     endif
     write(hline(iof:),'(1p,e11.4,'' 0.000 '')') c1h*1.0e-6
     iof=iof+18
     50 continue
   enddo
   if(iof+1.gt.80) then
     write(nsyso,'(1x,a80)') hline
     hline=' '
     iof=10
   endif
   write(hline(iof:),'(''/'')')
   write(nsyso,'(1x,a80)') hline
   write(nsyso,'(1x,80(''-''))')
   deallocate(scr)
   return
   end subroutine dradat
   !
   subroutine drahd(ngen,matno,ng,maxgr,ener)
   !-----------------------------------------------------------------
   ! write energy group data to the xsm file
   !-----------------------------------------------------------------
   use endf   ! provides endf routines and variables
   use util   ! provides error
   integer :: maxa
   parameter(maxa=2000)
   integer ngen,matno,ng,maxgr
   real(kr) ener(maxgr+1)
   integer ig,ilong,ityxsm,loc,nb,nw,nz
   logical lsame
   real,allocatable,dimension(:) :: gar
   real(kr),allocatable,dimension(:) :: scr
   !
   allocate(scr(maxa))
   call findf(matno,1,451,ngen)
   call contio(ngen,0,0,scr,nb,nw)
   nz=nint(scr(4))
   call listio(ngen,0,0,scr(1),nb,nw)
   loc=1+nw
   do while (nb.ne.0)
     if(loc+302.gt.maxa) call error('drahd','endf input size exceeded',' ')
     call moreio(ngen,0,0,scr(loc),nb,nw)
     loc=loc+nw
   enddo
   ng=nint(scr(3))
   if(ng.gt.maxgr) call error('drahd','maxgr overflow',' ')
   call xsmlen(draglib,'ENERGY',ilong,ityxsm)
   allocate(gar(ng+1))
   if(ilong.eq.0) then
     do ig=1,ng+1
       ener(ng-ig+2)=scr(7+nz+ig)
     enddo
     gar(1:ng+1)=ener(1:ng+1)
     call xsmput(draglib,'ENERGY',gar(1:ng+1))
   else
     if(ilong.ne.ng+1) call error('drahd','invalid number of groups',' ')
     call xsmget(draglib,'ENERGY',gar)
     ener(1:ng+1)=gar(1:ng+1)
     lsame=.true.
     do ig=1,ng
       lsame=lsame.and.(abs(ener(ng-ig+2)-scr(7+nz+ig)).le.1.0e-3*ener(ng-ig+2))
     enddo
     if(.not.lsame) call error('drahd','inconsistent energy mesh',' ')
   endif
   deallocate(gar)
   deallocate(scr)
   return
   end subroutine drahd
   !
   subroutine dramat(ngen,matno,ng,igres0,igres1,ipflag,nbesp,iesp)
   !-----------------------------------------------------------------
   !   write draglib data to the xsm (direct access) file for endf
   !   material matno
   !   the dilution-dependent xs deltas are recovered between groups
   !   igres0 and igres1.
   !-----------------------------------------------------------------
   use endf   ! provides endf routines and variables
   use util   ! provides error
   integer :: maxa,maxgr,maxnl,maxnz,maxtmp,maxedi
   parameter (maxa=2000,maxgr=400,maxnl=8,maxnz=30,maxtmp=10,maxedi=14)
   integer ngen,matno,ng,igres0,igres1,ipflag,nbesp,iesp(nbesp+1)
   real(kr) rv(maxgr,maxnl,maxnz),flux(maxgr,maxnl,maxnz),aa(6), &
   & deltau(maxgr),rv2(maxgr,maxnl,1),rm2(maxgr,maxgr,maxnl,1)
   logical lfind,lsame,lover,exist,exist2,exist3
   character cd*4,hsmg*131
   real awr(1),dilut(maxnz),olddil(maxnz),oldtmp(maxtmp),vector(maxgr)
   integer igfirs(maxnl),iglast(maxnl)
   integer ied,ig,ig1,ig2,igmax,il,ilong,ityxsm,imt,isbmat,itm,itm0,iz,iz0, &
   & loc,nb,ng0,nl,nl2,nlgar,ntmp,nw,nz,nz0,nz0bis,nzgar
   real(kr) temps(maxtmp),ytemp
   integer, save, dimension(maxedi) :: malist= &
   & (/ 1,2,4,16,17,18,28,37,102,103,104,105,107,108 /)
   character(len=8), save, dimension(maxedi) :: namedi= &
   & (/ 'NTOT0   ','NELAS   ','NINEL   ','N2N     ','N3N     ','NFTOT   ', &
   &    'NNP     ','N4N     ','NG      ','NP      ','ND      ','NT      ', &
   &    'NA      ','N2A     ' /)
   real(kr),allocatable,dimension(:) :: scr
   !
   ! recover the new dilution values.
   allocate(scr(maxa))
   call findf(matno,1,451,ngen)
   call contio(ngen,0,0,scr,nb,nw)
   awr(1)=scr(2)
   call xsmput(draglib,'AWR',awr)
   nz=nint(scr(4))
   if(nz.gt.maxnz) call error('dramat','maxnz overflow',' ')
   call listio(ngen,0,0,scr(1),nb,nw)
   loc=1+nw
   do while(nb.ne.0)
     if(loc+302.gt.maxa) call error('dramat','endf input size exceeded',' ')
     call moreio(ngen,0,0,scr(loc),nb,nw)
     loc=loc+nw
   enddo
   do iz=1,nz
     dilut(nz-iz+1)=scr(7+iz)
   enddo
   if(dilut(nz).lt.1.0e10) then
     call error('dramat','missing infinite dilution value',' ')
   endif
   do ig=1,ng
     deltau(ig)=log(scr(7+nz+(ng-ig+2))/scr(7+nz+(ng-ig+1)))
   enddo
   !
   ! recover the temperature values.
   ntmp=0
   10 lfind=.false.
   do while(.not.lfind)
     if(ngen.lt.0) then
       read(-ngen,end=20) math,mfh,mth,nb,nw
     else if(ngen.gt.0) then
       read(ngen,'(6e11.0,i4,i2,i3,i5)',end=20) aa,math,mfh,mth,nsp
     endif
     lfind=(math.eq.matno).and.(mfh.eq.3).and.(mth.eq.1)
   enddo
   if(.not.lfind) go to 20
   call listio(ngen,0,0,scr(1),nb,nw)
   ntmp=ntmp+1
   if(ntmp.gt.maxtmp) call error('dramat','maxtmp overflow',' ')
   temps(ntmp)=scr(1)
   call tomend(ngen,0,0,scr)
   go to 10
   20 if(ntmp.eq.0) call error('dramat','no temperatures on gendf',' ')
   !
   ! set the 'TEMPERATURE' record on xsm.
   call xsmlen(draglib,'TEMPERATURE',ilong,ityxsm)
   if(ilong.gt.0) then
     if(ilong.gt.maxtmp) call error('dramat','maxtmp overflow',' ')
     call xsmget(draglib,'TEMPERATURE',oldtmp)
     lover=.true.
     do itm=1,ntmp
       lover=lover.and.(temps(itm).gt.oldtmp(ilong))
     enddo
     if(lover) then
       if(ntmp+ilong.gt.maxtmp) call error('dramat','maxtmp overflow',' ')
       do itm=1,ntmp
          ilong=ilong+1
          oldtmp(ilong)=temps(itm)
       enddo
     endif
   else
     ilong=ntmp
     do itm=1,ntmp
        oldtmp(itm)=temps(itm)
     enddo
   endif
   call xsmput(draglib,'TEMPERATURE',oldtmp(1:ilong))
   !
   ! loop over the temperatures.
   do itm=1,ntmp
     do itm0=1,ilong
       if(abs(temps(itm)-oldtmp(itm0)).le.1.0e-5*temps(itm)) then
         isbmat=itm0
         go to 30
       endif
     enddo
     call error('dramat','missing temperature',' ')
     30   ytemp=temps(itm)
     write (cd,'(i4.4)') isbmat
     call xsmsix(draglib,'SUBTMP'//cd,1)
     !
     ! ***store the new dilutions
     call xsmlen(draglib,'DILUTION',nz0,ityxsm)
     if(nz0.gt.0) then
       if(nz0.gt.maxnz) call error('dramat','maxnz overflow',' ')
       call xsmget(draglib,'DILUTION',olddil)
       lover=.true.
       do iz=1,nz-1
         lover=lover.and.(dilut(iz).gt.olddil(nz0))
       enddo
       if(lover) then
         if(nz+nz0-1.gt.maxnz) call error('dramat','maxnz overflow',' ')
         do iz=1,nz-1
           olddil(nz0+iz)=dilut(iz)
         enddo
         call xsmput(draglib,'DILUTION',olddil(1:nz+nz0-1))
       else
         do iz0=1,nz0
           if(abs(dilut(1)-olddil(iz0)).le.1.0e-5*dilut(1)) then
             nz0bis=iz0-1
             go to 34
           endif
         enddo
         call error('dramat','inconsistent dilutions(1).',' ')
         34 if(nz0bis+nz-1.gt.nz0) then
           call error('dramat','inconsistent dilutions(2).',' ')
         endif
         do iz=1,nz-1
           if(abs(dilut(iz)-olddil(nz0bis+iz)).gt.1.0e-5*dilut(iz)) &
           & call error('dramat','inconsistent dilutions(3).',' ')
         enddo
         nz0=nz0bis
       endif
     else if(nz.gt.1) then
       call xsmput(draglib,'DILUTION',dilut(1:nz-1))
     endif
     !
     !   ***recover thermal correction x-sections and compute the
     !      correction term.
     !   NOTE: Apparently, there is a bug in THERMR so that the matrix
     !         221 to 250 reactions does not sum to the corresponding
     !         vector reactions. We will therefore avoid using the
     !         vector reactions for thermal scattering.
     !    call fvect(maxgr,maxnl,1,ngen,matno,imt,ytemp,ng,nl2,nzgar,rv2,
     ! &  flux,exist2)
     do il=1,maxnl
       do ig1=1,maxgr
         rv2(ig1,il,1)=0.0
       enddo
     enddo
     exist3=.false.
     do imt=250,221,-1
       call fmatr(maxgr,maxnl,1,ngen,matno,imt,ytemp,ng0,nl2,nzgar,rm2,flux, &
       & exist2)
       if(exist2) then
         exist3=.true.
         nlgar=nl2
         if(ng0.ne.ng) call error('dramat','inconsistent ng(1).',' ')
         do il=1,nl2
           do ig1=1,ng
             do ig2=1,ng
               rv2(ig1,il,1)=rv2(ig1,il,1)+rm2(ig1,ig2,il,1)
             enddo
           enddo
         enddo
       endif
     enddo
     35 if(exist3) then
       call fvect(maxgr,maxnl,maxnz,ngen,matno,2,ytemp,ng0,nl,nzgar, &
       & rv,flux,exist)
       if(nl.gt.nlgar) call error('dramat','inconsistent number o'// &
       & 'f Legendre orders(1)',' ')
       do il=1,nl
         do ig=1,ng
           if(rv2(ig,il,1).ne.0.0) then
             rv2(ig,il,1)=rv2(ig,il,1)-rv(ig,il,1)
           endif
         enddo
       enddo
     endif
     !
     ! ***loop over the vector cross section sets.
     do ied=1,maxedi
     !
     !  ***process infinite dilution x-sections.
       call fvect(maxgr,maxnl,maxnz,ngen,matno,malist(ied),ytemp,ng0, &
       & nl,nzgar,rv,flux,exist)
       if(.not.exist) go to 40
       if(ng0.ne.ng) call error('dramat','inconsistent ng(2).',' ')
       !
       if((malist(ied).le.2).and.exist3) then
         ! ***perform thermal correction on 'NTOT0' and 'NELAS' x-sections.
         do iz=1,nzgar
           do ig=1,ng
             rv(ig,1,iz)=rv(ig,1,iz)+rv2(ig,1,1)
           enddo
         enddo
       endif
       !
       do ig=ng,1,-1
         igmax=ig
         if(rv(ig,1,1).ne.0.0) go to 36
       enddo
       cycle
       36 do ig=1,igmax
         vector(ig)=rv(ig,1,1)
       enddo
       call xsmput(draglib,namedi(ied),vector(1:igmax))
       if(nzgar.eq.1) go to 40
       !
       ! ***find the last self-shielded group.
       do il=1,maxnl
         igfirs(il)=igres0
         iglast(il)=igres1+1
       enddo
       do iz=1,nz-1
         write (cd,'(i4.4)') nz0+iz
         call xsmsix(draglib,'SUBMAT'//cd,1)
         !
         ! ***process finite dilution x-sections
         do ig=1,iglast(1)-1
           vector(ig)=0.0
         enddo
         do ig=igfirs(1),iglast(1)-1
           if(flux(ig,1,1).eq.0.0) cycle
           if(rv(ig,1,1).eq.0.0) cycle
           flux(ig,1,nz-iz+1)=flux(ig,1,nz-iz+1)/flux(ig,1,1)
           if(flux(ig,1,nz-iz+1).gt.3.0) then
             write(hsmg,'(19hinconsistent flux (,1p,e10.3,5h) in , &
             & 5hgroup,i4,13h at dilution=,e10.2,5h barn)') &
             & flux(ig,1,nz-iz+1),ig,dilut(iz)
             call error('dramat',hsmg,' ')
           endif
           vector(ig)=rv(ig,1,nz-iz+1)*flux(ig,1,nz-iz+1)-rv(ig,1,1)
         enddo
         call xsmput(draglib,namedi(ied),vector(1:iglast(1)-1))
         !
         if(ied.eq.1) then
           ! ***process 'NWT0' finite dilution fluxes
           do ig=igfirs(1),iglast(1)-1
             vector(ig)=flux(ig,1,nz-iz+1)-1.0d0
           enddo
           call xsmput(draglib,'NWT0',vector(1:iglast(1)-1))
         endif
         !
         call xsmsix(draglib,' ',2)
       enddo
       40 continue
     enddo
     !
     ! ***process fission x-sections.
     call drafis(ngen,matno,ytemp,nz0,deltau,igfirs(1),iglast(1),nbesp,iesp)
     !
     ! ***process scattering x-sections.
     call drasc(ngen,matno,ytemp,nz0,deltau,igfirs,iglast,ipflag)
     !
     call xsmsix(draglib,' ',2)
   enddo
   deallocate(scr)
   return
   end subroutine dramat
   !
   subroutine drauto(npen,matno,ng,ener,eaut0,eaut1,igaut0,igaut1,deli)
   !-----------------------------------------------------------------
   !  put autolib data in draglib file for material matno
   !-----------------------------------------------------------------
   use mainio ! provides nsysi,contio,nsyso,nsyse
   use endf   ! provides endf routines and variables
   use util   ! provides timer,openz,repoz,error
   integer :: ndoubl,maxa,maxgr,maxtmp,igaut0,igaut1
   parameter (ndoubl=2)
   parameter(maxa=2000,maxgr=400,maxtmp=10)
   integer :: npen,matno,ng,igar,i,ibin,ibin0,idis,ig,ig1,ig2,ilfiss,isbmat, &
   & itm,ityxsm,nb,nbfine,nbin,ntmp,nw
   real(kr) ener(ng+1),eaut0,eaut1,deli
   real oldtmp(maxtmp),gar1t(maxgr),gar1s(maxgr),gar1f(maxgr),gar2(maxgr**2)
   integer nfs(maxgr),ijj(maxgr),njj(maxgr)
   real(kr) deltau,gar,em,ep,emlog,aa,enext,eplog,err,err1,err2,err3,errmax, &
   & fact1,fact2,fact3,sum1,sum2,sum3,sum4,temps,tm,tp,uuu,zbin
   real(kr), allocatable, dimension(:) :: bener,bsig1,bsig2,bsig3,scr1,scr
   real, allocatable, dimension(:) :: bgar
   logical lfind,lfiss
   character cd*4
   !
   allocate(scr(maxa))
   !**find the autolib energy limits.
   nbfine=0
   errmax=0.0
   do ig=igaut0,igaut1
     zbin=log(ener(ig)/ener(ig+1))
     nbin=int(zbin/deli+0.01)
     err1=abs(real(nbin)*deli-zbin)/abs(zbin)
     err2=abs(real(nbin)*deli+deli/3.0d0-zbin)/abs(zbin)
     err3=abs(real(nbin)*deli+2.0d0*deli/3.0d0-zbin)/abs(zbin)
     err=min(err1,err2,err3)
     if(err.ne.err1) nbin=nbin+1
     nbfine=nbfine+nbin
     errmax=max(errmax,err)
   enddo
   write(nsyso,'(/ &
   & '' number of bin energy groups in autolib....... '',i10/ &
   & '' percent accuracy of the bin mesh............. '',1p,d9.2, &
   & ''%'')') nbfine,errmax*100.0
   allocate(bener(nbfine+1))
   bener(1)=ener(igaut0)
   do ig=1,ng
     nfs(ig)=0
   enddo
   uuu=log(ener(1)/ener(igaut0))
   ibin=1
   do ig=igaut0,igaut1
     zbin=log(ener(ig)/ener(ig+1))
     nbin=int(zbin/deli+0.01)
     do i=1,nbin
       ibin=ibin+1
       uuu=uuu+deli
       bener(ibin)=ener(1)*exp(-uuu)
     enddo
     err1=abs(real(nbin)*deli-zbin)/abs(zbin)
     err2=abs(real(nbin)*deli+deli/3.0d0-zbin)/abs(zbin)
     err3=abs(real(nbin)*deli+2.0d0*deli/3.0d0-zbin)/abs(zbin)
     err=min(err1,err2,err3)
     if(err.eq.err2) then
       nbin=nbin+1
       ibin=ibin+1
       uuu=uuu+deli/3.0d0
       bener(ibin)=ener(1)*exp(-uuu)
     else if(err.eq.err3) then
       nbin=nbin+1
       ibin=ibin+1
       uuu=uuu+2.0d0*deli/3.0d0
       bener(ibin)=ener(1)*exp(-uuu)
     endif
     if(ibin.gt.nbfine+1) call error('drauto','bin overflow',' ')
     nfs(ig)=nbin
   enddo
   allocate(bsig1(nbfine),bsig2(nbfine),bsig3(nbfine),bgar(nbfine+1))
   do ibin=1,nbfine+1
     bgar(ibin)=real(bener(ibin))
   enddo
   call xsmput(draglib,'BIN-NFS',nfs(1:ng))
   call xsmput(draglib,'BIN-ENERGY',bgar(1:nbfine+1))
   !
   !**recover the temperature values.
   call repoz(npen)
   10 lfind=.false.
   do while(.not.lfind)
     if(npen.lt.0) then
       read(-npen,end=30) math,mfh,mth,nb,nw,aa
     else if(npen.gt.0) then
       read(npen,'(6e11.0,i4,i2,i3,i5)',end=30) aa,math,mfh,mth,nsp
     endif
     lfind=(math.eq.matno).and.(mfh.eq.1).and.(mth.eq.451)
   enddo
   if(.not.lfind) go to 30
   call contio(npen,0,0,scr,nb,nw)
   call contio(npen,0,0,scr,nb,nw)
   call contio(npen,0,0,scr,nb,nw)
   temps=c1h
   call xsmlen(draglib,'TEMPERATURE',ntmp,ityxsm)
   if(ntmp.eq.0) call error('drauto','missing temperatures on xsm',' ')
   if(ntmp.gt.maxtmp) call error('drauto','oldtmp overflow',' ')
   call xsmget(draglib,'TEMPERATURE',oldtmp)
   isbmat=0
   do itm=1,ntmp
     if(abs(temps-oldtmp(itm)).le.1.0e-3*temps) then
        isbmat=itm
        go to 20
     endif
   enddo
   20 if(isbmat.eq.0) call error('drauto','missing temperature',' ')
   write (cd,'(i4.4)') isbmat
   call xsmsix(draglib,'SUBTMP'//cd,1)
   nw=npage+50
   allocate(scr1(nw))
   !
   !**recover the pointwise total cross sections.
   call findf(matno,3,1,npen)
   call contio(npen,0,0,scr1,nb,nw)
   ep=0.0d0
   call gety1(ep,enext,idis,tp,npen,scr1)
   em=eaut0
   call gety1(em,enext,idis,tm,npen,scr1)
    ibin=nbfine
   gar=0.0d0
   emlog=log(eaut1/em)
   do while(ep*(1.0d0+1.0d-6).lt.eaut1)
     if(ibin.le.0) call error('drauto','invalid index',' ')
     ep=min(enext,bener(ibin))
     eplog=log(eaut1/ep)
     call gety1(ep,enext,idis,tp,npen,scr1)
     gar=gar+0.5d0*(tm+tp)*(emlog-eplog)
     if(ep.eq.bener(ibin)) then
       deltau=log(bener(ibin)/bener(ibin+1))
       bsig1(ibin)=gar/deltau
       ibin=ibin-1
       gar=0.0d0
     endif
     em=ep
     tm=tp
     emlog=eplog
   enddo
   !
   !**recover the pointwise scattering cross sections.
   call findf(matno,3,2,npen)
   call contio(npen,0,0,scr1,nb,nw)
   ep=0.0d0
   call gety1(ep,enext,idis,tp,npen,scr1)
   em=eaut0
   call gety1(em,enext,idis,tm,npen,scr1)
   ibin=nbfine
   gar=0.0d0
   emlog=log(eaut1/em)
   do while(ep*(1.0d0+1.0d-6).lt.eaut1)
     if(ibin.le.0) call error('drauto','invalid index',' ')
     ep=min(enext,bener(ibin))
     eplog=log(eaut1/ep)
     call gety1(ep,enext,idis,tp,npen,scr1)
     gar=gar+0.5d0*(tm+tp)*(emlog-eplog)
     if(ep.eq.bener(ibin)) then
       deltau=log(bener(ibin)/bener(ibin+1))
       bsig2(ibin)=gar/deltau
       ibin=ibin-1
       gar=0.0d0
     endif
     em=ep
     tm=tp
     emlog=eplog
   enddo
   !
   !**recover the pointwise fission cross sections.
   call xsmlen(draglib,'NUSIGF',ilfiss,ityxsm)
   if(ilfiss.gt.0) then
     call findf(matno,3,18,npen)
     call contio(npen,0,0,scr1,nb,nw)
     ep=0.0d0
     call gety1(ep,enext,idis,tp,npen,scr1)
     em=eaut0
     call gety1(em,enext,idis,tm,npen,scr1)
     ibin=nbfine
     gar=0.0d0
     emlog=log(eaut1/em)
     do while(ep*(1.0d0+1.0d-6).lt.eaut1)
       if(ibin.le.0) call error('drauto','invalid index',' ')
       ep=min(enext,bener(ibin))
       eplog=log(eaut1/ep)
       call gety1(ep,enext,idis,tp,npen,scr1)
       gar=gar+0.5d0*(tm+tp)*(emlog-eplog)
       if(ep.eq.bener(ibin)) then
         deltau=log(bener(ibin)/bener(ibin+1))
         bsig3(ibin)=gar/deltau
         ibin=ibin-1
         gar=0.0d0
       endif
       em=ep
       tm=tp
       emlog=eplog
     enddo
   endif
   !
   !**normalization of BIN cross sections.
   do ig=1,ng
     gar1t(ig)=0.0
     gar1s(ig)=0.0
     gar1f(ig)=0.0
   enddo
   do ig=1,ng*ng
     gar2(ig)=0.0
   enddo
   call xsmget(draglib,'NTOT0',gar1t)
   call xsmget(draglib,'IJJS00',ijj)
   call xsmget(draglib,'NJJS00',njj)
   call xsmget(draglib,'SCAT00',gar2)
   if(ilfiss.gt.0) call xsmget(draglib,'NUSIGF',gar1f)
   igar=0
   do ig2=1,ng
     do ig1=ijj(ig2),ijj(ig2)-njj(ig2)+1,-1
       igar=igar+1
       gar1s(ig1)=gar1s(ig1)+gar2(igar)
     enddo
   enddo
   ibin0=0
   do ig=igaut0,igaut1
     sum1=0.0
     sum2=0.0
     sum3=0.0
     sum4=0.0
     do ibin=ibin0+1,ibin0+nfs(ig)
       deltau=log(bener(ibin)/bener(ibin+1))
       sum1=sum1+deltau
       sum2=sum2+bsig1(ibin)*deltau
       sum3=sum3+(bsig1(ibin)-bsig2(ibin))*deltau
       if(ilfiss.gt.0) sum4=sum4+bsig3(ibin)*deltau
     enddo
     fact1=gar1t(ig)*(sum1/sum2)
     fact2=(gar1t(ig)-gar1s(ig))*(sum1/sum3)
     if((ilfiss.gt.0).and.(sum4.gt.0.0)) fact3=gar1f(ig)*(sum1/sum4)
     do ibin=ibin0+1,ibin0+nfs(ig)
       bsig2(ibin)=fact2*bsig2(ibin)+(fact1-fact2)*bsig1(ibin)
       bsig1(ibin)=fact1*bsig1(ibin)
       if(ilfiss.gt.0) bsig3(ibin)=fact3*bsig3(ibin)
     enddo
     ibin0=ibin0+nfs(ig)
   enddo
   !
   !**store the BIN cross sections on xsm.
   do ibin=1,nbfine
     bgar(ibin)=real(bsig1(ibin))
   enddo
   call xsmput(draglib,'BIN-NTOT0',bgar(1:nbfine))
   if(isbmat.eq.1) then
     write(nsyso,'(''-----------------------------------'')')
     write(nsyso,100) (ener(i),i=igaut0,igaut1+1)
     write(nsyso,'(''--BIN-ENERGY-----------------------'')')
     write(nsyso,100) (bener(i),i=1,nbfine+1)
     write(nsyso,'(''--BIN-NTOT0------------------------'')')
     write(nsyso,100) (bsig1(i),i=1,nbfine),bsig1(nbfine)
   endif
   do ibin=1,nbfine
     bgar(ibin)=real(bsig2(ibin))
   enddo
   call xsmput(draglib,'BIN-SIGS00',bgar(1:nbfine))
   if(ilfiss.gt.0) then
     lfiss=.false.
     do ibin=1,nbfine
       lfiss=lfiss.or.(bsig3(ibin).ne.0.0)
       bgar(ibin)=real(bsig3(ibin))
     enddo
     if(lfiss) call xsmput(draglib,'BIN-NUSIGF',bgar(1:nbfine))
   endif
   call xsmsix(draglib,' ',2)
   !
   call tomend(npen,0,0,scr)
   deallocate(scr1)
   go to 10
   30 deallocate(bgar,bsig3,bsig2,bsig1,bener)
   deallocate(scr)
   return
   100 format(10(1p,e12.5,1h,))
   end subroutine drauto
   !
   subroutine drafis(nin,matd,ytemp,nz0,deltau,igfirs,iglast,nbesp,iesp)
   !-----------------------------------------------------------------
   !   utility routine for recovering fission-related information from a
   !   gendf file.
   !   input parameters:
   !   nin     file unit number of the gendf file.
   !   matd    material number.
   !   ytemp   absolute temperature (Kelvin).
   !   nz0     previous dilution index.
   !   deltau  dilution widths.
   !   igfirs  fastest group index with self-shielding effect.
   !   iglast  fastest thermal group index with no self-shielding effect.
   !   nbesp   number of energy-dependent fission spectra.
   !   iesp    energy limit indices of the energy-dependent fission spectra.
   !-----------------------------------------------------------------
   use mainio ! provides nsysi,contio,nsyso,nsyse
   use endf   ! provides endf routines and variables
   use util   ! provides timer,openz,repoz,error
   integer :: maxa,maxgr,maxnl,maxnz,maxdel,lz
   real(kr) :: ytemp
   parameter (maxa=10000,maxgr=400,maxnl=8,maxnz=30,maxdel=10,lz=6)
   integer nin,matd,nz0,igfirs,iglast,nbesp,iesp(nbesp+1)
   logical lfind,exist,exist2
   character text2*2,cd*4
   real(kr) rv(maxgr,maxnl,maxnz),sigf(maxgr,maxnl,maxnz), &
   & flux(maxgr,maxnl,maxnz),aa(6),ssum(maxdel),deltau(maxgr), &
   & chidel(maxgr,maxdel),vsum(maxgr),sigf2(maxgr,maxnz)
   integer idel,ig,ig1,ig2,igmax,il,isp,iz,jg,loc,nb,ndel,nf,ng,ng2, &
   & nl,nw,nz,nz2,nzm,nzv
   real(kr) all
   real gar1(maxdel),gar2(maxgr)
   integer, save, dimension(4) :: mflist= (/ 19,20,21,38 /)
   real(kr),allocatable,dimension(:) :: scr
   real(kr),allocatable,dimension(:,:,:,:) :: sigfm,sigfm2
   !
   ! initialize the sigf and sigfm arrays.
   allocate(scr(maxa))
   allocate(sigfm(maxgr,maxgr,maxnl,maxnz),sigfm2(maxgr,maxgr,maxnl,maxnz))
   do iz=1,maxnz
     do il=1,maxnl
       do ig=1,maxgr
         sigf(ig,il,iz)=0.0
         do jg=1,maxgr
           sigfm(jg,ig,il,iz)=0.0
           sigfm2(jg,ig,il,iz)=0.0
         enddo
       enddo
     enddo
   enddo
   !
   ! recover the 'NFTOT' vector fission cross sections.
   call fvect(maxgr,maxnl,maxnz,nin,matd,18,ytemp,ng,nl,nzv,sigf2,flux,exist)
   if(.not.exist) return
   !
   ! recover the 'NFTOT' matrix fission cross sections.
   call fmatr(maxgr,maxnl,maxnz,nin,matd,18,ytemp,ng,nl,nzm,sigfm,flux,exist)
   if(.not.exist) then
     ! try to recover a fission matrix by summing partial fission matrices
     ! (if they exist).
     nzm=0
     do nf=1,4
       call fmatr(maxgr,maxnl,maxnz,nin,matd,mflist(nf),ytemp,ng,nl, &
       & nz2,sigfm2,flux,exist2)
       exist=exist.or.exist2
       if(exist2) then
         nzm=max(nzm,nz2)
         do iz=1,nz2
           do il=1,nl
             do ig=1,ng
               do jg=1,ng
                 sigfm(jg,ig,il,iz)=sigfm(jg,ig,il,iz)+sigfm2(jg,ig,il,iz)
               enddo
             enddo
           enddo
         enddo
       endif
     enddo
   endif
   if(.not.exist) then
     write(nsyso,'('' missing fission matrix  '', &
     & ''(endf identification ='',i8,'')'')') matd
     return
   endif
   !
   ! recover the delayed spectra.
   do idel=1,maxdel
     do ig=1,maxgr
       chidel(ig,idel)=0.0
     enddo
   enddo
   call repoz(nin)
   10 lfind=.false.
   do while (.not.lfind)
     if(nin.lt.0) then
       read(-nin,end=20) math,mfh,mth,nb,nw
     else if(nin.gt.0) then
       read(nin,'(6e11.0,i4,i2,i3,i5)',end=20) aa,math,mfh,mth,nsp
     endif
     lfind=(math.eq.matd).and.(mfh.eq.5).and.(mth.eq.455)
   enddo
   if(.not.lfind) go to 20
   if(nin.lt.0) then
     call skiprz(nin,-1)
     read(-nin) math,mfh,mth,nb,nw,aa
   endif
   ndel=nint(aa(3))
   nz=nint(aa(4))
   ng=nint(aa(6))
   call listio(nin,0,0,scr(1),nb,nw)
   if(abs(scr(1)-ytemp).gt.1.0e-3) then
     call tomend(nin,0,0,scr)
     go to 10
   endif
   if(ng.gt.maxgr) call error('drafis','maxgr overflow',' ')
   if(nz.ne.1) call error('drafis','no dilution dependence expected',' ')
   if(ndel.gt.maxdel) call error('drafis','maxdel overflow',' ')
   ng2=l1h
   loc=1+nw
   do while (nb.ne.0)
     if(loc+302.gt.maxa) call error('drafis','endf input size exceeded',' ')
     call moreio(nin,0,0,scr(loc),nb,nw)
     loc=loc+nw
   enddo
   do idel=1,ndel
     gar1(idel)=scr(lz+idel)
   enddo
   if(ndel.gt.0) call xsmput(draglib,'LAMBDA-D',gar1(1:ndel))
   do idel=1,ndel
     ssum(idel)=0.0
     do ig=1,ng2-1
       ssum(idel)=ssum(idel)+scr(lz+ndel+(ig-1)*ndel+idel)
     enddo
     do ig=1,ng2-1
       chidel(ng-ig+1,idel)=scr(lz+ndel+(ig-1)*ndel+idel)/ssum(idel)
     enddo
     do ig=ng2,ng
       chidel(ng-ig+1,idel)=0.0
     enddo
     do ig=1,ng
       gar2(ig)=chidel(ig,idel)
     enddo
     write(text2,'(i2.2)') idel
     call xsmput(draglib,'CHI'//text2,gar2(1:ng))
   enddo
   call fvect(maxgr,maxnl,maxnz,nin,matd,455,ytemp,ng,nl,nz,rv,flux,exist)
   call fvect(maxgr,maxnl,maxnz,nin,matd,18,ytemp,ng,nl,nz,sigf,flux,exist)
   do iz=1,nz
     do ig=igfirs,iglast-1
       if(flux(ig,1,1).eq.0.0) cycle
       flux(ig,1,nz-iz+1)=flux(ig,1,nz-iz+1)/flux(ig,1,1)
     enddo
   enddo
   do idel=1,ndel
     do ig=1,ng
       gar2(ig)=rv(ig,1,1)*sigf(ig,1,1)*ssum(idel)
     enddo
     do ig=ng,1,-1
       igmax=ig
       if(gar2(ig).ne.0.0) go to 15
     enddo
     15 write(text2,'(i2.2)') idel
     call xsmput(draglib,'NUSIGF'//text2,gar2(1:igmax))
     do iz=1,nz-1
     !
     ! ***process finite dilution x-sections
       do ig=1,iglast-1
         gar2(ig)=0.0
       enddo
       do ig=igfirs,iglast-1
         if(sigf(ig,1,1).eq.0.0) cycle
         gar2(ig)=rv(ig,1,1)*ssum(idel)*(sigf(ig,1,nz-iz+1)* &
         & flux(ig,1,nz-iz+1)-sigf(ig,1,1))
       enddo
       do ig=iglast-1,1,-1
         igmax=ig
         if(gar2(ig).ne.0.0) go to 16
       enddo
       go to 17
       16 write (cd,'(i4.4)') nz0+iz
       call xsmsix(draglib,'SUBMAT'//cd,1)
       call xsmput(draglib,'NUSIGF'//text2,gar2(1:igmax))
       call xsmsix(draglib,' ',2)
       17 continue
     enddo
     do iz=1,nz
       do ig2=1,ng
         do ig1=1,ng
           sigfm(ig1,ig2,1,iz)=sigfm(ig1,ig2,1,iz)+rv(ig1,1,1)* &
           & sigf(ig1,1,iz)*ssum(idel)*chidel(ig2,idel)
         enddo
       enddo
     enddo
   enddo
   !
   ! process the prompt 'NUSIGF' and 'CHI' vectors.
   20 call fvect(maxgr,maxnl,maxnz,nin,matd,18,ytemp,ng,nl,nz,sigf,flux,exist)
   if((nzm.ne.nz).and.(nzm.ne.1)) then
     call error('drafis','invalid number of dilutions with mf=6, mt='//'18',' ')
   endif
   do 25 isp=1,nbesp
   do iz=1,nzm
     if(iz.eq.1) then
       all=0.0
       do ig2=1,ng
         vsum(ig2)=0.0
         do ig1=iesp(isp)+1,iesp(isp+1)
           vsum(ig2)=vsum(ig2)+sigfm(ig1,ig2,1,iz)*flux(ig1,1,iz)
         enddo
         do ig1=iesp(isp)+1,iesp(isp+1)
           all=all+sigfm(ig1,ig2,1,iz)*flux(ig1,1,iz)
         enddo
       enddo
       do ig1=1,ng
         gar2(ig1)=vsum(ig1)/all
       enddo
       if(nbesp.eq.1) then
         call xsmput(draglib,'CHI',gar2(1:ng))
       else if(all.ne.0.0) then
         write(text2,'(i2.2)') isp
         call xsmput(draglib,'CHI--'//text2,gar2(1:ng))
       endif
     endif
   enddo
   25 continue
   do iz=1,nzm
     do ig1=1,ng
       vsum(ig1)=0.0
       do ig2=1,ng
         vsum(ig1)=vsum(ig1)+sigfm(ig1,ig2,1,iz)
       enddo
     enddo
     if(nzm.gt.1) then
       do ig=1,ng
         sigf2(ig,iz)=vsum(ig)
       enddo
     endif
   enddo
   if(nzm.eq.1) then
     do iz=nz,1,-1
       do ig=1,ng
         if(sigf(ig,1,1).ne.0.0) then
           sigf2(ig,iz)=sigf(ig,1,iz)*vsum(ig)/sigf(ig,1,1)
         endif
       enddo
     enddo
   endif
   do ig=ng,1,-1
     igmax=ig
     if(sigf2(ig,1).ne.0.0) go to 30
   enddo
   30 do ig=1,igmax
     gar2(ig)=sigf2(ig,1)
   enddo
   call xsmput(draglib,'NUSIGF',gar2(1:igmax))
   do iz=1,nz-1
     !
     ! ***process 'NUSIGF' finite dilution x-sections
     do ig=1,iglast-1
       gar2(ig)=0.0d0
     enddo
     do ig=igfirs,iglast-1
       if(flux(ig,1,1).eq.0.0) cycle
       if(sigf2(ig,1).eq.0.0) cycle
       flux(ig,1,nz-iz+1)=flux(ig,1,nz-iz+1)/flux(ig,1,1)
       gar2(ig)=sigf2(ig,nz-iz+1)*flux(ig,1,nz-iz+1)-sigf2(ig,1)
     enddo
     do ig=iglast-1,1,-1
       igmax=ig
       if(gar2(ig).ne.0.0) go to 40
     enddo
     go to 50
     40 write (cd,'(i4.4)') nz0+iz
     call xsmsix(draglib,'SUBMAT'//cd,1)
     call xsmput(draglib,'NUSIGF',gar2(1:igmax))
     call xsmsix(draglib,' ',2)
     50 continue
   enddo
   deallocate(sigfm2,sigfm)
   deallocate(scr)
   return
   end subroutine drafis
   !
   subroutine drasc(nin,matd,ytemp,nz0,deltau,igfirs,iglast,ipflag)
   !-----------------------------------------------------------------
   !   utility routine for recovering scattering-related information from
   !   a gendf file.
   !   input parameters:
   !   nin     file unit number of the gendf file.
   !   matd    material number.
   !   ytemp   absolute temperature (Kelvin).
   !   nz0     previous dilution index.
   !   deltau  dilution widths.
   !   igfirs  fastest group index with self-shielding effect.
   !   iglast  fastest thermal group index with no self-shielding effect.
   !   ipflag  fission product flag.
   !-----------------------------------------------------------------
   use mainio ! provides nsysi,contio,nsyso,nsyse
   use endf   ! provides endf routines and variables
   use util   ! provides timer,openz,repoz,error
   integer :: maxa,maxgr,maxnl,maxnz,maxmat,maxgar
   parameter (maxa=2000)
   parameter(maxgr=400,maxnl=8,maxnz=30,maxmat=45,maxgar=maxgr*maxgr)
   integer nin,matd,nz0,ijj(maxgr),njj(maxgr),igfirs(maxnl),iglast(maxnl), &
   & ipflag
   logical exist,exist2,exist3,lfind
   character cd*4,cdl*2
   integer i,ig,ig1,ig2,igar,igmax,igmin,il,imat,imax,imt,iz,loc,nb,ng, &
   & ngther,nl,nl2,nlgar,nw,nz,nze
   real(kr) ytemp,rm(maxgr,maxgr,maxnl,maxnz),flux(maxgr,maxnl,maxnz), &
   & sigsm(maxgr,maxgr,maxnl,maxnz),deltau(maxgr),aa(6),delta,dgar,garmax
   real gar(maxgar)
   real(kr),allocatable,dimension(:) :: scr
   integer,save,dimension(maxmat) :: mtlist= &
   & (/ 2,16,17,37,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,&
   & 70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91 /)
   !
   ! recover the number of thermal-corrected groups (ngther).
   allocate(scr(maxa))
   ngther=0
   call repoz(nin)
   10 lfind=.false.
   do while (.not.lfind)
     if(nin.lt.0) then
       read(-nin,end=20) math,mfh,mth,nb,nw
     else if(nin.gt.0) then
       read(nin,'(6e11.0,i4,i2,i3,i5)',end=20) aa,math,mfh,mth,nsp
     endif
     lfind=(math.eq.matd).and.(mfh.eq.6).and.(mth.ge.221).and.(mth.le.250)
   enddo
   if(.not.lfind) go to 20
   if(nin.lt.0) then
     call skiprz(nin,-1)
     read(-nin) math,mfh,mth,nb,nw,aa
   endif
   ng=nint(aa(6))
   call listio(nin,0,0,scr(1),nb,nw)
   if(abs(scr(1)-ytemp).gt.1.0e-3) then
     call tomend(nin,0,0,scr)
     go to 10
   endif
   lfind=.true.
   do while (mth.ne.0) ! loop over energy groups
     if(.not.lfind) call listio(nin,0,0,scr(1),nb,nw)
     if(n2h.eq.ng) go to 20
     ngther=max(ngther,n2h)
     lfind=.false.
     loc=1+nw
     do while (nb.ne.0)
       if(loc+302.gt.maxa) call error('drasc','endf input size exceeded',' ')
       call moreio(nin,0,0,scr(loc),nb,nw)
       loc=loc+nw
     enddo
   enddo
   20 ngther=ng-ngther
   write(nsyso,'(/ &
   & '' number of energy groups affected by thermalization for '', &
   & ''material'',i6,'' ='',i6)') matd,ng-ngther
   !
   ! **recover thermal correction x-sections and compute the
   ! correction term for groups indices higher than ngther.
   do iz=1,maxnz
     do il=1,maxnl
       do ig2=1,maxgr
         do ig1=1,maxgr
          sigsm(ig1,ig2,il,iz)=0.0
         enddo
       enddo
     enddo
   enddo
   exist3=.false.
   do imt=250,221,-1
     call fmatr(maxgr,maxnl,1,nin,matd,imt,ytemp,ng,nl2,nz,rm,flux,exist2)
     if(exist2) then
       exist3=.true.
       nlgar=nl2
       do iz=1,maxnz
         do il=1,nl2
           do ig2=1,ng
             do ig1=ngther+1,ng
               sigsm(ig1,ig2,il,iz)=sigsm(ig1,ig2,il,iz)+rm(ig1,ig2,il,1)
             enddo
           enddo
         enddo
       enddo
     endif
   enddo
   if(exist3) then
     call fmatr(maxgr,maxnl,maxnz,nin,matd,2,ytemp,ng,nl,nz,rm,flux,exist)
     if(nl.ne.nlgar) call error('drasc','inconsistent number of L'// &
     & 'egendre orders(2)',' ')
     do iz=1,maxnz
       do il=1,nl
         do ig2=1,ng
           do ig1=ngther+1,ng
             sigsm(ig1,ig2,il,iz)=sigsm(ig1,ig2,il,iz)-rm(ig1,ig2,il,1)
           enddo
         enddo
       enddo
     enddo
   endif
   !
   do imat=1,maxmat
     call fmatr(maxgr,maxnl,maxnz,nin,matd,mtlist(imat),ytemp,ng,nl, &
     & nz,rm,flux,exist)
     if(mtlist(imat).eq.2) nze=nz
     if(exist) then
       if(nz.eq.1) then
         do iz=1,nze
           do il=1,nl
             do ig2=1,ng
               do ig1=1,ng
                 sigsm(ig1,ig2,il,iz)=sigsm(ig1,ig2,il,iz)+rm(ig1,ig2,il,1)
               enddo
             enddo
           enddo
         enddo
       else if(nz.eq.nze) then
         do iz=1,nz
           do il=1,nl
             do ig2=1,ng
               do ig1=1,ng
                 sigsm(ig1,ig2,il,iz)=sigsm(ig1,ig2,il,iz)+rm(ig1,ig2,il,iz)
               enddo
             enddo
           enddo
         enddo
       else
         call error('drasc','invalid number of dilutions.',' ')
       endif
     endif
   enddo
   !
   ! store the scattering matrices on the xsm file
   call fmatr(maxgr,maxnl,maxnz,nin,matd,2,ytemp,ng,nl,nz,rm,flux,exist)
   do il=1,nl
     !
     ! ***lump the scattering matrices of a fission product
     if(ipflag.eq.1) then
       do iz=1,nz
         do ig1=1,ng
           dgar=0.0
           do ig2=1,ng
             dgar=dgar+sigsm(ig1,ig2,il,iz)
           enddo
           do ig2=1,ng
             sigsm(ig1,ig2,il,iz)=0.0
           enddo
           sigsm(ig1,ig1,il,iz)=dgar
         enddo
       enddo
     endif
     !
     write(cdl,'(i2.2)') il-1
     igar=0
     garmax=0.0
     ! ***ig2 is the secondary group
     do ig2=1,ng
       igmin=ig2
       igmax=ig2
       !
       ! ***ig1 is the primary group
       do ig1=ng,1,-1
         if(sigsm(ig1,ig2,il,1).ne.0.0) then
           igmin=min(igmin,ig1)
           igmax=max(igmax,ig1)
         endif
       enddo
       ijj(ig2)=igmax
       njj(ig2)=igmax-igmin+1
       do ig1=igmax,igmin,-1
         igar=igar+1
         if(igar.gt.maxgar) call error('drasc','gar overflow.',' ')
         gar(igar)=sigsm(ig1,ig2,il,1)
         if(abs(gar(igar)).gt.garmax) garmax=abs(gar(igar))
       enddo
     enddo
     !
     ! ***write on the xsm file
     call xsmput(draglib,'SCAT'//cdl,gar(1:igar))
     call xsmput(draglib,'NJJS'//cdl,njj(1:ng))
     call xsmput(draglib,'IJJS'//cdl,ijj(1:ng))
     do iz=1,nz-1
       write (cd,'(i4.4)') nz0+iz
       call xsmsix(draglib,'SUBMAT'//cd,1)
       !
       ! ***process finite dilution scattering information
       do ig=1,ng
         if(flux(ig,il,1).eq.0.0) cycle
         flux(ig,il,nz-iz+1)=flux(ig,il,nz-iz+1)/flux(ig,il,1)
       enddo
       igar=0
       garmax=0.0
       ! ***ig2 is the secondary group
       do ig2=1,igfirs(il)-1
         ijj(ig2)=ig2
         njj(ig2)=1
         igar=igar+1
         if(igar.gt.maxgar) call error('drasc','gar overflow.',' ')
         gar(igar)=0.0d0
       enddo
       do ig2=igfirs(il),ng
         igmin=ig2
         igmax=ig2
         !
         ! ***ig1 is the primary group
         do ig1=iglast(il)-1,igfirs(il),-1
           delta=sigsm(ig1,ig2,il,nz-iz+1)*flux(ig1,il,nz-iz+1)- &
           & sigsm(ig1,ig2,il,1)
           if(abs(delta).gt.1.0e-6*abs(sigsm(ig1,ig1,il,1))) then
             igmin=min(igmin,ig1)
             igmax=max(igmax,ig1)
           endif
         enddo
         ijj(ig2)=igmax
         njj(ig2)=igmax-igmin+1
         do ig1=igmax,igmin,-1
           igar=igar+1
           if(igar.gt.maxgar) call error('drasc','gar overflow.',' ')
           if((ig1.ge.igfirs(il)).and.(ig1.lt.iglast(il))) then
             gar(igar)=sigsm(ig1,ig2,il,nz-iz+1)*flux(ig1,il,nz-iz+1)- &
             & sigsm(ig1,ig2,il,1)
             if(abs(gar(igar)).gt.garmax) garmax=abs(gar(igar))
           else
             gar(igar)=0.0d0
           endif
         enddo
       enddo
       !
       ! ***write on the xsm file
       imax=igar
       do i=imax,1,-1
         if(abs(gar(i)).ge.1.0e-8*garmax) go to 30
         igar=igar-1
       enddo
       30 if(igar.gt.0) then
         call xsmput(draglib,'SCAT'//cdl,gar(1:igar))
         call xsmput(draglib,'NJJS'//cdl,njj(1:ng))
         call xsmput(draglib,'IJJS'//cdl,ijj(1:ng))
       endif
       call xsmsix(draglib,' ',2)
     enddo
   enddo
   !**end of Legendre order loop
   deallocate(scr)
   return
   end subroutine drasc
   !
   subroutine fmatr(maxgr,maxnl,maxnz,nin,matd,mt,ytemp,ng,nl,nz,rm,flux,exist)
   !-----------------------------------------------------------------
   !   utility routine for recovering a matrix reaction from a gendf file.
   !   input parameters:
   !   maxgr   first and second dimensions of matrix rm. Maximum number
   !           of energy groups.
   !   maxnl   third dimension of matrix rm. Maximum number of Legendre orders.
   !   maxnz   4-th dimensions of matrix rm. Maximum number of dilutions.
   !   nin     file unit number of the gendf file.
   !   matd    material number.
   !   mt      reaction index.
   !   ytemp   absolute temperature (Kelvin).
   !   output parameters:
   !   ng      number of energy groups.
   !   nl      number of Legendre orders.
   !   nz      number of dilutions.
   !   rm      matrix reaction (primary->secondary,Legendre,dilution).
   !   flux    flux (primary,Legendre,dilution).
   !   exist   matrix reaction existence flag.
   !-----------------------------------------------------------------
   use endf   ! provides endf routines and variables
   use util   ! provides error
   use util   ! provides timer,openz,repoz,error
   integer :: maxa,lz
   parameter (maxa=200000,lz=6)
   integer maxgr,maxnl,maxnz,nin,matd,mt,ng,nl,nz,ig,ig1,ig2,ig2lo,il,iz, &
   & jg,k,loc,loca,locf,nb,ng2,nw
   logical exist,lfind
   real(kr) ytemp,rm(maxgr,maxgr,maxnl,maxnz),flux(maxgr,maxnl,maxnz),aa(6)
   real(kr),allocatable,dimension(:) :: scr,cspc
   !
   do iz=1,maxnz
     do il=1,maxnl
       do ig2=1,maxgr
       flux(ig2,il,iz)=0.0
         do ig1=1,maxgr
           rm(ig1,ig2,il,iz)=0.0
         enddo
       enddo
     enddo
   enddo
   allocate(scr(maxa),cspc(maxgr))
   do ig1=1,maxgr
     cspc(ig1)=0.0
   enddo
   call repoz(nin)
   10 lfind=.false.
   do while (.not.lfind)
     if(nin.lt.0) then
       read(-nin,end=900) math,mfh,mth,nb,nw
     else if(nin.gt.0) then
       read(nin,'(6e11.0,i4,i2,i3,i5)',end=900) aa,math,mfh,mth,nsp
     endif
     lfind=(math.eq.matd).and.(mfh.eq.6).and.(mth.eq.mt)
   enddo
   if(.not.lfind) go to 900
   if(nin.lt.0) then
     call skiprz(nin,-1)
     read(-nin) math,mfh,mth,nb,nw,aa
   endif
   nl=nint(aa(3))
   nz=nint(aa(4))
   ng=nint(aa(6))
   call listio(nin,0,0,scr(1),nb,nw)
   if(abs(scr(1)-ytemp).gt.1.0e-3) then
     call tomend(nin,0,0,scr)
     go to 10
   endif
   if(ng.gt.maxgr) call error('fmatr','maxgr overflow',' ')
   if(nl.gt.maxnl) call error('fmatr','maxnl overflow',' ')
   if(nz.gt.maxnz) call error('fmatr','maxnz overflow',' ')
   lfind=.true.
   do while (mth.ne.0) ! loop over energy groups
     if(.not.lfind) call listio(nin,0,0,scr(1),nb,nw)
     ng2=l1h
     ig2lo=l2h
     ig=n2h
     lfind=.false.
     loc=1+nw
     do while (nb.ne.0)
       if(loc+302.gt.maxa) call error('fmatr','endf input size exceeded',' ')
       call moreio(nin,0,0,scr(loc),nb,nw)
       loc=loc+nw
     enddo
     if(ig.ne.0) then
       jg=ng-ig+1
       do il=1,nl
         do iz=1,nz
           locf=1+lz+nl*(iz-1)+(il-1)
           flux(jg,il,iz)=scr(locf)
           do k=2,ng2
             loca=locf+nl*nz*(k-1)
             if(ig2lo.ne.0) then
               ! matrix part
               ig2=ig2lo+k-2
               rm(jg,ng-ig2+1,il,iz)=rm(jg,ng-ig2+1,il,iz)+scr(loca)
             else
               ! spectrum part
               do ig2=1,ng
                 rm(jg,ng-ig2+1,il,iz)=rm(jg,ng-ig2+1,il,iz)+cspc(ig2)*scr(loca)
               enddo
             endif
           enddo
         enddo
       enddo
     else
       ! save constant spectrum
       do k=1,ng2
         cspc(ig2lo+k-1)=scr(1+lz+nl*nz*(k-1))
       enddo
     endif
   enddo
   exist=.true.
   deallocate(cspc,scr)
   return
   !
   900 exist=.false.
   ng=0
   nl=0
   nz=0
   deallocate(cspc,scr)
   return
   end subroutine fmatr
   !
   subroutine fvect(maxgr,maxnl,maxnz,nin,matd,mt,ytemp,ng,nl,nz,rv,flux,exist)
   !-----------------------------------------------------------------
   !   utility routine for recovering a vector reaction from a gendf file.
   !   input parameters:
   !   maxgr   first dimension of vector rv. Maximum number of energy groups.
   !   maxnl   second dimension of vector rv. Maximum number of Legendre orders.
   !   maxnz   third dimensions of vector rv. Maximum number of dilutions.
   !   nin     file unit number of the gendf file.
   !   matd    material number.
   !   mt      reaction index.
   !   ytemp   absolute temperature (Kelvin).
   !   output parameters:
   !   ng      number of energy groups.
   !   nl      number of Legendre orders.
   !   nz      number of dilutions.
   !   rv      vector reaction (group,Legendre,dilution).
   !   flux    flux (group,Legendre,dilution).
   !   exist   vector reaction existence flag.
   !-----------------------------------------------------------------
   use endf   ! provides endf routines and variables
   use util   ! provides error
   use util   ! provides timer,openz,repoz,error
   integer :: maxa,lz
   parameter (maxa=2000,lz=6)
   integer maxgr,maxnl,maxnz,nin,matd,mt,ng,nl,nz,ig,il,iz,jg,loc,locf,nb, &
   & ng2, nw
   logical exist,lfind
   real(kr) ytemp,rv(maxgr,maxnl,maxnz),flux(maxgr,maxnl,maxnz),aa(6)
   real(kr),allocatable,dimension(:) :: scr
   !
   allocate(scr(maxa))
   do iz=1,maxnz
     do il=1,maxnl
       do ig=1,maxgr
       flux(ig,il,iz)=0.0d0
       rv(ig,il,iz)=0.0d0
       enddo
     enddo
   enddo
   call repoz(nin)
   10 lfind=.false.
   do while (.not.lfind)
     if(nin.lt.0) then
       read(-nin,end=900) math,mfh,mth,nb,nw
     else if(nin.gt.0) then
       read(nin,'(6e11.0,i4,i2,i3,i5)',end=900) aa,math,mfh,mth,nsp
     endif
     lfind=(math.eq.matd).and.(mfh.eq.3).and.(mth.eq.mt)
   enddo
   if(.not.lfind) go to 900
   if(nin.lt.0) then
     call skiprz(nin,-1)
     read(-nin) math,mfh,mth,nb,nw,aa
   endif
   nl=nint(aa(3))
   nz=nint(aa(4))
   ng=nint(aa(6))
   call listio(nin,0,0,scr(1),nb,nw)
   if(abs(scr(1)-ytemp).gt.1.0e-3) then
     call tomend(nin,0,0,scr)
     go to 10
   endif
   if(ng.gt.maxgr) call error('fvect','maxgr overflow',' ')
   if(nl.gt.maxnl) call error('fvect','maxnl overflow',' ')
   if(nz.gt.maxnz) call error('fvect','maxnz overflow',' ')
   lfind=.true.
   do while (mth.ne.0) ! loop over energy groups
     if(.not.lfind) call listio(nin,0,0,scr(1),nb,nw)
     ng2=l1h
     ig=n2h
     lfind=.false.
     loc=1+nw
     do while (nb.ne.0)
       if(loc+302.gt.maxa) call error('fvect','endf input size exceeded', &
       & ' ')
       call moreio(nin,0,0,scr(loc),nb,nw)
       loc=loc+nw
     enddo
     jg=ng-ig+1
     do il=1,nl
       do iz=1,nz
         locf=1+lz+nl*(iz-1)+(il-1)
         flux(jg,il,iz)=scr(locf)
         rv(jg,il,iz)=scr(locf+nl*nz)
       enddo
     enddo
   enddo
   exist=.true.
   deallocate(scr)
   return
   !
   900 exist=.false.
   ng=0
   nl=0
   nz=0
   deallocate(scr)
   return
   end subroutine fvect
   !
   subroutine dradep(nfp,ndcy)
   !-----------------------------------------------------------------
   !   compute depletion-related information.
   !-----------------------------------------------------------------
   use mainio ! provides nsysi,contio,nsyso,nsyse
   use endf   ! provides endf routines and variables
   use util   ! provides timer,openz,repoz,error
   integer :: maxa,maxiso,nreac,nfath,maxch
   parameter(maxa=10000,maxiso=6500,nreac=13,nfath=800,maxch=6000)
   integer nfp,ndcy,izae,nbch,nbfiss,nbfp,nbfpch,nbiso,nw
   real(kr) en(nfath,maxch),br(nfath,maxch)
   integer mylist(maxiso,3)
   character(len=4) hiso(3,maxiso)
   integer i,i1,ia,ifp,ifps,igar,ii,ile,ind,iof,ipos,iso,itext4,iz,j, &
   & jpos,k,lep1,loc,maxfp,nb,nbdpf
   real(kr) awr,energy,za
   character hname*8,hsmg*72,hich(maxch)*8,hrch(nfath,maxch)*8,text4*4
   integer,allocatable,dimension(:,:) :: idreac,ipreac
   real(kr),allocatable,dimension(:) :: ddeca,scr
   real(kr),allocatable,dimension(:,:) :: dener,prate,yield
   !
   !**read the specification lines for the isotopes present in the
   !  burnup chain. The specification is:
   !   [[
   !   hich [[ hrch en br ]] /
   !   ]]
   !   end /
   !   where hich : character*8 name of the isotope
   !         hrch : character*8 name of a neutron induced reaction (not
   !                a scattering type reaction)
   !         en   : energy released by the neutron induced reaction
   !                (including the kinetic energy of the secondary neutrons)
   !         br   : branching ratio to an isomeric daughter.
   allocate(scr(maxa))
   do iso=1,maxiso
     mylist(iso,1)=0
     mylist(iso,2)=0
     mylist(iso,3)=0
   enddo
  nbch=0
   nbiso=0
   do
     nbch=nbch+1
     if(nbch.gt.maxch) call error('dradep','maxch overflow',' ')
     do i=1,nfath
       hrch(i,nbch)=' '
       en(i,nbch)=0.0
       br(i,nbch)=0.0
     enddo
     write(6,*) 'read chain iso#',nbch
     read(nsysi,*) hich(nbch),(hrch(i,nbch),en(i,nbch),br(i,nbch),i=1,nfath)
     do i=1,nfath
       if(br(i,nbch).ne.0.0) then
         write(nsyso,'('' isomeric '',a,'' branching ratio='',f7.3, &
         & '' set for '',a)') hrch(i,nbch),br(i,nbch),hich(nbch)
       endif
     enddo
     izae=0
     if(hich(nbch).eq.'end') go to 90
     i1=index(hich(nbch),'_')
     if(i1.eq.0) then
       hname=hich(nbch)
     else
       hname=hich(nbch)(:i1-1)
     endif
     call dranam(izae,hname)
     nbiso=nbiso+1
     if(nbiso.gt.maxiso) call error('dradep','maxiso overflow',' ')
     mylist(nbiso,1)=izae
     mylist(nbiso,3)=nbch
   enddo
   90 nbch=nbch-1
   if(nbch.eq.0) return
   !
   !**set the list of depleting isotopes
   nbfpch=0
   call repoz(nfp)
   100 if(nfp.gt.0) then
     read(nfp,'(66x,i4,i2,i3)') math,mfh,mth
   else
     read(-nfp) math,mfh,mth
   endif
   if(math.eq.0) go to 100
   nbfiss=0
   if((mfh.ne.0).and.(mth.ne.0)) call skiprz(nfp,-1)
   do
     150 call contio(nfp,0,0,scr,nb,nw)
     if(math.eq.-1) then
       go to 300
     else if(mfh.ne.1) then
       call tofend(nfp,0,0,scr)
       go to 150
     else if(mth.ne.451) then
       call tosend(nfp,0,0,scr)
       go to 150
     endif
     nbfiss=nbfiss+1
     za=c1h
     awr=c2h
     call contio(nfp,0,0,scr,nb,nw)
     izae=10*nint(za+0.1)+l2h
     if(izae.eq.0) call error('dradep','izae error',' ')
     call draind(nbiso,izae,mylist(1,1),ind)
     if(ind.le.0) then
       nbiso=nbiso+1
       if(nbiso.gt.maxiso) call error('dradep','maxiso overflow',' ')
       mylist(nbiso,1)=izae
       call dranam(izae,hname)
       do i=1,nbch
         if(hich(i).eq.hname) go to 160
       enddo
     endif
     160 call findf(math,8,454,nfp)
     call contio(nfp,0,0,scr,nb,nw)
     lep1=l1h
     do ile=1,lep1
       call listio(nfp,0,0,scr(1),nb,nw)
       nbfp=n2h
       energy=c1h
       loc=1+nw
       do while (nb.ne.0)
         if(loc+302.gt.maxa) call error('dradep','endf input size exceeded',' ')
         call moreio(nfp,0,0,scr(loc),nb,nw)
         loc=loc+nw
       enddo
       if(energy.le.2.0e6) then
         do ifp=1,nbfp
           iof=6+(ifp-1)*4+1
           if(scr(iof+2).gt.1.0e-10) then
             iz=nint(scr(iof)/1000+0.1)
             ifps=nint(scr(iof+1)+0.1)
             ia=mod(nint(scr(iof)+0.1),1000)
             izae=10000*iz+10*ia+ifps
             call draind(nbiso,izae,mylist(1,1),ind)
             if(ind.le.0) then
               nbiso=nbiso+1
               if(nbiso.gt.maxiso) call error('dradep','maxiso overflow',' ')
               mylist(nbiso,1)=izae
               call dranam(izae,hname)
               do i=1,nbch
                 if(hich(i).eq.hname) then
                   nbfpch=nbfpch+1
                   go to 170
                 endif
               enddo
             endif
             170 continue
           endif
         enddo
       endif
     enddo
     call tomend(nfp,0,0,scr)
   enddo
   300 nbdpf=nbiso-nbfiss
   nbfpch=nbch
   !
   call repoz(ndcy)
   400 if(ndcy.gt.0) then
     read(ndcy,'(66x,i4,i2,i3)') math,mfh,mth
   else
     read(-ndcy) math,mfh,mth
   endif
   if(math.eq.0) go to 400
   if((mfh.ne.0).and.(mth.ne.0)) call skiprz(ndcy,-1)
   do
     500 call contio(ndcy,0,0,scr,nb,nw)
     if(math.eq.-1) then
       go to 600
     else if(mfh.ne.1) then
       call tofend(ndcy,0,0,scr)
       go to 500
      else if(mth.ne.451) then
       call tosend(ndcy,0,0,scr)
       go to 500
     endif
     za=c1h
     call contio(ndcy,0,0,scr,nb,nw)
     izae=10*nint(za+0.1)+l2h
     call draind(nbiso,izae,mylist(1,1),ind)
     if(ind.le.0) then
       nbiso=nbiso+1
       if(nbiso.gt.maxiso) call error('dradep','maxiso overflow',' ')
       mylist(nbiso,1)=izae
       mylist(nbiso,2)=math
     else
       if((mylist(ind,2).ne.math).and.(mylist(ind,2).ne.0)) then
         write(hsmg,'(''invalid mat index (='',i6,''; expected='', &
         & i6,'' izae='',i8,'')'')') math,mylist(ind,2),izae
         call error('dradep',hsmg,' ')
       endif
       mylist(ind,2)=math
     endif
     call tomend(ndcy,0,0,scr)
   enddo
   !
   !*sort the izae entries.
   600 do k=2,nbiso
     i=k-1
     ipos=0
     if(mylist(i,3).gt.0) ipos=index(hich(mylist(i,3)),'_')+1
     do j=k,nbiso
       jpos=0
       if(mylist(j,3).gt.0) jpos=index(hich(mylist(j,3)),'_')+1
       if((mylist(i,1).gt.mylist(j,1)).or. &
       & ((mylist(i,1).eq.mylist(j,1)).and.(ipos.gt.jpos))) then
         do ii=1,3
           igar=mylist(i,ii)
           mylist(i,ii)=mylist(j,ii)
           mylist(j,ii)=igar
         enddo
       endif
     enddo
   enddo
   !
   text4=' '
   read(text4,'(a4)') itext4
   do iso=1,nbiso
     if(mylist(iso,3).ne.0) then
       hname=hich(mylist(iso,3))
     else
       call dranam(mylist(iso,1),hname)
     endif
     hiso(1,iso)=hname(1:4)
     hiso(2,iso)=hname(5:8)
     hiso(3,iso)=' '
   enddo
   !
   maxfp=nbdpf+50 ! reserve 50 location for lumped fp daughters
   allocate(idreac(nreac,nbiso),ipreac(nfath,nbiso))
   allocate(dener(nreac,nbiso),ddeca(nbiso),prate(nfath,nbiso), &
   & yield(nbfiss,maxfp))
   call draevo(maxfp,nbiso,nbfiss,nbdpf,nreac,nfath,mylist(1,1),nfp, &
   & ndcy,idreac,dener,ddeca,ipreac,prate,yield)
   !
   !**lump the burnup chain from nbiso to nbch isotopes
   call dralum(maxfp,nbiso,nbfiss,nbdpf,nreac,nfath,mylist(1,1), &
   & hiso,nfp,ndcy,nbch,nbfpch,hich,hrch,en,br,idreac,dener,ddeca, &
   & ipreac,prate,yield)
   deallocate(yield,prate,ddeca,dener)
   deallocate(ipreac,idreac)
   deallocate(scr)
   return
   end subroutine dradep
   !
   subroutine draevo(maxfp,nbiso,nbfiss,nbdpf,nreac,nfath,mylist,nfp, &
   & ndcy,idreac,dener,ddeca,ipreac,prate,yield)
   !-----------------------------------------------------------------
   !   compute blocks 'DEPLETE-IDEN', 'DEPLETE-REAC', 'DEPLETE-ENER',
   !   'DEPLETE-DECA', etc. for the non-lumped chain
   !-----------------------------------------------------------------
   use mainio ! provides nsysi,contio,nsyso,nsyse
   use endf   ! provides endf routines and variables
   use util   ! provides timer,openz,repoz,error
   integer :: maxa,maxen,lz
   parameter(maxa=10000,maxen=4,lz=6)
   integer maxfp,nbiso,nbfiss,nbdpf,nreac,nfath,nfp,ndcy,i,ia,idy,ifath, &
   & ifiss,ifp,ifpp,ifps,ifpss,ile,ind,iof,ipf,ireac,iso,iz,iza,izae,ja, &
   & jnd,jz,jzae,lep1,loc,nb,nbdy,nbfp,nw
   real(kr) summ,za,rtyp
   integer mylist(nbiso),idreac(nreac,nbiso),ipreac(nfath,nbiso)
   real(kr) awr,energy,dener(nreac,nbiso),ddeca(nbiso),prate(nfath,nbiso), &
   & yield(nbfiss,maxfp)
   integer, allocatable, dimension(:) :: indpf
   real(kr), allocatable, dimension(:) :: scr
   character text4*4,hname*8,hsmg*72
   real(kr), save, dimension(maxen) :: terp=(/ 1.0, 0.0, 0.0, 0.0 /)
   !
   allocate(indpf(nbdpf),scr(maxa))
   do iso=1,nbiso
     ddeca(iso)=0.0
     do ireac=1,nreac
       idreac(ireac,iso)=0
       dener(ireac,iso)=0.0
     enddo
   enddo
   do iso=1,nbiso
     do ifath=1,nfath
       ipreac(ifath,iso)=0
       prate(ifath,iso)=0.0
     enddo
   enddo
   do ipf=1,nbdpf
     indpf(ipf)=0
     do ifiss=1,nbfiss
       yield(ifiss,ipf)=0.0
     enddo
   enddo
   !
   call repoz(nfp)
   ifpss=0
   100 if(nfp.gt.0) then
     read(nfp,'(66x,i4,i2,i3)') math,mfh,mth
   else
     read(-nfp) math,mfh,mth
   endif
   if(math.eq.0) go to 100
   if((mfh.ne.0).and.(mth.ne.0)) call skiprz(nfp,-1)
   ifiss=0
   do
     150 call contio(nfp,0,0,scr,nb,nw)
     if(math.eq.-1) then
       go to 300
     else if(mfh.ne.1) then
       call tofend(nfp,0,0,scr)
       go to 150
     else if(mth.ne.451) then
       call tosend(nfp,0,0,scr)
       go to 150
     endif
     ifiss=ifiss+1
      if(ifiss.gt.nbfiss) call error('draevo','nbfiss overflow',' ')
     za=c1h
     awr=c2h
     call contio(nfp,0,0,scr,nb,nw)
     izae=10*nint(za+0.1)+l2h
     if(izae.eq.0) call error('draevo','izae error(1)',' ')
     call draind(nbiso,izae,mylist,ind)
     if(ind.le.0) then
       call dranam(izae,hname)
       call error('draevo','missing isotope(1):'//hname,' ')
     endif
     idreac(2,ind)=ifiss*100+4 ! ind is producing fp
     call findf(math,8,454,nfp)
     call contio(nfp,0,0,scr,nb,nw)
     lep1=l1h
     if(lep1.gt.maxen) then
       write(hsmg,'(44hInvalid number of incident neutron energies., &
       & 18h Increase maxen to,i3,1h.)') lep1
       call error('draevo',hsmg,' ')
     endif
     summ=0.0
     do ile=1,lep1
       call listio(nfp,0,0,scr(1),nb,nw)
       nbfp=n2h
       energy=c1h
       loc=1+nw
       do while (nb.ne.0)
         if(loc+302.gt.maxa) call error('draevo','endf input size exceeded',' ')
         call moreio(nfp,0,0,scr(loc),nb,nw)
         loc=loc+nw
       enddo
       if(energy.le.2.0e6) then
         do ifp=1,nbfp
           iof=6+(ifp-1)*4+1
           if(scr(iof+2).gt.1.0e-10) then
             iz=nint(scr(iof)/1000+0.1)
             ifps=nint(scr(iof+1)+0.1)
             ia=mod(nint(scr(iof)+0.1),1000)
             jzae=10000*iz+10*ia+ifps
             if(jzae.eq.0) call error('draevo','jzae error',' ')
             call draind(nbiso,jzae,mylist,jnd)
             if(jnd.le.0) then
               call dranam(jzae,hname)
               call error('draevo','missing isotope(2):'//hname,' ')
             endif
             do i=1,ifpss
               if(indpf(i).eq.jzae) then
                 ifpp=i
                 go to 200
               endif
             enddo
             ifpss=ifpss+1
             if(ifpss.gt.nbdpf) call error('draevo','nbdpf overflow',' ')
             ifpp=ifpss
             indpf(ifpp)=jzae
             200 idreac(2,jnd)=ifpp*100+5 ! jnd is a fission fragment
             yield(ifiss,ifpp)=yield(ifiss,ifpp)+terp(ile)*scr(iof+2)
             summ=summ+terp(ile)*scr(iof+2)
           endif
         enddo
       endif
     enddo
     call tomend(nfp,0,0,scr)
   enddo
   !
   300 call repoz(ndcy)
   400 if(ndcy.gt.0) then
     read(ndcy,'(66x,i4,i2,i3)') math,mfh,mth
   else
     read(-ndcy) math,mfh,mth
   endif
   if(math.eq.0) go to 400
   if((mfh.ne.0).and.(mth.ne.0)) call skiprz(ndcy,-1)
   do
   500 call contio(ndcy,0,0,scr,nb,nw)
     if(math.eq.-1) then
       go to 700
     else if(mfh.ne.1) then
       call tofend(ndcy,0,0,scr)
       go to 500
     else if(mth.ne.451) then
       call tosend(ndcy,0,0,scr)
       go to 500
     endif
     za=c1h
     awr=c2h
     call contio(ndcy,0,0,scr,nb,nw)
     izae=10*nint(za+0.1)+l2h
     if(izae.eq.0) call error('draevo','izae error(2)',' ')
     call draind(nbiso,izae,mylist,ind)
     if(ind.le.0) then
       call dranam(izae,hname)
       call error('draevo','missing isotope(3):'//hname,' ')
     endif
     idreac(1,ind)=1 ! ind can decay
     call findf(math,8,457,ndcy)
     call contio(ndcy,0,0,scr,nb,nw)
     iza=nint(c1h+0.1)
     iz=nint(c1h/1000+0.1)
     ia=mod(iza,1000)
     call listio(ndcy,0,0,scr,nb,nw)
     if(c1h.eq.0.0) then
       ddeca(ind)=0.0
     else
       ddeca(ind)=1.0e8*log(2.0)/c1h
     endif
     dener(1,ind)=(scr(lz+1)+scr(lz+3)+scr(lz+5))*1.0e-6
     call listio(ndcy,0,0,scr(1),nb,nw)
     nbdy=n2h
     loc=1+nw
     do while (nb.ne.0)
       if(loc+302.gt.maxa) call error('draevo','endf input size exceeded',' ')
       call moreio(ndcy,0,0,scr(loc),nb,nw)
       loc=loc+nw
     enddo
     do idy=1,nbdy
       rtyp=scr(6*idy+1)
       jz=iz
       ja=ia
       if(abs(rtyp-1.0).le.0.00001) then
         ! **beta- decay
         jz=iz+1
       else if(abs(rtyp-1.4).le.0.00001) then
         ! **beta- decay followed by alpha emission
         jz=iz-1
         ja=ia-4
       else if(abs(rtyp-1.5).le.0.00001) then
         ! **beta- decay followed by neutron emission (delayed neutron)
         jz=iz+1
         ja=ia-1
       else if(abs(rtyp-2.0).le.0.00001) then
         ! **beta+ decay or electron capture
         jz=iz-1
       else if(abs(rtyp-2.4).le.0.00001) then
         ! **beta+ decay followed by alpha emission
         jz=iz-3
         ja=ia-4
       else if(abs(rtyp-2.7).le.0.00001) then
         ! **beta+ decay followed by proton emission
         jz=iz-2
         ja=ia-1
       else if(abs(rtyp-3.0).le.0.00001) then
         ! **isomeric transition
       else if(abs(rtyp-3.4).le.0.00001) then
         ! **isomeric transition followed by alpha decay
         jz=iz-2
         ja=ia-4
       else if(abs(rtyp-3.5).le.0.00001) then
         ! **isomeric transition followed by neutron emission (delayed neutron)
         ja=ia-1
       else if(abs(rtyp-4.0).le.0.00001) then
         ! **alpha decay
         jz=iz-2
         ja=ia-4
       else if(abs(rtyp-5.0).le.0.00001) then
         ! **neutron emission (not delayed neutron)
         ja=ia-1
       else if(abs(rtyp-6.0).le.0.00001) then
         ! **spontaneous fission
         jz=0
       else if(abs(rtyp-7.0).le.0.00001) then
         ! **proton emission
         jz=iz-1
         ja=ia-1
       else if(abs(rtyp-10.0).le.0.00001) then
         ! **unknown origin
         jz=0
       else
         call dranam(izae,hname)
         write(text4,'(f4.2)') rtyp
         write(nsyso,'(A)')'unknown type of decay: '//text4//' for '//hname
         jz=0
       endif
       if(jz.ne.0) then
         izae=10000*jz+10*ja+nint(scr(6*idy+2)+0.1)
         call draind(nbiso,izae,mylist,jnd)
         if(jnd.gt.0) then
           ifath=0
           do i=1,nfath
             if(ipreac(i,jnd).eq.0) then
               ifath=i
               go to 600
             endif
           enddo
           call error('draevo','nfath overflow',' ')
           600 ipreac(ifath,jnd)=ind*100+1 ! decay father identification
           prate(ifath,jnd)=scr(6*idy+5)
         endif
       endif
     enddo
     call tomend(ndcy,0,0,scr)
   enddo
   700 deallocate(scr,indpf)
   return
   end subroutine draevo
   !
   subroutine draind(nb,izae,mylist,ind)
   !-----------------------------------------------------------------
   !   find the position ind of material izae in array mylist.
   !-----------------------------------------------------------------
   integer nb,izae,mylist(nb),ind,i
   do i=1,nb
     ind=i
     if(mylist(i).eq.izae) return
   enddo
   ind=-1
   return
   end subroutine draind
   !
   subroutine dralum(maxfp,nbiso,nbfiss,nbdpf,nreac,nfath,mylist, &
   & hiso,nfp,ndcy,nbch,nbfpch,hich,hrch,en,br,idreac,dener,ddeca, &
   & ipreac,prate,yield)
   !-----------------------------------------------------------------
   !   complete and lump the burnup chain from nbiso to nbch isotopes.
   !   write the lumped chain on the xsm file.
   !-----------------------------------------------------------------
   use mainio ! provides nsysi,contio,nsyso,nsyse
   use util   ! provides error
   integer :: maxrea,nstate,maxit,maxfat
   parameter (maxrea=13,nstate=40,maxit=20,maxfat=350)
   integer maxfp,nbiso,nbfiss,nbdpf,nreac,nfath,nfp,ndcy,nbch,nbfpch,ireac, &
   & iter,iso,isoo,i,j,ia,ja,ibfp,ida,ifa,ifath,ifi,ifp,ifps,iii,im,ind,ipgar, &
   & iz,j0,jfath,jfp,jnd,jnd1,jnd2,jso,jz,k,knd,kreac,kso,kt,nbheav,nlup,nn, &
   & nlump
   real(kr) prgar,ymax
   real(kr) en(nfath,nbch),br(nfath,nbch)
   character hrch(nfath,nbch)*8,hich(nbch)*8,hname*8,text4*4
   integer mylist(nbiso),idreac(nreac,nbiso),ipreac(nfath,nbiso),istate(nstate)
   character(len=4) hiso(3,nbiso),hreac(2,maxrea),in(2)
   real(kr) half,dener(nreac,nbiso),ddeca(nbiso),prate(nfath,nbiso), &
   & yield(nbfiss,maxfp)
   integer, allocatable, dimension(:,:) :: jpreac,jdreac,ipos
   character(len=4), allocatable, dimension(:,:) :: hhhh
   real, allocatable, dimension(:) :: dddd
   real, allocatable, dimension(:,:) :: rrate,eener,eyiel
   character(len=8), save, dimension(maxrea) :: reac= &
   & (/ 'DECAY   ','NFTOT   ','NG      ','N2N     ','N3N     ','N4N     ', &
   &    'NA      ','NP      ','N2A     ','NNP     ','ND      ','NT      ', &
   &    'NODECAY ' /)
   !
   if(nreac.ne.maxrea) call error('dralum','maxrea overflow',' ')
   allocate(jpreac(maxfat,nbch),jdreac(nreac-1,nbch),ipos(nbch,2),hhhh(3,nbch))
   allocate(rrate(maxfat,nbch),eener(nreac-1,nbch),eyiel(nbfiss,nbfpch), &
   & dddd(nbch))
   !
   ! **find the position of the lumped isotopes in the complete chain
   do iso=1,nbch
     in(1)=hich(iso)(1:4)
     in(2)=hich(iso)(5:8)
     do jso=1,nbiso
       j0=jso
       if((in(1).eq.hiso(1,jso)).and.(in(2).eq.hiso(2,jso))) go to 10
     enddo
     call error('dralum','unable to find '//hich(iso),' ')
     10 ipos(iso,1)=j0
   enddo
   !
   ! **complete idreac, dener, ipreac and prate.
   do iso=1,nbch
     ind=ipos(iso,1)
     iz=mylist(ind)/10000
     ia=mod(mylist(ind),10000)/10
     ifps=mod(mylist(ind),10)
     do i=1,nfath
       if(hrch(i,iso).eq.' ') go to 30
       jz=iz
       if(hrch(i,iso).eq.'nftot') then
         jz=0
         if(idreac(2,ind).eq.0) idreac(2,ind)=3
         ireac=2
         !
       else if(hrch(i,iso).eq.'NODECAY') then
         jz=0
         if(idreac(2,ind).eq.0) idreac(2,ind)=3
         ireac=13
         !
       else if(hrch(i,iso).eq.'ng') then
         ja=ia+1
         idreac(3,ind)=1
         ireac=3
       else if(hrch(i,iso).eq.'n2n') then
         ja=ia-1
         idreac(4,ind)=1
         ireac=4
       else if(hrch(i,iso).eq.'n3n') then
         ja=ia-2
         idreac(5,ind)=1
         ireac=5
       else if(hrch(i,iso).eq.'n4n') then
         ja=ia-3
         idreac(6,ind)=1
         ireac=6
       else if(hrch(i,iso).eq.'na') then
         jz=iz-2
         ja=ia-3
         idreac(7,ind)=1
         ireac=7
       else if(hrch(i,iso).eq.'np') then
         jz=iz-1
         ja=ia
         idreac(8,ind)=1
         ireac=8
       else if(hrch(i,iso).eq.'n2a') then
         jz=iz-4
         ja=ia-7
         idreac(9,ind)=1
         ireac=9
       else if(hrch(i,iso).eq.'nnp') then
         jz=iz-1
         ja=ia-1
         idreac(10,ind)=1
         ireac=10
       else if(hrch(i,iso).eq.'nd') then
         jz=iz-1
         ja=ia-1
         idreac(11,ind)=1
         ireac=11
       else if(hrch(i,iso).eq.'nt') then
         jz=iz-1
         ja=ia-2
         idreac(12,ind)=1
         ireac=12
       endif
       dener(ireac,ind)=en(i,iso)
       if(jz.ne.0) then
         call draind(nbiso,10000*jz+10*ja,mylist,jnd)
         if(jnd.gt.0) then
           do ifath=1,nfath
             if(ipreac(ifath,jnd).eq.0) then
               ipreac(ifath,jnd)=ind*100+ireac
               prate(ifath,jnd)=1.0-br(i,iso)
               go to 15
             endif
           enddo
           call error('dralum','nfath overflow-1',' ')
         endif
       endif
       15 if((jz.ne.0).and.(br(i,iso).ne.0.0)) then
         ! *** neutron induced production of an isomer
         call draind(nbiso,10000*jz+10*ja+1,mylist,jnd)
         if(jnd.gt.0) then
           do ifath=1,nfath
             if(ipreac(ifath,jnd).eq.0) then
               ipreac(ifath,jnd)=ind*100+ireac
               prate(ifath,jnd)=br(i,iso)
               go to 20
             endif
           enddo
           call error('dralum','nfath overflow-2',' ')
         endif
       endif
       20 continue
     enddo
     30 continue
   enddo
   !
   ! *lump idreac, dener, ipreac and prate.
   iter=0
   40 iter=iter+1
   if(iter.gt.maxit) call error('dralum','too many iterations',' ')
   nlump=0
   do iso=1,nbch
     ind=ipos(iso,1)
     do ifath=1,nfath
       if(ipreac(ifath,ind).eq.0) go to 60
       if(mod(ipreac(ifath,ind),100).ne.1) go to 50
       jnd=ipreac(ifath,ind)/100
       if(mylist(jnd).eq.0) go to 50
       do j=1,nbch
         if(ipos(j,1).eq.jnd) go to 50
       enddo
       nlump=nlump+1 ! isotope jnd is lumped
       do ida=1,nbiso
         do ifa=1,nfath
           ipgar=ipreac(ifa,ida)
           if((ipgar/100.eq.jnd).and.(mod(ipgar,100).eq.1)) then
             if(mylist(ida).eq.0) go to 50
           endif
         enddo
       enddo
       do ida=1,nbiso
         do ifa=1,nfath
           ipgar=ipreac(ifa,ida)
           if((ipgar/100.eq.jnd).and.(mod(ipgar,100).eq.1)) then
             if(ida.eq.jnd) call error('dralum','bug',' ')
             prgar=prate(ifa,ida)
             do im=ifa,nfath-1
               ipreac(im,ida)=ipreac(im+1,ida)
               prate(im,ida)=prate(im+1,ida)
             enddo
             ipreac(nfath,ida)=0
             prate(nfath,ida)=0.0
             do jfath=1,nfath
               if(ipreac(jfath,jnd).eq.0) go to 45
               im=nfath+1
               do k=nfath,1,-1
                 if(ipreac(k,ida).eq.ipreac(jfath,jnd)) then
                   prate(k,ida)=prate(k,ida)+prgar*prate(jfath,jnd)
                   go to 44
                 endif
                 if(ipreac(k,ida).eq.0) im=k
               enddo
               if(im.gt.nfath) then
                 call error('dralum','nfath overflow',' ')
               endif
               ipreac(im,ida)=ipreac(jfath,jnd)
               prate(im,ida)=prgar*prate(jfath,jnd)
               44 continue
             enddo
             45 if(mod(idreac(2,jnd),100).eq.5) then
               jfp=idreac(2,jnd)/100
               if(mod(idreac(2,ida),100).eq.5) then
                 ifp=idreac(2,ida)/100
               else
                 nbdpf=nbdpf+1
                 if(nbdpf.gt.maxfp) then
                   call error('dralum','maxfp overflow',' ')
                 endif
                 ifp=nbdpf
                 do ifi=1,nbfiss
                   yield(ifi,ifp)=0.0
                 enddo
               endif
               do ifi=1,nbfiss
                 yield(ifi,ifp)=yield(ifi,ifp)+yield(ifi,jfp)*prgar
               enddo
               idreac(2,ida)=ifp*100+5
             endif
           endif
         enddo
       enddo
       do jfath=1,nfath
         if(ipreac(jfath,jnd).gt.0) then
           kt=mod(ipreac(jfath,jnd),100)
           knd=ipreac(jfath,jnd)/100
           dener(kt,knd)=dener(kt,knd)+prate(jfath,jnd)*dener(1,jnd)
         endif
         ipreac(jfath,jnd)=0
         prate(jfath,jnd)=0.0
       enddo
       ymax=0.0
       if(mod(idreac(2,jnd),100).eq.5) then
         jfp=idreac(2,jnd)/100
         do kso=1,nbiso
           if(mod(idreac(2,kso),100).eq.4) then
             ifi=idreac(2,kso)/100
             dener(2,kso)=dener(2,kso)+yield(ifi,jfp)*dener(1,jnd)
           endif
         enddo
         do ifi=1,nbfiss
           ymax=max(ymax,abs(yield(ifi,jfp)))
           yield(ifi,jfp)=0.0
         enddo
         dener(2,jnd)=0.0
         idreac(2,jnd)=0
       endif
       dener(1,jnd)=0.0
       idreac(1,jnd)=0
       half=1.0e8*log(2.0)/ddeca(jnd)/86400.0
       if(ddeca(jnd).eq.0.0) then
         call dranam(mylist(jnd),hname)
         write(nsyso,'('' warning: isotope '',a8,'' is lumped and'', &
         & '' is stable. Max fission yield='',1p,e8.1,''%'')') hname,ymax*100.0
         if(ymax.gt.1.0E-2) call error('dralum','isotope '//hname// &
         & ' should not be lumped',' ')
       else if((half.gt.30.0).and.(half.lt.999999.99)) then
         call dranam(mylist(jnd),hname)
         write(nsyso,'('' warning: isotope '',a8,'' is lumped and'', &
         & '' has a half-life of'',f10.2,'' days. Max fission yield='', &
         & 1p,e8.1,''%'')') hname,half,ymax*100.0
         if(ymax.gt.1.0E-2) call error('dralum','isotope '//hname// &
         & ' should not be lumped',' ')
       else if(half.gt.30.0) then
         call dranam(mylist(jnd),hname)
         write(nsyso,'('' warning: isotope '',a8,'' is lumped and'', &
         & '' has a half-life of'',1p,e10.3,'' days. Max fission yi'', &
         & ''eld='',e8.1,''%'')') hname,half,ymax*100.0
         if(ymax.gt.1.0E-2) call error('dralum','isotope '//hname// &
         & ' should not be lumped',' ')
       endif
       ddeca(jnd)=0.0
       mylist(jnd)=0
       50 continue
     enddo
     60 continue
   enddo
   write(nsyse,'('' ......... nlump='',i5)') nlump
   write(nsyso,'('' ......... nlump='',i5)') nlump
   if(nlump.gt.0) go to 40
   !
   ! *write vectors 'PRODUCE-REAC' and 'PRODUCE-RATE' to the xsm file
   do iso=1,nbch
     do ifath=1,maxfat
       jpreac(ifath,iso)=0
       rrate(ifath,iso)=0.0
     enddo
     ind=ipos(iso,1)
     nn=0
     do ifath=1,nfath
       if(ipreac(ifath,ind).ne.0) then
         do j=1,ifath-1
           if(ipreac(ifath,ind).eq.ipreac(j,ind)) then
             jnd1=ipreac(ifath,ind)/100
             jnd2=ipreac(j,ind)/100
             write(nsyso,'(/27h dralum: duplicate fathers:,2a4, &
             & 1x,2a4)') hiso(1,jnd1),hiso(2,jnd1),hiso(1,jnd2),hiso(2,jnd2)
             write(hname,'(2a4)') hiso(1,ind),hiso(2,ind)
             call error('dralum','duplicate fathers for '//hname,' ')
           endif
         enddo
         call draind(nbch,ipreac(ifath,ind)/100,ipos(1,1),jso)
         if(jso.le.0) then
           jnd=ipreac(ifath,ind)/100
           write(nsyso,'(/24h dralum: unknown father ,2a4,5h for , &
           & 2a4)') hiso(1,jnd),hiso(2,jnd),hiso(1,ind),hiso(2,ind)
         else
           nn=nn+1
           if(nn.gt.maxfat) then
             write(text4,'(i4)') nn
             call error('dralum','maxfat overflow nn='//text4,' ')
           endif
           jpreac(nn,iso)=100*jso+mod(ipreac(ifath,ind),100)
           rrate(nn,iso)=prate(ifath,ind)
         endif
       endif
     enddo
   enddo
   call xsmput(draglib,'PRODUCE-REAC',reshape(jpreac,(/ maxfat*nbch /)))
   call xsmput(draglib,'PRODUCE-RATE',reshape(rrate,(/ maxfat*nbch /)))
   !
   ! *write the isotope ascii names and header information on xsm file
   do i=1,nreac-1
     hreac(1,i)=reac(i)(1:4)
     hreac(2,i)=reac(i)(5:8)
   enddo
   call xsmput(draglib,'DEPLETE-IDEN',reshape(hreac,(/ 2*(nreac-1) /)))
   !
   ! *write the lumped fission yield matrix to the xsm file
   ibfp=0
   do iso=1,nbch
     ind=ipos(iso,1)
     ipos(iso,2)=0
     if(mod(idreac(2,ind),100).eq.5) then
       ibfp=ibfp+1
       if(ibfp.gt.nbfpch) call error('dralum','nbfpch overflow',' ')
       ipos(iso,2)=ibfp
       do ifi=1,nbfiss
         eyiel(ifi,ibfp)=yield(ifi,idreac(2,ind)/100)
       enddo
     endif
   enddo
   if(ibfp.gt.0) then
     call xsmput(draglib,'FISSIONYIELD',reshape(eyiel,(/ nbfiss*ibfp /)))
   endif
   !
   ! *write vectors 'DEPLETE-REAC' and 'DEPLETE-ENER' to the xsm file
   do iso=1,nbch
     ind=ipos(iso,1)
     do i=1,nreac-1
       if(idreac(i,ind)/100.gt.0) then
         kreac=mod(idreac(i,ind),100)
         if((kreac.le.0).or.(kreac.gt.5)) then
           call error('dralum','invalid reaction',' ')
         endif
       endif
       if((i.eq.2).and.(mod(idreac(i,ind),100).eq.5)) then
         jdreac(i,iso)=ipos(iso,2)*100+5
       else
         jdreac(i,iso)=idreac(i,ind)
       endif
       eener(i,iso)=dener(i,ind)
     enddo
     if(dener(13,ind).ne.0.0) then
        do isoo=1,nbch
           eener(1,isoo)=0.0
           do iii=3,nreac-1
              eener(iii,isoo)=0.0
           enddo
          enddo
        eener(2,iso)=dener(13,ind)
     endif
   enddo
   call xsmput(draglib,'DEPLETE-REAC',reshape(jdreac,(/ (nreac-1)*nbch /)))
   call xsmput(draglib,'DEPLETE-ENER',reshape(eener,(/ (nreac-1)*nbch /)))
   !
   ! *write vectors 'CHARGEWEIGHT', 'DEPLETE-DECA', 'ISOTOPESDEPL'
   ! and 'STATE-VECTOR' to the xsm file
   nbheav=0
   do iso=1,nbch
     hhhh(1,iso)=hiso(1,ipos(iso,1))
     hhhh(2,iso)=hiso(2,ipos(iso,1))
     hhhh(3,iso)=hiso(3,ipos(iso,1))
     dddd(iso)=ddeca(ipos(iso,1))
     ipos(iso,1)=mylist(ipos(iso,1))
     if(ipos(iso,1).ge.900000) nbheav=nbheav+1
   enddo
   call xsmput(draglib,'ISOTOPESDEPL',reshape(hhhh,(/ 3*nbch /)))
   call xsmput(draglib,'CHARGEWEIGHT',ipos(1:nbch,1))
   call xsmput(draglib,'DEPLETE-DECA',dddd)
   do i=1,nstate
     istate(i)=0
   enddo
   istate(1)=nbch
   istate(2)=nbfiss
   istate(3)=ibfp
   istate(4)=nbheav
   istate(5)=nbch-nbheav
   istate(8)=nreac-1
   istate(9)=maxfat
   call xsmput(draglib,'STATE-VECTOR',istate)
   deallocate(dddd,eyiel,eener,rrate)
   deallocate(hhhh,ipos,jdreac,jpreac)
   return
   end subroutine dralum
   !
   subroutine dranam(izae,hname)
   !-----------------------------------------------------------------
   !   utility to compose a material ascii (character*8) name or to
   !   recover the izae for an existing name.
   !-----------------------------------------------------------------
   use util   ! provides error
   integer izae,i,ia,iz,ifps,iof1,iof2,nia,niz
   character hname*8,hformat*12
   character(len=2), save, dimension(0:111) :: cs= &
    (/    'n ','H ','He','Li','Be','B ','C ','N ','O ','F ','Ne', &
   & 'Na','Mg','Al','Si','P ','S ','Cl','Ar','K ','Ca','Sc','Ti', &
   & 'V ','Cr','Mn','Fe','Co','Ni','Cu','Zn','Ga','Ge','As','Se', &
   & 'Br','Kr','Rb','Sr','Y ','Zr','Nb','Mo','Tc','Ru','Rh','Pd', &
   & 'Ag','Cd','In','Sn','Sb','Te','I ','Xe','Cs','Ba','La','Ce', &
   & 'Pr','Nd','Pm','Sm','Eu','Gd','Tb','Dy','Ho','Er','Tm','Yb', &
   & 'Lu','Hf','Ta','W ','Re','Os','Ir','Pt','Au','Hg','Tl','Pb', &
   & 'Bi','Po','At','Rn','Fr','Ra','Ac','Th','Pa','U ','Np','Pu', &
   & 'Am','Cm','Bk','Cf','Es','Fm','Md','No','Lr','Rf','Db','Sg', &
   & 'Bh','Hs','Mt','Ds','Rg' /)
   character(len=2), save, dimension(0:3) :: cm= (/ '  ','m ','m2','m3' /)
   !
   if(izae.eq.0) then
     ! **compute the izae index from name
     iz=0
     ifps=0
     do i=1,111
       if(hname(:2).eq.cs(i)) then
         iz=i
         iof1=3
         go to 10
       endif
     enddo
     do i=1,111
       if((hname(:1).eq.cs(i)(:1)).and.(cs(i)(2:).eq.' ')) then
         iz=i
         iof1=2
         go to 10
       endif
     enddo
     call error('dranam','invalid name:'//hname,' ')
     10 iof2=index(hname(iof1:),'m')
     if(iof2.gt.0) then
       do i=1,3
         if(hname(iof1+iof2-1:).eq.cm(i)) then
           ifps=i
           go to 20
         endif
       enddo
       call error('dranam','invalid suffix:'//hname,' ')
     else
       iof2=index(hname(iof1:),' ')
     endif
     20   read(hname(iof1:iof1+iof2-2),'(i3)') ia
     izae=iz*10000+ia*10+ifps
   else
     ! **compute the name from izae index
     iz=izae/10000
     ia=mod(izae,10000)/10
     ifps=mod(izae,10)
     if(iz.gt.111) call error('dranam','cs overflow',' ')
     if(ifps.gt.3) call error('dranam','cm overflow',' ')
     niz=2
     if(cs(iz)(2:).eq.' ') niz=1
     nia=3
     if(ia.lt.100) nia=2
     if(ia.lt.10) nia=1
     write(hformat,'(''(a'',i1,'',i'',i1,'',a2)'')') niz,nia
     write(hname,hformat) cs(iz)(:niz),ia,cm(ifps)
   endif
   return
   end subroutine dranam

end module dragm
