module electm
   ! provides subroutine electr for NJOY2012
   use locale
   implicit none
   private
   public electr

   ! global variables
   integer::ige,igg,nge,ngg
   integer,parameter::maxntr=200
   integer,parameter::ngmax=400
   real(kr)::ege(ngmax),egg(ngmax),egem(ngmax-1),eggm(ngmax-1)
   real(kr)::znow
   integer::iwt
   integer,parameter::iwmax=200
   real(kr)::wght(iwmax)
   integer::matb,lord
   integer::nendf,npend,nstop,nacti
   integer::nbet1,nbet2,ntw
   real(kr)::rtitle(17)
   character(4)::title(17)
   equivalence(rtitle(1),title(1))
   integer::matd,mfd,mtd
   integer::iprint

   ! relaxation information
   integer,parameter::maxndg=5
   integer::ndg,ntr,iaup(maxndg,maxntr),iflp(maxndg,maxntr)
   real(kr)::be(maxndg),eff1(maxndg,maxntr),eff(maxndg,maxntr), &
   efn(maxndg,maxntr)

   ! stopping power information
   real(kr),dimension(:),allocatable::eestop,ebstop,eemtop,ebmtop

   ! temporary arrays for reaction data
   integer::nk
   real(kr),dimension(:),allocatable::sedist
   real(kr),dimension(:,:),allocatable::sede

contains

   subroutine electr
   !------------------------------------------------------------------
   !
   ! compute multigroup electron cross sections
   !
   ! Produce multigroup electron interaction cross sections, energy
   ! and charge deposition factors using ENDF cross sections and
   ! stopping powers.  Initial energy quadrature techiques are
   ! identical to those used in groupr. Secondary energy-angle
   ! quadrature is performed using Gaussian integration.
   !
   ! Author: Alain Hebert (adapted to Njoy2012 in 2021)
   !
   !---input specifications (free format)---------------------------
   !
   ! card1
   !    nendf   unit for endf tape
   !    npend   unit for pendf tape
   !    nstop   unit for output stopping power tape in endf-102 format
   !    nacti   unit for atomic relaxation tape
   !    nbet1   unit for input nbet tape (default=0)
   !    nbet2   unit for output nbet tape (default=0)
   ! card2
   !    matb    material to be processed
   !            input materials in ascending order
   !    ige     electron group structure option
   !    igg     gamma group structure option (=0 for single set)
   !    iwt     weight function option
   !    lord    legendre order
   !    iprint  print option (0/1=minimum/maximum) (default=1)
   ! card3
   !    title   run label up to 80 characters (delimited by ',
   !            ended with /)
   ! card4      (ige=-2 or -1 only)
   !    nge     number of electron groups
   !    emin    minimum energy bound (ev)
   !    emax    maximum energy bound (ev)
   ! card4      (ige=1 only)
   !    nge     number of electron groups
   !    ege     nge+1 group bounds (ev)
   ! card5      (igg=-2 or -1 only)
   !    ngg     number of gamma groups
   !    gmin    minimum energy bound (ev)
   !    gmax    maximum energy bound (ev)
   ! card5      (igg=1 only)
   !    ngg     number of gamma groups
   !    egg     ngg+1 group bounds (ev)
   ! card6      (iwt=1 only)
   !    wght    weight function as tab1 record
   ! card7
   !    mfd     file to be processed
   !    mtd     section to be processed
   !    mtname  description of section to be processed
   !            repeat for all reactions desired
   !            mfd=0/ terminates this material
   !            mfd=-1/ is a flag to process all sections present
   !            for this material  (termination is automatic)
   ! card8
   !    matd    next mat number to be processed
   !            terminate electr run with matd=0.
   !
   !---options for input variables----------------------------------
   !
   !        ige/igg meaning
   !        ---     -------
   !        -2      uniform logarithmic group structure
   !        -1      uniform linear group structure
   !         0      none
   !         1      arbitrary structure (read in)
   !         2      csewg 94-group structure
   !         3      lanl 12-group structure
   !         4      steiner 21-group gamma-ray structure
   !         5      straker 22-group structure
   !         6      lanl 48-group structure
   !         7      lanl 24-group structure
   !         8      vitamin-c 36-group structure
   !         9      vitamin-e 38-group structure
   !         10     vitamin-j 42-group structure
   !
   !        iwt     meaning
   !        ---     -------
   !         1      read in
   !         2      constant
   !         3      1/e + rolloffs
   !
   !------------------------------------------------------------------
   use mainio ! provides nsysi,nsyso,nsyse
   use endf   ! provides endf routines and variables
   use util   ! provides timer,repoz,skiprz,closz
   integer::nw,nb,nwds,nwst,ngep1,nggp1,np,loc,nleft,icnt,mflg,nl,ngg2
   integer::idis,ng2,iglo,nq,ig1,ig,ig2lo,idiscf
   integer::idone,igzero,j,ibase,i,lim
   integer::ii,jj,idg,itr,ma,ind
   real(kr)::time,za,awr,e,en,elo,ehi,enext,slst,slt,slt1,slt2
   real(kr)::thresh
   character(4)::mtname(15)
   real(kr)::z(10,10)
   character(66)::text
   character(4)::tt(17)
   real(kr)::rt(17)
   equivalence(tt(1),rt(1))
   integer,parameter::ncnt=46
   integer,dimension(ncnt),parameter::mflst=(/&
     23,23,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,&
     26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,&
     26,26,26,23,23,23/)
   integer,dimension(ncnt),parameter::mtlst=(/&
     507,508,525,527,534,535,536,537,538,539,540,541,542,543,544,545,&
     546,547,548,549,550,551,552,553,554,555,556,557,558,559,560,561,&
     562,563,564,565,566,567,568,569,570,571,572,501,530,531/)
   character(4),dimension(ncnt)::nmlst=(/&
     'estc','estr','elas','brem','shk1','shl1','shl2','shl3','shm1',&
     'shm2','shm3','shm4','shm5','shn1','shn2','shn3','shn4','shn5',&
     'shn6','shn7','sho1','sho2','sho3','sho4','sho5','sho6','sho7',&
     'sho8','sho9','shp1','shp2','shp3','shp4','shp5','shp6','shp7',&
     'shp8','shp9','sp10','sp11','shq1','shq2','shq3','totl','heat',&
     'char'/)
   real(kr),dimension(:),allocatable::scr
   real(kr),dimension(:),allocatable::sdat
   real(kr),dimension(:,:),allocatable::totin,toth,totc
   real(kr),dimension(:,:),allocatable::ff
   real(kr),dimension(:,:,:),allocatable::ans
   real(kr),parameter::emax=1.e12_kr
   real(kr),parameter::epair=.5110034e6_kr
   real(kr),parameter::zero=0
   real(kr),parameter::small=1.e-9_kr
   integer::nz=1

   !--write heading and read user input
   call timer(time)
   write(nsyso,'(/&
     &'' electr...produce photon interaction cross sections'',&
     &18x,f8.1,''s'')') time
   write(nsyse,'(/'' electr...'',59x,f8.1,''s'')') time
   call ruine

   !--allocate storage arrays
   nw=npage+50
   allocate(scr(nw))
   allocate(sdat(nw))
   allocate(totin(2,nge)) ! total cross section array
   allocate(toth(2,nge))  ! energy deposition array
   allocate(totc(2,nge))  ! charge deposition array
   allocate(eestop(nge+1),ebstop(nge+1),eemtop(nge),ebmtop(nge))
   znow=0

   !--determine endf format being used
   call repoz(nendf)
   call tpidio(nendf,0,0,scr,nb,nw)
   call contio(nendf,0,0,scr,nb,nw)
   call contio(nendf,0,0,scr,nb,nw)
   if (n1h.ne.0) then
      iverf=4
   else if (n2h.eq.0) then
      iverf=5
   else
      iverf=6
   endif
   write(nsyso,'(/'' using endf-'',i1,'' format'')') iverf
   call repoz(nendf)
   call repoz(npend)
   call repoz(nstop)
   call tpidio(nendf,0,0,scr,nb,nw)
   call tpidio(npend,0,0,scr,nb,nwds)
   call tpidio(nstop,0,0,scr,nb,nwst)
   scr(:17)=rtitle(:17)

   !--locate position for new material on old nbet tape.
   call repoz(nbet1)
   call repoz(nbet2)
   nsh=0
   if (nbet1.eq.0) call tpidio(0,nbet2,0,scr,nb,nwds)
   if (nbet1.eq.0.or.nbet2.eq.0) go to 150
   call tpidio(nbet1,nbet2,0,scr,nb,nwds)
  110 continue
   call contio(nbet1,0,0,scr,nb,nw)
   if (math.eq.-1) go to 140
   if (math.eq.1) go to 110
   if (math.eq.matb) go to 130
   if (math.gt.matb) go to 140
   call contio(0,nbet2,0,scr,nb,nw)
   call tomend(nbet1,nbet2,0,scr)
   go to 110
  130 continue
   call tomend(nbet1,0,0,scr)
   go to 150
  140 continue
   call skiprz(nbet1,-1)
  150 continue

   !--search for desired material on pendf tape
  160 continue
   call findf(matb,1,451,npend)
   call contio(npend,0,0,scr,nb,nw)
   za=c1h
   awr=c2h
   if (iverf.ge.5) call contio(npend,0,0,scr,nb,nw)
   if (iverf.ge.6) call contio(npend,0,0,scr,nb,nw)
   call hdatio(npend,0,0,scr,nb,nw)
   do i=1,17
      rt(i)=scr(6+i)
   enddo
   write(nsyso,'(/&
     &'' processing pendf mat '',i4/1x,66(''-'')/1x,17a4)')&
     matb,(tt(i),i=1,17)

   !--search for desired material on stopping power tape
   call findf(matb,1,451,nstop)
   call contio(nstop,0,0,scr,nb,nwst)
   za=c1h
   awr=c2h
   if (iverf.ge.5) call contio(nstop,0,0,scr,nb,nwst)
   if (iverf.ge.6) call contio(nstop,0,0,scr,nb,nwst)
   call hdatio(nstop,0,0,scr,nb,nwst)
   do i=1,17
      rt(i)=scr(6+i)
   enddo
   write(nsyso,'(/&
     &'' processing stopping power mat '',i4/1x,66(''-'')/1x,17a4)')&
     matb,(tt(i),i=1,17)

   !--recover auger and fluorescence relaxation data.
   mfd=28
   mtd=533
   call erelax(nacti,matb,mfd,mtd)
   call repoz(nacti)
   call closz(nacti)
   do idg = 1,ndg
      do itr = 1,ntr
         !
         ! calculate the destination groups for all fluorescence.
         if (ngg.gt.0) then
            if (eff(idg,itr) .ne. 0.0) then
               do ig = ngg+1,1,-1
                  if (efn(idg,itr) .ge. egg(ig)) go to 405
               enddo
               ig = 0
               405 iflp(idg,itr) = ig
               if (ig.eq.ngg+1) iflp(idg,itr) = 0
            endif
         endif
         !
         ! calculate the destination groups for all Auger.
         if (eff1(idg,itr) .ne. 0.0) then
            do ig = nge+1,1,-1
               if (efn(idg,itr) .ge. ege(ig)) go to 415
            enddo
            ig = 0
            415 iaup(idg,itr) = ig
            if (ig.eq.nge+1) iaup(idg,itr) = 0
         endif
      enddo
   enddo

   !--write head record for this material on nbet tape.
   if (nbet2.ne.0) then
      nsh=1
      math=matb
      mfh=1
      mth=451
      scr(1)=za
      scr(2)=awr
      scr(3)=0
      scr(4)=nz
      scr(5)=-1
      scr(6)=ntw
      call contio(0,nbet2,0,scr,nb,nw)
      scr(1)=0
      scr(2)=0
      scr(3)=nge
      scr(4)=ngg
      scr(5)=0
      scr(6)=0
      nw=6
      do i=1,ntw
         nw=nw+1
         tt(i)=title(i)
         scr(nw)=rt(i)
      enddo
      nw=nw+1
      scr(nw)=emax
      ngep1=nge+1
      do i=1,ngep1
         nw=nw+1
         scr(nw)=ege(i)
      enddo
      nggp1=ngg+1
      do i=1,nggp1
         nw=nw+1
         scr(nw)=egg(i)
      enddo
      if (ngg.eq.0) scr(nw)=0
      nw=nw+1
      scr(nw)=0
      np=nw-6
      scr(5)=np
      loc=1
      nw=np
      if (nw.gt.npage) nw=npage
      nw=nw+6
      call listio(0,nbet2,0,scr(loc),nb,nw)
      nleft=np-nw+6
      do while (nb.ne.0)
         loc=loc+nw
         nw=nleft
         if (nw.gt.npage) nw=npage
         call moreio(0,nbet2,0,scr(loc),nb,nw)
      enddo
      call afend(nbet2,0)
   endif

   !--main entry point for loop over reactions
   !--initialize and write appropriate heading.
   !--write head record on output tape.
   matd=matb
   totin(:2,:nge)=0.0
   toth(:2,:nge)=0.0
   totc(:2,:nge)=0.0
   icnt=0
   mflg=0
  220 continue
   if (mflg.eq.-1) go to 250
   text=' '
   read(nsysi,*) mfd,mtd,text
   mflg=mfd
   if (mflg.eq.-1) go to 250
   if (mfd.eq.0) go to 430
   read(text,'(15a4)') (mtname(i),i=1,15)
   go to 270
  250 continue
   icnt=icnt+1
   if (icnt.gt.ncnt) go to 430
   text=' '
   read(text,'(15a4)') (mtname(i),i=1,15)
   mfd=mflst(icnt)
   mtd=mtlst(icnt)
   read(nmlst(icnt),'(a4)') mtname(1)
  270 continue
   call timer(time)
   if (allocated(ans)) deallocate(ans)
   if (allocated(sedist)) deallocate(sedist)

   ! special branch for total cross section assigned to mt=501.
   if (mtd.eq.501) go to 400

   ! special branch for energy deposition assigned to mt=530.
   if (mtd.eq.530) go to 400

   ! special branch for charge deposition assigned to mt=531.
   if (mtd.eq.531) go to 400

   ! special branch for stopping powers assigned to mt=507 and 508.
   if ((mtd.eq.507).or.(mtd.eq.508)) go to 410

   ! branch for normal reactions
   ma=matd
   call findf(ma,23,mtd,npend)
   if (ma.eq.-1) then
      call repoz(npend)
      go to 250
   endif
   nl=1
   if (mfd.eq.26) nl=lord+1
   nz=1
   allocate(ans(nl,nz,nge+4))
   allocate(ff(nl,nge+3))
   e=0.0
   call etsig(e,thresh,idis,z,sdat)
   call etflx(e,en,idis,z,nl)
   call etff(e,0,en,idis,ff,nl,iglo,nq,z(1,1))
   ig1=0
   znow=nint(za/1000)

   !--process secondary electron production
   do ig=1,nge
      ng2=ig+4
      elo=ege(ig)
      ehi=ege(ig+1)
      ig2lo=0
      if (ehi.le.thresh) cycle
      enext=ehi
      ans(:nl,:nz,:nge+4)=0.0
      idone=0
      do while (idone.eq.0)
         call epanel(ig,elo,enext,ans,ff,nl,nz,ng2,ig2lo,sdat,etff)
         if (enext.eq.ehi) then
            idone=1
         else
            elo=enext
            enext=ehi
         endif
      enddo

      !--print results for this initial energy group
      ig2lo=1
      do jj=2,ig
        if (abs(ans(1,1,jj)).ge.small) exit
        ig2lo=ig2lo+1
      enddo
      do jj=2,ng2-ig2lo+1
        do ii=1,nl
          ans(ii,1,jj)=ans(ii,1,jj+ig2lo-1)
        enddo
      enddo
      if (mfd.eq.23) then
         ng2=2
      else
         ng2=ng2-ig2lo+1
      endif
      if (ig1.eq.0) then
         ig1=1
         write(nsyso,'(/'' group constants'',53x,f8.1,''s'')') time
         write(nsyso,'('' for mf'',i2,'' and mt'',i3,1x,15a4)')&
           mfd,mtd,(mtname(i),i=1,15)
         math=matb
         if (mfd.eq.23) mfh=53
         if (mfd.eq.26) mfh=56
         mth=mtd
         scr(1)=za
         scr(2)=awr
         scr(3)=nl
         scr(4)=nz
         scr(5)=0
         scr(6)=nge
         call contio(0,nbet2,0,scr,nb,nwds)
         call edspla(-1,ans,nl,nz,ng2,ig2lo,igzero)
      endif
      call edspla(ig,ans,nl,nz,ng2,ig2lo,igzero)

      !--accumulate total, and deposition cross sections.
      totin(1,ig)=ans(1,1,1)
      totin(2,ig)=totin(2,ig)+ans(1,1,ng2-2)
      toth(1,ig)=ans(1,1,1)
      toth(2,ig)=toth(2,ig)+ans(1,1,ng2-1)
      totc(1,ig)=ans(1,1,1)
      totc(2,ig)=totc(2,ig)+ans(1,1,ng2)

      !--write results on output tape
      nw=nl*nz*(ig-ig2lo+2)
      lim=nw
      if (nbet2.ne.0.and.(igzero.ne.0.or.ig.eq.nge)) then
         math=matb
         if (mfd.eq.23) mfh=53
         if (mfd.eq.26) mfh=56
         mth=mtd
         scr(1)=0
         scr(2)=0
         scr(3)=ig-ig2lo+2
         scr(4)=ig2lo
         scr(5)=nw
         scr(6)=ig
         j=6
         ibase=6
         do i=1,lim
            j=j+1
            jj=(i-1)/nl
            ii=i-nl*jj
            jj=jj+1
            scr(j)=ans(ii,1,jj)
            if (j.ge.(npage+ibase).or.i.eq.lim) then
               if (ibase.ne.0) then
                  call listio(0,nbet2,0,scr,nb,j)
                  ibase=0
                  j=0
               else
                  call moreio(0,nbet2,0,scr,nb,j)
                  j=0
               endif
            endif
         enddo
         if (ig.eq.nge) call asend(nbet2,0)
      endif
   enddo
   if (ig1.eq.0) write(nsyso,'(/&
     &'' threshold above highest energy bound for mt='',i3)') mtd
   deallocate(ff,ans)

   if (ngg.eq.0) go to 220
   if ((mfd.eq.26).and.((mtd.ge.534).and.(mtd.le.538))) nl=1
   allocate(ans(nl,nz,ngg+4),ff(nl,ngg+3))
   e=0.0
   call etsig(e,thresh,idis,z,sdat)
   call etffg(e,0,en,idis,ff,nl,iglo,nq,z(1,1))
   call etsig(e,thresh,idis,z,sdat)
   ig1=0
   !--process secondary gamma production
   if (((mtd.eq.527).and.(mfd.eq.26)).or. &
       (((mtd.ge.534).and.(mtd.le.538)).and.(mfd.eq.26))) then
      do ig=1,nge
         ngg2=ig+3
         elo=ege(ig)
         ehi=ege(ig+1)
         ig2lo=0
         enext=ehi
         ans(:nl,:nz,:ngg2+1)=0.0
         idone=0
         do while (idone.eq.0)
            call epanel(ig,elo,enext,ans,ff,nl,nz,ngg2,ig2lo,sdat,etffg)
            if (enext.eq.ehi) then
               idone=1
            else
               elo=enext
               enext=ehi
            endif
         enddo

         !--print results for this initial energy group
         ig2lo=1
         do jj=2,ig
           if (abs(ans(1,1,jj)).ge.small) exit
           ig2lo=ig2lo+1
         enddo
         do jj=2,ngg2-ig2lo+1
           do ii=1,nl
             ans(ii,1,jj)=ans(ii,1,jj+ig2lo-1)
           enddo
         enddo
         ngg2=ngg2-ig2lo+1
         if (ig1.eq.0) then
            ig1=1
            write(nsyso,'(/'' gamma production group constants'', &
            & 53x,f8.1,''s'')') time
            write(nsyso,'('' for mf'',i2,'' and mt'',i3,1x,15a4)')&
              mfd,mtd,(mtname(i),i=1,15)
            math=matb
            mfh=66 ! photon transition file number
            mth=mtd
            scr(1)=za
            scr(2)=awr
            scr(3)=nl
            scr(4)=nz
            scr(5)=0
            scr(6)=nge
            call contio(0,nbet2,0,scr,nb,nwds)
            call edspla(-1,ans,nl,nz,ngg2,ig2lo,igzero)
         endif
         call edspla(ig,ans,nl,nz,ngg2,ig2lo,igzero)

         !--accumulate deposition cross sections.
         toth(2,ig)=toth(2,ig)+ans(1,1,ngg2-1)
         totc(2,ig)=totc(2,ig)+ans(1,1,ngg2)

         !--write results on output tape
         nw=nl*nz*(ig-ig2lo+2)
         lim=nw
         call edspla(ig,ans,nl,nz,ngg2,ig2lo,igzero)
         if (nbet2.ne.0.and.(igzero.ne.0.or.ig.eq.nge)) then
            math=matb
            mfh=66 ! photon transition file number
            mth=mtd
            scr(1)=0
            scr(2)=0
            scr(3)=ig-ig2lo+2
            scr(4)=ig2lo
            scr(5)=nw
            scr(6)=ig
            j=6
            ibase=6
            do i=1,lim
               j=j+1
               jj=(i-1)/nl
               ii=i-nl*jj
               jj=jj+1
               if (jj.gt.ngg2) call error('electr','jj.gt.ngg2.',' ')
               scr(j)=ans(ii,1,jj)
               if (j.ge.(npage+ibase).or.i.eq.lim) then
                  if (ibase.ne.0) then
                     call listio(0,nbet2,0,scr,nb,j)
                     ibase=0
                     j=0
                  else
                     call moreio(0,nbet2,0,scr,nb,j)
                     j=0
                  endif
               endif
            enddo
            if (ig.eq.nge) call asend(nbet2,0)
         endif
      enddo
   endif
   znow=0.0

   !--loop over other desired reactions
   deallocate(ff,ans)
   go to 220

   !--edit out specific mt.
  400 continue
   ig=0
   ig1=0
   nl=1
   do while (ig.lt.nge)
      ig=ig+1
      ng2=2
      ig2lo=1
      allocate(ans(1,1,2))
      if (mtd.eq.501) then
         !--edit out total cross section.
         ans(1,1,1)=totin(1,ig)
         ans(1,1,2)=totin(2,ig)*totin(1,ig)
      else if (mtd.eq.530) then
         !--edit out energy deposition.
         ans(1,1,1)=toth(1,ig)
         ans(1,1,2)=max(0.0,toth(2,ig)*toth(1,ig))
      else if (mtd.eq.531) then
         !--edit out charge deposition.
         ans(1,1,1)=totc(1,ig)
         ans(1,1,2)=totc(2,ig)*totc(1,ig)
      else
         call error('electr','unknown mt.',' ')
      endif
      if (ig1.eq.0) then
         ig1=1
         write(nsyso,'(/'' group constants'',53x,f8.1,''s'')') time
         write(nsyso,'('' for mf'',i2,'' and mt'',i3,1x,15a4)')&
           mfd,mtd,(mtname(i),i=1,15)
         math=matb
         mfh=53
         mth=mtd
         scr(1)=za
         scr(2)=awr
         scr(3)=nl
         scr(4)=nz
         scr(5)=0
         scr(6)=nge
         call contio(0,nbet2,0,scr,nb,nwds)
         call edspla(-1,ans,nl,nz,ng2,ig2lo,igzero)
      endif
      call edspla(ig,ans,nl,nz,ng2,ig2lo,igzero)
      nw=nl*nz*ng2
      lim=nw
      if (nbet2.ne.0.and.(igzero.ne.0.or.ig.eq.nge)) then
         math=matb
         mfh=53
         mth=mtd
         scr(1)=0
         scr(2)=0
         scr(3)=ng2
         scr(4)=ig2lo
         scr(5)=nw
         scr(6)=ig
         scr(7)=ans(1,1,1)
         scr(8)=ans(1,1,2)
         j=8
         call listio(0,nbet2,0,scr,nb,j)
         if (ig.eq.nge) call asend(nbet2,0)
      endif
      deallocate(ans)
   enddo
   go to 220

   !--edit out stopping powers
  410 continue
   ma=matd
   call findf(ma,23,mtd,nstop)
   if (ma.eq.-1) then
      call error('electr','missing material on stopping power tape.',&
      ' ')
   endif
   e=0.0
   call findf(matd,mfd,mtd,nstop)
   call contio(nstop,0,0,sdat,nb,nw)
   call gety1(e,enext,idiscf,slt,nstop,sdat)
   if (mtd.eq.507) then
      do ig=1,nge
         call gety1(ege(ig),enext,idiscf,eestop(ig),nstop,sdat)
         call gety1(egem(ig),enext,idiscf,eemtop(ig),nstop,sdat)
      enddo
      call gety1(ege(nge+1),enext,idiscf,eestop(nge+1),nstop,sdat)
   else if (mtd.eq.508) then
       do ig=1,nge
         call gety1(ege(ig),enext,idiscf,ebstop(ig),nstop,sdat)
         call gety1(egem(ig),enext,idiscf,ebmtop(ig),nstop,sdat)
       enddo
       call gety1(ege(nge+1),enext,idiscf,ebstop(nge+1),nstop,sdat)
   endif
   allocate(ff(1,nge+4))
   call etff(e,0,en,idis,ff,1,iglo,nq,slst)
   znow=nint(za/1000)
   ind=int(znow)
   nq=10
   iglo=1
   do ig=1,nge+1
      elo=ege(ig)
      call etff(elo,ig,en,idiscf,ff,1,iglo,nq,slst)
      if (ig.eq.1) then
         slt=ff(1,2)
      else
         slt1=ff(1,2)
         call etff(elo,ig-1,en,idiscf,ff,1,iglo,nq,slst)
         slt2=ff(1,2)
         slt=0.5*(slt1+slt2)
      endif
      !--print results for this initial energy group
      if (ig.eq.1) then
         write(nsyso,'(/'' group constants'',53x,f8.1,''s'')') time
         write(nsyso,'('' for mf'',i2,'' and mt'',i3,1x,15a4)')&
           mfd,mtd,(mtname(i),i=1,15)
         math=matb
         mfh=53
         mth=mtd
         scr(1)=za
         scr(2)=awr
         scr(3)=1
         scr(4)=1
         scr(5)=0
         scr(6)=nge+1
         call contio(0,nbet2,0,scr,nb,nwds)
      endif

      !--write results on output tape
      nw=2
      if (nbet2.ne.0) then
         math=matb
         mfh=53
         mth=mtd
         scr(1)=0
         scr(2)=0
         scr(3)=2
         scr(4)=1
         scr(5)=nw
         scr(6)=ig
         scr(7)=1.0
         scr(8)=slt
         j=8
         call listio(0,nbet2,0,scr,nb,j)
         if (ig.eq.nge+1) call asend(nbet2,0)
      endif
      if (iprint.eq.1) write(nsyso,425) ig,slt
      425 format(' stopping power(',i5,')=',1p,e13.5)
   enddo
   deallocate(ff)
   go to 220

   !--loop over desired materials.
  430 continue
   call amend(nbet2,0)
   matb=0
   read(nsysi,*) matb
   if (matb.eq.0) go to 500
   if (nbet1.eq.0) go to 160
   go to 110

   !--electr is complete.
  500 continue
   if (nbet1.eq.0) call atend(nbet2,0)
   if (nbet1.ne.0) call totend(nbet1,nbet2,0,scr)
   call repoz(nbet1)
   call repoz(nbet2)
   call repoz(nendf)
   call repoz(npend)
   call repoz(nstop)
   call closz(nbet1)
   call closz(nbet2)
   call closz(nendf)
   call closz(npend)
   call closz(nstop)
   call timer(time)
   write(nsyso,'(69x,f8.1,''s''/1x,77(''*''))') time
   deallocate(totc)
   deallocate(toth)
   deallocate(totin)
   deallocate(sdat)
   deallocate(scr)
   deallocate(eestop,ebstop,eemtop,ebmtop)
   return
   end subroutine electr

   subroutine ruine
   !------------------------------------------------------------------
   ! Read user input for group averaging.
   !------------------------------------------------------------------
   use mainio ! provides nsysi,nsyso
   use util   ! provides openz
   ! internals
   integer::i
   character(66)::text

   !--read and display user input
   nbet1=0
   nbet2=0
   read(nsysi,*) nendf,npend,nstop,nacti,nbet1,nbet2
   call openz(nendf,0)
   call openz(npend,0)
   call openz(nstop,0)
   call openz(nacti,0)
   call openz(nbet1,0)
   call openz(nbet2,1)
   iprint=1
   read(nsysi,*) matb,ige,igg,iwt,lord,iprint
   read(nsysi,*) text
   read(text,'(16a4,a2)') (title(i),i=1,17)
   write(nsyso,'(/&
     &'' unit for endf tape ................... '',i10/&
     &'' unit for pendf tape .................. '',i10/&
     &'' unit for output stopping power tape .. '',i10/&
     &'' unit for atomic relaxation tape ...... '',i10/&
     &'' unit for input nbet tape ............. '',i10/&
     &'' unit for output nbet tape ............ '',i10)')&
     nendf,npend,nstop,nacti,nbet1,nbet2
   write(nsyso,'(&
     &'' mat to be processed .................. '',i10/&
     &'' electron group option ................ '',i10/&
     &'' photon group option .................. '',i10/&
     &'' weight function option ............... '',i10/&
     &'' legendre order ....................... '',i10/&
     &'' print option (0 min, 1 max) .......... '',i10)')&
     matb,ige,igg,iwt,lord,iprint
   write(nsyso,'(/'' run title''/1x,38(''-'')/&
     &6x,16a4,a2)') (title(i),i=1,17)
   write(nsyso,'('' '')')
   if (nbet2.ne.0) ntw=1
   call genegp(ige,nge,ege,egem,'electron')
   if (igg.ne.0) call genegp(igg,ngg,egg,eggm,'  photon')
   call enwtf
   return
   end subroutine ruine

   subroutine genegp(ig0,ng,eg,egm,hpart)
   !------------------------------------------------------------------
   ! Generate requested electron or photon group structure or read in
   ! from the system input file in the form of an ENDF/B list record.
   !
   !    ig0     meaning
   !    ---     --------------------------------------
   !    -2      uniform logarithmic group structure
   !    -1      uniform linear group structure
   !     0      none
   !     1      arbitrary structure (read in)
   !     2      csewg 94-group structure
   !     3      lanl 12-group structure
   !     4      steiner 21-group gamma-ray structure (ornl-tm-2564)
   !     5      straker 22-group structure
   !     6      lanl 48-group structure
   !     7      lanl 24-group structure
   !     8      vitamin-c 36-group structure
   !     9      vitamin-e 38-group structure (r. roussin, feb 86)
   !    10      vitamin-j 42-group structure
   !
   !------------------------------------------------------------------
   use mainio ! provides nsysi,nsyso
   use util   ! provides error
   integer::ig0,ng
   real(kr)::eg(ng+1),egm(ng)
   character(len=8)::hpart
   ! internals
   integer::ig,ngm,i,ngp
   real(kr)::delta,emin,emax
   real(kr),dimension(95),parameter::eg2=(/&
     .005e0_kr,.01e0_kr,.015e0_kr,.02e0_kr,.03e0_kr,.035e0_kr,&
     .04e0_kr,.045e0_kr,.055e0_kr,.06e0_kr,.065e0_kr,.075e0_kr,&
     .08e0_kr,.09e0_kr,.1e0_kr,.12e0_kr,.14e0_kr,.15e0_kr,.16e0_kr,&
     .19e0_kr,.22e0_kr,.26e0_kr,.3e0_kr,.325e0_kr,.35e0_kr,&
     .375e0_kr,.4e0_kr,.425e0_kr,.45e0_kr,.5e0_kr,.525e0_kr,&
     .55e0_kr,.575e0_kr,.6e0_kr,.625e0_kr,.65e0_kr,.675e0_kr,&
     .7e0_kr,.75e0_kr,.8e0_kr,.825e0_kr,.865e0_kr,.9e0_kr,1.e0_kr,&
     1.125e0_kr,1.2e0_kr,1.25e0_kr,1.33e0_kr,1.42e0_kr,1.5e0_kr,&
     1.6e0_kr,1.66e0_kr,1.75e0_kr,1.875e0_kr,2.e0_kr,2.166e0_kr,&
     2.333e0_kr,2.5e0_kr,2.666e0_kr,2.833e0_kr,3.e0_kr,3.166e0_kr,&
     3.333e0_kr,3.5e0_kr,3.65e0_kr,3.8e0_kr,3.9e0_kr,4.e0_kr,&
     4.2e0_kr,4.4e0_kr,4.5e0_kr,4.7e0_kr,5.e0_kr,5.2e0_kr,5.4e0_kr,&
     5.5e0_kr,5.75e0_kr,6.e0_kr,6.25e0_kr,6.5e0_kr,6.75e0_kr,&
     7.e0_kr,7.25e0_kr,7.5e0_kr,7.75e0_kr,8.e0_kr,8.5e0_kr,9.e0_kr,&
     9.5e0_kr,10.e0_kr,10.6e0_kr,11.e0_kr,12.e0_kr,14.e0_kr,20.e0_kr/)
   real(kr),dimension(13),parameter::eg3=(/&
     .01e0_kr,.10e0_kr,.50e0_kr,1.0e0_kr,2.0e0_kr,3.0e0_kr,4.0e0_kr,&
     5.0e0_kr,6.0e0_kr,7.0e0_kr,8.0e0_kr,9.0e0_kr,20.0e0_kr/)
   real(kr),dimension(22),parameter::eg4=(/&
     .01e0_kr,.1e0_kr,.2e0_kr,.4e0_kr,1.e0_kr,1.5e0_kr,2.e0_kr,&
     2.5e0_kr,3.e0_kr,3.5e0_kr,4.e0_kr,4.5e0_kr,5.e0_kr,5.5e0_kr,&
     6.e0_kr,6.5e0_kr,7.e0_kr,7.5e0_kr,8.e0_kr,10.e0_kr,12.e0_kr,&
     14.e0_kr/)
   real(kr),dimension(23),parameter::eg5=(/&
     .01e0_kr,.03e0_kr,.06e0_kr,.10e0_kr,.15e0_kr,.30e0_kr,.45e0_kr,&
     .60e0_kr,.80e0_kr,1.0e0_kr,1.33e0_kr,1.66e0_kr,2.0e0_kr,&
     2.5e0_kr,3.0e0_kr,3.5e0_kr,4.0e0_kr,5.0e0_kr,6.0e0_kr,7.0e0_kr,&
     8.0e0_kr,10.0e0_kr,14.0e0_kr/)
   real(kr),dimension(49),parameter::eg6=(/&
     .001e0_kr,.01e0_kr,.02e0_kr,.03e0_kr,.045e0_kr,.06e0_kr,&
     .08e0_kr,.1e0_kr,.15e0_kr,.2e0_kr,.3e0_kr,.4e0_kr,.45e0_kr,&
     .5e0_kr,.525e0_kr,.6e0_kr,.7e0_kr,.8e0_kr,.9e0_kr,1.e0_kr,&
     1.125e0_kr,1.2e0_kr,1.33e0_kr,1.5e0_kr,1.66e0_kr,1.875e0_kr,&
     2.e0_kr,2.333e0_kr,2.5e0_kr,2.666e0_kr,3.e0_kr,3.5e0_kr,&
     4.e0_kr,4.5e0_kr,5.e0_kr,5.5e0_kr,6.e0_kr,6.5e0_kr,7.e0_kr,&
     7.5e0_kr,8.e0_kr,9.e0_kr,10.e0_kr,12.e0_kr,14.e0_kr,17.e0_kr,&
     20.e0_kr,30.e0_kr,50.e0_kr/)
   real(kr),dimension(25),parameter::eg7=(/&
     1.e4_kr,3.e4_kr,6.e4_kr,1.e5_kr,2.e5_kr,3.e5_kr,5.e5_kr,&
     5.25e5_kr,7.5e5_kr,1.e6_kr,1.33e6_kr,1.66e6_kr,2.e6_kr,2.5e6_kr,&
     3.e6_kr,4.e6_kr,5.e6_kr,6.e6_kr,7.e6_kr,8.e6_kr,9.e6_kr,1.e7_kr,&
     1.2e7_kr,1.7e7_kr,3.e7_kr/)
   real(kr),dimension(39),parameter::eg8=(/&
     .01e0_kr,.02e0_kr,.03e0_kr,.045e0_kr,.06e0_kr,.07e0_kr,&
     .075e0_kr,.10e0_kr,.15e0_kr,.20e0_kr,.30e0_kr,.40e0_kr,.45e0_kr,&
     .510e0_kr,.512e0_kr,.60e0_kr,.70e0_kr,.80e0_kr,1.0e0_kr,&
     1.33e0_kr,1.50e0_kr,1.66e0_kr,2.0e0_kr,2.5e0_kr,3.0e0_kr,&
     3.5e0_kr,4.0e0_kr,4.5e0_kr,5.0e0_kr,5.5e0_kr,6.0e0_kr,6.5e0_kr,&
     7.0e0_kr,7.5e0_kr,8.0e0_kr,10.e0_kr,12.e0_kr,14.e0_kr,20.e0_kr/)
   real(kr),dimension(43),parameter::eg10=(/&
     1.0e3_kr,1.0e4_kr,2.0e4_kr,3.0e4_kr,4.5e4_kr,6.0e4_kr,7.0e4_kr,&
     7.5e4_kr,1.0e5_kr,1.50e5_kr,2.00e5_kr,3.00e5_kr,4.00e5_kr,&
     4.50e5_kr,5.10e5_kr,5.12e5_kr,6.00e5_kr,7.00e5_kr,8.00e5_kr,&
     1.00e6_kr,1.33e6_kr,1.34e6_kr,1.50e6_kr,1.66e6_kr,2.00e6_kr,&
     2.50e6_kr,3.00e6_kr,3.50e6_kr,4.00e6_kr,4.50e6_kr,5.00e6_kr,&
     5.50e6_kr,6.00e6_kr,6.50e6_kr,7.00e6_kr,7.50e6_kr,8.00e6_kr,&
     1.00e7_kr,1.20e7_kr,1.40e7_kr,2.00e7_kr,3.00e7_kr,5.00e7_kr/)
   real(kr),parameter::mev=1.e6_kr

   !--select structure
   ng=0
   eg(1)=0
   if (ig0.eq.0) return

   !--uniform logarithmic group structure.
   if (ig0.eq.-2) then
      read(nsysi,*) ng,emin,emax
      ngp=ng+1
      if (ngp.gt.ngmax) call error('genegp','too many groups.',' ')
      delta = log(emax/emin)/real(ng)
      eg(1) = emin
      do ig=2,ngp
         eg(ig) = exp(log(eg(ig-1)) + delta)
      enddo

   !--uniform linear group structure.
   else if (ig0.eq.-1) then
      read(nsysi,*) ng,emin,emax
      ngp=ng+1
      if (ngp.gt.ngmax) call error('genegp','too many groups.',' ')
      delta = (emax - emin)/real(ng)
      eg(1) = emin
      do ig = 2,ngp
         eg(ig) = eg(ig-1) + delta
      enddo

   !--group structure is read in.
   else if (ig0.eq.1) then

      read(nsysi,*) ng
      ngp=ng+1
      if (ngp.gt.ngmax) call error('genegp','too many groups.',' ')
      read(nsysi,*) (eg(i),i=1,ngp)

   !--csewg 94 group structure
   else if (ig0.eq.2) then
      ng=94
      do ig=1,95
         eg(ig)=eg2(ig)*mev
      enddo

   !--lanl 12 group structure
   else if (ig0.eq.3) then
      ng=12
      do ig=1,13
         eg(ig)=eg3(ig)*mev
      enddo

   !--steiner 21-group gamma structure (ornl-tm-2564)
   else if (ig0.eq.4) then
      ng=21
      do ig=1,22
         eg(ig)=eg4(ig)*mev
      enddo

   !--straker 22 group structure
   else if (ig0.eq.5) then
      ng=22
      do ig=1,23
         eg(ig)=eg5(ig)*mev
      enddo

   !--lanl 48-group structure
   else if (ig0.eq.6) then
      ng=48
      do ig=1,49
         eg(ig)=eg6(ig)*mev
      enddo

   !--lanl 24-group structure
   else if (ig0.eq.7) then
      ng=24
      do ig=1,25
         eg(ig)=eg7(ig)
      enddo

   !--vitamin-series 36- and 38-group structures
   else if (ig0.eq.8.or.ig0.eq.9) then
      ng=38
      if (ig0.eq.8) ng=36
      ngm=ng+1
      do ig=1,ngm
         eg(ig)=eg8(ig)*mev
      enddo
      if (ig0.ne.9) then
         ! remove group bounds eg8(7) and eg8(39) if ig0=8
         do ig=7,ngm
            eg(ig)=eg8(ig+1)*mev
         enddo
      endif

   !--vitamin-j 42-group structure
   else if (ig0.eq.10) then
      ng=42
      do ig=1,43
         eg(ig)=eg10(ig)
      enddo

   !--illegal value for ig0
   else
      call error('genegp','illegal group structure.',' ')
   endif

   !--display group structure
   if (ig0.eq.-2) write(nsyso,'(/1x,a8,&
     &'' logarithmic structure......read in'')') hpart
   if (ig0.eq.-1) write(nsyso,'(/1x,a8,&
     &'' linear structure......read in'')') hpart
   if (ig0.eq.1) write(nsyso,'(/1x,a8,&
     &'' group structure......read in'')') hpart
   if (ig0.eq.2) write(nsyso,'(/1x,a8,&
     &'' group structure......csewg 94 group'')') hpart
   if (ig0.eq.3) write(nsyso,'(/1x,a8,&
     &'' group structure......lanl 12 group'')') hpart
   if (ig0.eq.4) write(nsyso,'(/1x,a8,&
     &'' group structure......steiner 21-group'')') hpart
   if (ig0.eq.5) write(nsyso,'(/1x,a8,&
     &'' group structure......straker 22 group'')') hpart
   if (ig0.eq.6) write(nsyso,'(/1x,a8,&
     &'' group structure......lanl 48-group'')') hpart
   if (ig0.eq.7) write(nsyso,'(/1x,a8,&
     &'' group structure......lanl 24-group'')') hpart
   if (ig0.eq.8) write(nsyso,'(/1x,a8,&
     &'' group structure......vitamin-c 36-group'')') hpart
   if (ig0.eq.9) write(nsyso,'(/1x,a8,&
     &'' group structure......vitamin-e 38-group'')') hpart
   if (ig0.eq.10) write(nsyso,'(/1x,a8,&
     &'' group structure......vitamin-j 42-group'')') hpart
   do ig=1,ng
      write(nsyso,'(1x,i5,2x,1p,e12.5,''  - '',e12.5)')&
        ig,eg(ig),eg(ig+1)
   enddo

   !--compute averaged energy values
   do ig=1,ng
      if (ig0.eq.-2) then
         egm(ig) = exp((log(eg(ig)) + log(eg(ig+1)))/2.0d0)
      else
         egm(ig) = (eg(ig)+eg(ig+1))/2.0d0
      endif
   enddo
   return
   end subroutine genegp

   subroutine enwtf
   !------------------------------------------------------------------
   ! Set up calculation of weight functions or read in arbitary
   ! function in the form of an ENDF TAB1 record or
   ! read in parameters for an analytic weight function.
   !
   !    iwt     meaning
   !    ---     -------
   !     1      read in
   !     2      constant
   !     3      1/e + rolloffs
   !
   !------------------------------------------------------------------
   use mainio ! provides nsysi,nsyso
   use util   ! provides error
   ! internals
   integer::i
   real(kr),dimension(16),parameter::wt1=(/&
     0.e0_kr,0.e0_kr,0.e0_kr,0.e0_kr,1.e0_kr,4.e0_kr,4.e0_kr,&
     5.e0_kr,1.e3_kr,1.e-4_kr,1.e5_kr,1.e0_kr,1.e7_kr,1.e-2_kr,&
     3.e7_kr,1.e-4_kr/)

   !--branch on weight function option

   !--arbitary
   if (iwt.eq.1) then
      write(nsyso,'(/'' weight function......read in'')')
      read(nsysi,*) (wght(i),i=1,iwmax)

   !--constant
   else if (iwt.eq.2) then
      write(nsyso,'(/'' weight function......constant for all l'')')

   !--1/e with high and low energy rolloffs
   else if (iwt.eq.3) then
      write(nsyso,'(/'' weight function......1/e with rolloffs'')')
      do i=1,16
         wght(i)=wt1(i)
      enddo

   !--illegal value for iwt
   else
      call error('enwtf','illegal iwt',' ')
   endif
   return
   end subroutine enwtf

   subroutine etflx(e,enext,idis,flux,nl)
   !------------------------------------------------------------------
   ! Retrieve or compute required legendre component of the
   ! weight function constructed or read in by genwtf.
   !------------------------------------------------------------------
   use endf ! provides terpa
   ! externals
   integer::idis,nl
   real(kr)::e,enext,flux(10,10)
   ! internals
   integer::ip,ir,il
   real(kr)::wtf,enxt
   real(kr),parameter::emax=1.e12_kr
   real(kr),parameter::step=1.05e0_kr
   real(kr),parameter::zero=0.0
   save ip,ir

   !--initialize
   idis=0
   if (e.eq.zero) then
      ip=2
      ir=1
      enext=emax

   !--branch to desired method
   else
      !--tabulated
      if (iwt.ne.2) then
         call terpa(wtf,e,enext,idis,wght,ip,ir)
         do il=1,nl
            flux(1,il)=wtf
         enddo
         enxt=step*e
         if (enext.gt.enxt) idis=0
         if (enext.gt.enxt) enext=enxt

     !--constant for all orders
      else
         do il=1,nl
            flux(1,il)=1.0
         enddo
         enext=emax
         idis=0
      endif
   endif
   return
   end subroutine etflx

   subroutine epanel(igp,elo,ehi,ans,ff,nl,nz,ng,iglo,sdat,eetff)
   !------------------------------------------------------------------
   ! Perform generalized group constant integrals for one panel.
   ! The upper boundry of the panel is chosen to be the smallest
   ! of ehi, the next cross section point, the next flux point,
   ! and the next feed function point.  Use Lobatto quadrature
   ! with order two larger than that used for the feed function.
   !------------------------------------------------------------------
   use util ! provides error
   ! externals
   integer::igp,nl,nz,ng,iglo
   real(kr)::elo,ehi,ans(nl,nz,ng+1),ff(nl,ng),sdat(*)
   interface
      subroutine eetff(e,igp,enext,idisc,ff,nl,iglo,nq,slst)
         use locale
         integer::igp,idisc,nl,iglo,nq
         real(kr)::e,enext,ff(nl,igp+3),slst
      end subroutine eetff
   end interface
   ! internals
   integer::idiscf,ig1,nq,iz,il,iq,nqp,ig,igt
   real(kr)::en,ehigh,aq,bq,eq,wq,t1,rr,enext
   real(kr)::sig(10,10),slst(10,10),flux(10,10),flst(10,10)
   real(kr),dimension(2),parameter::qp2=(/-1.e0_kr,1.e0_kr/)
   real(kr),dimension(2),parameter::qw2=(/1.e0_kr,1.e0_kr/)
   real(kr),dimension(4),parameter::qp4=(/-1.e0_kr,-.4472135955e0_kr,&
      .4472135955e0_kr,1.e0_kr/)
   real(kr),dimension(4),parameter::qw4=(/1.6666666667e-1_kr,&
      8.3333333333e-1_kr,8.3333333333e-1_kr,1.6666666667e-1_kr/)
   real(kr),dimension(6),parameter::qp6=(/&
     -1.e0_kr,-.76505532e0_kr,-.28523152e0_kr,.28523152e0_kr,&
     .76505532e0_kr,1.e0_kr/)
   real(kr),dimension(6),parameter::qw6=(/&
     .06666667e0_kr,.37847496e0_kr,.55485838e0_kr,.55485838e0_kr,&
     .37847496e0_kr,.06666667e0_kr/)
   real(kr),dimension(8),parameter::qp8=(/-1.e0_kr,0.871740148510e0_kr,&
     -0.591700181433e0_kr,-0.209299217902e0_kr,0.209299217902e0_kr,&
     0.591700181433e0_kr,0.871740148510e0_kr,1.e0_kr/)
   real(kr),dimension(8),parameter::qw8=(/0.035714285714e0_kr,&
     0.210704227144e0_kr,0.341122692484e0_kr,0.412458794659e0_kr,&
     0.412458794659e0_kr,0.341122692484e0_kr,0.210704227144e0_kr,&
     0.035714285714e0_kr/)
   real(kr),dimension(10),parameter::qp10=(/&
     -1.e0_kr,-.9195339082e0_kr,-.7387738651e0_kr,-.4779249498e0_kr,&
     -.1652789577e0_kr,.1652789577e0_kr,.4779249498e0_kr,&
     .7387738651e0_kr,.9195339082e0_kr,1.e0_kr/)
   real(kr),dimension(10),parameter::qw10=(/&
     .0222222222e0_kr,.1333059908e0_kr,.2248893420e0_kr,&
     .2920426836e0_kr,.3275397612e0_kr,.3275397612e0_kr,&
     .2920426836e0_kr,.2248893420e0_kr,.1333059908e0_kr,&
     .0222222222e0_kr/)
   real(kr),parameter::rndoff=1.000002e0_kr
   real(kr),parameter::delta=0.999995e0_kr
   integer::idisc=0
   real(kr)::elast=0
   save nq,enext,elast,slst,flst,idisc,ig1

   !--retrieve factors in integrands at lower boundry.
   if (elo.gt.ehi) call error('epanel','elo gt ehi.',' ')
   if (elo.ne.elast) then
      if (elo*rndoff.lt.ehi) elo=elo*rndoff
      elast=elo
      call etsig(elo,enext,idisc,slst,sdat)
      call etflx(elo,en,idiscf,flst,nl)
      if (en.eq.enext.and.idiscf.gt.idisc) idisc=idiscf
      if (en.lt.enext) idisc=idiscf
      if (en.lt.enext) enext=en
      call eetff(elo,igp,en,idiscf,ff,nl,ig1,nq,slst(1,1))
      if (en.eq.enext.and.idiscf.gt.idisc) idisc=idiscf
      if (en.lt.enext) idisc=idiscf
      if (en.lt.enext) enext=en
      nq=nq+2
      if (nq.gt.10) nq=10
   endif

   !--integrate over panel using Lobatto quadrature.
   if (enext.lt.delta*ehi) then
      ehi=enext
      ehigh=ehi
      if (idisc.gt.0.and.ehi*delta.gt.elo) ehigh=ehi*delta
   else
      ehigh=delta*ehi
   endif
   aq=(ehigh+elo)/2.0
   bq=(ehigh-elo)/2.0
   do iq=1,nq
      wq=1.0
      if (nq.eq.2) then
         eq=aq+bq*qp2(iq)
         wq=bq*qw2(iq)
      else if (nq.eq.4) then
         eq=aq+bq*qp4(iq)
         wq=bq*qw4(iq)
      else if (nq.eq.6) then
         eq=aq+bq*qp6(iq)
         wq=bq*qw6(iq)
      else if (nq.eq.8) then
         eq=aq+bq*qp8(iq)
         wq=bq*qw8(iq)
      else if (nq.eq.10) then
         eq=aq+bq*qp10(iq)
         wq=bq*qw10(iq)
      else
         call error('epanel','bad nq in panel',' ')
      endif
      t1=(eq-elo)/(ehi-elo)

      !--interpolate reaction rate and retrieve the feed function at the
      !--quadrature point. first point was last point of previous panel.
      if (iq.eq.1) then
         sig(:nz,1)=slst(:nz,1)
         flux(:nz,:nl)=flst(:nz,:nl)
      else
         call etsig(eq,enext,idisc,sig(1,1),sdat)
         call etflx(eq,en,idiscf,flux(1,1),nl)
         if (en.eq.enext.and.idiscf.gt.idisc) idisc=idiscf
         if (en.lt.enext) idisc=idiscf
         if (en.lt.enext) enext=en
         if (eq.gt.ehigh) eq=ehigh
         call eetff(eq,igp,en,idiscf,ff,nl,ig1,nqp,sig(1,1))
      endif

      !--accumulate the ng*nl*nz integrals simultaneously assuming
      !--that the flux and reaction rate are linear across the panel.
      if (iglo.eq.0) iglo=ig1
      do iz=1,nz
         do il=1,nl
            rr=flux(iz,il)*wq
            ans(il,iz,1)=ans(il,iz,1)+flux(iz,il)*wq
            do ig=1,igp+3
               igt=ig1+ig-iglo+1
               if (igt.gt.1) then
                  if (ig.gt.ng) call error('epanel','ig.gt.ng.',' ')
                  if (igt.gt.ng+1) call error('epanel','igt.gt.ng+1.',' ')
                  ans(il,iz,igt)=ans(il,iz,igt)+rr*ff(il,ig)
               endif
            enddo
         enddo
      enddo
   enddo

   !--save cross section and flux for next panel.
   !--determine next point and quadrature order from last ff.
   elast=ehigh
   do iz=1,nz
      slst(iz,1)=sig(iz,1)
      do il=1,nl
         flst(iz,il)=flux(iz,il)
      enddo
   enddo
   if (en.eq.enext.and.idiscf.gt.idisc) idisc=idiscf
   if (en.lt.enext) idisc=idiscf
   if (en.lt.enext) enext=en
   if (enext.le.ehi) enext=rndoff*ehi
   if (enext.eq.elo) call error('epanel','no epanel step.',' ')
   nq=nqp+2
   if (nq.gt.10) nq=10
   return
   end subroutine epanel

   subroutine edspla(ig,ans,nl,nz,ng2,ig2lo,igzero)
   !------------------------------------------------------------------
   ! Display generalized group constants generated by panel.
   !------------------------------------------------------------------
   use mainio ! provides nsyso
   use util ! provides a10
   ! externals
   integer::ig,nl,nz,ng2,ig2lo,igzero
   real(kr)::ans(nl,nz,*)
   ! internals
   integer::i,ig2,igt,max,il
   real(kr)::result(20)
   character(10)::field(20)
   real(kr),parameter::small=1.e-9_kr
   real(kr),parameter::zero=0

   !--write appropriate heading.
   igzero=0
   if (ig.lt.0) then
      if (iprint.eq.1) then
         write(nsyso,'('' '')')
         if (mfd.ne.26) then
            if (mtd.eq.501.or.mtd.eq.522.or.mtd.eq.525)&
              write(nsyso,'(&
              &'' electron sigma  ''/'' group   (barns)'')')
            if (mtd.eq.527) write(nsyso,'(&
              &'' electron sigma  ''/'' group   (barns)'')')
            if (mtd.eq.530) write(nsyso,'(&
              &'' electron energy deposition''/&
              &'' group   (ev-barns)'')')
            if (mtd.eq.531) write(nsyso,'(&
              &'' electron charge deposition''/&
              &'' group   (ev-barns)'')')
         else
            if (mtd.eq.522.or.mtd.eq.525) write(nsyso,'(&
              &'' initl  final  cross sections vs''/&
              &'' group  group  legendre order   '')')
            if (mtd.eq.527) write(nsyso,'(&
              &'' initl  final  cross sections vs''/&
              &'' group  group  legendre order   '')')
         endif
      endif
      return
   endif

   !--write out results in appropriate format.
   !--cross sections and heating values.
   if (mfd.eq.23) then
      do i=2,ng2
         result(i-1)=0
         if (ans(1,1,1).ne.zero.and.abs(ans(1,1,i)).ge.small) then
            result(i-1)=ans(1,1,i)/ans(1,1,1)
            ans(1,1,i)=result(i-1)
            if (abs(ans(1,1,i)).ge.small) igzero=1
         endif
      enddo
      if (result(1).ne.zero.and.iprint.eq.1) then
         do i=2,ng2
            call a10(result(i-1),field(i-1))
         enddo
         write(nsyso,'(1x,i3,2x,1p,2a11)') ig,(field(i-1),i=2,ng2)
      endif

   !--scattering matrices.
   else if (mfd.eq.26) then
      do ig2=2,ng2
         igt=ig2lo+ig2-2
         if (mtd.eq.516.and.ig2.gt.2) then
            result(1)=ans(1,1,ig2)/ans(1,1,1)
            ans(1,1,ig2)=result(1)
            if (ans( 1,1,ig2).ge.small) igzero=1
            if (iprint.eq.1) then
               write(nsyso,'(1x,i3,3x,''heat'',2x,1p,e11.3)')&
                 ig,result(1)
            endif
         else if (igt.le.ig) then
            max=nl
            if (max.gt.6) max=6
            do il=1,max
               result(il)=ans(il,1,ig2)/ans(il,1,1)
               ans(il,1,ig2)=result(il)
               if (ans(il,1,ig2).ge.small) igzero=1
               call a10(result(il),field(il))
            enddo
            if (iprint.eq.1)&
              write(nsyso,'(1x,i3,4x,i3,2x,1p,6a11)')&
              ig,igt,(field(i),i=1,max)
            if (nl.gt.6) then
               do il=7,nl
                  result(il)=ans(il,1,ig2)/ans(il,1,1)
                  ans(il,1,ig2)=result(il)
                  if (ans(il,1,ig2).ge.small) igzero=1
                  call a10(result(il),field(il))
               enddo
               if (iprint.eq.1)&
                 write(nsyso,'(13x,1p,6a11)') (field(i),i=7,nl)
            endif
         else
            result(1)=ans(1,1,ig2)/ans(1,1,1)
            ans(1,1,ig2)=result(1)
            if (ans( 1,1,ig2).ge.small) igzero=1
            call a10(result(1),field(1))
            if (iprint.eq.1) then
               if (igt.eq.ig+1) write(nsyso,'(&
                 &1x,i3,3x,''xsec'',2x,1p,a11)') ig,field(1)
               if (igt.eq.ig+2) write(nsyso,'(&
                 &1x,i3,3x,''heat'',2x,1p,a11)') ig,field(1)
            endif
         endif
      enddo
   endif
   return
   end subroutine edspla

   subroutine etsig(e,enext,idis,sig,sdat)
   !------------------------------------------------------------------
   ! Retrieve the reaction cross-section defined by mfd and mtd.
   !------------------------------------------------------------------
   use endf ! provides endf routines and variables
   ! externals
   integer::idis
   real(kr)::e,enext,sig(10,10),sdat(*)
   ! internals
   integer::nb,nw,mf,mt
   real(kr)::s
   real(kr),parameter::zero=0

   !--initialize
   if (e.eq.zero) then
      mf=23
      mt=mtd
      call findf(matd,mf,mt,npend)
      call contio(npend,0,0,sdat,nb,nw)
      call gety1(e,enext,idis,s,npend,sdat)

   !--normal entry
   else
      call gety1(e,enext,idis,s,npend,sdat)
      sig(1,1)=s
   endif
   return
   end subroutine etsig

   subroutine etff(e,igp,enext,idisc,ff,nl,iglo,nq,slst)
   !------------------------------------------------------------------
   ! Compute feed function or yield for desired reaction type.
   !------------------------------------------------------------------
   use util ! provides error
   use endf ! provides endf routines and variables
   use mathm ! provides legndr
   ! externals
   integer::igp,idisc,nl,iglo,nq
   real(kr)::e,enext,ff(nl,igp+3),slst,pp
   ! internals
   integer::il,ig,ind,nedg,itr
   real(kr)::pl(50)
   real(kr)::tei,tef,tet,tes,smol,smol1,unow,aslt,stp,tkbe,sigtot,&
   sigtt0,sigtt1,beta2,f1,f2,f3,tnow
   real(kr),parameter::c2=0.249467e0_kr ! 3/8 Thomson cross section
   real(kr),parameter::c3=1.95693e-6_kr ! inverse electron rest mass
   real(kr),parameter::emax=1.e12_kr
   real(kr),parameter::zero=0.0

   !--electron interaction cross sections and energy/charge deposition.
   tei=c3*e ! k
   ff(:nl,:igp+3)=0.0
   ind=int(znow)
   if (mfd.eq.23) then
      nq=0
      iglo=1
      idisc=0
      if (e.eq.zero) then
         e=emax
         return
      endif
      ff(1,1)=1.0

      aslt=0.0
      !--collisional stopping power
      if (mtd.eq.507) then
         if (e.eq.ege(igp)) then
            stp=eestop(igp)
         else if (e.eq.ege(igp+1)) then
            stp=eestop(igp+1)
         endif
         beta2=1.0-1.0/(1+tei)**2
         if (igp.gt.1) then
            tnow=c3*ege(igp-1)
            if (tei/2.0.lt.tnow) then
               f1 = 2.0-tei/tnow+log(tei**2/(4.0*tnow*(tei-tnow)))
               f2 = 0.5*(tnow*(2.0*tei-tnow)-(3.0*tei*tei/4.0))
               f3 = log(0.5*tei/tnow)
               aslt=2.0e-6*znow*c2/beta2/c3*(f1+f2/((tei+1.0)**2)+ &
                    f3*(2*tei+1)/(tei+1)**2)
            endif
         endif

      !--radiative stopping power
      else if (mtd.eq.508) then
         if (e.eq.ege(igp)) then
            stp=ebstop(igp)
         else if (e.eq.ege(igp+1)) then
            stp=ebstop(igp+1)
         endif
         if (igp.gt.1) aslt=ebstop(igp-1)
      endif
      ff(1,2)=max(0.0,stp-aslt)

   !--electron scattering cross sections.
   !--initialize.
   else if (mfd.eq.26) then
      nq=6
      if (nl.gt.4) nq=10

      !--Recover scattering laws from endf tape
      if ((e.eq.zero).and.(mtd.ge.525)) then
         call eetsed(e,enext,idisc,sede,ege,nge,nl,nk,matd,26,mtd,nendf)
         if (allocated(sede)) deallocate(sede)
         if (mtd.eq.525) then
            allocate(sede(nk,nl))
         else
            allocate(sede(nk,nge))
         endif
         return
      endif
      call eetsed(e,enext,idisc,sede,ege,nge,nl,nk,matd,26,mtd,nendf)

      !--large angle elastic angular distributions
      if (mtd.eq.525) then
         do il=1,nl
            ! A transport correction is chosen such that the
            ! maximum legendre order of the corrected cross
            ! section is zero: sigma(nl) = 0
            ff(il,igp) = slst*(sede(1,il)-sede(1,nl))
         enddo
         ff(1,igp+1)=ff(1,igp) ! total xs

      !--bremsstrahlung
      else if (mtd.eq.527) then
         if (igp.eq.1) then
            enext=ege(2)
            return
         endif

         !--scattered electron
         sigtot=0.0
         sigtt1=0.0
         do ig=igp-2,1,-1
            smol=slst*sede(3,ig)
            smol1=slst*sede(4,ig)
            ff(1,ig)=smol
            do il=2,nl
               ff(il,ig)=ff(1,ig)
            enddo
            sigtot=sigtot+smol
            sigtt1=sigtt1+smol1
         enddo

         !--include contributions below ene(1)
         pp=1.0
         do ig=1,igp
           pp=pp-sede(3,ig)
         enddo
         sigtot=sigtot+slst*pp
         ff(1,igp+1)=sigtot

         !--energy deposition
         ff(1,igp+2)=(sigtot*e-sigtt1)*1.e-6 ! catastrophic
         ff(1,igp+2)=ff(1,igp+2)+(ebmtop(igp)-ebstop(igp-1)) ! soft
         enext=ege(igp+1)

      !--impact electroionization and relaxation production
      else if ((mtd.ge.534).and.(mtd.le.572)) then
         if (igp.eq.1) then
            enext=ege(2)
            return
         endif
         nedg=mtd-533
         if (nedg.le.maxndg) then
            tet=c3*be(nedg) ! k-threshold
         else
            tet=0.0
         endif

         !--principal scattered electron
         sigtot=0.0
         sigtt0=0.0
         sigtt1=0.0
         do ig=igp-2,1,-1
            tef=c3*egem(ig) ! k'
            unow=sqrt(tef*(tei-tet+2.0)/((tei-tet)*(tef+2.0))) ! deviation cos
            smol=slst*sede(3,ig)
            smol1=slst*sede(4,ig)
            call legndr(unow,pl,nl)
            do il=1,nl
               ff(il,ig)=ff(il,ig)+smol*pl(il)
            enddo
            sigtot=sigtot+smol
            sigtt1=sigtt1+smol1
         enddo
         ff(1,igp+1)=sigtot

         !--recoil electron
         do ig=igp-2,1,-1
            tes=c3*egem(ig) ! k'
            unow=sqrt(tes*(tei-tet+2.0)/((tei-tet)*(tes+2.0))) ! deviation cos
            smol=slst*sede(1,ig)
            smol1=slst*sede(2,ig)
            call legndr(unow,pl,nl)
            do il=1,nl
               ff(il,ig)=ff(il,ig)+smol*pl(il)
            enddo
            sigtt0=sigtt0+smol
            sigtt1=sigtt1+smol1
         enddo

         !--energy deposition
         if (mtd.eq.534) then
            tei=c3*egem(igp)
            tnow=c3*ege(igp-1)
            aslt=0.0
            if (tei/2.0.lt.tnow) then
               beta2=1.0-1.0/(1+tei)**2
               f1 = 2.0-tei/tnow+log(tei**2/(4.0*tnow*(tei-tnow)))
               f2 = 0.5*(tnow*(2.0*tei-tnow)-(3.0*tei**2/4.0))
               f3 = log(0.5*tei/tnow)
               aslt=2.0e-6*znow*c2/beta2/c3*(f1+f2/((tei+1.0)**2)+ &
                    f3*(2*tei+1)/(tei+1)**2)
               ff(1,igp+2)=ff(1,igp+2)+(eemtop(igp)-aslt) ! soft
            endif
         endif
         ff(1,igp+2)=ff(1,igp+2)+(sigtot*e-sigtt1)*1.e-6 ! catastrophic
         ff(1,igp+3)=ff(1,igp+3)-sigtt0

         !--Auger electron
         if (nedg.le.maxndg) then
            tkbe=c3*be(nedg)
            if (tkbe.gt.1.0d-12) then
               do itr=1,ntr
                  ig=iaup(nedg,itr)
                  if ((ig.ne.0).and.(ig.ne.igp)) then
                     if (c3*ege(ig+1).lt.tkbe) exit
                     ff(1,ig)=ff(1,ig)+slst*eff1(nedg,itr) ! isotropic in LAB
                     ff(1,igp+2)=ff(1,igp+2)-slst*eff1(nedg,itr)*efn(nedg,itr)*&
                     1.e-6
                     ff(1,igp+3)=ff(1,igp+3)-slst*eff1(nedg,itr)
                  endif
               enddo
            endif
         endif
         enext=ege(igp+1)
      endif

   !--bad file type
   else
      call error('etff','illegal file type.',' ')
   endif
   return
   end subroutine etff

   subroutine etffg(e,igp,enext,idisc,ff,nl,iglo,nq,slst)
   !------------------------------------------------------------------
   ! Compute photon feed function or photon yield for desired reaction
   ! type.
   !------------------------------------------------------------------
   use util ! provides error
   use endf ! provides endf routines and variables
   use mathm ! provides legndr
   ! externals
   integer::igp,idisc,nl,iglo,nq
   real(kr)::e,enext,ff(nl,igp+3),slst
   ! internals
   integer::ig,il,itr,nedg
   real(kr)::smol,smol1,tkbe,bc,sigtt1,tei
   real(kr),parameter::c3=1.95693e-6_kr ! inverse electron rest mass
   real(kr),parameter::zero=0.0

   !--electron->photon scattering cross sections.
   !--initialize.
   tei=c3*e ! k
   ff(:nl,:igp+3)=0.0
   if (mfd.eq.26) then
      nq=6
      if (nl.gt.4) nq=10

      !--Recover scattering laws from endf tape
      if ((e.eq.zero).and.(mtd.eq.527)) then
         call eetsed(e,enext,idisc,sede,egg,ngg,nl,nk,matd,26,mtd,nendf)
         if (allocated(sede)) deallocate(sede)
         allocate(sede(nk,ngg))
         return
      endif
      call eetsed(e,enext,idisc,sede,egg,ngg,nl,nk,matd,26,mtd,nendf)

      !--bremsstrahlung photon energy spectra
      if (mtd.eq.527) then
         !--scattered photon
         sigtt1=0.0
         do ig=igp,1,-1
            smol=slst*sede(1,ig)
            smol1=slst*sede(2,ig)
            do il=1,nl
               bc=ebcof(tei,il)
               ff(il,ig)=smol*bc
            enddo
            sigtt1=sigtt1+smol1
         enddo

         !--energy deposition
         ff(1,igp+2) = -sigtt1
         enext=egg(igp+1)

      !--relaxation production
      else if ((mtd.ge.534).and.(mtd.le.538)) then
         !--fluorescence photon
         nedg=mtd-533
         if (nedg.le.maxndg) then
            tkbe=c3*be(nedg)
            if (tkbe.gt.1.0d-12) then
               do itr=1,ntr
                  ig=iflp(nedg,itr)
                  if ((ig.ne.0).and.(ig.ne.igp)) then
                     if (c3*egg(ig+1).lt.tkbe) exit
                     ff(1,ig)=ff(1,ig)+slst*eff(nedg,itr) ! isotropic in LAB
                     ff(1,igp+2)=ff(1,igp+2)-slst*eff(nedg,itr)*efn(nedg,itr)*&
                     1.e-6
                     ff(1,igp+3)=ff(1,igp+3)-slst*eff(nedg,itr)
                  endif
               enddo
            endif
         endif
         enext=egg(igp+1)
      endif

   !--bad file type
   else
      call error('etffg','illegal file type.',' ')
   endif
   return
   end subroutine etffg

   real(kr) function ebcof(t,il)
   implicit real(kr) (a-h,p-z)
   implicit integer (i-n)
   !
   ! Legendre expansion coefficient for the normalized bremsstrahlung
   ! angular distribution (see Sandyl manual pgs. 56,64 in SCL-or-72-0109)
   !
   !  t   initial energy of the incident electron in mc**2 units
   !  il  the order of the coefficient desired
   !
   real(kr)::t
   integer::il
   !
   beta2 = (t + 2.0)*t/(t + 1)**2 ! beta square
   b0 = sqrt(beta2)
   if (il .eq. 1) then
      ebcof = 1.0
    else if (il .eq. 2) then
      ebcof = 1.0/b0 + (1.0 - beta2)/(2.0*beta2)*log((1.0 - b0)/(1.0 + b0))
    else
      iflag = 1
      s0 = 1.0
      s1 = 1.0/b0 + (1.0 - beta2)/(2.0*beta2)*log((1.0 - b0)/(1.0 + b0))
      do n=2,il-1
         if (iflag .eq. 1) then
            xn = real(n)
            ebcof = ((2.0*xn - 1.0)*s1 - b0*xn*s0)/(b0*(xn - 1.0))
            s0 = s1
            s1 = ebcof
            if (ebcof .le. 1.0d-4) iflag = 0
          else
            ebcof = 0.0
         endif
      enddo
   endif
   return
   end function ebcof

   subroutine erelax(nin,ma,mf,mt)
   !------------------------------------------------------------------
   ! Recover Auger and fluorescence relaxation data
   !------------------------------------------------------------------
   use util ! provides error
   use endf ! provides endf routines and variables
   integer::nin,ma,mf,mt
   ! internals
   integer::nb,nw,jnow,nedg,jdict,itr,subj,subk
   integer,parameter::nwpff=50000
   real(kr)::zz
   real(kr),dimension(:),allocatable::pff
   !
   nacti=nin
   matd=ma
   mfd=mf
   mtd=mt
   be(:maxndg)=0.0d0
   eff1(:maxndg,:maxntr)=0.0d0
   eff(:maxndg,:maxntr)=0.0d0
   efn(:maxndg,:maxntr)=0.0d0
   allocate(pff(nwpff))
   call repoz(nacti)
   call findf(matd,mfd,mtd,nacti)
   jnow=1
   call contio(nacti,0,0,pff(jnow),nb,nw)
   zz=pff(jnow+1)
   ndg=nint(pff(jnow+4)) ! number of subshells
   jnow=jnow+nw
   do nedg=1,min(ndg,maxndg)
     call contio(nacti,0,0,pff(jnow),nb,nw)
     ntr=nint(pff(jnow+5)) ! number of transitions
     if (ntr.gt.maxntr) call error('erelax',&
        'number of transitions exceeded',' ')
     jnow=jnow+nw
     jdict=jnow
     nw=ntr+1
     call dictio(nacti,0,0,pff(jnow),nb,nw)
     jnow=jnow+nw
     if (jnow.gt.nwpff) call error('erelax',&
        'pff storage exceeded',' ')
     be(nedg)=pff(jdict)
     do itr=1,ntr
       subj=nint(pff(jdict+itr*6))
       subk=nint(pff(jdict+itr*6+1))
       if (subk.eq.0) then
         ! radiative transition (fluorescence)
         eff(nedg,itr)=pff(jdict+itr*6+3)
       else
         ! non-radiative transition (Auger)
         eff1(nedg,itr)=pff(jdict+itr*6+3)
       endif
       efn(nedg,itr)=pff(jdict+itr*6+2)
     enddo
   enddo
   deallocate(pff)
   end subroutine erelax

   subroutine lgwt(N,a,b,x,w)
   !------------------------------------------------------------------
   ! This subroutine is for computing definite integrals using Legendre-Gauss
   ! Quadrature. Computes the Legendre-Gauss nodes and weights on an interval
   ! [a,b] with truncation order N
   !
   ! Suppose you have a continuous function f(x) which is defined on [a,b]
   ! which you can evaluate at any x in [a,b]. Simply evaluate it at all of
   ! the values contained in the x vector to obtain a vector f. Then compute
   ! the definite integral using sum(f*w).
   !------------------------------------------------------------------
   use util   ! provides error
   implicit real(kr)(a-h,o-z)
   ! externals
   integer::N
   real(kr)::a,b
   real(kr),dimension(N)::x,w
   ! internals
   integer::i,k,iter,N2
   real(kr)::xu
   real(kr),dimension(:,:),allocatable::L
   real(kr),dimension(:),allocatable::y0,y,Lp
   real(kr),parameter::pi=3.14159265359d0,eps=1.0d-10
   !
   N2=N+1

   ! Initial guess
   allocate(y(N))
   xu=-1.0d0
   do i=0,N-1
      y(i+1)=cos(real(2*i+1)*pi/real(2*N))+(0.27d0/real(N))*&
      sin(pi*xu*real(N-1)/real(N2))
      xu=xu+2.0/real(N-1)
   enddo
   allocate(y0(N),L(N,N2),Lp(N))

   ! Legendre-Gauss Vandermonde Matrix
   L=0

   ! Compute the zeros of the N+1 Legendre Polynomial
   ! using the recursion relation and the Newton-Raphson method

   y0(:)=1.0d10

   ! Iterate until new points are uniformly within epsilon of old points
   iter=0
   do
      iter=iter+1

      if (maxval(abs(y-y0)) <= eps) exit
      if (iter > 20) call error('lgwt','Newton-Raphton convergence failure',' ')
      L(:,1)=1.0d0

      L(:,2)=y(:)

      do k=2,N
         L(:,k+1)=( real(2*k-1)*y(:)*L(:,k)-real(k-1)*L(:,k-1) )/real(k)
      enddo

      Lp(:)=real(N2)*( L(:,N)-y(:)*L(:,N2) )/(1.0d0-y(:)**2)

      y0(:)=y(:)
      y(:)=y0(:)-L(:,N2)/Lp(:)
   enddo
   deallocate(y0,L)

   ! Linear map from[-1,1] to [a,b]
   x(:)=(a*(1.0d0-y)+b*(1.0d0+y))/2.0d0

   ! Compute the weights
   w(:)=(b-a)/((1.0d0-y(:)**2)*Lp(:)**2)*(real(N2)/real(N))**2
   deallocate(y,Lp)
   end subroutine lgwt

   subroutine eetsed(ed,enext,idis,sed,eg,ng,nl,nk,matd,mfd,mtd,nin)
   !-------------------------------------------------------------------
   ! Compute secondary energy distribution for all sink groups
   ! simultaneously.  Laws 1, 2 and 8 are coded.
   ! law 1: unit base interpolation for energy slowing down (collision/
   !        ionization and bremsstrahlung)
   ! law 2: energy-dependent angular distribution
   ! law 8: energy-dependent averaged energy loss (TAB1 record).
   ! Initialize if ed=0 and return number of subsections in nk.
   ! On a normal entry if nk=1, return the sum of all the subsections,
   ! but if nk.gt.1, return the contributions separately.
   ! Based on subroutine getsed in module groupr.
   !-------------------------------------------------------------------
   use mainio ! provides nsyso
   use util   ! provides mess
   use endf   ! provides endf routines and variables
   use mathm  , only:legndr ! provides legndr
   ! externals
   integer::idis,ng,nl,nk,matd,mfd,mtd,nin,nc
   real(kr)::ed,enext,sed(nk,*),eg(ng+1)
   ! internals
   integer::nupm,nb,nw,l,il,ik,lf,ip,ne,nbt,nnow,idisc
   integer::nne,ir,nr,np,ig,lnow,mnow
   integer::ntmp,klo,nplo,khi,nphi,jnt,knt,iphi,imu
   integer::ier
   real(kr)::elo,ee,e1,e2,ehi,pe,eihi
   real(kr)::xlo,xhi,xend,e1lo,e1hi,e2lo,e2hi,flo,fhi,fe,thresh,asum
   real(kr)::pl(50)
   integer,parameter::nkmax=20
   integer,parameter::nmu=256 ! number of direction cosine sampling bins
   integer::loc(nkmax)
   real(kr),dimension(:),allocatable::tmp,mu_lim,mu,mu1,wmu,smol
   real(kr),parameter::emax=1.e10_kr
   real(kr),parameter::eps=.001e0_kr
   real(kr),parameter::small=1.e-10_kr
   real(kr),parameter::ebig=1.e8_kr
   real(kr),parameter::tenth=0.1e0_kr
   save nupm,loc

   !--initialize
   if (ed.eq.0.0) then
      ier=1
      ntmp=250000
      do while (ier.ne.0)
         if (allocated(tmp)) deallocate(tmp)
         allocate(tmp(ntmp),stat=ier)
         if (ier.ne.0) ntmp=ntmp/2
      enddo
      call findf(matd,mfd,mtd,nin)
      call contio(nin,0,0,tmp,nb,nw)
      nk=nint(tmp(5))
      if (nk.gt.nkmax) call error('eetsed',&
         'too many subsections.',' ')
      l=1
      ik=0
      enext=emax
      do while (ik.lt.nk)
         ik=ik+1
         loc(ik)=l-1
         call tab1io(nin,0,0,tmp(l),nb,nw)
         il=l
         l=l+nw
         if (l.gt.ntmp) call error('eetsed',&
           'insufficient storage for TAB2 data.',' ')
         do while (nb.ne.0)
            call moreio(nin,0,0,tmp(l),nb,nw)
            l=l+nw
            if (l.gt.ntmp) call error('eetsed',&
              'insufficient storage for TAB2 data.',' ')
         enddo
         lf=nint(tmp(il+3))
         nr=nint(tmp(il+4))
         np=nint(tmp(il+5))
         elo=tmp(il+6+2*nr)
         if (elo.lt.enext) enext=elo

         if ((lf.eq.1).or.(lf.eq.2)) then
            !--law 1 or 2
            !--tabulated subsection
            call tab2io(nin,0,0,tmp(l),nb,nw)
            ne=nint(tmp(l+5))
            l=l+nw

            !--read and average spectrum for each incident energy
            !--for electroatomic laws, tab1io data is replaced by listio
            !--data.
            nne=0
            do while (nne.lt.ne)
               call listio(nin,0,0,tmp(l),nb,nw)
               l=l+nw
               if (l.gt.ntmp) call error('eetsed',&
                  'storage for tmp exceeded',' ')
               do while (nb.ne.0)
                  call moreio(nin,0,0,tmp(l),nb,nw)
                  l=l+nw
                  if (l.gt.ntmp) call error('eetsed',&
                  'storage for tmp exceeded',' ')
               enddo
               nne=nne+1
            enddo

            if (lf.eq.1) then
               !--add information related to first moment of the recoil electron
               ik=ik+1
               nk=nk+1
               loc(ik)=l-1
               klo=loc(ik-1)+1
               tmp(l:l+6+2*nr+2*np)=tmp(klo:klo+6+2*nr+2*np)
               l=l+6+2*nr+2*np
               klo=klo+6+2*nr+2*np
               nr=nint(tmp(klo+4))
               tmp(l:l+6+2*nr)=tmp(klo:klo+6+2*nr)
               l=l+6+2*nr
               klo=klo+6+2*nr
               do nne=1,ne
                  ee=tmp(klo+1) ! energy of incident electron
                  nphi=nint(tmp(klo+5))
                  tmp(l:l+5)=tmp(klo:klo+5)
                  l=l+6
                  do iphi=1,nphi
                     e1=tmp(klo+5+2*(iphi-1)+1)
                     tmp(l)=e1
                     tmp(l+1)=tmp(klo+5+2*(iphi-1)+2)*e1
                     l=l+2
                  enddo
                  klo=klo+6+2*nphi
               enddo

               !--add information related to principal scattered electron
               ik=ik+1
               nk=nk+1
               loc(ik)=l-1
               klo=loc(ik-2)+1
               if ((mtd.ge.534).and.(mtd.le.572)) then
                  thresh=tmp(klo+6+2*nr) ! threshold energy
               else
                  thresh=0.0
               endif
               tmp(l:l+6+2*nr+2*np)=tmp(klo:klo+6+2*nr+2*np)
               l=l+6+2*nr+2*np
               klo=klo+6+2*nr+2*np
               nr=nint(tmp(klo+4))
               tmp(l:l+6+2*nr)=tmp(klo:klo+6+2*nr)
               l=l+6+2*nr
               klo=klo+6+2*nr
               do nne=1,ne
                  ee=tmp(klo+1) ! energy of incident electron
                  nphi=nint(tmp(klo+5))
                  tmp(l:l+5)=tmp(klo:klo+5)
                  l=l+6
                  do iphi=1,nphi
                     e1=ee-tmp(klo+5+2*(nphi-iphi)+1)-thresh+0.1
                     tmp(l)=e1 ! energy of scattered electron
                     tmp(l+1)=tmp(klo+5+2*(nphi-iphi)+2)
                     l=l+2
                  enddo
                  klo=klo+6+2*nphi
               enddo

               !--add information related to first moment of the principal
               !--scattered electron
               ik=ik+1
               nk=nk+1
               loc(ik)=l-1
               klo=loc(ik-3)+1
               if ((mtd.ge.534).and.(mtd.le.572)) then
                  thresh=tmp(klo+6+2*nr) ! threshold energy
               else
                  thresh=0.0
               endif
               tmp(l:l+6+2*nr+2*np)=tmp(klo:klo+6+2*nr+2*np)
               l=l+6+2*nr+2*np
               klo=klo+6+2*nr+2*np
               nr=nint(tmp(klo+4))
               tmp(l:l+6+2*nr)=tmp(klo:klo+6+2*nr)
               l=l+6+2*nr
               klo=klo+6+2*nr
               do nne=1,ne
                  ee=tmp(klo+1) ! energy of incident electron
                  nphi=nint(tmp(klo+5))
                  tmp(l:l+5)=tmp(klo:klo+5)
                  l=l+6
                  do iphi=1,nphi
                     e1=ee-tmp(klo+5+2*(nphi-iphi)+1)-thresh+0.1
                     tmp(l)=e1 ! energy of scattered electron
                     tmp(l+1)=tmp(klo+5+2*(nphi-iphi)+2)*e1
                     l=l+2
                  enddo
                  klo=klo+6+2*nphi
               enddo
            endif
         else if (lf.eq.8) then
            !--law 8
            ik=ik+1
            nk=nk+1
            call tab1io(nin,0,0,tmp(l),nb,nw)
            l=l+nw
            if (l.gt.ntmp) call error('eetsed',&
              'insufficient storage for TAB1 data.',' ')
            do while (nb.ne.0)
               call moreio(nin,0,0,tmp(l),nb,nw)
               l=l+nw
               if (l.gt.ntmp) call error('eetsed',&
                 'insufficient storage for TAB1 data.',' ')
            enddo
         else
            call error('eetsed','unsupported scattering law',' ')
         endif
      enddo

      !--initialization complete
      nc=l-1
      if (nc.gt.ntmp) call error('eetsed',&
        'insufficient storage for TAB1 data.',' ')
      if (allocated(sedist)) deallocate(sedist)
      allocate(sedist(nc))
      sedist(:nc)=tmp(:nc)
      deallocate(tmp)
      nupm=0
      return
   endif

   !--normal entry.
   enext=emax
   idis=0
   ik=0
   do while (ik.lt.nk)
      ik=ik+1
      lnow=loc(ik)+1
      lf=nint(sedist(lnow+3))
      nr=nint(sedist(lnow+4))
      if (lf.eq.2) then
         sed(ik,:nl)=0.0
      else
         sed(ik,:ng)=0.0
         if (lf.eq.8) sed(ik+1,:ng)=0.0
      endif

      !--interpolate for fractional probability.
      ip=2
      ir=1
      call terpa(pe,ed,eihi,idisc,sedist(lnow),ip,ir)
      if (abs(eihi-enext).lt.enext*small.and.idisc.gt.idis) idis=idisc
      if (eihi.lt.enext*(1-small)) idis=idisc
      if (eihi.lt.enext*(1-small)) enext=eihi
      if (pe.gt.0.0) then
         nr=nint(sedist(lnow+4))
         np=nint(sedist(lnow+5))
         mnow=lnow+6+2*nr+2*np

         if (lf.eq.1) then
            !--tabulated subsection. interpolate for sed at e.
            nr=nint(sedist(mnow+4))
            ne=nint(sedist(mnow+5))
            nnow=mnow+6+2*nr
            nbt=nint(sedist(mnow+6))
            jnt=nint(sedist(mnow+7))
            if (jnt.eq.2) jnt=22 !force unit base interpolation if lin-lin
            klo=nnow
            elo=sedist(klo+1)
            nplo=nint(sedist(klo+5))
            khi=klo+6+2*nplo
            ehi=sedist(khi+1)
            nphi=nint(sedist(khi+5))
            xlo=sedist(klo+4+2*nplo)
            xhi=sedist(khi+4+2*nphi)
            nne=1
            do while (nne.lt.ne.and.ed.gt.ehi*(1+small))
               nne=nne+1
               klo=khi
               elo=ehi
               nplo=nphi
               xlo=xhi
               khi=klo+6+2*nplo
               ehi=sedist(khi+1)
               nphi=nint(sedist(khi+5))
               xhi=sedist(khi+4+2*nphi)
            enddo
            !--unit base. A special version of intega has to be used.
            if (jnt.ge.21) then
               knt=jnt-20
               call terp1(elo,xlo,ehi,xhi,ed,xend,knt)
               do ig=1,ng
                  e1=eg(ig)
                  e1lo=e1*xlo/xend
                  e1hi=e1*xhi/xend
                  e2=eg(ig+1)
                  e2lo=e2*xlo/xend
                  e2hi=e2*xhi/xend
                  ip=2
                  call intega_e(flo,e1lo,e2lo,sedist(klo),ip,knt)
                  ip=2
                  call intega_e(fhi,e1hi,e2hi,sedist(khi),ip,knt)
                  call terp1(elo,flo,ehi,fhi,ed,fe,knt)
                  sed(ik,ig)=sed(ik,ig)+pe*fe
               enddo
            !--other schemes not implemented.
            else
               call error('eetsed','corresponding points and Cartesian'&
               &//' schemes not implemented.',' ')
            endif
            if (jnt.eq.1.and.ehi.lt.enext) idisc=1
            if (ehi.lt.enext*(1-small)) enext=ehi

         else if (lf.eq.2) then
            !--set Legendre quadrature
            allocate(mu_lim(nmu+1),mu(nmu))
            allocate(wmu(nmu),mu1(nmu))
            call lgwt(nmu,-1.0d0,1.0d0,mu1,wmu)
            mu_lim(1)=-1.0d0;
            do imu=1,nmu
               mu(imu)=mu1(nmu-imu+1)
               mu_lim(imu+1)=mu_lim(imu)+wmu(imu)
            enddo
            mu_lim(nmu+1)=1.0d0
            deallocate(mu1,wmu)

            !--tabulated subsection. interpolate for sed at e.
            nr=nint(sedist(mnow+4))
            ne=nint(sedist(mnow+5))
            nnow=mnow+6+2*nr
            nbt=nint(sedist(mnow+6))
            jnt=nint(sedist(mnow+7))
            if (jnt.eq.2) jnt=22 !force unit base interpolation if lin-lin
            klo=nnow
            elo=sedist(klo+1)
            nplo=nint(sedist(klo+5))
            khi=klo+6+2*nplo
            ehi=sedist(khi+1)
            nphi=nint(sedist(khi+5))
            nne=1
            do while (nne.lt.ne.and.ed.gt.ehi*(1+small))
               nne=nne+1
               klo=khi
               elo=ehi
               khi=klo+6+2*nphi
               ehi=sedist(khi+1)
               nphi=nint(sedist(khi+5))
            enddo
            allocate(smol(nmu))
            smol(:nmu)=0.0d0
            !--unit base. A special version of intega has to be used.
            if (jnt.ge.21) then
               knt=jnt-20
               do imu=1,nmu
                  e1=mu_lim(imu)
                  e2=mu_lim(imu+1)
                  ip=2
                  call intega_e(flo,e1,e2,sedist(klo),ip,knt)
                  ip=2
                  call intega_e(fhi,e1,e2,sedist(khi),ip,knt)
                  call terp1(elo,flo,ehi,fhi,ed,fe,knt)
                  smol(imu)=smol(imu)+pe*fe
               enddo
            !--other schemes not implemented.
            else
               call error('eetsed','corresponding points and Cartesian'&
               &//' schemes not implemented.',' ')
            endif
            if (jnt.eq.1.and.ehi.lt.enext) idisc=1
            if (ehi.lt.enext*(1-small)) enext=ehi

            !--normalization
            asum=sum(smol(:nmu))
            smol(:nmu)=smol(:nmu)/asum

            !--compute Legendre moments
            do imu=1,nmu
               call legndr(mu(imu),pl,nl)
               do il=1,nl
                  sed(ik,il)=sed(ik,il)+smol(imu)*pl(il)
               enddo
            enddo
            deallocate(smol,mu,mu_lim)

         else if (lf.eq.8) then
            ip=2
            ir=1
            call terpa(e1,ed,enext,idis,sedist(mnow),ip,ir)
            do ig=1,ng
               e2=eg(ig+1)
               if (ig.eq.ng) e2=ebig
               if (ed-e1.lt.e2) then
                  sed(ik,ig)=1.0
                  sed(ik+1,ig)=ed-e1
                  exit
               endif
            enddo
            ik=ik+1
         endif
      endif
   enddo
   return
   contains
      subroutine intega_e(f,x1,x2,a,ip,jnt)
      !--------------------------------------------------------------------
      ! Integrate from x1 to x2 in the listio record packed in a.
      ! Assume function is zero outside the range of the table.
      ! The contribution of each panel in the data is computed
      ! analytically in function gral. On entry, ip is the starting
      ! estimate for the first data point greater than x1. Initialize
      ! it to 2 before first call. jnt is the interpolation scheme.
      !--------------------------------------------------------------------
      ! externals
      real(kr)::f,x1,x2
      real(kr)::a(*)
      integer::ip,jnt
      ! internals
      integer::np,jp
      real(kr)::xlo,xhi

      !--initialize integral.  set up limits and pointers
      knt=jnt
      if (jnt.ge.21) knt=jnt-20
      f=0
      np=nint(a(6))
      if (ip.gt.np) ip=np
      jp=5+2*ip
      if (a(2).eq.0.0) jp=jp+2

      !--locate first data point greater than x1
      110 continue
      if (x1.lt.a(jp)) go to 120
      if (ip.eq.np) go to 170
      ! move up
      jp=jp+2
      ip=ip+1
      go to 110
      120 continue
      if (x1.ge.a(jp-2)) go to 130
      if (ip.gt.2) go to 125
      if (x2.le.a(jp-2)) go to 170
      xlo=a(jp-2)
      go to 150
      125 continue
      ! move down
      jp=jp-2
      ip=ip-1
      go to 110

      !--accumulate contributions to integral
      130 continue
      xlo=x1
      150 continue
      xhi=a(jp)
      if (xhi.gt.x2) xhi=x2
      f=f+gral(a(jp-2),a(jp-1),a(jp),a(jp+1),xlo,xhi,knt)
      if (xhi.eq.x2) go to 170
      if (ip.eq.np) go to 170
      xlo=xhi
      jp=jp+2
      ip=ip+1
      go to 150

      !--integral is complete
      170 continue
      return
      end subroutine intega_e
   end subroutine eetsed

end module electm
