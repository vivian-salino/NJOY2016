module reskm
  ! provides subroutine reskr for NJOY2016
  use locale
  implicit none
  private
  integer::nin1,nin2,nout
  public reskr
  public::resk_type

  !-----------------------------------------------------------------------------
  !> resonance elastic scattering kernel type
  !> store the basic paramters and the arguments which are dependent with
  !> temperature.
  type :: resk_type
    logical :: is_init = .false.         !< flag to define whether this
                                         !< type is initialized.

    integer :: matb = 0                  !< standard MAT identifier in ENDF.
                                         !< (n_temp)

    integer :: n_temp = 0                !< # of temperatures
                                         !< It is initialized in the UI module.

    real(kr) :: za = 0._kr               !< charge parameter for an isotope

    real(kr) :: awr = 0._kr              !< Ratio of the mass of a
                                         !< particular isotope to that
                                         !< of the neutron.

    real(kr) :: ehi = 100._kr            !< upper incident energy boundary
                                         !< for the RESK calculation.

    real(kr) :: elo = 0._kr              !< lower incident energy boundary
                                         !< for the RESK calculation.

    integer :: n_order = 1               !< highest legendre order

    integer :: n_point = 1               !< # of check points for the
                                         !< multi-point lienarization method.

    integer :: nout                      !< unit # of output file

    real(kr) :: beta = 0.0_kr            !< 1/(1+awr)

    real(kr),allocatable :: temp(:)      !< the vaule of temperatures

    real(kr),allocatable :: kt(:)        !< temp*Boltzmann constant (n_temp)

    real(kr),allocatable :: bkt(:)       !< beta*kt (n_temp)

    real(kr),allocatable :: bkt_a(:)     !< bkt/awr (n_temp)

    real(kr),allocatable :: a1_kt(:)     !< (awr+1)/kt (n_temp)

    real(kr),pointer :: en_list(:)       !< initial secondary energy grids

  contains

    procedure, pass :: calc => calculation
    procedure, pass :: ker_calc => kernel_calculation
    procedure, pass :: dis_calc
    procedure, pass :: mt300_outp
    procedure, pass :: en_list_append
    procedure, pass :: clear
  endtype

  !-----------------------------------------------------------------------------
  !> cont format data type
  type :: cont_type
    real(kr) :: c(2) = 0.0_kr                   !< cont format parameter

    integer :: d(4) = 0                         !< cont format parameter
  endtype

  !-----------------------------------------------------------------------------
  !> TAB2 format data type
  type :: tab2_type
    real(kr) :: c(2)  = 0.0_kr                  !< tab2 format parameter

    integer :: d(10) = 0                        !< tab2 format parameter
  endtype

  !-----------------------------------------------------------------------------
  !> tab1 format data type
  type :: tab1_type
    real(kr) :: c(2)  = 0.0_kr                  !< tab1 format parameter

    integer :: d(22) = 0                        !< tab1 format parameter

    real(kr),allocatable :: tab1(:,:)           !< tab1 data
    real(kr),allocatable :: list(:,:)           !< list data

    real(kr) :: low = 0._kr,high = 0._kr        !< lower and higher x value
                                                !< for interpolation

    integer :: idis = 0                     !< flag of discontinuous point.

    integer :: ip = 0                       !< lower interpolation pair
  contains
    procedure, pass :: terp1 => interpolate_tab1 !< interpolate tab1 method
  endtype
  type(tab1_type) :: tab1_mf3mt2                   !< tab1 data for mf3mt2 at 0K
  type(tab1_type) :: tab1_mf3mt2_br                !< tab1 data for mf3mt2 > 0K
  type(tab1_type) :: tab1_mf3mt300                 !< tab1 data for mf3mt300
  type(cont_type) :: cont_mf6mt300                 !< cont data for mf6mt300
  type(tab1_type), allocatable :: tab1_mf6mt300(:) !< tab1 data for mf6mt300
  type(tab2_type) :: tab2_mf6mt300                 !< tab2 data for mf6mt300

!>==============================other information=============================<!
!< Note: The reaction types (MF3MT300 and MF6MT300) are added into the output
!< PENDF file where only the moments of different orders are stored. The
!< MF6MT300 data can solely utilized by the groupr module for solving the
!< neutron slowing-down equation considering the up-scattering effect;
!>==============================other information=============================<!

  !< Boltzmann constant/eV*K^-1
  real(kr), parameter :: bk = 8.617342e-5_kr
  !< neutron mass/amu
  real(kr), parameter :: mass_n = 1.00866491588_kr
!===============================================================================

contains

  subroutine reskr
    !-------------------------------------------------------------------
    !
    ! Resonance elastic scattering kernel processing
    !
    ! The resonance elastic scattering kernel (RESK) formulations for
    ! anisotropic scattering up to any Legendre order is used to
    ! represent the exact Doppler broadened energy transfer kernels.
    ! A semi-analytical integration method is applied to perform the
    ! RESK calculations. Combining with the RESK calculations, a
    ! linearization algorithm is proposed to generate the RESK
    ! interpolation tables.
    !
    ! The results are written out in pendf format with each
    ! temperature represented as a different MAT. Dictionaries
    ! are corrected to reflect the effects of thinning.
    !
    ! Authors: Jialong Xu, Tiejun Zu (original version -- 2019)
    !          Alain Hebert (adapted to Njoy2012 in 2021)
    !
    !---input specifications (free format)---------------------------
    !
    ! card 1
    !    nin1     input pendf tape containing 0K data
    !    nin2     input pendf tape containing Doppler broadened data
    !    nout     output pendf tape
    ! card 2
    !    mat1     material to broadened
    !    ntemp2   number of final temperatures (default=1)
    ! card 3
    !    elo      lower incident energy boundary for the RESK calculation
    !    ehi      upper incident energy boundary for the RESK calculation
    !    lord     maximum legendre order
    ! card 4
    !    temp2    final temperatures (deg Kelvin)
    ! card 5
    !    mat1     next MAT number to be processed with these
    !             parameters. Terminate with mat1=0.
    !
    !-------------------------------------------------------------------
    use mainio  ! provides nsysi,nsyso,nsyse
    use util    ! provides openz,timer,error,mess,repoz,closz,loada,finda
    use endf    ! provides endf routines and variables
    ! internals
    integer::i,mat1,ntemp2,lord,loop,nb,nw,it,lru,ner
    integer::nwscr=10000000
    real(kr)::elo,ehi,time
    real(kr),dimension(:),allocatable::temp2,scr
    class(resk_type),pointer :: this
    character(4)::tt(17)
    real(kr)::rt(17)
    real(kr)::thnmax
    equivalence(tt(1),rt(1))

    !--read and display user input.
    call timer(time)
    write(nsyso,'(/&
     &'' reskr...resonance elastic scattering  broadening of endf data'',&
     &28x,f8.1,''s'')') time
    write(nsyse,'(/'' reskr...'',60x,f8.1,''s'')') time
    read(nsysi,*) nin1,nin2,nout
    if (nin1 < 0.and.nout > 0)&
      call error('reskr','nin1 and nout must be same mode.',' ')
    if (nin1 > 0.and.nout < 0)&
      call error('reskr','nin1 and nout must be same mode.',' ')
    if (nin2 < 0.and.nout > 0)&
      call error('reskr','nin2 and nout must be same mode.',' ')
    if (nin2 > 0.and.nout < 0)&
      call error('reskr','nin2 and nout must be same mode.',' ')
    call openz(nin1,0)
    call openz(nin2,0)
    call openz(nout,1)
    ntemp2=1
    read(nsysi,*) mat1,ntemp2
    read(nsysi,*) elo,ehi,lord
    allocate(temp2(ntemp2))
    read(nsysi,*) (temp2(i),i=1,ntemp2)
    write(nsyso,'(/&
     &'' unit for input 0K pendf tape ......... '',i10/&
     &'' unit for input broadened pendf tape .. '',i10/&
     &'' unit for output pendf tape ........... '',i10/&
     &'' legendre order ....................... '',i10/&
     &'' material to be processed ............. '',i10/&
     &'' number of final temperatures ......... '',i10)')&
     nin1,nin2,nout,lord,mat1,ntemp2
    write(nsyso,'(&
     &'' final temperatures ................... '',1p,e10.3/&
     &(40x,e10.3))') (temp2(i),i=1,ntemp2)

    !--allocate storage.
    allocate(this)
    allocate(scr(nwscr))

    !--copy through to desired material
    nsh=0
    loop=0
    this%matb=mat1
    call tpidio(nin2,nout,0,scr,nb,nw)
   110 continue
    call contio(nin2,0,0,scr,nb,nw)
    if (math == mat1) go to 120
    if (math < 0)&
      call error('reskr','desired material not on pendf tape.',' ')
    call contio(0,nout,0,scr,nb,nw)
    call tomend(nin2,nout,0,scr)
    go to 110
   120 continue
    call findf(mat1,1,451,nin2)

    this%za=real(c1h)
    this%awr=real(c2h)
    !ner=n1h
    call contio(nin2,0,0,scr(7),nb,nw)
    call contio(nin2,0,0,scr(7),nb,nw)
    if (n1h /= 0) then
      iverf=4
    else if (n2h == 0) then
      iverf=5
    else
      iverf=6
    endif
    call skiprz(nin2,-1)
    write(nsyso,'(/'' pendf uses endf-'',i1,'' format'')') iverf
    if (iverf >= 5) call contio(nin2,0,0,scr,nb,nw)
    if (iverf >= 6) call contio(nin2,0,0,scr,nb,nw)
    call hdatio(nin2,0,0,scr,nb,nw)
    do i=1,17
      rt(i)=scr(6+i)
    enddo
    write(nsyso,'(/&
     &'' processing pendf mat '',i4/1x,66(''-'')/1x,17a4)')&
     this%matb,(tt(i),i=1,17)

    ! execute resk
    write(nsyso,'(75x,"init.")')
    this%matb=mat1
    this%nout=nout
    this%n_order=lord
    this%elo=real(elo)
    this%ehi=real(ehi)
    this%n_temp=ntemp2
    allocate(this%temp(ntemp2))
    this%temp(:ntemp2)=real(temp2(:ntemp2))

    if (this%is_init) then
      call mess('reskr','this module has been initialized',' ')
      return
    endif

    !> Check the Z #. The up-scattering effect is dependent with the mass of
    !> nuclei. So, when the Z # is large, the effect is strong. For the light
    !> nuclides, it is unnecessary to perform the resk calculation and free gas
    !> calculation in therm_calc is recomended. The Z # in recomended to be
    !> larger than 56.
    !> This checking procedure is performed in the UI module.

    !> check ehi, considering the lower boundary of URR.
    if (this%ehi > 200._kr)then
      this%ehi = 200._kr
      call mess('reskr','ehi is changed to 200.0 eV',' ')
    endif

    call findf(mat1,2,151,nin1)
    if (mat1 == -1) call error('reskr','MF2 MT151 not found on pendf(0K)',' ')
    call contio(nin1,0,0,scr,nb,nw)
    call contio(nin1,0,0,scr,nb,nw)
    ner = n1h
    call contio(nin1,0,0,scr,nb,nw)
    lru = l1h
    thnmax = real(c2h)
    if (this%ehi > thnmax)then
      this%ehi = thnmax
      write(nsyso,'("ehi is changed to ",f6.1," eV")')thnmax
    endif
    !> set other basic parameters.
    this%beta = 1.0_kr+1.0_kr/this%awr
    allocate(this%kt(this%n_temp))
    allocate(this%bkt(this%n_temp))
    allocate(this%bkt_a(this%n_temp))
    allocate(this%a1_kt(this%n_temp))
    do it = 1,this%n_temp
      this%kt(it) = this%temp(it)*bk
      this%bkt(it) = this%beta*this%kt(it)
      this%bkt_a(it) = this%bkt(it)/this%awr
      this%a1_kt(it) = (this%awr+1.0_kr)/this%kt(it)
    enddo
    this%is_init = .true.

    if (this%is_init)then
      write(nsyso,'(75x,"calc.")')
      !------------------------
      call this%calc(nwscr,scr)
      !------------------------
      call this%clear()
    endif
    mat1=0

    !--loop over desired materials.
    read(nsysi,*) mat1
    if (mat1 == 0) go to 410
    loop=loop+1
    go to 110

    !--reskr is finished
   410 continue
    nsh=0
    call totend(nin2,nout,0,scr)
    deallocate(scr)
    deallocate(temp2)
    call repoz(nout)
    call repoz(nin2)
    call repoz(nin1)
    call closz(nout)
    call closz(nin2)
    call closz(nin1)
    call timer(time)
    write(nsyso,'(69x,f8.1,''s''/1x,77(''*''))') time
    return
  end subroutine reskr

!-------------------------------------------------------------------------------
!> main calculation.
!>
!> init. the incident grids to calculate kernels and linearized the grids
!> based on the multi-point liearization method.
!> calc. the moment distributions of different Legendre orders at each incident
!> energy.
!>
!> @param this - inout, resk_type data
!-------------------------------------------------------------------------------
  subroutine calculation(this,nwscr,scr)
    use endf    ! provides endf routines and variables
    use util    ! provides openz,timer,error,mess,repoz,closz,loada,finda
    use mainio
    ! externals
    class(resk_type), intent(inout) :: this
    integer::nwscr
    real(kr),dimension(nwscr)::scr
    ! internals
    integer :: it,ie,nnp0,nnp,matb,l,nb
    real(kr),dimension(:),allocatable::tmp
    integer::nscr2
    logical::lido
    real(kr) :: x2,x1,xm,test,dely,e,ym,eb,judge
    integer :: iiflag,iflag,i,j,k,stack_num,iresk,iself,loop_e,ne,order,&
    nc,nd,nwd,nx,loop_order,ii,loop_point,n_2,nw,ncn,ie1,ie2
    real(kr),allocatable :: en_stack(:),xs_stack(:),init_e_grid(:),e_grid(:)
    real(kr),allocatable :: kernel(:),em_point(:),y1(:),y2(:),self(:)
    real(kr),allocatable :: en1_point(:),en2_point(:)
    real(kr),dimension(:),allocatable::c,d
    integer, parameter :: stack_max = 500000
    real(kr), parameter :: eps=1.e-7_kr
    real(kr), parameter :: tol = 1.e-3_kr
    integer, parameter :: imax = 50
    real(kr), parameter :: small = 1.e-10_kr
    real(kr), parameter :: judge_fac = 0.05_kr
    real(kr) :: x(imax),time,x1_kr,y1_kr,x2_kr,y2_kr,value
    character(len = 79) :: errmsg

    !--duplication of nin2 data.
    matb = this%matb
    allocate(tmp(npage+50))
    nscr2=14
    if (nin2 < 0) nscr2=-nscr2
    call repoz(nscr2)
    do it = 1,this%n_temp
      call findf(matb,3,2,nin2)
      if (matb == -1) call error('calculation', &
         'MF3 MT2 not found on broadened pendf',' ')
      nsc=1
      math=1
      call afend(0,nscr2)
      call tosend(nin2,0,nscr2,tmp)
      call amend(0,nscr2)
      call tomend(nin2,0,0,tmp)
    enddo
    call atend(0,nscr2)
    deallocate(tmp)
    call repoz(nscr2)
    call repoz(nin2)

    !> generate the incident energy grid.
    !> the initial incident energy grid is obtained from the MF3MT2(elastic
    !> scattering XS)
    call findf(matb,3,2,nin1)
    if (matb == -1) call error('calculation', &
       'MF3 MT2 not found on pendf(0K)',' ')
    call contio(nin1,0,0,scr,nb,nw)
    l=1
    call tab1io(nin1,0,0,scr(l),nb,nw)
    l=l+nw
    if (l > nwscr) call error('calculation',&
      'insufficient storage for TAB1 data on nin1.',' ')
    do while (nb /= 0)
      call moreio(nin1,0,0,scr(l),nb,nw)
      l=l+nw
      if (l > nwscr) call error('calculation',&
        'insufficient storage for TAB1 data on nin1.',' ')
    enddo
    allocate(tab1_mf3mt2%tab1(2,(l-8)/2))
    do ii = 1,(l-8)/2
      tab1_mf3mt2%tab1(1,ii)=scr((ii-1)*2+9)
      tab1_mf3mt2%tab1(2,ii)=scr((ii-1)*2+10)
    enddo
    tab1_mf3mt2%c(1) = 0._kr
    tab1_mf3mt2%c(2) = 0._kr
    tab1_mf3mt2%d(1:2) = 0
    tab1_mf3mt2%d(3) = 1
    tab1_mf3mt2%d(4) = (l-8)/2
    tab1_mf3mt2%d(5) = (l-8)/2
    tab1_mf3mt2%d(6) = 2

    ie1=1
    ie2=(l-8)/2
    do ie = 1,ie2+1
      if (scr((ie-1)*2+9) > this%elo) exit
      ie1=ie1+1
    enddo
    do ie = (l-8)/2,ie1,-1
      if (scr((ie-1)*2+9) < this%ehi) exit
      ie2=ie2-1
    enddo
    nnp0=ie2-ie1+3
    allocate(init_e_grid(nnp0))
    init_e_grid(1)=this%elo
    do ii = 2,nnp0-1
      ie=(ie1+ii-2)
      init_e_grid(ii)=scr((ie-1)*2+9)
    enddo
    init_e_grid(nnp0)=this%ehi

    !> set some variables for the multi-point linearization method.
    !> real value of the moment with 0th order at the check points for the
    !> specified incident energy.
    allocate(kernel(this%n_point))
    !> real value of the moment with 0th order at the check points for the
    !> higher incident energy.
    allocate(y1(this%n_point))
    !> real value of the moment with 0th order at the check points for the
    !> lower incident energy.
    allocate(y2(this%n_point))
    !> real value of the moment with 0th order at the check points for the
    !> mid incident energy.
    allocate(self(this%n_point))
    !> the secondary energies for 2D interpolation for the higher incident
    !> energy.
    allocate(en1_point(this%n_point))
    !> the secondary energies for 2D interpolation for the lower incident
    !> energy.
    allocate(en2_point(this%n_point))
    !> the secondary energies (check points) defined by [ln(Emax/E)]/N and
    !> [ln(E/Emin)]/N for the mid incident energy.
    allocate(em_point(this%n_point))
    order = this%n_order
    !> the final incident energy grid.
    allocate(e_grid(stack_max))
    !> the secondary incident energy grid. (grid for e1, grid for e2, ...)
    allocate(en_stack(stack_max))
    !> the moment of different Legendre order corresponding to en_stack.
    allocate(xs_stack((order+1)*stack_max))
    !> the moment of 0th Legendre order and equally probable bins corresponding
    !> to en_stack.
    n_2 = this%n_point/2

    allocate(tmp(npage+50))
    nsh=0
    call tpidio(nin2,0,0,tmp,nb,nw)
    do it = 1,this%n_temp
      write(nsyso,'(" Processing resonance elastic scattering kernels for ",   &
      & f6.1," Kelvin")')this%temp(it)
      if (this%n_temp > 1) then
        call timer(time)
        write(nsyse,'(f11.1,'' deg'',54x,f8.1,''s'')') this%temp(it),time
      endif

      ! recover broadened PENDF data on nscr2 unit
      call findf(matb,3,2,nscr2)
      if (matb == -1) call error('calculation', &
         'MF3 MT2 not found on broadened pendf)',' ')
      call contio(nscr2,0,0,scr,nb,nw)
      l=1
      call tab1io(nscr2,0,0,scr(l),nb,nw)
      l=l+nw
      if (l > nwscr) call error('calculation',&
         'insufficient storage for TAB1 data on nscr2.',' ')
      do while (nb /= 0)
        call moreio(nscr2,0,0,scr(l),nb,nw)
        l=l+nw
        if (l > nwscr) call error('calculation',&
         'insufficient storage for TAB1 data on nscr2.',' ')
      enddo
      call tomend(nscr2,0,0,scr(l))
      allocate(tab1_mf3mt2_br%tab1(2,(l-8)/2))
      do ii = 1,(l-8)/2
        tab1_mf3mt2_br%tab1(1,ii)=scr((ii-1)*2+9)
        tab1_mf3mt2_br%tab1(2,ii)=scr((ii-1)*2+10)
      enddo
      tab1_mf3mt2_br%c(1) = 0._kr
      tab1_mf3mt2_br%c(2) = 0._kr
      tab1_mf3mt2_br%d(1:2) = 0
      tab1_mf3mt2_br%d(3) = 1
      tab1_mf3mt2_br%d(4) = (l-8)/2
      tab1_mf3mt2_br%d(5) = (l-8)/2
      tab1_mf3mt2_br%d(6) = 2

      ie = 1
      x2 = init_e_grid(ie)
      stack_num = 0
      iiflag = 0
      do while (ie < nnp0)
        x1 = init_e_grid(ie+1)
        eb = 0.1_kr*x2*(1._kr-small)
        xm = (x1+x2)*.5_kr
        iiflag = 0
        if (xm /= x1 .and. xm /= x2) then
          em_point(1) = 0._kr
          call point_kernel_calc(this,xm,it,self,em_point)
          !> judge_fac is a factor to avoid the check when the moment of the
          !> check point is too small since it is difficult to meet the criteria
          !> when the true value of the moment is small. The small value of
          !> moment has little impact on the neutonics parameters.
          judge = self(this%n_point/2+1)*judge_fac
          do loop_point = 1,this%n_point
            en2_point(loop_point) = em_point(loop_point)-xm+x2
            en1_point(loop_point) = em_point(loop_point)-xm+x1
            if (en2_point(loop_point) < eb)then
              en2_point(loop_point)=em_point(loop_point)*eb/(xm-x2+eb)
              en1_point(loop_point)=em_point(loop_point)*(x1-x2+eb)/(xm-x2+eb)
            endif
          enddo
          call point_kernel_calc(this,x2,it,kernel,en2_point)
          y2 = kernel
          call point_kernel_calc(this,x1,it,kernel,en1_point)
          y1 = kernel
          !> self-scattering and lethargy poitns
          do loop_point = 1,this%n_point
            ym = (y1(loop_point)+y2(loop_point))*.5_kr
            test = tol*abs(ym)
            dely = abs(self(loop_point)-ym)
            if (dely > test .and. self(loop_point) > judge) then
              iiflag=1
              exit
            endif
          enddo
        endif
        if (iiflag == 1)then
          !> need to stack more points, and then skip to the next panel
          x(2) = x2
          x(1) = x1
          i = 2
          do while (i > 1)
            !> test for convergence.
            iflag = 0
            if (i > 1 .and. i < imax) then
              xm=(x(i-1)+x(i))*.5_kr
              eb = 0.1_kr*x(i)*(1._kr-small)
              if (xm /= x(i-1).and.xm /= x(i)) then
                em_point(1) = 0._kr
                call point_kernel_calc(this,xm,it,self,em_point)
                judge = self(this%n_point/2+1)*judge_fac
                do loop_point = 1,this%n_point
                  en2_point(loop_point) = em_point(loop_point)-xm+x(i)
                  en1_point(loop_point) = em_point(loop_point)-xm+x(i-1)
                  if (en2_point(loop_point) < eb)then
                    en2_point(loop_point)=em_point(loop_point)*eb/(xm-x(i)+eb)
                    en1_point(loop_point)=em_point(loop_point)*(x(i-1)-x(i)+   &
                    eb)/(xm-x(i)+eb)
                  endif
                enddo
                call point_kernel_calc(this,x(i),it,kernel,en2_point)
                y2 = kernel
                call point_kernel_calc(this,x(i-1),it,kernel,en1_point)
                y1 = kernel
                do loop_point = 1,this%n_point
                  ym = (y1(loop_point)+y2(loop_point))*.5_kr
                  test = tol*abs(ym)
                  dely = abs(self(loop_point)-ym)
                  if (dely > test .and. self(loop_point) > judge) then
                    iflag=1
                    exit
                  endif
                enddo
              endif
            endif
            !> fails tests.
            !> add midpoint to stack and continue.
            if (iflag == 1) then
              i=i+1
              x(i)=x(i-1)
              x(i-1)=xm
            !> passes tests.
            !> take top point off of the stack.
            else
              stack_num = stack_num+1
              if (stack_num > stack_max) then
                write(errmsg,'("storage exceeded when init. the grid, e = ",   &
                &es12.5)')e_grid(stack_num-1)
                call error('calculation',errmsg,' ')
              endif
              e_grid(stack_num) = x(i)
              i=i-1
            endif
          enddo
          x2 = x1
          y2 = y1
          ie = ie+1
        else
          !> extend the panel to the next point
          ie = ie+1
        endif
      enddo
      if (iiflag == 0)then
        stack_num = stack_num+1
        e_grid(stack_num) = x2
        stack_num = stack_num+1
        e_grid(stack_num) = x1
      else
        stack_num = stack_num+1
        e_grid(stack_num) = x(1)
      endif

      !> set mftp array for mf3mt300
      ncn = 0
      nnp = 0
      ie1=1
      ie2=(l-8)/2
      do ie = 1,ie2+1
        if (tab1_mf3mt2_br%tab1(1,ie) > this%elo) exit
        ie1=ie1+1
      enddo
      do ie = (l-8)/2,ie1,-1
        if (tab1_mf3mt2_br%tab1(1,ie) < this%ehi) exit
        ie2=ie2-1
      enddo
      nnp=ie2-ie1+3
      allocate(tab1_mf3mt300%tab1(2,nnp+2))
      tab1_mf3mt300%tab1(1,1)=this%elo
      x1_kr=tab1_mf3mt2_br%tab1(1,ie-1)
      x2_kr=tab1_mf3mt2_br%tab1(1,ie)
      y1_kr=tab1_mf3mt2_br%tab1(2,ie-1)
      y2_kr=tab1_mf3mt2_br%tab1(2,ie)
      call terp1(x1_kr,y1_kr,x2_kr,y2_kr,this%elo,value,2)
      tab1_mf3mt300%tab1(2,1)=real(value)
      do ii = 2,nnp-1
        ie=ie1+ii-2
        tab1_mf3mt300%tab1(1,ii)=tab1_mf3mt2_br%tab1(1,ie)
        tab1_mf3mt300%tab1(2,ii)=tab1_mf3mt2_br%tab1(2,ie)
      enddo
      tab1_mf3mt300%tab1(1,nnp)=this%ehi
      x1_kr=tab1_mf3mt2_br%tab1(1,ie)
      x2_kr=tab1_mf3mt2_br%tab1(1,ie+1)
      y1_kr=tab1_mf3mt2_br%tab1(2,ie)
      y2_kr=tab1_mf3mt2_br%tab1(2,ie+1)
      call terp1(x1_kr,y1_kr,x2_kr,y2_kr,this%ehi,value,2)
      tab1_mf3mt300%tab1(2,nnp)=real(value)

      tab1_mf3mt300%tab1(1,nnp+1)=this%ehi*1.00001_kr
      tab1_mf3mt300%tab1(2,nnp+1)=0.0_kr

      tab1_mf3mt300%tab1(1,nnp+2)=2.e7_kr
      tab1_mf3mt300%tab1(2,nnp+2)=0.0_kr
      tab1_mf3mt300%c(1) = this%temp(it)
      tab1_mf3mt300%c(2) = 0._kr
      tab1_mf3mt300%d(1:2) = 0
      tab1_mf3mt300%d(3) = 1
      tab1_mf3mt300%d(4) = nnp+2
      tab1_mf3mt300%d(5) = nnp+2
      tab1_mf3mt300%d(6) = 2
      deallocate(tab1_mf3mt2_br%tab1)

      !> calculate the kernels for the e_grid, set mftp array for mf6mt300
      ne = stack_num
      cont_mf6mt300%c(1) = this%za
      cont_mf6mt300%c(2) = this%awr
      cont_mf6mt300%d = 0
      cont_mf6mt300%d(1) = this%n_order
      tab2_mf6mt300%c(1) = this%temp(it)
      tab2_mf6mt300%c(2) = 0._kr
      tab2_mf6mt300%d(1:2) = 0
      tab2_mf6mt300%d(2) = 0
      tab2_mf6mt300%d(3) = 1
      tab2_mf6mt300%d(4) = ne
      tab2_mf6mt300%d(5) = ne
      tab2_mf6mt300%d(6) = 2
      ncn = ncn+3

      allocate(tab1_mf6mt300(ne))
      do loop_e = 1,ne
        e = e_grid(loop_e)
        write(nsyso,*)loop_e,ne,e
        call this%dis_calc(e,it,stack_max,en_stack,xs_stack,iresk,iself,order)
        !> copy the MF6MT300 data to tab1_mf6mt300
        tab1_mf6mt300(loop_e)%c(2) = e
        tab1_mf6mt300(loop_e)%d(3) = 1
        tab1_mf6mt300(loop_e)%d(4) = iresk
        tab1_mf6mt300(loop_e)%d(5) = iresk
        tab1_mf6mt300(loop_e)%d(6) = 2
        ncn = ncn+2
        allocate(tab1_mf6mt300(loop_e)%tab1(2,iresk))
        if (mod(iresk,3) == 0) then
          ncn = ncn+iresk/3
        else
          ncn = ncn+iresk/3+1
        endif
        ii = 1
        tab1_mf6mt300(loop_e)%tab1(1,1) = en_stack(1)
        tab1_mf6mt300(loop_e)%tab1(2,1) = 0._kr
        do i = 2,iresk-1
          ii = ii+order+1
          tab1_mf6mt300(loop_e)%tab1(1,i) = en_stack(i)
          tab1_mf6mt300(loop_e)%tab1(2,i) =  xs_stack(ii)
        enddo
        tab1_mf6mt300(loop_e)%tab1(1,iresk) = en_stack(iresk)
        tab1_mf6mt300(loop_e)%tab1(2,iresk) = 0._kr
        if (order > 0) then
          allocate(tab1_mf6mt300(loop_e)%list(order,iresk))
          do loop_order = 1,order
            ncn=ncn+1
            if (mod(iresk,6) == 0)then
              ncn = ncn+iresk/6
            else
              ncn = ncn+iresk/6+1
            endif
            tab1_mf6mt300(loop_e)%list(loop_order,1) = 0._kr
            do i = 2,iresk
              tab1_mf6mt300(loop_e)%list(loop_order,i) = &
                    xs_stack((i-2)*(order+1)+loop_order+1)
            enddo
          enddo
        endif
      enddo

      !--write new pendf tape.
      nsh=1
      call contio(nin2,nout,0,tmp,nb,nw)
      nx=n2h
      if ((iverf < 5).and.(nx > 0)) tmp(6)=tmp(6)+2
      if (iverf >= 5) call contio(nin2,nout,0,tmp,nb,nw)
      if (iverf >= 6) call contio(nin2,nout,0,tmp,nb,nw)
      call hdatio(nin2,0,0,tmp,nb,nw)
      if (iverf >= 5) then
         nx=n2h
         if (nx > 0) tmp(6)=tmp(6)+2
      endif
      call hdatio(0,nout,0,tmp,nb,nw)
      do while (nb /= 0)
         call moreio(nin2,nout,0,tmp,nb,nw)
      enddo
      nw=nx
      if (nw /= 0) then
        lido=.true.
        nc=6*nx+12
        nd=6*nx
        allocate(c(nc),d(nd))
        call dictio(nin2,0,0,d,nb,nw)
        j=0
        do i=1,nw,6
          if ((d(i+2) > 3).and.lido) then
            c(j+1)=0
            c(j+2)=0
            c(j+3)=3
            c(j+4)=300
            c(j+5)=5+(2*nnp-1)/6
            c(j+6)=0
            j=j+6
            c(j+1)=0
            c(j+2)=0
            c(j+3)=6
            c(j+4)=300
            c(j+5)=ncn
            c(j+6)=0
            j=j+6
            lido=.false.
          endif
          do k=1,6
            c(k+j)=d(k+i-1)
          enddo
          j=j+6
        enddo
        c(5)=c(5)+2
        nwd=j/6
        call dictio(0,nout,0,c,nb,nwd)
        deallocate(d,c)
      endif
      call hdatio(nin2,nout,0,tmp,nb,nw)
      do
        nsh=1
        call contio(nin2,nout,0,tmp,nb,nw)
        call contio(nin2,0,0,tmp,nb,nw)
        call skiprz(nin2,-1)
        if (mfh > 3) exit
        call tosend(nin2,nout,0,tmp)
        call skiprz(nout,-1)
        call asend(nout,0)
      enddo
      call skiprz(nout,-1)
      nsh=1
      call this%mt300_outp()
      do
        nsh=1
        call contio(nin2,nout,0,tmp,nb,nw)
        call contio(nin2,0,0,tmp,nb,nw)
        call skiprz(nin2,-1)
        if (math == 0) exit
        call tosend(nin2,nout,0,tmp)
        call skiprz(nout,-1)
        call asend(nout,0)
      enddo
      deallocate(tab1_mf6mt300)
      deallocate(tab1_mf3mt300%tab1)
      call closz(nscr2)

      write(nsyso,*) 'end of processing temperature =',it
    enddo
    nsh=0
    deallocate(tmp)
    deallocate(tab1_mf3mt2%tab1)
  endsubroutine calculation

!-------------------------------------------------------------------------------
!> calculate the resonance elastic scattering kernel/distribution at specified
!> incident energy.
!>
!> @param e - in, incident energy
!> @param it - in, loop # of temperature
!> @param stack_max - in, maximum stack #
!> @param en_stack - inout, secondary energy grids
!> @param xs_stack - inout, kernels for all legendre order at each energy grid
!> @param stack_num - inout, # of array size
!> @param iself - inout, the index in en_stack for self-scattering
!> @param order - in, absolute legendre order
!-------------------------------------------------------------------------------
  subroutine dis_calc(this,e,it,stack_max,en_stack,xs_stack,stack_num,iself,   &
  order)
    use util    ! provides openz,timer,error,mess,repoz,closz,loada,finda
    ! externals
    class(resk_type), intent(inout) :: this
    real(kr), intent(in) :: e
    integer, intent(in) :: it,stack_max
    real(kr), intent(inout), allocatable :: en_stack(:),xs_stack(:)
    integer, intent(inout) :: stack_num,iself
    integer, intent(in) :: order
    ! internals
    integer :: loop_beta,i,j,ii,iflag,loop_order,nElement
    real(kr) :: en,da,e_kt,f1,beta,kt,inte_xs
    real(kr) :: temp_beta(12)
    real(kr),allocatable :: init_en_grid(:),y(:,:),kernel(:)
    real(kr) :: enext,enxt,x1,x2,y1,y2
    integer, parameter :: imax = 50
    real(kr) :: x(imax)
    real(kr) :: rat,xm,ym,dely,test
    real(kr), parameter :: tol = 1.e-3_kr
    real(kr), parameter :: eps = 1.e-7_kr
    real(kr), parameter :: small = 1.e-10_kr
    real(kr), parameter :: emax = 1.e10_kr
    real(kr), parameter :: zero = 0._kr
    real(kr) :: efirst = 0._kr
    logical :: bool
    character(len = 79) :: errmsg

    !> init. the secondary energy grid
    temp_beta = (/0._kr,.0001_kr,.001_kr,.01_kr,.1_kr,2._kr,4._kr,             &
    6._kr,8._kr,10._kr,15._kr,25._kr/)
    allocate(y(order+1,imax),kernel(order+1))
    nElement=0
    do loop_beta = size(temp_beta),1,-1
      en = e-temp_beta(loop_beta)*this%kt(it)
      if (en > efirst)then
        call this%en_list_append(en,nElement)
      endif
    enddo
    do loop_beta = 2,size(temp_beta)
      en = e+temp_beta(loop_beta)*this%kt(it)
      call this%en_list_append(en,nElement)
    enddo
    allocate(init_en_grid(nElement))
    do i = 1,nElement
      init_en_grid(i) = this%en_list(i)
    enddo
    deallocate(this%en_list)
    stack_num = 0
    beta = this%beta
    kt = this%kt(it)
    f1 = beta*beta*sqrt(beta)*.25_kr/e
    e_kt = e/kt
    call this%ker_calc(e,efirst,f1,e_kt,it,order,init_en_grid,enext,kernel)
    x(2) = enext
    call this%ker_calc(e,x(2),f1,e_kt,it,order,init_en_grid,enext,kernel)
    y(:,2) = kernel
    !> prime the stack with the top of the next panel
    do while (enext < emax*(1._kr-small))
      x(1) = enext
      call this%ker_calc(e,x(1),f1,e_kt,it,order,init_en_grid,enext,kernel)
      y(:,1) = kernel
      i = 2
      bool = .false.
      do while (i > 1)
        !> test for convergence.
        iflag=0
        if (i > 1 .and. i < imax) then
          da=(x(i-1)-x(i))*(y(1,i-1)+y(1,i))*.5_kr
          rat=2._kr
          if (x(i) /= zero) rat=x(i-1)/x(i)
          if (rat > 1+tol*0.01_kr) then
             if (abs(da) >= eps) then
                xm=(x(i-1)+x(i))*.5_kr
                if (xm /= x(i-1).and.xm /= x(i)) then
                  call this%ker_calc(e,xm,f1,e_kt,it,order,init_en_grid,enext,&
                  kernel)
                  bool = .true.
                  do loop_order = 1,order+1
                    ym = (y(loop_order,i-1)+y(loop_order,i))*.5_kr
                    test = tol*abs(ym)
                    dely = abs(kernel(loop_order)-ym)
                    if (dely > test) then
                      iflag=1
                      exit
                    endif
                  enddo
                endif
             endif
          endif
        endif
        !> fails tests.
        !> add midpoint to stack and continue.
        if (iflag == 1) then
          i=i+1
          x(i)=x(i-1)
          y(:,i)=y(:,i-1)
          x(i-1)=xm
          y(:,i-1)=kernel
        !> passes tests.
        !> take top point off of the stack.
        else
          stack_num = stack_num+1
          if (stack_num > stack_max) then
            write(errmsg,'("storage exceeded at dis_calc, max = ",i8)')stack_max
            call error('dis_calc',errmsg,' ')
          endif
          if (x(i) == e) iself = stack_num
          en_stack(stack_num) = x(i)
          xs_stack((stack_num-1)*(order+1)+1:stack_num*(order+1)) = y(:,i)
          i=i-1
        endif
      enddo
      !> continue looping over panels.
      if (bool) then
        enxt = enext
        call this%ker_calc(e,enxt,f1,e_kt,it,order,init_en_grid,enext,kernel)
      endif
      x(2)=x(1)
      y(:,2)=y(:,1)
    enddo
    stack_num = stack_num+1
    if (stack_num > stack_max) then
      write(errmsg,'("storage exceeded at dis_calc, max = ",i8)')stack_max
      call error('dis_calc',errmsg,' ')
    endif
    en_stack(stack_num) = x(1)
    xs_stack((stack_num-1)*(order+1)+1:stack_num*(order+1)) = y(:,1)
    !> check zero data
    ii = 1
    y1 = xs_stack(ii)
    if (y1 == 0._kr)then
      do j = 2,stack_num
        ii = ii+order+1
        y1 = xs_stack(ii)
        if (y1 /= 0._kr)exit
      enddo
      xs_stack(1:(stack_num-j+1)*(order+1)) =                                  &
      xs_stack((j-1)*(order+1)+1:stack_num*(order+1))
      stack_num = stack_num-j+1
      en_stack(1:stack_num-j+1) = en_stack(j:stack_num)
    endif
    ii = (stack_num-1)*(order+1)+1
    y1 = xs_stack(ii)
    if (y1 == 0._kr)then
      do j = stack_num-1,2,-1
        ii = ii-order-1
        y1 = xs_stack(ii)
        if (y1 /= 0._kr) exit
      enddo
      stack_num = j
    endif
    !> normalization
    inte_xs = 0.0_kr
    x1 = en_stack(1)
    ii = 1
    y1 = xs_stack(ii)
    do j = 2,stack_num
      ii = ii+order+1
      x2 = en_stack(j)
      y2 = xs_stack(ii)
      inte_xs = inte_xs+(y1+y2)*(x2-x1)*.5_kr
      x1 = x2
      y1 = y2
    enddo
    xs_stack = xs_stack/inte_xs
  end subroutine dis_calc

!-------------------------------------------------------------------------------
!> calculate the free gas kernel for higher order with semi-analytical method.
!>
!> @param e,en - in, incident/secondary energy
!> @param f1 - in, beta*beta*sqrt(beta)*.25_kr/e
!> @param e_kt, - in, e/kt
!> @param it - in, temperature loop #
!> @parma order, in, Legendre order
!> @param init_en_grid - in, initial secondary energy grids
!> @param enext - inout, next secondary energy
!> @param kernel - out, desired kernel
!-------------------------------------------------------------------------------
  subroutine kernel_calculation(this,e,en,f1,e_kt,it,order,init_en_grid,enext,&
  kernel)
    ! externals
    class(resk_type), intent(inout) :: this
    real(kr), intent(in) :: e,en,f1,e_kt
    real(kr), intent(inout) :: enext
    integer, intent(in), optional :: order
    integer, intent(in) :: it
    real(kr),allocatable, intent(in) :: init_en_grid(:)
    real(kr),allocatable, intent(inout) :: kernel(:)
    ! internals
    real(kr),allocatable :: inte(:),sum_value(:)
    integer :: i,nen
    real(kr) :: beta,eps_max,eps_min,kt,bkt_a,a1_kt,efirst
    real(kr) :: t1,t2,t3,t_p,t_n,e1,e2,e3,mu,bkt,ra
    real(kr), parameter :: erf_limit = 6.3_kr
    real(kr), parameter :: step = 1.01_kr
    real(kr), parameter :: emax = 1.e10_kr
    real(kr), parameter :: small=1.e-10_kr
    real(kr), parameter :: sqrtpi = 1.772453850905516_kr
    real(kr) :: va_max,vn,vn_max,vn_min,rbkt_a
    real(kr) :: eps2_max,eps2_min
    real(kr), save :: elo_limit,ehi_limit

    beta = this%beta
    kt = this%kt(it)
    a1_kt = this%a1_kt(it)
    bkt = this%bkt(it)
    bkt_a = this%bkt_a(it)
    rbkt_a = 1/bkt_a
    ra = 1/this%awr
    kernel = 0.0_kr
    if (en == 0.0_kr)then
      vn = sqrt(2._kr*e/mass_n)
      va_max = 4.3_kr*sqrt(2._kr*kt/(this%awr*mass_n))
      mu = (va_max-this%awr*va_max)/(2._kr*vn)
      if (mu < -1._kr)then
        vn_max = (2._kr*this%awr*va_max+(this%awr-1._kr)*vn)/(this%awr+1._kr)
      else
        vn_max = sqrt(this%awr*va_max*va_max+vn*vn)
      endif
      ehi_limit = 0.5_kr*mass_n*vn_max*vn_max
      mu = (this%awr-1._kr)*vn/(2._kr*this%awr*va_max)
      if (mu > 1._kr)then
        vn_min = ((this%awr-1._kr)*vn-2._kr*this%awr*va_max)/(this%awr+1._kr)
      else
        vn_min = 0._kr
      endif
      if (vn_min == 0._kr) then
        elo_limit = 1.e-9_kr
      else
        elo_limit = 0.5_kr*mass_n*vn_min*vn_min
      endif
      enext = elo_limit
      return
    elseif(en >= emax)then
      enext = emax
      kernel = 0._kr
      return
    endif
    allocate(inte(0:order))
    inte = 0.0_kr
    allocate(sum_value(0:order))
    sum_value = 0._kr
    !> calculate kernel
    eps_min = sqrt(a1_kt*min(en,e))
    eps_max = sqrt(a1_kt*max(en,e))
    t_p = 0.5_kr*(eps_max+eps_min)
    t_n = 0.5_kr*(eps_max-eps_min)
    eps2_min = eps_min*eps_min
    eps2_max = eps_max*eps_max
    !> define the whole integration area
    t1 = max(t_n,eps_max-erf_limit)
    t2 = t_p
    t3 = erf_limit+eps_min
    if (t1 > t2 .or. t2 > t3)then
      kernel = 0._kr
      nen = size(init_en_grid)
      if (en == init_en_grid(nen) .or. en >= ehi_limit)then
        enext = emax
      elseif (en == init_en_grid(nen-1))then
        enext = init_en_grid(nen)
      else
        do i = 1,nen-1
          if (init_en_grid(i) > en)then
            enext = init_en_grid(i)
            exit
          endif
        enddo
      endif
      return
    endif
    !> define the integration sub-area
    e1 = t1*t1*bkt_a
    e2 = t2*t2*bkt_a
    e3 = t3*t3*bkt_a
    if (e1 < 1.e-9_kr) e1 = 1.e-9_kr
    efirst = tab1_mf3mt2%tab1(1,1)
    sum_value = 0._kr
    if (e3 < efirst)then
      !> whole 1/v approximation.
      call part_inte(e1,e2,bkt_a,rbkt_a,eps_max,eps_min,eps2_max,eps2_min,     &
      ra,e_kt,1,1,order,inte)
      sum_value(:) = sum_value(:)+inte(:)
      call part_inte(e2,e3,bkt_a,rbkt_a,eps_max,eps_min,eps2_max,eps2_min,     &
      ra,e_kt,2,1,order,inte)
      sum_value(:) = sum_value(:)+inte(:)
    elseif (e2 < efirst .and. efirst <= e3)then
      !> first part is 1/v, second part is 1/v and point-wise XS.
      call part_inte(e1,e2,bkt_a,rbkt_a,eps_max,eps_min,eps2_max,eps2_min,ra   &
        ,e_kt,1,1,order,inte)
      sum_value(:) = sum_value(:)+inte(:)
      call part_inte(e2,efirst,bkt_a,rbkt_a,eps_max,eps_min,eps2_max,eps2_min, &
        ra,e_kt,2,1,order,inte)
      sum_value(:) = sum_value(:)+inte(:)
      call part_inte(efirst,e3,bkt_a,rbkt_a,eps_max,eps_min,eps2_max,eps2_min, &
        ra,e_kt,2,0,order,inte)
      sum_value(:) = sum_value(:)+inte(:)
    elseif (e1 < efirst .and. efirst <= e2)then
      !> first part is 1/v and point-wise XS, second part is point-wise XS.
      call part_inte(e1,efirst,bkt_a,rbkt_a,eps_max,eps_min,eps2_max,eps2_min, &
        ra,e_kt,1,1,order,inte)
      sum_value(:) = sum_value(:)+inte(:)
      call part_inte(efirst,e2,bkt_a,rbkt_a,eps_max,eps_min,eps2_max,eps2_min, &
        ra,e_kt,1,0,order,inte)
      sum_value(:) = sum_value(:)+inte(:)
      call part_inte(e2,e3,bkt_a,rbkt_a,eps_max,eps_min,eps2_max,eps2_min,ra   &
        ,e_kt,2,0,order,inte)
      sum_value(:) = sum_value(:)+inte(:)
    else
      !> whole point-wise XS.
      call part_inte(e1,e2,bkt_a,rbkt_a,eps_max,eps_min,eps2_max,eps2_min,ra   &
        ,e_kt,1,0,order,inte)
      sum_value(:) = sum_value(:)+inte(:)
      call part_inte(e2,e3,bkt_a,rbkt_a,eps_max,eps_min,eps2_max,eps2_min,ra   &
        ,e_kt,2,0,order,inte)
      sum_value(:) = sum_value(:)+inte(:)
    endif
    sum_value = sum_value*f1
    if (order > 0)sum_value(1) = sum_value(1)*.25_kr/(sqrtpi*eps_max*eps_min)
    kernel(1:order+1) = sum_value(0:order)
    nen = size(init_en_grid)
    if (en == init_en_grid(nen) .or. en >= ehi_limit)then
      enext = emax
    elseif (en == init_en_grid(nen-1))then
      enext = init_en_grid(nen)
    else
      do i = 1,nen-1
        if (init_en_grid(i) > en)then
          enext = init_en_grid(i)
          exit
        endif
      enddo
    endif
  endsubroutine kernel_calculation

!>------------------------------------------------------------------------------
!> calculate the 0th element for self-scattering and lethargy-decrement points
!> of up/down scattering.
!>
!> @param this - inout, resk_type data
!> @param e - in, incident energy
!> @param it - in, temperature loop #
!> @param kernel - inout, result
!> @param en_point - inout, lethargy-decrement points
!>------------------------------------------------------------------------------
  subroutine point_kernel_calc(this,e,it,kernel,en_point)
    ! externals
    class(resk_type), intent(inout) :: this
    real(kr), intent(in) :: e
    integer, intent(in) :: it
    real(kr),allocatable, intent(inout) :: kernel(:)
    real(kr),allocatable, intent(inout) :: en_point(:)
    ! internals
    real(kr),allocatable :: inte(:)
    integer :: loop_point,n_2
    real(kr) :: beta,eps_max,eps_min,kt,bkt_a,a1_kt,f1,sum,xs_tk
    real(kr) :: t1,t2,t3,t_p,t_n,e1,e2,e3,e_kt
    real(kr) :: bkt,rbkt_a,ra,en_step,eps2_max,eps2_min
    real(kr) :: vn,va_max,mu,vn_max,ehi_limit,vn_min,elo_limit,efirst,tfirst
    real(kr), parameter :: erf_limit = 5.9_kr
    real(kr), parameter :: step = 1.01_kr
    real(kr), parameter :: emax = 1.e10_kr
    real(kr), parameter :: small=1.e-10_kr

    allocate(inte(0:0))
    kt = this%kt(it)
    if (en_point(1) == 0.0_kr)then
      vn = sqrt(2._kr*e/mass_n)
      va_max = 4.3_kr*sqrt(2._kr*kt/(this%awr*mass_n))
      mu = (va_max-this%awr*va_max)/(2._kr*vn)
      if (mu < -1._kr)then
        vn_max = (2._kr*this%awr*va_max+(this%awr-1._kr)*vn)/(this%awr+1._kr)
      else
        vn_max = sqrt(this%awr*va_max*va_max+vn*vn)
      endif
      ehi_limit = 0.5_kr*mass_n*vn_max*vn_max
      mu = (this%awr-1._kr)*vn/(2._kr*this%awr*va_max)
      if (mu > 1._kr)then
        vn_min = ((this%awr-1._kr)*vn-2._kr*this%awr*va_max)/(this%awr+1._kr)
      else
        vn_min = 0._kr
      endif
      if (vn_min == 0._kr) then
        elo_limit = 1.e-9_kr
      else
        elo_limit = 0.5_kr*mass_n*vn_min*vn_min
      endif
      n_2 = this%n_point/2
      en_point(n_2+1) = e
      en_step = exp(log(e/elo_limit)/(n_2+1))
      do loop_point = n_2,1,-1
        en_point(loop_point) = en_point(loop_point+1)/en_step
      enddo
      en_step = exp(log(ehi_limit/e)/(n_2+1))
      do loop_point = n_2+2,this%n_point
        en_point(loop_point) = en_point(loop_point-1)*en_step
      enddo
    endif
    beta = this%beta
    a1_kt = this%a1_kt(it)
    bkt = this%bkt(it)
    bkt_a = this%bkt_a(it)
    rbkt_a = 1/bkt_a
    ra = 1/this%awr
    kernel = 0.0_kr
    f1 = beta*beta*sqrt(beta)/e*.25_kr
    xs_tk = tab1_mf3mt2_br%terp1(e)
    e_kt = e/kt
    efirst = tab1_mf3mt2%tab1(1,1)
    tfirst = sqrt(efirst/bkt_a)
    do loop_point = 1,this%n_point
      !> calculate kernel
      eps_min = sqrt(a1_kt*min(en_point(loop_point),e))
      eps_max = sqrt(a1_kt*max(en_point(loop_point),e))
      t_p = 0.5_kr*(eps_max+eps_min)
      t_n = 0.5_kr*(eps_max-eps_min)
      !> define the whole integration area
      t1 = max(t_n,eps_max-erf_limit)
      t2 = t_p
      t3 = erf_limit+eps_min
      if (t1 >= t2 .or. t2 >= t3)then
        kernel(loop_point) = 0._kr
        return
      endif
      !> define the integration sub-area
      e1 = t1*t1*bkt_a
      e2 = t2*t2*bkt_a
      e3 = t3*t3*bkt_a
      if (e1 < 1.e-9_kr) e1 = 1.e-9_kr
      !> resonance XS
      eps2_min = 0._kr
      eps2_max = 0._kr
      sum = 0._kr
      if (e3 < efirst)then
        !> whole 1/v approximation.
        call part_inte(e1,e2,bkt_a,rbkt_a,eps_max,eps_min,eps2_max,eps2_min,   &
        ra,e_kt,1,1,0,inte)
        sum = sum+inte(0)
        call part_inte(e2,e3,bkt_a,rbkt_a,eps_max,eps_min,eps2_max,eps2_min,   &
        ra,e_kt,2,1,0,inte)
        sum = sum+inte(0)
      elseif (e2 < efirst .and. efirst <= e3)then
        !> first part is 1/v, second part is 1/v and point-wise XS.
        call part_inte(e1,e2,bkt_a,rbkt_a,eps_max,eps_min,eps2_max,eps2_min,ra &
          ,e_kt,1,1,0,inte)
        sum = sum+inte(0)
        call part_inte(e2,efirst,bkt_a,rbkt_a,eps_max,eps_min,eps2_max,eps2_min&
          ,ra,e_kt,2,1,0,inte)
        sum = sum+inte(0)
        call part_inte(efirst,e3,bkt_a,rbkt_a,eps_max,eps_min,eps2_max,eps2_min&
          ,ra,e_kt,2,0,0,inte)
        sum = sum+inte(0)
      elseif (e1 < efirst .and. efirst <= e2)then
        !> first part is 1/v and point-wise XS, second parte is point-wise XS.
        call part_inte(e1,efirst,bkt_a,rbkt_a,eps_max,eps_min,eps2_max,eps2_min&
          ,ra,e_kt,1,1,0,inte)
        sum = sum+inte(0)
        call part_inte(efirst,e2,bkt_a,rbkt_a,eps_max,eps_min,eps2_max,eps2_min&
          ,ra,e_kt,1,0,0,inte)
        sum = sum+inte(0)
        call part_inte(e2,e3,bkt_a,rbkt_a,eps_max,eps_min,eps2_max,eps2_min,ra &
          ,e_kt,2,0,0,inte)
        sum = sum+inte(0)
      else
        !> whole point-wise XS.
        call part_inte(e1,e2,bkt_a,rbkt_a,eps_max,eps_min,eps2_max,eps2_min,ra &
          ,e_kt,1,0,0,inte)
        sum = sum+inte(0)
        call part_inte(e2,e3,bkt_a,rbkt_a,eps_max,eps_min,eps2_max,eps2_min,ra &
          ,e_kt,2,0,0,inte)
        sum = sum+inte(0)
      endif
      sum = sum*f1
      kernel(loop_point) = sum/xs_tk
    enddo
  endsubroutine point_kernel_calc

!>------------------------------------------------------------------------------
!> evaluate the integration of first and second part. The integration procedure
!> is based on the Gauss-Legendre integration method. The paper "Deterministic
!> modeling of higher angular moments of resonant neutron scattering,
!> Annals of Nuclear Energy,38,2291-2297,2011" is refered to perform the
!> calculation of the three-layer integrands.
!>
!> @param e1 - in, lower boundary for integration
!> @param e2 - in, upper boundary for integration
!> @param bkt_a - in, beta*bk*T/awr
!> @param rbkt_a - in, 1/bkt_a
!> @param eps_max - in, epsilon max
!> @param eps_min - in, epsilon min
!> @param eps2_max - in, eps_max^2
!> @param eps2_min - in, eps_min^2
!> @param ra - in, 1/awr
!> @param e_kt - in, e/(bk*T)
!> @param part_flag - in, the flag to deternmin which part of integration is
!>                        calculated.
!> @param v_1 - in, the flag to determin whether the XS follow 1/v law
!> @param order - in, Legendre order
!> @param inte - inout, results
!>------------------------------------------------------------------------------
  subroutine part_inte(e1,e2,bkt_a,rbkt_a,eps_max,eps_min,eps2_max,eps2_min,ra,&
  e_kt,part_flag,v_1,order,inte)
    ! externals
    real(kr), intent(in) :: e1,e2,bkt_a,rbkt_a,eps_max,eps_min,eps2_max
    real(kr), intent(in) :: eps2_min,ra,e_kt
    integer, intent(in) :: part_flag,v_1,order
    real(kr),allocatable, intent(inout) :: inte(:)
    ! internals
    real(kr), parameter :: e_step = 1._kr
    real(kr), parameter :: sqrtpi = 1.772453850905516_kr
    real(kr),allocatable :: t(:),wtt(:)
    real(kr) :: t1,t2,tt,er,nexp,psi,psi0,rho,g1,j1,j2,t_min,t_max,e_max,tlo
    real(kr) :: tstep
    real(kr) :: elo,ehi,thi,xs,xsp,e_min
    integer :: loop_order,i,loop_np,n_gau
    integer, parameter :: nt = 4

    inte = 0._kr
    t1 = sqrt(e1*rbkt_a)
    t2 = sqrt(e2*rbkt_a)
    tstep = exp(log(t2/t1)/nt)
    n_gau = 5
    allocate(t(n_gau))
    allocate(wtt(n_gau))
    select case(part_flag)
    case(1)
      !> first part
      if (v_1 == 1)then
        !> 1/v
        call gauss_legendre_integral_define(n_gau,t,wtt,t1,t2)
        do i = 1,n_gau
          tt = t(i)*t(i)
          er = tt*bkt_a
          xs = tab1_mf3mt2%terp1(er,1)
          nexp = exp(-tt*ra+e_kt)
          xsp = xs*nexp
          psi = erf(t(i)+eps_min)-erf(eps_max-t(i))
          inte(0) = inte(0)+wtt(i)*t(i)*xsp*psi
          do loop_order = 1,order
            if (loop_order == 1)then
              psi0 = psi
              rho = sqrtpi*(eps2_max+eps2_min-2*tt+.5_kr-2*(eps2_max-tt)*    &
              (eps2_min-tt))
              g1 = exp(-tt-2*t(i)*eps_min-eps2_min)*(t(i)+eps_min-2*         &
                (eps2_max-tt)*(t(i)-eps_min))
              j1 = exp(-tt+2*t(i)*eps_max-eps2_max)*(t(i)-eps_max-2*         &
                (eps2_min-tt)*(t(i)+eps_max))
              j2 = exp(-tt+2*t(i)*eps_min-eps2_min)*(t(i)-eps_min-2*         &
                (eps2_max-tt)*(t(i)+eps_min))
              psi = rho*psi0-g1-j1
              inte(1) = inte(1)+wtt(i)*t(i)*xsp*psi
            else
              psi = get_psi(t(i),loop_order,eps_min,eps_max,1)
              inte(loop_order) = inte(loop_order)+wtt(i)*t(i)*xsp*psi
            endif
          enddo
        enddo
      else
        !> point-wise
        do loop_np = 1,tab1_mf3mt2%d(4)
          if (tab1_mf3mt2%tab1(1,loop_np) > e1)  &
          exit
        enddo
        t_min = t1
        e_min = e1
        do while(tab1_mf3mt2%tab1(1,loop_np) < e2)
          e_max = tab1_mf3mt2%tab1(1,loop_np)
          t_max = sqrt(e_max*rbkt_a)
          tlo = t_min
          elo = e_min
          do while (elo < e_max)
            ehi = elo+e_step
            if (ehi > e_max) ehi = e_max
            thi = sqrt(ehi*rbkt_a)
            call gauss_legendre_integral_define(n_gau,t,wtt,tlo,thi)
            do i = 1,n_gau
              tt = t(i)*t(i)
              er = tt*bkt_a
              xs = tab1_mf3mt2%terp1(er)
              nexp = exp(-tt*ra+e_kt)
              xsp = xs*nexp
              psi = erf(t(i)+eps_min)-erf(eps_max-t(i))
              inte(0) = inte(0)+wtt(i)*t(i)*xsp*psi
              do loop_order = 1,order
                if (loop_order == 1)then
                  psi0 = psi
                  rho = sqrtpi*(eps2_max+eps2_min-2*tt+.5_kr-2*(eps2_max-tt) &
                  *(eps2_min-tt))
                  g1 = exp(-tt-2*t(i)*eps_min-eps2_min)*(t(i)+eps_min-2*     &
                    (eps2_max-tt)*(t(i)-eps_min))
                  j1 = exp(-tt+2*t(i)*eps_max-eps2_max)*(t(i)-eps_max-2*     &
                    (eps2_min-tt)*(t(i)+eps_max))
                  j2 = exp(-tt+2*t(i)*eps_min-eps2_min)*(t(i)-eps_min-2*     &
                    (eps2_max-tt)*(t(i)+eps_min))
                  psi = rho*psi0-g1-j1
                  inte(1) = inte(1)+wtt(i)*t(i)*xsp*psi
                else
                  psi = get_psi(t(i),loop_order,eps_min,eps_max,1)
                  inte(loop_order) = inte(loop_order)+wtt(i)*t(i)*xsp*psi
                endif
              enddo
            enddo
            elo = ehi
            tlo = thi
          enddo
          loop_np = loop_np+1
          t_min = t_max
          e_min = e_max
        enddo
        tlo = t_min
        elo = e_min
        e_max = e2
        do while (elo < e_max)
          ehi = elo+e_step
          if (ehi > e_max) ehi = e_max
          thi = sqrt(ehi*rbkt_a)
          call gauss_legendre_integral_define(n_gau,t,wtt,tlo,thi)
          do i = 1,n_gau
            tt = t(i)*t(i)
            er = tt*bkt_a
            xs = tab1_mf3mt2%terp1(er)
            nexp = exp(-tt*ra+e_kt)
            xsp = xs*nexp
            psi = erf(t(i)+eps_min)-erf(eps_max-t(i))
            inte(0) = inte(0)+wtt(i)*t(i)*xsp*psi
            do loop_order = 1,order
              if (loop_order == 1)then
                psi0 = psi
                rho = sqrtpi*(eps2_max+eps2_min-2*tt+.5_kr-2*(eps2_max-tt)*  &
                (eps2_min-tt))
                g1 = exp(-tt-2*t(i)*eps_min-eps2_min)*(t(i)+eps_min-2*       &
                  (eps2_max-tt)*(t(i)-eps_min))
                j1 = exp(-tt+2*t(i)*eps_max-eps2_max)*(t(i)-eps_max-2*       &
                  (eps2_min-tt)*(t(i)+eps_max))
                j2 = exp(-tt+2*t(i)*eps_min-eps2_min)*(t(i)-eps_min-2*       &
                  (eps2_max-tt)*(t(i)+eps_min))
                psi = rho*psi0-g1-j1
                inte(1) = inte(1)+wtt(i)*t(i)*xsp*psi
              else
                psi = get_psi(t(i),loop_order,eps_min,eps_max,1)
                inte(loop_order) = inte(loop_order)+wtt(i)*t(i)*xsp*psi
              endif
            enddo
          enddo
          elo = ehi
          tlo = thi
        enddo
      endif
    case(2)
      !> second part
      if (v_1 == 1)then
        !> 1/v
        call gauss_legendre_integral_define (n_gau,t,wtt,t1,t2)
        do i = 1,n_gau
          tt = t(i)*t(i)
          er = tt*bkt_a
          xs = tab1_mf3mt2%terp1(er,1)
          nexp = exp(-tt*ra+e_kt)
          xsp = nexp*xs
          psi = erf(t(i)+eps_min)-erf(t(i)-eps_min)
          inte(0) = inte(0)+wtt(i)*t(i)*xsp*psi
          do loop_order = 1,order
            if (loop_order == 1)then
              psi0 = psi
              rho = sqrtpi*(eps2_max+eps2_min-2*tt+.5_kr-2*(eps2_max-tt)*    &
              (eps2_min-tt))
              g1 = exp(-tt-2*t(i)*eps_min-eps2_min)*(t(i)+eps_min-2*         &
                (eps2_max-tt)*(t(i)-eps_min))
              j1 = exp(-tt+2*t(i)*eps_max-eps2_max)*(t(i)-eps_max-2*         &
                (eps2_min-tt)*(t(i)+eps_max))
              j2 = exp(-tt+2*t(i)*eps_min-eps2_min)*(t(i)-eps_min-2*         &
                (eps2_max-tt)*(t(i)+eps_min))
              psi = rho*psi0-g1+j2
              inte(1) = inte(1)+wtt(i)*t(i)*xsp*psi
            else
              psi = get_psi(t(i),loop_order,eps_min,eps_max,2)
              inte(loop_order) = inte(loop_order)+wtt(i)*t(i)*xsp*psi
            endif
          enddo
        enddo
      else
        !> point-wise
        do loop_np = 1,tab1_mf3mt2%d(4)
          if (tab1_mf3mt2%tab1(1,loop_np) > e1)  &
          exit
        enddo
        t_min = t1
        e_min = e1
        do while(tab1_mf3mt2%tab1(1,loop_np) < e2)
          e_max = tab1_mf3mt2%tab1(1,loop_np)
          t_max = sqrt(e_max*rbkt_a)
          tlo = t_min
          elo = e_min
          do while (elo < e_max)
            ehi = elo+e_step
            if (ehi > e_max) ehi = e_max
            thi = sqrt(ehi*rbkt_a)
            call gauss_legendre_integral_define(n_gau,t,wtt,tlo,thi)
            do i = 1,n_gau
              tt = t(i)*t(i)
              er = tt*bkt_a
              xs = tab1_mf3mt2%terp1(er)
              nexp = exp(-tt*ra+e_kt)
              xsp = xs*nexp
              psi = erf(t(i)+eps_min)-erf(t(i)-eps_min)
              inte(0) = inte(0)+wtt(i)*t(i)*xsp*psi
              do loop_order = 1,order
                if (loop_order == 1)then
                  psi0 = psi
                  rho = sqrtpi*(eps2_max+eps2_min-2*tt+.5_kr-2*(eps2_max-tt) &
                  *(eps2_min-tt))
                  g1 = exp(-tt-2*t(i)*eps_min-eps2_min)*(t(i)+eps_min-2*     &
                    (eps2_max-tt)*(t(i)-eps_min))
                  j1 = exp(-tt+2*t(i)*eps_max-eps2_max)*(t(i)-eps_max-2*     &
                    (eps2_min-tt)*(t(i)+eps_max))
                  j2 = exp(-tt+2*t(i)*eps_min-eps2_min)*(t(i)-eps_min-2*     &
                    (eps2_max-tt)*(t(i)+eps_min))
                  psi = rho*psi0-g1+j2
                  inte(1) = inte(1)+wtt(i)*t(i)*xsp*psi
                else
                  psi = get_psi(t(i),loop_order,eps_min,eps_max,2)
                  inte(loop_order) = inte(loop_order)+wtt(i)*t(i)*xsp*psi
                endif
              enddo
            enddo
            elo = ehi
            tlo = thi
          enddo
          loop_np = loop_np+1
          t_min = t_max
          e_min = e_max
        enddo
        tlo = t_min
        elo = e_min
        e_max = e2
        do while(elo < e_max)
          ehi = elo+e_step
          if (ehi > e_max) ehi = e_max
          thi = sqrt(ehi*rbkt_a)
          call gauss_legendre_integral_define(n_gau,t,wtt,tlo,thi)
          do i = 1,n_gau
            tt = t(i)*t(i)
            er = tt*bkt_a
            xs = tab1_mf3mt2%terp1(er)
            nexp = exp(-tt*ra+e_kt)
            xsp = xs*nexp
            psi = erf(t(i)+eps_min)-erf(t(i)-eps_min)
            inte(0) = inte(0)+wtt(i)*t(i)*xsp*psi
            do loop_order = 1,order
              if (loop_order == 1)then
                psi0 = psi
                rho = sqrtpi*(eps2_max+eps2_min-2*tt+.5_kr-2*(eps2_max-tt)*  &
                (eps2_min-tt))
                g1 = exp(-tt-2*t(i)*eps_min-eps2_min)*(t(i)+eps_min-2*       &
                  (eps2_max-tt)*(t(i)-eps_min))
                j1 = exp(-tt+2*t(i)*eps_max-eps2_max)*(t(i)-eps_max-2*       &
                  (eps2_min-tt)*(t(i)+eps_max))
                j2 = exp(-tt+2*t(i)*eps_min-eps2_min)*(t(i)-eps_min-2*       &
                  (eps2_max-tt)*(t(i)+eps_min))
                psi = rho*psi0-g1+j2
                inte(1) = inte(1)+wtt(i)*t(i)*xsp*psi
              else
                psi = get_psi(t(i),loop_order,eps_min,eps_max,2)
                inte(loop_order) = inte(loop_order)+wtt(i)*t(i)*xsp*psi
              endif
            enddo
          enddo
          elo = ehi
          tlo = thi
        enddo
      endif
    end select
  endsubroutine part_inte

!-------------------------------------------------------------------------------
!> calculate the psi value
!>
!> @param t - in, argument
!> @param n - in, Legendre order
!> @param eps_min,eps_max - in, epsilon max/epsilon min
!> @param flag - in, flag to define integration type
!>
!> Note :
!>       1) flag = 1, first part integration
!>       2) flag = 2, second part integration
!-------------------------------------------------------------------------------
  function get_psi(t,n,eps_min,eps_max,flag) result(value)
    real(kr), intent(in) :: t,eps_min,eps_max
    integer, intent(in) :: n,flag
    real(kr) :: value
    real(kr),allocatable :: x1(:),wtt1(:)
    integer :: n_gau,i
    real(kr) :: xmax,xmin,xlo,xhi
    real(kr) :: expflag = 15._kr,step = 1._kr

    value = 0.0_kr
    n_gau = n+3
    allocate(x1(n_gau),wtt1(n_gau))
    if(flag == 1)then
      if (eps_max-t > expflag)then
        value = 0.0_kr
      else
        xmin = eps_max-t
        xmax = t+eps_min
        if (xmax > expflag) xmax = expflag
        xlo = xmin
        xhi = xlo+step
        do while (xhi < xmax)
          call gauss_legendre_integral_define(n_gau,x1,wtt1,xlo,xhi)
          do i = 1,n_gau
            value = value+wtt1(i)*exp(-x1(i)*x1(i))*qn(n,x1(i),t,eps_min,      &
            eps_max)
          enddo
          xlo = xhi
          xhi = xlo+step
        enddo
        call gauss_legendre_integral_define(n_gau,x1,wtt1,xlo,xmax)
        do i = 1,n_gau
          value = value+wtt1(i)*exp(-x1(i)*x1(i))*qn(n,x1(i),t,eps_min,eps_max)
        enddo
      endif
    elseif (flag == 2)then
      if (t-eps_min > expflag)then
        value = 0.0_kr
      else
        xmin = t-eps_min
        xmax = t+eps_min
        if (xmax > expflag) xmax = expflag
        xlo = xmin
        xhi = xlo+step
        do while (xhi < xmax)
          call gauss_legendre_integral_define(n_gau,x1,wtt1,xlo,xhi)
          do i = 1,n_gau
            value = value+wtt1(i)*exp(-x1(i)*x1(i))*qn(n,x1(i),t,eps_min,      &
            eps_max)
          enddo
          xlo = xhi
          xhi = xlo+step
        enddo
        call gauss_legendre_integral_define(n_gau,x1,wtt1,xlo,xmax)
        do i = 1,n_gau
          value = value+wtt1(i)*exp(-x1(i)*x1(i))*qn(n,x1(i),t,eps_min,eps_max)
        enddo
      endif
    endif
  endfunction get_psi

!-------------------------------------------------------------------------------
!> calculate the Qn(x,t) value based on analytical integration for 0-10 orders.
!> higher orders will be calculated using numerical integration.
!>
!> @param n - in, Legendre order
!> @param x,t - in, argument
!> @param eps_min,eps_max - in, epsilon max/epsilon min
!> @oaram value - inout, result
!-------------------------------------------------------------------------------
  function qn(n,x,t,eps_min,eps_max) result(value)
    ! externals
    integer, intent(in) :: n
    real(kr), intent(in) :: x,t
    real(kr), intent(in) :: eps_min,eps_max
    real(kr) :: value
    ! internals
    real(kr) :: c,b,xx,tt,xx_tt,max2,min2,tx,t_x,f1,f2,denom
    integer :: n_gau,i
    real(kr),allocatable :: x1(:),wtt(:)
    real(kr), parameter :: factor = 0.1795871221_kr
    real(kr),parameter :: pi = 3.141592653589793_kr
    real(kr),parameter :: sqrtpi = 1.772453850905516_kr
    real(kr),parameter :: twopi = 6.283185307179586_kr
    real(kr),allocatable :: pn(:)

    value = 0.0_kr
    xx = x*x
    tt = t*t
    tx = (t+x)*(t+x)
    t_x = (t-x)*(t-x)
    xx_tt = xx-tt
    max2 = eps_max*eps_max
    min2 = eps_min*eps_min
    c = (max2+xx_tt)*(min2+xx_tt)
    b = sqrt((tx-max2)*(tx-min2)*(max2-t_x)*(min2-t_x))
    denom = 4._kr*xx*eps_max*eps_min
    f1 = c/denom
    f2 = b/denom
    if (n == 0)then
      value = 2._kr/sqrtpi
    elseif (n == 1)then
      value = f1*2._kr/sqrtpi
    elseif (n == 2)then
      value = .5_kr/sqrtpi*((6._kr*c*c+3._kr*b*b)/x**4/16._kr/max2/min2-2._kr)
    elseif (n == 3)then
      value = pi*factor*0.5_kr*(2._kr*(5*f1**3-3*f1)+15*f1*f2**2)
    elseif (n == 4)then
      value = pi*factor/8*(2._kr*(35*f1**4-30*f1**2+3)+(210*f2**2*f1**2-30*    &
      f2**2)+35*f2**4*3/4)
    elseif (n == 5)then
      value = pi*((63*f1**5-70*f1**3+15*f1)/4+945._kr/32*f1*f2**4+             &
      (630*f1**3*f2**2-210*f1*f2**2)/8)*factor
    elseif (n == 6)then
      value = pi*factor/16*(2._kr*(231*f1**6-315*f1**4+105*f1**2-5)+           &
      (231*f2**6)*5/8+                                                         &
      (3465*f1**2*f2**4-315*f2**4)*3/4+                                        &
      (3465*f1**4*f2**2-1890*f1**2*f2**2+105*f2**2))
    elseif (n == 7)then
      value = pi*factor/16*(2._kr*(429*f1**7-693*f1**5+315*f1**3-35*f1)+       &
      3003*f1*f2**6*5/8+(15015*f1**3*f2**4-3465*f1*f2**4)*3/4+                 &
      (9009*f1**5*f2**2-6930*f1**3*f2**2+945*f1*f2**2))
    elseif (n == 8)then
      value = pi*factor/128*(2._kr*(6435*f1**8-12012*f1**6+6930*f1**4-1260*f1  &
      **2+35)+6435*f2**8*35/64+(180180*f1**2*f2**6-12012*f2**6)*5/8+           &
      (450450*f1**4*f2**4-180180*f1**2*f2**4+6930*f2**4)*3/4+                  &
      (180180*f1**6*f2**2-180180*f1**4*f2**2+41580*f1**2*f2**2-1260*f2**2))
    elseif (n == 9)then
      value = pi*factor/128*                                                   &
      (2._kr*(12155*f1**9-25740*f1**7+18018*f1**5-4620*f1**3+315*f1)+          &
      (109395*f1*f2**8)*35/64+                                                 &
      (1021020*f1**3*f2**6-180180*f1*f2**6)*5/8+                               &
      (1531530*f1**5*f2**4-900900*f1**3*f2**4+90090*f1*f2**4)*3/4+             &
      (437580*f1**7*f2**2-540540*f1**5*f2**2+180180*f1**3*f2**2-13860*f1*f2**2))
    elseif (n == 10)then
      value = pi*factor/256*                                                   &
      (2._kr*(46189*f1**10-109395*f1**8+90090*f1**6-30030*f1**4+3465*f1**2-63) &
      +(46189*f2**10)*63/128+                                                  &
      (2078505*f1**2*f2**8-109395*f2**8)*35/64+                                &
      (9699690*f1**4*f2**6-3063060*f1**2*f2**6+90090*f2**6)*5/8+               &
      (9699690*f1**6*f2**4-7657650*f1**4*f2**4+1351350*f1**2*f2**4-30030*f2**4)&
      *3/4+                                                                    &
      (2078505*f1**8*f2**2-3063060*f1**6*f2**2+1351350*f1**4*f2**2-180180*f1**2&
      *f2**2+3465*f2**2))
    else
      n_gau = 6+n
      allocate(x1(n_gau),wtt(n_gau))
      call gauss_legendre_integral_define(n_gau,x1,wtt,0._kr,twopi)
      allocate(pn(0:n))
      do i = 1,n_gau
        call legendre_poly(f1+f2*cos(x1(i)),n,pn)
        value = value+wtt(i)*pn(n)
      enddo
      value = value*factor
    endif
  endfunction qn

!-------------------------------------------------------------------------------
!> clear the memory
!>
!> @param this - inout, resk_type data
!-------------------------------------------------------------------------------
  subroutine clear(this)
    class(resk_type), intent(inout) :: this

    if (this%is_init)then
      this%is_init = .false.
      this%matb = 0
      this%n_temp = 0
      this%awr = 0._kr
      this%ehi = 0._kr
      this%elo = 0._kr
      this%n_order = 1
      this%beta = 0.0_kr
      if(allocated(this%temp)) deallocate(this%temp)
      deallocate(this%kt)
      deallocate(this%bkt)
      deallocate(this%bkt_a)
      deallocate(this%a1_kt)
    endif
  endsubroutine clear

  function interpolate_tab1(this,x,xs_flag) result(value)
    use endf
    ! externals
    class(tab1_type), intent(inout) :: this
    real(kr), intent(in) :: x
    integer, intent(in), optional :: xs_flag
    real(kr) :: value
    ! externals
    integer :: nrw,i,j,me,np
    real(kr) :: x1,y1,x2,y2
    integer,allocatable :: method(:)
    integer,allocatable :: pair(:)
    integer, save :: last_i = 1,last_np = 1
    real(kr), parameter :: xbig = 1.e12_kr

    value = 0.0_kr
    !> total pair
    this%idis = 0
    np = this%d(4)
    if (last_np > np) last_i = 1
    last_np = np
    !> total range number
    nrw = this%d(3)
    allocate(pair(nrw))
    allocate(method(nrw))
    do i = 1,nrw
      pair(i) = this%d(4+2*i-1)
      method(i) = this%d(4+2*i)
    enddo
    !> search
    if (x < this%tab1(1,1))then
      if(present(xs_flag))then
        !> 1/v approximation
        value = sqrt(this%tab1(1,1)/x)*this%tab1(2,1)
      else
        value = 0.0_kr
      endif
      this%ip = 0
      this%high = this%tab1(1,1)
    elseif (x >= this%tab1(1,np))then
      if(present(xs_flag))then
        !> constant approximation
        value = this%tab1(2,np)
      elseif (x == this%tab1(1,np))then
        value = this%tab1(2,np)
      else
        value = 0._kr
      endif
      this%ip = np
      this%high = xbig
    else
      i = last_i
      do while (x >= this%tab1(1,1) .and. x < this%tab1(1,np))
        if (x < this%tab1(1,last_i)) i = i-1
        if (x >= this%tab1(1,last_i+1)) i = i+1
        if (x >= this%tab1(1,i) .and. x < this%tab1(1,i+1))then
          x1 = this%tab1(1,i)
          y1 = this%tab1(2,i)
          x2 = this%tab1(1,i+1)
          y2 = this%tab1(2,i+1)
          this%low = x1
          this%high = x2
          this%ip = i
          !> define interpolation method
          me=2
          do j = 1,nrw
            if (i < pair(j)) then
              me = method(j)
              exit
            endif
          enddo
          call terp1(x1,y1,x2,y2,x,value,me)
          if (me == 1) this%idis = 1
          if (i <= np-2)then
            if (x2 == this%tab1(1,i+2)) this%idis = 1
          endif
          last_i = i
          exit
        endif
      enddo
    endif
  end function interpolate_tab1

!>------------------------------------------------------------------------------
!> output mf3mt300 and mf6mt300 (RESK) data for tape
!>
!> @param nout - in, unit of tape
!> @param it - in, temperature loop index
!> @param line_num - inout, line number
!>------------------------------------------------------------------------------
  subroutine mt300_outp(this)
    use endf
    ! externals
    class(resk_type), intent(inout) :: this
    ! internals
    integer :: i,nb,nw,n_order,loop_order,ie,nne,nep,indx
    real(kr),allocatable :: a(:)

    n_order = cont_mf6mt300%d(1)
    math=this%matb

    ! recover mf3mt300 data
    mfh=3
    mth=300
    allocate(a(6))
    a(1) = this%za
    a(2) = this%awr
    a(3:6) = 0
    nw=6
    call contio(0,nout,0,a,nb,nw)
    deallocate(a)
    nw = 8+tab1_mf3mt300%d(4)*2
    allocate(a(nw))
    a(1:2) = tab1_mf3mt300%c(1:2)
    a(3:8) = tab1_mf3mt300%d(1:6)
    do i = 1,tab1_mf3mt300%d(4)
      a(8+2*i-1) = tab1_mf3mt300%tab1(1,i)
      a(8+2*i) = tab1_mf3mt300%tab1(2,i)
    enddo
    indx=1
    call tab1io(0,nout,0,a,nb,nw)
    do while (nb /= 0)
      indx=indx+nw
      call moreio(0,nout,0,a(indx),nb,nw)
    enddo
    deallocate(a)
    call asend(nout,0)
    call afend(nout,0)

    ! recover mf6mt300 data
    mfh=6
    mth=300
    allocate(a(6))
    a(1:2) = cont_mf6mt300%c(1:2)
    a(3:6) = cont_mf6mt300%d(1:4)
    nw=6
    call contio(0,nout,0,a,nb,nw)
    deallocate(a)
    nne = tab2_mf6mt300%d(4)
    allocate(a(8))
    a(1:2) = tab2_mf6mt300%c(1:2)
    a(3:8) = tab2_mf6mt300%d(1:6)
    nw=8
    call tab2io(0,nout,0,a,nb,nw)
    deallocate(a)
    do ie = 1,nne
      nep = tab1_mf6mt300(ie)%d(4)
      nw = 8+nep*2
      allocate(a(nw))
      a(1:2) = tab1_mf6mt300(ie)%c(1:2)
      a(3:8) = tab1_mf6mt300(ie)%d(1:6)
      do i = 1,nep
        a(8+2*i-1) = tab1_mf6mt300(ie)%tab1(1,i)
        a(8+2*i) = tab1_mf6mt300(ie)%tab1(2,i)
      enddo
      indx=1
      call tab1io(0,nout,0,a,nb,nw)
      do while (nb /= 0)
        indx=indx+nw
        call moreio(0,nout,0,a(indx),nb,nw)
      enddo
      deallocate(a)
      if (n_order > 0)then
        do loop_order = 1,n_order
          nw = 6+nep
          allocate(a(nw))
          a(1:6) = 0
          a(5) = nep
          a(7:nw) = tab1_mf6mt300(ie)%list(loop_order,1:nep)
          indx=1
          call listio(0,nout,0,a(indx),nb,nw)
          do while (nb /= 0)
            indx=indx+nw
            call moreio(0,nout,0,a(indx),nb,nw)
          enddo
          deallocate(a)
        enddo
      endif
    enddo
    call asend(nout,0)
    call afend(nout,0)
  endsubroutine mt300_outp

!-------------------------------------------------------------------------------
!> get the weights and integration point of gauss legendre integration method.
!>
!> @param n   - in, # of gauss points
!> @param x   - inout, absciassaes
!> @param wtt - inout, weights
!> @param a   - in, lower and upper integral limits
!> @param b   - in, lower and upper integral limits
!>
!> @Note:
!>   - do not use n > 40 for that the error is large.
!-------------------------------------------------------------------------------
  subroutine gauss_legendre_integral_define(n, x, w, a, b)
    use util   ! provides error
    implicit real(kr)(a-h,o-z)
    ! externals
    integer, intent(in) :: n
    real(kr), intent(inout) :: x(n),w(n)
    real(kr), intent(in) :: a,b
    ! internals
    integer::i,k,iter,n2
    real(kr)::xu
    real(kr),dimension(:,:),allocatable::L
    real(kr),dimension(:),allocatable::y0,y,Lp
    real(kr),parameter::pi=3.14159265359d0,eps=1.0d-10
    !
    n2=n+1

    ! Initial guess
    allocate(y(n))
    xu=-1.0d0
    do i=0,n-1
      y(i+1)=cos(real(2*i+1)*pi/real(2*n))+(0.27d0/real(n))*&
      sin(pi*xu*real(n-1)/real(n2))
      xu=xu+2.0/real(n-1)
    enddo
    allocate(y0(n),L(n,n2),Lp(n))

    ! Legendre-Gauss Vandermonde Matrix
    L=0

    ! Compute the zeros of the n+1 Legendre Polynomial
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

      do k=2,n
         L(:,k+1)=( real(2*k-1)*y(:)*L(:,k)-real(k-1)*L(:,k-1) )/real(k)
      enddo

      Lp(:)=real(n2)*( L(:,n)-y(:)*L(:,n2) )/(1.0d0-y(:)**2)

      y0(:)=y(:)
      y(:)=y0(:)-L(:,n2)/Lp(:)
    enddo
    deallocate(y0,L)

    ! Linear map from[-1,1] to [a,b]
    x(:)=(a*(1.0d0-y)+b*(1.0d0+y))/2.0d0

    ! Compute the weights
    w(:)=(b-a)/((1.0d0-y(:)**2)*Lp(:)**2)*(real(n2)/real(n))**2
  end subroutine gauss_legendre_integral_define

!-------------------------------------------------------------------------------
!> generate Legendre polynomial at x by recursion
!>
!> @param x  -
!> @param np -
!> @param p  -
!>
!> Note :
!>       The of index of array p must start from 0.
!-------------------------------------------------------------------------------
  subroutine legendre_poly(x,np,p)
    ! externals
    real(kr) :: x
    integer :: np
    real(kr),dimension(0:np) :: p
    ! internals
    integer :: i

    if (np < 1)then
      p(0) = 1.0_kr
    elseif (np < 2)then
      p(0) = 1.0_kr
      p(1) = x
    else
      p(0) = 1.0_kr
      p(1) = x
      do i = 2,np
        p(i) = ((2._kr*i-1._kr)*x*p(i-1)-(i-1)*p(i-2))/i
      enddo
    endif
  end subroutine legendre_poly

!-------------------------------------------------------------------------------
!> add a value in the initial secondary energy grid
!>
!> @param en       - in, new energy value to add
!> @param nElement - out, number of items in the list
!-------------------------------------------------------------------------------
  subroutine en_list_append(this,en,nElement)
    ! externals
    class(resk_type), intent(inout) :: this
    real(kr),intent(in) :: en
    integer,intent(out) :: nElement
    ! internals
    integer :: ie,ipos
    integer, save :: nSize,nElem
    integer, parameter :: maxsiz=100
    real(kr),pointer :: en_list_bis(:)

    if (associated(this%en_list)) then
      if (nElem+1 > nSize) then
        allocate(en_list_bis(nSize+maxsiz))
        en_list_bis(:nSize)=this%en_list(:nSize)
        nSize=nSize+maxsiz
        deallocate(this%en_list)
        this%en_list => en_list_bis
      endif
      ipos=nElem+1
      do ie=1,nElem
        if (en == this%en_list(ie)) then
          go to 100
        else if (en < this%en_list(ie)) then
          ipos=ie
          exit
        endif
      enddo
      do ie=nElem,ipos,-1
        this%en_list(ie+1)=this%en_list(ie)
      enddo
      this%en_list(ipos)=en
      nElem=nElem+1
    else
      nElem=1
      nSize=maxsiz
      allocate(this%en_list(nSize))
      this%en_list(1)=en
    endif
   100 continue
    nElement=nElem
    return
  end subroutine en_list_append
endmodule reskm
