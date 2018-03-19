!---------------------------------------------------------------------- 
! Program: handy
!> Purpose: Human and Nature Dynamics (from Motesharrei et al., 2014)
!> <br>
!> Author: Benjamin Menetrier
!> <br>
!> Licensing: this code is distributed under Apache License 2.0
!----------------------------------------------------------------------
program handy

use handy_mod
implicit none

! Local variables
integer :: it,jt
real :: eta,deltastar,deltastar2,chimax
type(statetype) :: k1,k2,k3,k4
type(statetype),allocatable :: state(:)
type(partype) :: par
character(1024) :: filename
character(1024) :: subr = 'handy'

!----------------------------------------------------------------------
! Header
!----------------------------------------------------------------------

write(*,'(a)') '-------------------------------------------------------------------'
write(*,'(a)') '--- You are running HANDY  (from Motesharrei et al., 2014) --------'
write(*,'(a)') '--- Author : Benjamin Ménétrier -----------------------------------'

!----------------------------------------------------------------------
! Initialization
!----------------------------------------------------------------------

write(*,'(a)') '-------------------------------------------------------------------'
write(*,'(a)') '-1- Initialization'

!----------------------------------------------------------------------
! Read namelist
!----------------------------------------------------------------------

write(*,'(a)') '-------------------------------------------------------------------'
write(*,'(a)') '    Read namelist'
read(*,nml=handy_param)

!----------------------------------------------------------------------
! Memory allocation
!----------------------------------------------------------------------

write(*,'(a)') '-------------------------------------------------------------------'
write(*,'(a)') '    Memory allocation'
allocate(state(0:nt*dtrat))

!----------------------------------------------------------------------
! Initialization
!----------------------------------------------------------------------

write(*,'(a)') '-------------------------------------------------------------------'
write(*,'(a)') '    Initialization'

! Fixed parameters
par%alpham = 1.0e-2
par%alphamax = 7.0e-2
par%s = 5.0e-4
par%rho = 5.0e-3
par%gamma = 1.0e-2
par%lambda = 1.0e2

! Fixed initial variables
state(0)%y = par%lambda
state(0)%w = 0.0

select case(trim(society))
case('egalitarian')
   ! Fixed parameters
   par%betac = 3.0e-2
   par%betae = 0.0
   par%kappa = 0.0
   state(0)%xc = 1.0e2
   state(0)%xe = 0.0

   ! Eta parameter
   eta = (par%alphamax - par%betac)/(par%alphamax - par%alpham)
   write(*,'(a,f6.2)') '    Eta parameter: ',eta

   ! Egalitarian optimal delta
   deltastar = 2.0*eta*par%s/par%lambda

   ! Select depletion factor
   select case(trim(config))
   case('optimal_soft_landing')
      par%delta = deltastar
   case('suboptimal_soft_landing')
      par%delta = 0.75*deltastar
   case('oscillatory_landing')
      par%delta = 2.5*deltastar
   case('cycles')
      par%delta = 4.0*deltastar
   case('collapse')
      par%delta = 5.5*deltastar
   case default
      write(*,'(a)') '    Not a valid config for an '//trim(society)//' society'
      stop
   end select

   ! Maximum carrying capacity
   chimax = par%gamma/(eta*par%s)*(par%lambda/2)**2
case('equitable')
   select case(trim(config))
   case('decreased_depletion')
      par%betac = 3.0e-2
      par%betae = 3.0e-2
      par%kappa = 1.0
      state(0)%xc = 1.0e2
      state(0)%xe = 6.0e2
   case default
      par%betac = 3.0e-2
      par%betae = 3.0e-2
      par%kappa = 1.0
      state(0)%xc = 1.0e2
      state(0)%xe = 2.5e1
   end select

   ! Eta parameter
   eta = (par%alphamax - par%betac)/(par%alphamax - par%alpham)
   write(*,'(a,f6.2)') '    Eta parameter: ',eta

   ! Egalitarian optimal delta
   deltastar2 = 2.0*eta*par%s/par%lambda*(1.0 + state(0)%xe/state(0)%xc)

   ! Select depletion factor
   select case(trim(config))
   case('optimal_soft_landing')
      par%delta = deltastar2
   case('suboptimal_soft_landing')
      par%delta = 0.75*deltastar2
   case('oscillatory_landing')
      par%delta = 2.64*deltastar2
   case('cycles')
      par%delta = 3.46*deltastar2
   case('collapse')
      par%delta = 5.0*deltastar2
   case('decreased_depletion')
      par%delta = 5.0*deltastar2
   case default
      write(*,'(a)') '    Not a valid config for an '//trim(society)//' society'
      stop
   end select

   ! Maximum carrying capacity
   chimax = par%gamma/(eta*par%s)*(par%lambda/2)**2

case('unequal')
   ! Predatory elite
   select case(trim(config))
   case('type-L_collapse')
      par%betac = 3.0e-2
      par%betae = 3.0e-2
      par%kappa = 100.0
      state(0)%xc = 1.0e2
      state(0)%xe = 1.0e-2
   case('type-N_collapse')
      par%betac = 3.0e-2
      par%betae = 3.0e-2
      par%kappa = 100.0
      state(0)%xc = 1.0e2
      state(0)%xe = 0.2
   case('optimal_soft_landing')
      par%betac = 6.5e-2
      par%betae = 2.0e-2
      par%kappa = 10.0
      state(0)%xc = 1.0e4
      state(0)%xe = 3.0e3
   case('oscillatory_landing')
      par%betac = 6.5e-2
      par%betae = 2.0e-2
      par%kappa = 10.0
      state(0)%xc = 1.0e4
      state(0)%xe = 3.0e3
   end select

   ! Eta parameter
   eta = (par%alphamax - par%betac)/(par%alphamax - par%alpham)
   write(*,'(a,f6.2)') '    Eta parameter: ',eta

   ! Egalitarian optimal delta
   deltastar = 2.0*eta*par%s/par%lambda

   ! Select depletion factor
   select case(trim(config))
   case('type-L_collapse')
      par%delta = deltastar
   case('type-N_collapse')
      par%delta = 1.0e-4
   case('optimal_soft_landing')
      par%delta = 6.35e-6
   case('oscillatory_landing')
      par%delta = 1.3e-5
   case default
      write(*,'(a)') '    Not a valid config for an '//trim(society)//' society'
      stop
   end select

   ! Maximum carrying capacity
   chimax = par%gamma/(eta*par%s)*(par%lambda/2)**2
case default
   write(*,'(a)') '    Not a valid society'
   stop
end select
par%nature_switch = .true.

!----------------------------------------------------------------------
! Integration
!----------------------------------------------------------------------

write(*,'(a)') '-------------------------------------------------------------------'
write(*,'(a)') '-2- Integration'

do it=0,nt*dtrat-1
   ! Initialization
   state(it+1) = state(it)

   ! Check nature value
   if (lpos==3) par%nature_switch = par%nature_switch.and.(state(it+1)%y>1.0e-6*par%lambda)

   ! Integration
   if (trim(scheme)=='rk') then
      ! Fourth-order Runge-Kutta scheme
      call handy_step(state(it+1),par,k1)
      call handy_step(state(it+1) + k1/(2.0*float(dtrat)),par,k2)
      call handy_step(state(it+1) + k2/(2.0*float(dtrat)),par,k3)
      call handy_step(state(it+1) + k3/float(dtrat),par,k4)
      state(it+1) = state(it+1) + (k1 + 2.0*k2 + 2.0*k3 + k4)/(6.0*float(dtrat))
   elseif (trim(scheme)=='fe') then
      ! Forward Euler scheme
      call handy_step(state(it+1),par,k1)
      state(it+1) = state(it+1) + k1/float(dtrat)
   else
      write(*,'(a)') '    Not a valid integration scheme'
      stop
   end if

   if (it==0) write(*,'(a7,a6,a12,a12,a12,a12)') '','Year','Commoners','Elite','Nature','Accumulated wealth'
   write(*,'(a7,i6,e12.3,e12.3,e12.3,e12.3)') '',it,state(it+1)%xc,state(it+1)%xe,state(it+1)%y,state(it+1)%w

   if ((lpos>0).and.((state(it+1)%xc<0.0).or.(state(it+1)%xe<0.0).or.(state(it+1)%y<0.0).or.(state(it+1)%w<0.0))) then
      if (lpos==1) then
         ! Stop when negative values
         do jt=it+2,nt*dtrat
            state(jt)%xc = -999.0
            state(jt)%xe = -999.0
            state(jt)%y = -999.0
            state(jt)%w = -999.0
         end do
         exit
      elseif (lpos==2) then
         ! Force positivity
         state(it+1)%xc = max(0.0,state(it+1)%xc)
         state(it+1)%xe = max(0.0,state(it+1)%xe)
         state(it+1)%y = max(0.0,state(it+1)%y)
         state(it+1)%w = max(0.0,state(it+1)%w)
      elseif (lpos==3) then
         ! Threshold on nature regeneration (see handy_step)
      else
         write(*,'(a)') '    Not a valid positivity option'
         stop
      end if
   end if
end do

!----------------------------------------------------------------------
! Write data
!----------------------------------------------------------------------

write(*,'(a)') '-------------------------------------------------------------------'
write(*,'(a)') '-3- Write data'

if (kind(1.0)==4) then
   write(filename,'(a,a,a,a,a,a,a,i2.2,a,i1,a)') '../data/',trim(society),'-',trim(config),'-',trim(scheme),'-',dtrat, &
 & '-lpos',lpos,'-sp.dat'
elseif (kind(1.0)==8) then
   write(filename,'(a,a,a,a,a,a,a,i2.2,a,i1,a)') '../data/',trim(society),'-',trim(config),'-',trim(scheme),'-',dtrat, &
 & '-lpos',lpos,'-dp.dat'
end if
open(unit=20,file=trim(filename),action='write',status='replace')
do it=0,nt*dtrat
   if (state(it)%xc>-999.0) then
      write(20,*) it,float(it)/float(dtrat),state(it)%xc/chimax,state(it)%xe/chimax,state(it)%y/par%lambda,state(it)%w/par%lambda
   else
      write(20,*) it,float(it)/float(dtrat),state(it)%xc,state(it)%xe,state(it)%y,state(it)%w
   end if
end do
close(unit=20)

end program handy
