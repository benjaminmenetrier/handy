!---------------------------------------------------------------------- 
! Subroutine: handy_step
!> Purpose: HANDY integration step
!> <br>
!> Author: Benjamin Menetrier
!> <br>
!> Licensing: this code is distributed under Apache License 2.0
!----------------------------------------------------------------------
subroutine handy_step(state_in,par,state_out)

use handy_mod
implicit none

! Passed variables
type(statetype),intent(in) :: state_in
type(partype),intent(in) :: par
type(statetype),intent(inout) :: state_out

! Local variables 
real :: wth,omega,cc,ce,alphac,alphae

! Diagnostics
wth = par%rho*state_in%xc + par%kappa*par%rho*state_in%xe
if (abs(wth)>0.0) then
   omega = state_in%w/wth
else
   omega = huge(1.0)
end if
cc = min(1.0,omega)*par%s*state_in%xc
ce = min(1.0,omega)*par%kappa*par%s*state_in%xe
if (abs(par%s*state_in%xc)>0.0) then
   alphac = par%alpham + max(0.0,1.0-cc/(par%s*state_in%xc))*(par%alphamax - par%alpham)
else
   alphac = 0.0
endif
if (abs(par%s*state_in%xe)>0.0) then
   alphae = par%alpham + max(0.0,1.0-ce/(par%s*state_in%xe))*(par%alphamax - par%alpham)
else
   alphae = 0.0
end if

! Variables
state_out%xc = par%betac*state_in%xc - alphac*state_in%xc
state_out%xe = par%betae*state_in%xe - alphae*state_in%xe
if (par%nature_switch) then
   state_out%y = par%gamma*state_in%y*(par%lambda - state_in%y) - par%delta*state_in%xc*state_in%y
else
   state_out%y = -par%delta*state_in%xc*state_in%y
end if
state_out%w = par%delta*state_in%xc*state_in%y - cc - ce 

end subroutine handy_step
