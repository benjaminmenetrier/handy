!---------------------------------------------------------------------- 
! Module: handy_mod
!> Purpose: handy module variables
!> <br>
!> Author: Benjamin Menetrier
!> <br>
!> Licensing: this code is distributed under Apache License 2.0
!----------------------------------------------------------------------
module handy_mod

implicit none

! Namelist variables
character(1024) :: society !< Society type
character(1024) :: config  !< Configuration
character(1024) :: scheme  !< Numerical scheme
integer :: nt              !< Integration time (years)
integer :: dtrat           !< Integration step ratio (per years)
integer :: lpos            !< Positivity option (0: nothing to do, 1: stop when negative, 2: force to 0 when negative)
namelist/handy_param/society,config,scheme,nt,dtrat,lpos

! Handy state type
type statetype
   ! Variable
   real :: xc  !< Commoners
   real :: xe  !< Elite
   real :: y   !< Nature
   real :: w   !< Accumulated wealth
end type statetype

! Handy parameters type
type partype
   ! Variable
   real :: alpham
   real :: alphamax
   real :: betac
   real :: betae
   real :: s
   real :: rho
   real :: gamma
   real :: lambda
   real :: kappa
   real :: delta
   logical :: nature_switch
end type partype

interface operator (+)
   module procedure add
end interface

interface operator (*)
   module procedure mul
end interface

interface operator (/)
   module procedure div
end interface

contains

!---------------------------------------------------------------------- 
! Function: add
!> Purpose: add states
!----------------------------------------------------------------------
type(statetype) function add(state1,state2)

implicit none

! Passed arguments
type(statetype),intent(in) :: state1 !< First state
type(statetype),intent(in) :: state2 !< Second state

! Add data
add%xc = state1%xc + state2%xc
add%xe = state1%xe + state2%xe
add%y = state1%y + state2%y
add%w = state1%w + state2%w

end function add

!---------------------------------------------------------------------- 
! Function: add
!> Purpose: multiply state by a real
!----------------------------------------------------------------------
type(statetype) function mul(r,state)

implicit none

! Passed arguments
real,intent(in) :: r !< Real factor
type(statetype),intent(in) :: state !< State

! Multiply data
mul%xc = r*state%xc
mul%xe = r*state%xe
mul%y = r*state%y
mul%w = r*state%w

end function mul

!---------------------------------------------------------------------- 
! Function: div
!> Purpose: divide state by a real
!----------------------------------------------------------------------
type(statetype) function div(state,d)

implicit none

! Passed arguments
type(statetype),intent(in) :: state !< State
real,intent(in) :: d !< Real divider

! Divide data
div%xc = state%xc/d
div%xe = state%xe/d
div%y = state%y/d
div%w = state%w/d

end function div

end module handy_mod
