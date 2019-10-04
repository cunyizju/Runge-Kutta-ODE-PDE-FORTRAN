subroutine inital(neqn,t,u0)
! 
!  Subroutine inital sets the initial condition vector
!  for the nonlinear PDE problem
! 
!  Double precision coding is used
implicit double precision(a-h,o-z)
! 
!  Global variables
common/p/ alpha, xk, sigma, xl, ui, ua, a, e, rhos, cps, xls, cs
! 
!  Size the arrays
dimension u0(neqn)
! 
!  Problem parameters
call par
! 
!  Initial condition
do i=1,neqn
u0(i)=ui
end do
return
! 
!  End of inital
end