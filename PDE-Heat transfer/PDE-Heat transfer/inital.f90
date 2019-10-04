subroutine inital(neqn,t,u0)
! 
!  Subroutine inital sets the initial condition vector
!  for the linear PDE problem
! 
!  Double precision coding is used
implicit double precision(a-h,o-z)
! 
!  Size the arrays
dimension u0(neqn)
! 
!  Problem parameters
call par(xl,xu,pi)
! 
!  Initial condition
do i=1,neqn
x=xl+dfloat(i-1)/dfloat(neqn-1)*(xu-xl)
u0(i)=dsin(pi*x)
end do
return
! 
!  End of inital
end