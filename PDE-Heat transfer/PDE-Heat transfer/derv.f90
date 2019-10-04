subroutine derv(neqn,t,u,ut)
! 
!  Subroutine derv computes the derivative vector
!  of the linear PDE problem
! 
!  Double precision coding is used
implicit double precision(a-h,l,o-z)
! 
!  Size the arrays
dimension u(neqn), ut(neqn)
! 
!  Problem parameters
call par(xl,xu,pi)
! 
!  BC at x = 0
ut(1)=0.0d0
! 
!  BC at x = 1
ut(neqn)=0.0d0
! 
!  Interior points
dx=(xu-xl)/dfloat(neqn-1)
dxs=dx*dx
do i=2,neqn-1
ut(i)=(u(i+1)-2.0d0*u(i)+u(i-1))/dxs
end do
return
! 
!  End of derv
end