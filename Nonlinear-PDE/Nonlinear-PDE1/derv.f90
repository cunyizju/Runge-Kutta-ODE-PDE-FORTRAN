subroutine derv(neqn,t,u,ut)
! 
!  Subroutine derv computes the derivative vector
!  of the nonlinear PDE problem
! 
!  Double precision coding is used
implicit double precision(a-h,o-z)
! 
!  Global variables
common/p/ alpha, xk, sigma, xl, ui, ua, a, e, rhos, cps, xls, cs
! 
!  Size the arrays
dimension u(neqn), ut(neqn)
! 
!  Spatial grid
dx=1.0d0/dfloat(neqn-2)
dxs=dx*dx
! 
!  Insulation
do i=1,neqn-2
if(i.eq.1)then
u0=u(2)+2.0d0*dx*xl*(sigma/xk)*(a*ua**4-e*u(1)**4)
ut(i)=(u(2)-2.0d0*u(1)+u0)/dxs
else
ut(i)=(u(i+1)-2.0d0*u(i)+u(i-1))/dxs
end if
end do
! 
!  Steel
ut(neqn)=(xk*xl/alpha)*(1.0d0/cs)*(u(neqn-2)-u(neqn-1))/dx
ut(neqn-1)=ut(neqn)
! 
!  End of derv
return
end