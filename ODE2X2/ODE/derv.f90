subroutine derv(neqn,t,u,ut)
! 
! Subroutine derv computes the derivative vector
! of the 2 x 2 ODE problem
! 
! Double precision coding is used
implicit double precision(a-h,o-z)
! 
! Size the arrays
dimension u(neqn), ut(neqn)
! 
! Problem parameters
call par(a,b)
! 
! Derivative vector
ut(1)=-a*u(1)+b*u(2)
ut(2)= b*u(1)-a*u(2)
return
! 
! End of derv
end