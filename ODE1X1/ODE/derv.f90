subroutine derv(neqn,t,u,ut)
! 
! Subroutine derv computes the derivative vector
! of the 1 x 1 ODE problem
! 
! Double precision coding is used
implicit double precision(a-h,l,o-z)
! 
! Size the arrays
dimension u(neqn), ut(neqn)
! 
! Problem parameters
alpha=1.0d0
lambda=1.0d0
! 
! Derivative vector
ut(1)=lambda*dexp(-alpha*t)*u(1)
return
! 
! End of derv
end