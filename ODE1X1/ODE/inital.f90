subroutine inital(neqn,t,u0)
! 
! Subroutine inital sets the initial condition vector
! for the 1 x 1 ODE problem
! 
! Double precision coding is used
implicit double precision(a-h,o-z)
! 
! Size the arrays
dimension u0(neqn)
! 
! Initial condition
u0(1)=1.0d0
return
! 
! End of inital
end