subroutine euler2a(neqn,t0,tf,u0,nsteps,u)
! 
! Subroutine euler2a computes an ODE solution by a fixed
! step modified Euler method for a series of points along
! the solution by repeatedly calling subroutine sseuler for
! a single modified Euler step.
! 
! Argument list
! 
! neqn number of first order ODEs
! 
! t0 initial value of independent variable
! 
! tf final value of independent variable
! 
! u0 initial condition vector of length neqn
! 
! nsteps number of modified Euler steps
! 
! u ODE solution vector of length neqn after
! nsteps steps
! 
! Double precision coding is used
implicit double precision(a-h,o-z)
! 
! Size arrays
dimension u0(neqn), u(neqn), e(neqn)
! 
! Integration step
h=(tf-t0)/dfloat(nsteps)
! 
! nsteps modified Euler steps
do j=1,nsteps
! 
! Modified Euler step
call sseuler(neqn,t0,u0,h,t,u,e)
! 
! Reset base point values for the next modified
! Euler step
do i=1,neqn
u0(i)=u(i)
end do
t0=t
! 
! Next modified Euler step
end do
! 
! nsteps modified Euler steps completed
return
! 
! End of euler2a
end