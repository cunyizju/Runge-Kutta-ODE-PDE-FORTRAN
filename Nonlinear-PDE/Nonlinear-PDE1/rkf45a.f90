subroutine rkf45a(neqn,t0,tf,u0,nsteps,u)
! 
! Subroutine rkf45a computes an ODE solution by a fixed
! step rkf45 method for a series of points along the
! solution by repeatedly calling subroutine ssrkf45 for
! a single rkf45 step.
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
! nsteps number of rkf45 steps
! 
! u ODE solution vector of length neqn after
! nsteps steps
! 
! Double precision coding is used
implicit double precision(a-h,o-z)
! 
! 
! Size arrays
parameter(neq=500)
dimension e(neq)
dimension u0(neqn), u(neqn)
! 
! Integration step
h=(tf-t0)/dfloat(nsteps)
! 
! nsteps rkf45 steps
do j=1,nsteps
! 
! Single rkf45 step
call ssrkf45(neqn,t0,u0,h,t,u,e)
! 
! Reset base point values for the next rkf45 step
do i=1,neqn
u0(i)=u(i)
end do
t0=t
! 
! Next rkf45 step
end do
! 
! nsteps rkf45 steps completed
return
! 
! End of rkf45a
end
