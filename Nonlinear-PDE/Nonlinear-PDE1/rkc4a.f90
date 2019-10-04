subroutine rkc4a(neqn,t0,tf,u0,nsteps,u)
! 
! Subroutine rkc4a computes an ODE solution by a fixed step
! rkc4 method for a series of points along the solution by
! repeatedly calling subroutine ssrkc4 for a single rkc4
! step.
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
! nsteps number of rkc4 steps
! 
! u ODE solution vector of length neqn after
! nsteps steps
! 
! Double precision coding is used
implicit double precision(a-h,o-z)
! 
! Size arrays
parameter(neq=500)
dimension e(neq)
dimension u0(neqn), u(neqn)
! 
! Integration step
h=(tf-t0)/dfloat(nsteps)
! 
! nsteps rkc4 steps
do j=1,nsteps
! 
! Single rkc4 step
call ssrkc4(neqn,t0,u0,h,t,u,e)
! 
! Reset base point values for the next rkc4 step
do i=1,neqn
u0(i)=u(i)
end do
t0=t
! 
! Next rkc4 step
end do
! 
! nsteps rkc4 steps completed
return
! 
! End of rkc4a
end