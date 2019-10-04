subroutine euler2b(neqn,t0,tf,u0,nsteps,abserr,relerr,u)
! 
! Subroutine euler2b computes an ODE solution by a variable
! step modified Euler method for a series of points along
! the solution by repeatedly calling subroutine sseuler for
! a single modified Euler step. The truncation error is
! estimated along the solution to adjust the integration
! step according to a specified error tolerance.
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
! nsteps maximum number of modified Euler steps
! 
! abserr absolute error tolerance
! 
! relerr relative error tolerance
! 
! u ODE solution vector of length neqn after
! nsteps steps
! 
! Double precision coding is used
implicit double precision(a-h,o-z)
! 
! Size the arrays
parameter(neq=500)
dimension e(neq)
dimension u0(neqn), u(neqn)
! 
! Initial integration step
h=(tf-t0)/8.0d0
! 
! Minimum allowable step
hmin=(tf-t0)/dfloat(nsteps)
! 
! Start integration
t=t0
! 
! While independent variable is less than the final
! value, continue the integration
do while(t.le.tf*0.999d0)
! 
! If the next step along the solution will go past
! the final value of the independent variable, set
! the step to the remaining distance to the final
! value
if((t+h).gt.tf)then
h=tf-t
end if
! 
! Single modified Euler step
call sseuler(neqn,t0,u0,h,t,u,e)
! 
! Flag for the end of the integration
nfin1=1
! 
! Check if any of the ODEs have violated the error
! criteria
do i=1,neqn
if(dabs(e(i)).gt.dabs(u(i))*relerr+abserr)then
! 
! Error violation, so integration is not
! complete. Reduce integration step because
! of error violation and repeat integration
! from the base point
nfin1=0
h=h/2.0d0
! 
! If the current step is less than the minimum
! allowable step, set the step to the minimum
! allowable value and continue integration from
! new base point
if(h.lt.hmin)then
h=hmin
nfin1=1
end if
go to 1
end if
end do
! 
! Ifthere is no error violation, continue the
! integration from new base point
1 if(nfin1.eq.1)then
do i=1,neqn
u0(i)=u(i)
end do
t0=t
! 
! Test if integration step can be increased
do i=1,neqn
if(dabs(e(i)).gt.(dabs(u(i))*relerr+abserr)/4.0d0)then
! 
! Integration step cannot be increased
go to 2
end if
end do
! 
! Increase integration step
h=h*2.0d0
! 
! Continue for no error violation (nfin1=1)
2 end if
! 
! Continue do while
end do
return
! 
! End of euler2b
end