subroutine sseuler(neqn,t0,u0,h,t,u,e)
! 
! Subroutine sseuler computes an ODE solution by the
! modified Euler method for one step along the solution
! (by calls to derv to define the ODE derivative vector).
! It also estimates the truncation error of the solution,
! and applies this estimate as a correction to the
! solution vector.
! 
! Argument list
! 
! neqn number of first order ODEs
! 
! t0 initial value of independent variable
! 
! u0 initial condition vector of length neqn
! 
! h integration step
! 
! t independent variable
! 
! u ODE solution vector of length neqn after
! one modified Euler step
! 
! e estimate of truncation error of the solu-
! tion vector
! 
! Double precision coding is used
implicit double precision(a-h,o-z)
! 
! Size the arrays
parameter(neq=500)
dimension ut(neq),ut0(neq)
dimension u0(neqn), u(neqn), e(neqn)
! 
! Derivative vector at initial (base) point
call derv(neqn,t0,u0,ut0)
! 
! First order (Euler) step
do i=1,neqn
u(i)=u0(i)+ut0(i)*h
end do
t=t0+h
! 
! Derivative vector at advance point
call derv(neqn,t,u,ut)
! 
! Second order step
do i=1,neqn
! 
! Truncation error estimate
e(i)=(ut(i)-ut0(i))*h/2.0d0
! 
! Second order (modified Euler) solution vector
u(i)=u(i)+e(i)
end do
return
! 
! End of sseuler
end