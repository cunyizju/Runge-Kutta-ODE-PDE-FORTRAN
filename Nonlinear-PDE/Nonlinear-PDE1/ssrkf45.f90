subroutine ssrkf45(neqn,t0,u0,h,t,u,e)
! 
! Subroutine ssrkf45 computes an ODE solution by the RK
! Fehlberg 45 method for one step along the solution (by
! calls to derv to define the ODE derivative vector).
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
! one rkf45 step
! 
! e estimate of truncation error of the solu-
! tion vector
! 
! Double precision coding is used
implicit double precision(a-h,k,o-z)
! 
! Size the arrays
parameter(neq=500)
dimension ut0(neq), ut(neq), u5(neq),k1(neq), k2(neq), k3(neq),k4(neq), k5(neq), k6(neq)
dimension u0(neqn), u(neqn), e(neqn)
! 
! Derivative vector at initial (base) point
call derv(neqn,t0,u0,ut0)
! 
! k1, advance of dependent variable vector and
! independent variable for calculation of k2
do i=1,neqn
k1(i)=h*ut0(i)
u(i)=u0(i)+0.25d0*k1(i)
end do
t=t0+0.25d0*h
! 
! Derivative vector at new u, t
call derv(neqn,t,u,ut)
! 
! k2, advance of dependent variable vector and
! independent variable for calculation of k3
do i=1,neqn
k2(i)=h*ut(i)
u(i)=u0(i)+(3.0d0/32.0d0)*k1(i)+(9.0d0/32.0d0)*k2(i)
end do
t=t0+(3.0d0/8.0d0)*h
! 
! Derivative vector at new u, t
call derv(neqn,t,u,ut)
! 
! k3, advance of dependent variable vector and
! independent variable for calculation of k4
do i=1,neqn
k3(i)=h*ut(i)
u(i)=u0(i)+(1932.0d0/2197.0d0)*k1(i)-(7200.0d0/2197.0d0)*k2(i)+(7296.0d0/2197.0d0)*k3(i)
end do
t=t0+(12.0d0/13.0d0)*h
! 
! Derivative vector at new u, t
call derv(neqn,t,u,ut)
! 
! k4, advance of dependent variable vector and
! independent variable for calculation of k5
do i=1,neqn
k4(i)=h*ut(i)
u(i)=u0(i)+( 439.0d0/ 216.0d0)*k1(i)-( 8.0d0 )*k2(i)+(3680.0d0/ 513.0d0)*k3(i)-( 845.0d0/4104.0d0)*k4(i)
end do
t=t0+h
! 
! Derivative vector at new u, t
call derv(neqn,t,u,ut)
! 
! k5, advance of dependent variable vector and
! independent variable for calculation of k6
do i=1,neqn
k5(i)=h*ut(i)
u(i)=u0(i)-( 8.0d0/ 27.0d0)*k1(i)+( 2.0d0 )*k2(i)-(3544.0d0/2565.0d0)*k3(i)+(1859.0d0/4104.0d0)*k4(i)-( 11.0d0/ 40.0d0)*k5(i)
end do
t=t0+0.5d0*h
! 
! Derivative vector at new u, t
call derv(neqn,t,u,ut)
! 
! k6, stepping
do i=1,neqn
k6(i)=h*ut(i)
! 
! Fourth order step
u(i)=u0(i)+( 25.0d0 / 216.0d0)*k1(i)+(1408.0d0 / 2565.0d0)*k3(i)+(2197.0d0 / 4104.0d0)*k4(i)-( 1.0d0 / 5.0d0)*k5(i)
! 
! Fifth order step
u5(i)=u0(i)+( 16.0d0/ 135.0d0)*k1(i)+( 6656.0d0/12825.0d0)*k3(i)+(28561.0d0/56430.0d0)*k4(i)-( 9.0d0/ 50.0d0)*k5(i)+( 2.0d0/ 55.0d0)*k6(i)
end do
do i=1,neqn
! 
! Truncation error estimate
e(i)=u5(i)-u(i)
! 
! Fifth order solution vector (from (4,5) RK pair)
u(i)=u(i)+e(i)
end do
t=t0+h
return
! 
! End of ssrkf45
end