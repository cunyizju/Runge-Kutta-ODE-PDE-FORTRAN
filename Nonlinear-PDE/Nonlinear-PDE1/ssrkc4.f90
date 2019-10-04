subroutine ssrkc4(neqn,t0,u0,h,t,u,e)
! 
! Subroutine ssrkc4 computes an ODE solution by the
! classical fourth order RK method for one step along the
! solution (by calls to derv to define the ODE derivative
! vector). It also estimates the truncation error of the
! solution, and applies this estimate as a correction
! to the solution vector.
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
! one rkc4 step
! 
! e estimate of truncation error of the solu-
! tion vector
! 
! Double precision coding is used
implicit double precision(a-h,k,o-z)
! 
! Size the arrays
parameter(neq=500)
dimension ut0(neq), ut(neq), u4(neq),k1(neq), k2(neq), k3(neq), k4(neq)
dimension u0(neqn), u(neqn), e(neqn)
! 
! Derivative vector at initial (base) point
call derv(neqn,t0,u0,ut0)
! 
! k1, advance of dependent variable vector and
! independent variable for calculation of k2
do i=1,neqn
    k1(i)=h*ut0(i)
    u(i)=u0(i)+0.5d0*k1(i)
end do
!
t=t0+0.5d0*h
! 
! Derivative vector at new u, t
call derv(neqn,t,u,ut)
! 
! k2, advance of dependent variable vector and
! independent variable for calculation of k3
do i=1,neqn
    k2(i)=h*ut(i)
    u(i)=u0(i)+0.5d0*k2(i)
end do
!
t=t0+0.5d0*h
! 
! Derivative vector at new u, t
call derv(neqn,t,u,ut)
! 
! k3, advance of dependent variable vector and
! independent variable for calculation of k4
do i=1,neqn
    k3(i)=h*ut(i)
    u(i)=u0(i)+k3(i)
end do
t=t0+h
! 
! Derivative vector at new u, t
call derv(neqn,t,u,ut)
! 
! k4, stepping
do i=1,neqn
    k4(i)=h*ut(i)
    ! 
    ! Second order step
    u(i)=u0(i)+k2(i)
    ! 
    ! Fourth order step
    u4(i)=u0(i)+(1.0d0/6.0d0)*(k1(i)+2.0d0*k2(i)+2.0d0*k3(i)+k4(i))
end do
do i=1,neqn
    ! 
    ! Truncation error estimate
    e(i)=u4(i)-u(i)
    ! 
    ! Fourth order solution vector (from (2,4) RK pair)
    u(i)=u(i)+e(i)
end do
return
! 
! End of ssrkc4
end