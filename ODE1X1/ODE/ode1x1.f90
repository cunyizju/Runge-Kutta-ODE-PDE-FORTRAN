!****************************************************************************
!
!  PROGRAM: ODE1X1
!
!  PURPOSE:  Solve first-order, linear, ordinary differential equation using 
!            Runge Kutta Method.
!
!****************************************************************************
!  Author: lcy
!  
!  Data: 2019.09.27
!****************************************************************************

program ode1x1
! 
! Numerical solution to the 1 x 1 ODE system by six
! integrators
! 
! Double precision coding is used
implicit double precision(a-h,o-z)
! 
! Size arrays
parameter(neq=500)
dimension u0(neq),u(neq)
! 
! Open a file for output
no=2
open(no,file='G:\Documents\programming\Fortran\ODE&PDE\ODE1X1\ODE\ode1x1for.out')
! 
! Step through six integrators
do ncase=1,6
! 
! Integration parameters
    call intpar(neqn,nout,nsteps,t0,tf,abserr,relerr)
! 
! Initial condition
    call inital(neqn,t0,u0)
! 
! Output time
    tp=tf-t0
! 
! Step through nout grid points
    do j=1,nout
! 
! Print solution
        call fprint(no,ncase,neqn,t0,u0)
! 
! Select ODE integrator
! 
! Fixed step modified Euler integrator
        if(ncase.eq.1)then
            call euler2a(neqn,t0,tf,u0,nsteps,u)
        end if
! 
! Variable step modified Euler integrator
        if(ncase.eq.2)then
            call euler2b(neqn,t0,tf,u0,nsteps,abserr,relerr,u)
        end if
! 
! Fixed step classical fourth order RK integrator
        if(ncase.eq.3)then
            call rkc4a(neqn,t0,tf,u0,nsteps,u)
        end if
! 
! Variable step classical fourth order RK integrator
        if(ncase.eq.4)then
            call rkc4b(neqn,t0,tf,u0,nsteps,abserr,relerr,u)
        end if
! 
! Fixed step RK Fehlberg (RKF45) integrator
        if(ncase.eq.5)then
            call rkf45a(neqn,t0,tf,u0,nsteps,u)
        end if
! 
! Variable step Fehlberg (RKF45) integrator
        if(ncase.eq.6)then
            call rkf45b(neqn,t0,tf,u0,nsteps,abserr,relerr,u)
        end if
! 
! Advance solution
        t0=tf
        tf=tf+tp
        do i=1,neqn
            u0(i)=u(i)
        end do
! 
! Next output
        end do
! 
! Next integrator
        end do
! 
! End of ode1x1
        end