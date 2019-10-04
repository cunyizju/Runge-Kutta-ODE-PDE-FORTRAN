subroutine fprint(no,ncase,neqn,t,u)
! 
!  Subroutine fprint displays the numerical solution to the
!  nonlinear PDE problem
! 
!  Double precision coding is used
implicit double precision(a-h,o-z)
! 
!  Global variables
common/p/ alpha, xk, sigma, xl, ui, ua, a, e, rhos, cps, xls, cs
! 
!  Size the arrays
dimension u(neqn)
! 
!  Print a heading for the solution at t = 0
if(t.le.0.0d0)then
! 
!  Label for ODE integrator
! 
!  Fixed step modfied Euler
if(ncase.eq.1)then
write(no,11)
11 format(//,' euler2a integrator')
! 
!  Variable step modified Euler
else if(ncase.eq.2)then
write(no,12)
12 format(//,' euler2b integrator')
! 
!  Fixed step classical fourth order RK
else if(ncase.eq.3)then
write(no,13)
13 format(//,' rkc4a integrator')
! 
!  Variable step classical fourth order RK
else if(ncase.eq.4)then
write(no,14)
14 format(//,' rkc4b integrator')
! 
!  Fixed step RK Fehlberg 45
else if(ncase.eq.5)then
write(no,15)
15 format(//,' rkf45a integrator')
! 
!  Variable step RK Fehlberg 45
else if(ncase.eq.6)then
write(no,16)
16 format(//,' rkf45b integrator')
end if
! 
!  Heading
write(no,2)ncase,neqn
2 format(/,' ncase = ',i2,' neqn = ',i2,/)
write(no,3)
3 format(' t u(1) u(im) u(neqn)')
! 
!  End of t = 0 heading
end if
! 
!  Numerical and analytical solution output
! 
!  Grid index of midpoint
im=neqn/2
! 
!  Display the numerical solution
tmin=t*xl**2/alpha/60.0d0
write(no,4) tmin, u(1), u(im), u(neqn)
4 format(f6.1,3f10.2)
return
! 
!  End of fprint
end