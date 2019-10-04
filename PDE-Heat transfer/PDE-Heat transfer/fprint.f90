subroutine fprint(no,ncase,neqn,t,u)
! 
!  Subroutine fprint displays the numerical and
!  exact solutions to the linear PDE problem
! 
!  Double precision coding is used
implicit double precision(a-h,l,o-z)
! 
!  Size the arrays
dimension u(neqn)
! 
!  Problem parameters
call par(xl,xu,pi)
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
write(no,4)ncase,neqn
4 format(/,' ncase = ',i1,' neqn = ',i2)
write(no,2)
2 format(/,4x,'t',4x,'u1(num)',5x,'u1(ex)',8x,'diff1',/)
! 
!  End of t = 0 heading
end if
! 
!  Numerical and analytical solution output
! 
!  Midpoint value of x
x=(xu-xl)/2.0d0
! 
!  Analytical solution at midpoint
ue=dexp(-pi*pi*t)*dsin(pi*x)
! 
!  Grid index of midpoint
im=dfloat((neqn+1)/2)
! 
!  Display the numerical and exact solutions, and their
!  difference
write(no,3)t,u(im),ue,u(im)-ue
3 format(f5.2,2f11.6,e13.4)
return
! 
!  End of fprint
end