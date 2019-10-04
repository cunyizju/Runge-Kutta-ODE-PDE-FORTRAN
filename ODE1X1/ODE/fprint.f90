subroutine fprint(no,ncase,neqn,t,u)
! 
! Subroutine fprint displays the numerical and
! analytical solutions to the 1 x 1 ODE problem
! 
! Double precision coding is used
implicit double precision(a-h,l,o-z)
! 
! Size the arrays
dimension u(neqn)
! 
! Problem parameters
u0=1.0d0
alpha=1.0d0
lambda=1.0d0
! 
! Print a heading for the solution at t = 0
if(t.le.0.0d0)then
! 
! Label for ODE integrator
! 
! Fixed step modfied Euler
if(ncase.eq.1)then
write(no,11)
11 format(//,6x,'euler2a integrator')
! 
! Variable step modified Euler
else if(ncase.eq.2)then
write(no,12)
12 format(//,6x,'euler2b integrator')
! 
! Fixed step classical fourth order RK
else if(ncase.eq.3)then
write(no,13)
13 format(//,6x,'rkc4a integrator')
! 
! Variable step classical fourth order RK
else if(ncase.eq.4)then
write(no,14)
14 format(//,6x,'rkc4b integrator')
! 
! Fixed step RK Fehlberg 45
else if(ncase.eq.5)then
write(no,15)
15 format(//,6x,'rkf45a integrator')
! 
! Variable step RK Fehlberg 45
else if(ncase.eq.6)then
write(no,16)
16 format(//,6x,'rkf45b integrator')
end if
! 
! Heading
write(no,2)
2 format(/,9x,'t',3x,'u1(num)',4x,'u1(ex)',8x,'diff1',/)
! 
! End of t = 0 heading
end if
! 
! Analytical solution
u1exact=u0*dexp(lambda/alpha*(1.0d0-dexp(-alpha*t)))
! 
! Difference between exact and numerical solution vectors
diff1=u(1)-u1exact
! 
! Display the numerical and exact solutions, and their
! difference
write(no,3)t,u(1),u1exact,diff1
3 format(f10.2,2f10.5,e13.4)
return
! 
! End of fprint
end