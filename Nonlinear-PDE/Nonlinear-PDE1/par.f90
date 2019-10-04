subroutine par
! 
!  Double precision coding is used
implicit double precision(a-h,o-z)
! 
!  Global variables
common/p/ alpha, xk, sigma, xl, ui, ua, a, e, rhos, cps, xls, cs
! 
!  Subroutine par sets the parameters for the nonlinear PDE
alpha=1.0d-06
xk=1.0d0
sigma=5.67d-08
xl=0.1d0
ui=298.0d0
ua=2000.0d0
a=1.0d0
e=1.0d0
rhos=7800.0d0
cps=435.0d0
xls=0.025d0
cs=rhos*cps*xls
return
! 
!  End of par
end