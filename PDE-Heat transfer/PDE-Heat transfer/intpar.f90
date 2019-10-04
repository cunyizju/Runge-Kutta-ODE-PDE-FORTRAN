subroutine intpar(neqn,nout,nsteps,t0,tf,abserr,relerr)
! 
!  Subroutine intpar sets the parameters to control the
!  integration of the linear PDE problem
! 
!  Double precision coding is used
implicit double precision(a-h,o-z)
! 
!  Number of ODEs
neqn=21
! 
!  Number of output points
nout=6
! 
!  Maximum number of steps in the interval t0 to tf
nsteps=250
! 
!  Initial, final values of the independent variable
t0=0.0d0
tf=0.2d0
! 
!  Error tolerances
abserr=1.0d-05
relerr=1.0d-05
return
! 
!  End of intpar
end