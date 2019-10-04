subroutine par(xl,xu,pi)
! 
!  Subroutine par sets the parameters for the linear
!  PDE problem
! 
!  Double precision coding is used
implicit double precision(a-h,o-z)
! 
!  Problem parameters
xl=0.0d0
xu=1.0d0
pi=4.0d0*datan(1.0d0)
return
! 
!  End of par
end