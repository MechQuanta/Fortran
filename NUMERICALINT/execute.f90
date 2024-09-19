program sajid
  use compositTRAPEZOIDAL
  implicit none
  real :: res = 0.0
  call TRAPEZOIDAL(100,0.0,1.0,res)
  print*,res



end program sajid
