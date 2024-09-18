program math
  use side
  real, parameter :: b=3.0,c=5.0,A=50.0

  s1 = side_length(b,c,A)
  print*,s1
  call perimeter(s1,b,c)
  print*,p
  val = Area(a,b,c,A)
  print*,val
end program math
