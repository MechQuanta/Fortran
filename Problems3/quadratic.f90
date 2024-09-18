program THOMAS
  use dash
  integer,parameter :: n=4
  real :: d(n),a(n),c(n),b(n)
  d = (/1.0,2.0,-1.0,-1.0/)
  c = (/0.0,-1.0,4.0,2.0/)
  a = (/1.0,3.0,2.0,0.0/)
  b = (/1.5,1.0,1.0,1.0/)
  call xyz(d,a,c,b,n)
  print*,x



end program THOMAS
