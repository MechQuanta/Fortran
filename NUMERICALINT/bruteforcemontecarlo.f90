program xyz
  implicit none
  integer :: n,i
  real :: p,crude_mc,sigma,length,actual,var,x

  actual = exp(3.0) - 1
  length = 3.0
  open(unit =1 , file='exp_crude_mc.dat')
  write(*,*) "Give the value of n: "
  read(*,*) n
7 crude_mc = 0.0
  sigma = 0.0
  do i = 1,n
    call random_number(p)
    x = 3.0*p
    crude_mc = crude_mc + func(x)
    sigma = sigma + func(x)*func(x)
  end do
  crude_mc = crude_mc / real(n)
  sigma = sigma /real(n)
  var = sigma - crude_mc*crude_mc
  crude_mc = length * crude_mc
  var = length * sqrt(var/real(n))
  write(*,*) n, " " , crude_mc , " ",var

  write(1,*) n , " ", crude_mc , " ",abs(crude_mc - actual)

  n = n*10
  if(n .le. 10000000) goto 7




  contains
    real pure elemental function func(x) result(res)
      real , intent(in) :: x
      res = exp(x)
    end function func


end program xyz
