program md_mc
  implicit none
  real :: x(6),p(2)
  real :: func,gauss_dev
  real :: sqrt2,sigma,in_mc,var,length,volume
  integer :: i,j,n
  sqrt2 = 1/sqrt(2.0)
  length = 5.0
  volume = acos(-1.0)**3

  write(*,*) "Give me the value of n: "
  read(*,*) n

  open(unit=1,file='exp_chng_var_mc_data.dat')

7 sigma = 0.0
  in_mc = 0.0
  var = 0.0
  do i = 1,n
    do j = 1,6
      call random_number(p)
      x(j) = gauss_dev(p) * sqrt2
    end do
    in_mc = in_mc + func(x)
    sigma = sigma + func(x)*func(x)
  end do

  in_mc = in_mc / real(n)
  sigma = sigma / real(n)
  var = sigma - in_mc*in_mc

  in_mc = in_mc * volume
  sigma = volume * sqrt(var/real(n))
  write(*,*) n, " ",in_mc," ",sigma
  n = n*10
  write(1,*) n, " ", in_mc," ",sigma
  if(n .lt. 1000000000) goto 7

end program md_mc

real function func(x)
  implicit none
  real :: x(6),xy,a
  a=0.5
  xy = (x(1)-x(4))**2+(x(2)-x(5))**2+(x(3)-x(6))**2
  func = exp(-a*xy)
end function func

real function gauss_dev(x)
  implicit none
  real :: x(2),x1,x2,p,sqr,fact,un_root
7 call random_number(p)
  x1 = 2.0*p -1.0
  call random_number(p)
  x2 = 2.0*p -1.0
  sqr = x1*x1+x2*x2
  if(sqr .ge. 1.0 .or. sqr .eq. 0.0) goto 7
  un_root = sqrt(-2.0*log(sqr)/sqr)
  gauss_dev = un_root*x1
end function gauss_dev
