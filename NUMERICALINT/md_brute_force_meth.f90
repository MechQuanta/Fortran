program brute_force
  real :: p(6),x(6)
  real :: length,int_mc,sigma,var,volume
  integer :: i,n


  length = 5.0
  volume= (2.0*length)**6


  write(*,*) "Give the value of n: "
  read(*,*) n
  open(unit=1,file='md_exp_bru_forc.dat')


7 int_mc = 0.0
  sigma = 0.0
  do i = 1, n
    call random_number(p)
    do j = 1,size(p)
      x(j) = (2.0*p(j) -1)*length
    end do
    int_mc = int_mc + func(x)
    sigma = sigma + func(x)*func(x)
  end do
  int_mc = int_mc/real(n)
  sigma = sigma/real(n)
  var = sigma - int_mc*int_mc
  int_mc = int_mc*volume
  sigma = volume*sqrt(var/real(n))

  write(1,*) n," ",int_mc," ",sigma
  n = n*10
  if(n .lt. 1000000000) goto 7


  contains
    real pure function func(x) result(res)
      real,intent(in) :: x(:)
      real :: a,b,xx,yy,xy
      a = 1.0
      b=0.5
      xx = x(1)**2+x(2)**2+x(3)**2
      yy = x(4)**2+x(5)**2+x(6)**2
      xy = (x(1)-x(4))**2+(x(2)-x(5))**2+(x(3)-x(6))**2
      res = exp(-a*xx-a*yy-b*xy)
    end function func
end program brute_force
