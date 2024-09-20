program test_rnd
  implicit none
  integer :: n,i,j
  ! n :no of grid points
  ! i :counter
  real :: a, b, h, fa, fb, trap_sum,error ,pi
  ! a:lower limit of the integration
  ! b : upper limit of the integration
  ! h : bin size
  ! fa : value of function at a
  ! fb : value of function at b
  ! trap_sum : value of integral is stored

  open(unit = 7,file='trap_exp.dat')

  write(*,*) "give the grid point n: "
  read(*,*) n
  write(*,*) "give the lower limit: "
  read(*,*) a
  write(*,*) "give the upper limit: "
  read(*,*) b
  pi = exp(3.0) - 1

  do
    n = n*10
    h = (b-a)/real(n)
    fa = func(a)/2.0d0
    fb = func(b)/2.0d0
    trap_sum = 0.0d0
    do i = 1,(n-1)
      trap_sum = trap_sum + func(a+i*h)
    end do
    trap_sum = trap_sum *h+ (h/2)*(fa+ fb)
    error = (pi - trap_sum)
    write(7,*) n , " ",trap_sum, " ", error
    if (n .ge. 100000000) exit
  end do



  contains
  real pure elemental function func(x) result(res)
    real , intent(in) :: x
    res = exp(x)
  end function func

end program test_rnd
