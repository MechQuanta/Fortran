program acc_rej
  implicit none
  integer :: i,n,pt_curve
  real :: x,p,y,integral,exact

  write(*,*) "Give the value of n: "
  read(*,*) n

  open(unit = 1, file = 'exp_acc_rej.dat')
7 pt_curve = 0
  do i = 1,n
    ! find random value of x between 0 and 3
    call random_number(p)
    x = 3.0*p
    ! find value of function between 0 and exp(3)
    y = exp(3.0)*p
    ! if y at exp(x) is below the curve we accept
    if (y .lt. exp(x)) then
      pt_curve = pt_curve+1
    end if
  end do
  !  multiply with area of rectangle and divide by the no. of cycles

  integral = 3.0 *exp(3.0)*(real(pt_curve)/real(n))
  exact= exp(3.0) -1
  write(1,*) n , " ", integral , " ",abs(integral - exact)
  n= n*10
  if (n .le. 1000000000) goto 7




  contains
    real pure elemental function func(x) result(res)
      real , intent(in) :: x
      res = exp(x)
    end function func

end program acc_rej
