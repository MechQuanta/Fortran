program test
  real :: a
  integer :: center = 11
  real :: decay = 0.01
  real :: h(100)
  a = 1.0
  call add(a,1.0,res)
  print*,res
  call set_gaussian(h,center,decay)
  print*,size(h)
  print*, sum([2.0,3.0],[6.0,3.0])

  contains
  subroutine add(a,b,res)

    real,intent(in) :: a,b
    real,intent(out) :: res
    res= a+b
  end subroutine add

  subroutine set_gaussian(x,center,decay)
    real ,intent(in out) :: x(:)
    integer ,intent(in) :: center
    real , intent(in) :: decay
    integer :: i
    do concurrent(i = 1:size(x))
      x(:) = exp(-(decay)*(i-center)**2)
    end do
  end subroutine set_gaussian
  pure elemental real function sum(c,d)
    real, intent(in) :: c,d
    y = c+d
  end function sum

end program test
