module compositTRAPEZOIDAL
  implicit none
  public
  contains
  real pure elemental function f(x) result(res)
    real,intent(in) :: x
    res = (1/1+(x**2)) ! Whatever the function you want to compute
  end function f
  subroutine TRAPEZOIDAL(n,a,b,res)
    integer,intent(in) :: n
    real,intent(in) :: a,b
    integer :: i
    real :: h ,x
    real,intent(out) :: res
    x = a
    res =0.0
    h = (b-a)/real(n)
    do concurrent(i=1:n)
      res = res + (h/2)*(f(x)+f(x+h))
      x = x+h
    end do
  end subroutine TRAPEZOIDAL


end module compositTRAPEZOIDAL
