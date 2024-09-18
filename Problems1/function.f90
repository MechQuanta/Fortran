module function
  use pec
  implicit none
  contains
    real pure elemental function fx(x) result(res)
      real ,intent(in) :: x
      res = x+x**2+pi
    end function fx
    elemental subroutine swap(a,b)
      real,intent(in out) :: a
      real,intent(in out) :: b
      real :: temp
      temp = a
      a = b
      b = temp
    end subroutine swap


end module function
