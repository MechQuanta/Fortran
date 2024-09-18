module mod_diff
  use iso_fortran_env , only: int32,real32
  implicit none
  contains
    pure function diff(x) result(dx)
      real(kind = real32),intent(in) :: x(:)
      real(kind = real32) :: dx(size(x))
      integer(kind=int32) :: x_size
      x_size = size(x)
      dx(1) = x(1) - x(x_size)
      dx(2:x_size) = x(2:x_size) - x(1:x_size-1)
    end function diff

    subroutine set_gaussian(h,center,decay)
      integer,intent(in) :: center
      real,intent(in) :: decay
      real, intent(in out) :: h(:)
      integer :: i
      do concurrent(i = 1:size(h))
        h(i) = exp(-decay*(i-center)**2)
      end do
    end subroutine set_gaussian



end module mod_diff
