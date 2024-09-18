module side
  implicit none
  contains
    real pure elemental function side_length(b,c,angle) result(x)
      real,intent(in) :: b,c,angle
      x = sqrt(b**2+c**2-2*b*c*cos(angle))
    end function side_length
    pure elemental subroutine perimeter(a,b,c)
      real ,intent(in) :: a,b,c
      real :: p
      p = a+b+c
    end subroutine perimeter
    real pure elemental function Area(a,b,c,angle) result(val)
      real, intent(in) :: a,b,c,angle
      val = 0.5*b*c*sin(angle)
    end function Area


end module side
