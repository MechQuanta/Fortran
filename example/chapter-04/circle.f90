module circle
  use iso_fortran_env
  implicit none
  private
  real, parameter :: pi = 3.141569
  public :: Area_finding

  contains
    real pure elemental function Area_finding(radius) result(area)
      real , intent(in) :: radius
      area = pi*(radius**2)
    end function Area_finding




end module circle
