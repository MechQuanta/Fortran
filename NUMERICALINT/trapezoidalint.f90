program xyz
  implicit none
  real :: a,b,h,res,x,fa,fb
  integer :: i,n
  res = 0.0
  write(*,*)  "Give lower limit: "
  read(*,*) a
  write(*,*)  "Give upper limit: "
  read(*,*) b
  write(*,*)  "Give number grid: "
  read(*,*) n
  x = a
  fa = f(a)
  fb = f(b)
  h = (b-a)/real(n)
  do i = 1,(n-1)
    res= res + f(x+i*h)
  end do
  res = res*h + (h/2)*(fa + fb)

  write(*,*) "Value :",res





  contains
  real pure elemental function f(x) result(res)
    real,intent(in) :: x
    res = 4*(1/(1+x**2))
  end function f

end program xyz
