module dash
  implicit none
  contains
    subroutine xyz(d,a,c,b,n)
      integer, intent(in) :: n
      real :: ratio
      integer :: i,k
      real,intent(in out) :: d(n),a(n),c(n),b(n)
      do k = 1,n-1
        if(d(k) == 0.0) then
          print*,"No Solution!!"
        end if
        ratio = b(k)/d(k)
        d(k+1) = d(k+1) - c(k+1)*ratio
      end do
      if(d(n) == 0) then
        print*,"No Solution!!"
      end if
      b(n) = b(n) / d(n)
      do i=n-1,1,-1
        b(i) = (b(i) - a(i)*b(i+1))/d(i)
      end do
    end subroutine xyz

end module dash
