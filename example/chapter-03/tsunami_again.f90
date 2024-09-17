program xyz
  implicit none
  integer,parameter :: center = 20,grid_size=100, num_time_steps=100
  real, parameter :: dx = 0.1,decay = 0.01,c=2,dt=0.01
  integer :: n
  real :: h(grid_size)
  call set_gaussian(h,center,decay)
  print*,h
  time_loop: do n=1,num_time_steps
    h = h - c*(diff(h)/dx)*dt
    print*,"*************************Number : - ",n,h
  end do time_loop

  contains
  subroutine set_gaussian(h,center,decay)
    real,intent(in out) :: h(:)
    integer,intent(in) :: center
    real , intent(in) :: decay
    integer :: i,h_size
    h_size = size(h)
    do concurrent(i = 1:h_size)
      h(:) = exp(-(decay)*(i-center)**2)
    end do
  end subroutine set_gaussian
  pure function diff(h) result(dx)
    real , intent(in) :: h(:)
    real :: dx(size(h))
    integer :: h_size
    h_size = size(h)
    dx(1) = h(1) - h(h_size)
    dx(2:h_size) = h(2:h_size) - h(1:h_size-1)
  end function diff
end program xyz
