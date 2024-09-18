program main

  use mod_diff
  implicit none
  integer,parameter :: grid_size = 100, num_time_steps =100,center = 20
  real ,parameter :: c = 2, dx= 0.01,dt=0.01,decay=0.01
  real :: h(grid_size)
  integer :: n
  call set_gaussian(h,center,decay)
  print*,0,h
  time_loop : do n = 1,num_time_steps
    h = h - c*diff(h)*dt
    print*,h
  end do time_loop







end program main
