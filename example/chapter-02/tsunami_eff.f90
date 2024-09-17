program tsunami_eff

  ! implicit none
  integer , parameter :: grid_size = 100
  integer , parameter :: num_time_steps = 100

  real , parameter :: dt = 1 , dx = 1 , c = 1
  real :: h(grid_size), dh(grid_size)


  integer , parameter :: center = 25
  real , parameter :: decay = 0.02



  if (grid_size <= 0) stop 'grid_size must be > 0'
  if (dt <= 0) stop 'time step dt must be > 0'
  if (dx <= 0) stop 'grid spacing must be > 0'
  if (c <= 0) stop 'background flow speed must be > 0'

  do concurrent(i = 1:grid_size)
    h(i) = exp(-decay * (i - center)**2)
  end do
  print*,SHAPE(h)
  print*,0,h
  time_loop : do n = 1,num_time_steps
    dh(1) = h(1) - h(grid_size)
    do i = 2 , grid_size
      dh(i) = h(i) - h(i-1)
    end do

    do i = 1, grid_size
      h(i) = h(i) - c * dh(i)/dx *dt
    end do
    print*,n,h
  end do time_loop




end program tsunami_eff
