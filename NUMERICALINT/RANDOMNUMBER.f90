program random
  implicit none
  integer :: i_seed,i,n=7
  integer , dimension(:), allocatable :: a_seed
  integer , dimension(1:8) :: dt_seed
  real :: r

  call random_seed(size=i_seed)
  write(*,*) i_seed
  allocate(a_seed(1:i_seed))
  write(*,*) a_seed
  call random_seed(get = a_seed)
  write(*,*) a_seed
  call date_and_time(values=dt_seed)  ! year , month , day , diff, HR , MIN , SEC , mill sec.
  write(*,*) dt_seed
  do i = 1,8
    a_seed(i) = dt_seed(i)
  end do
  call random_seed(put = a_seed)
  deallocate(a_seed)


  do i = 1, n
    call random_number(r)
    write(6,*) "random number is ",r
  end do
end program random
