program test

  integer :: i
  real , dimension(10,2,3) :: h
  if(1>2) then
    print*,"2 greater than 1"
  else if(2>1) then
    print*,"1 not greater than 2"
  end if


  i = 0
  do n = 1,20
    i = i+1
    print*,i,'Sajid'
  end do

  outerloop : do j =1,6
    innerloop : do k = 1,5
      print* , 'j,k = ',j,k
    end do innerloop
  end do outerloop

end program test
