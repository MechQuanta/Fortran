program sine
  implicit none
  integer :: i,n,s,pi
  real :: area_acc_rej,x,p,expect,y
  pi = 3.14159265
  open(unit = 1,file='sin_acc_rej.dat')
  write(*,*) "Give the values of n: "
  read(*,*) n

7 s = 0
  do i = 1,n
    call random_number(p)
    x = p*pi
    y = p*1
    if(y .lt. sin(x)) then
      s =s+1
    end if
  end do
  area_acc_rej = 1*pi*(real(s)/real(n))
  expect = 2
  write(1,*) n," ",area_acc_rej," ",abs(area_acc_rej-expect)
  n = n*10
  if(n .lt. 100000000) goto 7



end program sine
