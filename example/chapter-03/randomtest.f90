program sajid
  real :: dx = 0.01
  call derivative(2.0,3.0,0.01,res)
  print*,res
  print*, gradient([1.0,2.0,3.0,4.0],[5.0,6.0,7.0,8.0],dx)
  call xyz(2.0,2.0,res,.true.)
  print*,res

  contains
  subroutine derivative(f1,f2,dx,res)
    real , intent(in) :: f1,f2,dx
    real , intent(out) :: res
    res = (f2 - f1)/dx
  end subroutine derivative
  pure elemental real function gradient(y1,y2,dx)
    real , intent(in) :: y1,y2,dx
    ddx = (y2 - y1)/dx
  end function gradient
  subroutine xyz(a,b,res,debug)
    real , intent(in) ::a,b
    real , intent(out) ::res
    logical , intent(in),optional :: debug
    if(present(debug)) then
      if (debug) then
        print*,'hi , maja aya?'
      end if
    end if
    res = a**b
    if(present(debug)) then
      if(debug) then
        print*,res
      end if
    end if
  end subroutine xyz

end program sajid
