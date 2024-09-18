program xyz

  use iso_fortran_env
  use circle
  print*,"compiler version: ",compiler_version()
  print*,"compiler options: ",compiler_options()
  area = Area_finding(2.0)
  print*,area
end program xyz
