program typedef_main
  use type_t1_module
  use type_t2_module
  type(t1) :: my_t1var
  type(t2) :: my_t2var, my_other_t2var
  real :: r

  my_t1var%c1 = pi
  my_t1var%c2 = 2*pi
  print *, my_t1var
  my_t2var%a = 1.23
  my_t2var%b = 3.45
  my_other_t2var = t2fun(my_t2var)
  print *, my_other_t2var
  my_t2var = t2fun( foo(r) )! r and foo(r) are wrong types for args

end program typedef_main
