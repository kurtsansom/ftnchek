program prog
  integer, parameter :: one = index(substring = "t", string = "they")
  integer, parameter :: two = index("hello", substring = "el")
  integer, parameter :: three = abs(a = -3)
  integer, parameter :: four = index(string = "lookin", substring = "kin")
  integer, parameter :: five = mod(p = 6, a = 11)
  integer, parameter :: sixtyfive = ichar(c = 'A')

  character :: c1*(one), c2*(two), c3*(three), c4*(four), c5*(five), &
               c65*(sixtyfive)

  integer, parameter :: long = -selected_int_kind(r = 8)
  integer, parameter :: double = -selected_real_kind(r = 100, p = 18)
  integer, parameter :: float = -selected_real_kind(p = 6)
  integer, parameter :: double_d = -selected_real_kind(10, r = 80)
  integer, parameter :: big = -selected_real_kind(r=80)
  character :: c_long*(long), c_double*(double), c_float*(float), &
  c_double_d*(double_d), c_big*(big)

  integer, parameter :: k = kind(x = 1.0)
end program prog
