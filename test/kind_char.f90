program p

    real (kind = 4) a
    real c, b
    real d

    character (kind = 1) cl1
    character (kind = 1) cl2
    character (kind = 4) ch1
    character cd1, cd2, cd3

    !checks on character types

    ch1 = cl1 // cl2
    ch1 = cl1 // ch1        ! error kind mismatch on concatenation
    cl1 = ch1 // ch1
    cd1 = cd2 // cd3

    c = a + b + d           ! OK to mix default with concrete kind

end program p
