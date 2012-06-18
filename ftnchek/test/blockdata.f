      program use_blockdata
        common w, x
        common /block1/ y, z
        print *, w, x, y, z
      end program use_blockdata

      block data
         common w, x
         data w, x / 1.23, 4.56 /
      end block data

      block data bd1
         common /block1/ y, z
         data y, z / 7.89, 10.11 /
      end blockdata bd1

      blockdata bd2
         common /block2/ q
         data q / 3.14 /
      endblockdata

      block data                      !erroneous 2nd anonymous unit
         common / block3 / i, j
         data i / 13 /, j /23/
      end blo ckdata                  !embedded space example 1

      blockdata bd3
      endblo ckdata                   !embedded space example 2

      block data bd3                  !erroneous 2nd BD3
      e ndblock data                  !embedded space example 3-not caught
