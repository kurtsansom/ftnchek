      complex cc,cd
      complex*16 ce
      parameter (cc=(1.2,3.4))
      integer dp
      parameter (dp = kind(1.0d0))
      data cd,de / ( +23e5 , 0 ), (-5 ,  +77)/
      a = (12,32)
      a = (5,-3e5)
      ce = (2.3_dp
     $     ,0.0d0) * ((1.2d6,0.0_dp)+
     $     (3.14q0,2.718q0))
      write(11,7) a
 7    format(1x,2f10.2)
      print 22, (1,2) ,a,(1,2,i=1,2)
 22   format(1x,2e10.3,2f10.2,4i4)
      call abc(123,(456,789))
      end
      subroutine abc(x,z)
      integer x
      complex z
      print *,z+x
      end
