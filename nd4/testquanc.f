      implicit none
      real a,b,abserr,relerr,result,errest,flag
      real fun
      external fun
      integer nofun
      a=0
      b=1
      abserr=1e-8
      relerr=1e-8

      call quanc8(fun,a,b,abserr,relerr,result,errest,nofun,flag)

      print *, 'flag = ', flag

      print *, 'result = ', result * 4
      print *, 'errest = ', errest
      print *, 'nofun = ', nofun
      end

      real function fun(x)
      real x
      fun=sqrt(1-x*x)

      end