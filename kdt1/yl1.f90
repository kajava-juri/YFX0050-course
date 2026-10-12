implicit none

 integer n,i,ifakt
 read *,n
 if(n.lt.0)then
    print *,"n on neg."
    stop
 endif

 ifakt = 1
 if(n.eq.0.or.n.eq.1)then
    ! print (100),"fakt=", ifakt
    print 100, ifakt
    100 format ("fakt=", i10)
    stop
 endif
 do i=1,n
    ifakt=ifakt*i
 enddo
 print *,ifakt
 stop

end
