implicit none

real y(2),work(15),mu_atm,m_keha,a(2), t_atm,r,ro_atm_0,p1,p2,p3
real dt,relerr,abserr,tout,t,g_rask,r_keha,s_keha,c_keha,v_keha,p_atm

real, parameter :: pi = 3.141592653589793

integer iwork(5), iflag,neqn,it, nt
common g_rask,ro_atm_0,p1,p2,p3
external func
open(10,file="dim1_rkf45.dat")
!*******************
g_rask=9.814
r_keha=1.8
c_keha=0.47
m_keha=15.
p_atm=102700.
t_atm=285.

! m_keha=5.
mu_atm=29.
r=8314.
nt=50000
dt=0.1
relerr=1.e-6
abserr=1.e-6
iflag=1
t=0.
neqn=2
!********************
s_keha=pi*r_keha**2
v_keha=4.*pi*r_keha**3/3
ro_atm_0=p_atm*mu_atm/(r*t_atm)
p1=g_rask*mu_atm/(r*t_atm)
p2=0.5*c_keha*s_keha/m_keha
p3=g_rask*v_keha/m_keha

y(1)=0.
y(2)=0.

do it=1,nt
tout=t+dt
call rkf45(func,neqn,y,t,tout,relerr,abserr,iflag,work,iwork)

if (iflag /= 2) then
print *,iflag
iflag=2
  else
call func(t,y,a)
write (10,*) tout,y,a(2)
endif

if(y(1).le.0.) then
stop
endif


enddo
stop
end

subroutine func(t,y,yp)
real y(2),yp(2),m_keha
common g_rask,ro_atm_0,p1,p2,p3
yp(1)=y(2)
roh=ro_atm_0*exp(-p1*y(1))
v=abs(y(2))
yp(2)=-g_rask+p3*roh-p2*roh*v*y(2)
return
end
