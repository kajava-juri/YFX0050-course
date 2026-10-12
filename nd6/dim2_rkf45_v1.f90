real y(4),work(27),mu_atm,m_keha
integer iwork(5)
common g_rask,ro_atm_0,p1,p2,p3
external f
open(10,file="dim2.dat")

pi=3.1415926536
g_rask=9.814
r_keha=0.05
s_keha=pi*r_keha**2
c_keha=0.47
v_keha=4.*pi*r_keha**3/3
m_keha=0.1
ro_keha=m_keha/v_keha
p_atm=100000.
t_atm=293.
mu_atm=29.
r_gaas=8314.

ro_atm_0=p_atm*mu_atm/(r_gaas*t_atm)
p1=g_rask*mu_atm/(r_gaas*t_atm)
p2=0.5*c_keha*s_keha/m_keha
p3=g_rask*v_keha/m_keha

neqn=4
iflag=1
nt=4000
dt=0.01
t=0.
tout=0.
relerr=1.e-6
abserr=1.e-6

y(1)=0.
y(2)=0.
y(3)=10.
y(4)=10.

do it=1,nt
tout=t+dt

call rkf45(f,neqn,y,t,tout,relerr,abserr,iflag,work,iwork)
if (iflag /= 2)then
iflag=2
endif
if (y(2) <0.) then
stop
endif

ekin=0.5*m_keha*(y(3)*y(3)+y(4)*y(4))
epot=m_keha*g_rask*y(2)
etot=ekin+epot
write (10,*)tout,y,ekin,epot,etot

enddo

stop
end

subroutine f(t,y,yp)
real y(4),yp(4)
common g_rask,ro_atm_0,p1,p2,p3
yp(1)=y(3)
yp(2)=y(4)
ro=ro_atm_0*exp(-p1*y(2))
vabs=sqrt((y(3))**2+y(4)**2)
yp(3)=-p2*ro*vabs*(y(3))
yp(4)=-g_rask-p2*ro*vabs*y(4)+ro*p3
return
end
