implicit none
integer, parameter :: dp = kind(1.d0)      ! double precision

integer  :: it, nt
real(dp) :: g_rask, r_keha, s_keha, c_keha, v_keha, m_keha
real(dp) :: p_atm, t_atm, mu_atm, r_atm, ro_atm_0
real(dp) :: p1, p2, p3, dt, t, x, v, dx, a, at, aa, roh
real(dp) :: ekin, epot, etot, workt, worka

real(dp), parameter :: pi = 3.141592653589793

open(10, file='pall_1d.dat')

g_rask=9.814
r_keha=1.8
s_keha=pi*r_keha**2
c_keha=0.47
v_keha=4.*pi*r_keha**3/3
m_keha=15.
p_atm=102700.
t_atm=285.
mu_atm=29.
r_atm=8314.
ro_atm_0=p_atm*mu_atm/(r_atm*t_atm)
p1=0.5*c_keha*s_keha/m_keha
p2=g_rask*v_keha/m_keha
p3=g_rask*mu_atm/(r_atm*t_atm)
nt=40000
dt=0.001
v=10.
x=0.

dt = 0.1
x = 0
v = 0
t = 0

nt=50000

do it=1,nt
    t = t + dt
    roh = ro_atm_0*exp(-p3*x)
    at = -p1*roh*abs(v)*v
    aa = p2*roh
    a = -g_rask + at + aa

    x = x + v*dt + 0.5*a*dt*dt
    v = v + a*dt

    ekin = 0.5*m_keha*v*v
    epot = m_keha*g_rask*x
    etot = ekin + epot

    write(10, '(15f15.5)') t, x, v, roh, a, ekin, epot, etot
enddo

end