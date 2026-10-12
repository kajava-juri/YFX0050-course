clear
a=load('dim2.dat');
subplot (2,1,1)
hold on
plot(a(:,2),a(:,3))
t=1:30:length(a(:,1));
quiver(a(t,2),a(t,3),a(t,4),a(t,5),0.8)
legend('traj','v')
grid on
subplot(2,1,2)
plot(a(:,2),a(:,3))

grid on