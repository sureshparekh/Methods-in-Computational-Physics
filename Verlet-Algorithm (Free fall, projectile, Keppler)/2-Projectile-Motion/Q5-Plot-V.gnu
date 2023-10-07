#set terminal type 'qt'
set xlabel 'Time'
set ylabel "Velocity Y-Component"
set title "Projectile Motion Using Verlet Algorithm"

v0=1000
theta=3.14/6   #Angel of throw
vy0=v0*sin(theta)      #intial velocity
a=-9.8      #acceleration
y(x)=vy0+a*x
pl './Q5-Velocity.txt' u 1:3 w p title "By Verlet Algorithm",y(x) title "Analytical Solution"