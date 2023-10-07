#set terminal type 'qt'
set xlabel 'Time'
set ylabel "Velocity"
set title "Verlet Algorithm for Newtons Eqn (Motion Under Gravity)"

v0=0.0      #intial velocity
a=-9.8      #acceleration
y(x)=v0+a*x
pl './Q4.txt' u 1:3 w p title "By Verlet Algorithm",y(x) title "Analytical Solution"