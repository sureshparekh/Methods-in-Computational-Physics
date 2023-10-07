#set terminal type 'qt'
set xlabel 'Time'
set ylabel 'Postion'
set title "Verlet Algorithm for Newtons Eqn (Motion Under Gravity)"

x0=1000.0     #Hight
v0=0.0      #intial velocity
a=-9.8      #acceleration
y(x)=x0+(v0*x)+((a*x**2)/2)
pl './Q4.txt' u 1:2 w p title "By Verlet Algorithm",y(x) title "Analytical Solution"