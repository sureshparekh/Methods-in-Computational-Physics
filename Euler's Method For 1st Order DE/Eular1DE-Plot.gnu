#set terminal type 'qt'
set xlabel 't'
set ylabel 'y'
set title "Euler's method for 1st ODE"

# f=dy/dt=3t/y==>y^2=3t^2 + 1
y(x)=sqrt(3*(x**2)+1)
pl './output.txt' w p title "By Euler's Method",y(x) title "Analytical Solution"
