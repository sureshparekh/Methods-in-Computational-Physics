#set terminal type 'qt'
set xlabel 't'
set ylabel "dy/dx"
set title "Euler's method for 1st ODE"

# y"(x)=-y  
# ==> y=AsinX+B & X=0,y=0==> B=0
#y'=AcosX & X=0,y'=1==> A=1
y(x)=cos(x)
pl './output.txt' u 1:3 w p title "y' By Euler's Method",y(x) title "Analytical Solution"
