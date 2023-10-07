#set ternminal type 'qt'
set xlabel 'x'
set ylabel 'p(x)'

pi=3.14159
f(x)=(exp(-((x-10)**2)/8))/(sqrt(2*pi)*2)

pl 'C:\Users\DELL\Fortran\Assinment-4\Q2\q2-hist.txt' w l title "Simulated Dist",f(x) title 'Theoretical Dist' 


