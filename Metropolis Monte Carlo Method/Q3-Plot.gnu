#set ternminal type 'qt'
set xlabel 'x'
set ylabel 'p(x)'

f(x)=exp(-x)/(1.0-exp(-1.0))

pl 'C:\Users\DELL\Fortran\Assinment-4\Q3\q3-hist.txt' w p title "Simulated Dist",f(x) title 'Theoretical Dist' 