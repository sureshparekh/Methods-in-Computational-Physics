#set terminal type 'qt'
set xlabel 'x'
set ylabel 'p(x)'

f(x)=x>=0 & x<=1 ? 2*(1-x):0
plot '.\Q1-Hist.txt' w p title 'Simulated Dist', f(x) title'Theoratical Dist'

