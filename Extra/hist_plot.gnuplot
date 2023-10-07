#set terminal 'qt' enhanced
set xlabel 'x'
set ylabel '{/Symbol m}(x)'
set xrange [-150:150]
set yrange [-0.001:0.30]

set title "Distribution of data and Gaussian (SJ)"

pl 'q2-hist.txt' using 1:2 w lp 
