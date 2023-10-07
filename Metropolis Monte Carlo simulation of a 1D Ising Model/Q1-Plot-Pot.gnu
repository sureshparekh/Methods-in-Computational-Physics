#set terminal type 'qt'
set xlabel 'nmc'
set ylabel 'Potential Energy per spin'
set title '1D Ising Model'
set xrange [0:500]

pl './Ising1D-P.txt' u 1:2 w l title'Parallel Spin','./Ising1D-AP.txt' u 1:2 w l title'Anti-Parallel Spin'