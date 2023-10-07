#set terminal type 'qt'
set xlabel 'nmc'
set ylabel 'Magnetization per Spin'
set title '1D Ising Model'
set xrange [0:5000]

pl './Ising1D-P.txt' u 1:3 w l title'Parallel Spin','./Ising1D-AP.txt' u 1:3 w l title'Anti-Parallel Spin'