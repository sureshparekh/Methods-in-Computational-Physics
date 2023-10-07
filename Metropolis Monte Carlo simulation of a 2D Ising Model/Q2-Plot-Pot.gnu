#set terminal type 'qt'
set xlabel 'nmc'
set ylabel 'Total Potential Energy Per Spin'
set title '2D Ising Model'
set yrange [-2.1:0]

pl './Ising2D.txt' u 1:2 w l title'Total Potential Energy'