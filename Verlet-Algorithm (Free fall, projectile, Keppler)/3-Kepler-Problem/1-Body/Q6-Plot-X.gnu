#set terminal type 'qt'
set xlabel 'X'
set ylabel 'Y'
set title "Q6 - Kepler's 1 Body Problem"

set yrange [-1.1:1.1]

pl './Q6-Position.txt' u 2:3 w lp title "By Verlet Algorithm"