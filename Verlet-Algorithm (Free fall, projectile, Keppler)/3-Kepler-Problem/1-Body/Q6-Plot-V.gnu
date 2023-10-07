#set terminal type 'qt'
set xlabel 'Time'
set ylabel "Velocity Y-Component"
set title "Q6 - Kepler's 1 Body Problem"

pl './Q6-Velocity.txt' u 1:3 w p title "By Verlet Algorithm"