#set terminal type 'qt'
set xlabel 'Vx'
set ylabel "Vy"
set title "Q6 - Kepler's 2 Body Problem"

pl './Q6-Velocity-A.txt' u 2:3 w lp title "Paticle A",'./Q6-Velocity-B.txt' u 2:3 w lp title "Paticle B"