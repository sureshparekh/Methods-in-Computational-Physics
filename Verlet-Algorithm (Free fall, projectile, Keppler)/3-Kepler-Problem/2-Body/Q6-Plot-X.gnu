#set terminal type 'qt'
set xlabel 'X'
set ylabel 'Y'
set title "Q6 - Kepler's 2 Body Problem"

pl './Q6-Position-A.txt' u 2:3 w lp title "Paticle A",'./Q6-Position-B.txt' u 2:3 w lp title "Paticle B"