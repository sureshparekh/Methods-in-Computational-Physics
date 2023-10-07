#set terminal type 'qt'
set xlabel 'X'
set ylabel 'Y'
set title "Projectile Motion Using Verlet Algorithm"

pl './Q5-Position.txt' u 2:3 w lp title "By Verlet Algorithm"