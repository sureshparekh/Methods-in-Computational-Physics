!Name: Suresh Parekh
! Random Walk Generator

program randwalk
implicit none
integer:: n,i,j,x,sumx,m
real::var,r,mean,sumx2,p
m = 1000000
sumx2 = 0
sumx = 0
n = 1000
open(unit=1, file='output.txt')
do j = 1,m
	x = 0
	do i = 1,n
		r = rand()
		if (r>=(0.666)) then
			x=x+1
		else if (r <=(0.333)) then
			x=x-1
		endif
	enddo
	write(1,*)x,r
	sumx = sumx + x
	sumx2 = sumx2 + x**2
enddo
close(1)

mean = real(sumx)/real(m)
var = (sumx2/real(m)) - mean**2
write(*,*) mean
write(*,*) var



end program randwalk
