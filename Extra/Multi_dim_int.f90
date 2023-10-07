program q2
implicit none

integer :: i,j,n
real:: r1,r2,r3,pi,f,mean,x,y,z,area
pi=3.14159
open(unit=1,file='q2output.txt')

do i=1,6
	mean=0
	
	n=10**i
	do j=1,n
		call random_number(r1)
		x = pi*(r1+1)
		call random_number(r2)
		y = (pi/4)*(r2+1)
		call random_number(r3)
		z = r3
		
		f= (x**2)*(cos(x*y)*sin(y*z))
		mean=mean+f
		
	end do
	mean = mean /n
	

	
	
	!area = (2*pi-pi)*(pi/2-pi/4)*(1)*mean 
	area = (pi**2)*mean/4 
	write(1,*) n,area

end do 

end program q2
