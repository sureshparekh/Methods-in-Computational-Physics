program q3
implicit none

real:: a1,b,mean,std_dev,x2bar
integer :: nsteps, nwalks, i, j

nwalks=10**6
nsteps=1000
open(unit=1,file='q3.txt')

x2bar=0.0
mean=0.0
do j=1, nwalks                              ! generating 4 different randome no. 
    a1=0.0
    
    do i=1,nsteps
        call random_number(b)
        if (b <= (1.0/3.0)) then 
        	a1=a1+1
        else if (b > (1.0/3.0) .AND. b <= (2.0/3.0)) then
        	a1=a1
        else if (b > (2.0/3.0) .AND. b < 1.0) then
        	a1=a1-1
        end if 
        
! 	if (b <= (1.0/3.0)) then
! 		a1=a1+1
! 	else if (b>=(2.0/3.0)) then
! 		a1=a1-1
! 	end if 
 		       
        
        
    end do
    mean=mean+a1
    x2bar=x2bar+a1**2
    write(1,*) a1 
    
end do 

mean=real(mean)/real(nwalks)

x2bar=x2bar/real(nwalks)

std_dev= (x2bar-mean**2)**0.5

write(*,*) mean, std_dev
end program q3

! 3.999 e -02 , 25.8132
