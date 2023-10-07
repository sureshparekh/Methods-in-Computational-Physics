!Metropolis of taking ration of two integral


program q4c1
implicit none

real :: r1,r2,x,w,f,delta,integral
integer :: i,n

n=110000
delta=1.0
x=0.0

integral = 0
open (unit=1, file ='q1.txt')

do i = 1,n
    call random_number(r1)
    call random_number(r2)
    r1 = delta*(2*r1 - 1)
    w = f(x + r1)/f(x)
    
    if(r2 <= w) then
        x = x + r1
    endif
    if (i > 10000) then
        write(1,*)x
        integral = integral + 
    endif  
        
enddo
write(*,*) "the value of the integral is :", integral/100000  ! 11000-1000

end program q4c1

function f(x)
implicit none 
real :: f,x
if (x<0.0) then
    f=0.0
elseif (x>=0.0) then
    f=exp(-2*x) ! A=1
end if 
end function f

!OUTPUT
!the value of the integral is :  0.746771812
