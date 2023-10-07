!Suresh Parekh
!To find the area overlapped between two overlapping circles

program q2b
implicit none   

real ::r1,r2,x,error,area,a,b,area_rect,f,h,y
integer ::i,j,n,n1

a=0.5
b=0.5*(3**0.5)
area_rect= a*b

open(unit=1,file='q1.txt')

do i=1,7
    n1=0
    n=100*4**(i-1)                   
    do j=1,n
        call random_number(r1)     
        x=0.5*r1
        call random_number(r2)        
        y=0.5*(3**0.5)*r2
        
        if (y<=f(x)) then
            n1=n1+1
        end if
        
    end do
    area=(real(n1)/real(n))*area_rect

    
    write(1,*) n,4*area
end do 

end program q2b

function f(x)
implicit none
real :: f,x
f=(1-x**2)**0.5
end function f
