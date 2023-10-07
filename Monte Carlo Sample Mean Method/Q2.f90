!Name: Suresh Parekh
!Monte Carlo Sample Mean Method

program SampleMean
    implicit none
    real::x,f,tArea,tA,abError,sumfx
    Integer:: i,n

    !a trapezium which has parallel sides equal to 2 and 1 units respectively and height 1 unit
    !I have put trapezium's 1 unit edge at origin
    !So equition of uper line is f(x)=x+1

    open(1, file='SampleMean.txt', status='unknown')
    tA=1.5      !Analytical Area of Trapezium
    n=5     !total n

    do 

        tArea=0     !Area of trapezium 
        sumfx=0
        do i=1,n
            x=rand()
            sumfx=sumfx+f(x)
        end do
        
        tArea=sumfx/n      !tArea = {(b-a)/n}*sum(f(xi)), b=1 a=0
        abError=abs(tArea-tA)

        write(1,*)n, tArea, abError
        if (n==81920) exit
        n=n*2
    end do

end program SampleMean


function f(x)   !Defining Function
    implicit none
    real, intent(in)::x
    real ::f

    f=x+1
end function f
