!Name: Suresh Parekh
!Monte Carlo hit or miss method

program HitOrMiss
    implicit none
    real::x,y,f,rArea,tArea,tA,abError
    Integer:: i,n,nh

    !a trapezium which has parallel sides equal to 2 and 1 units respectively and height 1 unit
    !I have put trapezium's 1 unit edge at origin
    !So equition of uper line is f(x)=x+1
    !Take a Rectangle  of edge length  2,1 
    
    open(1, file='HitOrMiss.txt', status='unknown')
    tA=1.5      !Analytical Area of Trapezium
    rArea=2.0    !Area of Rectangle A=2 sq unit
    n=5     !total n
    

    do 
        nh=0    !n hit
        tArea=0     !Area of trapezium 
        do i=1,n
            x=rand()
            y=rand()
            y=2*y   !x=a+(b-a)*r  r is random number
            
            if(f(x)>y) nh=nh+1
        end do

        tArea=rArea*(real(nh)/real(n))      !tArea=A*Nhit/N
        abError=abs(tArea-tA)               !finding absolute error

        write(1,*)n, tArea, abError
        if (n==81920) exit
        n=n*2
    end do

end program HitOrMiss


function f(x)   !Defining Function
    implicit none
    real, intent(in)::x
    real ::f

    f=x+1
end function f
