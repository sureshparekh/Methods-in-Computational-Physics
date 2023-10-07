!Name: Suresh Parekh
!Generating Final Position of random walker after 1000 steps

program random
    implicit none
    real::var,mn,ervar
    integer::i,n,dest, m,j ,x,sqx

    n=1000          !number of steps
    m=1000000     !number of iteration
    open(2,file="Q2.txt",status="unknown")	    !Q2 result file
    
    x=0
    sqx=0
    do j = 1,m       !loop to get 10^6 position
        dest=0          !destination or position
        do i=1,n        !loop to get positions after 1000 steps
            dest= dest+ (-1)**nint(rand())
        enddo
        write(2,*) j,dest

        x = x + dest				!Summing the final positions
        sqx = sqx + dest**2			!Summing the squares of final positions
        
    enddo

    !Mean
    mn= real(x)/real(m)          
    write(*,*) "Theoratical Value of Mean : 0"
    write(*,*) "The calculated value of Mean : ", mn, "& Absorlute Error : ", mn  !Absolute Error=|theoratical value-calulated value|
    
    !Variance = <x^2> - <x>^2
    var = (real(sqx)/real(m)) - mn**2 
    ervar=var-n
    write(*,*) "The theoratical value of variance : ", n
    write(*,*) "The calculated value of Variance = <x^2> - <x>^2:", var, "& Absorlute Error : ",ervar
    
    close(2)

end program random
