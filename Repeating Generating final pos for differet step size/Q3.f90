!Name: Suresh Parekh
!Repeating Q2 for differet step size

program Q3
    implicit none
    real::sigma,rootn
    integer::n(5),i,m

    open(1, file='Q3.txt', status='unknown')
    n=[1000,2000,4000,8000,16000]   !Different n value
    m=1000000

    do i=1,5
        call Q2(n(i),m,sigma)
        rootn=sqrt(real(n(i)))
        write(1,*)rootn,sigma
    enddo 
end program Q3

subroutine Q2(n,m,sigma)
    implicit none
    integer,intent(in)::n,m
    real, intent(out)::sigma
    real::mn,sqx
    integer::i,dest,j ,x

    x=0
    sqx=0
    do j = 1,m       !loop to get 10^6 position
        dest=0          !destination or position
        do i=1,n        !loop to get positions after 1000 steps
            dest= dest+ (-1)**nint(rand())
        enddo
        
        x = x + dest				!Summing the final positions
        sqx = sqx + dest**2			!Summing the squares of final positions
        
    enddo

    !Mean
    mn= real(x)/real(m)          

    !Variance = <x^2> - <x>^2
    !sigma=sqrt(vatiance)
    sigma = sqrt((sqx/m) - mn**2)
     

end subroutine Q2
