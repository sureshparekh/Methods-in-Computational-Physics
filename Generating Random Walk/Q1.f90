!Name: Suresh Parekh
!Generating Random Walk


program randomwalk
    implicit none
    integer::i,n,dest,j

    n=1000  
    open(1,file="Q1-1.txt",status="unknown")	!Q1 result file 1
    open(2,file="Q1-2.txt",status="unknown")	!Q1 result file 2
    open(3,file="Q1-3.txt",status="unknown")	!Q1 result file 3
    open(4,file="Q1-4.txt",status="unknown")	!Q1 result file 4
    
    do j=1,4        !To get 4 random walk
        dest=0  !destination
        write(j,*)0,dest
        do i=1,n
            dest= dest+ (-1)**nint(rand()) !if random number>0.5 nint=1 else 0 so 1 is subtracted & added rispectively
            write(j,*)i,dest
        enddo
    enddo
    
    
end program randomwalk
