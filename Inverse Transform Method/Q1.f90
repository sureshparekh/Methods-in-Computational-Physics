!Name: Suresh Parekh
!Inverse Transform Method

program ITM
    implicit none
    real::x,r
    Integer:: i,n

    open(1,file='Q1.txt',status='unknown')
    n=10000
    do i=1,n    
        r=rand()    !Generating Random Number
        
        !we Get X by using Inverse Transform Method
        !P(x)=integration(p(x))=r   where limits are -infi to x
        !here p(x)=2(1-x) if x in range (0,1) 
        
        x=1.0-sqrt(abs(1.0-r))
        write(1,*)x         
    enddo
end program ITM
