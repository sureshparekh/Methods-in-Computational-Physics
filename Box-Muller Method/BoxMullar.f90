!Name: Suresh Parekh
!Box-Muller Method

program BMM
    implicit none
    real:: rho,s,t,x,pi
    integer::i,n

    open(1,file='output.txt',status='unknown')
    pi=3.14159
    n=100000 

    do i=1,n   
        s=rand()    !Generating Random Number Rho
        t=rand()    !Generating Random Number t

        rho=-4*log(s)   !rho=lamda*log(r) according to ITM on (1/lmd)*e^(-lmd*x)

        !We Get X by using Box-Muller Method
        !By Solving Gaussian for 2D Problem for mu=0,sigma=1
        !Then putting x=(x'-mu)
        !Here x=sqrt(2*rho)*cos(2*pi*t)
        
        !x=mu+(sigma*(sqrt(-2log(s))*cos(2*pi*t))) Direct equation, here i have put rho=-sigma^2*log(s)
        x=10.0+(sqrt(2*rho)*cos(2*pi*t))   
        write(1,*)x         
    enddo
end program BMM
