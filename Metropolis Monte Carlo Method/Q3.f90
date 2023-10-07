!Name: Suresh Parekh
!Metropolis Monte Carlo Method

program Metropolis
    implicit none
    real::r,x0,x1,dx,del,xtest,w,p,s,sumfx,integral
    integer:: i,m,n

    open(1,file='Q3.txt', status='unknown')

    x0=0.0 
    del=0.3     !Delta   
    m=10000     !Number of iteration given to discard
    n=110000    !Number of Iteration
    sumfx=0.0   
    do i=1,n
        r=rand()        !Generating Random Number
        dx=(2*r-1)*del  !dx=a+(b-a)r here [a,b]=[-delta,delta]
        
        xtest=x0+dx
        w=p(xtest)/p(x0)

        if (w>=1) then      !conditions whether to accept or regect xtest values
            x1=xtest
        else
            s=rand()    !Generating Random Number
            if(w>=s) then   
                x1=xtest
            else
                x1=x0
            endif
        endif
        if (i>m) then
            write(1,*)x1
            sumfx=sumfx+(exp(-x1**2)/p(x1))  
        endif 
        x0=x1
    enddo
    !integration = (1/n)*Sum(i=1,n)f(xi)  
    !where f(x)=given function/p(x)
    integral=sumfx/(n-m)
    
    write(*,*)'The value of the integral is', integral
    
end program Metropolis

function p(x)
    implicit none
    real,intent(in)::x
    real::p
    
    if(x>=0 .and. x<=1) then
        p=exp(-x)/(1.0-exp(-1.0))       !p(x)=Ae^-x, A is Normalization Constant
    else                                !A=1/(1-e^-1) by integration p(x)dx=1 Formula
        p=0
    endif
end function p
