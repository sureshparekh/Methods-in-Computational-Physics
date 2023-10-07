!Name: Suresh Parekh
!Multidimentional Integrals

module g7
    implicit none
    real,allocatable::x(:)
    integer::n
end module g7

program MultInte
    use g7
    implicit none
    real:: r,pi,sumfx,integral,f1,sumfx2,sigma,error,u
    integer::i,j,nmc

    n=3         !total number of dimentions
    nmc=1000000   !number of MC cycles
    pi=3.1415       
    sumfx=0.0  
    sumfx2=0.0
    
    allocate(x(n))
    do i=1,nmc    !for MC Cycles

        do j=1,n    !to generate array 
            r=rand()     !Generating Random Number
            x(j)=pi*r      !dx=a+(b-a)r here [a,b]=[0,pi]
        enddo
        
        call f(u)
        f1=u
        sumfx =sumfx+f1          !summing values
        sumfx2=sumfx2+f1**2      !summing squares
    enddo
    
    integral=(sumfx*pi**3)/nmc              !Integral = {(f-e)(d-c)(b-a)/n}*sum(f(xi,x2,x3)), b,d,f=pi a,c,e=0
    
    sumfx=sumfx/nmc                !taking mean of fx
    sumfx2=sumfx2/nmc              !taking mean of fx**2
    sigma=sqrt(abs(sumfx**2 - sumfx2))  !calculating sigma of fx
    error=sigma/sqrt(real(nmc))         !calculating erroe in calculation of integral
    
    write(*,*)"Ingragral, I=",integral
    write(*,*)"Error =",error
end program MultInte

subroutine f(u)
    use g7
    implicit none
    real,intent(out)::u
    real::z
    integer::i
    
    z=1
    do i =1,n
        z=z*x(i)
    enddo
    
    u=sin(z)
end subroutine f
