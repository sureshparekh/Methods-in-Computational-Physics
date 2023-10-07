!Name: Suresh Parekh
!Metropolis Monte Carlo simulation of a 1D Lennard-Jones fluid

module ModGen
    implicit none
    real*8::L,T,xCut,halfL
    real*8,allocatable::x(:)
    integer::n
end module ModGen

program LJFluid
    use ModGen
    implicit none
    real*8::xTest,delta,r,delX,uOld,uNew,xj,delU,u1,uTot,w,a(2),nDisc,sumU,avgU
    integer::i,j,k,p,nmc

    L=40.0d0        !box length
    n=20            !number of particles
    T=0.3d0         !Temperature
    nmc=5000        !number of monte-carlo cycles
    xCut=3.0d0      !cutoff value 
    delta=0.3d0     !given range
    halfL=L/2.0d0   !calculating half lenght to reduce calculation
    a=[0.95d0,1.12d0]   !for different values of a(lattiece constant)
	nDisc=3000		!number of mcs value we have to discard
    
    allocate(x(n))  !allocating array size

    open(1,file='LJa95.txt', status='unknown')		!to store output file for a=0.95
    open(2,file='LJa112.txt', status='unknown')		!to store output file for a=1.12

    do k=1,2		!for select value of a
        do i=0,n-1	
	        x(i+1)=i*a(k)	!setting particle's initial positions
	    enddo
	    sumU=0.0d0
        do i=1,nmc      !MC cycle
            do p=1,n    !generate random positions
                r=rand()
                j=int(n*r)+1    !teking random particle
                if (j>n)j=n     
                
                r=rand()
                delX=2.0d0*(r-0.5d0)*delta
                xj=x(j)             !taking jth partical position
                xTest=xj+delX       !changing randomly choosen particles position
                
                if (xtest<0) then   !applying boundary condition 
                    xtest=xtest+L
                else if (xtest>L) then
                    xtest=xtest-L
                endif
                
                call Energy1(j,xj,u1)   !calculating old and new energy for each flip
                uOld=u1
                
                xj=xTest
                call Energy1(j,xj,u1)
                uNew=u1
                
                if (uNew<=uOld) then    !applying transition probabality condition
                    x(j)=xTest
                else
                    delU=uNew-uOld
                    w=exp(-delU/T)      !w=transition probability
                    r=rand()
                    if (r<=w) x(j)=xTest
                endif
            enddo

            call uTotal(uTot)   !calculating total energy
            write(k,*) i,uTot
            if (i>nDisc) sumU=sumU+uTot
        enddo
        avgU=sumU/(nmc-nDisc)
        write(*,*)'Average value of Total Potential Energy for a=',a(k),'is',avgU  
    enddo   
end program LJFluid

subroutine Energy1(j,xj,u1)
    use ModGen
    implicit none
    real*8,intent(in)::xj
    integer,intent(in)::j
    real*8,intent(out)::u1
    real*8::xji,x6i
    integer::i

    u1=0.0d0
    do i=1,n
        if(i==j) cycle  !if condition follow then cycle statement discard bellow calculation for that i
        
        xji=xj-x(i)             !calculating distance between 2 particle
        if (xji<-halfL) then    !applying boundary condition 
            xji=xji+L
        elseif (xji>halfL) then
            xji=xji-L
        endif 
        
        if (abs(xji)>xCut) cycle
        x6i=1.0d0/(xji**6)  !calculating inverse of distance^6
        
        u1= u1 + (4.0d0*x6i*(x6i-1.0d0))    !calculating energy of 1 spin
    enddo
    return
end subroutine Energy1

subroutine uTotal(uTot)
    use ModGen 
    implicit none
    real*8,intent(out)::uTot
    real*8::xij,x6i
    integer::i,j

    uTot=0.0d0

    do i=1,n-1
        do j=i+1,n
            xij=x(i)-x(j)           !calculating distance between 2 particle
            if (xij<-halfL) then    !applying boundary condition 
                xij=xij+L
            elseif (xij>halfL) then
                xij=xij-L
            endif 

            if (abs(xij)>xCut) cycle
            x6i=1.0d0/(xij**6)  !calculating inverse of distance^6
            
            uTot=uTot + (4.0d0*x6i*(x6i-1.0d0))     !calculating total energy
        enddo
    enddo
    return
end subroutine uTotal
