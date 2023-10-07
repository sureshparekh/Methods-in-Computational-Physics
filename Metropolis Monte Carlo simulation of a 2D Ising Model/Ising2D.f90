!Name: Suresh Parekh
!Metropolis Monte Carlo simulation of a 2D Ising Model

module ModGen
    implicit none
    real*8::temp
    integer,allocatable::S(:,:)
    integer::n,n2,nmc
end module ModGen

program Ising2D
    use ModGen
    implicit none
    real*8::r,w,mm,een
    integer::i,j,k,l,totMag,uTot,u1,delU
    
    !J=1.0d0        	
    !Kb=1.0d0		!Boltzmann COnstant
    n=10	        !size of lattice
    n2=n*n          !number of spins
    temp=1.0d0      !Temperature
    nmc=1000        !number of monte-carlo cycles 
    
    allocate(S(0:n+1,0:n+1))  !allocating array size 0 to n+1 to add boundary conditions 

    open(1,file='Ising2D.txt', status='unknown')		!to store output file for a=0.95
    
    do i=1,n	!initialization
        do j=1,n 
            r=rand()
            S(i,j)=(-1)**nint(r)		!there is 3 methods 1 is randomly orient spins
        enddo
    enddo
    
    call applyPBC()		!to add 0th and (n+1)th row and collumn for boundary cond.

    do i=1,nmc		!monte-carlo cycle
        do j=1,n2	!selecting random particle and flip it's spin 
            r=rand()		!to generate random particle's row index
            k=int(n*r)+1
            if (k>n) k=n

            r=rand()		!to generate random particle's collumn index
            l=int(n*r)+1
            if (l>n) l=n
            
            call Energy1(k,l,u1)		!calculating particle's old energy 
            !uOld=u1
            !uNew=-uOld				!particle's new energy is simply minus of old energy
            !delU=uNew-uOld =-2*uOld
            
            delU=-2*u1			!calculating delta U
            
            if(delU<=0) then 
                S(k,l)=-S(k,l)			!for delU<=0  transition prob. w>=1
                call applyPBC1(k,l)		!if random particle is at boundary then to change its copy in at other boundory
            else
                w=exp(-dble(delU)/temp)	!calculating transition probability
                r=rand()
                if (r<w) then
                    S(k,l)=-S(k,l)
                    call applyPBC1(k,l)	 !same as line 6 line above
                endif
            endif
        enddo
        
        call TotEnergy(uTot)
        totMag=abs(sum(S))           !Total magnetization
        mm=dble(totMag)/dble(n2)   !Total magnetization per spin
        een=dble(uTot)/dble(n2)    !Total energy per spin
        
        write(1,*)i,een,mm
    enddo
end program Ising2D
        
subroutine applyPBC()
    use ModGen,only: S,n
    implicit none
    
    S(0,:)=S(n,:)	!copy nth row to 0th row
    S(n+1,:)=S(1,:)	!copy 1th row to (n+1)th row
    S(:,0)=S(:,n)	!copy nth collumn to 0th collumn
    S(:,n+1)=S(:,1)	!copy 0th collumn to (n+1)th collumn
    return
end subroutine applyPBC

subroutine applyPBC1(k,l)
    use ModGen,only:S,n
    implicit none
    integer::k,l
    
    !this subroutine changes copy of changed spin if it is at boundary  
    if (k==1) S(n+1,l)=S(k,l)
    if (k==n) S(0,l)=S(k,l)
    if (l==1) S(k,n+1)=S(k,l)  
    if (l==1) S(k,0)=S(k,l)
    return
end subroutine applyPBC1

subroutine Energy1(k,l,u1)
    use ModGen,Only:S,n
    implicit none
    integer,intent(in)::k,l
    integer,intent(out)::u1
    
    u1=-S(k,l)*(S(k-1,l)+S(k+1,l)+S(k,l-1)+S(k,l+1))	!same formula for all spin whether it is on boundary or not
    return
end subroutine Energy1

subroutine TotEnergy(uTot)
    use ModGen, only:S,n
    implicit none
    integer,intent(out)::uTot
    integer::i,j
    uTot=0.0d0
    do i=1,n
        do j=1,n
        uTot=uTot-S(i,j)*(S(i+1,j)+S(i,j+1))	!addinfg each pair energy
        enddo
    enddo
    return
end subroutine TotEnergy
