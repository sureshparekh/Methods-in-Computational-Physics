!Name: Suresh Parekh
!Metropolis Monte Carlo simulation of a 1D Ising Model

module ModGen
    implicit none
    real*8::temp
    integer,allocatable::S(:)
    integer::n,nmc
end module ModGen

program Ising1D
    use ModGen
    implicit none
    real*8::r,w,uTot1,totMag1,sumM,sumU,sum2M,sum2U,Cv,Xm
    integer::i,j,k,delU,u1,uTot,totMag,nDisc
    
    !J=1.0d0        	
   	!Kb=1.0d0		!Boltzmann COnstant
    !B=0.0d0        !External Magnetic Field
    n=1000          !number of particles
    temp=0.7d0      !Temperature
    nmc=500000      !number of monte-carlo cycles 
	nDisc=200000	!number of cycles we want to discard
    
    allocate(S(n))  !allocating array size

    open(1,file='Ising1D-AP.txt', status='unknown')		!to store output file for a=0.95
	
	!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Initialization %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	! do i=1,n
	! 	r=rand()
	! 	S(i)=(-1)**nint(r)		!there is 3 methods 1st is randomly orient spins
	! enddo
	
	! S(:)=1						!2nd is all spins up (or Down)

	do i=1,n					! 3rd is alternatively up-down
		if (mod(i,2)==0) then
			S(i)=-1		
		else
			S(i)=1
		endif	
	enddo
	!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	sumU=0
	sumM=0
	do i=1,nmc		!monte-carlo cycle
        do j=1,n	!selecting random particle and flip it's spin 
			r=rand()
			k=int(n*r)+1
			if (k>n) k=n
			
			call Energy1(k,u1)		!calculating particle's old energy 
			!uOld=u1
			!uNew=-uOld				!particle's new energy is simply minus of old energy
			!delU=uNew-uOld =-2*uOld
			
			delU=-2*u1			    !calculating delta U
			
			if(delU<=0) then 
				S(k)=-S(k)			!for delU<=0  transition prob. w>=1
			else
				w=exp(-dble(delU)/temp)	!calculating transition probability
				r=rand()
				if (r<w) S(k)=-S(k)
			endif
		enddo

		call TotEnergy(uTot)
		totMag=sum(S)         !Total magnetization

		uTot1=(dble(uTot))/n			! Energy per spin
		totMag1=dble(totMag)/n			! Magnetization per spin
		write(1,*)i,uTot1,totMag1

		if (i>nDisc) then
			sumU=sumU+uTot1
			sum2U=sum2U + uTot1**2
			sumM=sumM+totMag1
			sum2M=sum2M + totMag1**2
		endif
	enddo
	
	Cv = ((sum2U/(nmc-nDisc))-(sumU/(nmc-nDisc))**2)/temp**2		!Cv=Var(E)/KbT^2
    Xm = ((sum2M/(nmc-nDisc))-(sumM/(nmc-nDisc))**2)/temp		!Xm=Var(M)/KbT

    write(*,*) "Specific Heat = ", Cv

    write(*,*) "Susceptibility = ", Xm

end program	Ising1D
		
subroutine Energy1(k,u1)
	use ModGen,Only:S,n
	implicit none
	integer,intent(in)::k
	integer,intent(out)::u1
	
	if (k==1) then
		u1=-S(1)*(S(2)+S(n))		!boundary particle
	elseif (k==n) then
		u1=-S(n)*(S(1)+S(n-1))		!boundary partiicle
	else
		u1=-S(k)*(S(k-1)+S(k+1))	!non-Boundary Particle
	endif
	return
end subroutine Energy1

subroutine TotEnergy(uTot)
	use ModGen, only:S,n
	implicit none
	integer,intent(out)::uTot
	integer::i
    uTot=0.0d0
	do i=1,n-1
		uTot=uTot-S(i)*S(i+1)	!addinfg each pair energy
	enddo

	uTot=uTot-S(1)*S(n)			!adding boundary pair energy
	return
end subroutine TotEnergy


    ! Specific Heat =    4.2098012530230309E-004
    ! Susceptibility =    2.4692457996298777E-002
