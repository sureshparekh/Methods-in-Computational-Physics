!Name: Suresh Parekh
!Kepler's 2 Body Problem 

module ModGen
    implicit none
    real*8::G,mA,mB
end module ModGen

program Q6B
    use ModGen
    implicit none
    real::dt,t0,t1
    real*8::vxA0,vxA1,vyA0,vyA1,xA0,xA1,yA0,yA1
    real*8::vxB0,vxB1,vyB0,vyB1,xB0,xB1,yB0,yB1
    real*8::Axa0,Axb0,Aya0,Ayb0,Axa1,Axb1,Aya1,Ayb1

    open(1,file='Q6-Position-A.txt', status='unknown')
    open(2,file='Q6-Velocity-A.txt', status='unknown')
    open(3,file='Q6-Position-B.txt', status='unknown')
    open(4,file='Q6-Velocity-B.txt', status='unknown')

    !%%%%%%%%%%%%%%%%%%%%%%% Initialzation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    G=1.0d0        ! G=Universal Gravitational Cont
    dt=0.001         !increement 
    t0=0.0         !intial time

    mA=1.0d0        ! mass of Object A
    xA0=0.5d0       ! x position of A
    yA0=0.0d0       ! y position of A
    vxA0=0.0d0      ! velocity in x direction of A
    vyA0=0.5d0      ! velocity in y direction of A
    
    mB=1.0d0        ! mass of Object B
    xB0=-0.5d0      ! x position of B
    yB0=0.0d0       ! y position of B
    vxB0=0.0d0      ! velocity in x direction of B
    vyB0=-0.5d0     ! velocity in y direction of B
    !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    ! For A
    write(1,*) "#   t                   xA                         yA"
    write(1,*) t0,xA0,yA0
    write(2,*) "#   t                   vxA                       vyA"
    write(2,*) t0,vxA0,vyA0

    ! For B
    write(3,*) "#   t                  xB                         yB"
    write(3,*) t0,xB0,yB0
    write(4,*) "#   t                   vxB                       vyB"
    write(4,*) t0,vxB0,vyB0

    call acceleration(xA0,xB0,yA0,yB0,Axa0,Axb0,Aya0,Ayb0)

    do
        t1=t0+dt        ! time
        
        ! %%%%%%%%%%  Position calculation  %%%%%%%%%%%%
        ! ##########  Particle A  ###########
        xA1=xA0+(vxA0*dt)+(0.5d0*Axa0*dt**2)
        yA1=yA0+(vyA0*dt)+(0.5d0*Aya0*dt**2)
        ! ##########  Particle B  ###########
        xB1=xB0+(vxB0*dt)+(0.5d0*Axb0*dt**2)
        yB1=yB0+(vyB0*dt)+(0.5d0*Ayb0*dt**2)

        call acceleration(xA1,xB1,yA1,yB1,Axa1,Axb1,Aya1,Ayb1)

        ! %%%%%%%%%%  Velocity calculation  %%%%%%%%%%%%
        ! ##########  Particle A  ###########
        vxA1=vxA0+(0.5d0*(Axa0+Axa1)*dt)
        vyA1=vyA0+(0.5d0*(Aya0+Aya1)*dt)

        ! ##########  Particle B  ###########
        vxB1=vxB0+(0.5d0*(Axb0+Axb1)*dt)
        vyB1=vyB0+(0.5d0*(Ayb0+Ayb1)*dt)

    
        ! For A
        write(1,*) t0,xA0,yA0
        write(2,*) t0,vxA0,vyA0

        ! For B
        write(3,*) t0,xB0,yB0
        write(4,*) t0,vxB0,vyB0

        
        t0=t1

        xA0=xA1
        yA0=yA1
        xB0=xB1
        yB0=yB1

        vxA0=vxA1
        vyA0=vyA1
        vxB0=vxB1
        vyB0=vyB1
        
        Axa0=Axa1
        Axb0=Axb1
        Aya0=Aya1
        Ayb0=Ayb1

        if (t1>2.5) exit
    enddo
end program Q6B

subroutine acceleration(xA,xB,yA,yB,axA,axB,ayA,ayB)
    use ModGen
    real*8, intent(in) :: xA,xB,yA,yB
    real*8, intent(out) :: axA,ayA,axB,ayB
    real*8::rAB
    
    rAB=sqrt(((xB-xA)**2)+(yB-yA)**2)

    axA=-(G*mB*(xA-xB))/rAB**3
    ayA=-(G*mB*(yA-yB))/rAB**3

    axB=-(mA*axA)/mB
    ayB=-(mA*ayA)/mB

    return
end subroutine acceleration
