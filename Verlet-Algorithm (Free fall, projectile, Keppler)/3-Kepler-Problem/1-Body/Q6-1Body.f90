!Name: Suresh Parekh
!Kepler's 1 Body Problem 

module ModGen
    implicit none
    real::GM
    real*8::r    
end module ModGen

program Q6A
    use ModGen
    implicit none
    real::dt,t0,t1
    real*8::x0,x1,vx0,vx1,vy0,vy1,ax,ay,y1,y0,v

    open(1,file='Q6-Position.txt', status='unknown')
    open(2,file='Q6-Velocity.txt', status='unknown')

    GM=1.0         ! G=Universal Gravitational COnt, M=Mass of Masive Object
    dt=0.01           !increement 
    t0=0.0         !intial time
    x0=1.0d0       !intial x position
    y0=0.0d0       !intial y position
    vx0=0.3d0      !initial velocity in x direction
    vy0=0.7d0      !initial velocity in y direction

    ! for cicular orbit vx=0 vy=1 at x=1 y=0, there should be defferent values for elliptical orbit try it


    write(1,*) "#   t                  x                         y"
    write(1,*) t0,x0,y0
    write(2,*) "#   t                   Vx                       Vy"
    write(2,*) t0,vx0,vy0

    r = (x0**2) + (y0**2)
    v = (vx0**2) + (vy0**2)

    do
        t1=t0+dt        ! time
        
        ! X Component
        x1=x0+(vx0*dt)+(0.5d0*ax(x0)*dt**2)    !Position at time t1
        vx1=vx0+(0.5d0*(ax(x1)+ax(x0))*dt)       !velocity at time t1
        
        ! Y Component
        y1=y0+(vy0*dt)+(0.5d0*ay(y0)*dt**2)    !calculating value of x at time t1
        vy1=vy0+(0.5d0*(ay(y1)+ay(y0))*dt)       !calculating value of x velocity at time t1
        
        write(1,*) t1,x1,y1
        write(2,*) t1,vx1,vy1

        t0=t1
        x0=x1
        y0=y1
        vx0=vx1
        vy0=vy1
        if (t0>2*3.1415) exit   !Period=2*pi*r/v , r=v=1
    enddo
end program Q6A
   
function ax(x)
    use ModGen
    implicit none
    real*8,intent(in)::x 
    real*8::ax

    ax=-GM*x/r**3     ! acceleration along x direction
    return 
end function ax

function ay(y)
    use ModGen
    implicit none
    real*8,intent(in)::y
    real*8::ay

    ay=-GM*y/r**3     ! acceleration along y direction
    return 
end function ay
