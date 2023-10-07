!Name: Suresh Parekh
!Projectile Motion Using Verlet Algorithm 

program Q5
    implicit none
    real::dt,t0,t1,theta
    real*8::x0,v0,x1,vx0,vx1,vy0,vy1,ax,ay,y1,y0

    open(1,file='Q5-Position.txt', status='unknown')
    open(2,file='Q5-Velocity.txt', status='unknown')

    dt=1                 !increement 
    t0=0.0               !intial time
    x0=0.0d0             !intial x position
    y0=0.0d0             !intial y position
    v0=1000              !initial velocity
    theta=3.14/6         !Angel of throw
    vx0=v0*cos(theta)    !initial velocity in x direction
    vy0=v0*sin(theta)    !initial velocity in y direction

    write(1,*) "#   t                        x                            y"
    write(1,*) t0,x0,y0
    write(2,*) "#   t                       Vx                         Vy"
    write(2,*) t0,vx0,vy0

    do
        t1=t0+dt        ! time
        ! X Component
        x1=x0+(vx0*dt)+(0.5d0*ax(t0)*dt**2)    !Position at time t1
        vx1=vx0+(0.5d0*(ax(t1)+ax(t0))*dt)       !velocity at time t1
        
        ! Y Component
        y1=y0+(vy0*dt)+(0.5d0*ay(t0)*dt**2)    !calculating value of x at time t1
        vy1=vy0+(0.5d0*(ay(t1)+ay(t0))*dt)       !calculating value of x velocity at time t1
        
        if (y1<0.0d0) exit
        write(1,*) t1,x1,y1
        write(2,*) t1,vx1,vy1

        t0=t1
        x0=x1
        y0=y1
        vx0=vx1
        vy0=vy1
    enddo
end program Q5
   
function ax(t)
    implicit none
    real,intent(in)::t
    real*8::ax

    ax=0.0d0     ! acceleration along x direction
    return 
end function ax

function ay(t)
    implicit none
    real,intent(in)::t
    real*8::ay

    ay=-9.8d0     ! acceleration along y direction
    return 
end function ay
