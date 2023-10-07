!Name: Suresh Parekh
!Solution of Newton's Eqn of Motion Using Verlet Algorithm 

program Q4
    implicit none
    real::dt,t0,t1
    real*8::x0,v0,x1,v1,a

    open(1,file='Q4.txt', status='unknown')

    dt=0.1           !increement 
    t0=0.0           !intial time
    x0=1000.0d0      !intial position
    v0=0.0d0         !initial velocity
    write(1,*) "#   time                Position                 Velocity"
    write(1,*) t0,x0,v0

    do
        t1=t0+dt        ! time
        x1=x0+(v0*dt)+(0.5d0*a(t0)*dt**2)    !calculating value of Position at time t1
        v1=v0+(0.5d0*(a(t1)+a(t0))*dt)       !calculating value of velocity at time t1
        
        if (x1<1e-6) exit
        write(1,*) t1,x1,v1
        t0=t1
        x0=x1
        v0=v1
    enddo
end program Q4

   
function a(t)
    implicit none
    real,intent(in)::t
    real*8::a

    a=-9.8d0     !Acceleration
    return 
end function a
