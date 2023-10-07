!Name: Suresh Parekh
!Runge Kutta Method For 2nd Order DE

program Q3
    implicit none
    real::h,x0,x1
    real*8::u0,v0,u1,v1,f,g,k1u,k2u,k3u,k4u,k1v,k2v,k3v,k4v,sixthpart
    integer::i,n

    open(1,file='Q3.txt', status='unknown')

    h=0.01      !increement 
    n=1000      !number of steps
    x0=0.0      !intial time
    u0=0.0      !u0=y(x0)=0.0
    v0=1.0      !v0=y'(x0)=1.0
    sixthpart=1.0d0/6.0d0   !calcuating 1/6 
    write(1,*) "#   x         u=y           v=dy/dx"
    write(1,*) x0,u0,v0

    do i=1,n
        ! K1
        k1u=h*f(x0,u0,v0)
        k1v=h*g(x0,u0,v0)
        ! K2
        k2u=h*f(x0+h/2.0,u0+(k1u/2.0d0),v0+(k1v/2.0d0))
        k2v=h*g(x0+h/2.0,u0+(k1u/2.0d0),v0+(k1v/2.0d0))
        ! K3
        k3u=h*f(x0+h/2.0,u0+(k2u/2.0d0),v0+(k2v/2.0d0))
        k3v=h*g(x0+h/2.0,u0+(k2u/2.0d0),v0+(k2v/2.0d0))
        ! K4
        k4u=h*f(x0,u0+k3u,v0+k3v)
        k4v=h*g(x0,u0+k3u,v0+k3v)
            
        u1=u0+sixthpart*(k1u+2.0d0*k2u+2.0d0*k3u+k4u)    !calculating value of y at x1
        v1=v0+sixthpart*(k1v+2.0d0*k2v+2.0d0*k3v+k4v)    !calculating value of y' at x1
        x1=x0+h

        write(1,*) x1,u1,v1
        x0=x1
        u0=u1
        v0=v1
    enddo
end program Q3

! Here given function is 
!     d2y/dx2 = -y
! in this Method we use 
!     u=y     ,   v=dy/dx
!     and f=du/dx=dy/dx     ,   g=dv/dx=d2y/dx2
    
function f(x,u,v)
    implicit none
    real*8,intent(in)::u,v
    real,intent(in)::x
    real*8::f

    f=v     !f=du/dt = dy/dt = v
    return 
end function

function g(x,u,v)
    implicit none
    real*8,intent(in)::u,v
    real,intent(in)::x
    real*8::g

    g=-u     !g=dv/dx=d2y/dx2 = -y = -u
    return 
end function g
