!Name: Suresh Parekh
!Euler's Method For 2nd Order DE

program Q2
    implicit none
    real::h,x0,x1
    real*8::u0,v0,u1,v1,f,g
    integer::i,n

    open(1,file='output.txt', status='unknown')

    h=0.01      !increement 
    n=1000      !number of steps
    x0=0.0      !intial time
    u0=0.0      !u0=y(x0)=0.0
    v0=1.0      !v0=y'(x0)=1.0

    write(1,*) "#   x         u=y           v=dy/dx"
    write(1,*) x0,u0,v0

    do i=1,n
        u1=u0+h*f(x0,u0,v0)    !calculating value of y at x1
        v1=v0+h*g(x0,u0,v0)    !calculating value of y' at x1
        x1=x0+h

        write(1,*) x1,u1,v1
        x0=x1
        u0=u1
        v0=v1
    enddo
end program Q2

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
