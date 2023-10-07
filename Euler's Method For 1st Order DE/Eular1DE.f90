!Name: Suresh Parekh
!Euler's Method For 1st Order DE

program Q1
    implicit none
    real::h,t0,t1
    real*8::y0,y1,f
    integer::i,n

    open(1,file='output.txt', status='unknown')

    h=0.01      !increement 
    n=1000      !number of steps
    t0=0.0      !intial time
    y0=1.0d0    !y(t0)=1.0

    write(1,*) "#     t0          y0"
    write(1,*) t0,y0

    do i=1,n
        y1=y0+h*f(t0,y0)    !calculating value of y at t1
        t1=t0+h

        write(1,*) t1,y1
        t0=t1
        y0=y1
    enddo
end program Q1

function f(t,y)
    implicit none
    real,intent(in)::t
    real*8,intent(in)::y
    real*8::f

    f=3.0d0*t/y     !f=dy/dt=3t/y
    return 
end function f
