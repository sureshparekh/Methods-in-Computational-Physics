!Write a program to
!calculate the trajectory of the projectile, using the velocity Verlet algorithm to integrate
!Ql ( a) A body of mass m =
!the Newton 's equation of motion. Take the origin as the point of projection on the
!ground , x-axis along horizontal and y-axis along vertical direction. Take the instant of
!projection as time t = 0. Plot the potential ~ energy V and kinetic energy K of the
!body as a function of time on the same set of axis for the period TI ::; t ::; tmax , where
!tmax is the time when body hits the ground (Hint: To detect the body hitting the
!ground , check for the change of sign of a particular coordinate). Use a time interval of
!/Sl= 0.001 s for integration, and acceleration due to gravity g
!= 9.81
!m/s .





PROGRAM projectile
    IMPLICIT NONE
    REAL*8 :: x, y, vx, vy, ax, ay, dt, t, tmax, g
    REAL*8 :: k, v, xmax, ymax
    INTEGER :: i, n
    PARAMETER :: m = 1.0
    PARAMETER :: pi = 3.14159265358979323846264338327950288
    
    ! Set initial conditions
    x = 0.0
    y = 0.0
    vx = 1.0 / sqrt(2)
    vy = 1.0 / sqrt(2)
    dt = 0.001
    t = 0.0
    tmax = 2.0 * vy / g
    g = 9.81
    
    ! Open output file for plotting
    OPEN(10, FILE='output.dat', STATUS='replace')
    
    ! Integration loop
    n = INT(tmax / dt)
    DO i = 1, n
        ! Calculate acceleration
        ax = 0.0
        ay = -g
        
        ! Update position
        x = x + vx * dt + 0.5 * ax * dt**2
        y = y + vy * dt + 0.5 * ay * dt**2
        
        ! Calculate potential and kinetic energy
        v = m * g * y
        k = 0.5 * m * (vx**2 + vy**2)
        
        ! Write data to output file
        WRITE(10, *) t, x, y, v, k
        
        ! Update velocity
        ax = 0.0
        ay = -g
        vx = vx + 0.5 * ax * dt
        vy = vy + 0.5 * ay * dt
        ax = 0.0
        ay = -g
        vx = vx + 0.5 * ax * dt
        vy = vy + 0.5 * ay * dt
        
        ! Update time
        t = t + dt
        
        ! Stop simulation when projectile hits ground
        IF (y < 0.0) EXIT
    END DO
    
    ! Close output file
    CLOSE(10)
    
    ! Plot data using external program (e.g. gnuplot)
    PRINT *, 'Plotting data...'
    CALL SYSTEM('gnuplot -e "set xlabel ''Time (s)''; set ylabel ''Energy (J)''; plot ''output.dat'' using 1:4 with lines title ''Potential Energy'', ''output.dat'' using 1:5 with lines title ''Kinetic Energy''"')
    
END PROGRAM projectile

