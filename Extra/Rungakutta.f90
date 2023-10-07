!Write a program to numerically integrate the following second order differential
!equation using the fourth order Runge-Kutta method :



program runge_kutta

implicit none

integer, parameter :: dp = selected_real_kind(15, 307)
real(dp) :: x, h, y, y1, y2, k1, k2, k3, k4, a

x = 0.0_dp      ! initial x value
y = 1.0_dp      ! initial y value
a = 0.0_dp      ! initial value of second derivative y''

h = 0.001_dp    ! step size
do while (x <= 0.5_dp)
    ! Calculate k1
    k1 = h * y1
    y1 = (2.0_dp * x * y - 6.0_dp * y + a * (1.0_dp - x**2.0_dp)) / (1.0_dp - x**2.0_dp)

    ! Calculate k2
    k2 = h * (y1 + k1 / 2.0_dp)
    y2 = (2.0_dp * (x + h/2.0_dp) * (y + k1/2.0_dp) - 6.0_dp * (y + k1/2.0_dp) + a * (1.0_dp - (x + h/2.0_dp)**2.0_dp)) / (1.0_dp - (x + h/2.0_dp)**2.0_dp)

    ! Calculate k3
    k3 = h * (y2 + k2 / 2.0_dp)
    y2 = (2.0_dp * (x + h/2.0_dp) * (y + k2/2.0_dp) - 6.0_dp * (y + k2/2.0_dp) + a * (1.0_dp - (x + h/2.0_dp)**2.0_dp)) / (1.0_dp - (x + h/2.0_dp)**2.0_dp)

    ! Calculate k4
    k4 = h * (y2 + k3)
    y1 = (2.0_dp * (x + h) * (y + k3) - 6.0_dp * (y + k3) + a * (1.0_dp - (x + h)**2.0_dp)) / (1.0_dp - (x + h)**2.0_dp)

    ! Update y and x values
    y = y + (k1 + 2.0_dp * k2 + 2.0_dp * k3 + k4) / 6.0_dp
    x = x + h
end do

write(*,*) "y(x=0.5) = ", y

end program runge_kutta

