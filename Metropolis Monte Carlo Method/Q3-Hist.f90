!Name: Suresh Parekh
!Metropolis Monte Carlo Method - Histogram


program histogram
    implicit none
    real:: x(100000), xmid, xmin, xmax, dx, prob(100000),probDensity(110000)
    integer:: j, n, nbin, bin(100000), ibin

    n = 100000
    open(1,file="Q3.txt")   !Given data 
    open(2,file="Q3-Hist.txt")  !To write the data
    do j=1,n
        read(1,*)x(j)
    enddo
    xmin = minval(x)
    xmax = maxval(x)
    dx=0.01
    
    nbin=int((xmax-xmin)/dx)        !Calculating number of bins
    bin=0
    
    do j=1,n
        ibin=int((x(j)-xmin)/dx)+1
        
        if(ibin>nbin) ibin=nbin
        
        bin(ibin)=bin(ibin)+1
    end do
    
    !Calculating probability and probability density and writing them in output file
    do j=1,nbin
        prob(j)=real(bin(j))/real(n)
        probDensity(j)=prob(j)/dx
        
        if(j==1) then
            xmid=(2*xmin+dx)/2.0
            write(2,*) xmid, probDensity(j)
        else
            xmid=xmid+dx
            write(2,*) xmid, probDensity(j)
        end if
    end do
    
    ! Closing the input and output files
    close(1)
    close(2)
    
end program histogram
