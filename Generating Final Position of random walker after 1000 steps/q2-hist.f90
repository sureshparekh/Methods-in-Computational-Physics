!Name:Suresh Parekh
!Histogram of Random Walk

program Histogram

    implicit none 
    real:: x(1000000), xmid, xmin, xmax, dx, prob(1000000),probDensity(1000000)
    integer:: j, n, nbin, bin(1000000), ibin,s

    open(3,file="q2.txt")       !Given data 
    open(4,file="Q2-Hist.txt")      !To write the data

    n = 1000000
    do j=1,n
        read(3,*)s, x(j)
    enddo
    xmin = minval(x)
    xmax = maxval(x)
    write(*,*)'Enter Bin Size'      !Take Bin Size 0.1 & 2 
    read(*,*)dx    !Delta x or width or bin
    
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
            write(4,*) xmid, probDensity(j)
        else
            xmid=xmid+dx
            write(4,*) xmid, probDensity(j)
        end if
    end do
    
    
    ! Closing the input and output files
    close(3)
    close(4)
    
end program Histogram
