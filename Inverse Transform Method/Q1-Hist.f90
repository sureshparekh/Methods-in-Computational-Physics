!Name: Suresh Parekh
!Inverse Tranform Method  - Histogram


program histogram
    implicit none
    real:: x(10000), xmid, xmin, xmax, dx, prob(10000),probDensity(10000)
    integer:: j, n, nbin, bin(10000), ibin

    n = 10000
    xmin = 0
    xmax = 1.0
    dx = 0.01 !Delta x or width
    
    nbin=int((xmax-xmin)/dx)    ! Calculating number of bins
    
    open(1,file="Q1.txt")   !Given data 
    open(2,file="Q1-Hist.txt")  !To write the data
    
    bin=0
    
    do j=1,n
        read(1,*) x(j)
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
