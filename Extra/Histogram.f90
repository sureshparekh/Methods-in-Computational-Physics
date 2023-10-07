!Name: Joshi Sachinkumar J
!PRN: 2202100455
!Date: 1/8/2022
!Assinment1: Histogram
    

program histogram
    implicit none
    real:: x(100000), xmid, xmin, xmax, dx, prob(100000),probDensity(100000)
    integer:: j, n, nbin, bin(100000), ibin

    n = 100000
    xmin = -1.0
    xmax = 1.0
    dx = 0.1 !Delta x or width
    
    ! Calculating number of bins
    nbin=int((xmax-xmin)/dx)
    
    !Given data 
    open(1,file="assign01_input.dat")
    
    !To write the data
    open(2,file="output.txt")
    
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
    
    !Verifying the total sum of data in bins and total probability
    if(sum(bin)==n) write(*,*) "Total numbers verified"
    if(sum(prob)>0.999) write(*,*) "Normalization verified"
    
    ! Closing the input and output files
    close(1)
    close(2)
    
end program histogram