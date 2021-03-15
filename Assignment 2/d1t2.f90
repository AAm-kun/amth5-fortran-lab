program Diatomicsequence
    integer ::n,i
    integer,allocatable,dimension(:) ::x
    read(*,*)n
    allocate(x(n))
    x(1)=1
    do i=0,n
        if ( i==0)then
            write(*,"(i10)",advance="no")i
            write(*,*)0
        else if (i==1) then
            write(*,"(i10)",advance="no")i
            write(*,*)x(1) 
        else
            if(mod(i,2)==0)then
                x(i)=x(i/2)
                write(*,"(i10)",advance="no")i
                write(*,*)x(i)
            else
            x(i)=x((i-1)/2)+x((i+1)/2)
            write(*,"(i10)",advance="no")i
            write(*,*)x(i)
            end if      
        end if     
    end do    
    
end program

