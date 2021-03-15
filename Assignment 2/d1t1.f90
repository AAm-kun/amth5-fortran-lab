program  quadratic
 
    implicit none
    integer :: a,b,c,dis
    read(*,*)a,b,c
    dis=(b**2)-(4*a*c)
    print*,a,b,c,dis
    if(dis==0)then
        write(*,*)"Roots are real and equal"
    else if(dis>0)then
        write(*,*)"Roots are real and different"
    else
        write(*,*)"Roots are complex"        
    end if

    
end program quadratic