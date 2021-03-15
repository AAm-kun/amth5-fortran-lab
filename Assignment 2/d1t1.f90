program  quadratic
 
    implicit none
    integer :: a,b,c,dis
    read(*,*)a,b,c
    dis=(b**2)-(4*a*c)
    print*,a,b,c,dis
    open(unit=10,file="result.txt")
    if(dis==0)then
        write(10,*)"Roots are real and equal"
    else if(dis>0)then
        write(10,*)"Roots are real and different"
    else
        write(10,*)"Roots are complex"        
    end if

    
end program quadratic