program  quadratic
 
    implicit none
    integer :: a,b,c
    real ::dis,res1,res2
    complex :: con
    read(*,*)a,b,c
    dis=(b**2)-(4*a*c)
    
    open(unit=10,file="result.txt")
    if(dis==0)then
        write(10,*)"Roots are real and equal"
        res1=(-b+sqrt(dis))/(2*a)
        write(10,"(f10.4)")res1
    else if(dis>0)then
        write(10,*)"Roots are real and different"
        res1=(-b+sqrt(dis))/(2*a)
        res2=(-b-sqrt(dis))/(2*a)
        write(10,"(f10.4,f10.4)")res1,res2
    else
        write(10,*)"Roots are complex"  
        con=cmplx((-b/2*a),(sqrt((-1*dis))/2*a))
        write(10,*)con,conjg(con)      
    end if
    

end program quadratic