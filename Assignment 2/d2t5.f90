program primedivisor
    implicit none
    integer::n,i
   
    read(*,*)n
   
    do i=2,(n/2)
        if(mod(n,i)==0)then
        call prime(i)
        end if    
    end do
end program primedivisor

subroutine prime (x)
    implicit none
    integer, intent(in)::x
    integer ::i,check=0
    do i=2,(x/2)
        if(mod(x,2)==0)then
            check=check+1
        end if    
    end do    
    if (check==0) then
       print*,x 
    end if     
end subroutine prime