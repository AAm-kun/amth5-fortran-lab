program euiler  
    implicit none
    integer ::n,i,count=0
    integer, external::gsd
    read(*,*)n
    do i=1,n
        if(gsd(n,i)==1)then
            count=count+1
        end if
    end do
    print*,count
end program

recursive integer function gsd(x,y) result(ans)
    implicit none
    integer,intent(in):: x,y
    
    if(mod(x,y)==0)then
        ans=y
    else 
        
       ans=gsd(y,mod(x,y))
    end if
    return
end function gsd