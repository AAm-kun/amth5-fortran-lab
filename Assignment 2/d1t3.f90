program gcd 

    implicit none
    integer :: a,b,c
    integer ,external ::gsd
    read(*,*)a,b
    if(a<b)then
        c=a
        a=b
        b=c
    end if
    c=gsd(a,b)
    print*, c
    
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
