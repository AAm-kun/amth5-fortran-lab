program three
    implicit none
    integer::i
    real::tol
    real,dimension(500)::x
    x(0)=-2.0
    do i=1,500
        x(i)=x(i-1)-(f(x(i-1))/fpr(x(i-1)))
        tol=((abs(x(i)-(x(i-1))))/abs(x(i)))
       ! write(*,*)tol
        if(tol<.00001)then
            write(*,*)"Ans is",x(i)
            exit
        end if
        write(*,*)i,x(i-1),fpr(x(i-1)),x(i),f(x(i))
    end do
end program
real function f(a) result(ans)
    ans=(16*a**4)+88*a**3+159*a**2+76*a-240
end function
real function fpr(b) result(ans)
    ans=64*b**3+264*b**2+318*b+76
end function
