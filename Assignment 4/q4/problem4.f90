program four
    implicit none
    integer::i
    real::tol,fun,a,b
    real,dimension(500)::x
    a=0.0
    b=1.0
    do i=1,500

        x(i)=(a*fun(b)-b*fun(a))/(fun(a)-fun(b))
        tol=abs(x(i)-x(i-1))/abs(x(i))
        write(*,*)i,a,x(i),b,tol,fun(x(i))
        if(fun(a)*fun(x(i+1))>0.0)then
            a=x(i)
            else
            b=x(i)

        end if
        if(tol<.000001)then
            write(*,*)"ANS IS",x(i)
            exit
        end if

    end do

end program
real function fun(a) result(ans)
    ans=230*a**4+18*a**3+9*a**2-221*a-9
end function
