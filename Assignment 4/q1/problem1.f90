program one
    integer::i=1,j
    real::a,b,x,tol
    real,dimension(100)::ans
    a=-2.0
    b=0.0
    do i=1,100
            x=(a+b)/2
            ans(i)=x
         tol=(abs(ans(i)-ans(i-1))/abs(ans(i)))
         write(*,*)i,a,b,ans(i),tol

             if((pr(a)*pr(x)).gt.0)then
                a=x
                else
                b=x
            end if
        if((tol<.0000001))exit

    end do
end program
real function pr(x) result(ans)
    real::x,b
    ans=(1.5*(x**3))-(7*x)-1-exp(x)
end function pr
