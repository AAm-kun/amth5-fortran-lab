program seven
    real::s=0,h,a
    integer::i=0
    h=(2*exp(1.0)-exp(1.0))/30
    a=exp(1.0)
    do
        s=s+(f(a+i*h)+5*f(a+(i+1)*h)+f(a+(i+2)*h)+6*f(a+(i+3)*h)+f(a+(i+4)*h)+5*f(a+(i+5)*h)+f(a+(i+6)*h))
        i=i+6
        if(i==30)exit
    end do
    s=(3*h/10)*S
    write(*,*)s
end program
real function f(x) result(ans)
    ans=1/(x*log(x))
end function
