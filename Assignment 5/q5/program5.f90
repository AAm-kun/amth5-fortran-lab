program five
    implicit none
    integer::i
    real::x,y,h,s=0,f,trap,sim,g
    h=7.5/30
    !trapozoidal
    do i=1,29
        s=s+f(h*i)
    end do
    Write(*,"(3(a15))")"Method","Value","Error"
    trap=(f(0.0)+f(7.5)+2*s)*(h/2)
     write(*,*)"Trapezoidal method",trap,(abs(g(7.5)-g(0)-trap))
    !simpson 1/3
    s=0
    h=7.5/18
    do i=1,17
        if(mod(i,2)==0)then
            s=s+2*(f(i*h))
        else
            s=s+4*(f(i*h))
        end if
    end do
    sim=(f(0.0)+f(7.5)+s)*(h/3)
    write(*,*)"Simpson's 1/3 Method",sim,(abs(g(7.5)-g(0)-sim))

end program

real function f(x) result(ans)
    ans=1.5*x**3-7*x-1-exp(x)
end function
FUNCTION G(x)
    G=0.375*x**4-3.5*x**2-x-exp(x)
END FUNCTION
