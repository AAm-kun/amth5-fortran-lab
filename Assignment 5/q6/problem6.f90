program six
    integer::i
    real::s,h,sim
    h=(atan(1.0))/36
    do i=1,35
        if(mod(i,2)==0)then
            s=s+2*f(i*h)
            else
            s=s+3*f(i*h)
        end if
    end do
    sim=(f(0)+f(atan(1.0))+s)*((3*h)/8)
    write(*,*)sim
end program
real function f(x) result(ans)
    ans=exp(3*x)*sin(2*x)
end function
