program two
    real::tol
    real,dimension(500)::x
    x(1)=.99
    do i=1,500
        x(i+1)=g(x(i))
        tol=(abs(x(i)-x(i-1)))
         if(tol<.0001)exit
         write(*,*)i,x(i),x(i+1),g(x(i))
    end do
end program
real FUNCTION g(x) result(ans)
    ans=2**(-x)-x**3+0.5*x**2
END FUNCTION
