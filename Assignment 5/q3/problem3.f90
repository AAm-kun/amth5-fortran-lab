program three
        implicit none
        integer::i
        real::j,tol1,tol2,tol3,xone,xtwo,xthree
        real,dimension(100)::x,y,z
        z(0)=0.0
        do i=1,100
            x(i)=xone(z(i-1))
            y(i)=xtwo(z(i-1),x(i))
            z(i)=xthree(x(i),y(i))
            tol1=(abs(x(i)-x(i-1))/abs(x(i)))
            tol2=(abs(y(i)-y(i-1))/abs(y(i)))
            tol2=(abs(z(i)-z(i-1))/abs(z(i)))
            write(*,*)i,x(i),y(i),z(i)
            if(tol1<.001 .and. tol2<.001 .and. tol3<.001)then
                write(*,*)"Ans is",x(i),y(i),z(i)
                exit
            end if
        end do
end program

real function xone(z) result(ans)
    ans=0.2+z
end function

real function xtwo(z,x) result(ans)
    ans=(.5*x+.25*z-1.425)
end function

real function xthree(x,y) result(ans)
    ans=2-x+.5*y
end function
