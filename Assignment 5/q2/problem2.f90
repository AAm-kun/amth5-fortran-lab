program two
        implicit none
        integer::i
        real::j,tol1,tol2,tol3,xone,xtwo,xthree
        real,dimension(100)::x,y,z
        x(0)=0
        y(0)=0
        z(0)=0
        do i=1,100
            x(i)=xone(y(i-1),z(i-1))
            y(i)=xtwo(z(i-1),x(i-1))
            z(i)=xthree(x(i-1),y(i-1))
            tol1=(abs(x(i)-x(i-1))/abs(x(i)))
            tol2=(abs(y(i)-y(i-1))/abs(y(i)))
            tol2=(abs(z(i)-z(i-1))/abs(z(i)))
            write(*,*)i,x(i),y(i),z(i)
            if(tol1<.000001 .and. tol2<.000001 .and. tol3<.000001)then
                write(*,*)"Ans is",x(i),y(i),z(i)
                exit
            end if
        end do
end program

real function xone(y,z) result(ans)
    ans=(1+y-z)/3
end function

real function xtwo(z,x) result(ans)
    ans=(-3*x-2*z)/6
end function

real function xthree(x,y) result(ans)
    ans=(4-3*x-3*y)/7
end function
