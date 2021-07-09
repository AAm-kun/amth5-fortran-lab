program three
    implicit none

    !passing dynamic array in subroutine

    interface
    subroutine mul(n,a,b,c)
    integer,dimension(:,:),allocatable::a,b,c
    integer::n,i,j,l
    end subroutine
    end interface

    !end of passing dynamic array in subroutine

    integer,dimension(:,:),allocatable::m,n,p,q
    character(len=20)::str
    integer::x,i,j,tmp=0
    read(*,*)x
    allocate(m(x,x),n(x,x),p(x,x),q(x,x))

    !formatting section

    write(str,"(a,i0,a7,a)")"(",x,"(i0,2x)",")"
    read(*,*)((m(i,j),i=1,x),j=1,x),((n(i,j),i=1,x),j=1,x)
    !subroutine calling
    call mul(x,m,n,p)
    call mul(x,n,m,q)
    !matrix checking section

    do i=1,x
        do j=1,x
            if(p(i,j).ne.q(i,j))tmp=tmp+1
        end do
    end do
    if(tmp.ne.0)then
        write(*,*)"Doesn't commute"
        else
        write(*,*)"Commutes"
    end if
    write(*,"(//,a20)")"Matrix M"
    write(*,str,advance="no")((m(i,j),i=1,x),j=1,x)
    write(*,"(//,a20)")"Matrix N"
    write(*,str,advance="no")((n(i,j),i=1,x),j=1,x)
    write(*,"(//,a20)")"Matrix MN"
    write(*,str,advance="no")((p(i,j),i=1,x),j=1,x)
    write(*,"(//,a20)")"Matrix NM"
    write(*,str,advance="no")((q(i,j),i=1,x),j=1,x)
end program

subroutine mul(n,a,b,c)

    integer,dimension(:,:),allocatable::a,b,c
    integer::n,i,j,l

    do i=1,n
        do j=1,n
            c(i,j)=0
            do l=1,n
             C(i,j)=C(i,j)+ a(i,l)*b(l,j)
            end do
        end do
    end do

end subroutine
