program two
    integer,dimension(4,4)::x
    character::p
    read(*,*)p
    if(p=="a")then
        call quesa(x)
    else if(p=="b")then
        call quesb(x)
    else if(p=="c")then
        call quesc(x)

    end if
end program

subroutine quesa(a)
    integer,dimension(4,4)::a
    integer::i,j,k
    do i=1,4
        do j=1,4
            a(i,j)=i+j
        end do
    end do
    write(*,"(4(i0,2x))")((a(i,j),i=1,4),j=1,4)
end subroutine

subroutine quesb(a)
    integer,dimension(4,4)::a
    integer::i,j,k
    do i=1,4
        do j=1,4
            a(i,j)=i**(j-1)
        end do
    end do
    write(*,"(4(i0,2x))")((a(i,j),i=1,4),j=1,4)
end subroutine
subroutine quesc(a)
    integer,dimension(4,4)::a
    integer::i,j,k
    do i=1,4
        do j=1,4
            if((i-j).gt.1)then
            a(i,j)=1
            else
             a(i,j)=-1
             end if
        end do
    end do
    write(*,"(4(i0,2x))")((a(i,j),i=1,4),j=1,4)
end subroutine
