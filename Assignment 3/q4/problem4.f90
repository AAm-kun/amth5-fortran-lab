program four
    implicit none
    integer,dimension(3,3)::mat
    real, dimension(3,3)::env
    integer::n,i,j
    open(10,file="input.txt")
    read(10,*)((mat(i,j),i=1,3),j=1,3)
    call inv(mat,env)
    write(*,"(/,a10)")"inverse"
    write(*,"(3(f10.1,2x))",advance="no")((env(i,j),i=1,3),j=1,3)
end program

subroutine inv(mat,env)
    integer,dimension(3,3)::mat
    real, dimension(3,3)::env
    integer::i,det,j
    logical::p=.true.
    do i=1,3
        if(mat(i,i)==0)p=.false.
    end do
   ! write(*,"(3(i0,2x))",advance="no")((mat(i,j),i=1,3),j=1,3)
    if(p .neqv. .false.)then
        det=mat(1,1)*mat(2,2)*mat(3,3) &
            -mat(1,1)*mat(2,3)*mat(3,2) &
            -mat(2,1)*mat(1,2)*mat(3,3) &
            +mat(2,1)*mat(3,2)*mat(1,1) &
            +mat(3,1)*mat(1,2)*mat(2,3) &
            +mat(3,1)*mat(2,2)*mat(1,1)
        env(1,1)=mat(2,2)*mat(3,3)-mat(3,2)*mat(2,2)
        env(1,2)=-(mat(2,1)*mat(3,3)-mat(3,1)*mat(2,3))
        env(1,3)=mat(2,1)*mat(3,2)-mat(3,1)*mat(2,2)
        env(2,1)=-(mat(1,2)*mat(3,3)-mat(3,2)*mat(1,1))
        env(2,2)=mat(1,1)*mat(3,3)-mat(3,1)*mat(1,3)
        env(2,3)=-(mat(1,1)*mat(3,2)-mat(3,1)*mat(1,2))
        env(3,1)=mat(1,2)*mat(2,3)-mat(2,2)*mat(1,3)
        env(3,2)=-(mat(1,1)*mat(2,3)-mat(2,1)*mat(1,3))
        env(3,3)=-(mat(1,1)*mat(2,2)-mat(2,1)*mat(1,2))
        write(*,"(/,a10)")"transpose"
        write(*,"(3(f10.1,2x))",advance="no")((env(i,j),i=1,3),j=1,3)

        env=transpose(env)/det

    end if
end subroutine
