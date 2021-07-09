program one
    implicit none
    integer::n,i,j,k=0
    character(len=20)::string
    integer,dimension(:,:),allocatable::mat
    read(*,*)n
    allocate(mat(n,n))
    read(*,*)((mat(i,j),i=1,n),j=1,n)
    !formating with variable
    write(string,'(a,i0,a7,a)')"(",n,"(i0,2x)",")"

    do i=1,n
        do j=1,n
            if(mat(i,j).ne.mat(j,i))k=k+1
        end do
    end do
    if(k==0)then
        write(*,*)"Symmetric"
        else
            write(*,*)"Not symmetric"
    end if

    write(*,string)((mat(i,j),i=1,n),j=1,n)


end program

