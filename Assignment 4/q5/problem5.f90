program five
    IMPLICIT NONE
    INTEGER::i,j,n,fact
    DOUBLE PRECISION::P,S,D,t,h
    DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:,:)::f(:,:)
    DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:)::x(:)

    OPEN(47,FILE="in4q5.txt")

    READ(47,*)n
    ALLOCATE(x(0:n),f(0:n,0:n))
    READ(47,*)(x(i),i=0,n)
    READ(47,*)(f(i,0),i=0,n)
   !  write(*,*)(f(0,i),i=0,n)
    do i=0,n
        do j=0,i
            f(i+1,j+1)=f(i+1,j)-f(i,j)
             WRITE(*,'(F15.5,2X)',ADVANCE='NO')f(i,j)
        end do
        WRITE(*,*)
    end do
    !interpolotion
h=20
p=(t-x(0))/h
do i=1,n-1
    s=s+
end do
end program
integer function fact(x) result(ans)
    integer::i,x
    ans=1
    if(x==0)then
        ans=1
        else
            do i=1,x
                ans=ans*i
            end do
    end if
end function
