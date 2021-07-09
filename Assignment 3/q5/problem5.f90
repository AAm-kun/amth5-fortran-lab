PROGRAM five
    IMPLICIT NONE

    INTEGER,PARAMETER::n=4
    INTEGER::i,j

    REAL::s
    REAL,DIMENSION(n,n+1)::a
    REAL,DIMENSION(n)::x

    OPEN(101,FILE='input.txt')


    READ(101,*)((a(i,j),j=1,n+1),i=1,n)

    WRITE(*,*)"Augmented Matrix"
    write(*,"(5(f10.1,2x))")((a(i,j),j=1,n+1),i=1,n)
    do i=1,n
        do j=i+1,n
            a(j,:)=a(j,:)-a(i,:)*a(j,i)/a(i,i)
        end do
    end do
     WRITE(*,*)"Pivoted Matrix"
    write(*,"(5(f10.1,2x))")((a(i,j),j=1,n+1),i=1,n)
    do i=4,1,-1
        s=a(i,n+1)
        do j=i+1,n
            s=s-a(i,j)*x(j)
        end do
        x(i)=s/a(i,i)

    end do

    WRITE(*,9)"X=",(x(i),i=1,n)
    9 FORMAT(a,/,4(f7.2,/))
END PROGRAM

