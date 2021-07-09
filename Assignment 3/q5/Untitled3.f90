
PROGRAM five
    IMPLICIT NONE

    INTEGER,PARAMETER::n=4
    INTEGER::i,j

    REAL::s
    REAL,DIMENSION(n,n+1)::a
    REAL,DIMENSION(n)::x

    OPEN(101,FILE='input.txt')
    OPEN(102,FILE='output.txt')

    READ(101,*)((a(i,j),j=1,n+1),i=1,n)

    WRITE(*,*)"Augmented Matrix"
    write(*,"(5(f10.1,2x))")((a(i,j),j=1,n+1),i=1,n)

    DO j=1,n

        DO i=j+1,n
            a(i,:)=a(i,:)-a(j,:)*a(i,j)/a(j,j)
        END DO
    END DO
    WRITE(*,*)"Pivoted Matrix"
    write(*,"(5(f10.1,2x))")((a(i,j),j=1,n+1),i=1,n)
    DO i=n,1,-1
        s=a(i,n+1)
        DO j=i+1,n
            s=s-a(i,j)*x(j)
        END DO
        x(i)=s/a(i,i)
    END DO

    WRITE(*,9)"X=",(x(i),i=1,n)
    9 FORMAT(a,/,4(f7.2,/))

END PROGRAM

