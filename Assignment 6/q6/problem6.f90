PROGRAM six
    IMPLICIT NONE
    INTEGER,ALLOCATABLE,DIMENSION(:,:)::A,B
    INTEGER::i,j,n,r

    OPEN(10,FILE='input.txt')
    open(11,file="output.txt")
    READ(10,*)n
    ALLOCATE(A(n,n),B(n,n))
    READ(10,*)((A(i,j),j=1,n),i=1,n)

    WRITE(11,'("  Adjacency matrix, A : ",/)')
    DO i=1,n
        DO j=1,n
            WRITE(11,'(I5,2X)',ADVANCE='NO')A(i,j)
         END DO
            WRITE(11,'(/)')
    END DO

    WRITE(11,*)"Enter the ith and jth vertex and path length r  :"
    READ(*,*)i,j,r

    CALL powma(A,B,n,r)


    WRITE(11,'(/,A,I4,A,I0)')"There are ",B(i,j)," length of Paths ",r
    WRITE(11,'(A,I0,A,I0)')"Between the vertices u",i," and u",j

    WRITE(11,'(//,A,I0,A,/)')"A^",r,":"
    DO i=1,n
        DO j=1,n
            WRITE(11,'(I5,2X)',ADVANCE='NO')B(i,j)
         END DO
            WRITE(11,'(/)')
    END DO

END PROGRAM

SUBROUTINE powma(A,B,n,r)
    IMPLICIT NONE
    INTEGER::i,n,r
    INTEGER::A(n,n),B(n,n),C(n,n)
   C=A
   DO i=1,r-1
    B=matmul(C,A)
    C=B
   END DO

END SUBROUTINE
