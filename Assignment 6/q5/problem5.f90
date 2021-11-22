PROGRAM five
    IMPLICIT NONE
    INTEGER::i,j,n,temp1,temp2,coun

    OPEN(10,FILE='output.txt')

WRITE(10,*)"The prime number n such that 1<n<100 and n+1,n+2,n+3 and n+4 are not prime numbers :"
    DO i=2,99
        temp1=1
      call  prime(i,temp1)
        IF(temp1==1)then

             coun=0
            DO j=i+1,i+4
                temp2=1
                call prime(j,temp2)
                 coun=coun+temp2
            END DO
               if(coun==0)then
                write(10,1)i
               end if

        end IF
    END DO
1 FORMAT(I3)

END PROGRAM

SUBROUTINE prime(i,m)
    IMPLICIT NONE
    INTEGER::j,i,m
    m=1

    DO j=2,NINT(SQRT(REAL(i)))
        IF(MOD(i,j)==0)THEN
            m=0
            EXIT
        ELSE
        END IF
    END DO

END SUBROUTINE prime
