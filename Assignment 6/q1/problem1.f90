PROGRAM one
    IMPLICIT NONE
    INTEGER::i,j,k,ans=0
    LOGICAL::p,q,r

    !opening file
    OPEN(1,FILE='output.txt')

    WRITE(1,11)"p","q","r","p \/ (q /\ r)","(p /\ q) \/ (p /\ r)"

    !table generation

    DO i=0,1
        call con(p,i)
       DO j=0,1
          call con(q,j)
           DO k=0,1
            call con(r,k)
             WRITE(1,10)p,q,r,p.or.(q.and.r),(p.or.q).and.(p.or.r)
             if((p.or.(q.and.r)).eqv.((p.or.q).and.(p.or.r))) ans=1
            END DO
       END DO
    END DO

    !equivalancy check

    WRITE(1,'(/,A)',ADVANCE='NO')"The logical statements p \/ (q /\ r) and (p /\ q) \/ (p /\ r) are"
     if(ans==1)then
        write(1,*)"equivalent"
    else
        write(1,*)"not equivalent"
    end if


    !formating section
    10 FORMAT(L,2X,L,2X,L,2X,L4,6X,L6)
    11 FORMAT(A1,2X,A1,2X,A1,2X,A8,2X,A20)


END PROGRAM

SUBROUTINE con(d,c)
    integer::c
    logical::d
    if(c==0)then
            d=.true.
    else if(c==1)then
            d=.false.
    end if
END SUBROUTINE

