PROGRAM two
    IMPLICIT NONE
    INTEGER::i,j,ans=0
    LOGICAL::p,q,implication,bc1,bc2

OPEN(20,FILE='output.txt')
WRITE(20,'(/,A)')"Truth table for p->q and q<->p: "
WRITE(20,1)"p","q","p->q","q<->p"

    DO i=0,1
        call con(p,i)
       DO j=0,1
          call con(q,j)
           call bicon(q,p,bc1)
            WRITE(20,2)p,q,implication(p,q),bc1
       END DO
    END DO


WRITE(20,'(/,A)')"Truth table for p<->q and (p \/ q) \/ (~p \/ ~q): "
WRITE(20,3)"p","q","p<->q","(p /\ q) \/ (~p /\ ~q)"

    DO i=0,1
        call con(p,i)
       DO j=0,1
          call con(q,j)
           call bicon(p,q,bc2)
            WRITE(20,4)p,q,bc2,(p.and.q).or.(.not.p .and. .not.q)
            if(bc2.eqv.((p.and.q).or.(.not.p .and. .not.q)))ans=1


       END DO
    END DO

WRITE(20,'(/,A)',ADVANCE='NO')"The logical statements p<->q and (p /\ q) \/ (~p /\ ~q) are"
    if(ans==1)then
      write(20,*)" equivalent"
    else
      write(20,*)"not equivalent"
    end if


1 FORMAT(A1,2X,A1,2X,A4,2X,A5)
2 FORMAT(L,2X,L,2X,L2,4X,L3)
3 FORMAT(A1,2X,A1,2X,A5,2X,A24)
4 FORMAT(L,2X,L,2X,L3,4X,L7)
END PROGRAM


!biconditional function

SUBROUTINE bicon(a,b,c)
    LOGICAL::a,b,c,implication
    c=implication(a,b).and.implication(b,a)

END SUBROUTINE

!conversation function

SUBROUTINE con(d,c)
    integer::c
    logical::d
    if(c==0)then
            c=.false.
    else if(k==1)then
            d=.true.
    end if
END SUBROUTINE

!implication funciton

FUNCTION implication(a,b)
    LOGICAL::a,b,implication
    implication=.not.a.or.b

END FUNCTION


