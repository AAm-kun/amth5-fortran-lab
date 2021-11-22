PROGRAM three
    IMPLICIT NONE
    INTEGER::i,j,an1=1,an2=1
    LOGICAL::p,q,imlplicaion,con1,con2,con3,con4


OPEN(20,FILE='output.txt')

!truth table

WRITE(20,'(/,A)')"Truth table for p<->q and ~p<->~q : "
WRITE(20,1)"p","q","p<->q","~p<->~q"

    DO i=0,1
        call cov(p,i)
       DO j=0,1
          call cov(q,j)
           call bicon(p,q,con1)
            call bicon(.not.p,.not.q,con2)
            WRITE(20,2)p,q,con1,con2
            if(con1.neqv.con2)then
               an1=0
            end if
       END DO
    END DO

!equivalance check

WRITE(20,'(/,A)',ADVANCE='NO')"The logical statements p<->q and ~p<->~q are "
    if(an1==0)then
      write(20,*)"not equivalent"
    else if(an1==1)then
      write(20,*)"equivalent"
    end if

!truth table

WRITE(20,'(//,A)')"Truth table for ~(p<->q) and p<->~q: "
WRITE(20,3)"p","q","~(p<->q)","p<->~q"

    DO i=0,1
        call cov(p,i)
       DO j=0,1
          call cov(q,j)
           call bicon(p,q,con2)
                            con3=.not.con2
             call bicon(p,.not.q,con4)
            WRITE(20,4)p,q,con3,con4
            if(con3.neqv.con4)then
               an2=0
            end if
       END DO
    END DO

!equivalance check

WRITE(20,'(/,A)',ADVANCE='NO')"The logical statements ~(p<->q) and p<->~q are "
    if(an2==0)then
      write(20,*)"not equivalent"
    else if(an2==1)then
      write(20,*)"equivalent"
    end if

!formatting section

1 FORMAT(A1,2X,A1,2X,A5,2X,A7)
2 FORMAT(L,2X,L,2X,L3,4X,L4)
3 FORMAT(A1,2X,A1,2X,A8,2X,A6)
4 FORMAT(L,2X,L,2X,L5,5X,L3)

END PROGRAM

!implication fucntion

FUNCTION imlplicaion(a,b)
    LOGICAL::a,b,imlplicaion
    imlplicaion=.not.a.or.b

END FUNCTION

!biconditional section

SUBROUTINE bicon(a,b,c)
    LOGICAL::a,b,c,imlplicaion
    c=imlplicaion(a,b).and.imlplicaion(b,a)
END SUBROUTINE

!conversation subroutine

SUBROUTINE cov(c,d)
    integer::d
    logical::c
    if(d==0)then
            c=.FALSE.
    else if(d==1)then
            c=.TRUE.
    end if
END SUBROUTINE



