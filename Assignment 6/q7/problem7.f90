PROGRAM seven
    IMPLICIT NONE
    INTEGER::x,fact
    open(10,file="input.txt")
    open(11,file="output.txt")
    READ(10,*)x

    WRITE(11,*)"The number of circuits required to be considered :",fact(x-1)/2
END PROGRAM

RECURSIVE FUNCTION fact(y) RESULT(ans)
IMPLICIT NONE
INTEGER::y,ans

  IF(y==0)THEN
    ans=1
  ELSE
    ans=y*fact(y-1)
  END IF

END FUNCTION
