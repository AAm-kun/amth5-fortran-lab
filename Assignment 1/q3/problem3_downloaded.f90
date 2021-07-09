PROGRAM three
    IMPLICIT NONE
    INTEGER::y
    CHARACTER(len=20)::x
    DO y=1,20
        write(x,20)"File_",y,".txt"
        20 FORMAT(a5,i0.2,a4)
        OPEN(y,FILE=x)
        WRITE(y,30)"This is file number ",y
        30 FORMAT(a20,i2.2)
    END DO
END PROGRAM three

