PROGRAM four
    IMPLICIT NONE
    INTEGER::n,r,P,C,factorial

    OPEN(11,FILE='input.txt')
    OPEN(10,FILE='output.txt')

    READ(11,*)n,r

    WRITE(10,1)" P(",n,",",r,")=",P(n,r)
    WRITE(10,1)" C(",n,",",r,")=",C(n,r)

    1 FORMAT(A,I0,A,I0,A,I0)

END PROGRAM

RECURSIVE FUNCTION factorial(m) RESULT(ans)
INTEGER::m,ans

if(m==0)then
    ans=1
else
    ans=m*factorial(m-1)
end if

END FUNCTION

FUNCTION P(n,r)
    INTEGER::n,r,P,factorial

    P=factorial(n)/factorial(n-r)
END FUNCTION

FUNCTION C(n,r)
    INTEGER::n,r,C,factorial
    C=factorial(n)/(factorial(r)*factorial(n-r))
END FUNCTION
