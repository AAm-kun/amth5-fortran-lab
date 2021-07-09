
program one
implicit none
!variable declaration
integer, dimension(:),allocatable :: s2
real, dimension(:),allocatable :: s3,s4
character, dimension(:),allocatable :: s1 !allocatable array
integer::p,n,q

!File Opening section
open(1,file='InputFile_1.txt')
open(2,file='InputFile_2.txt')
open(3,file='InputFile_3.txt')
open(4,file='InputFile_4.txt')
open(5,file='outputFile_5.txt')

! file 1 reading section

read(1,110,advance='no')p
write(5,110,advance='no')p

allocate(s1(p))

read(1,101)(s1(n),n=1,p)

write(5,101)(s1(n),n=1,p)

! file 2 reading section

read(2,110)p
write(5,110)p
allocate(s2(p))

read(2,102)(s2(n),n=1,p)

write(5,102)(s2(n),n=1,p)

! file 3 reading section

read(3,110)p
write(5,110)p

allocate(s3(p))
read(3,10)s3(2)
write(5,10)s3(2)
do n=2,p
read(3,103)s3(n)
write(5,103)s3(n)
end do

! file 4 reading section

read(4,110)p
write(5,110)p

allocate(s4(p))
read(4,104)(s4(n),n=1,p)

write(5,104)(s4(n),n=1,p)

!Formating section
110 format(i1)
101 FORMAT(1X,A1,X,A1,X,A1,X,A1,X,A1,X,A1)
102 FORMAT(I2,X,I1,X,I1,X,I2,X,I1,X,I2)
10 format(f4.1)
103 format(f3.1)
104 format(f4.1,1x,f3.1,1x,f3.1,/,f4.2,x,f3.1,/,f3.1)
end program
