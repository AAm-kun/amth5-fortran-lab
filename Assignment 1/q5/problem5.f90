program five
implicit none
integer::i
real,dimension(36)::x
!file opening
open(1,file="ot_a1q3_F.txt")
open(2,file="ot_a1q3_E.txt")
open(3,file="ot_a1q3_ES.txt")
open(4,file="ot_a1q3_sorted.txt")
!generation section
do i=1,36
    x(i)=110+rand(i)
end do

!writting into file

do i=1,6
write(1,10)x(i),x(i+1),x(i+2),x(i+3),x(i+4),x(i+5)
write(2,11)x(i),x(i+1),x(i+2),x(i+3),x(i+4),x(i+5)
write(3,12)x(i),x(i+1),x(i+2),x(i+3),x(i+4),x(i+5)
end do
10 format(6(f15.8))
11 format(6(e15.7))
12 format(6(es15.7))

!calling bubble sort subroutine

call sort(x)

end program

!bubble sort subroutine

subroutine sort(a)

real,dimension(36),intent(in)::a
real,dimension(36)::b
integer::j,k
real::temp

!putting values from dummy variable to local variable

b=a

!bubble sorting

do j=1,36
    do k=1,j-1
        if(b(k)>=b(k+1))then
        temp=b(k)
        b(k)=b(k+1)
        b(k+1)=temp
        end if
    end do
end do

!wrting to file
    write(4,10)(b(i),i=1,36)
    10 format(6(f15.8))
end subroutine

