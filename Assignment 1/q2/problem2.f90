program two
implicit none

!variable declaration
character(len=20)::x
integer::y,i

open(1,file="name.txt")
read(1,10)x
y=len(trim(x))

!without space

do i=1,y
if (x(i:i) == " ") then
cycle
else
write(*,11,advance="no")x(i:i)
end if
end do

write(*,*)

!without vowels

do i=1,y
if (x(i:i) == "a" .or. x(i:i) == "e" .or. x(i:i) == "i" .or. x(i:i) == "o" .or. x(i:i) == "u" ) then
cycle
else if (x(i:i) == "A" .or. x(i:i) == "E" .or. x(i:i) == "I" .or. x(i:i) == "O" .or. x(i:i) == "U" ) then
cycle
else
write(*,11,advance="no")x(i:i)
end if
end do

write(*,*)

!in capital letters

do while (y>0)
if (x(i:i) .ne. " ") then
cycle
else
write(*,11,advance="no")x(y:y)
end if
y=y-1
end do


10 format(a20)
11 format(a)
end program

