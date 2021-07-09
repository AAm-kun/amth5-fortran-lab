program three
implicit none
integer::i
character(len=20)::x
do i=1,20
if(i<10)then
open(unit=i,file="File_"//char(48)//char(i+48)//".txt")
else if (i<20) then
open(unit=i,file="File_"//char(49)//char(38+i)//".txt")
else
open(unit=i,file="File_"//char(50)//char(28+i)//".txt")
end if
write(i,'(a20,i0.2)')"this is file number ",i

end do


write(*,*)char(48)

end program
