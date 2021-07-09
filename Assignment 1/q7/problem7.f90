program seven
implicit none
integer::x,y,i,z=0,coun=0

write(*,*)"Input two digit number"
read(*,*)x

!calculating number of digits

10 y=int(log10(real(x))+1)

!square sum of the digit

do i=1,y
    z=z+mod(x,10)**2
    x=x/10
end do

!happy checking

if(z==1) then
write(*,*)"Happy number"
else
x=z
z=0
coun=coun+1

!checking iteration

if(coun<100)then
    goto 10
    else
    write(*,*)"Not a Happy Number"
    end if

end if

end program


