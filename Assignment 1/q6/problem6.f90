program six
implicit none
integer::x,y,z
write(*,*)"Input three positive real numbers"
read(*,*)x,y,z
if(x+y>z .or. y+z>x .or. z+x>y)then
    if( x==y .and. y==z .and. z==x )  then
    write(*,*)"An eqilateral triangel"
    else if(x==y .or. y==z .or. z==x) then
    write(*,*)"An isoscelese triangle"
    else if( y**2==(x**2+z**2) .or. z**2==(x**2+y**2) .or. x**2==(y**2+z**2) ) then
    write(*,*)"A right angle triangle"
    else
    write(*,*)"A Scelese triangle"
    end if
else
    write(*,*)"Not a triangle"
end if

end program
