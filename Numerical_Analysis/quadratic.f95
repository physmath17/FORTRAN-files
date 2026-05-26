program quad

implicit real*8(a-h,o-z)

!real:: a, b, c, d, r1, r2
!complex:: c1, c2

print*, 'Enter the coefficients '
read*, a, b, c

if ((b**2 - 4*a*c) >= 0) then
print*, 'The roots are real and given by '
r1 = (-b + (b**2 - 4*a*c)**0.5)/(2*a)
r2 = (-b - (b**2 - 4*a*c)**0.5)/(2*a)
print*, r1, r2
else if ((b**2 - 4*a*c) < 0 ) then
print*, 'The roots are complex and given by '
d = 4*a*c - b**2
print*, -b/(2*a),' + i', (d**0.5)/(2*a)
print*, -b/(2*a),' - i', (d**0.5)/(2*a)
endif

end program quad

