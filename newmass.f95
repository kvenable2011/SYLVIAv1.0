subroutine newmass(nmass, nconc)
real, dimension (9,1), target ::timeout
integer:: j
real,pointer :: nmass, nconc
nmass=>timeout(8,j)
nconc=>timeout(9,j)
end subroutine newmass