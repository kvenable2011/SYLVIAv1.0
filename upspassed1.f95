subroutine passed1(state_var,i,m_1,maxlyrd,tsf,pdx,pdy,state_att,a)!freq_count (state_var, i, rhos, sv, maxlyrd, tsf, pdx,pdy, &state_att,freq_count) !Don't think I need all the single vars 
implicit none 
real :: state_att(4), state_var(6)!freq_count(7), 
real :: m_1, tsf, maxlyrd, pdx, pdy, rhos, ds, a, dlayer
integer :: i,seg,j
j=0
pdx=0
pdy=0 
call location (seg)
call sedflux2 (rhos,ds,tsf)!,freq_count
m_1 = tsf * a!-times 1 day in g/d*m^2 For coversion scaling conversion for tsf cm^2 to m^2. Has an assumption that 1 day is multiplied to get the concentration 
state_var(1:6)= (/real(i), m_1, tsf, maxlyrd, pdx, pdy/)
write(10,*) state_var
print*, state_var
state_att(1:4)= (/a, ds,real(seg),rhos/)
write(10,*) state_att
print*, state_att
!print*, freq_count
end subroutine passed1