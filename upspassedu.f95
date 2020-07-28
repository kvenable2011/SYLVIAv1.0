subroutine passed(state_var,i,m_1,maxlyrd,tsf,pdx,pdy,state_att,a,freq_count) !(state_var, i, rhos, sv, maxlyrd, tsf, pdx,pdy, state_att,freq_count) !Don't think I need all the single vars 
implicit none 
real :: freq_count(7), state_att(4), state_var(6)
real :: m_1, tsf, maxlyrd, pdx, pdy, rhos, ds, a, rsv, dlayer
integer :: i,seg,j
j=0
pdx=0
pdy=0 
call location (seg)
call sedflux(rhos,ds,tsf,freq_count)
m_1 = tsf * a!-times 1 day in g/d*m^2 For coversion scaling conversion for tsf cm^2 to m^2. Has an assumption that 1 day is multiplied to get the concentration 
print*, m_1 
state_var(1:6)= (/real(i), m_1, tsf, maxlyrd, pdx, pdy/)
print*, state_var
state_att(1:4)= (/a,ds,real(seg),rhos/)
print*, state_att
!print*, freq_count
end subroutine passed