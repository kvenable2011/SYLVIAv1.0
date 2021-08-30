subroutine passed2(state_var,i,m_1,maxlyrd,tsf,pdx,pdy,state_att,a,freq_count,taub,dprob) !(state_var, i, rhos, sv, maxlyrd, tsf, pdx,pdy, state_att,freq_count) !Don't think I need all the single vars 
implicit none 
real :: freq_count(7), state_att(6), state_var(6)
real :: m_1, tsf, maxlyrd, pdx, pdy, rhos, ds, a, dlayer,tb, dprob 
integer :: i,seg,j
real, intent(inout)::taub
!real, intent(inout)::dprob
j=0
pdx=0
pdy=0 
!tb=taub * 1
!call mec1(taub)
!call probofdep (dprob)
call location (seg)
call sedflux4 (rhos,ds,tsf,freq_count,dprob)
m_1 = tsf * a!-times 1 day in g/d*m^2 For coversion scaling conversion for tsf cm^2 to m^2. Has an assumption that 1 day is multiplied to get the concentration 
!Make call here for new subroutine for Van Rajin
state_var(1:6)= (/real(i), m_1, tsf, maxlyrd, pdx, pdy/)
write(10,*) state_var
print*, state_var
state_att(1:6)= (/a, ds,real(seg),rhos, taub, dprob/)
write(10,*) state_att
print*, state_att
!print*, freq_count
end subroutine passed2