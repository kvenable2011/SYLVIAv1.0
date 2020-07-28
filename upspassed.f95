subroutine passed(state_var,state_att,freq_count)!state_var, i, rhos, tsf, maxlyrd, dx, pdx, pdy) 
real ::state_var(6), freq_count(7), state_att(4)
real :: ds, rhos
real :: pdx, pdy, maxlyrd, tsf, rsf
integer :: i,j,seg
                j=0
                !maxlyrd=dlayer* real(i)
                call grainsz (rhos,ds)
                call location(seg)
                call sedflux (tsf,rsf)
                !pdx=dx +(u*real(j))
                !pdy=dy+((tsf/rhos)*real(j))
                !vol=m/rhos
                freq_count(:)=0
                !allocate(state_var(7,i))
                state_var(1:6)= (/m_1,real(i),tsf, maxlyrd, pdx, pdy/)
                print*, state_var
                state_att(1:4)= (/a,real(seg),ds,rhos/) 
                print*, state_att
                !deallocate (state_var)
end subroutine passed                 