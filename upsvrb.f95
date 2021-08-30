subroutine passed3 (state_var,i,m_1,maxlyrd_b,tsf,pdx,pdy,state_att,a,freq_count,tb,dprob)!taub
implicit none 
real:: tsf, sf, rsf, rsv, rhos, sv, drho, sds,ds, boulder, cobble, pebble, granule, sand, silt, clay
real:: bf, wb, rhob, dtb, ddtb, PCTWA, lw, l , n, vol, rhod, m_1, a, maxlyrd_b, pdx, pdy, tbs, dprob, tb
real, parameter:: rhow= .998, g=9.8, vis=.001, db=0.010
real ::freq_count(7), state_var(6), state_att(6)
logical:: y
integer :: yn
integer :: i,seg,j
!real, intent(inout)::tb!taub
!real, intent(in)::dprob
print*,"Would you like to include an initial solids concentration in the surface benthic layer? &
Enter any number for Yes and (1) for No"
read (5,*) yn
y= .true. 
Do while(y)
    if(yn==1) then 
        y=.false. 
        rhos = 0
        ds = 0 
    else  
        y= .true. 
        exit
    end if 
end do           
j=0
pdx=0
pdy=0 
!tb = taub*1                
call location(seg) 
!common /shear/taub
call sedflux5(rhob,ds,tsf,freq_count,dprob)
m_1 = tsf * a!-times 1 day in g/d*m^2 For coversion scaling conversion for tsf cm^2 to m^2. Has an assumption that 1 day is multiplied to get the concentration 
print*, m_1 
state_var(1:6)=(/real(i), m_1, tsf, maxlyrd_b, pdx, pdy/)
write(10,*) state_var
print*, state_var                      
state_att(1:6)= (/a, ds,real(seg),rhob,tb,dprob/)  !taub
print*, state_att                      
end subroutine passed3