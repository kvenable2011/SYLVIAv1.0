subroutine benthic (state_var,i,m_1,rhob, wb, bf, tsf, pdx, pdy,state_att, comid, PCTWA, freq_count, vol,dh,a)
implicit none 
real, dimension (7)::freq_count, fc
real, dimension (7)::state_att
real, dimension (9)::state_var_time_1, state_var_time_2, state_var_time_3, state_var
real, dimension (:,:), allocatable::benthic_layer_3 
real, dimension (:,:), allocatable:: sed_1, sed_2
real, dimension (:,:), allocatable ::timeout, timeout_1 
integer::comid, yn, so, seg,lnumb
logical::y
real :: om, sm, gs, lw, l, PCTWA, rhob, rhod, choice, sds, ds, dds, dv, dprob,dvf,d1, d2, d3, d4, d5, d6, d7, tmxc, tmx,dlayer
real :: sfrs, rsv, f, ks, md, grho, ks1, ks2, rd, vs2, y1,drho, vs1, sf, gp, x, ws,ch, taub1, um, taub, sv, ts, area, tsf, rsf, lc
real :: pdx, pdy, maxlyrd, dx, d8, d9, d10, d11, st, dy, feq_count, wb, bf, n,dtb, ddtb, m, ww, dh, vol 
real :: dm, boulder, cobble, pebble, granule, sand, silt, clay, a,m_1 
real, parameter:: rhow=.998, g=9.807, vis=.001, db=0.010
real, pointer:: x0
integer, parameter::ns=3
real, target:: rhos
integer :: i,j,irow, rtm, tinc
real::h, da, u !-n don't see where it is located

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
                                    go to 10 !- line 39 had go to statment, i just left as the call to seg. Maybe it will now allow for no bethic allocation
                                        exit
                                    end if 
                        end do   

10 call location(seg) 
call sedflux3 (rhos,ds,m,rsf,bf,tsf,rhob,wb,vol,PCTWA, freq_count, boulder, cobble, pebble, granule, sand, silt, clay)
pdx=0
pdy=0
!vol=m_1/rhob 
!dh=vol/a
m_1 = tsf * a!-times 1 day in g/d*m^2 For coversion scaling conversion for tsf cm^2 to m^2. Has an assumption that 1 day is multiplied to get the concentration 
print*, m_1 
vol=m_1/rhob
state_var(1:9)= (/real(i), m_1, wb, vol, tsf, pdx, pdy,m_1,m_1/)  
print*, state_var                      
state_att(1:7)= (/a, ds, real(seg), bf, vol, rhob, PCTWA/)  
print*, state_att                      
end subroutine benthic                        