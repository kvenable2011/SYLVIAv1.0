subroutine sedflux2 (rhos,ds,sv,tsf,freq_count)
implicit none
real:: tsf, sf, rsf, rsv, rhos, sv, drho, sds,ds, m, boulder, cobble, pebble, granule, sand, silt, clay, load_i, a, xks
real:: h, xterm, xt, dt, lpt, svol, q, conc_a, conc_ss, conc_i, advf_x, num_par_tot, d_conc_i, m_1, dx_1, d_m,u, tmx
real, parameter:: rhow= .998, g=9.8, vis=.001
real, dimension (7)::state_var, freq_count
real, dimension (5) :: int_con
logical:: y
integer :: yn, j, comid
call grainsz(rhos, ds, m) 
call graintype (freq_count,boulder, cobble, pebble, granule, sand, silt, clay)
drho=rhos-rhow
print*, "drho", drho
sds=ds**2
print*, "sds", sds
sv=(((86400*g)/(18*vis))*drho*sds)!(- this is given in (g/m^2-day) *86400 (in m/day  /24 (hr) - divide by 24 to get the hourly
sf=(rhos)*sv
!Time increment respentative of 1 day to calculate segment volume need to make sure j=1 doens't impact the iteration 
print*, "sv", sv
print*,sf
rsf=rsv*rhos
Tsf=(sf-rsf)*10**6 !*3600 to scale up to (g/m^2-day) -->Removed the 10^4 scaling conversion for tsf cm^2 to m^2
!Could used -vs1=sv-- but I would like more direct so changing over to vs1 in later equations
print*, "Would you like to enter a resuspension velocity, enter any number for yes/(1) for no?" 
            y= .true.
            read (5,*) yn
                 Do while(y)
                    if(yn==1) then 
                    y=.false. 
                    rsv= 0.0
                    print*, "RSV is set at 0.0" 
                    else  
                    print*, "Enter a resuspension velocity (m/s) "
                    read (5,*) rsv
                        exit
                    end if 
                end do 
!May need to place conditional statement to describe when does this occur
print*, "tsf=", tsf
!call area (a) 
!m_1 = tsf * a!-times 1 day in g/d*m^2 For coversion scaling conversion for tsf cm^2 to m^2. Has an assumption that 1 day is multiplied to get the concentration 
!print*, m_1 
!d_m = -(m_1/dx_1)
!d_conc_i = -(tsf/(dx_1*10**6)) !Change in transport concentration over time through flux over time 
!print*, d_conc_i
!conc_i = (tsf/(dx_1*10**6)) !converted to g/ml 
!print*, conc_i 
!call asettle (load_i, conc_i, conc_ss, conc_a, advf_x, num_par_tot)
end subroutine sedflux2