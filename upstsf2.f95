subroutine sedflux2 (rhos,ds,tsf,freq_count)!, boulder, cobble, pebble, granule, sand, silt, clay)
implicit none
real:: tsf, sf, rsf, rsv, rhos, sv, drho, sds,ds, boulder, cobble, pebble, granule, sand, silt, clay
real, parameter:: rhow= .998, g=9.8, vis=.001
real ::freq_count(7)
logical:: y
integer :: yn
call grainsz(rhos, ds) 
call graintype (freq_count,boulder, cobble, pebble, granule, sand, silt, clay)
drho=(rhos-rhow)*1000
print*, "drho", drho
sds=ds**2
print*, "sds", sds
sv=(((86400*g)/(18*vis))*drho*sds)!(- this is given in (kg/m^2-day) *86400 (in m/day  /24 (hr) - divide by 24 to get the hourly
sf=(rhos)*sv*1000
!Time increment respentative of 1 day to calculate segment volume need to make sure j=1 doens't impact the iteration 
print*, "sv", sv
print*,sf
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
rsf=rsv*rhos*1000*86400!*(((86400*g)/(18*vis))*drho*sds)
print*,rsf
Tsf=(sf-rsf)/86400 !*3600 to scale up to (kg/m^2-day)
print*, tsf
!Could used -vs1=sv-- but I would like more direct so changing over to vs1 in later equations
!May need to place conditional statement to describe when does this occur
print*, "tsf=", tsf
end subroutine sedflux2