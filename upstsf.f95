subroutine sedflux(rhos,ds,tsf,freq_count)
implicit none
real:: tsf, sf, rsf, rsv, rhos, sv, ds
real:: boulder, cobble, pebble, granule, sand, silt, clay
real:: freq_count(7)
logical:: y
integer :: yn
!to incorporate for hourly depostion patterns scale tsf(g/m^2*s to g/m^2*day by 3600s
call grainsz(rhos,ds) 
call graintype (freq_count,boulder, cobble, pebble, granule, sand, silt, clay)
sf=sv*rhos*86400*1000 !-----> in kg/(m^2*day)
rsf=rsv*rhos*86400*1000
Tsf=(sf-rsf)/86400!-----> in kg/(m^2*s)
print*, "Would you like to enter a settling velocity (m/s), enter any number for yes/(1) for no?" 
            y= .true.
            read(5,*) yn
                 Do while(y)
                    if(yn==1) then 
                    y=.false. 
                    sv= 1.0
                    print*, "SV is set at 1.0" 
                    else  
                    print*, "Enter a settling velocity (m/s) "
                    read (5,*) sv
                        exit
                    end if 
                end do 
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
print*, "tsf=", tsf
end subroutine sedflux