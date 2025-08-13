subroutine sedflux3 (rhos,ds,m,rsf,bf,tsf,rhob,wb,vol,PCTWA,freq_count, boulder, cobble, pebble, granule, sand, silt, clay)
implicit none
real:: tsf, sf, rsf, rsv, rhos, sv, drho, sds,ds, m, boulder, cobble, pebble, granule, sand, silt, clay
real:: bf, wb, rhob, dtb, ddtb, PCTWA, lw, l , n, vol, rhod
real, parameter:: rhow= .998, g=9.8, vis=.001, db=0.010
real, dimension (7)::state_var, freq_count
logical:: y
integer :: yn
call grainsz(rhos, ds, m) 
call graintype (freq_count,boulder, cobble, pebble, granule, sand, silt, clay)
drho=rhos-rhow
print*, "drho", drho
sds=ds**2
print*, "sds", sds
sv=((86400*g)/(18*vis))*drho*sds !(*86400 (s/day  /24 (hr) - divide by 24 to get the hourly
sf=(rhos)*sv
print*, "sv", sv 
print*,sf
rsf=rsv*rhos
print*, "Enter liquid water and liquid content of sediment in the reach segment (lw,l)" 
read (5, *) lw,l
n=lw/l
rhod=rhos*(1-n)
print*,rhod
rhob=rhow*n+rhod
print*, rhob
PCTWA=100*(rhob/rhod)
print*, PCTWA
!vol=m/rhob
print*, "Would you like to include change the benthic time step(dtb)? Enter number for yes and 1 for no. (Default is 1 hour)"
                    read (5,*) yn
                    y= .true. 
                        Do while(y)
                                    if(yn==1) then 
                                    y=.false. 
                                    dtb=1
                                    print*, "Your benthic time step is 1 hour"
                                    else  
                                        y= .true. 
                                    print*, "Enter your benthic time step"
                                    read(5,*) ddtb
                                    dtb=ddtb
                                        exit
                                    end if 
                         end do            
wb=db/dtb !burial velocity given in m/sec default dtb is 1 day
bf=rhob*wb
print*, "bf=", bf
Tsf=(bf-rsf)*(10**6)!*3600 --to account for benthic burial at the indicated rate and possible ressuspension 
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
print*, "tsf=", tsf
end subroutine sedflux3