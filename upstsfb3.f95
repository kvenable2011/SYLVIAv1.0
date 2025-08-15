subroutine benthic2 (state_var,i,m_1,maxlyrd_b,tsf,pdx,pdy,state_att,a,freq_count,taub,dprob)
real:: tsf, sf, rsf, rsv, rhos, sv, drho, sds,ds, boulder, cobble, pebble, granule, sand, silt, clay
real:: bf, wb, rhob, dtb, ddtb, PCTWA, lw, l , n, vol, rhod
real, parameter:: rhow= .998, g=9.8, vis=.001, db=0.010
real ::freq_count(7)
real,allocatable :: state_var(:)
logical:: y
integer :: yn
call grainsz(rhos, ds) 
call graintype (freq_count,boulder, cobble, pebble, granule, sand, silt, clay)
print*, "Enter liquid water and liquid content of sediment in the reach segment (lw,l)" 
read (5, *) lw,l
n=lw/l
rhod=rhos*(1-n)
print*,rhod
rhob=rhow*n+rhod
print*, rhob
PCTWA=100*(rhob/rhod)
print*, PCTWA
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
wb=(db/dtb)*3600!burial velocity given in m/sec default dtb is 1 day used 3600 to scale to an hour but probably should scale again only by 3600
bf=rhob*wb
print*, "bf=", bf
Tsf=(bf-(rsf*3600))!*10**4!*3600 --to account for benthic burial at the indicated rate and possible ressuspension 
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
end subroutine benthic2