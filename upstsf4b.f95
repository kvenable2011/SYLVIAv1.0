subroutine sedflux5 (rhob,ds,tsf,dprob)!freq_count)!, boulder, cobble, pebble, granule, sand, silt, clay)
implicit none
real:: tsf, sf, rsf, rsv, rhos, sv, drho, sds,ds, boulder, cobble, pebble, granule, sand, silt, clay
real :: ks1, ks2, da, h, ks, f, u
real:: gp, x, rd, rhob_kg, rd_1, rd_2, sq
real, parameter:: rhow_kg= 998, g=9.807, vis=.001, rhow =.998, db=0.010
real:: bf, wb, rhob, dtb, ddtb, PCTWA, lw, l , n, vol, rhod,tb,dprob !taub
!real, allocatable :: freq_count(:)
!real ::freq_count(7)
!real,allocatable :: state_var(:)
logical:: y
integer :: yn
real ::taucd1, taucd2, pd, t2, t1
!real, intent(inout):: taub
call grainsz(rhos, ds)
!call graintype (freq_count,boulder, cobble, pebble, granule, sand, silt, clay)
!tb = taub*1
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
!wb=(db/dtb)*3600!burial velocity given in m/sec default dtb is 1 day used 3600 to scale to an hour but probably should scale again only by 3600
!wb=(sv/dtb)*3600!burial velocity given in m/sec default dtb is 1 day used 3600 to scale to an hour but probably should scale again only by 3600
!bf=rhob*wb
!print*, "bf=", bf
!Tsf=(bf-(rsf*3600))!*10**4!*3600 --to account for benthic burial at the indicated rate and possible ressuspension 
!Could used -vs1=sv-- but I would like more direct so changing over to vs1 in later equations
rhob_kg= rhob*1000
gp=g*((rhob_kg/rhow_kg)-1)
rd_2 =vis/rhow_kg
sq=gp*ds
rd_1 = ds*sqrt(sq)
!rd_1 =ds*((gp*ds)**0.5)
rd= rd_1/rd_2!(What units is this in does it need to be converted)
!rd=(ds*(gp*ds)**0.5)/(vis/rhow_kg)
print*, 'gp=' , gp
print*, 'rd_1 & 2=', rd_2,rd_1
print*, 'rd=' , rd
print*, 'ds=', ds
    if (ds<=.0001) then 
        sv=(rd/18)*(sqrt(gp*ds))
    else if (0.0001<ds .and. ds<=0.001) then 
        x=1+(0.01*(rd**2))
        sv=(10/rd)*(sqrt(x)-1)*sqrt(gp*ds)
    else 
        sv=1.1*(sqrt(gp*ds))
    end if 
print*, "sv", sv 
wb=(sv/dtb)*3600!burial velocity given in m/sec default dtb is 1 day used 3600 to scale to an hour but probably should scale again only by 3600
!bf=rhob*wb
!print*, "bf=", bf
!Tsf=(bf-(rsf*3600))
!ks1 = 3*da
!ks2 = 0.01*h
!ks = max(ks1,ks2)
!print*, ks1, ks2, 
!ks = max(ks1,ks2) 
!print *," ks=", ks
!taub=(rhow_kg*f*u**2)/8
!    if (da <=.0005 .or. h>.05) then 
!        f=0.0253
!        print*, 'f=', f  
!    else 
!        f=0.24/((log10(12*h/ks))**2)
!        print*, 'f=', f  
!    end if 
!taub=rhow_kg*f*u*u    
   
!taub=rhow_kg*f*(u**2)
!print*, "Benthic Shear Stress =", taub
call probofdep(dprob)
bf=rhob*wb*dprob
!sf=rhob*sv*dprob!Q- does this statement need to come eariler
print*,bf
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
rsf=rsv*rhos!*(((86400*g)/(18*vis))*drho*sds)
print*,rsf                
Tsf=(bf-(rsf*3600))
!Could used -vs1=sv-- but I would like more direct so changing over to vs1 in later equations
!May need to place conditional statement to describe when does this occur
print*, "tsf=", tsf
end subroutine sedflux5