subroutine sedflux4(rhos,ds,tsf,freq_count,dprob)!, boulder, cobble, pebble, granule, sand, silt, clay)
implicit none
real:: tsf, sf, rsf, rsv, rhos, sv, drho, sds,ds, boulder, cobble, pebble, granule, sand, silt, clay
real :: ks1, ks2, da, h, ks, f, u, tbs, tb, taub, b 
real:: gp, x, rd, rhos_kg, rd_1, rd_2, sq, dprob
real, parameter:: rhow_kg= 998, g=9.807, vis=.001
real ::freq_count(7)
logical:: y
integer :: yn
real ::taucd1, taucd2, pd, t2, t1
!real, intent(inout):: dprob
call grainsz(rhos, ds)
call graintype (freq_count,boulder, cobble, pebble, granule, sand, silt, clay)
!tb = taub*1
rhos_kg= rhos*1000
gp=g*((rhos_kg/rhow_kg)-1)
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
!ks1 = 3*da
!ks2 = 0.01*h
!ks = max(ks1,ks2)
!print*, ks1, ks2, 
!ks = max(ks1,ks2) 
!print *," ks=", ks
!    if (da <=.0005 .or. h>.05) then 
!        f=0.0253
!    else 
!        f=0.24/((log10(12*h/ks))**2)
!    end if 
!taub=(rhow_kg*f*u*u)/8    
!print*, 'f=', f     
!taub=rhow_kg*f*(u**2)
!print*, "Benthic Shear Stress =", taub
call probofdep(dprob)
!print*, "Would you like to enter a value for (taucd1), enter any number; for the default press (1) for no?"
!    read(5,*) yn
!    y= .true. 
!        Do while(y)
!        if(yn==1) then 
!        y=.false.
!        taucd1= 0.0
!        print*, "LCSS is set at 0.0 [N/m^2]"
!        write(10,*) "LCSS is set at 0.0 [N/m^2]"
!            else 
!            y= .true. 
!                print*, "Enter your lcss"
!                    read(5,*) taucd1
!                Print*, "LCSS is set at", taucd1, "[N/m^2]."
!                write(10,*) "LCSS is set at", taucd1, "[N/m^2]."
!                exit
!            end if 
!         end do       
!         
!    print*, "Would you like to enter a value for (taucd2), enter any number; for the default press (1) for no?"
!    read(5,*) yn 
!    y= .true. 
!       Do while(y)
!            if(yn==1) then 
!            y=.false. 
!            taucd2= 0.2
!                print*, "UCSS is set at 0.2 [N/m^2]."
!                write(10,*) "UCSS is set at 0.2 [N/m^2]"
!            else 
!            y= .true. 
!                print*, "Enter your ucss"
!                    read(5,*) taucd2
!                    write(10,*) "UCSS is set at", taucd2, "[N/m^2]."
!                    print*, "UCSS is set at", taucd2, "[N/m^2]."
!            exit
!            end if
!!            t2 = taucd2 - taub
!!            t1 = taucd2 - taucd1
!!            print*, "Your upper critical threshold difference and threshold range is", t2, t1
!        end do  
!t2 = taucd2 - taub
!t1 = taucd2 - taucd1
!b = t2/t1
!print*, "Your upper critical threshold difference and threshold range is", t2, t1
!
!    print*, "Would you like to enter a value for (pd), enter any number for yes; for the default press (1) for no?" 
!    read (5,*) yn
!    y= .true.
!        Do while(y)
!        if(yn==1) then 
!        y=.false. 
!        pd= 1.0
!        print*, "Dimensionless exponent is set at 1.0"
!        write(10,*) "Dimensionless exponent is set at 1.0"
!            else 
!            y= .true.
!                print*, "Enter your dimensionless exponent."
!                    read(5,*) pd
!                    print*, "Dimensionless exponent is set at", pd
!                    write   (10,*) "Dimensionless exponent is set at", pd
!                    exit
!            end if 
!        end do    
!dprob = b**pd 
!!dprob = ((taucd2 - taub)/(taucd2-taucd1))**pd
!print*, "Your deposition probability for the system is", dprob, "[N/m^2]."                    
!write(10,*)"Your deposition probability for the system is", dprob, "[N/m^2]."  
sf=rhos*sv*dprob *1000*86400 ![m/day]Q- does this statement need to come eariler
print*,sf
!call graintype (freq_count,boulder, cobble, pebble, granule, sand, silt, clay)
!Start new code for new settling (VR) - moved up
!call probofdep(dprob)

!Time increment respentative of 1 day to calculate segment volume need to make sure j=1 doens't impact the iteration 

!Called Subroutine for probofdep in order to get new settling flux calc inlusive of dprob

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
Tsf=(sf-rsf)/86400 !in kg/(m^2*s)*3600 to scale up to (g/m^2-day) - Not sure which scalling factor
print*, tsf
!Could used -vs1=sv-- but I would like more direct so changing over to vs1 in later equations

!May need to place conditional statement to describe when does this occur
print*, "tsf=", tsf
end subroutine sedflux4