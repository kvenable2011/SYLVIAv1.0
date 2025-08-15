subroutine input(h,da,lc,q,u,a,dx,dx_1,ks1,ks2,ks,taub)
implicit none
real::h, da,a,lc, q, u, ks1, ks2, ks, logb, dx, dx_1, f, taub
!real,intent(out):: taub
real,parameter::rhow_kg=998
!common /shear/taub
Print *, "Enter water depth (m), median diameter size (m) and length (m)"
   read(5, *) h, da, lc
ks1 = 3*da
ks2 = 0.01*h
ks = max(ks1,ks2)
print*, ks1, ks2, ks
!ks = max(ks1,ks2)  
    if (da <=.0005 .or. h>.05) then 
        f=0.0253
    else 
        logb=12*h/ks
        f=0.24/((log10(logb))**2)
    end if  
Print*, "f=", f    
Print *, "Enter your water discharge (m^3/s) and water velocity m/s)"
   read(5, *) q, u    
a=q/u 
write(10,*) "Your cross-sectional area is", a
taub=(rhow_kg*f*u*u)/8    
write(10,*) "The initial benthic shear stress is", taub 
!save taub   
dx=u*1 !displacement in 1 sec 
dx_1= u*1*86400 !displacement in 1 day 
write(10,*) "Total displacement in 1 sec is=" ,dx 
write(10,*) "Total displacement in 1 day withing reach is=" , dx_1
!write(10,*)"Your initial", comid,  "and max time of travel and displacement(per day and per sec) is=", int_con

!print*, "If using the (3) Van Rijn, settling or (4) Roberts (erosion) enter your deposition probability &
!type, (1) for Krone and (2) for Wool."
!    read (5,*) dpc
!if (dpc==1) then
!    call probofdep1o(dprob)
!    !write(10,*) "Your probablity of deposition is",  dprob.
!else if (dpc==2) then
!    call probofdep2o(dprob)
!    !write(10,*) "Your probablity of deposition is",  dprob.
!else 
!    dprob =  1
!    !write(10,*) "Your probablity of deposition is",  dprob.
!end if 
end subroutine input       