program ups
implicit none
real :: state_att(4), freq_count(7), fc(7), int_con(3), cumm_freq(7),state_var(6)
real :: state_att_1(4,3)
!real, pointer :: rhos1, rhos2, rhos3
real, allocatable ::timeout_1(:,:),timeout(:,:),benthic_layer_3(:,:),state_var_time_1(:), state_var_time_2(:), state_var_time_3(:)
real, allocatable ::mad(:), mad_b(:), mad_1(:), ml_ly1(:), ml_ly2(:), ml_ly3(:)
real, allocatable ::tot_loss_ly1(:), tot_loss_ly2(:), tot_loss_ly3(:) !For mass adjustment to use within interation
real :: om, sm, gs, lw, l, PCTWA, rhob, rhod, choice, sds, ds, dds, dv, dprob, dvf, tmxc, tmx,dlayer
real :: sfrs, rsv, f, ks, md, grho, ks1, ks2, rd, vs2, y1,drho, vs1, sf, gp, x, ws,ch, taub1, um, taub, sv, ts, a, tsf, rsf, lc
real :: pdx, pdy, maxlyrd, dx, st, dy, dm, ddtb, dtb, m, vol, wb, bf, mb
real :: boulder, cobble, pebble, granule, sand, silt, clay ,dx_1,num_par, m_1
real, parameter:: rhow=.998, g=9.807, vis=.001
integer, parameter::ns=3,k=12
real :: rhos, q, d_m_1, d_m_2, d_m_3, start, finish
integer :: i,j,irow, rtm, tinc, so
real:: n, h, da,u, svol, tot_mass, tot_vol, tot_rhos, maxlyrd_b, tsv_1, tsv_2, tsv_3
character(len=16):: comid
!filename = "C:\Users\kvenable\Output\GlobalStateFile" + "date" +"time"
open(unit=10, file="C:\Users\kvenable\Output\GlobalStateFile16JUN20d.txt",status="new")
call cpu_time(start)
!call date_and_time(
open(unit=15, file="C:\Users\kvenable\Output\XY16JUN20d.txt",status="new")
open(unit=25, file="C:\Users\kvenable\Output\mass16JUN20d.txt", status="new")
open(unit=35, file="C:\Users\kvenable\Output\conc16JUN20d.txt", status="new")
open(unit=45, file="C:\Users\kvenable\Output\mad16JUN20d.txt", status="new")
open(unit=55, file="C:\Users\kvenable\Output\ml16JUN20d.txt", status="new")

print*, "What is your comid?"!Will turn into an outside read statement from file/API  
read (5, *) comid   
!a = q/u
call input(h,da,lc,q,u,a,dx,dx_1)
dlayer=h/real(ns-1) !can set to (ns-1 for equality in depth of active layers) 
write(10,*) "layer depth=", dlayer
maxlyrd_b=h+ 0.1
write(10,*) "Maximum benthic depth set to", maxlyrd_b
svol=q/dlayer
tmx=lc/u
tmxc= tmx/3600
!Place statement here about recalc of this based on reaching tmx and
print*, "total maximum time(x)in sec and total time in hours",tmx, tmxc
print*, "enter run time max in x seconds, and time increment - will place back after working properly with units (no scalling yet)"
read (5, *) rtm, tinc
!dx=u*1 !displacement in 1 sec 
!dx_1= u*1*86400 !displacement in 1 day 
!print*, "dx=" ,dx 
!print*, "dx_1=" , dx_1
pdx=0
dy=0
pdy=0
int_con(1:3) = (/tmx, dx, dx_1/) 
write(10,*)"Your initial", comid,  "and max time of travel and displacement(per day and per sec) is=", int_con
print*, "Would you like to choose (1) Descriptive, (2) Van Rijn, or (3) Roberts (erosion) transport"
read (5,*) so
if (so==1) then 
do i=1,ns
     if (i<ns) then 
     maxlyrd =dlayer* real(i)
     call passed(state_var,i,m_1,maxlyrd,tsf,pdx,pdy,state_att,a,freq_count) !(or passed 1)
     if (i==1) then
     !allocate (state_var_time_1(k),state_var_time_2(k), state_var_time_3(k))
        allocate(state_var_time_1, SOURCE=state_var)  
        !state_var_time_1=state_var(1:6)
        state_att_1(1:4,i) =state_att(1:4)
        fc = freq_count(:)
        else 
        allocate(state_var_time_1, SOURCE=state_var) 
        !state_var_time_2=state_var(1:6)
        state_att_1(1:4,i) = state_att (1:4)
        fc = freq_count(:)
        end if 
        else if( i==ns) then 
        call benthic(state_var,i,m_1,maxlyrd_b,tsf,pdx,pdy,state_att,a,freq_count)
        !maxlyrd_b=h+ 0.1
!This is where the new type of memory allocation should begin using MALLOC AND MOVE_ALLOC        
        allocate(state_var_time_1, SOURCE=state_var) 
        !state_var_time_3=state_var(1:6)
        state_att_1(1:4,i)=state_att(1:4)
        fc = freq_count(:)
        tot_mass= state_var_time_1(2)+state_var_time_2(2)+state_var_time_3(2)
        tot_rhos=state_att_1(4,1)+state_att_1(4,2)+state_att_1(4,3)
        tot_vol=tot_mass/tot_rhos
        print*, "Your tot_mass is,=",tot_mass
        print*, "Your total sed volume,=", tot_vol
        end if
    end do     
else if (so==2) then 
do i=1,ns
!fc(:)=0
!lnumb= i*1 - place statement here for else when i==ns-- down below
    if (i<ns) then 
    maxlyrd=dlayer* real(i) 
    !m_1= tsf * a
    call passed1(state_var,i,m_1,maxlyrd,tsf,pdx,pdy,state_att,a,freq_count)
       if (i==1) then
        allocate(state_var_time_1, SOURCE=state_var) 
        !state_var_time_1=state_var(1:4)
        state_att_1(1:4,i) =state_att(1:4)
        fc = freq_count(:)
        else 
        allocate(state_var_time_2, SOURCE=state_var) 
        !state_var_time_2=state_var(1:4)
        state_att_1(1:4,i) = state_att (1:4)
        fc = freq_count(:)
        end if 
        else if( i==ns) then 
        call benthic(state_var,i,m_1,maxlyrd_b,tsf,pdx,pdy,state_att,a,freq_count)
        !maxlyrd_b=h+ 0.1
!This is where the new type of memory allocation should begin using MALLOC AND MOVE_ALLOC
        allocate(state_var_time_3, SOURCE=state_var) 
        !state_var_time_3=state_var(1:6)
        state_att_1(1:4,i)=state_att(1:4)
        write(10,*) state_att_1(1:4,i)
        fc = freq_count(:)
        tot_mass= state_var_time_1(2)+state_var_time_2(2)+state_var_time_3(2)
        tot_rhos=state_att_1(4,1)+state_att_1(4,2)+state_att_1(4,3)
        tot_vol=tot_mass/tot_rhos
        write(10,*) "Your tot_mass is,=",tot_mass
        print*, "Your tot_mass is,=",tot_mass
        write(10,*) "Your total sed volume,=", tot_vol
        print*, "Your total sed volume,=", tot_vol
    !cumm_freq(:) = 0 
    !cumm_freq = (fc/ns)*100 !As percentage
    end if
    cumm_freq(:) = 0 
    cumm_freq = (fc/ns)*100 !As percentage
    !print*, cumm_freq
end do
 end if
mad = [real ::] !create an empty array [] for mass adjustment on the fly 
mad_1 = [real ::]
mad_b = [real ::]
tot_loss_ly1 = [real ::] !create an empty array [] for total mass loss summation to be used with mass balance on the fly 
tot_loss_ly2 = [real ::] 
tot_loss_ly3 = [real ::]
ml_ly1 = [real ::] !create an empty array [] for finite difference on the fly 
ml_ly2 = [real ::]
ml_ly3 = [real ::]
do j=1,rtm,tinc
    print*, "Time increment is" , j-1
    pdx=(u*real(j-1)) !Since this is in scope at all times in, not needed to include in allocated space. Just need to print out 
    tsv_1 =(state_var_time_1(3)/10**4)/state_att_1(4,1)!Total Settling Velocity LY1
    tsv_2 =(state_var_time_2(3)/10**4)/state_att_1(4,2)!Total Settling Velocity LY2
    tsv_3 =(state_var_time_3(3)/10**4)/state_att_1(4,3)!Total Settling Velocity LY3
    d_m_1 =state_var_time_1(2)/dx_1 !MSF/dx rate of change for LY1
    d_m_2 =state_var_time_2(2)/dx_1 !MSF/dx rate of change for LY2
    d_m_3 =state_var_time_3(2)/dx_1 !MSF/dx rate of change for LY3
    mad = [mad,real(j)]
    mad_1 = [mad_1,real(j)]
    mad_b = [mad_b,real(j)]
    tot_loss_ly1 = [tot_loss_ly1,real(j)]
    tot_loss_ly2 = [tot_loss_ly2,real(j)]
    tot_loss_ly3 = [tot_loss_ly3,real(j)]
    ml_ly1 = [ml_ly1, real(j)]
    ml_ly2 = [ml_ly2, real(j)]
    ml_ly3 = [ml_ly3, real(j)]
    !if (j==1) then
    !    print*, tsv_1,tsv_2,tsv_3,d_m_1,d_m_2,d_m_3
    !end if 
    allocate(timeout(3,rtm))
    timeout(1,j)=(state_var_time_1(4)-dlayer)+(tsv_1*real(j-1)) !y displacement
    timeout(2,1)=state_var_time_1(2)-(d_m_1*pdx)!change in mass flux
    timeout(3,1)=timeout(2,j)/svol!concentration scaling factor
    timeout(2,2:j)= mad(j-1)-(d_m_1*pdx)!change in mass flux
    timeout(3,2:j)=timeout(2,j)/svol!concentration scaling factor
    allocate(timeout_1(3,rtm))
    timeout_1(1,j)=(state_var_time_2(4)-dlayer)+(tsv_2*real(j-1))!y displacement               !y displacement
    timeout_1(2,1)=state_var_time_2(2)-(d_m_2*pdx)!change in mass flux
    timeout_1(3,1)=timeout_1(2,j)/svol!concentration scaling factor
    timeout_1(2,2:j)=mad_1(j-1)-(d_m_2*pdx)!change in mass flux
    timeout_1(3,2:j)=timeout_1(2,j)/svol!concentration scaling factor
    allocate(benthic_layer_3(5,rtm))
    benthic_layer_3(1,j)=((h+.01)+(tsv_3*real(j-1))) !y displacement
    !((maxlyrd_b-0.1)+.01) +(benthic_layer_3(3,j)*real(j)) !For relative depth below the water column 
    benthic_layer_3(2,1)= state_var_time_3(2)-(d_m_3*pdx)!change in mass flux
    benthic_layer_3(2,2:j)= mad_b(j-1)- (d_m_3*pdx) !change in mass flux
    benthic_layer_3(3,j)= benthic_layer_3(2,j)/svol!concentration scaling factor
    benthic_layer_3(4,j)= benthic_layer_3(2,j)/tot_rhos!accummulated total vol in benthic layer
    benthic_layer_3(5,j)= 0
    !if (j==1) then !This may have changed the true calc by setting to zero
        !mad(j) = 0
        !mad_1(j) = 0
        !mad_b(j) = 0
    !else 
    mad(j) = timeout(2,j)
    mad_1(j) = timeout_1(2,j)
    mad_b(j) = benthic_layer_3(2,j)
    !end if 
    ml_ly1(j) = mad(j) - mad(j-1)
    ml_ly2(j) = mad_1(j) - mad_1(j-1) 
    ml_ly3(j) = mad_b(j) - mad_b(j-1)
    tot_loss_ly1(j) = sum(ml_ly1)
    tot_loss_ly2(j) = sum(ml_ly2)
    tot_loss_ly3(j) = sum(ml_ly3)
    if(timeout(2,j)<0) then 
        timeout(2,j) =0
    else if (timeout_1(2,j)<0) then 
        timeout_1(2,j) =0
    else if (benthic_layer_3(2,j)<0) then 
        benthic_layer_3(2,j) = 0 
        write(10,*) "Model simmulation completed at", j, "seconds. Terminate model run."
        exit
    end if    
    !if (j>=3) then 
        !timeout(2,j) = (state_var_time_1(2)-tot_loss_ly1(j))-(d_m_1*pdx)
        !timeout_1(2,j) = (state_var_time_2(2)-tot_loss_ly2(j))-(d_m_2*pdx)
        !benthic_layer_3(2,j) = (state_var_time_3(2)- tot_loss_ly3(j))- (d_m_3*pdx) 
    !end if    
    !num_par_1 = conc_i*lc*a---can we now also provide a number of particles Will allocate later?
    !timeout(k,j) =0
    !state_var_time_1(1:6)=timeout(1:6,j) 
    !MOVE_ALLOC(state_var_time_1,timeout)
!!!!!!!!!!!!----Start back here to change the indexes only for the new allocation 
     if (timeout(1,j)>state_var_time_1(4)) then !Switch for deposition if pdy>maxlyrd
                !tsv_2 = (tsv_1 + tsv_2)!Total Settling Velocity after depostion into layer - Should this be an or Average or totality settling velocity
                !tsv_1 = tsv_1-tsv_1 !Setting LY1 TSV to zero No more depostion in layer
                timeout(1,j)=timeout(1,j)-timeout(1,j) 
                timeout_1(1,j)=(state_var_time_2(4)-dlayer)+((0.5*(tsv_1+tsv_2)*real(j-1)))  
                timeout_1(2,j)=timeout(2,j) + timeout_1(2,j) !
                timeout(2,j)= timeout(2,j)-timeout(2,j)
                timeout_1(3,j)= timeout(3,j)+timeout_1(3,j)
                timeout(3,j)= timeout(3,j)-timeout(3,j)
                mad_1(j) = mad(j) +mad_1(j) 
                mad(j) = mad(j) - mad(j) 
                tot_loss_ly2(j) = tot_loss_ly1(j) + tot_loss_ly2(j)
                tot_loss_ly1(j) = tot_loss_ly1(j) - tot_loss_ly1(j)
                ml_ly2(j) = ml_ly1(j) + ml_ly2 (j) 
                ml_ly1(j) = ml_ly1(j) - ml_ly1(j)
     end if        
     !used for the depostion into the benthic layer from second layers
        if(timeout_1(1,j)>state_var_time_2(4)) then   
                !tsv_3= tsv_2+tsv_3
                !tsv_2= tsv_2-tsv_2
                timeout_1(1,j) = timeout(1,j) -timeout(1,j)
                !benthic_layer_3(1,j)=((h+.01)+((tsv_3+tsv_2)*real(j-1)))
                benthic_layer_3(2,j) = (timeout_1(2,j) + benthic_layer_3(2,j))!This will show the total depostional mass 
                timeout_1(2,j)= timeout_1(2,j)-timeout_1(2,j) !Zero out the second layer mass
                benthic_layer_3(3,j) = (timeout_1(3,j) + benthic_layer_3(3,j))!This will show the total depostional conc
                timeout_1(3,j)= timeout_1(3,j)-timeout_1(3,j)!Zero out the second layer conc
                mad_b(j) = mad_1(j) +mad_b(j) 
                mad_1(j) = mad_1(j) - mad_1(j)
                tot_loss_ly3(j) = tot_loss_ly2(j) + tot_loss_ly3(j)
                tot_loss_ly2(j) = tot_loss_ly2(j) - tot_loss_ly2(j)
                ml_ly3(j) = ml_ly2(j) + ml_ly3 (j) 
                ml_ly2(j) = ml_ly2(j) - ml_ly2(j)
       end if 
       if (timeout(1,j)>=maxlyrd_b) then
            timeout(1,j)=timeout(1,j)-timeout(1,j) 
                !timeout_1(1,j)=(state_var_time_2(4)-dlayer)+((0.5*(tsv_1+tsv_2)*real(j-1)))  
                benthic_layer_3(2,j)=timeout(2,j) + benthic_layer_3(2,j) !
                timeout(2,j)= timeout(2,j)-timeout(2,j)
                benthic_layer_3(3,j)= timeout(3,j)+benthic_layer_3(3,j)
                timeout(3,j)= timeout(3,j)-timeout(3,j)
                mad_b(j) = mad(j) +mad_b(j) 
                mad(j) = mad(j) - mad(j) 
                tot_loss_ly3(j) = tot_loss_ly1(j) + tot_loss_ly3(j)
                tot_loss_ly1(j) = tot_loss_ly1(j) - tot_loss_ly1(j)
                ml_ly3(j) = ml_ly1(j) + ml_ly3 (j) 
                ml_ly1(j) = ml_ly1(j) - ml_ly1(j)
       end if
        if(benthic_layer_3(1,j)>= maxlyrd_b) then
            benthic_layer_3(1,j)= maxlyrd_b -(tsv_3*real(j-1)) !For relative height above benthic sfc layer
            benthic_layer_3(5,j)= maxlyrd_b-benthic_layer_3(1,j)
            benthic_layer_3(3,j)=benthic_layer_3(2,j)/q
        end if 
        print*, pdx, timeout(:,j)
        print*, pdx, timeout_1(:,j)
        print*, pdx, benthic_layer_3(:,j)
        !mb(j) = tot_mass-benthic_layer_3(2,j)
        !if mb(j) <= 0 then 
        !print*,'Run completed'
        !stop
    write(15,*) j-1, pdx, timeout(1,j), timeout_1(1,j), benthic_layer_3(1,j)
    write(25,*) j-1, pdx, timeout(2,j), timeout_1(2,j), benthic_layer_3(2,j), benthic_layer_3(4,j), benthic_layer_3(5,j)
    write(35,*) j-1, pdx, timeout(3,j), timeout_1(3,j), benthic_layer_3(3,j)
    write(45,*) j-1, pdx, mad(j), mad_1(j), mad_b(j)
    write(55,*) j-1, pdx, ml_ly1(j), ml_ly2(j), ml_ly3(j)
    deallocate(timeout) 
    deallocate(timeout_1)
    deallocate(benthic_layer_3)
     !if(benthic_layer_3(2,j)<0) then
       ! write(10,*) "No more sediment in model at",  j ,  "seconds. Simmulation complete."
        !exit
    !end if 
end do
close(55) 
close(45)    
close(35)
close(25)
close(15)
call cpu_time(finish)
write(10,*)"Total run time=",finish-start,"seconds"
close(10) 
end program ups