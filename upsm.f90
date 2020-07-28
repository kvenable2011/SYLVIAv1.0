program ups
implicit none
real :: state_att_1(4,1), state_att(4), freq_count(7), fc(7), int_con(4), cumm_freq(7)
real :: state_var_time_1(6), state_var_time_2(6), state_var_time_3(6), state_var(6)
real, allocatable :: benthic_layer_3(:,:)
real :: om, sm, gs, lw, l, PCTWA, rhob, rhod, choice, sds, ds, dds, dv, dprob, dvf, tmxc, tmx,dlayer
real :: sfrs, rsv, f, ks, md, grho, ks1, ks2, rd, vs2, y1,drho, vs1, sf, gp, x, ws,ch, taub1, um, taub, sv, ts, a, tsf, rsf, lc
real :: pdx, pdy, maxlyrd, dx, st, dy, dm, ddtb, dtb, m, vol, wb, bf
real :: boulder, cobble, pebble, granule, sand, silt, clay ,dx_1,num_par, m_1
real, parameter:: rhow=.998, g=9.807, vis=.001
integer, parameter::ns=3
real :: rhos, q
integer :: i,j,irow, rtm, tinc, comid,so
real:: n, h, da,u, svol, tot_mass, tot_vol, tot_rhos, maxlyrd_b
open(unit=15, file="C:\Users\kvenable\Output\mass05MAY20f.txt",status="new")
open(unit=25, file="C:\Users\kvenable\Output\conc05MAY20f.txt", status="new")
print*, "What is your comid?"!Will turn into an outside read statement from file/API  
read (5, *) comid   
!a = q/u
call input(h,da,lc,q,u,a,dx,dx_1)
dlayer=h/real(ns-1) !can set to (ns-1 for equality in depth of active layers) 
print*, "layer depth=", dlayer
svol=q/dlayer
tmx=lc/u
tmxc= tmx/3600
!Place statement here about recalc of this based on reaching tmx and
print*, "total maximum time(x)in sec and total time in hours",tmx, tmxc
print*, "enter run time max in x seconds"
read (5, *) rtm, tinc
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
int_con(1:4) = (/real(comid),tmx, dx, dx_1/) 
print*, int_con
print*, "Would you like to choose (1) Descriptive, (2) Van Rijn, or (3) Roberts (erosion) transport"
read (5,*) so
if (so==1) then 
do i=1,ns
     if (i<ns) then 
     maxlyrd =dlayer* real(i)
     m_1 = tsf * a
     call passed (state_var,state_att) !(or passed 1)
     if (i==1) then
        state_var_time_1=state_var(:)
        state_att_1(1:4,i) =state_att(1:4)
        fc = freq_count(:)
        else 
        state_var_time_2=state_var(:)
        state_att_1(1:4,i) = state_att (1:4)
        fc = freq_count(:)
        end if 
        else if( i==ns) then 
        call benthic(state_var,i,rhob, state_att,freq_count)
        maxlyrd_b=state_var_time_2(4) + 0.1
        state_var_time_3=state_var(:)
        state_att_1(1:4,i)=state_att(1:4)
        fc = freq_count(:)
        tot_mass= state_var_time_1(2)+state_var_time_2(2)+state_var_time_3(2)
        tot_rhos=(state_att_1(4,1)+state_att_1(4,2) + state_att_1(4,3))
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
    m_1= tsf * a
    call passed1(state_var,state_att,freq_count) 
        if (i==1) then
        state_var_time_1=state_var(:)
        state_att_1(1:4,i) =state_att(1:4)
        fc = freq_count(:)
        else 
        state_var_time_2=state_var(:)
        state_att_1(1:4,i) = state_att (1:4)
        fc = freq_count(:)
        end if 
        else if( i==ns) then 
        call benthic(state_var,i,rhob, state_att,freq_count)
        maxlyrd_b=state_var_time_2(4) + 0.1
        state_var_time_3=state_var(:)
        state_att_1(1:4,i)=state_att(1:4)
        fc = freq_count(:)
        tot_mass= state_var_time_1(2)+state_var_time_2(2)+state_var_time_3(2)
        tot_rhos=(state_att_1(4,1)+state_att_1(4,2) + state_att_1(4,3))
        tot_vol=tot_mass/tot_rhos
        print*, "Your tot_mass is,=",tot_mass
        print*, "Your total sed volume,=", tot_vol
    !cumm_freq(:) = 0 
    !cumm_freq = (fc/ns)*100 !As percentage
    end if
    cumm_freq(:) = 0 
    cumm_freq = (fc/ns)*100 !As percentage
    !print*, cumm_freq
end do
 end if
    
end program ups