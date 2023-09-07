program sed3layer
implicit none 
real, dimension (7)::freq_count, cumm_freq, fc
real, dimension (7,1):: state_att_1,state_att_2, state_att_3
real, dimension (7):: state_att 
real, dimension (9),target::state_var_time_1, state_var_time_2, state_var_time_3, state_var
real, dimension (:,:), allocatable, target::benthic_layer_3 
real, dimension (4) :: int_con
!real, dimension (7,3) ::state_att_all
real, dimension (:,:), allocatable:: sed_1, sed_2,mass_out, conc_out
real, dimension (:,:), allocatable, target ::timeout, timeout_1,p
real, dimension (:,:), pointer ::conc_time_1, mass_time_1,conc_time_2, conc_time_3, mass_time_2, mass_time_3
real, dimension (:), pointer :: test1
real, pointer:: output_mass(:,:), state_att_all(:,:), state_var_all(:,:) 
real, dimension (3):: report
real, dimension (:),allocatable::conc_1,conc_2,conc_3
integer::comid, yn, so, seg,lnumb
logical::y
real :: om, sm, gs, lw, l, PCTWA, rhob, rhod, choice, sds, ds, dds, dv, dprob,dvf,d1, d2, d3, d4, d5, d6, d7, tmxc, tmx,dlayer
real :: sfrs, rsv, f, ks, md, grho, ks1, ks2, rd, vs2, y1,drho, vs1, sf, gp, x, ws,ch, taub1, um, taub, sv, ts, a, tsf, rsf, lc
real :: pdx, pdy, maxlyrd, dx, d8, d9, d10, d11, st, dy, dm, ddtb, dtb, m, vol, wb, bf, dh, xks
real :: boulder, cobble, pebble, granule, sand, silt, clay ,load_i, conc_i, dx_1, advf_x, conc_a, conc_ss, num_par  
real :: num_par_1, num_par_t, num_par_tot, xterm, xt, dt, lpt, d_conc_i, mb_conc, m_1,d_m,d_m_i
real, parameter:: rhow=.998, g=9.807, vis=.001
real, pointer:: x0
integer, parameter::ns=3
real, target:: rhos, q
integer :: i,j,irow, rtm, tinc, ww
real:: n, h, da,u, svol, tot_mass, tot_vol, tot_rhos, maxlyrd_b
integer::jsize
real, dimension(:), allocatable::list, m_list
real:: element
real, dimension(2):: newmc
real,pointer :: nmass, nconc
open(unit=2, file="F:\Sediment HMS\mass15APR20j.text",status="new")
open(unit=3, file="F:\Sediment HMS\conc15APR20j.text", status="new")
print*, "What is your comid?"!Will turn into an outside read statement from file/API  
read (5, *) comid   
!a = q/u
call input(h,da,lc,q,u,a,dx,dx_1)
!Print *, "Enter water depth (m), median diameter size (m) and length (m)"
   !read(5, *) h, da, lc
!Print *, "Enter your water discharge (m^3/s) and water velocity m/s)"
 !  read(5, *) q, u    
!x0=0
!call area(a)
irow=ns
dlayer=h/real(ns-1) !can set to (ns-1 for equality in depth of active layers) 
print*, "layer depth=", dlayer
svol=q/dlayer
tmx=lc/u
tmxc= tmx/3600
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
!j = 1
!svol = q !Assuming multiplied by 1 second in order to get volume of streamflow for concentration calc
!print*, svol 
int_con(1:4) = (/real(comid),tmx, dx, dx_1/) 
print*, int_con
print*, "Would you like to choose (1) Descriptive, (2) Van Rijn, or (3) Roberts (erosion) transport"
read (5,*) so
!open(unit=8, file="C:\Users\kvenable\Documents\example\test16.txt", status="new", action="write")-Ichanged the options in 2lyrtest
if (so==1) then 
do i=1,ns
                !lnumb= i*1 - place statement here for else when i==ns-- down below
                if (i<ns) then 
                maxlyrd=dlayer* real(i)
                call passed (state_var,state_att)!state_var i, rhos, tsf, maxlyrd, dx, pdx, pdy)  
                    if (i==1) then
                    state_var_time_1=state_var(:)
                    else 
                    state_var_time_2=state_var(:)
                    end if 
                end if
            
              end do 
do j=1,rtm,tinc
    print*, "Time increment is" , j
    pdx=(u*real(j))
    !svol = q * j
    !call passtime (sed_1, state_var_time_1, pdx, pdy) ---statement which changes the calculation of 
    allocate(timeout(7,rtm)) 
    timeout(1,j) = state_var_time_1(1)
    timeout(2,j) = state_var_time_1(2)
    timeout(3,j) = state_var_time_1(3)
    timeout(4,j) = state_var_time_1(4) 
    timeout(5,j) = state_var_time_1(5)
    timeout(6,j) = pdx
    timeout(7,j) = (state_var_time_1(4)-dlayer)+((state_var_time_1(3)/state_var_time_1(2))*real(j))
    allocate(timeout_1(7,rtm)) 
    timeout_1(1,j) = state_var_time_2(1)
    timeout_1(2,j) = state_var_time_2(2)
    timeout_1(3,j) = state_var_time_2(3)
    timeout_1(4,j) = state_var_time_2(4) 
    timeout_1(5,j) = state_var_time_2(5)
    timeout_1(6,j) = pdx
    timeout_1(7,j) = (state_var_time_2(4)-dlayer)+((state_var_time_2(3)/state_var_time_2(2))*real(j))
    allocate(benthic_layer_3(7,rtm))
     benthic_layer_3(1,j)= real(ns)
     benthic_layer_3(2,j)=0
     benthic_layer_3(3,j)=tsf
     benthic_layer_3(4,j)=h
     benthic_layer_3(5,j)=pdx
     benthic_layer_3(6,j)=pdx
     benthic_layer_3(7,j)= pdy
      if (timeout(7,j)>timeout(4,j)) then
                timeout_1(3,j) = (timeout_1(3,j)+timeout(3,j))
                timeout_1(2,j)=timeout(2,j) + timeout_1(2,j)
                timeout(2,j)= timeout(2,j)-timeout(2,j)
                timeout(6,j)=0
                timeout(7,j)=0
      else
                timeout(2,j)=timeout(2,j)
                timeout_1(2,j)=timeout_1(2,j)
                
      end if
    print*, "i    rhos    tsf    maxlyrd    dx    pdx    pdy    " 
    print*, timeout (:,j)
    print*, timeout_1(:,j)
    !print*, benthic_layer_3(:,j)
    deallocate(timeout) 
    deallocate(timeout_1) 
end do              
else if (so==2) then 
do i=1,ns
!fc(:)=0
!lnumb= i*1 - place statement here for else when i==ns-- down below
    if (i<ns) then 
    maxlyrd=dlayer* real(i)   
    call passed1(state_var,i,rhos,sv,maxlyrd,tsf,pdx,pdy,state_att,a,freq_count,conc_i,d_m_i,d_conc_i,num_par_1,d_m) 
        if (i==1) then
        state_var_time_1=state_var(:)
        state_att_1(1:7,i) =state_att(7)
        fc = freq_count(:)
        else 
        state_var_time_2=state_var(:)
        state_att_1(1:7,i) = state_att (7)
        fc = freq_count(:)
        end if 
        else if( i==ns) then 
        call benthic(state_var,i,m_1,rhob, wb, bf, tsf, pdx, pdy,state_att, comid, PCTWA, freq_count, vol,dh,a)
        maxlyrd_b=state_var_time_2(4) + 0.1
        state_var_time_3=state_var(:)
        state_att_1(1:7,i)=state_att(7)
        fc = freq_count(:)
        tot_mass= state_var_time_1(2)+state_var_time_2(2)+state_var_time_3(2)
        tot_rhos=(state_att_1(6,1)+state_att_1(6,2) + state_att_1(6,3))
        tot_vol=tot_mass/tot_rhos
        print*, "Your tot_mass is,=",tot_mass
        print*, "Your total sed volume,=", tot_vol
    end if
    !allocate(state_att_all(7,3))
    !state_att_all (1:7,1) = state_att_1(1:7,i)
    !state_att_all (1:7,2) = state_att_2(1:7,i)
    !state_att_all (1:7,3) = state_att_3(1:7,i)


    cumm_freq(:) = 0 
    cumm_freq = (fc/ns)*100 !As percentage
    !print*, cumm_freq
    !print*, state_att_all
end do
 !print*, state_att_all(7,3)
 
do j=1,rtm,tinc
    print*, "Time increment is" , j
    pdx=(u*real(j))
    ! (j>1) then
    !newmc(1)=nmass
    !newmc(2)=nconc
    !print*, newmc
    !end if
    !d_conc_i = -(tsf/dx)*real(j) !Change in transport concentration over time through flux over time 
    !This will need to be calculated at each area/discharge change every hour, place conditional statement to adjust for this 
    !mb_conc = conc_i + d_conc_i 
!d_m = -(m_1/dx_1)
!d_m_i = -(tsf/(dx_1*10**6)) !Change in transport concentration over time through flux over time 
!print*, d_m_i
!conc_i = (tsf/(dx_1*10**6)) !converted to g/ml 
!print*, conc_i 
!d_conc_i = m_1/svol
!print*, d_conc_i
!num_par_1 = conc_i*lc*a
    !num_par_1 = conc_i*(lc-pdx)*a
    !mb_conc = conc_i + d_conc_i 
    !call passtime (sed_1, state_var_time_1, pdx, pdy) ---statement which changes the calculation of 
    !test1=>state_var_time_1
    allocate(timeout(9,rtm)) 
    timeout(1,j) = state_var_time_1(1)
    timeout(2,j) = state_var_time_1(2)
    timeout(3,j) = state_var_time_1(3)
    timeout(4,j) = state_var_time_1(4) 
    timeout(5,j) = state_var_time_1(5)
    timeout(6,j) = pdx
    timeout(7,j) = (state_var_time_1(4)-dlayer)+(state_var_time_1(3)*real(j))
    timeout(8,j) = state_var_time_1(8)-((state_var_time_1(8)/dx_1)*pdx)
    timeout(9,j) = (state_var_time_1(9)/(svol*10**6)) - (pdx*(state_var_time_1(9)/(svol*10**6))/dx_1) 
    allocate(timeout_1(9,rtm)) 
    timeout_1(1,j) = state_var_time_2(1)
    timeout_1(2,j) = state_var_time_2(2)
    timeout_1(3,j) = state_var_time_2(3)
    timeout_1(4,j) = state_var_time_2(4) 
    timeout_1(5,j) = state_var_time_2(5)
    timeout_1(6,j) = pdx
    timeout_1(7,j) = (state_var_time_2(4)-dlayer)+(state_var_time_2(3)*real(j))
    timeout_1(8,j) = state_var_time_2(8)-((state_var_time_2(8)/dx_1)*pdx)
    timeout_1(9,j) = (state_var_time_2(9)/(svol*10**6)) - (pdx*(state_var_time_2(9)/(svol*10**6))/dx_1) 
    allocate(benthic_layer_3(10,rtm))
    benthic_layer_3(1,j)= state_var_time_3(1)
    benthic_layer_3(2,j)= state_var_time_3(2)
    benthic_layer_3(3,j)= state_var_time_3(3) 
    benthic_layer_3(4,j)= state_var_time_3(4)
    benthic_layer_3(5,j)= state_var_time_3(5)
    benthic_layer_3(6,j)= pdx
    benthic_layer_3(7,j)= ((maxlyrd_b-0.1)+.01) +(state_var_time_3(5)*real(j)) !For relative depth below the water column 
    benthic_layer_3(8,j)=state_var_time_3(8)-((state_var_time_3(8)/dx_1)*pdx)
    benthic_layer_3(9,j)=(state_var_time_3(9)/(svol*10**6)) - (pdx*(state_var_time_3(9)/(svol*10**6))/dx_1) 
    benthic_layer_3(10,j) =state_var_time_3(4)/a
    !allocate (sed_1(7,j))
    !sed_1(1,j) = cumm_freq(1)
    !sed_1(2,j) = cumm_freq(2)
    !sed_1(3,j) = cumm_freq(3)
    !sed_1(4,j) = cumm_freq(4)
    !sed_1(5,j) = cumm_freq(5)
    !sed_1(6,j) = cumm_freq(6)
    !sed_1(7,j) = cumm_freq(7)
    !mass_time_1=> timeout(7:9,j:j+4)
    !mass_time_2=> timeout_1(7:9,j:j+4)
    !mass_time_3=> benthic_layer_3(7:10,j:j+4)
        
!    !allocate(mass_time_1(rtm))
!    allocate(mass_time_2(rtm))
!    allocate(mass_time_3(rtm))
    !mass_time_1=> timeout(1:9,j:j+1)
    !mass_time_2=> timeout_1(1:9,j:rtm)
    !mass_time_3=> benthic_layer_3(1:10,j:rtm)
!    mass_time_1(j) = timeout(8,j)
!    mass_time_2(j) = timeout_1(8,j)
!    mass_time_3(j) = benthic_layer_3(8,j)
    !call lister (list, element)
        if (j==1) then
        write(2,*) "TIME","ML1","ML2","ML3"
        write(3,*) "TIME", "CL1", "CL2", "CL3"
        !allocate(mass_time_1(rtm))
        !allocate(mass_time_2(rtm))
        !allocate(mass_time_3(rtm))
        !mass_time_1=> timeout(7:9,j:j+4)
        !mass_time_2=> timeout_1(7:9,j:j+4)
        !mass_time_3=> benthic_layer_3(7:10,j:j+4)
       !mass_time_1=> timeout(7:9,j:j)
        !mass_time_2=> timeout_1(7:9,j:j)
        !mass_time_3=> benthic_layer_3(7:10,j:j)
        end if 
    !write(2,*) j, mass_time_1(j), mass_time_2(j), mass_time_3(j)
    !allocate(output_mass(4))! ------> use subroutine here for element list progression
   ! output_mass(:)=(/ real(j), mass_time_1(j), mass_time_2(j), mass_time_3(j)/) 
       
    !write(2,*) output_mass
    !call outmat (report)
    !call relocate (conc_1,conc_2,conc_3)
    !allocate(mass_out(4,tmx))
    !mass_out(1:4,j)= output_mass(:,:)
      if (timeout(7,j)>timeout(4,j)) then
                timeout_1(3,j) = (timeout_1(3,j) + timeout(3,j))!Should this be an or Average or totality settling velocity
                timeout_1(2,j)=timeout(2,j) + timeout_1(2,j)
                timeout(2,j)= timeout(2,j)-timeout(2,j)
                timeout(6,j)=0
                timeout(7,j)=0
                timeout_1(5,j)=timeout_1(5,j)+timeout(5,j)
                timeout(5,j)=timeout(5,j)-timeout(5,j)
                timeout_1(8,j)=timeout(8,j) + timeout_1(8,j)
                timeout(8,j)= timeout(8,j)-timeout(8,j)
                timeout_1(9,j)=timeout(9,j) + timeout_1(9,j)
                timeout(9,j)= timeout(9,j)-timeout(9,j)
                state_att_2(6,1)= state_att_2(6,1) + state_att_1(6,1) 
                state_att_1(6,1)=0
                !mass_time_2(j) = timeout_1(8,j) + timeout(8,j)
                !mass_time_1(j) = timeout(8,j) - timeout(8,j)
                !sed_1(:,j)= sed_1(:,j) + sed_1(:,(j- tinc))
                !sed_1(:,j-tinc) =0
        end if        
     !used for the depostion into the benthic layer from second layers
        if(timeout_1(7,j)>timeout_1(4,j)) then   
                benthic_layer_3(3,j) = (timeout_1(3,j) + benthic_layer_3(3,j))!This will show the total depostional/settling velocity within the system (don't think this is necessary)
                benthic_layer_3(2,j)=  timeout_1(2,j) + benthic_layer_3(2,j) !Bulk density plus the additional density of sediments in the water column 
                timeout_1(2,j)= timeout_1(2,j)-timeout_1(2,j)
                timeout_1(6,j)=0
                timeout_1(7,j)=0
                benthic_layer_3(5,j)=timeout_1(5,j)+benthic_layer_3(5,j) !Total Flux in the benthic layer 
                timeout_1(5,j)=timeout_1(5,j)-timeout_1(5,j)
                benthic_layer_3(8,j)=  timeout_1(8,j) + benthic_layer_3(8,j)
                timeout_1(8,j)= timeout_1(8,j)-timeout_1(8,j)
                benthic_layer_3(9,j)=  timeout_1(9,j) + benthic_layer_3(9,j)
                timeout_1(9,j)= timeout_1(9,j)-timeout_1(9,j)
                state_att_3(6,1)= state_att_3(6,1) + state_att_2(6,1) 
                state_att_2(6,1)=0
        if(benthic_layer_3(7,j)>= maxlyrd_b) then
        benthic_layer_3(4,j)= state_var_time_3(4)-(benthic_layer_3(8,j)/state_var_time_3(4))
        end if
                !mass_time_3(j) = timeout_1(8,j) + benthic_layer_3(8,j)
                !mass_time_2(j) = timeout(8,j) - timeout(8,j)
                !sed_1(:,j)= sed_1(:,j) +sed_1(:,(j-tinc))
                !sed_1(:,j-tinc) =0
      !else
!                timeout(2,j)=timeout(2,j)
!                timeout_1(2,j)=timeout_1(2,j)
!                timeout(5,j)=timeout(5,j)
!                timeout_1(5,j) = timeout_1(5,j) 
!                timeout(8,j)=timeout(8,j)
!                timeout_1(8,j) = timeout_1(8,j) 
!                timeout(9,j)=timeout(9,j)
!                timeout_1(9,j) = timeout_1(9,j) 
!                benthic_layer_3(5,j) = benthic_layer_3(5,j)
!                benthic_layer_3(2,j) = benthic_layer_3(2,j)
!                benthic_layer_3(8,j) = benthic_layer_3(8,j)
!                benthic_layer_3(9,j) = benthic_layer_3(9,j) 
!                sed_1(:,j)= sed_1(:,j) 
      end if  
    print*, "i    rhos    sv    maxlyrd    tsf    pdx    pdy    "   
!    allocate(conc_time_1(rtm))
!    allocate(conc_time_2(rtm))
!    allocate(conc_time_3(rtm))
!    conc_time_1(j) = timeout(8,j)
!    conc_time_2(j) = timeout_1(8,j)
!    conc_time_3(j) = benthic_layer_3(8,j)
    print*, timeout (:,j)
    print*, timeout_1(:,j)
    print*, benthic_layer_3(:,j)
    
    
    !if(j>=2) then
    !print*, timeout(8,1), timeout_1(8,3), benthic_layer_3(8,5)
    !end if 
    !print*, sed_1(:,j) 
    write(2,*) j, timeout(8,j), timeout_1(8,j), benthic_layer_3(8,j), benthic_layer_3(4,j),benthic_layer_3(10,j)
    write(3,*) j, timeout(9,j), timeout_1(9,j), benthic_layer_3(9,j)
    !print*,timeout(8,j)
    !print*,timeout_1(8,j)
    !print*,benthic_layer_3(8,j)
    !print*, mass_time_1(j)
    !print*, mass_time_2(j)
    !print*, mass_time_3(j)
    !print*, list(j)
    !write (2,*) output_mass
    !if(j==tmx) then
    !write(2,*) mass_out(1:4,j)
    !end if
    !if (j>=2) then 
    !call newmass(nmass,nconc)
    !newmc(1) = nmass
    !newmc(2) = nconc
    !nmassm(1,j-1) = nmass
    !nconcm(2,j-1) = nconc
    deallocate(timeout) 
    deallocate(timeout_1)
    deallocate(benthic_layer_3)
    !deallocate(sed_1) 
    !deallocate(mass_time_1)
    !deallocate(mass_time_2)
    !deallocate(mass_time_3)
    !deallocate (output_mass)
    !deallocate (mass_out)
    !call graph(p)
    !print*, "memormass", mass_time_1(:,j), mass_time_2(:,j), mass_time_3(:,j)
end do 
!print*, output_mass
!deallocate (output_mass)
end if
!print*, conc_out
!print*, test1
!print*, conc_time_1(j:tmx)
!print*, conc_time_2(j:tmx)
!print*, conc_time_3(j:tmx)
!deallocate(conc_out)
!print*, mass_time_1, mass_time_2, mass_time_3!(8,j:rtm), mass_time_2(8,j:rtm), mass_time_3(8,j:rtm)
!print*, "memormass",mass_time_1(:,1:4), mass_time_2(:,1:4), mass_time_3(:,1:4)
!deallocate(mass_time_1)
!deallocate(mass_time_2)
!deallocate(mass_time_3)
close(2)
close(3)
end program sed3layer
