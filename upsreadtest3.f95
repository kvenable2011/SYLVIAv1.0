program newinput
IMPLICIT NONE
real :: state_att(6), freq_count(7), fc(7), int_con(4), cumm_freq(7),state_var(6)
real :: state_att_1(6,3)
!real, pointer :: rhos1, rhos2, rhos3
real, allocatable ::timeout_1(:,:),timeout(:,:),benthic_layer_3(:,:),state_var_time_1(:), state_var_time_2(:), state_var_time_3(:)
real, allocatable ::mad(:), mad_b(:), mad_1(:), ml_ly1(:), ml_ly2(:), ml_ly3(:)
real, allocatable ::tot_loss_ly1(:), tot_loss_ly2(:), tot_loss_ly3(:) !For mass adjustment to use within interation
real :: om, sm, gs, lw, l, PCTWA, rhob, rhod, choice, sds, ds, dds, dv,dvf, tmxc, tmx,dlayer
real :: sfrs, rsv, md, grho, rd, vs2, y1,drho, vs1, sf, gp, x, ws,ch, taub1, um,sv, ts, tsf, rsf
real :: pdx, pdy, maxlyrd, st, dy, dm, ddtb, dtb, m, vol, wb, bf, mb, dprob, u2
real :: boulder, cobble, pebble, granule, sand, silt, clay,num_par, m_1, tbs, t,l1,md1,d,q1,u1,tss
real, parameter:: rhow=.998, g=9.807, vis=.001
integer, parameter::ns=3!,k=12
real :: rhos,d_m_1, d_m_2, d_m_3, start, finish
integer :: irow, rtm, tinc, so,yn, i, j, J1, new_end_step, next_row, comid
real:: n, svol, tot_mass, tot_vol, tot_rhos, maxlyrd_b, tsv_1, tsv_2, tsv_3, rks
!character(len=:):: date  !Activate with line 
!character(len=16):: comid
logical :: y
REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: mydata, runtime
!REAL, DIMENSION(2) ::stp
INTEGER(KIND=4) :: nr, k, start_j
CHARACTER(LEN=100) :: infile, outfile
real::da,h,q,u, tic, tmx2, tc, rid, sumcount, stp, extratime, lasttimestep
real::init_area, final_j, next_time, akm
real, allocatable:: tica(:)
real:: ks1, ks2, ks, f, logb, sfcr,df, dxf, lsv, a, taub, area, tb, dx, dx_1
real,parameter::rhow_kg=998
common /shear/tb 
open(unit=10, file="C:\Users\kvenable\Output\GlobalStateFile19May22b.txt",status="new")
call cpu_time(start)
!call date_and_time
open(unit=15, file="C:\Users\kvenable\Output\XY19May22b.txt",status="new")
open(unit=25, file="C:\Users\kvenable\Output\mass19May22b.txt", status="new")
open(unit=35, file="C:\Users\kvenable\Output\conc19May22b.txt", status="new")
open(unit=45, file="C:\Users\kvenable\Output\mad19May22b.txt", status="new")
open(unit=55, file="C:\Users\kvenable\Output\ml19May22b.txt", status="new")
open(unit=65, file="C:\Users\kvenable\Output\inputmatrixpass19May22b.txt", status="new")
!! Ask the user for some information about the data to be read in
!write(*,*) "Enter the name of the data file to read..."
!read(*,*) infile
!write(*,*) "Enter the number of lines in the input data file..."
!read(*,*) nr 
! Allocate the memory required in variable mydata
tica = [real::]
call rivernet(infile,outfile,nr)
ALLOCATE(mydata(7,nr))
tica = [real:: (nr)]
!ALLOCATE(tica(nr))
! Open up the file to read
OPEN(UNIT=1,FILE=infile, status="old")
OPEN(UNIT=2,FILE=outfile, status="new")
! Now read the file into variable mydata
DO k=1,nr
 read(1,*)comid,l,da,h,q,u,akm !,date !Activate with line 18, 56, 58
 mydata(1:7,k)=(/real(comid),l,da,h,q,u,akm/) !Need to place date and mydata to (1:8,k) and declare in variables below
!mydata(1:8,k)=(/real(comid),l,da,h,q,u,akm,date/)
! tic = mydata(2,k)/mydata(6,k)
! tica = [tic*3280.84, real(k)-1]!---> Conversion from km/(ft/s) to m/s to s
! write(10,*) tica
 ks1 = 3*mydata(3,k) !---> just 3 but scalling for my dummy set right now
 ks2 = 0.01*mydata(4,k) !---> just 0.01 but scaling for the data assuming in m depth
 ks = max(ks1,ks2)
 !ks(k) = max(ks1,ks2)
 write(*,*) ks1, ks2, ks
 tic = 0
 tic = ((mydata(2,k))/(mydata(6,k)))/3600 !------> time in channel based on l/u m/(m/s) now hours
 
 !write(10,*) tica
    if (mydata(3,k) <=.0005 .or. mydata(4,k)>.05) then 
        f=0.0253
    else 
        logb=.012*mydata(4,k)/ks !--> scalling for the conversion by 1000
        f=0.24/((log10(logb))**2)
    end if  
 write(*,*) "f=", f   
 !a = (mydata(5,k)/mydata(6,k))/10.765 !conversion from q/v = a(sediment area) ftsq conversion to msq
 a = (mydata(7,k))! new read in without conversion units for are are bankful xsectional area m^2 activate with 56
 write(*,*) "Your cross-sectional area is", a
 !u2 = (mydata(6,k)*mydata(6,k))/10.765
 u2 = (mydata(6,k)*mydata(6,k)) !activate with line 56 as units of velocity are given in m/s
 taub = (rhow_kg*f*u2)/8  !---> in N/m^2 from 77 to change from 
 write(*,*) "The benthic shear stress is for reach is", taub 
!!save taub   
!dx=u*1 !displacement in 1 sec  
!dx_1= u*1*86400 !displacement in 1 day 
!write(10,*) "Total displacement in 1 sec is=" ,dx 
!write(10,*) "Total displacement in 1 day withing reach is=" , dx_1
!tica = [tic*3280.84, ks, f, a, taub, real(k)-1]!---> TIC Conversion from km/(ft/s) to m/s to s, everything has been converted to SI now
tica = [tic, ks, f, a, taub, real(k)-1] ! date]! Activate with 56---> TIC Conversion to m/s to s, everything has been converted to SI now

 write(2,*) tica
 !tmx2 =sum(tica(1:nr))
 !tmx2 =sum(tica(1,1:nr))
END DO
CLOSE(1)
CLOSE(2)
write(10,*) "Start COMID Network", mydata(1,1)
!write(10,*) "End COMID Network", mydata(nr,1)
write(*,*) mydata(1,25), mydata(2,25)
tmx = sum(mydata(2,1:nr))
write(10,*) "Total length of river network (km)", tmx
pdx=0
dy=0
pdy=0
!write(*,*) tmx2
! We are done with the file so now close it out
!CLOSE(1)
!CLOSE(20)
! For fun, let's write back out to standard out
!DO k=1,nr
!    write(*,*) mydata(:,k)
!ENDDO
!print*, "Enter run time max in x seconds, and time increment - will place back after working properly with units (no scalling yet)"
!read (5, *) rtm
    !if (rtm >= tmx2) then 
OPEN(UNIT=2,FILE=outfile, status="old")
ALLOCATE(runtime(6,nr))
DO  k=1,nr
 read(2,*)tc,sfcr,df,area,tb,rid
 runtime(1:6,k)=(/tc,sfcr,df,area,tb,rid/)
 sumcount = 0 
 sumcount = sum(runtime(1,1:k))
 !sumcount = runtime(1,k) !for the tic for each not total overall
 final_j = sumcount +1
 next_row = k+1
 write(10,*) "Total time travel is (inlcuding RID )", sumcount, k
 dx = 0
 dx_1 = 0 
 !dx = mydata(6,k)/3.2804 *1!displacement in 1 sec (m/s) --> update cfs
 !dx = mydata(6,k) *1!displacement in 1 sec (m/s) --> activate with 56
 dx = mydata(6,k)! * 3600!displacement in 1 hr (m/hr) --> activate with 56 05/19/22 updated since already scaled prior
 !dx_1= dx*1*86400 !displacement in 1 day from s 
 dx_1= dx*24 !displacement in 1 day 

 write(10,*) "Total displacement in 1 hour is=" ,dx, runtime(6,k)
 write(10,*) "Total displacement in 1 day withing reach is=" , dx_1, runtime(6,k)
 if (k > 1 .and. j1 >= rtm) then 
    !-----> For the j time step iteration close !Need to place conditional statement
            !if (rtm >= sumcount) then 
    write(10,*) "Runtime has been exceeded and stopped at RID,", runtime(6,k)
    print*, "Do loop interval is", k
    write(10,*) "The time of travel in last COMID is", runtime(1,k)
    !print*, "reach id is", runtime(2,k)
    stp = runtime(6,k)!-1
    write(*,*) stp
    write(10,*) "The sediment travel stopped in COMID", mydata(1,k)!-->if 1 was the comid designation but can extrapolate any paratemer by indexing
    extratime = rtm - sumcount
    write(10,*) "Excess time in reach is (s) =", extratime
    write(10,*) "Remaining time for run to stop is (s) =", lasttimestep
    !lsv = (mydata(6,int(stp))/3.2804)
    lsv = (mydata(6,int(stp))) !Activate with line 56

    write(10,*) "Last stream velocity is (m/s)", lsv
    dxf = lasttimestep*lsv
    write(10,*) "Remaning distance in reach is,", dxf
    !exit !Use these parameters to calc next pdx final displacement in network
    exit
 end if 
 if (k == 1) then 
    init_area = runtime(4,1) !area of first reach for initial mass/concentration calculations
    dlayer= mydata(4,1)/real(ns-1) !can set to (ns-1 for equality in depth of active layers) - based first reach can make this more active later version 
    write(10,*) "layer depth=", dlayer
    print*, "layer depth=", dlayer
    maxlyrd_b=mydata(4,1)+ 0.1
    write(10,*) "Maximum benthic depth set to", maxlyrd_b
    print*, "Maximum benthic depth set to", maxlyrd_b
    !svol= (mydata(5,1)/35.3147)/dlayer  !IN CUBIC METERS for cfs conversion old
    svol= (mydata(5,1))/dlayer  !IN CUBIC METERS activate with 56
    print*, "svol=", svol
    print*, "Enter run time max in x seconds, and time increment" 
    read (5, *) rtm
    write(10,*) "Runtime maximum is set at", rtm 
 !Asssuming discharge from the initial reach
 !end if   
 !Old program starts 
    print*, "Would you like to choose (1) Legacy, (2) Descriptive, or (3) Van Rijn transport?" !or (4) Roberts (erosion) transport"
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
                allocate(state_var_time_2, SOURCE=state_var) 
                !state_var_time_2=state_var(1:6)
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
                fc = freq_count(:)
                tot_mass= state_var_time_1(2)+state_var_time_2(2)+state_var_time_3(2)
                tot_rhos=state_att_1(4,1)+state_att_1(4,2)+state_att_1(4,3)
                tot_vol=tot_mass/tot_rhos
                write(10,*) "Your tot_mass is,=",tot_mass
                print*, "Your tot_mass is,=",tot_mass
                write(10,*) "Your total sed volume,=", tot_vol
                print*, "Your total sed volume,=", tot_vol
                cumm_freq(:) = 0 
                cumm_freq = (fc/ns)*100 !As percentage
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
        cumm_freq(:) = 0 
        cumm_freq = (fc/ns)*100 !As percentage
    end if
end do
    else if (so==3) then 
    do i=1,ns
    !fc(:)=0
    !lnumb= i*1 - place statement here for else when i==ns-- down below
        if (i<ns) then 
        maxlyrd=dlayer* real(i)
        call passed2(state_var,i,m_1,maxlyrd,tsf,pdx,pdy,state_att,a,freq_count,tb,dprob)!taub
           if (i==1) then
            allocate(state_var_time_1, SOURCE=state_var) 
            !state_var_time_1=state_var(1:4)
            state_att_1(1:6,i) =state_att(1:6)
            fc = freq_count(:)
            else 
            allocate(state_var_time_2, SOURCE=state_var) 
            !state_var_time_2=state_var(1:4)
            state_att_1(1:6,i) = state_att (1:6)
            end if 
            else if( i==ns) then 
            call passed3 (state_var,i,m_1,maxlyrd_b,tsf,pdx,pdy,state_att,a,freq_count,tb,dprob)!taub
             !maxlyrd_b=h+ 0.1
    !This is where the new type of memory allocation should begin using MALLOC AND MOVE_ALLOC
            allocate(state_var_time_3, SOURCE=state_var) 
            !state_var_time_3=state_var(1:6)
            state_att_1(1:6,i)=state_att(1:6)!These statements are redundant and can be in on place after the if so. 
            write(10,*) state_att_1(1:6,i)
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
    !end if
    !print*, cumm_freq    
    !Time step iteratin begins 
!    mad = [real ::] !create an empty array [] for mass adjustment on the fly 
!    mad_1 = [real ::]
!    mad_b = [real ::]
!    tot_loss_ly1 = [real ::] !create an empty array [] for total mass loss summation to be used with mass balance on the fly 
!    tot_loss_ly2 = [real ::] 
!    tot_loss_ly3 = [real ::]
!    ml_ly1 = [real ::] !create an empty array [] for finite difference on the fly 
!    ml_ly2 = [real ::]
!    ml_ly3 = [real ::]
  end if !---->going to need the information from J1 and new_end to continue iteration from a nested loop calculating these same things
! start_j = 1
 end if
    if (k==1) then 
    start_j = 1
    else
    start_j = J1
    end if
        do j= start_j,nint(sumcount)!int(final_j)!,tinc
            if (j == rtm) then
            exit
!            !-----> For the j time step iteration close !Need to place conditional statement
!            !if (rtm >= sumcount) then 
!            write(10,*) "Runtime has been exceeded and stopped at,", runtime(4,k)
!            print*, "Do loop interval is", k
!            write(10,*) "The time of travel in last COMID is", runtime(1,k)
!            !print*, "reach id is", runtime(2,k)
!            stp = runtime(4,k)!-1
!            write(*,*) stp
!            write(*,*) mydata(1,int(stp))!-->if 1 was the comid designation but can extrapolate any paratemer by indexing
!            extratime = rtm - sumcount
!            write(10,*) "Excess time in reach is (s) =", extratime
!            write(10,*) "Remaining time for run to stop is (s) =", lasttimestep
!            lsv = (mydata(6,int(stp))/3.2804)
!            write(10,*) "Last stream velocity is (m/s)", lsv
!            dxf = lasttimestep*lsv
!            write(10,*) "Remaning distance in reach is,", dxf
!            exit !Use these parameters to calc next pdx final displacement in network
        end if 
        print*, "Time increment is" , j-1
        pdx=(dx*real(j-1)) !Since this is in scope at all times in, not needed to include in allocated space. Just need to print out 
        tsv_1 =state_var_time_1(3)/(1000*state_att_1(4,1))!Total Settling Velocity LY1
        tsv_2 =state_var_time_2(3)/(1000*state_att_1(4,2))!Total Settling Velocity LY2
        tsv_3 =state_var_time_3(3)/(1000*state_att_1(4,3))!Total Settling Velocity LY3
        d_m_1 =state_var_time_1(2)/dx_1 !MSF/dx rate of change for LY1
        d_m_2 =state_var_time_2(2)/dx_1 !MSF/dx rate of change for LY2
        d_m_3 =state_var_time_3(2)/dx_1 !MSF/dx rate of change for LY3
        allocate(mad(j))
        !mad = [mad,real(j)]
        allocate(mad_1(j))
        !mad_1 = [mad_1,real(j)]
        allocate(mad_b(j))
        !mad_b = [mad_b,real(j)]
        allocate(tot_loss_ly1(j))
        !tot_loss_ly1 = [tot_loss_ly1,real(j)]
        allocate(tot_loss_ly2(j))
        !tot_loss_ly2 = [tot_loss_ly2,real(j)]
        allocate(tot_loss_ly3(j))
        !tot_loss_ly3 = [tot_loss_ly3,real(j)]
        allocate(ml_ly1(j))
        !ml_ly1 = [ml_ly1, real(j)]
        allocate(ml_ly2(j))
        !ml_ly2 = [ml_ly2, real(j)]
        allocate(ml_ly3(j))
        !ml_ly3 = [ml_ly3, real(j)]
        !if (j==1) then
        !    print*, tsv_1,tsv_2,tsv_3,d_m_1,d_m_2,d_m_3
        !end if 
        allocate(timeout(3,rtm))
        timeout(1,j)=(state_var_time_1(4)-dlayer)+(tsv_1*real(j-1)) !y displacement
        timeout(2,1)=state_var_time_1(2)-(d_m_1*pdx)!change in mass flux
        timeout(3,1)=timeout(2,j)/svol!concentration scaling factor
        timeout(2,2:j)= mad(j-1)-(d_m_1*pdx)!change in mass flux
        timeout(3,2:j)=timeout(2,j)/svol!concentration scaling factor in mg/L
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
        benthic_layer_3(3,j)= benthic_layer_3(2,j)/svol!concentration scaling factor in mg/L
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
        else if(timeout_1(2,j)<0) then 
            timeout_1(2,j) =0
        else if (benthic_layer_3(2,j)<0) then 
            benthic_layer_3(2,j) = 0 
            write(10,*) "Model simmulation completed at", j, "seconds. Terminate model run."
            exit
        end if        
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
                benthic_layer_3(3,j)=benthic_layer_3(2,j)/q !Need to back refrence /q
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
        deallocate(mad)
        deallocate(mad_1)
        deallocate(mad_b)
        deallocate(tot_loss_ly1)
        deallocate(tot_loss_ly2)
        deallocate(tot_loss_ly3)
        deallocate(ml_ly1)
        deallocate(ml_ly2)
        deallocate(ml_ly3)
        J1 = J +1
        !next_time = (mydata(2,next_row)/mydata(6,next_row))*3280.84
        next_time = (mydata(2,next_row)/mydata(6,next_row))! *Use with debugging;this is tic for next iter but has outputfile with info; in SI not scaled activate with line 56 (next time step travel not using anywhere else)

        !J1 = J + 1
         !if(benthic_layer_3(2,j)<0) then
           ! write(10,*) "No more sediment in model at",  j ,  "seconds. Simmulation complete."
            !exit
        !end if 
        if (j>sumcount) then 
        new_end_step = nint(sumcount + next_time)
        write(10,*) J1, new_end_step
        !cycle
        exit
!        70 continue

        end if
!            do P:J1,new_end_step
!                dx = 0
!                dx = mydata(6,k+1)/3.2080 *1!displacement in 1 sec 
!                dx_1= dx*1*86400 !displacement in 1 day 
!                write(10,*) "Total displacement in 1 sec is=" ,dx, runtime(6,k+1)
!                write(10,*) "Total displacement in 1 day withing reach is=" , dx_1, runtime(6,k+1)
!            end do 
    end do !-----> For the j time step iteration close !Need to place conditional statement
!        if (sumcount>= rtm) then
!        !if (rtm >= sumcount) then 
!        write(10,*) "Runtime has been exceeded and stopped at,", runtime(4,k)
!        print*, "Do loop interval is", k
!        write(10,*) "The time of travel in last COMID is", runtime(1,k)
!        !print*, "reach id is", runtime(2,k)
!        stp = runtime(4,k)!-1
!        write(*,*) stp
!        write(*,*) mydata(1,int(stp))!-->if 1 was the comid designation but can extrapolate any paratemer by indexing
!        extratime = rtm - sumcount
!        write(10,*) "Excess time in reach is (s) =", extratime
!        write(10,*) "Remaining time for run to stop is (s) =", lasttimestep
!        lsv = (mydata(6,int(stp))/3.2804)
!        write(10,*) "Last stream velocity is (m/s)", lsv
!        dxf = lasttimestep*lsv
!        write(10,*) "Remaning distance in reach is,", dxf
!        exit !Use these parameters to calc next pdx final displacement in network
!    end if 
        !go to 62
        !print*, "reach id is", runtime(2,k)
 !write(*,*) time
  !write(*,*) runtime(3,47), runtime(4,25)
  !write(*,*) tc(47), rid(25)
!end if
end do 
close(2)
write(*,*) mydata(1,1), mydata(2,1)
write(*,*) mydata(1,25), mydata(2,25)
write(*,*) tica(1:6)!Only last parameter but reads in all in the loop
write(*,*) tmx
tmx2 = sum(runtime(1,1:nr))
write(*,*) tmx2
write(*,*) runtime
write(*,*) stp

write(*,*) runtime(3,47), runtime(4,25)
!write(*,*) tc(47), rid(25)


!DO k=1,nr
!ENDDO
write(65,*) mydata(1,50), mydata(7,50)

close(65)    
close(55) 
close(45)    
close(35)
close(25)
close(15)
call cpu_time(finish)
!close(30)
write(10,*)"Total run time=",finish-start,"seconds"
close(10)    
end program newinput