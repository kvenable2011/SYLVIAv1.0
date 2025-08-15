program add mass
integer: so, mi
print*, "Enter the number of mass injections you would like to have during the simulation?"
read*, mi
print*, "Enter your sedmintation option?" 
read*, so

    do i = 1, mi
        if (so==1) then    
        call passed(state_var,i,m_1,maxlyrd,tsf,pdx,pdy,state_att,a,freq_count) 
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
!                state_att_1(1:4,i)=state_att(1:4)
!                fc = freq_count(:)
!                tot_mass= state_var_time_1(2)+state_var_time_2(2)+state_var_time_3(2)
!                tot_rhos=state_att_1(4,1)+state_att_1(4,2)+state_att_1(4,3)
!                tot_vol=tot_mass/tot_rhos
!                write(10,*) "Your tot_mass is,=",tot_mass
!                print*, "Your tot_mass is,=",tot_mass
!                write(10,*) "Your total sed volume,=", tot_vol
!                print*, "Your total sed volume,=", tot_vol
!                cumm_freq(:) = 0 
!                cumm_freq = (fc/ns)*100 !As percentage
              end if
            end do 
            else if (so==2) then 
        do i=1,ns
        !fc(:)=0
        !lnumb= i*1 - place statement here for else when i==ns-- down below
        if (i<ns) then 
        !maxlyrd=dlayer* real(i) 
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
!        fc = freq_count(:)
!        tot_mass= state_var_time_1(2)+state_var_time_2(2)+state_var_time_3(2)
!        tot_rhos=state_att_1(4,1)+state_att_1(4,2)+state_att_1(4,3)
!        tot_vol=tot_mass/tot_rhos
!        write(10,*) "Your tot_mass is,=",tot_mass
!        print*, "Your tot_mass is,=",tot_mass
!        write(10,*) "Your total sed volume,=", tot_vol
!        print*, "Your total sed volume,=", tot_vol
!        cumm_freq(:) = 0 
!        cumm_freq = (fc/ns)*100 !As percentage
    end if
    else if (so==3) then 
    do i=1,ns
    !fc(:)=0
    !lnumb= i*1 - place statement here for else when i==ns-- down below
        if (i<ns) then 
        !maxlyrd=dlayer* real(i)
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
!            !state_var_time_3=state_var(1:6)
!            state_att_1(1:6,i)=state_att(1:6)!These statements are redundant and can be in on place after the if so. 
!            write(10,*) state_att_1(1:6,i)
!            fc = freq_count(:)
!            tot_mass= state_var_time_1(2)+state_var_time_2(2)+state_var_time_3(2)
!            tot_rhos=state_att_1(4,1)+state_att_1(4,2)+state_att_1(4,3)
!            tot_vol=tot_mass/tot_rhos
!            write(10,*) "Your tot_mass is,=",tot_mass
!            print*, "Your tot_mass is,=",tot_mass
!            write(10,*) "Your total sed volume,=", tot_vol
!            print*, "Your total sed volume,=", tot_vol
!        !cumm_freq(:) = 0 
!        !cumm_freq = (fc/ns)*100 !As percentage
!        end if
!        cumm_freq(:) = 0 
!        cumm_freq = (fc/ns)*100 !As percentage
!        !print*, cumm_freq
    end do
    end do 
end add mass 