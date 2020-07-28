subroutine benthic (state_var,i,m_1,maxlyrd_b,tsf,pdx,pdy,state_att,a,freq_count)
implicit none 
real :: freq_count(7), state_att(4), state_var(6)
logical::y
real, parameter:: rhow=.998, g=9.807, vis=.001, db=0.010
integer :: i,seg, yn
real :: m_1, tsf, maxlyrd_b, pdx, pdy,a,ds,rhob,rhos
print*,"Would you like to include an initial solids concentration in the surface benthic layer? &
Enter any number for Yes and (1) for No"
read (5,*) yn
                    y= .true. 
                        Do while(y)
                            
                                if(yn==1) then 
                                    y=.false. 
                                    rhos = 0
                                    ds = 0 
                                else  
                                    y= .true. 
                                    exit
                                end if 
                        end do   

call location(seg) 
call sedflux3(rhob,ds,tsf,freq_count)
m_1 = tsf * a!-times 1 day in g/d*m^2 For coversion scaling conversion for tsf cm^2 to m^2. Has an assumption that 1 day is multiplied to get the concentration 
print*, m_1 
state_var(1:6)=(/real(i), m_1, tsf, maxlyrd_b, pdx, pdy/)
write(10,*) state_var
print*, state_var                      
state_att(1:4)= (/a, ds,real(seg),rhob/)  
print*, state_att                      
end subroutine benthic                        