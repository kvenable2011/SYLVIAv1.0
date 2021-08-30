subroutine probofdep1o(dprob)
implicit none
real :: taucd1, taucd2, pd, yn, tb, taub
logical :: y
real,intent(inout):: dprob
   !Need to include pass though for defaults and user 
print*, "Would you like to enter a value for (taucd1), enter any number; for the default press (1) for no?"
    y= .true. 
        Do while(y)
            if(yn==1) then 
                y=.false. 
                taucd1= 0.0
                write(10,*) "LCSS is set at 0.0 [N/m^2]"
            else  
                print*, "Enter your lcss"
                read*, taucd1
                write(10,*) "LCSS is set at", taucd1, "[N/m^2]."
                exit
            end if 
         end do       
         
   print*, "Would you like to enter a value for (taucd2), enter any number; for the default press (1) for no?"
        Do while(y)
            if(yn==1) then 
                y=.false. 
                taucd2= 0.2
                write(10,*) "UCSS is set at 0.2 [N/m^2]"
            else  
                print*, "Enter your ucss"
                read*, taucd2
                write(10,*) "UCSS is set at", taucd2, "[N/m^2]."
            exit
            end if 
         end do             
if (taub<taucd1) then  
    dprob=1
else if (taub>taucd2) then
    dprob=0
end if 
write(10,*) " Your deposition probability for the system is", dprob, "[N/m^2]."           
end subroutine probofdep1o
