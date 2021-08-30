subroutine probofdep (dprob)
implicit none
real ::taucd1, taucd2, pd, yn, taub
real,intent(inout):: dprob
   logical :: y
   !Need to include pass though for defaults and user 
   print*, "Would you like to enter a value for (taucd1), enter any number; for the default press (1) for no?"
    y= .true. 
        Do while(y)
            if(yn==1) then 
            y=.false. 
            taucd1= 0.0
            print*, "LCSS is set at 0.0 [N/m^2]"
            else  
            print*, "Enter your lcss"
                read*, taucd1
            exit
            end if 
         end do       
         
   print*, "Would you like to enter a value for (taucd2), enter any number; for the default press (1) for no?"
        Do while(y)
            if(yn==1) then 
            y=.false. 
            taucd2= 0.2
            print*, "UCSS is set at 0.2 [N/m^2]"
            else  
            print*, "Enter your ucss"
                read*, taucd2
            exit
            end if 
         end do             
 
    !call mec1(taub)
        !if (taub<taucd1) then   1963---->Old Krone
        !dprob=1
        !else if (taub>taucd2) then
        !dprob=0
        !else 
print*, "Would you like to enter a value for DE, enter any number; for the default press (1) for no?"
        read*, yn
                    Do while(y)
                        if(yn==1) then 
                            y=.false. 
                            pd= 1.0
                            print*, "Dimensionless exponent is set at 1.0 [N/m^2]"
                        else 
                            print*, "Enter your dimensionless exponent."
                            read*, pd
                            exit
                        end if 
                    dprob = ((taucd2 - taub)/(taucd2-taucd1))**pd                    
                    print*, "Deposition probability", dprob
                    end do 
!dprob = ((taucd2 - taub)/(taucd2-taucd1))**pd                    
!print*, "Deposition probability", dprob
end subroutine probofdep
