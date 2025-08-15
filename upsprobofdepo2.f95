subroutine probofdep(dprob)
implicit none
real ::taucd1, taucd2, pd, t2, t1, b, dprob,tb !taub 
logical :: y
integer :: yn
common /shear/tb !taub
!real, intent(in):: taub
!real, intent(inout)::taub
!taub=tb
   !Need to include pass though for defaults and user 
    print*, "Would you like to enter a value for (taucd1), enter any number; for the default press (1) for no?"
    read(5,*) yn
    y= .true. 
        Do while(y)
        if(yn==1) then 
        y=.false.
        taucd1= 0.0
        print*, "LCSS is set at 0.0 [N/m^2]"
        write(10,*) "LCSS is set at 0.0 [N/m^2]"
            else 
            y= .true. 
                print*, "Enter your lcss"
                    read(5,*) taucd1
                Print*, "LCSS is set at", taucd1, "[N/m^2]."
                write(10,*) "LCSS is set at", taucd1, "[N/m^2]."
                exit
            end if 
         end do       
         
    print*, "Would you like to enter a value for (taucd2), enter any number; for the default press (1) for no?"
    read(5,*) yn 
    y= .true. 
       Do while(y)
            if(yn==1) then 
            y=.false. 
            taucd2= 0.2
                print*, "UCSS is set at 0.2 [N/m^2]."
                write(10,*) "UCSS is set at 0.2 [N/m^2]"
            else 
            y= .true. 
                print*, "Enter your ucss"
                    read(5,*) taucd2
                    write(10,*) "UCSS is set at", taucd2, "[N/m^2]."
                    print*, "UCSS is set at", taucd2, "[N/m^2]."
            exit
            end if
!            t2 = taucd2 - taub
!            t1 = taucd2 - taucd1
!            print*, "Your upper critical threshold difference and threshold range is", t2, t1
        end do  
t2 = taucd2 - tb!taub
t1 = taucd2 - taucd1
b = t2/t1
print*, "Your upper critical threshold difference and threshold range is", t2, t1

    print*, "Would you like to enter a value for (pd), enter any number for yes; for the default press (1) for no?" 
    read (5,*) yn
    y= .true.
        Do while(y)
        if(yn==1) then 
        y=.false. 
        pd= 1.0
        print*, "Dimensionless exponent is set at 1.0"
        write(10,*) "Dimensionless exponent is set at 1.0"
            else 
            y= .true.
                print*, "Enter your dimensionless exponent."
                    read(5,*) pd
                    print*, "Dimensionless exponent is set at", pd
                    write   (10,*) "Dimensionless exponent is set at", pd
                    exit
            end if 
        end do    
dprob = b**pd 
!dprob = ((taucd2 - taub)/(taucd2-taucd1))**pd
print*, "Your deposition probability for the system is", dprob, "[N/m^2]."                    
write(10,*)"Your deposition probability for the system is", dprob, "[N/m^2]."  
  
!end do                   
end subroutine probofdep
