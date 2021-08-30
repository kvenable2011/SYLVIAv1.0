subroutine grainsz (rhos,ds)
implicit none
real :: rhos, ds, dds, dm, taub, dprob
real, parameter:: om=1.270, sm=2.650, gs=4.000 !g/cm^3 - Changed to reflect instead of kg/m^3  
logical:: y
integer:: yn, choice, so
print*, "Choose your particle type and a density will be entered"
        print*, "1  Organic Matter"
        print*, "2  Siliceous Minerals"
        print*, "3  Garnett Sands"
        print*, "4  Enter density in g/mL"
         read (5, *) choice
            if (choice==1) then 
            rhos=om
            else if (choice==2) then
            rhos=sm
            else if (choice==3) then 
            rhos=gs
            else  
            read (5,*) rhos
            end if
          !Incorporates user defaults 
           Print *,"Would you like to enter a grain size, 'any number' for yes & '1' for no (defaults @ 0.025 mm)"
                    read (5,*) yn
                    y= .true. 
                        Do while(y)
                                    if(yn==1) then 
                                    y=.false. 
                                    ds=0.000025
                                    print*, "Grain size default is .025 mm"
                                    else  
                                        y= .true. 
                                    print*, "Enter your grain size in m"
                                    read(5,*) dds
                                    ds=dds
                                        exit
                                    end if 
                        end do
!if (so==3) then
    !call mec1(taub)
    !call probofdep(dprob)
!end if                         
end subroutine grainsz                            