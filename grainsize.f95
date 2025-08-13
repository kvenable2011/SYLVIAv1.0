subroutine grainsz (rhos,ds,m)
implicit none
real :: rhos,choice, ds, dds,m, dm
real, parameter:: om=1.270, sm=2.650, gs=4.000 !g/cm^3 - Changed to reflect instead of kg/m^3  
logical:: y
integer:: yn, ww 
!select case(ww)
        !case(1)
        !ww = 1
        !print*, "Wentworth classification is a boulder"
        !case(2)
        !ww = 2
        !print*, "Wentworth classification is a cobble"
        !case(3)
        !ww = 3
        !print*,"Wentworth classification is a pebble"
        !case(4)
        !ww = 4
        !print*, "Wentworth classification is a granule"
        !case(5) 
        !ww = 5
        !print*, "Wentworth classification is a sand"
        !case(6)
        !ww = 6
        !print*, "Wentworth classification is a silt"
        !case default
        !Print*, "Wentworth classification is a clay"
! end select
!case default 
  


print*, "Choose your particle type and a density will be entered"
        print*, "1  Organic Matter"
        print*, "2  Siliceous Minerals"
        print*, "3  Garnett Sands"
        print*, "4  Enter density in g/mL"
         read (5, *) choice
            if (choice ==1) then 
            rhos=om
            else if (choice ==2) then
            rhos=sm
            else if (choice ==3) then 
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

              !Print *,"Would you like to enter a mass for your solid?, press 'any number' for yes & '1' for no (defaults @ 100g)"
                    !read (5,*) yn
                    !y= .true. 
                        !Do while(y)
                                    !if(yn==1) then 
                                    !y=.false. 
                                    !m=100
                                    !print*, "Your solids mass default is 100 grams"
                                    !else  
                                        !y= .true. 
                                    !print*, "Enter your mass for the solid"
                                    !read(5,*) dm
                                    !m=dm
                                    !    exit
                                    !end if 
                        !end do  
                        
end subroutine grainsz                            