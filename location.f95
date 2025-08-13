subroutine location (seg)
implicit none
integer:: seg                                        
select case(seg)
case (1)
print *, "Sfc Water Segment"
case (2)
print*, "Water Column Segment"
    if (seg==1) then
            print*, "Invalid selection and choose again 2-4." 
            print*, "2  water column"
            print*, "3  surface benthic"
            print*, "4  sub surface benthic"
            read (5,*) seg
    end if 
case (3)
print*,"Surface Benthic Segment"
    if (seg==1 .or. seg==2) then
    print*, "Invalid selection and choose again 3-4."
    print*, "3  surface benthic"
    print*, "4  sub surface benthic"
    read (5,*) seg
    end if 
case (4) 
print*, "Subsfc Benthic Segment"
    if (seg==1 .or. seg==2 .or. seg==3) then
    print*, "Invalid selection. You may only use 4."
    print*, "4  sub surface benthic"
    read (5,*) seg
    end if          
case default
    if (seg >5) then
    Print*, "Invalid" 
    end if 
end select    
print *, "Choose your water segment type for your solid"
        print*, "1  surface water"
        print*, "2  water column"
        print*, "3  surface benthic"
        print*, "4  sub surface benthic"
        read (5,*) seg
end subroutine location         
            !Do while(seg==2)
                 !if (seg==1) then
            !print*, "Invalid selection and choose again 2-4." 
            !goto 10 
            !end if 
            !end do
            !Do while(seg==3)
              !if (seg==1 .or. seg==2) then
           !print*, "Invalid selection. You may only use 3 or 4."
           !goto 10 
           !end if 
           !end do 
           !Do while(seg==4)
              !if (seg==1 .or. seg==2 .or. seg==3) then
           !print*, "Invalid selection. case default
        !Print*, "Invalid""
          ! goto 10 
           !end if 
           !end do 
    !select case(seg)
        !case (1)
        !print *, "Sfc Water Segment"
        !case (2)
        !print*, "Water Column Segment"
           !if (seg==1) then
           !print*, "Invalid selection and choose again 2-4." 
           !goto 10 
           !end if
        !case (3)
        !print*,"Surface Benthic Segment"
           !if (seg==1 .or. seg==2) then
           !print*, "Invalid selection. You may only use 3 or 4."
           !goto 10 
           !end if 
        !case (4) 
        !print*, "Subsfc Benthic Segment"
           !if (seg==1 .or. seg==2 .or. seg==3) then
           !print*, "Invalid selection. You may only use 4."
           !go to 10 
           !end if 
        !case default
        !Print*, "Invalid"
        !goto 10
    !end select 
!end subroutine location    