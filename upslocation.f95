subroutine location (seg)
implicit none
integer:: seg                                            
print *, "Choose your water segment type for your solid"
        print*, "1  surface water"
        print*, "2  water column"
        print*, "3  surface benthic"
        print*, "4  sub surface benthic"
        read (5,*) seg
select case(seg)
case (1)
print *, "Sfc Water Segment"
case (2)
print*, "Water Column Segment"
case (3)
print*,"Surface Benthic Segment"
case (4) 
print*, "Subsfc Benthic Segment"
case(5:)
print*,"You  have chosen an invalid selection, please select a valid selction"
    read (5,*) seg
case default !Need to make sure this works may not be able to call the subroutine in the subroutine
end select
end subroutine location    