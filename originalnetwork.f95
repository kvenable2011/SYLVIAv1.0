program network0
character:: reaches(5)*16
integer:: n_r, i
!real::comid
!We will need a way to identify the count of the COMIDs from the STREAMCAT or NHD+ segments i.e .count 
character(len=16):: comid
print*, "How many COMIDs are in the streamflow network?"
read (5,*) n_r
    do i=1,n_r
    !comid = reaches(i)
            print*, "Enter your COMID"
            read (5,*) comid
        reaches(i) = comid
        
    end do 
print*, reaches
end program network0