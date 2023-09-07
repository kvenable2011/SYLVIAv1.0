program network2
character:: reaches(5)*16
integer:: n_r, i
real:: wchar(8,3)
real::l,md,d,q,u, tss
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
            !allocate(wchar(6,real(n_r)))
            wchar(1:6,i)= (/l,md,d,q,u,tss/)
            print*, "Enter your water characteristics"
            read(5,*) l,md,d,q,u,tss
    print*, wchar(:,i)
    !deallocate(:,i)
    end do 
print*, reaches
print*, wchar
end program network2