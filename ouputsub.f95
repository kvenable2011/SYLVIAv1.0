subroutine outmat (report)
integer:: j
real :: conc_time_1, conc_time_2, conc_time_3
real, dimension(3):: output_mass
real, dimension (3):: report
!allocate(report(:))
output_mass(1) = report(1) 
output_mass(2) = report(2)
output_mass(3) = report(3) 
print*, report
!output_mass(4,1) = report(4,j)
!print*, report
!deallocate
end subroutine outmat