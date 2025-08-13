subroutine relocate (conc_1,conc_2,conc_3)
integer:: j
real:: tmx
real :: conc_time_1, conc_time_2, conc_time_3
real, dimension(3):: output_mass
real, dimension (:)::conc_1,conc_2,conc_3
conc_1(j)=output_mass(1)
conc_2(j)=output_mass(2)
conc_3(j)=output_mass(3)
print*, conc_1(:)
print*, conc_2(:)
print*, conc_3(:)
end subroutine relocate
