subroutine ma (mad, m1, m2, m3)
real, dimension(:,:) ::mad(3,1)
real, allocatable ::timeout_1(:,:),timeout(:,:),benthic_layer_3(:,:)
real:: m1, m2, m3
m1 = timeout(2,j)
m2 = timeout_1(2,j)
m3 = benthic_layer_3(2,j)
mad(1,1) = m1
mad(2,1) = m2
mad(3,1) = m3
print*, mad 
end subroutine ma