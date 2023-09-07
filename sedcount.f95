subroutine graintype (freq_count,boulder, cobble, pebble, granule, sand, silt, clay)
implicit none
real :: rhos,choice, ds, dds,m, dm, boulder, cobble, pebble, granule, sand, silt, clay
real, dimension (7) :: freq_count
real, parameter:: om=1.270, sm=2.650, gs=4.000 !kg/m^3 - changed back to (g/ml) and need to scale up on stokes settling by 10^**3   
logical:: y
boulder= 0
cobble= 0
pebble = 0 
granule = 0  
sand = 0 
silt = 0 
clay = 0 
!freq_count (:) = 0 
freq_count=(/boulder, cobble, pebble, granule, sand, silt, clay/)
                        
if (ds>=0.256) then 
freq_count(1)= 1
print*, "Your solids type is a boulder"
else if(ds>=0.064 .and. ds<0.256) then
freq_count(2)= 1
print*, "Your solids type is a cobble"
else if (ds>=0.004 .and. ds<0.064) then 
freq_count(3)= 1
print*, "Your solids type is a pebble"
else if (ds>=0.002 .and. ds<0.004) then 
freq_count(4)= 1
print*, "Your solids type is a granule"
else if (ds>=0.001 .and. ds<0.002) then
freq_count(5)= 1
print*, "Your solids type is a sand"
else if (ds>=0.000625 .and. ds<0.001) then
freq_count(6)= 1
print*, "Your solids type is a silt"
else
freq_count(7)= 1
print*, "Your solids type is a clay"
end if             
print*, freq_count
end subroutine graintype