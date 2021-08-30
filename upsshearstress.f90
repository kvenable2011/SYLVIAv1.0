subroutine mec1(taub)
implicit none
!calculation of benthic shear stress excluding changing depth 
real :: ks1, ks2, da, h, ks, f, rhow_kg, taub, u
   !Print *, "Enter water depth (m), median diameter size (m), and water velocity (h, m, m/s)"
   !read *, h, da, u
   rhow_kg = 998
   ks1 = 3*da
   ks2 = 0.01 *h
   ks = max(ks1,ks2) 
   print *," ks=", ks
        if (da <=.0005 .or. h>.05) then 
        f=0.0253
        else 
        f=0.24/((log10(12*h/ks))**2)
        end if 
   taub=rhow_kg*f*(u**2)
   print*, "Benthic Shear Stress =", taub
! End of benthic shear stress calc
! using the van Rijn method for settling velocity
end subroutine mec1