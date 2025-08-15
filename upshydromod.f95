module hydro
    implicit none
    real::h, da,a,lc, q, u, ks1, ks2, ks, logb, taub, dx, dx_1, f,dprob
    integer:: i,j
    !real,intent(out):: taub
    real,parameter::rhow_kg=998
    save

!contains
!    subroutine input()
!    end subroutine input
end module hydro