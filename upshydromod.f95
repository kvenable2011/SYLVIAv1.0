module hydro
implicit none
    real::h, da, lc, q, u, ks1, ks2, ks, logb, dprob,taub
    !real,intent(out):: taub
    real,parameter::rhow_kg=998
    integer:: dpc,so

contains
    subroutine input()
    end subroutine input
end module hydro