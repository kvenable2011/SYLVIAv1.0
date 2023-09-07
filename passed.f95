subroutine passed (state_var,state_att)!state_var, i, rhos, tsf, maxlyrd, dx, pdx, pdy) 
real, dimension (7) ::state_var
real, dimension (6) ::state_att
integer::comid, yn, so, seg,lnumb
logical::y
real :: om, sm, gs, lw, l, PCTWA, rhob, rhod, choice, sds, ds, dds, dv, dprob,dvf,d1, d2, d3, d4, d5, d6, d7, tmxc, tmx,dlayer
real :: sfrs, rsv, f, ks, md, grho, ks1, ks2, rd, vs2, y1,drho, vs1, sf, gp, x, ws,ch, taub1, um, taub, sv, ts, area, tsf, rsf, lc
real :: pdx, pdy, maxlyrd, dx, d8, d9, d10, d11, st, dy, ddtb,db, m, freq_count,dm, vol  
real, parameter:: rhow=.998, g=9.807, vis=.001
real, pointer:: x0
integer, parameter::ns=3
real, target:: rhos
integer :: i,j,irow, rtm, tinc, ww
real:: n, h, da,u 
                j=0
                !maxlyrd=dlayer* real(i)
                call grainsz (rhos, ds, m, ww)
                call location(seg)
                call sedflux (tsf,sf,rsf, rsv)
                pdx=dx +(u*real(j))
                pdy=dy+((tsf/rhos)*real(j))
                vol=m/rhos
                freq_count=0
                !allocate(state_var(7,i))
                state_var(1:7)= (/real(i), rhos, tsf, maxlyrd, dx, pdx, pdy/)
                print*, state_var
                state_att(1:6)= (/real(i), real(seg),ds, m, vol, dh/) 
                print*, state_att
                !deallocate (state_var)
end subroutine passed                 