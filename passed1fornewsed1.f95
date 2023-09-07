subroutine passed1(state_var,i,rhos,sv, maxlyrd,state_att,a,freq_count) 
implicit none 
real, dimension (7)::freq_count, fc 
real, dimension (4)::state_att
real, dimension (5)::state_var
real, dimension (:,:), allocatable::benthic_layer_3 
real, dimension (5) :: int_con
integer::comid, yn, so, seg,lnumb
logical::y
real :: om, sm, gs, lw, l, PCTWA, rhob, rhod, choice, sds, ds, dds, dv, dprob,dvf,d1, d2, d3, d4, d5, d6, d7, tmxc, tmx,dlayer
real :: sfrs, rsv, f, ks, md, grho, ks1, ks2, rd, vs2, y1,drho, vs1, sf, gp, x, ws,ch, taub1, um, taub, sv, ts, a, tsf, rsf, lc
real :: pdx, pdy, maxlyrd, dx, d8, d9, d10, d11, st, dy, m, m_1, vol, dh, dm, ww, boulder, cobble, pebble, granule, sand, silt, clay    
real :: svol, xks, load_i, xterm, xt, dt,lpt, q, conc_i, dx_1, conc_a, conc_ss, num_par,d_m
real :: num_par_tot, num_par_1, advf_x, d_conc_i, mb_conc, d_m_i
real, parameter:: rhow=.998, g=9.807, vis=.001
real, pointer:: x0
integer, parameter::ns=3
real, target:: rhos
integer :: i,j,irow, rtm, tinc
real :: n, h, da,u
j=0
call location (seg)
call sedflux2 (rhos,ds,sv,tsf,freq_count)!(rhos,ds,m,sv,rsf,tsf,freq_count, boulder, cobble, pebble, granule, sand, silt, clay)
!load_i = (tsf * area) !Has an assumption that 1 day is multiplied to get the concentration 
!print*, load_i 
!conc_i = (load_i/svol)!Need To convert to mg/L from kg/m^3 
!print*, conc_i 
!advf_x = -(tsf/dx) !Change in transport concentration over time through flux over time 
!This will need to be calculated at each area/discharge change every hour, place conditional statement to adjust for this 
!d_conc_i = -(conc_i/dx) !Change in transport concentration over time through flux over time
!call asettle (conc_ss, conc_a, advf_x)
!maxlyrd=dlayer* real(i)
!pdx=dx +(u*real(j))
!pdx=0
!pdy=0 
m_1 = tsf * a!-times 1 day in g/d*m^2 For coversion scaling conversion for tsf cm^2 to m^2. Has an assumption that 1 day is multiplied to get the concentration 
print*, m_1 
!d_m = 0
!d_m_i = 0 !Change in transport concentration over time through flux over time 
!print*, d_m_i
!conc_i = (tsf/(dx_1*10**6)) !converted to g/ml 
!print*, conc_i 
!d_conc_i = 0
!print*, d_conc_i
!num_par_1 = 0 !Should this be q volume or area volume and may need conversion 
!vol=m/rhos     
!pdy=dy+((tsf/rhos)*real(j))
!allocate(state_var(7,i
!m_1=rhos*svol
!print*, m_1
!state_var(1:9)= (/real(i), m_1, sv, maxlyrd,tsf, pdx, pdy, d_m, conc_i/)
!state_att(1:7)= (/a, ds,real(seg),d_m_i,d_conc_i,rhos,num_par_1/)
state_var(1:5)= (/real(i), m_1, sv, maxlyrd,tsf/)
print*, state_var
state_att(1:4)= (/a, ds,real(seg),rhos/)
print*, state_att
!print*, freq_count
end subroutine passed1