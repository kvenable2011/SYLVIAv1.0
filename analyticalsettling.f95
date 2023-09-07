subroutine asettle (conc_ss, conc_a, advf_x)
implicit none
real :: load_i, area, xks, h, conc_ss, xterm, xt, rhosa, dt, lpt, svol
real ::  q, tsf, conc_i, sv, rhos, conc_a, tinc, dx, pdx,lc
real :: num_par, num_par_t, num_par_tot, advf_x, d_conc_i, mb_conc 
!load_i = (tsf * area) !Has an assumption that 1 day is multiplied to get the concentration 
!print*, load_i 
!conc_i = (load_i/svol)!Need To convert to mg/L from kg/m^3 
!print*, conc_i 
!d_conc_i = -(tsf/dx) !Change in transport concentration over time through flux over time 
!This will need to be calculated at each area/discharge change every hour, place conditional statement to adjust for this 
!mb_conc = conc_i + d_conc_i 
num_par = conc_i*dx*area
print*, num_par
!num_par_tot = conc_i*lc*area
!print*, num_par_tot
!num_par_t = num_par/tinc
!print*, num_par_t
!place conditional statement where this runs if (tinc is to large for advection) and probably place in the time loop iteration 
advf_x=num_par/(tinc)
print*, advf_x

xks = (q/svol) + (sv/h)
print*, xks
conc_ss = (load_i/svol)/xks 
print*, conc_ss
print*, "Enter your time step dt bwteen 0 and 1." !this doesn't need to be enter each time 
read (5, *) dt
xt =xks*dt
print*, xt
xterm = (1-exp(-xt))/xt
print*, xterm 
conc_a= rhos*xterm + conc_ss*(1-xterm)
print*, conc_a       
end subroutine asettle