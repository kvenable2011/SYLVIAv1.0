subroutine input(h,da,lc,q,u,a,dx,dx_1)
real::h, da, lc, q, u
Print *, "Enter water depth (m), median diameter size (m) and length (m)"
   read(5, *) h, da, lc
Print *, "Enter your water discharge (m^3/s) and water velocity m/s)"
   read(5, *) q, u    
a=q/u 
print*, "Your cross-sectional area is", a
dx=u*1 !displacement in 1 sec 
dx_1= u*1*86400 !displacement in 1 day 
print*, "dx=" ,dx 
print*, "dx_1=" , dx_1
end subroutine input       