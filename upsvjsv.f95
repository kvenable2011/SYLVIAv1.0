subroutine vjsv 
implicit none
real :: g, gp, rhos_kg, vis, ds, sv, rd, rhow, x
vis =.001
g=9.807
rhow_kg = 998
read*, ds, rhos
gp=g*((rhos/rhow)-1)
rd=(ds*(gp*ds)**0.5)/(vis/rhow)
    if (ds<=.0001) then 
    sv=(rd/18)*(sqrt(gp*ds))
    else if (0.0001<ds .and. ds<=0.001) then 
    x=1+(0.01*(rd**2))
    sv=(10/rd)*(sqrt(x)-1)*sqrt(gp*ds)
    else 
    sv=1.1*(sqrt(gp*ds))
    end if 
end subroutine vjsv


