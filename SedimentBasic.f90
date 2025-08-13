!for the computation of solids flux and settling velocity for a descriptive approach!

  program settle

   implicit none
   real :: rhos, rhow, om, sm, gs, lw, l, n, vs1, vis, ds, g, sf, PCTWA, rhob, rhod, choice, dds
   real :: sfrs, resv, taub, f, h, ks, md, grho, ks1, ks2, rd, vs2, u, y1
   integer :: ns, s, i, so
   real, allocatable,dimension(:) :: iv
   integer, parameter :: rmax = 100, cmax = 100, pi =3.141593

   Print *, "Choose whether you have option (1) descriptive or (2)solids transport "
        read *, so
        if (so ==1) then
        end if
        if (so ==2) then
        continue
        end if
   ns = 1
     granules: do i=1, ns
     write(6,*) i
     20 continue
   write(*,*) "Enter liquid water and liquid content of sediment in the reach segment (lw,l)"
   read (5, *) lw,l
    !these are the givens below for knowns in the settling. Some options will allow for user inputs!
    n=lw/l
  !to loop iteratively for multiple particles)!
        print*, "Choose your particle denisity"
        print*, "1  Organic Matter"
        print*, "2  Siliceous Minerals"
        print*, "3  Garnett Sands"
        print*, "4  Enter density"
         read *, choice
        om=1270
        sm=2650
        gs=4000
            if (choice ==1) then
            rhos=om
            end if
            if (choice ==2) then
            rhos=sm
            end if
            if (choice ==3) then
            rhos=gs
            end if
            if (choice ==4) then
            read*, rhos
            end if
            rhow= 1000
          !would have liked to place a dummy variable here but couldn't get the functionality!
                    write (*, *) "Enter grain size in mm, if not entering zero will default will be 0.025"
                    read (*, *) dds
                    if (dds==0) then
                    ds=0.000025
                    else if (dds>0) then
                    dds=ds
                    end if

            g=9.807
            vis=.001
            rhod=rhos* (1-n)
            rhob=rhow*n+rhod
            PCTWA=100*(rhob/rhod)
            vs1=((86400*g)/18*vis)*(rhos-rhow)*(ds**2)
            sf=rhos*vs1
            s = 6
            print *, "Porosity         Percent Water      Solids Flux     Bulk Density    Settling Velocity   Dry Density"
            allocate(iv(s))
            iv(1) = n
            iv(2) = PCTWA
            iv(3) = sf
            iv(4) = rhob
            iv(5) = vs1
            iv(6) = rhod
            print*, iv
            deallocate (iv)
            end do granules
     Print*, "Enter resuspension rate, (m/day) and water velocity (m/s) (resv,u)"
     read *, resv, u
     sfrs = -1*resv*rhos
     write (*,*) "sfrs =", sfrs
     !Process Based Solids Transport
     Print *, "enter water depth (m) and median diameter size (m) (h, md)"
       read *, h, md
       ks1 = 3*md
       ks2 = 0.01*h
        if (ks1 .GT. ks2) then
        ks1 = ks
        end if
        if  (ks2 .GT. ks1) then
        ks2 = ks
        end if
            if (md> .005 .and. h<.05) then
            y1=log10(12*h/ks)
            f=0.24/(y1)**2
            taub = rhos*f*(u**2)
            end if
            if (md <= .005 .and. h>.05) then
            taub = 3.16*10**(-3)*(u**2)!bc f=0.0253
            end if
                print *, "taub=", taub
    !using the van Rijn method
             grho = g*(rhos/rhow -1)
             rd = (ds*(grho*ds)**0.5)/(vis/rhow)
                if (ds <.0001) then
                vs2= rd/18
                else if (.0001< ds .and. ds <=.001) then
                vs2 = 10/rd* ((1+ 0.01*Rd**2)**0.5 -1)
                else  if (ds>.001) then
                vs2= 4.425*(ds**0.5)
                end if
     end program settle
