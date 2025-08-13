subroutine graph (p)
use aplot
real, dimension (:,:), allocatable ::timeout, timeout_1
integer:: j
real:: aplot_1
type(aplot_1)::p
!use aplot
!type(aplot_1)::p
p=initialize_plot()
call set_title(p,"Simluated Surface Sediments")
call add_dataset(p,timeout(9,j),j)
call set_serieslabel(p,0,"Layer 1")
call set_seriestype(p,0, aplot_style_dot,.)
call set_xlabel(p, "Solid Conc mg/L")
call set_ylabel(p, "Time (s)")
call display_plot(p)
call destroy_plot(p)
end subroutine graph
