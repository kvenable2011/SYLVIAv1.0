Function pivot (p1)
    real, dimension(:,:):: p1
    real, dimension (:), pointer ::piv_row
    integer,  dimension (2) :: location
    integer, dimension(:), allocatable, target:: r
    
    location = maxloc(p1)
    
    r = allocate(size(p1(1,:))
    r = p1(location(1), :)
    piv_row=>p1
    
end function pivot    