subroutine lister (list, element)
implicit none
integer:: j
integer::jsize
real, dimension(:), allocatable::list, m_list
real, dimension (:), allocatable ::conc_time_1!, conc_time_2, conc_time_3
real:: element

if (allocated(conc_time_1)) then 
    jsize = size(conc_time_1)
    allocate(m_list(jsize+1))
    do j=1,jsize
        m_list(j)=list(j)
    end do
    m_list(jsize+1)=element
    deallocate(list)
    allocate(list(jsize+1))
    do j=1, jsize+1
        list(j) = m_list(j)
    end do
    print*, m_list(j)
    deallocate(m_list)
end if    
end subroutine lister