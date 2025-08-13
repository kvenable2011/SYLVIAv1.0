module m_employee
  implicit none
  private
  public t_date, t_address, t_person, t_employee
  ! Note another way of using the public attribute:
  ! gathering all public data types in one place.

  type :: t_date
    integer :: year, month, day
  end type

  type :: t_address
    character(len=:), allocatable :: city, road_name
    integer :: house_number
  end type

  type, extends(t_address) :: t_person
    character(len=:), allocatable :: first_name, last_name, e_mail
  end type

  type, extends(t_person)  :: t_employee
    type(t_date) :: hired_date
    character(len=:), allocatable :: position
    real :: monthly_salary
  end type

end module m_employee