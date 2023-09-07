program string0
  implicit none
  character, allocatable :: string(:)
  character(len = :), allocatable :: text

  text = 'this is a pen'
  string = transfer(text, ' ', size = len_trim(text))
  string = achar(iachar(string) - 32)
  text = transfer(string, text)
  print *, text

  stop
end program string0