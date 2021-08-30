subroutine rivernet(infile,outfile,nr)
implicit none 
INTEGER(KIND=4) :: nr, k
CHARACTER(LEN=100) :: infile, outfile
!! Ask the user for some information about the data to be read in
write(*,*) "Enter the name of the data file to read..."
read(*,*) infile
write(*,*) "Enter the name of the output reach network time data ..."
read(*,*) outfile
write(*,*) "Enter the number of lines in the input data file..."
read(*,*) nr 

! Allocate the memory required in variable mydata
end subroutine rivernet