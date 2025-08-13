subroutine aa (mydata)
IMPLICIT NONE
REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: mydata
INTEGER(KIND=4) :: nr, k
CHARACTER(LEN=100) :: infile
real::t,l,md,d,q,u, tss
! Ask the user for some information about the data to be read in
write(*,*) "Enter the name of the data file to read..."
read(*,*) infile
write(*,*) "Enter the number of lines in the input data file..."
read(*,*) nr 
! Allocate the memory required in variable mydata
ALLOCATE(mydata(7,k))
! Open up the file to read
OPEN(UNIT=1,FILE=infile)
! Now read the file into variable mydata
DO k=1,nr
 read(1,*)t,l,md,d,q,u,tss 
 mydata(1:7,k)=(/t,l,md,d,q,u,tss/)
ENDDO
! We are done with the file so now close it out
CLOSE(1)
! For fun, let's write back out to standard out
!DO k=1,nr
!    write(*,*) mydata(:,k)
!ENDDO
write(*,*) mydata(1,2678), mydata(7,2678)
END subroutine aa