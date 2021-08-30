INTEGER FUNCTION get_sequence_number()
      USE iso_fortran_env
      INTEGER :: number = 0
      TYPE(lock_type) lock[*]
      LOCK(lock[1])
      number = number + 1
      get_sequence_number = number
      UNLOCK(lock[1])
    END FUNCTION
    
    LOGICAL gotit
    LOCK(lock[1],ACQUIRED_LOCK=gotit)
    IF (gotit) THEN
      ! We have the lock.
    ELSE
      ! We do not have the lock - some other image does.
    END IF