MODULE generic_swap

! A module which defines a generic subroutine, swap, and defines
! the specific subroutines which implement it for several data
! types.

PUBLIC  ::  swap ! Only the generic name is visable 
                 ! outside of the module
PRIVATE ::  i_swap, r_swap, c_swap, l_swap, ch_swap

INTERFACE swap
    MODULE PROCEDURE  i_swap, r_swap, c_swap, l_swap, ch_swap
END INTERFACE

CONTAINS

SUBROUTINE i_swap (a,b)
	INTEGER,  INTENT(inout)  ::  a,b
	INTEGER                  ::  t
	t = a
	a = b
	b = t
END SUBROUTINE i_swap

SUBROUTINE r_swap (a,b)
	REAL,  INTENT(inout) ::  a,b
	REAL                 ::  t
	t = a
	a = b
	b = t
END SUBROUTINE r_swap

SUBROUTINE c_swap (a,b)
	COMPLEX,  INTENT(inout)  ::  a,b
	COMPLEX                  ::  t
	t = a
	a = b
	b = t
END SUBROUTINE c_swap

SUBROUTINE l_swap (a,b)
	LOGICAL,  INTENT(inout)  ::  a,b
	LOGICAL                  ::  t
	t = a
	a = b
	b = t
END SUBROUTINE l_swap

SUBROUTINE ch_swap (a,b)
	CHARACTER(len=*),  INTENT(inout)  ::  a,b
	CHARACTER(len=1)                  ::  t
	INTEGER                           ::  i
	IF (LEN(a) == LEN(b) ) THEN
	DO i = 1, LEN(a)
		t = a(i:i)
		a(i:i) = b(i:i)
		b(i:i) = t
	ENDDO
	ELSE
		PRINT *, "Lengths of character strings differ--can't swap"
		STOP
	ENDIF
END SUBROUTINE ch_swap

END MODULE generic_swap