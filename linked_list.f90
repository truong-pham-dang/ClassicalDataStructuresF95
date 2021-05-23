MODULE linked_list
	IMPLICIT NONE

	PRIVATE
	PUBLIC :: node

	TYPE node
		INTEGER              :: value                    ! data field
		TYPE (node), POINTER :: next => null()           ! pointer field
	END TYPE node

	TYPE (node), POINTER     :: list => null()

	INTERFACE create_node
		MODULE PROCEDURE allocate_and_assign_value
	END INTERFACE

	INTERFACE print_list
		MODULE PROCEDURE tranverse_and_print
	END INTERFACE

	CONTAINS

		SUBROUTINE allocate_and_assign_value(value)
			IMPLICIT NONE
			INTEGER, INTENT(in)  :: value
			INTEGER              :: status
			TYPE (node), POINTER :: current

			ALLOCATE(current, STAT = status)    ! create new node
			
			IF (status > 0) STOP 'Fail to allocate a new node'
			
			current%value = value               ! give the value
			current%next => list                ! point to previous one
			list => current                     ! update head of list

		END SUBROUTINE

		SUBROUTINE tranverse_and_print(list)
			IMPLICIT NONE
			TYPE (node), POINTER :: current, list

			current => list                         ! make current as alias of list

			DO
				IF (.NOT. ASSOCIATED(current)) EXIT ! exit if null pointer
				PRINT *, current%value              ! print the value
				current => current%next             ! make current alias of next node
			END DO

		END SUBROUTINE

END MODULE