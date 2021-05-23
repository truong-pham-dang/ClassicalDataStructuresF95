SUBROUTINE list_radix_sort
	USE linked_list
	USE improved_linked_list
	IMPLICIT NONE
	TYPE (linked_list_t) :: list, list2

	CALL create_head(list, 4)
	CALL append_tail(list, 6)
	CALL append_tail(list, 5)
	CALL append_tail(list, 1)
	CALL append_tail(list, 8)
	CALL append_tail(list, 2)

	PRINT *, 'Test 1:'
	PRINT *, '*****************************************************************'
	PRINT *, 'Transverse the list built up and print the values before sorting:'

	CALL print_list(list)

	CALL radix_sort(list)

	PRINT *, 'Transverse the list built up and print the values after sorting:'

	CALL print_list(list)
	PRINT *, '*****************************************************************'

	PRINT *, 'Test 2:'
	PRINT *, '*****************************************************************'
    CALL create_head(list2, 8)
    CALL append_tail(list2, 6)
    CALL append_tail(list2, 1)
    CALL append_tail(list2, 5)
    CALL append_tail(list2, 2)
    CALL append_tail(list2, 9)
    CALL append_tail(list2, 4)
    CALL append_tail(list2, 7)
    CALL append_tail(list2, 3)

	PRINT *, 'Transverse the list built up and print the values before sorting:'

	CALL print_list(list2)

	CALL radix_sort(list2)

	PRINT *, 'Transverse the list built up and print the values after sorting:'

	CALL print_list(list2)

END SUBROUTINE
