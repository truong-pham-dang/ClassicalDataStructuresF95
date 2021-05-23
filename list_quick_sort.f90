SUBROUTINE list_quick_sort
	USE linked_list
	USE improved_linked_list
	IMPLICIT NONE
	TYPE (linked_list_t) :: list1, list2
    
    CALL create_head(list2, 4)
	CALL append_tail(list2, 6)
	CALL append_tail(list2, 5)
	CALL append_tail(list2, 1)
	CALL append_tail(list2, 8)
	CALL append_tail(list2, 2)
    
    PRINT *, 'Test 1:'
    PRINT *, 'Transverse the list built up and print the values before sorting:'

	CALL print_list(list2)

	CALL quick_sort(list2)

	PRINT *, 'Transverse the list built up and print the values after sorting:'

	CALL print_list(list2)

	CALL create_head(list1, 10)
	CALL append_tail(list1, 8)
	CALL append_tail(list1, 6)
	CALL append_tail(list1, 1)
	CALL append_tail(list1, 5)
	CALL append_tail(list1, 2)
	CALL append_tail(list1, 9)
	CALL append_tail(list1, 4)
	CALL append_tail(list1, 7)
	CALL append_tail(list1, 3)

	PRINT *, 'Test 2:'
    PRINT *, 'Transverse the list built up and print the values before sorting:'

	CALL print_list(list1)

	CALL quick_sort(list1)

	PRINT *, 'Transverse the list built up and print the values after sorting:'

	CALL print_list(list1)
        
END SUBROUTINE