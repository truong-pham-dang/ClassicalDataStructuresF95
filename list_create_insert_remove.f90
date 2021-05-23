SUBROUTINE list_create_insert_remove
	USE linked_list
	USE improved_linked_list
	IMPLICIT NONE
	INTEGER              :: num
	TYPE (linked_list_t) :: singly_linked_list

	DO num = 1, 5
		IF (num == 1) THEN 
			CALL create_head(singly_linked_list, num)
		ELSE
			CALL append_tail(singly_linked_list,num)
		ENDIF
	ENDDO

	PRINT *, 'Transverse the list built up and print the values'

	CALL print_list(singly_linked_list)

	PRINT *, 'Insert new node with value is 10 after node 3 ...'

	CALL append_after_node(singly_linked_list, 3, 10)

	PRINT *, 'Transverse the list built up and print the values'

	CALL print_list(singly_linked_list)

	PRINT *, 'Remove node has value 10 ...'

	CALL remove_node(singly_linked_list, 10)

	PRINT *, 'Transverse the list built up and print the values'

	CALL print_list(singly_linked_list)

END SUBROUTINE