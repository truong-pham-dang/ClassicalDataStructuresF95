!------------------------------------------------------------------------------
!
!  Program name:
!
!    simple_linked_list
!
!  Purpose:
!
!    This program demonstrates:
!    * How to build, transverse a
!      simple linked list by using pointers and dynamic allocation
!      facilities.
!    * Add new node to linked list at tail and/or head of linked list.
!    * Remove node has given key in linked list.
!    * Some sorting algorithms over linked list.
!
!    Notice the linked list implementation in module linked_list 
!    stores the typed-in numbers in reverse order, whereas
!    the linked list implementation in module improved_linked_list 
!    preserves the order of points read.
!
!------------------------------------------------------------------------------

PROGRAM simple_linked_list
  IMPLICIT NONE
  INTEGER :: option
  
  PRINT*,'Please selection one option.'
  PRINT*,'Option 1: Demo create a linked list, insert a new node to that list, remove that node'
  PRINT*,'Option 2: Demo sorting a linked list by selection sort algorithm - Manipulate data field'
  PRINT*,'Option 3: Demo sorting a linked list by selection sort algorithm - Manipulate pointer next'
  PRINT*,'Option 4: Demo sorting a linked list by quick sort algorithm'
  PRINT*,'Option 5: Demo sorting a linked list by merge sort algorithm (DEFECT !!!)'
  PRINT*,'Option 6: Demo sorting a linked list by radix sort algorithm'
  PRINT*,'Option 7: Demo sorting a linked list by interchange sort algorithm'

  READ*, option
  SELECT CASE (option)
      CASE (1) 
          CALL list_create_insert_remove
      CASE (2) 
          CALL list_selection_sort
      CASE (3) 
          CALL list_selection_sort_2
      CASE (4) 
          CALL list_quick_sort
      CASE (5) 
          CALL list_merge_sort
      CASE (6) 
          CALL list_radix_sort
      CASE (7) 
          CALL list_interchange_sort
  END SELECT

END PROGRAM simple_linked_list


