# ClassicalDataStructuresF95
A folk of my repo DataStructuresF95, with Intel Fortran (part of Intel OneAPI HPC toolkit), and Visual Studio 2019 integration.

Classical data structures written in F95 and strictly conformant to F95 standard. 

Data structures in this repo:

- Singly linked list (fully implemented). Some sorting algorithms will be implemented for linked list, including:
  + Selection sort (fully implemented).
  + Quick sort (partially implemented, one test case passed). The defect is lacking of manipulation when call quick_sort(list) when list is empty. Thanks to Visual Studio debugger (probably Intel debugger), that bug has been fixed.
  + Merge sort (partially implemented, ie. defect).
  + Radix sort (partially implemented, two test cases passed).
- Stack (to be implemented).
- Queue (to be implemented).

Reference:

[1] Ramsden, Lin, et al. Fortran 90, a conversion course for Fortran 77 programmers (Manchester lecture notes, v3.0, 1995).

[2] Tran Hanh Nhi, Duong Anh Duc - Introduction to data structures and algorithms (textbook in HCMUS's library, 2003).

[3] N.Wirth - Algorithms + Data structures = Programs (Prentice Hall, 1976).
