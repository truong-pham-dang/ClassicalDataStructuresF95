# ClassicalDataStructuresF95
A folk of my archived repo [DataStructuresF95](https://github.com/truongd8593/DataStructuresF95), but using Visual Studio 2019 project and Intel Fortran compiler (part of Intel OneAPI HPC Toolkit which is free for any developer). This allows debugging with Intel debugger, thus enhances production.

Classical data structures written in F95 and strictly conformant to F95 standard. 

Data structures in this repo:

- Singly linked list (fully implemented). Some sorting algorithms will be implemented for linked list, including:
  + Selection sort (fully implemented).
  + Interchange sort (fully implemented).
  + Quick sort (fully implemented). Bug fixed: lacking of manipulation when call quick_sort(list) when list is empty; loosing tail during computation. Thanks to Visual Studio debugger (probably Intel debugger), all defects have been resolved.
  + Merge sort (partially implemented, ie. defect).
  + Radix sort (partially implemented, two test cases passed).
- Stack (to be implemented).
- Queue (to be implemented).

Reference:

[1] Ramsden, Lin, et al. Fortran 90, a conversion course for Fortran 77 programmers (Manchester lecture notes, v3.0, 1995).

[2] Tran Hanh Nhi, Duong Anh Duc - Introduction to data structures and algorithms (textbook in HCMUS's library, 2003).

[3] N.Wirth - Algorithms + Data structures = Programs (Prentice Hall, 1976).
