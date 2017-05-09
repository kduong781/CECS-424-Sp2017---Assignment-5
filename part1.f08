PROGRAM BinarySearch
IMPLICIT NONE
  real, dimension (:), allocatable :: darray
  integer :: temp, mid, left, right, i, j, s1, s2, sizeT, search, run
  logical :: swapped = .true.
  print*, "Enter the size of the array:"
  read*, s1

  ! allocate memory
  allocate ( darray(s1) )

  !enter array
  do i = 1, s1
        print*, "Enter the number for index", i
        read*, s2
        darray(i) = s2
        !print*, "darray(", i ,") = ", darray(i)
  end do

  !bubble sort
  do j=size(darray)-1,1,-1
    swapped = .false.
    do i=1, j
      if(darray(i+1) < darray(i)) then
        temp = darray(i)
        darray(i) = darray(i+1)
        darray(i+1) = temp
        swapped = .true.
      end if
    end do
    if(.not. swapped) exit
   end do

  !print array
  do j = 1, s1
   print*, darray(j)
  end do

  !binary search
  left = 1
  right = s1
  sizeT = right - left
  mid = (left + right)/2
  run = 0
  print*, "Enter a number you want to search for: "
  read*, search
  do while(run == 0)
    do while(darray(mid) /= search .and. sizeT > 0)
      if(darray(mid) < search) then
        left = mid + 1
      else
        right = mid - 1
      end if
      sizeT = right - left
      mid = (left + right)/ 2
    end do
    if(darray(mid) == search) then
      print*, "The number you searched for is at index", mid
      !run = 1 keep this if u want to break out of loop
    else
      print*, "The number you searched for is not in this array. Pick a new number:"
      read*, search
      left = 1
      right = s1
      sizeT = right - left
      mid = (left + right)/2
    end if
  end do



  !deallocate array
  deallocate (darray)
END PROGRAM
