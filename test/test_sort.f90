module test_suite1
   use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed
   use hs_sort
   use, intrinsic :: iso_fortran_env, only: error_unit
   implicit none
   private

   public :: collect_suite1

contains

!> Collect all exported unit tests
   subroutine collect_suite1(testsuite)
      !> Collection of tests
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      integer, parameter :: ntests = 3

      allocate (testsuite(ntests))
      testsuite(1) = new_unittest("isort", test_isort)
      testsuite(2) = new_unittest("qsort", test_qsort)
      testsuite(3) = new_unittest("hsort", test_hsort)

   end subroutine collect_suite1

   subroutine test_isort(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp), allocatable :: key(:)
      integer, allocatable :: indices(:)
      integer, parameter :: length = 10
      integer, parameter :: expected(10) = [7,9,3,4,2,10,1,5,8,6]
      integer :: i 
      allocate(key(length), indices(length))
      key = [17.0_dp, 8.0_dp, 1.0_dp, 3.0_dp, 42.0_dp, 420.0_dp,-17.0_dp, 80.0_dp, 0.0_dp, 10.0_dp]
      indices = [(i, i=1, length)]

      call isort(length, indices, key)

      call check(error, all(indices == expected), .true., "Sorting failed!")
      if (allocated(error)) return

   end subroutine test_isort

   subroutine test_qsort(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp), allocatable :: key(:)
      integer, allocatable :: indices(:)
      integer, parameter :: length = 10
      integer, parameter :: expected(10) = [7,9,3,4,2,10,1,5,8,6]
      integer :: i 
      allocate(key(length), indices(length))
      key = [17.0_dp, 8.0_dp, 1.0_dp, 3.0_dp, 42.0_dp, 420.0_dp,-17.0_dp, 80.0_dp, 0.0_dp, 10.0_dp]
      indices = [(i, i=1, length)]


      call qsort(length, indices, key)

      call check(error, all(indices == expected), .true., "Sorting failed!")
      if (allocated(error)) return

   end subroutine test_qsort

   subroutine test_hsort(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp), allocatable :: key(:)
      integer, allocatable :: indices(:)
      integer, parameter :: length = 10
      integer, parameter :: expected(10) = [7,9,3,4,2,10,1,5,8,6]
      integer :: i 
      allocate(key(length), indices(length))
      key = [17.0_dp, 8.0_dp, 1.0_dp, 3.0_dp, 42.0_dp, 420.0_dp,-17.0_dp, 80.0_dp, 0.0_dp, 10.0_dp]
      indices = [(i, i=1, length)]

      call hsort(length, indices, key)

      call check(error, all(indices == expected), .true., "Sorting failed!")
      if (allocated(error)) return

   end subroutine test_hsort

end module test_suite1
