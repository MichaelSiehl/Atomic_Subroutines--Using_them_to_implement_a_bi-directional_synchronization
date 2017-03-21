# Atomic_Subroutines--Using_them_to_implement_a_bi-directional_synchronization
Fortran 2008 coarray programming with unordered execution segments (user-defined ordering) - Atomic Subroutines: Using them to implement a bi-directional synchronization -- an example program

# Overview
This GitHub repository aims to show briefly that we can use Fortran 2008 atomic subroutines to develop parallel logic codes that operate safely even with user-defined (unordered) execution segments: A simple example program implements a bi-directional (two-sided) synchronization method using atomic subroutines for the required remote data transfers. To do so in a safe manner, the code uses single calls to atomic_define (and atomic_ref) to transmit more than just one atomic value at a time.

# Output
A program run (using 4 coarray images) gives the following output on screen:
```fortran
entering execution segment           1 on image           1
entering execution segment           1 on image           2
entering execution segment           1 on image           3
entering execution segment           1 on image           4
step 1: on image           1
entering execution segment           2 on image           1
entering execution segment           2 on image           2
step 2: on image           2
entering execution segment           2 on image           3
step 2: on image           3
entering execution segment           2 on image           4
step 2: on image           4
entering execution segment           3 on image           2
entering execution segment           3 on image           3
entering execution segment           4 on image           3
Execution finished on image           3
entering execution segment           4 on image           2
Execution finished on image           2
entering execution segment           3 on image           4
entering execution segment           4 on image           4
Execution finished on image           4
entering execution segment           3 on image           1
entering execution segment           4 on image           1
entering execution segment           5 on image           1
step 3: on image           1
entering execution segment           6 on image           1
Execution finished on image           1
```

# test
```fortran
  !
  do
    do intCount = 1, intNumberOfImages
      !
      intImageNumber = intA_RemoteImageNumbers(intCount)
      if (intImageNumber .ne. this_image()) then ! (synchronization is only required between distinct images)
        if (.not. logA_CheckImageStates(intCount)) then ! only if logA_CheckImageStates for the remote image is still false:
          if (OOOPimscGAElement_check_atomic_intImageActivityFlag99_CA (OOOPimscImageStatus_CA_1, &
                           OOOPimscEnum_ImageActivityFlag % WaitForSegmentSynchronization, &
                           intArrayIndex = intImageNumber, intAdditionalAtomicValue = intSetFromImageNumber)) then
            logA_CheckImageStates(intCount) = .true. ! the remote image is in state WaitForSegmentSynchronization
          end if
        end if
      end if
    end do
    !
    if (all(logA_CheckImageStates)) exit ! exit the do loop if all involved remote images are in state
                                         ! WaitForSegmentSynchronization
  end do
```
