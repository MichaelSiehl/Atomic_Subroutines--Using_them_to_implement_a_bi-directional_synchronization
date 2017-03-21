# Atomic_Subroutines--Using_them_to_implement_a_bi-directional_synchronization
Fortran 2008 coarray programming with unordered execution segments (user-defined ordering) - Atomic Subroutines: Using them to implement a bi-directional synchronization -- an example program

# Overview
This GitHub repository aims to show briefly that we can use Fortran 2008 atomic subroutines to develop sophisticated parallel logic codes that operate safely even with user-defined (unordered) execution segments: A simple example program implements a bi-directional (two-sided) synchronization method using atomic subroutines for the required remote data transfers. To do so in a safe manner, the code uses single calls to atomic_define (and atomic_ref) to transmit more than just one atomic value at a time.

The example program runs with 4 coarray images. The code executed on Image 1 aims to initiate a restoring of the segment order among all four images. To do so, it sets the OOOPimscEnum_ImageActivityFlag enumeration atomically to value 'InitializeSegmentSynchronization' on the remote images 2, 3, and 4. Then, the code executed on images 2, 3, and 4 respectively, sets it's own remote enumeration value (on image 1) atomically to value 'WaitForSegmentSynchronization'. See the more detailed explanations with code snippets below.<br />
See also the explanations in my other GitHub repositories:<br />
https://github.com/MichaelSiehl/Atomic_Subroutines--Using_Integers_Efficiently<br />
https://github.com/MichaelSiehl/Atomic_Subroutines--Using_Coarray_Arrays_to_Allow_for_Safe_Remote_Communication<br />
https://github.com/MichaelSiehl/Atomic_Subroutines--How_to_Encapsulate_Access_to_Them<br />

# The three steps of implementing a bi-directional (two-sided) synchronization using atomic subroutines (within the example program):

STEP 1 (executed on image 1): 
Initiate the segment synchronization on the involved remote images 2, 3, and 4. To do so, set the ImageActivityFlag atomically to value InitializeSegmentSynchronization on these remote images, and also transmit the image number of the executing image (1) within the same single call to atomic_define. The following code snippet is taken from subroutine OOOPimsc_SynchronizeTheInvolvedImages_CA in Module OOOPimsc_admImageStatus_CA.f90. The call to atomic_define is encapsulated in subroutine OOOPimscSAElement_atomic_intImageActivityFlag99_CA.
```fortran
  ! this is the first part of the bi-directional synchronization:
write(*,*) 'step 1: on image', this_image()
  intImageActivityFlag = OOOPimscEnum_ImageActivityFlag % InitializeSegmentSynchronization
  call OOOPimsc_subSyncMemory (Object_CA) ! execute sync memory
  do intCount = 1, intNumberOfImages
    !
    intImageNumber = intA_RemoteImageNumbers(intCount)
    if (intImageNumber .ne. this_image()) then ! (synchronization is only required between distinct images)
    ! initialize the segment synchronization on the involved remote images:
      ! pack the ImageActivityFlag enumeration together with this_image():
      call OOOPimsc_PackEnumValue_ImageActivityFlag (Object_CA, intImageActivityFlag, this_image(), intPackedEnumValue)
      ! send the packed enum value atomically to the remote image (intImageNumber):
      call OOOPimscSAElement_atomic_intImageActivityFlag99_CA (Object_CA, intPackedEnumValue, &
            intImageNumber, logExecuteSyncMemory = .false.) ! do not execute SYNC MEMORY
    end if
  end do
```

STEP 2 (executed on images 2, 3, and 4):
On these coarray images the ImageActivityFlag is permanently checked for it's actual value. If it has value InitializeSegmentSynchronization, set the ImageActivityFlag on these images to value WaitForSegmentSynchronization and signal to the remote image 1 atomically that the ImageActivityFlag of the executing image is set to state WaitForSegmentSynchronization. The following code snippets are taken from subroutine IIimma_SYNC_CheckActivityFlag in Module OOOPimma_admImageManager.f90 and from subroutine OOOPimsc_Start_SegmentSynchronization_CA in Module OOOPimsc_admImageStatus_CA. 
```fortran
  do ! check the ImageActivityFlag in local PGAS memory permanently until it has
     !         value OOOPimscEnum_ImageActivityFlag % ExecutionFinished
    ! ****************************************
    ! this is the first counter-part of the bi-directional synchronization:
    ! execution segment synchronization:
    if (OOOPimscGAElement_check_atomic_intImageActivityFlag99_CA (OOOPimscImageStatus_CA_1, &
                       OOOPimscEnum_ImageActivityFlag % InitializeSegmentSynchronization, &
                       intAdditionalAtomicValue = intSetFromImageNumber)) then
write(*,*) 'step 2: on image', this_image()
      ! start the execution segment synchronization on the executing image:
      call OOOPimsc_Start_SegmentSynchronization_CA (OOOPimscImageStatus_CA_1, intSetFromImageNumber)
    !
    else if (OOOPimscGAElement_check_atomic_intImageActivityFlag99_CA (OOOPimscImageStatus_CA_1, &
                        OOOPimscEnum_ImageActivityFlag % ExecutionFinished)) then
      write(*,*) 'Execution finished on image', this_image()
      exit ! exit the loop to finish image execution
    end if
    !
  end do
```
```fortran
  ! ********************************************************
  ! this is the second part of the bi-directional synchronization:
  ! (the first part is in routine IIimma_SYNC_CheckActivityFlag)
  intRemoteImageNumber = intSetFromImageNumber
  !
  ! initialize the segment synchronization on the involved images:
  intImageActivityFlag = OOOPimscEnum_ImageActivityFlag % WaitForSegmentSynchronization
  !
  call OOOPimsc_PackEnumValue_ImageActivityFlag (Object_CA, intImageActivityFlag, this_image(), intPackedEnumValue)
  ! signal to the remote image (image 1) that this executing image is now in state 'WaitForSegmentSychronization':
  call OOOPimscSAElement_atomic_intImageActivityFlag99_CA (Object_CA, intPackedEnumValue, &
                         intRemoteImageNumber, intArrayIndex = this_image(), logExecuteSyncMemory = .true.)
  ! in real world programming, there would be another spin-wait loop synchronization here
  !
```

STEP 3 (executed on image 1):
All the involved remote images (2, 3, and 4) have atomically signaled that they are in state WaitForSegmentSynchronization, see the last and the following code snippets. (The example program does now terminate its's execution. But in real world programming, image 1 would procceed to give further instructions to images 2, 3, and 4 by transmitting values atomically to them). The next code snippet is taken from subroutine OOOPimsc_SynchronizeTheInvolvedImages_CA in Module OOOPimsc_admImageStatus_CA.f90.
```fortran
  !************************************************
  ! this is the second counter-part of the bi-directional synchronization:
  ! wait until all the involved remote image(s) do signal that they are in state WaitForSegmentSynchronization:
  intImageActivityFlag = OOOPimscEnum_ImageActivityFlag % WaitForSegmentSynchronization
  ! initialize the array elements with .false.:
  logA_CheckImageStates = .false.
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
    ! (be aware: this would be error prone in real world programming, but it is safe for this example program)
  end do
write(*,*) 'step 3: on image', this_image()
  !
```

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
