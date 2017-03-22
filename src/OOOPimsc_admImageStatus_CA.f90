! https://github.com/MichaelSiehl/Atomic_Subroutines--Using_them_to_implement_a_bi-directional_synchronization

module OOOPimsc_admImageStatus_CA
!************************************************
! Namespace: OOOP - Parallel
!************************************************
! Abstact Data Type Short Name: OOOPimsc
!********************************************************
! Abstract Data Type (ADT):         OOOPimsc_adtImageStatus_CA
! Abstract Data Type Module (adm):  OOOPimsc_admImageStatus_CA.f90
!********************************************************
! Purpose:                    ImageStatus_CA-Object
! Language:                   mainly Fortran 95 with Fortran 2008 coarrays
! Programmer:                 Michael Siehl
! Date:                       March 2017
!********************************************************
! Naming Conventions:
!
!  for scalar members:
!                             m: object member
!                             S: setter, G: getter,
!                             S_atomic: the setter operates on atomic values using atomic_define and SYNC MEMORY
!                             G_check_atomic: the getter only checks local PGAS memory for specific values atomically
!
!  for array members:
!                             A: array
!                             mA: array member
!                             SA: set array, GA: get array,
!
!  for elements of array members:
!                             SAElement: set only one array element
!                             GAElement: get only one array element
!
!                             99: signals a static array member which has an upper array bound
!                                 larger than necessary; the upper bound is given by a global parameter
!
!  other naming conventions:
!                             _CA: coarray routine / coarray declaration
!                             _SYNC_: synchronization routine
!
!                             Enum: enumeration
!
!                             OO: public (outer) scope (the two leading namespace letters)
!                             II: private (inner) scope
!                             UU: sub-object
!********************************************************
!___________________________________________________________

use OOOGglob_Globals
use OOOEerro_admError
use, intrinsic :: iso_fortran_env
!___________________________________________________________

implicit none
!___________________________________________________________

private
!__________________________________________________________
!
! access routines for scalar
! and static array members:
public :: OOOPimscSAElement_atomic_intImageActivityFlag99_CA, &
                             OOOPimscGAElement_check_atomic_intImageActivityFlag99_CA
!***
! logic codes:
public :: OOOPimsc_subSyncMemory
public :: OOOPimsc_SynchronizeSegmentOrdering_CA
private :: OOOPimsc_SynchronizeTheInvolvedImages_CA
public :: OOOPimsc_Start_SegmentSynchronization_CA
!***
! coarray ADT management:
public :: OOOPimsc_StructureConstructor_CA
!___________________________________________________________
!
!***************************
!****  Error Handling:  ****
!***************************
!***
! local ADT:
private :: IIimsc_ErrorHandler
!***
! coarray ADT:
private :: IIimsc_ImageNumberBoundError_CA
!___________________________________________________________
!
!************************
!****  Enumerations:  ***
!************************
!***  ImageActivityFlag:
type, private :: OOOPimsc_DontUse1
  integer(kind=OOOGglob_kint) :: Enum_StepWidth ! = 1000000
  integer(kind=OOOGglob_kint) :: InitialWaiting ! = 2000000
  integer(kind=OOOGglob_kint) :: TeamManager ! = 3000000
  integer(kind=OOOGglob_kint) :: TeamMember ! = 4000000
  integer(kind=OOOGglob_kint) :: ExecutionFinished ! = 5000000
  integer(kind=OOOGglob_kint) :: InitializeSegmentSynchronization ! = 6000000
  integer(kind=OOOGglob_kint) :: WaitForSegmentSynchronization ! = 7000000
  integer(kind=OOOGglob_kint) :: ContinueSegmentSynchronization ! = 8000000
  integer(kind=OOOGglob_kint) :: Enum_MaxValue ! = 9000000
end type OOOPimsc_DontUse1
!
type (OOOPimsc_DontUse1), public, parameter :: OOOPimscEnum_ImageActivityFlag &
     = OOOPimsc_DontUse1 (1000000,2000000,3000000,4000000,5000000,6000000,7000000,8000000, 9000000)
!___________________________________________________________
!
!***************************************
!***  Type Definition: *****************
!***************************************
type, public :: OOOPimsc_adtImageStatus_CA
  private
  !*****
  integer(atomic_int_kind), dimension (1:OOOGglob_NumImages_UpperBound, 1:2) :: &
                  mA_atomic_intImageActivityFlag99 = OOOPimscEnum_ImageActivityFlag % InitialWaiting
  !*****
  integer(atomic_int_kind), dimension (1:OOOGglob_NumImages_UpperBound) :: mA_atomic_intImageSyncMemoryCount99 = 0
  !*****
  type (OOOEerroc_colError) :: m_UUerrocError ! error collection
  !
end type OOOPimsc_adtImageStatus_CA
!___________________________________________________________
!
!****************************************************
!***  Corresponding Coarray Declaration:  ***********
!****************************************************
!***
type (OOOPimsc_adtImageStatus_CA), public, codimension[*], volatile, save :: OOOPimscImageStatus_CA_1
!___________________________________________________________





contains


!##################################################################################################
!##################################################################################################
!##################################################################################################


!*******************************
! access routines for scalar   *
! and static array members:    *
!*******************************
!__________________________________________________________
!
!**********
subroutine OOOPimsc_PackEnumValue_ImageActivityFlag (Object_CA, intEnumValue, intAdditionalValue, intPackedEnumValue)
  ! pack the both integer input arguments into a single integer scalar
  type (OOOPimsc_adtImageStatus_CA), codimension[*], volatile, intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent (in) :: intEnumValue
  integer(OOOGglob_kint), intent (in) :: intAdditionalValue
  integer(OOOGglob_kint), intent (out) :: intPackedEnumValue
  integer(OOOGglob_kint) :: intEnum_StepWidth
  integer(OOOGglob_kint) :: status
  !
                                                                call OOOGglob_subSetProcedures &
                                                              ("OOOPimsc_PackEnumValue_ImageActivityFlag")
  !
                                                                ! check if intAdditionalValue argument is to large:
                                                                ! ToDo: check if it is negative
                                                                intEnum_StepWidth = OOOPimscEnum_ImageActivityFlag % &
                                                                  Enum_StepWidth
                                                                if (intAdditionalValue >= intEnum_StepWidth) then
                                                                  call IIimsc_ErrorHandler (Object_CA, &
                                                                       "intAdditionalValue is to large", &
                                                                    OOOGglob_error, status)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
                                                                !
  !
  intPackedEnumValue = intEnumValue + intAdditionalValue
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimsc_PackEnumValue_ImageActivityFlag
!
!**********
subroutine OOOPimsc_UnpackEnumValue (intPackedEnumValue, intEnum_StepWidth, &
                             intUnpackedEnumValue, intUnpackedAdditionalValue)
  ! unpack the integer enum value into two integer scalars
  integer(OOOGglob_kint), intent (in) :: intPackedEnumValue
  integer(OOOGglob_kint), intent (in) :: intEnum_StepWidth
  integer(OOOGglob_kint), intent (out) :: intUnpackedEnumValue
  integer(OOOGglob_kint), intent (out) :: intUnpackedAdditionalValue
  !
                                                                call OOOGglob_subSetProcedures &
                                                                   ("OOOPimsc_UnpackEnumValue")
  !
  intUnpackedAdditionalValue = mod(intPackedEnumValue, intEnum_StepWidth)
  !
  intUnpackedEnumValue = intPackedEnumValue - intUnpackedAdditionalValue
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimsc_UnpackEnumValue
!
!**********
!__________________________________________________________
!
subroutine OOOPimscSAElement_atomic_intImageActivityFlag99_CA (Object_CA, intImageActivityFlag, &
                                intImageNumber, intArrayIndex, logExecuteSyncMemory)
  ! set an array element atomically
  type (OOOPimsc_adtImageStatus_CA), codimension[*], volatile, intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent (in) :: intImageActivityFlag
  integer(OOOGglob_kint), intent (in) :: intImageNumber ! the (remote) image number
  integer(OOOGglob_kint), optional, intent (in) :: intArrayIndex
  integer(OOOGglob_kint) :: intArrIndex
  integer(OOOGglob_kint) :: intMaxVal
  logical(OOOGglob_klog), optional, intent (in) :: logExecuteSyncMemory
  logical(OOOGglob_klog) :: logSyncMemoryExecution
  integer(OOOGglob_kint) :: status = 0 ! error status
                                                                call OOOGglob_subSetProcedures &
                                                                 ("OOOPimscSAElement_atomic_intImageActivityFlag99_CA")
  !
  if (present(intArrayIndex)) then
                                                                ! check if intArrayIndex argument is to large:
                                                                ! ToDo: check if it is negative
                                                                intMaxVal = OOOGglob_NumImages_UpperBound
                                                                if (intArrayIndex .gt. intMaxVal) then
                                                                  call IIimsc_ErrorHandler (Object_CA, &
                                                                       "intArrayIndex is to large", &
                                                                    OOOGglob_error, status)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
                                                                !
    intArrIndex = intArrayIndex
  else ! default:
    intArrIndex = intImageNumber
  end if
  !
  !
  if (present(logExecuteSyncMemory)) then
    logSyncMemoryExecution = logExecuteSyncMemory
  else ! default:
    logSyncMemoryExecution = .true.
  end if
  !
  !
  if (intImageNumber == this_image()) then ! local atomic define
    ! don't execute sync memory for local atomic_define:
    call atomic_define(Object_CA % mA_atomic_intImageActivityFlag99(intArrIndex,1), intImageActivityFlag)
  !
  else ! remote atomic define
                                                                ! check if the image number is valid:
                                                                if (IIimsc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumber)) then
                                                                  call IIimsc_ErrorHandler (Object_CA, &
                                                                       "no valid image number", &
                                                                    OOOGglob_error, status)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
                                                                !
    ! execute sync memory for remote atomic_define:
    if (logSyncMemoryExecution) call OOOPimsc_subSyncMemory (Object_CA) ! execute sync memory
    call atomic_define(Object_CA[intImageNumber] % mA_atomic_intImageActivityFlag99(intArrIndex,1), intImageActivityFlag)
    !
  end if
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimscSAElement_atomic_intImageActivityFlag99_CA
!**********
logical(OOOGglob_klog) function OOOPimscGAElement_check_atomic_intImageActivityFlag99_CA (Object_CA, intCheckImageActivityFlag, &
                                                          intArrayIndex, intAdditionalAtomicValue)
  ! check array element atomically
  ! in order to hide the sync memory statement herein, this Getter does not allow
  ! to access the member directly, but instead does only allow to check the atomic member
  ! for specific values (this Getter is intented for synchronizations)
  type (OOOPimsc_adtImageStatus_CA), codimension[*], volatile, intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent (in) :: intCheckImageActivityFlag
  integer(OOOGglob_kint), optional, intent (in) :: intArrayIndex
  integer(OOOGglob_kint) :: intArrIndex
  integer(OOOGglob_kint) :: intMaxVal
  integer(OOOGglob_kint), optional, intent (out) :: intAdditionalAtomicValue
  integer(OOOGglob_kint) :: intImageActivityFlag
  integer(OOOGglob_kint) :: intPackedEnumValue
  integer(OOOGglob_kint) :: intEnum_StepWidth
  integer(OOOGglob_kint) :: intUnpackedEnumValue
  integer(OOOGglob_kint) :: intUnpackedAdditionalValue
  integer(OOOGglob_kint) :: status = 0 ! error status
                                                                call OOOGglob_subSetProcedures &
                                                                 ("OOOPimscGAElement_check_atomic_intImageActivityFlag99_CA")
  !
  OOOPimscGAElement_check_atomic_intImageActivityFlag99_CA = .false.
  !
  if (present(intArrayIndex)) then
                                                                ! check if intArrayIndex argument is to large:
                                                                ! ToDo: check if it is negative
                                                                intMaxVal = OOOGglob_NumImages_UpperBound
                                                                if (intArrayIndex .gt. intMaxVal) then
                                                                  call IIimsc_ErrorHandler (Object_CA, &
                                                                       "intArrayIndex is to large", &
                                                                    OOOGglob_error, status)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
                                                                !
    intArrIndex = intArrayIndex
  else ! default:
    intArrIndex = this_image()
  end if
  !
  ! access an array element in local PGAS memory atomically:
  call atomic_ref(intImageActivityFlag, Object_CA % mA_atomic_intImageActivityFlag99(intArrIndex,1))
  ! unpack the intImageActivityFlag value:
  intPackedEnumValue = intImageActivityFlag
  intEnum_StepWidth = OOOPimscEnum_ImageActivityFlag % Enum_StepWidth
  call OOOPimsc_UnpackEnumValue (intPackedEnumValue, intEnum_StepWidth, &
                             intUnpackedEnumValue, intUnpackedAdditionalValue)
  intImageActivityFlag = intUnpackedEnumValue
  !
  if (intCheckImageActivityFlag == intImageActivityFlag) then
    call OOOPimsc_subSyncMemory (Object_CA) ! execute sync memory (ToDo: this must be implemented as an option)
    OOOPimscGAElement_check_atomic_intImageActivityFlag99_CA = .true.
  end if
  !
  if (present(intAdditionalAtomicValue)) then
    intAdditionalAtomicValue = intUnpackedAdditionalValue
  end if
  !
                                                                call OOOGglob_subResetProcedures
end function OOOPimscGAElement_check_atomic_intImageActivityFlag99_CA
!**********
!__________________________________________________________
!
!**********
subroutine OOOPimsc_subSyncMemory (Object_CA)
  type (OOOPimsc_adtImageStatus_CA), codimension[*], volatile, intent (inout) :: Object_CA
  ! encapsulates access to SYNC MEMORY
                                                                call OOOGglob_subSetProcedures &
                                                                 ("OOOPimsc_subSyncMemory")
  sync memory
  ! increment the ImageSyncMemoryCount to track the execution segment order
  ! on the executing image:
  call OOOPimscSAElement_atomic_increment_intImageSyncMemoryCount99_CA (Object_CA)
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimsc_subSyncMemory
!
!**********
! private:
subroutine OOOPimscSAElement_atomic_increment_intImageSyncMemoryCount99_CA (Object_CA)
  type (OOOPimsc_adtImageStatus_CA), codimension[*], volatile, intent (inout) :: Object_CA
  integer(OOOGglob_kint) :: intTemp
  integer(OOOGglob_kint) :: status = 0 ! error status
integer(OOOGglob_kint) :: test
  !
                                                                call OOOGglob_subSetProcedures &
                                                            ("OOOPimscSAElement_atomic_increment_intImageSyncMemoryCount99_CA")
  !
  ! increment (by 1) the ImageSyncMemoryCount member atomically on the executing image only:
  ! every image uses its own array index (this_image())
  !
  ! Fortran 2015 syntax:
  !call atomic_add(Object_CA % mA_atomic_intImageSyncMemoryCount99(this_image()), 1)
  !
  ! Fortran 2008 syntax:
  call atomic_ref(intTemp, Object_CA % mA_atomic_intImageSyncMemoryCount99 (this_image()))
  intTemp = intTemp + 1
  ! don't execute sync memory for local atomic_define:
  call atomic_define(Object_CA % mA_atomic_intImageSyncMemoryCount99 (this_image()), intTemp)
!
! test:
call atomic_ref(test, Object_CA % mA_atomic_intImageSyncMemoryCount99 (this_image()))
write(*,*) 'entering execution segment', test, 'on image', this_image()
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimscSAElement_atomic_increment_intImageSyncMemoryCount99_CA
!___________________________________________________________
!
subroutine OOOPimsc_SynchronizeSegmentOrdering_CA (Object_CA, intNumberOfImages,intA_RemoteImageNumbers)
  type (OOOPimsc_adtImageStatus_CA), codimension[*], volatile, intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent (in) :: intNumberOfImages
  integer(OOOGglob_kint), dimension (intNumberOfImages), intent (in) :: intA_RemoteImageNumbers
  integer(OOOGglob_kint) :: status = 0 ! error status
  !
                                                                call OOOGglob_subSetProcedures &
                                                            ("OOOPimsc_SynchronizeSegmentOrdering_CA")
  ! firstly, the execution on the involved images must be held within the current execution segment:
  call OOOPimsc_SynchronizeTheInvolvedImages_CA (Object_CA, intNumberOfImages,intA_RemoteImageNumbers)
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimsc_SynchronizeSegmentOrdering_CA
!___________________________________________________________
! private
subroutine OOOPimsc_SynchronizeTheInvolvedImages_CA (Object_CA, intNumberOfImages,intA_RemoteImageNumbers)
  ! firstly, the execution on the involved remote images must be held within the current execution segment
  !!!  synchronization counterpart routine  !!!!
  !!!  for IIimma_SYNC_CheckActivityFlag --> OOOPimsc_Start_ExecutionSegmentSynchronization_CA  !!!!
  type (OOOPimsc_adtImageStatus_CA), codimension[*], volatile, intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent (in) :: intNumberOfImages
  integer(OOOGglob_kint), dimension (intNumberOfImages), intent (in) :: intA_RemoteImageNumbers
  integer(OOOGglob_kint) :: status = 0 ! error status
  integer(OOOGglob_kint) :: intCount
  integer(OOOGglob_kint) :: intImageNumber
  integer(OOOGglob_kint) :: intImageActivityFlag
  integer(OOOGglob_kint) :: intSetFromImageNumber = 0
  logical(OOOGglob_klog), dimension (intNumberOfImages) :: logA_CheckImageStates
  integer(OOOGglob_kint) :: intPackedEnumValue
  !
                                                                call OOOGglob_subSetProcedures &
                                                            ("OOOPimsc_SynchronizeTheInvolvedImages_CA")
  !
  !************************************************
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
  !
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
    ! (be aware: this would be error prone in real world programming, due to the first if statement,
    !  but it is safe for this example program)
  end do
write(*,*) 'step 3: on image', this_image()
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimsc_SynchronizeTheInvolvedImages_CA
!___________________________________________________________
!
!#####################################################################################################################
!#####################################################################################################################
! From here, the routines are called from the ImageManager objects:
!___________________________________________________________
!
! public
subroutine OOOPimsc_Start_SegmentSynchronization_CA (Object_CA, intSetFromImageNumber)
  ! the involved images (not image 1) will execute this
  type (OOOPimsc_adtImageStatus_CA), codimension[*], volatile, intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent (in) :: intSetFromImageNumber ! this is the remote image number (image 1)
                                                               ! which initiated the synchronization
  integer(OOOGglob_kint) :: status = 0 ! error status
  integer(OOOGglob_kint) :: intImageActivityFlag
  integer(OOOGglob_kint) :: intPackedEnumValue
  integer(OOOGglob_kint) :: intRemoteImageNumber
  !
                                                                call OOOGglob_subSetProcedures &
                                                            ("OOOPimsc_Start_SegmentSynchronization_CA")
  !
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
  ! finish execution on the executing image (only for this example and to avoid an error stop statement
  ! to terminate execution):
  call OOOPimscSAElement_atomic_intImageActivityFlag99_CA (OOOPimscImageStatus_CA_1, OOOPimscEnum_ImageActivityFlag % &
                                    ExecutionFinished, this_image(), logExecuteSyncMemory = .false.)
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimsc_Start_SegmentSynchronization_CA
!__________________________________________________________


!##################################################################################################
!##################################################################################################
!##################################################################################################


!**************************
! coarray ADT management: *
!**************************
!___________________________________________________________
!
!
subroutine OOOPimsc_StructureConstructor_CA (Object_CA)
  type (OOOPimsc_adtImageStatus_CA), codimension[*], volatile, intent (inout) :: Object_CA
  !
                                                                call OOOGglob_subSetProcedures ("OOOPimsc_StructureConstructor_CA")

  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimsc_StructureConstructor_CA
!___________________________________________________________


!##################################################################################################
!##################################################################################################
!##################################################################################################

!**********************************************************
!*************  Error Handling:   *************************
!**********************************************************
!__________________________________________________________
!
!Private
subroutine IIimsc_ErrorHandler (Object, chrErrorDescription, &
  intErrorType, intErrorNumber)
  ! ErrorHandler for the ADT and CA routines
  type(OOOPimsc_adtImageStatus_CA), intent(inout) :: Object
  character(kind=1, len=*), intent(in) :: chrErrorDescription
  integer(OOOGglob_kint), intent(in) :: intErrorType ! 1=warning, 2=Severe System error
  integer(OOOGglob_kint), intent(in) :: intErrorNumber ! Run Time error Number (e.g. status)
  call OOOEerroc_AddObject (Object % m_UUerrocError, chrErrorDescription, &
    intErrorType)
end subroutine IIimsc_ErrorHandler
!__________________________________________________________
!
!Private
logical(OOOGglob_klog) function IIimsc_ImageNumberBoundError_CA (Object_CA, intImageNumber)
  ! error handling routine
  ! checks if the image number does exist
  type(OOOPimsc_adtImageStatus_CA), codimension[*], volatile, intent(inout) :: Object_CA
  integer(OOOGglob_kint), intent(in) :: intImageNumber
  !
  IIimsc_ImageNumberBoundError_CA = .false.
  !
  if (intImageNumber .lt. 1) then ! image number is too small
    IIimsc_ImageNumberBoundError_CA = .true.
  end if
  !
  if (intImageNumber .gt. num_images()) then ! image number is too large
    IIimsc_ImageNumberBoundError_CA = .true.
  end if
  !
end function IIimsc_ImageNumberBoundError_CA
!__________________________________________________________

end module OOOPimsc_admImageStatus_CA
