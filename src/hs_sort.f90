module hs_sort

  implicit none

  integer, parameter, public :: dp = kind(1.0d0)
  private
  public :: hsort, isort, qsort


contains 

subroutine HSORT(LIST, KEY)
  implicit none
  integer, intent(inout) :: LIST(:)
  double precision, intent(in) :: KEY(:)

  integer, parameter :: MAXSTK = 32, NCUT = 12
  integer :: LL, LR, LM, NL, NR, LTEMP, STKTOP, I, J, K
  integer :: LSTACK(MAXSTK), RSTACK(MAXSTK)
  double precision :: GUESS, VALUE
  logical :: quicksort_done
  integer :: N 
  N = size(LIST, 1)
  LL = 1
  LR = N
  STKTOP = 0

  quicksort_done = .false.

  do while (.not. quicksort_done)

    if ((LR - LL) >= NCUT) then
       NL = LL
       NR = LR
       LM = (LL + LR) / 2
       GUESS = KEY(LIST(LM))

       do
          do while (KEY(LIST(NL)) < GUESS)
             NL = NL + 1
          end do

          do while (GUESS < KEY(LIST(NR)))
             NR = NR - 1
          end do

          if (NL < (NR - 1)) then
             LTEMP = LIST(NL)
             LIST(NL) = LIST(NR)
             LIST(NR) = LTEMP
             NL = NL + 1
             NR = NR - 1
          else
             exit
          end if
       end do

       if (NL <= NR) then
          if (NL < NR) then
             LTEMP = LIST(NL)
             LIST(NL) = LIST(NR)
             LIST(NR) = LTEMP
          end if
          NL = NL + 1
          NR = NR - 1
       end if

       STKTOP = STKTOP + 1
       if (NR < LM) then
          LSTACK(STKTOP) = NL
          RSTACK(STKTOP) = LR
          LR = NR
       else
          LSTACK(STKTOP) = LL
          RSTACK(STKTOP) = NR
          LL = NL
       end if

    else if (STKTOP /= 0) then
       LL = LSTACK(STKTOP)
       LR = RSTACK(STKTOP)
       STKTOP = STKTOP - 1

    else
       quicksort_done = .true.
    end if

  end do

  ! Second pass: Insertion sort for final ordering

  J = 1
  K = LIST(1)
  VALUE = KEY(K)
  do I = 2, min(N, NCUT)
     if (KEY(LIST(I)) < VALUE) then
        J = I
        VALUE = KEY(LIST(I))
     end if
  end do
  LIST(1) = LIST(J)
  LIST(J) = K

  do I = 2, N
     J = I
     K = LIST(I)
     VALUE = KEY(K)
     do while (J > 1 .and. VALUE < KEY(LIST(J-1)))
        LIST(J) = LIST(J-1)
        J = J - 1
     end do
     LIST(J) = K
  end do

end subroutine HSORT

subroutine ISORT(LIST, KEY)
  implicit none
  integer, intent(inout) :: LIST(:)
  double precision, intent(in) :: KEY(:)

  integer :: N
  integer :: I, J, K
  double precision :: VALUE

  ! Find the index with the minimum key and move it to the front
  N = size(LIST, 1)
  J = 1
  K = LIST(1)
  VALUE = KEY(K)
  do I = 2, N
     if (KEY(LIST(I)) < VALUE) then
        J = I
        VALUE = KEY(LIST(I))
     end if
  end do
  LIST(1) = LIST(J)
  LIST(J) = K

  ! Insertion sort
  do I = 2, N
     J = I
     K = LIST(I)
     VALUE = KEY(K)
     do while (J > 1 .and. VALUE < KEY(LIST(J-1)))
        LIST(J) = LIST(J-1)
        J = J - 1
     end do
     LIST(J) = K
  end do

end subroutine ISORT

subroutine QSORT(LIST, KEY)
  implicit none
  integer, intent(inout) :: LIST(:)
  double precision, intent(in) :: KEY(:)

  integer, parameter :: MAXSTK = 32
  integer :: LL, LR, LM, NL, NR, LTEMP, STKTOP
  integer :: LSTACK(MAXSTK), RSTACK(MAXSTK)
  double precision :: GUESS
  logical :: done

  LL = 1
  LR = size(LIST, 1)
  STKTOP = 0
  done = .false.

  do while (.not. done)
     if (LL < LR) then
        NL = LL
        NR = LR
        LM = (LL + LR) / 2
        GUESS = KEY(LIST(LM))

        ! Partition the array
        do
           do while (KEY(LIST(NL)) < GUESS)
              NL = NL + 1
           end do

           do while (GUESS < KEY(LIST(NR)))
              NR = NR - 1
           end do

           if (NL < (NR - 1)) then
              LTEMP = LIST(NL)
              LIST(NL) = LIST(NR)
              LIST(NR) = LTEMP
              NL = NL + 1
              NR = NR - 1
           else
              exit
           end if
        end do

        if (NL <= NR) then
           if (NL < NR) then
              LTEMP = LIST(NL)
              LIST(NL) = LIST(NR)
              LIST(NR) = LTEMP
           end if
           NL = NL + 1
           NR = NR - 1
        end if

        ! Push the larger sub-list to the stack and continue with the smaller
        STKTOP = STKTOP + 1
        if (NR < LM) then
           LSTACK(STKTOP) = NL
           RSTACK(STKTOP) = LR
           LR = NR
        else
           LSTACK(STKTOP) = LL
           RSTACK(STKTOP) = NR
           LL = NL
        end if

     else if (STKTOP > 0) then
        LL = LSTACK(STKTOP)
        LR = RSTACK(STKTOP)
        STKTOP = STKTOP - 1
     else
        done = .true.
     end if
  end do

end subroutine QSORT




end module
