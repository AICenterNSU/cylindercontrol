!==============================================================================!
  subroutine User_Mod_Allocate(grid)
!------------------------------------------------------------------------------!
!   This function allocates memory for user-defined variables.                 !
!---------------------------------[Modules]------------------------------------!
!------------------------------------------------------------------------------!
  use Work_Mod, only: i_vars => i_cell_04
  !  n_Ctrl = i_vars(1)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Grid_Type), target :: grid
!-----------------------------------[Locals]-----------------------------------!
  integer :: us, ua, fu
  integer :: n_Ctrl
!==============================================================================!

  !-----------------------------!
  !   User scalars and arrays   !
  !-----------------------------!
  call Control_Mod_Number_Of_User_Arrays(grid % n_user_arrays,  &
                                         verbose = .true.)

  !-------------------------------------!
  !   Allocate memory for user arrays   !
  !-------------------------------------!
  allocate(grid % user_array(grid % n_user_arrays,  &
                             -grid % n_bnd_cells:grid % n_cells))
  do ua = 1, grid % n_user_arrays
    grid % user_array(ua,:) = 0.
  end do

  ! Read n_Ctrl
  open(newunit = fu, file='freq.dat', action='read')
  read(fu,*) n_Ctrl
  close(fu)

  ! Update n_Ctrl
  i_vars(1) = n_Ctrl

  end subroutine
