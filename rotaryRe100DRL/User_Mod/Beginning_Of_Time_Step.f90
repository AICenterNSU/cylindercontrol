!==============================================================================!
  subroutine User_Mod_Beginning_Of_Time_Step(flow, turb, mult, swarm, n, time)
!------------------------------------------------------------------------------!
!   This function is called at the beginning of time step.                     !
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use File_Mod
  use Const_Mod, only: NANO
  use Comm_Mod,  only: Comm_Mod_Global_Sum_Real, Comm_Mod_Wait
  use Work_Mod, only: r_vars => r_cell_30,  &
                      i_vars => i_cell_04

  !  n_Ctrl          = i_vars(1)
  !
  !  time            = r_vars(1)
  !  Omega_Ctrl      = r_vars(2)
  !  Omega           = r_vars(3)
  !  Fx_mean         = r_vars(4)
  !  Fy_mean         = r_vars(5)
  !  Freq_Ctrl       = r_vars(6)
  !  angle           = r_vars(7)

!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Field_Type),      target :: flow
  type(Turb_Type),       target :: turb
  type(Multiphase_Type), target :: mult
  type(Swarm_Type),      target :: swarm
  integer                       :: n     ! time step
  real                          :: time  ! physical time
  integer                       :: first_n ! starting timestep
!-----------------------------------[Locals]-----------------------------------!
  type(Grid_Type), pointer :: grid
  type(Var_Type),  pointer :: u, v, w
  real, parameter          :: Omega1 = 1.0 ! Forcing amplitude
  real, parameter          :: St = 0.143   ! Strouhal number, Re=100, from POD
  integer                  :: c1, c2, s, fu
  logical                  :: controlFileExists = .false.
  character(len=80)        :: file_name
  integer                  :: n_Ctrl
  integer                  :: stepCounter
  real                     :: Omega_Ctrl, Omega, Freq_Ctrl
  integer, parameter       :: n_rel = 5
!==============================================================================!

  ! Take aliases
  grid => flow % pnt_grid
  u    => flow % u
  v    => flow % v
  w    => flow % w

  n_Ctrl     = i_vars(1)  ! Acquire n_Ctrl
  stepCounter    = i_vars(2)
  ! time
  Omega_Ctrl = r_vars(2)  ! Acquire Omega_Ctrl
  Omega      = r_vars(3)  ! Acquire Omega_Ctrl_pre

  if ( this_proc < 2 ) then
    print *, 'stepCounter', stepCounter
  end if

  ! set first step values
  if ( stepCounter.eq.0 ) then
    Omega_Ctrl = 0!2 * sin(2 * PI * 3 * St * time)
    Omega = Omega_Ctrl
  end if

  ! Fx_mean
  ! Fy_mean
  Freq_Ctrl  = r_vars(6)

  if ( stepCounter.eq.1 .or. mod(stepCounter, n_Ctrl).eq.0 .and. stepCounter.ne.0 ) then  ! stop here

    Omega_Ctrl = 0
    Freq_Ctrl  = 0

    if ( this_proc < 2) then

      file_name = 'reg_control.dat'
      ! wait until the input file will be created
      inquire(file=trim(file_name), exist=controlFileExists)
      do while (controlFileExists .eqv. .false.)
          !print *, 'Current control value:', Omega_Ctrl
          call sleep(1)
          inquire(file=trim(file_name), exist=controlFileExists)
          print *, 'waiting for the ', trim(file_name), '...'
      end do

      ! read input file
      open(newunit = fu, file=trim(file_name), action='read')
      call File_Mod_Read_Line(fu)
      read(line % tokens(1), *) Omega_Ctrl
      read(line % tokens(2), *) Freq_Ctrl
      print *, 'Omega_Ctrl, Freq_Ctrl:', Omega_Ctrl, Freq_Ctrl
      close(fu, status='delete')
      !print *, 'stop here untill a key pressed'
      !read (*,*)
      !call sleep(1)

    end if ! Main proc

    call Comm_Mod_Wait
    call Comm_Mod_Global_Sum_Real(Omega_Ctrl)
    call Comm_Mod_Global_Sum_Real(Freq_Ctrl)

    ! Update time
    r_vars(1) = time
    ! Update Omega_Ctrl
    r_vars(2) = Omega_Ctrl
    ! Update Freq_Ctrl
    r_vars(6) = Freq_Ctrl

  end if ! new Omega_Ctrl

  !---------------------------------------------!
  !   Set tangential velocity on the boundary   !
  !---------------------------------------------!
  !   Vx = - y / R U_inf Omega sin(2*pi*St*t)   !
  !   Vy =   x / R U_inf Omega sin(2*pi*St*t)   !
  !---------------------------------------------!
  !   Relaxation for Omega:                     !
  !   \Omega(t) = \Omega_0 \exp(\alpha(t-t_0))  !
  !   \Omega(t_0) = \Omega_0                    !
  !   \Omega(t_1) = \Omega_1                    !
  !   \alpha = ln(\Omega_1/\Omega_0)/(t1-t0)    !
  !   d\Omega(t)/dt = \alpha \Omega(t)          !
  !   \Omega_n+1 = \Omega_n (1 + \alpha dt*)    !
  !   dt* is not simulation dt ,                !
  !   but dt* = n_rel * dt - how many dt it     !
  !   takes to reach \Omega_1                   !
  !---------------------------------------------!

  ! Omega with relaxation
  !Omega = Omega + 1./n_rel * (Omega_Ctrl - Omega)
  ! linear
  Omega = Omega + 1./(n_Ctrl - mod(stepCounter, n_Ctrl)) * (Omega_Ctrl - Omega)
  ! step
  !Omega = Omega_Ctrl

  !if ( this_proc < 2 ) then
  !  open(newunit = fu, file='reg_control_log.dat', position='append')
  !  write(fu,'(I9,2E17.7E3)') n, Omega, 2 * sin(2 * PI * St * 3 * time)
  !  close(fu)
  !end if

  ! Update Omega_Ctrl_prev
  r_vars(3) = Omega

  ! check relaxation
  !if ( this_proc < 2 ) then
  !  print *, "Omega, Omega_Ctrl, time-time_prev", Omega, Omega_Ctrl, time - r_vars(1)
  !  call sleep(1)
  !end if ! check relaxation

  do s = 1, grid % n_faces ! all faces
    c1 = grid % faces_c(1,s)
    c2 = grid % faces_c(2,s)

    if(c2 < 0) then ! b.c. cells
      if(Grid_Mod_Bnd_Cond_Type(grid,c2) .eq. WALL .or.  &
        Grid_Mod_Bnd_Cond_Type(grid,c2) .eq. WALLFL) then ! wall
        ! amp sin control
        !u % n(c2) = -2.0 * grid % yc(c2) * Omega * sin(2*PI*St*time)
        !v % n(c2) =  2.0 * grid % xc(c2) * Omega * sin(2*PI*St*time)
        ! direct control
        u % n(c2) = -2.0 * grid % yc(c2) * Omega
        v % n(c2) =  2.0 * grid % xc(c2) * Omega
        ! amp+freq sin control
        !u % n(c2) = -2.0 * grid % yc(c2) * Omega * sin(2*PI*St*Freq_Ctrl*time)
        !v % n(c2) =  2.0 * grid % xc(c2) * Omega * sin(2*PI*St*Freq_Ctrl*time)
      end if ! wall
    end if ! bc cells
  end do ! all faces

  end subroutine
