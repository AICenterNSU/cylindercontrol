!==============================================================================!
  subroutine User_Mod_Calculate_Mean(turb, n0, n1)
!------------------------------------------------------------------------------!
!   User-defined calculation of time-averaged values.                          !
!------------------------------------------------------------------------------!
!---------------------------------[Modules]------------------------------------!
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
  type(Turb_Type), target   :: turb
  integer                   :: n0, n1
!-----------------------------------[Locals]-----------------------------------!
  type(Field_Type), pointer :: flow
  type(Grid_Type),  pointer :: grid
  type(Var_Type),   pointer :: u, v, w, p
  integer                   :: c, n, s, c1, c2, fu
  real                      :: Fx, Fy, Fpx, Fpy, Fvx, Fvy
  character(len=80)         :: file

  integer                   :: n_Ctrl
  integer                   :: stepCounter
  real                      :: time
  real                      :: angle, Omega, Omega_Ctrl
  real                      :: Fx_mean, Fy_mean
!==============================================================================!

  !if(.not. turbulence_statistics) return

  n = n1 - n0

  ! Take aliases
  flow => turb % pnt_flow
  grid => flow % pnt_grid
  u    => flow % u
  v    => flow % v
  w    => flow % w
  p    => flow % p

  n_Ctrl      = i_vars(1)
  stepCounter = i_vars(2)

  time  = r_vars(1)
  Omega_Ctrl = r_vars(2)
  Omega      = r_vars(3)
  Fx_mean = r_vars(4)
  Fx_mean = r_vars(5)
  ! Freq_Ctrl
  angle   = r_vars(7)

  ! Init value
  Fx  = 0.
  Fy  = 0.
  Fpx = 0.
  Fpy = 0.
  Fvx = 0.
  Fvy = 0.

  ! Force calculation block
  do s = 1, grid % n_faces ! all faces
    c1 = grid % faces_c(1,s)
    c2 = grid % faces_c(2,s)

    if(c2 < 0) then ! b.c. cells
      if(Grid_Mod_Bnd_Cond_Type(grid,c2) .eq. WALL .or.  &
         Grid_Mod_Bnd_Cond_Type(grid,c2) .eq. WALLFL) then ! wall
        Fpx = Fpx + p % n(c1)*grid % sx(s)
        Fpy = Fpy + p % n(c1)*grid % sy(s)
        Fvx = Fvx + flow % viscosity(c1)*  &
              (u % x(c1)*grid % sx(s) + u % y(c1)*grid % sy(s))
        Fvy = Fvy + flow % viscosity(c1)*  &
              (v % x(c1)*grid % sx(s) + v % y(c1)*grid % sy(s))
      end if ! wall
    end if ! bc cells
  end do ! all faces

  ! Total force
  Fx = Fpx - Fvx
  Fy = Fpy + Fvy

  ! Summ through all procs
  call Comm_Mod_Global_Sum_Real(Fx)
  call Comm_Mod_Global_Sum_Real(Fy)
  call Comm_Mod_Global_Sum_Real(Fpx)
  call Comm_Mod_Global_Sum_Real(Fpy)
  call Comm_Mod_Global_Sum_Real(Fvx)
  call Comm_Mod_Global_Sum_Real(Fvy)

  if ( stepCounter.eq.0 .or. mod(stepCounter+1, n_Ctrl).eq.0 ) then
    call User_Mod_Save_Results2(flow, turb, n1)
  end if

  ! Omega is defined in User_Mod_Beginning_Of_Time_Step
  angle = angle + flow % dt * Omega

  if(this_proc < 2) then

    Fx_mean = r_vars(4)
    Fy_mean = r_vars(5)

    Fx_mean = Fx_mean + Fx
    Fy_mean = Fy_mean + Fy

    ! save state every n_Ctrl timesteps
    if ( stepCounter.eq.0 .or. mod(stepCounter+1, n_Ctrl).eq.0 ) then

      !call User_Mod_Save_Results2(flow, turb)

      Fx_mean = Fx_mean / n_Ctrl
      Fy_mean = Fy_mean / n_Ctrl

      open(newunit = fu, file='reg_sensor.dat', action='write')
      write(fu,*) Fx_mean, Fy_mean, n1
      print *, "Data was written to reg_sensor.dat as", Fx_mean, Fy_mean, n1
      print *, "Compare with the instant data", Fx, Fy
      close(fu)

      file='force_avg.dat'
      if ( stepCounter.eq.0 ) then
        call File_Mod_Open_File_For_Writing(file, fu, this_proc)
        close(fu)
      else
        call File_Mod_Append_File_For_Writing(file, fu, this_proc)
        write(fu,'(I9,3E17.7E3)') n1, time, Fx_mean, Fy_mean
        close(fu)
      end if

      file='angle.dat'
      if ( stepCounter.eq.0 ) then
        call File_Mod_Open_File_For_Writing(file, fu, this_proc)
        close(fu)
      else
        call File_Mod_Append_File_For_Writing(file, fu, this_proc)
        write(fu,'(I9,4E17.7E3)')      &
            n1,                          &
            time,                        &
            mod(angle, PI) / PI * 180.,  &
            Omega,                       &
            Omega_Ctrl
        close(fu)
      end if

      Fx_mean = 0
      Fy_mean = 0

    end if ! save state every n_Ctrl timesteps

    r_vars(4) = Fx_mean
    r_vars(5) = Fy_mean

   ! each 10 ts
    if (mod(n1, 10) .eq. 0) then
      file='force.dat'
      if (n1 .eq. 1) then
        call File_Mod_Open_File_For_Writing(file, fu, this_proc)
      else
        call File_Mod_Append_File_For_Writing(file, fu, this_proc)
      end if
      write(fu,'(I9,7E17.7E3)') n1, time, Fx, Fy, Fpx, Fpy, Fvx, Fvy
      close(fu)
    end if

  end if ! this_proc < 2

  call Comm_Mod_Wait

  ! increment stepCounter from run
  i_vars(2) = stepCounter + 1

  ! Update angle
  r_vars(7) = angle

! The way how Vladimir washes away a vortex on outflow
  do c = -grid % n_bnd_cells, grid % n_cells
    if (grid % xc(c) > 13. ) then ! 2D before outflow
      turb % vis_t(c) = turb % vis_t(c) +  (13. - grid % xc(c)) / 2. * turb % vis_t(c)
    end if
  end do

  end subroutine
