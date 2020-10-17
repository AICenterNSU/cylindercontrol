!==============================================================================!
  subroutine User_Mod_Save_Results2(flow, turb, time_step)
!------------------------------------------------------------------------------!
!   This subroutines interpolates vars on regular mesh                         !
!     and saves them in file                                                   !
!---------------------------------[Modules]------------------------------------!
  use Const_Mod, only: HUGE
  use Work_Mod,  only: om_z => r_cell_01
!------------------------------------------------------------------------------!
  implicit none
!--------------------------------[Arguments]-----------------------------------!
  type(Field_Type),      target :: flow
  type(Turb_Type),       target :: turb
  integer                       :: time_step
!----------------------------------[Locals]------------------------------------!
  !character(len=80)             :: store_name, name_out
  type(Var_Type),   pointer     :: u, v, w, p
  type(Var_Type),   pointer     :: uu, vv, ww, uv, uw, vw
  type(Grid_Type),  pointer     :: grid

  integer                       :: c, fu
  character(len=13)             :: file_inst, file_mean
  character(len=15)             :: file_sens
  character(len=80)             :: rel_path, sys_comm
  character(len=:), allocatable :: tmp

  real                          :: xi, yi, zi, dx, dy, xmin, xmax, ymin, ymax
  integer                       :: ix, iy, nx, ny, c_min, stat
  real                          :: pi, om_zi
  real                          :: rx, ry, rz, min_dist, sq_dist,  &
                                   min_dist_glob
  logical                       :: exist
  real                          :: time_tot
!==============================================================================!

  ! Take aliases
  grid => flow % pnt_grid
  !bulk => flow % bulk
  u  => flow % u
  v  => flow % v
  w  => flow % w
  p  => flow % p
  uu => turb % uu
  vv => turb % vv
  ww => turb % ww
  uv => turb % uv
  uw => turb % uw
  vw => turb % vw

  ! ----------------------------------------------------------------------------
  ! z-vorticity slice
  ! ----------------------------------------------------------------------------

  ! z-vorticity
  om_z = v % x - u % y

  ! x: [-2  ; 8  ]
  ! y: [-2.5; 2.5]
  ! dx = 0.25
  ! dy = 0.25

  xmin = -2
  xmax =  8
  ymin = -2.5
  ymax =  2.5
  dx = 0.125
  dy = 0.125

  nx = int((xmax - xmin) / dx)
  ny = int((ymax - ymin) / dy)
  zi = 1.33 / 2.

  rel_path = 'slices/'
  sys_comm = 'mkdir -p ' // trim(rel_path)
  call system(trim(sys_comm))

  ! nnnnnn = ts
  file_inst = rel_path // 'nnnnnn'

  write(file_inst(8:13), '(I6.6)') time_step

  ! If file exists -> delete it
  call File_Mod_Open_File_For_Writing(file_inst, fu, 2)
  if(this_proc < 2) write (fu,'(4A17)')  &
                      '#1:x',            &
                      ' 2:y',            &
                      ' 3:p',            &
                      ' 4:om_z'
  close(fu)
  call Comm_Mod_Wait

  do ix = 0, nx
    xi = xmin + dx * ix
    do iy = 0, ny
      yi = ymin + dy * iy

    ! Interpolation on regular mesh
    ! phi = phi_0 + r_j * d phi / d x_j

      min_dist = HUGE
      do c = 1, grid % n_cells - grid % comm % n_buff_cells
        sq_dist = (xi-grid % xc(c))**2+(yi-grid % yc(c))**2+(zi-grid % zc(c))**2

        if(sq_dist < min_dist) then
          min_dist = sq_dist  ! new minimum distance
          c_min = c           ! c_min is the closest inside cell index
        end if

      end do ! 1, grid % n_cells

      min_dist_glob = min_dist
      if(n_proc > 1) then
        call Comm_Mod_Global_Min_Real(min_dist_glob)
      end if

      if( min_dist .eq. min_dist_glob ) then

        ! Vector which connects particle position and cell centre
        rx = xi - grid % xc(c_min)
        ry = yi - grid % yc(c_min)
        rz = zi - grid % zc(c_min)

        pi = p % n(c_min)       &
           + p % x(c_min) * rx  &
           + p % y(c_min) * ry  &
           + p % z(c_min) * rz

        om_zi = om_z(c_min)       &
              + om_z(c_min) * rx  &
              + om_z(c_min) * ry  &
              + om_z(c_min) * rz

        call File_Mod_Append_File_For_Writing(file_inst, fu, 2)
        write (fu,'(4ES17.7E3)') &
          xi,                    &  ! 1:x
          yi,                    &  ! 2:y
          pi,                    &  ! 3:p
          om_zi                     ! 4:om_z

        close(fu) ! 'nnnnnn.dat'

      end if ! min_dist .eq. min_dist_glob

    end do ! 1, ny
  end do ! 1, nx

  if(this_proc < 2) print *,  &
    '# Finished with User_Mod_Save_Results.f90: exported structured slice'

  ! ----------------------------------------------------------------------------
  ! Pressure sensors
  ! ----------------------------------------------------------------------------

  ! Interpolation on regular mesh
  ! phi = phi_0 + r_j * d phi / d x_j

  ! x : [-2  ; 8  ]
  ! y : [-2.5; 2.5]
  ! dx = 0.25
  ! dy = 0.25
  xmax = 4
  xmin = 1
  ymax = 1
  ymin = -1
  dx = 1
  dy = 1
  nx = int((xmax - xmin)/dx)
  ny = int((ymax - ymin)/dy)
  zi = 0.5

  file_sens = 'reg_sensor2.dat'

  ! If file exists -> delete it
  call File_Mod_Open_File_For_Writing(file_sens, fu, 2)
  close(fu)

  call Comm_Mod_Wait

  do ix = 0, nx
    xi = xmin + dx * real(ix)
    do iy = 0, ny
      yi = ymin + dy * real(iy)

      min_dist = HUGE
      do c = 1, grid % n_cells - grid % comm % n_buff_cells
        sq_dist = (xi-grid % xc(c))**2 + (yi-grid % yc(c))**2

        if(sq_dist < min_dist) then
          min_dist = sq_dist  ! new minimum distance
          c_min = c           ! c_min is the closest inside cell index
        end if

      end do ! 1, grid % n_cells

      min_dist_glob = min_dist
      if(n_proc > 1) then
        call Comm_Mod_Global_Min_Real(min_dist_glob)
      end if

      if( min_dist .eq. min_dist_glob ) then

        ! Vector which connects particle position and cell centre
        rx = xi - grid % xc(c_min)
        ry = yi - grid % yc(c_min)
        rz = zi - grid % zc(c_min)

        pi = p % n(c_min)       &
           + p % x(c_min) * rx  &
           + p % y(c_min) * ry  &
           + p % z(c_min) * rz

        call File_Mod_Append_File_For_Writing(file_sens, fu, 2)
        write (fu,'(ES17.7E3)') pi
        close(fu) ! 'reg_sensor2.dat'

      end if ! min_dist .eq. min_dist_glob

    end do ! 1, ny
  end do ! 1, nx

  if(this_proc < 2)  write(*, *) '# Finished with User_Mod_Save_Results2.f90.'

  end subroutine
