!==============================================================================!
  subroutine User_Mod_Save_Results(flow, turb, mult, swrm, time_step)
!------------------------------------------------------------------------------!
!   This subroutines interpolates vars on regular mesh                         !
!     and saves them in file                                                   !
!---------------------------------[Modules]------------------------------------!
  use Const_Mod, only: HUGE
  use Work_Mod, only: r_vars => r_cell_30
  !  time  = r_vars(1)
!------------------------------------------------------------------------------!
  implicit none
!--------------------------------[Arguments]-----------------------------------!
  type(Field_Type),      target :: flow
  type(Turb_Type),       target :: turb
  type(Multiphase_Type), target :: mult
  type(Swarm_Type),      target :: swrm
  integer                       :: time_step
!----------------------------------[Locals]------------------------------------!
  character(len=80)             :: store_name, name_out

  type(Var_Type),   pointer     :: u, v, w, p
  type(Grid_Type),  pointer     :: grid

  integer                       :: c, fu, fi
  character(len=10)             :: file
  character(len=:), allocatable :: tmp

  real                          :: xi, yi, zi, dx, dy, xmin, xmax, ymin, ymax
  integer                       :: ix, iy, nx, ny, c_min, stat
  real                          :: ui, vi, wi, pi
  real                          :: rx, ry, rz, min_dist, sq_dist,  &
                                   min_dist_glob
  logical                       :: exist
  real                          :: time_tot
!==============================================================================!

  call Cpu_Timer_Mod_Start('User_Mod_Save_Results')

  ! Take aliases
  grid => flow % pnt_grid
  !bulk => flow % bulk
  u    => flow % u
  v    => flow % v
  w    => flow % w
  p    => flow % p

  ! Interpolation on regular mesh
  ! phi = phi_0 + r_j * d phi / d x_j

  ! x : [-2  ; 8  ]
  ! y : [-2.5; 2.5]
  ! dx = 0.25
  ! dy = 0.25
  xmax = 8
  xmin = -2
  ymax = 2.5
  ymin = -2.5
  dx = 0.125
  dy = 0.125
  nx = int((xmax - xmin)/dx)
  ny = int((ymax - ymin)/dy)
  zi = 0.5

  file = 'nnnnnn.dat'
  write(file(1:6), '(I6.6)') time_step

  call Comm_Mod_Wait

  ! If file exists -> delete it
  open(newunit = fi, iostat=stat, FILE=file, status='replace')
  close(fi)

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

        ui = u % n(c_min)       &
           + u % x(c_min) * rx  &
           + u % y(c_min) * ry  &
           + u % z(c_min) * rz

        vi = v % n(c_min)       &
           + v % x(c_min) * rx  &
           + v % y(c_min) * ry  &
           + v % z(c_min) * rz

        wi = w % n(c_min)       &
           + w % x(c_min) * rx  &
           + w % y(c_min) * ry  &
           + w % z(c_min) * rz

        pi = p % n(c_min)       &
           + p % x(c_min) * rx  &
           + p % y(c_min) * ry  &
           + p % z(c_min) * rz

        inquire(FILE=file, exist=exist)
        if (exist) then
          open(newunit = fi, file=file, status='old', action='write', position='append')
        else
          open(newunit = fi, file=file, status='new', action='write')
        end if

        write (fi,'(8ES17.7E3)')  &
          xi,                     & ! 1 - x
          yi,                     & ! 2 - y
          ui,                     & ! 3 - u
          vi,                     & ! 4 - v
          wi,                     & ! 5 - w
          pi                        ! 6 - p
      close(fi) ! 'nnnnnn.dat'

      end if ! min_dist .eq. min_dist_glob

    end do ! 1, ny
  end do ! 1, nx

  call Cpu_Timer_Mod_Stop('User_Mod_Save_Results')
  if(this_proc < 2)  write(*, *) '# Finished with User_Mod_Save_Results.f90.'

  end subroutine

  include 'User_Mod/Save_Results2.f90'