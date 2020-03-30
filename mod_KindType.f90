MODULE KindType
  implicit none
  save
  !
  integer,     parameter :: ip = 4
  integer,     parameter :: rp = 4
  integer(ip), parameter :: s_name  = 64
  integer(ip), parameter :: s_mess  = 512
  integer(ip), parameter :: s_file  = 512
  !
  type ERROR_STATUS
     integer(ip)       :: flag 
     character(s_mess) :: source 
     character(s_mess) :: message 
  end type ERROR_STATUS
  !
  type FILE_LIST
     integer(ip)       :: lulog        = 10
     integer(ip)       :: luinp        = 11
     integer(ip)       :: luout        = 12
     character(s_file) :: commonpath   = ' '
     character(s_file) :: problempath  = ' '
     character(s_file) :: file_log     = ' '
     character(s_file) :: file_inp     = 'input.nml'
     character(s_file) :: file_output  = 'output.dat'
     character(s_file) :: file_outnc   = 'output.nc'
  end type FILE_LIST
  !
  type CONFIG_PARAMS
      integer(ip)      :: ta !asympthomatic time
      integer(ip)      :: ts !sympthomatic time
      integer(ip)      :: tmax
      real(rp)         :: qmin
      real(rp)         :: qmax
      integer(ip)      :: threshold_cases
      real(rp)         :: pmin
      real(rp)         :: pmax
      integer(ip)      :: N 
      integer(ip)      :: start_rec
      integer(ip)      :: end_rec
  end type CONFIG_PARAMS
  !
  type NETWORK
      integer(ip)              :: N          !linear dimension
      integer(ip), allocatable :: nn(:)      !number of neighbours
      integer(ip), allocatable :: con(:,:)   !connectivity array
      integer(ip), allocatable :: ix(:)
      integer(ip), allocatable :: iy(:)
  end type NETWORK
  !
  type SIR
      type(NETWORK)            :: net
      integer(ip), allocatable :: phase(:)   !infection phase           (N**2)
      integer(ip), allocatable :: IR(:)      !nodes I or R              (nir)
      integer(ip)              :: ni         !number of active infected
      integer(ip)              :: nir        !number of total infected
      integer(ip)              :: t          !time step
      real(rp)                 :: q          !probabiliy of infection per unit time and contact
  end type SIR
  !
END MODULE KindType
