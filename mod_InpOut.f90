MODULE InpOut
  use KindType
  use Shared
  use netcdf
  implicit none
  save
  !
  PRIVATE
  !
  PUBLIC :: inpout_read_config
  PUBLIC :: inpout_print_greeting
  PUBLIC :: inpout_init_nc
  PUBLIC :: inpout_save_nc
  PUBLIC :: inpout_close_nc
  !
  integer(ip)  :: ncID
  !
  integer(ip)  :: xdimID
  integer(ip)  :: ydimID
  integer(ip)  :: tdimID
  integer(ip)  :: stateID
  integer(ip)  :: nirID
  integer(ip)  :: niID
  
  character(len=48 ) :: attr_units
  character(len=128) :: attr_title

  integer(ip), allocatable :: work2d(:,:)

  contains
      subroutine inpout_read_config
          implicit none
          !
          integer(ip)     :: my_iostat
          character (256) :: my_iomsg
          !
          integer(ip) :: TA,TS,TMAX
          integer(ip) :: THRESHOLD_CASES
          integer(ip) :: START_TIME,END_TIME,N
          real(rp) :: QMIN,QMAX,PMIN,PMAX
          !
          namelist /TIME/ TA,TS,TMAX
          namelist /PROBABILITY/ QMIN,QMAX,THRESHOLD_CASES
          namelist /NETWORK/ PMIN,PMAX,N
          namelist /RECLUSION/ START_TIME,END_TIME
          !
          open(MY_FILES%luinp,file=MY_FILES%file_inp, iostat=my_iostat, iomsg=my_iomsg, status='old')
          if(my_iostat.ne.0) then
              write(*,*) trim(my_iomsg)
              STOP 1
          else
              read(MY_FILES%luinp,nml=TIME)
              read(MY_FILES%luinp,nml=PROBABILITY)
              read(MY_FILES%luinp,nml=NETWORK)
              read(MY_FILES%luinp,nml=RECLUSION)
          end if
          close(MY_FILES%luinp)
          !
          MY_CONFIG%ta   = TA
          MY_CONFIG%ts   = TS
          MY_CONFIG%tmax = TMAX
          !
          MY_CONFIG%qmin = QMIN
          MY_CONFIG%qmax = QMAX
          MY_CONFIG%threshold_cases = THRESHOLD_CASES
          !
          MY_CONFIG%pmin = PMIN
          MY_CONFIG%pmax = PMAX
          MY_CONFIG%N    = N
          !
          MY_CONFIG%start_rec = START_TIME
          MY_CONFIG%end_rec   = END_TIME
          !
      end subroutine inpout_read_config
      !
      subroutine inpout_print_greeting
          implicit none
          !
          write(*,*)'Input file:                 ', TRIM(MY_FILES%file_inp)
          write(*,*)'Tiempo asintomatico:        ', MY_CONFIG%ta
          write(*,*)'Tiempo sintomatico:         ', MY_CONFIG%ts
          write(*,*)'Maximum time:               ', MY_CONFIG%tmax
          write(*,*)'Min. Infection probability: ', MY_CONFIG%qmin
          write(*,*)'Max. Infection probability: ', MY_CONFIG%qmax
          write(*,*)'Min. shortcuts density:     ', MY_CONFIG%pmin
          write(*,*)'Max. shortcuts density:     ', MY_CONFIG%pmax
          write(*,*)'Network linear size:        ', MY_CONFIG%N
          write(*,*)'Reclusion start time:       ', MY_CONFIG%start_rec
          write(*,*)'Reclusion end time:         ', MY_CONFIG%end_rec
          !
      end subroutine inpout_print_greeting
      !
  subroutine inpout_init_nc
      implicit none
      !
      integer(ip)           :: istat, mode_flag
      character(len=s_file) :: nc_file
      !
      nc_file = MY_FILES%file_outnc
      mode_flag = IOR(NF90_CLOBBER,NF90_NETCDF4)
      istat = nf90_create(TRIM(nc_file),     &
                          cmode = mode_flag, &
                          ncid  = ncID)

      istat = nf90_def_dim(ncID, 'x', MY_MODEL%net%N, xdimID)
      istat = nf90_def_dim(ncID, 'y', MY_MODEL%net%N, ydimID)
      istat = nf90_def_dim(ncID, 't', NF90_UNLIMITED, tdimID)

      attr_title = 'state'
      attr_units = 'S:0 I:1 R:2'
      istat = nf90_def_var(ncID, 'state', NF90_INT, (/xdimID,ydimID,tdimID/), stateID)
      istat = nf90_put_att(ncID, stateID, 'title', attr_title)
      istat = nf90_put_att(ncID, stateID, 'units', attr_units)

      attr_title = 'number of I or R'
      attr_units = '#'
      istat = nf90_def_var(ncID, 'nir', NF90_INT, (/tdimID/), nirID)
      istat = nf90_put_att(ncID, nirID, 'title', attr_title)
      istat = nf90_put_att(ncID, nirID, 'units', attr_units)

      attr_title = 'number of I'
      attr_units = '#'
      istat = nf90_def_var(ncID, 'ni', NF90_INT, (/tdimID/), niID)
      istat = nf90_put_att(ncID, niID, 'title', attr_title)
      istat = nf90_put_att(ncID, niID, 'units', attr_units)

      istat = nf90_put_att(ncID, NF90_GLOBAL, 'Model','SIR')
      istat = nf90_put_att(ncID, NF90_GLOBAL, 'Infection_probability', MY_MODEL%q)
      istat = nf90_put_att(ncID, NF90_GLOBAL, 'Author','LAM')

      istat = nf90_enddef(ncID)

      allocate(work2d(MY_MODEL%net%N,MY_MODEL%net%N))

  end subroutine inpout_init_nc
      !
  subroutine inpout_save_nc
      implicit none
      !
      integer(ip) :: istat
      integer(ip) :: i,inode,ix,iy,it,nir,ni,N
      integer     :: start(3), count(3)
      !
      nir = MY_MODEL%nir
      ni  = MY_MODEL%ni 
      N   = MY_MODEL%net%N
      !
      work2d = 0
      do i=1,nir-ni
        inode = MY_MODEL%IR(i)
        ix = MY_MODEL%net%ix(inode)
        iy = MY_MODEL%net%iy(inode)
        work2d(ix,iy) = 2
      end do
      do i=nir-ni+1,nir
        inode = MY_MODEL%IR(i)
        ix = MY_MODEL%net%ix(inode)
        iy = MY_MODEL%net%iy(inode)
        work2d(ix,iy) = 1
      end do
      !
      it    = 1 + MY_MODEL%t
      start = (/ 1,1,it /)
      count = (/ N,N,1 /)
      istat = nf90_put_var(ncID, stateID, &
                           work2d,        &
                           start = start, &
                           count = count)      

      istat = nf90_put_var(ncID, nirID,   &
                           (/MY_MODEL%nir/),  &
                           start = (/it/),    &
                           count = (/1/))      

      istat = nf90_put_var(ncID, niID,    &
                           (/MY_MODEL%ni/),   &
                           start = (/it/),    &
                           count = (/1/))                      
      !
  end subroutine inpout_save_nc
      !
  subroutine inpout_close_nc
      implicit none
      !
      integer(ip) :: istat
      !
      istat = nf90_close(ncID)
      !
      deallocate(work2d)
      !
  end subroutine inpout_close_nc
      !
END MODULE InpOut
