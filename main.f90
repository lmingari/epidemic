program main
    use KindType
    use Shared
    use InpOut
    use Model
    !
    implicit none
    !
    integer(ip)       :: nargs, counter
    CHARACTER(len=32) :: arg
    !
    nargs = command_argument_count()
    IF (nargs .gt. 0) then
        CALL get_command_argument(1, arg)
        MY_FILES%file_inp = TRIM(arg)
    else
        WRITE(*,*) "Error"
        STOP 1
    endif
    !
    call inpout_read_config
    call inpout_print_greeting
    !
    call model_init(MY_CONFIG%pmax,MY_CONFIG%N)
!    call inpout_init_nc
    !
    counter=0
    open(MY_FILES%luout,file=MY_FILES%file_output,status='unknown')
    do
        if(MY_MODEL%nir-MY_MODEL%ni.ge.100) counter = counter + 1
        write(MY_FILES%luout,*) MY_MODEL%t, MY_MODEL%ni, MY_MODEL%nir-MY_MODEL%ni
!        call inpout_save_nc
        MY_MODEL%q = model_get_q(MY_CONFIG%qmin,MY_CONFIG%qmax,MY_CONFIG%threshold_cases)
        call model_timestep(MY_MODEL%q)
        if(counter.eq.MY_CONFIG%start_rec) call model_init_network(MY_CONFIG%pmin)
        if(counter.eq.MY_CONFIG%end_rec)   call model_init_network(MY_CONFIG%pmax)
        if(MY_MODEL%ni.eq.0 .or. MY_MODEL%t.eq.MY_CONFIG%tmax) EXIT
    end do
    close(MY_FILES%luout)
    !
    call model_end
!    call inpout_close_nc
    !
end program main
