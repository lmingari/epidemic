MODULE Model
    use KindType
    use Shared
    implicit none
    save
    !
    integer, parameter          :: MAX_NN=60
    type(NETWORK)               :: net_local
    integer(ip), allocatable    :: Inn(:)
    !
    contains
    !
    subroutine model_init(p,N)
        implicit none
        !
        real(rp),    intent(in) :: p
        integer(ip), intent(in) :: N
        !
        integer(ip) :: infected_node
        !
        net_local%N = N
        call model_local_links(net_local)
        !
        MY_MODEL%net = net_local
        !
        call random_seed()
        !
        call model_add_shortcuts(p)
        !
        allocate(MY_MODEL%phase(N**2))
        allocate(MY_MODEL%IR   (N**2))
        allocate(Inn           (N**2))
        !
        MY_MODEL%phase = 0
        MY_MODEL%IR    = 0
        MY_MODEL%ni    = 1
        MY_MODEL%nir   = 1
        MY_MODEL%t     = 0
        !
        infected_node  = int((N**2+N)*0.5,ip)
        MY_MODEL%IR(1) = infected_node
        MY_MODEL%phase(infected_node) = 1
        !
    end subroutine model_init
    !
    subroutine model_end
        implicit none
        !
        deallocate(MY_MODEL%net%nn  )
        deallocate(MY_MODEL%net%con )
        deallocate(MY_MODEL%net%ix  )
        deallocate(MY_MODEL%net%iy  )
        deallocate(MY_MODEL%phase   )
        deallocate(MY_MODEL%IR      )
        !
        deallocate(Inn)
        !
    end subroutine model_end
    !
    subroutine model_timestep(q)
        implicit none
        !
        real(rp), intent(in) :: q
        !
        integer(ip) :: i,is,ti
        integer(ip) :: ni, nir, N
        integer(ip) :: new_i, old_i
        integer(ip) :: S_node,I_node
        real(rp), dimension(MAX_NN) :: prob_infection
        real(rp)                    :: r
        !
        prob_infection = (/ (1-(1-q)**i,i=1,MAX_NN)  /)
        !
        N   = MY_MODEL%net%N
        nir = MY_MODEL%nir
        ni  = MY_MODEL%ni
        ti  = MY_CONFIG%ta + MY_CONFIG%ts
        !
        Inn = 0
        !
        !$OMP PARALLEL DO PRIVATE(i,is,I_node,S_node)
        infected: do i=nir-ni+1,nir
            I_node = MY_MODEL%IR(i)
            susceptibles: do is=1,MY_MODEL%net%nn(I_node)
                S_node = MY_MODEL%net%con(I_node,is)
                if(MY_MODEL%phase(S_node).eq.0) then
                    !$OMP CRITICAL
                    Inn(S_node) = Inn(S_node) + 1
                    !$OMP END CRITICAL
                end if
            end do susceptibles
        end do infected
        !$OMP END PARALLEL DO
        !
        ! Update next step
        new_i = 0
        old_i = count(MY_MODEL%phase.eq.ti)
        where (MY_MODEL%phase.gt.0) MY_MODEL%phase = MY_MODEL%phase + 1
        !
        do i=1,N**2
            if(Inn(i).gt.0) then
                call random_number(r)
                if(r.le.prob_infection(Inn(i))) then
                    new_i = new_i + 1
                    MY_MODEL%IR(nir+new_i) = i
                    MY_MODEL%phase(i) = 1
                end if
            end if
        end do
        !
        MY_MODEL%ni  = MY_MODEL%ni  + new_i - old_i
        MY_MODEL%nir = MY_MODEL%nir + new_i
        MY_MODEL%t   = MY_MODEL%t + 1
        !
    end subroutine model_timestep
    !
    subroutine model_local_links(net)
        implicit none
        type(NETWORK), intent(inout) :: net
        !
        integer(ip) :: i,nn,N
        integer(ip) :: ix,iy
        !
        N = net%N
        allocate(net%nn(N**2))
        allocate(net%con(N**2,MAX_NN))
        allocate(net%ix(N**2))
        allocate(net%iy(N**2))
        net%nn  = 0
        net%con = 0
        !
        i=0
        do iy = 1,N
        do ix = 1,N
          i = i + 1
          net%ix(i) = ix
          net%iy(i) = iy
          if(ix.ne.1) then
              nn = net%nn(i) + 1  
              net%nn(i)     = nn   !number of neighbours
              net%con(i,nn) = i-1  !index of neighbours
              if(iy.gt.1) then
                  nn = net%nn(i) + 1  
                  net%nn(i)     = nn 
                  net%con(i,nn) = i-1-N
              end if
              if(iy.lt.N) then
                  nn = net%nn(i) + 1  
                  net%nn(i)     = nn 
                  net%con(i,nn) = i-1+N
              end if
          end if
          if(ix.ne.N) then
              nn = net%nn(i) + 1  
              net%nn(i)     = nn 
              net%con(i,nn) = i+1
              if(iy.gt.1) then
                  nn = net%nn(i) + 1  
                  net%nn(i)     = nn 
                  net%con(i,nn) = i+1-N
              end if
              if(iy.lt.N) then
                  nn = net%nn(i) + 1  
                  net%nn(i)     = nn 
                  net%con(i,nn) = i+1+N
              end if
          end if
          if(iy.gt.1) then
              nn = net%nn(i) + 1  
              net%nn(i)     = nn 
              net%con(i,nn) = i-N
          end if
          if(iy.lt.N) then
              nn = net%nn(i) + 1  
              net%nn(i)     = nn 
              net%con(i,nn) = i+N
          end if
        end do
        end do
        !
    end subroutine
    !
    subroutine model_add_shortcuts(p)
        implicit none
        !
        real(rp), intent(in) :: p
        real(rp) :: r1, r2
        !
        integer(ip) :: L,N
        integer(ip) :: i1,i2,nn1,nn2,i_add
        !
        N = MY_MODEL%net%N
        L = nint(4*p*N**2,ip)
        !
        i_add = L
        !
        do
            call random_number(r1)
            call random_number(r2)
            i1  = int(N**2*r1,ip)+1
            i2  = int(N**2*r2,ip)+1
            if(i1.eq.i2) cycle
            nn1 = MY_MODEL%net%nn(i1)
            if(ANY(MY_MODEL%net%con(i1,1:nn1).eq.i2)) cycle
            nn2 = MY_MODEL%net%nn(i2)
            MY_MODEL%net%nn(i1) = nn1 + 1 
            MY_MODEL%net%nn(i2) = nn2 + 1
            MY_MODEL%net%con(i1,nn1+1) = i2
            MY_MODEL%net%con(i2,nn2+1) = i1
            i_add = i_add - 1
            !
            if(i_add.eq.0) exit
        end do
        !
        if( MAXVAL(MY_MODEL%net%nn)>MAX_NN) then
            write(*,*) "Maximum number of neighbours exceeded. Decrease p"
            STOP 1
        end if
        !
    end subroutine model_add_shortcuts
    !
    subroutine model_init_network(p)
        implicit none
        !
        real(rp), intent(in) :: p
        !
        MY_MODEL%net = net_local
        if(p.gt.0) call model_add_shortcuts(p)
        !
    end subroutine model_init_network
    !
    function model_get_q(qmin,qmax,threshold) result(q)
        implicit none
        real(rp),    intent(in) :: qmin
        real(rp),    intent(in) :: qmax
        integer(ip), intent(in) :: threshold
        real(rp)                :: q
        !
        real(rp) :: inv_threshold
        !
        inv_threshold = 1.0_rp/threshold
        q = (qmax-qmin) * exp(-inv_threshold*MY_MODEL%nir) + qmin
        !
    end function model_get_q
    !
END MODULE Model
