subroutine setprob()

    use adjoint_module, only: read_adjoint_data, set_time_window

    implicit none

    !# Defining time window of interest
    real(kind=8) :: t1, t2
    t1 = 3.5*3600.
    t2 = 11*3600.

    call set_time_window(t1, t2)      !# Set time window
    call read_adjoint_data()          !# Reading adjoint data

end subroutine setprob
