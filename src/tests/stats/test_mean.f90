module test_stats_mean
    use stdlib_test, only : new_unittest, unittest_type, error_type, check
    use stdlib_stats, only: mean
    use stdlib_kinds, only : int8, int16, int32, int64, sp, dp, qp
    use, intrinsic :: ieee_arithmetic, only : ieee_is_nan
    implicit none
    private

    public :: collect_stats_mean

    real(sp), parameter :: sptol = 1000 * epsilon(1._sp)
    real(dp), parameter :: dptol = 2000 * epsilon(1._dp)
    real(qp), parameter :: qptol = 2000 * epsilon(1._qp)

    integer(int8), parameter :: d1(16) = [10, 2, -3, 4, 6, -6, 7, -8, 9, 0, 1, 20, -9, 10, 14, 15]
    integer(int8), parameter :: d2(4, 4) = reshape(d1, [4, 4])
    integer(int8), parameter :: d3(2, 4, 2) = reshape(d1, [2, 4, 2])
    integer(int8), parameter :: d4(2, 2, 2, 2) = reshape(d1, [2, 2, 2, 2])

contains

    !> Collect all exported unit tests
    subroutine collect_stats_mean(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("test_stats_mean_all_int8", test_stats_mean_all_int8) &
            , new_unittest("test_stats_mean_all_optmask_int8", test_stats_mean_all_optmask_int8) &
            , new_unittest("test_stats_mean_int8", test_stats_mean_int8) &
            , new_unittest("test_stats_mean_optmask_int8", test_stats_mean_optmask_int8) &
            , new_unittest("test_stats_mean_mask_all_int8", test_stats_mean_mask_all_int8) &
            , new_unittest("test_stats_mean_mask_int8", test_stats_mean_mask_int8) &
            ]
    end subroutine collect_stats_mean

    subroutine test_stats_mean_all_int8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, mean(d1), sum(d1)/real(size(d1), dp)&
                    , 'mean(d1): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return

        call check(error, mean(d2), sum(d2)/real(size(d2), dp)&
                    , 'mean(d2): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return

        call check(error, mean(d3), sum(d3)/real(size(d3), dp)&
                    , 'mean(d3): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return

        call check(error, mean(d4), sum(d4)/real(size(d4), dp)&
                    , 'mean(d4): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return

    end subroutine

    subroutine test_stats_mean_all_optmask_int8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, ieee_is_nan(mean(d1, .false.))&
                    , 'mean(d1, .false.): uncorrect answer')
        if (allocated(error)) return

        call check(error, ieee_is_nan(mean(d2, .false.))&
                    , 'mean(d2, .false.): uncorrect answer')
        if (allocated(error)) return

        call check(error, ieee_is_nan(mean(d3, .false.))&
                    , 'mean(d3, .false.): uncorrect answer')
        if (allocated(error)) return

        call check(error, ieee_is_nan(mean(d4, .false.))&
                    , 'mean(d4, .false.): uncorrect answer')
        if (allocated(error)) return

    end subroutine

    subroutine test_stats_mean_int8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error&
                    , abs(mean(d1, 1) - sum(d1, 1)/real(size(d1, 1), dp)) <dptol&
                    , 'mean(d1, 1): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error&
                    , sum(abs(mean(d2, 1) - sum(d2, 1)/real(size(d2, 1), dp))) < dptol&
                    , 'mean(d2, 1): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error&
                    , sum(abs(mean(d3, 1) - sum(d3, 1)/real(size(d3, 1), dp))) < dptol&
                    , 'mean(d3, 1): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error&
                    , sum(abs(mean(d4, 1) - sum(d4, 1)/real(size(d4, 1), dp))) < dptol&
                    , 'mean(d4, 1): uncorrect answer'&
                    )
        if (allocated(error)) return

    end subroutine

    subroutine test_stats_mean_optmask_int8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, ieee_is_nan(mean(d1, 1, .false.))&
                    , 'mean(d1, 1, .false.): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error, any(ieee_is_nan(mean(d2, 1, .false.)))&
                    , 'mean(d2, 1, .false.): uncorrect answer')
        if (allocated(error)) return

        call check(error, any(ieee_is_nan(mean(d3, 1, .false.)))&
                    , 'mean(d3, 1, .false.): uncorrect answer')
        if (allocated(error)) return

        call check(error, any(ieee_is_nan(mean(d4, 1, .false.)))&
                    , 'mean(d4, 1, .false.): uncorrect answer')
        if (allocated(error)) return

    end subroutine

    subroutine test_stats_mean_mask_all_int8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, mean(d1, d1 > 0)&
                    , sum(d1, d1 > 0)/real(count(d1 > 0), dp)&
                    , 'mean(d1, d1 > 0): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return

        call check(error, mean(d2, d2 > 0)&
                    , sum(d2, d2 > 0)/real(count(d2 > 0), dp)&
                    , 'mean(d2, d2 > 0): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return

        call check(error, mean(d3, d3 > 0)&
                    , sum(d3, d3 > 0)/real(count(d3 > 0), dp)&
                    , 'mean(d3, d3 > 0): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return

        call check(error, mean(d4, d4 > 0)&
                    , sum(d4, d4 > 0)/real(count(d3 > 0), dp)&
                    , 'mean(d4, d4 > 0): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_mean_mask_int8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error&
                    , abs(mean(d1, 1, d1 > 0) - sum(d1, 1, d1 > 0)/real(count(d1 > 0, 1), dp)) < dptol&
                    , 'mean(d1, 1, d1 > 0): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error&
                    , sum(abs(mean(d2, 1, d2 > 0) - sum(d2, 1, d2 > 0)/real(count(d2 > 0, 1), dp))) < dptol&
                    , 'mean(d2, 1, d2 > 0): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error&
                    , sum(abs(mean(d3, 1, d3 > 0) - sum(d3, 1, d3 > 0)/real(count(d3 > 0, 1), dp))) < dptol&
                    , 'mean(d3, 1, d3 > 0): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error&
                    , sum(abs(mean(d4, 1, d4 > 0) - sum(d4, 1, d4 > 0)/real(count(d4 > 0, 1), dp))) < dptol&
                    , 'mean(d4, 1, d4 > 0): uncorrect answer'&
                    )
        if (allocated(error)) return

    end subroutine

end module test_stats_mean

program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use stdlib_test, only : run_testsuite, new_testsuite, testsuite_type
    use test_stats_mean, only : collect_stats_mean
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("stats_mean", collect_stats_mean) &
        ]

    do is = 1, size(testsuites)
        write(error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if
end program
