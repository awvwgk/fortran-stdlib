submodule(stdlib_lapack_solve) stdlib_lapack_solve_chol_comp
  implicit none


  contains

     pure module subroutine stdlib_spocon( uplo, n, a, lda, anorm, rcond, work, iwork,info )
     !! SPOCON estimates the reciprocal of the condition number (in the
     !! 1-norm) of a real symmetric positive definite matrix using the
     !! Cholesky factorization A = U**T*U or A = L*L**T computed by SPOTRF.
     !! An estimate is obtained for norm(inv(A)), and the reciprocal of the
     !! condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           real(sp), intent(in) :: anorm
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           character :: normin
           integer(ilp) :: ix, kase
           real(sp) :: ainvnm, scale, scalel, scaleu, smlnum
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( anorm<zero ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SPOCON', -info )
              return
           end if
           ! quick return if possible
           rcond = zero
           if( n==0_ilp ) then
              rcond = one
              return
           else if( anorm==zero ) then
              return
           end if
           smlnum = stdlib_slamch( 'SAFE MINIMUM' )
           ! estimate the 1-norm of inv(a).
           kase = 0_ilp
           normin = 'N'
           10 continue
           call stdlib_slacn2( n, work( n+1 ), work, iwork, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              if( upper ) then
                 ! multiply by inv(u**t).
                 call stdlib_slatrs( 'UPPER', 'TRANSPOSE', 'NON-UNIT', normin, n, a,lda, work, &
                           scalel, work( 2_ilp*n+1 ), info )
                 normin = 'Y'
                 ! multiply by inv(u).
                 call stdlib_slatrs( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', normin, n,a, lda, work, &
                           scaleu, work( 2_ilp*n+1 ), info )
              else
                 ! multiply by inv(l).
                 call stdlib_slatrs( 'LOWER', 'NO TRANSPOSE', 'NON-UNIT', normin, n,a, lda, work, &
                           scalel, work( 2_ilp*n+1 ), info )
                 normin = 'Y'
                 ! multiply by inv(l**t).
                 call stdlib_slatrs( 'LOWER', 'TRANSPOSE', 'NON-UNIT', normin, n, a,lda, work, &
                           scaleu, work( 2_ilp*n+1 ), info )
              end if
              ! multiply by 1/scale if doing so will not cause overflow.
              scale = scalel*scaleu
              if( scale/=one ) then
                 ix = stdlib_isamax( n, work, 1_ilp )
                 if( scale<abs( work( ix ) )*smlnum .or. scale==zero )go to 20
                 call stdlib_srscl( n, scale, work, 1_ilp )
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm/=zero )rcond = ( one / ainvnm ) / anorm
           20 continue
           return
     end subroutine stdlib_spocon

     pure module subroutine stdlib_dpocon( uplo, n, a, lda, anorm, rcond, work, iwork,info )
     !! DPOCON estimates the reciprocal of the condition number (in the
     !! 1-norm) of a real symmetric positive definite matrix using the
     !! Cholesky factorization A = U**T*U or A = L*L**T computed by DPOTRF.
     !! An estimate is obtained for norm(inv(A)), and the reciprocal of the
     !! condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           real(dp), intent(in) :: anorm
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           character :: normin
           integer(ilp) :: ix, kase
           real(dp) :: ainvnm, scale, scalel, scaleu, smlnum
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( anorm<zero ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DPOCON', -info )
              return
           end if
           ! quick return if possible
           rcond = zero
           if( n==0_ilp ) then
              rcond = one
              return
           else if( anorm==zero ) then
              return
           end if
           smlnum = stdlib_dlamch( 'SAFE MINIMUM' )
           ! estimate the 1-norm of inv(a).
           kase = 0_ilp
           normin = 'N'
           10 continue
           call stdlib_dlacn2( n, work( n+1 ), work, iwork, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              if( upper ) then
                 ! multiply by inv(u**t).
                 call stdlib_dlatrs( 'UPPER', 'TRANSPOSE', 'NON-UNIT', normin, n, a,lda, work, &
                           scalel, work( 2_ilp*n+1 ), info )
                 normin = 'Y'
                 ! multiply by inv(u).
                 call stdlib_dlatrs( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', normin, n,a, lda, work, &
                           scaleu, work( 2_ilp*n+1 ), info )
              else
                 ! multiply by inv(l).
                 call stdlib_dlatrs( 'LOWER', 'NO TRANSPOSE', 'NON-UNIT', normin, n,a, lda, work, &
                           scalel, work( 2_ilp*n+1 ), info )
                 normin = 'Y'
                 ! multiply by inv(l**t).
                 call stdlib_dlatrs( 'LOWER', 'TRANSPOSE', 'NON-UNIT', normin, n, a,lda, work, &
                           scaleu, work( 2_ilp*n+1 ), info )
              end if
              ! multiply by 1/scale if doing so will not cause overflow.
              scale = scalel*scaleu
              if( scale/=one ) then
                 ix = stdlib_idamax( n, work, 1_ilp )
                 if( scale<abs( work( ix ) )*smlnum .or. scale==zero )go to 20
                 call stdlib_drscl( n, scale, work, 1_ilp )
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm/=zero )rcond = ( one / ainvnm ) / anorm
           20 continue
           return
     end subroutine stdlib_dpocon


     pure module subroutine stdlib_cpocon( uplo, n, a, lda, anorm, rcond, work, rwork,info )
     !! CPOCON estimates the reciprocal of the condition number (in the
     !! 1-norm) of a complex Hermitian positive definite matrix using the
     !! Cholesky factorization A = U**H*U or A = L*L**H computed by CPOTRF.
     !! An estimate is obtained for norm(inv(A)), and the reciprocal of the
     !! condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           real(sp), intent(in) :: anorm
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           character :: normin
           integer(ilp) :: ix, kase
           real(sp) :: ainvnm, scale, scalel, scaleu, smlnum
           complex(sp) :: zdum
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( anorm<zero ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CPOCON', -info )
              return
           end if
           ! quick return if possible
           rcond = zero
           if( n==0_ilp ) then
              rcond = one
              return
           else if( anorm==zero ) then
              return
           end if
           smlnum = stdlib_slamch( 'SAFE MINIMUM' )
           ! estimate the 1-norm of inv(a).
           kase = 0_ilp
           normin = 'N'
           10 continue
           call stdlib_clacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              if( upper ) then
                 ! multiply by inv(u**h).
                 call stdlib_clatrs( 'UPPER', 'CONJUGATE TRANSPOSE', 'NON-UNIT',normin, n, a, lda,&
                            work, scalel, rwork, info )
                 normin = 'Y'
                 ! multiply by inv(u).
                 call stdlib_clatrs( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', normin, n,a, lda, work, &
                           scaleu, rwork, info )
              else
                 ! multiply by inv(l).
                 call stdlib_clatrs( 'LOWER', 'NO TRANSPOSE', 'NON-UNIT', normin, n,a, lda, work, &
                           scalel, rwork, info )
                 normin = 'Y'
                 ! multiply by inv(l**h).
                 call stdlib_clatrs( 'LOWER', 'CONJUGATE TRANSPOSE', 'NON-UNIT',normin, n, a, lda,&
                            work, scaleu, rwork, info )
              end if
              ! multiply by 1/scale if doing so will not cause overflow.
              scale = scalel*scaleu
              if( scale/=one ) then
                 ix = stdlib_icamax( n, work, 1_ilp )
                 if( scale<cabs1( work( ix ) )*smlnum .or. scale==zero )go to 20
                 call stdlib_csrscl( n, scale, work, 1_ilp )
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm/=zero )rcond = ( one / ainvnm ) / anorm
           20 continue
           return
     end subroutine stdlib_cpocon

     pure module subroutine stdlib_zpocon( uplo, n, a, lda, anorm, rcond, work, rwork,info )
     !! ZPOCON estimates the reciprocal of the condition number (in the
     !! 1-norm) of a complex Hermitian positive definite matrix using the
     !! Cholesky factorization A = U**H*U or A = L*L**H computed by ZPOTRF.
     !! An estimate is obtained for norm(inv(A)), and the reciprocal of the
     !! condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           real(dp), intent(in) :: anorm
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           character :: normin
           integer(ilp) :: ix, kase
           real(dp) :: ainvnm, scale, scalel, scaleu, smlnum
           complex(dp) :: zdum
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( anorm<zero ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZPOCON', -info )
              return
           end if
           ! quick return if possible
           rcond = zero
           if( n==0_ilp ) then
              rcond = one
              return
           else if( anorm==zero ) then
              return
           end if
           smlnum = stdlib_dlamch( 'SAFE MINIMUM' )
           ! estimate the 1-norm of inv(a).
           kase = 0_ilp
           normin = 'N'
           10 continue
           call stdlib_zlacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              if( upper ) then
                 ! multiply by inv(u**h).
                 call stdlib_zlatrs( 'UPPER', 'CONJUGATE TRANSPOSE', 'NON-UNIT',normin, n, a, lda,&
                            work, scalel, rwork, info )
                 normin = 'Y'
                 ! multiply by inv(u).
                 call stdlib_zlatrs( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', normin, n,a, lda, work, &
                           scaleu, rwork, info )
              else
                 ! multiply by inv(l).
                 call stdlib_zlatrs( 'LOWER', 'NO TRANSPOSE', 'NON-UNIT', normin, n,a, lda, work, &
                           scalel, rwork, info )
                 normin = 'Y'
                 ! multiply by inv(l**h).
                 call stdlib_zlatrs( 'LOWER', 'CONJUGATE TRANSPOSE', 'NON-UNIT',normin, n, a, lda,&
                            work, scaleu, rwork, info )
              end if
              ! multiply by 1/scale if doing so will not cause overflow.
              scale = scalel*scaleu
              if( scale/=one ) then
                 ix = stdlib_izamax( n, work, 1_ilp )
                 if( scale<cabs1( work( ix ) )*smlnum .or. scale==zero )go to 20
                 call stdlib_zdrscl( n, scale, work, 1_ilp )
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm/=zero )rcond = ( one / ainvnm ) / anorm
           20 continue
           return
     end subroutine stdlib_zpocon




     pure module subroutine stdlib_spotrf( uplo, n, a, lda, info )
     !! SPOTRF computes the Cholesky factorization of a real symmetric
     !! positive definite matrix A.
     !! The factorization has the form
     !! A = U**T * U,  if UPLO = 'U', or
     !! A = L  * L**T,  if UPLO = 'L',
     !! where U is an upper triangular matrix and L is lower triangular.
     !! This is the block version of the algorithm, calling Level 3 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, jb, nb
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SPOTRF', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! determine the block size for this environment.
           nb = stdlib_ilaenv( 1_ilp, 'SPOTRF', uplo, n, -1_ilp, -1_ilp, -1_ilp )
           if( nb<=1_ilp .or. nb>=n ) then
              ! use unblocked code.
              call stdlib_spotrf2( uplo, n, a, lda, info )
           else
              ! use blocked code.
              if( upper ) then
                 ! compute the cholesky factorization a = u**t*u.
                 do j = 1, n, nb
                    ! update and factorize the current diagonal block and test
                    ! for non-positive-definiteness.
                    jb = min( nb, n-j+1 )
                    call stdlib_ssyrk( 'UPPER', 'TRANSPOSE', jb, j-1, -one,a( 1_ilp, j ), lda, one, a(&
                               j, j ), lda )
                    call stdlib_spotrf2( 'UPPER', jb, a( j, j ), lda, info )
                    if( info/=0 )go to 30
                    if( j+jb<=n ) then
                       ! compute the current block row.
                       call stdlib_sgemm( 'TRANSPOSE', 'NO TRANSPOSE', jb, n-j-jb+1,j-1, -one, a( &
                                 1_ilp, j ), lda, a( 1_ilp, j+jb ),lda, one, a( j, j+jb ), lda )
                       call stdlib_strsm( 'LEFT', 'UPPER', 'TRANSPOSE', 'NON-UNIT',jb, n-j-jb+1, &
                                 one, a( j, j ), lda,a( j, j+jb ), lda )
                    end if
                 end do
              else
                 ! compute the cholesky factorization a = l*l**t.
                 do j = 1, n, nb
                    ! update and factorize the current diagonal block and test
                    ! for non-positive-definiteness.
                    jb = min( nb, n-j+1 )
                    call stdlib_ssyrk( 'LOWER', 'NO TRANSPOSE', jb, j-1, -one,a( j, 1_ilp ), lda, one,&
                               a( j, j ), lda )
                    call stdlib_spotrf2( 'LOWER', jb, a( j, j ), lda, info )
                    if( info/=0 )go to 30
                    if( j+jb<=n ) then
                       ! compute the current block column.
                       call stdlib_sgemm( 'NO TRANSPOSE', 'TRANSPOSE', n-j-jb+1, jb,j-1, -one, a( &
                                 j+jb, 1_ilp ), lda, a( j, 1_ilp ),lda, one, a( j+jb, j ), lda )
                       call stdlib_strsm( 'RIGHT', 'LOWER', 'TRANSPOSE', 'NON-UNIT',n-j-jb+1, jb, &
                                 one, a( j, j ), lda,a( j+jb, j ), lda )
                    end if
                 end do
              end if
           end if
           go to 40
           30 continue
           info = info + j - 1_ilp
           40 continue
           return
     end subroutine stdlib_spotrf

     pure module subroutine stdlib_dpotrf( uplo, n, a, lda, info )
     !! DPOTRF computes the Cholesky factorization of a real symmetric
     !! positive definite matrix A.
     !! The factorization has the form
     !! A = U**T * U,  if UPLO = 'U', or
     !! A = L  * L**T,  if UPLO = 'L',
     !! where U is an upper triangular matrix and L is lower triangular.
     !! This is the block version of the algorithm, calling Level 3 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, jb, nb
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DPOTRF', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! determine the block size for this environment.
           nb = stdlib_ilaenv( 1_ilp, 'DPOTRF', uplo, n, -1_ilp, -1_ilp, -1_ilp )
           if( nb<=1_ilp .or. nb>=n ) then
              ! use unblocked code.
              call stdlib_dpotrf2( uplo, n, a, lda, info )
           else
              ! use blocked code.
              if( upper ) then
                 ! compute the cholesky factorization a = u**t*u.
                 do j = 1, n, nb
                    ! update and factorize the current diagonal block and test
                    ! for non-positive-definiteness.
                    jb = min( nb, n-j+1 )
                    call stdlib_dsyrk( 'UPPER', 'TRANSPOSE', jb, j-1, -one,a( 1_ilp, j ), lda, one, a(&
                               j, j ), lda )
                    call stdlib_dpotrf2( 'UPPER', jb, a( j, j ), lda, info )
                    if( info/=0 )go to 30
                    if( j+jb<=n ) then
                       ! compute the current block row.
                       call stdlib_dgemm( 'TRANSPOSE', 'NO TRANSPOSE', jb, n-j-jb+1,j-1, -one, a( &
                                 1_ilp, j ), lda, a( 1_ilp, j+jb ),lda, one, a( j, j+jb ), lda )
                       call stdlib_dtrsm( 'LEFT', 'UPPER', 'TRANSPOSE', 'NON-UNIT',jb, n-j-jb+1, &
                                 one, a( j, j ), lda,a( j, j+jb ), lda )
                    end if
                 end do
              else
                 ! compute the cholesky factorization a = l*l**t.
                 do j = 1, n, nb
                    ! update and factorize the current diagonal block and test
                    ! for non-positive-definiteness.
                    jb = min( nb, n-j+1 )
                    call stdlib_dsyrk( 'LOWER', 'NO TRANSPOSE', jb, j-1, -one,a( j, 1_ilp ), lda, one,&
                               a( j, j ), lda )
                    call stdlib_dpotrf2( 'LOWER', jb, a( j, j ), lda, info )
                    if( info/=0 )go to 30
                    if( j+jb<=n ) then
                       ! compute the current block column.
                       call stdlib_dgemm( 'NO TRANSPOSE', 'TRANSPOSE', n-j-jb+1, jb,j-1, -one, a( &
                                 j+jb, 1_ilp ), lda, a( j, 1_ilp ),lda, one, a( j+jb, j ), lda )
                       call stdlib_dtrsm( 'RIGHT', 'LOWER', 'TRANSPOSE', 'NON-UNIT',n-j-jb+1, jb, &
                                 one, a( j, j ), lda,a( j+jb, j ), lda )
                    end if
                 end do
              end if
           end if
           go to 40
           30 continue
           info = info + j - 1_ilp
           40 continue
           return
     end subroutine stdlib_dpotrf


     pure module subroutine stdlib_cpotrf( uplo, n, a, lda, info )
     !! CPOTRF computes the Cholesky factorization of a complex Hermitian
     !! positive definite matrix A.
     !! The factorization has the form
     !! A = U**H * U,  if UPLO = 'U', or
     !! A = L  * L**H,  if UPLO = 'L',
     !! where U is an upper triangular matrix and L is lower triangular.
     !! This is the block version of the algorithm, calling Level 3 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, jb, nb
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CPOTRF', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! determine the block size for this environment.
           nb = stdlib_ilaenv( 1_ilp, 'CPOTRF', uplo, n, -1_ilp, -1_ilp, -1_ilp )
           if( nb<=1_ilp .or. nb>=n ) then
              ! use unblocked code.
              call stdlib_cpotrf2( uplo, n, a, lda, info )
           else
              ! use blocked code.
              if( upper ) then
                 ! compute the cholesky factorization a = u**h *u.
                 do j = 1, n, nb
                    ! update and factorize the current diagonal block and test
                    ! for non-positive-definiteness.
                    jb = min( nb, n-j+1 )
                    call stdlib_cherk( 'UPPER', 'CONJUGATE TRANSPOSE', jb, j-1,-one, a( 1_ilp, j ), &
                              lda, one, a( j, j ), lda )
                    call stdlib_cpotrf2( 'UPPER', jb, a( j, j ), lda, info )
                    if( info/=0 )go to 30
                    if( j+jb<=n ) then
                       ! compute the current block row.
                       call stdlib_cgemm( 'CONJUGATE TRANSPOSE', 'NO TRANSPOSE', jb,n-j-jb+1, j-1,&
                                  -cone, a( 1_ilp, j ), lda,a( 1_ilp, j+jb ), lda, cone, a( j, j+jb ),lda )
                       call stdlib_ctrsm( 'LEFT', 'UPPER', 'CONJUGATE TRANSPOSE','NON-UNIT', jb, &
                                 n-j-jb+1, cone, a( j, j ),lda, a( j, j+jb ), lda )
                    end if
                 end do
              else
                 ! compute the cholesky factorization a = l*l**h.
                 do j = 1, n, nb
                    ! update and factorize the current diagonal block and test
                    ! for non-positive-definiteness.
                    jb = min( nb, n-j+1 )
                    call stdlib_cherk( 'LOWER', 'NO TRANSPOSE', jb, j-1, -one,a( j, 1_ilp ), lda, one,&
                               a( j, j ), lda )
                    call stdlib_cpotrf2( 'LOWER', jb, a( j, j ), lda, info )
                    if( info/=0 )go to 30
                    if( j+jb<=n ) then
                       ! compute the current block column.
                       call stdlib_cgemm( 'NO TRANSPOSE', 'CONJUGATE TRANSPOSE',n-j-jb+1, jb, j-1,&
                                  -cone, a( j+jb, 1_ilp ),lda, a( j, 1_ilp ), lda, cone, a( j+jb, j ),lda )
                       call stdlib_ctrsm( 'RIGHT', 'LOWER', 'CONJUGATE TRANSPOSE','NON-UNIT', n-j-&
                                 jb+1, jb, cone, a( j, j ),lda, a( j+jb, j ), lda )
                    end if
                 end do
              end if
           end if
           go to 40
           30 continue
           info = info + j - 1_ilp
           40 continue
           return
     end subroutine stdlib_cpotrf

     pure module subroutine stdlib_zpotrf( uplo, n, a, lda, info )
     !! ZPOTRF computes the Cholesky factorization of a complex Hermitian
     !! positive definite matrix A.
     !! The factorization has the form
     !! A = U**H * U,  if UPLO = 'U', or
     !! A = L  * L**H,  if UPLO = 'L',
     !! where U is an upper triangular matrix and L is lower triangular.
     !! This is the block version of the algorithm, calling Level 3 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, jb, nb
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZPOTRF', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! determine the block size for this environment.
           nb = stdlib_ilaenv( 1_ilp, 'ZPOTRF', uplo, n, -1_ilp, -1_ilp, -1_ilp )
           if( nb<=1_ilp .or. nb>=n ) then
              ! use unblocked code.
              call stdlib_zpotrf2( uplo, n, a, lda, info )
           else
              ! use blocked code.
              if( upper ) then
                 ! compute the cholesky factorization a = u**h *u.
                 do j = 1, n, nb
                    ! update and factorize the current diagonal block and test
                    ! for non-positive-definiteness.
                    jb = min( nb, n-j+1 )
                    call stdlib_zherk( 'UPPER', 'CONJUGATE TRANSPOSE', jb, j-1,-one, a( 1_ilp, j ), &
                              lda, one, a( j, j ), lda )
                    call stdlib_zpotrf2( 'UPPER', jb, a( j, j ), lda, info )
                    if( info/=0 )go to 30
                    if( j+jb<=n ) then
                       ! compute the current block row.
                       call stdlib_zgemm( 'CONJUGATE TRANSPOSE', 'NO TRANSPOSE', jb,n-j-jb+1, j-1,&
                                  -cone, a( 1_ilp, j ), lda,a( 1_ilp, j+jb ), lda, cone, a( j, j+jb ),lda )
                       call stdlib_ztrsm( 'LEFT', 'UPPER', 'CONJUGATE TRANSPOSE','NON-UNIT', jb, &
                                 n-j-jb+1, cone, a( j, j ),lda, a( j, j+jb ), lda )
                    end if
                 end do
              else
                 ! compute the cholesky factorization a = l*l**h.
                 do j = 1, n, nb
                    ! update and factorize the current diagonal block and test
                    ! for non-positive-definiteness.
                    jb = min( nb, n-j+1 )
                    call stdlib_zherk( 'LOWER', 'NO TRANSPOSE', jb, j-1, -one,a( j, 1_ilp ), lda, one,&
                               a( j, j ), lda )
                    call stdlib_zpotrf2( 'LOWER', jb, a( j, j ), lda, info )
                    if( info/=0 )go to 30
                    if( j+jb<=n ) then
                       ! compute the current block column.
                       call stdlib_zgemm( 'NO TRANSPOSE', 'CONJUGATE TRANSPOSE',n-j-jb+1, jb, j-1,&
                                  -cone, a( j+jb, 1_ilp ),lda, a( j, 1_ilp ), lda, cone, a( j+jb, j ),lda )
                       call stdlib_ztrsm( 'RIGHT', 'LOWER', 'CONJUGATE TRANSPOSE','NON-UNIT', n-j-&
                                 jb+1, jb, cone, a( j, j ),lda, a( j+jb, j ), lda )
                    end if
                 end do
              end if
           end if
           go to 40
           30 continue
           info = info + j - 1_ilp
           40 continue
           return
     end subroutine stdlib_zpotrf




     pure recursive module subroutine stdlib_spotrf2( uplo, n, a, lda, info )
     !! SPOTRF2 computes the Cholesky factorization of a real symmetric
     !! positive definite matrix A using the recursive algorithm.
     !! The factorization has the form
     !! A = U**T * U,  if UPLO = 'U', or
     !! A = L  * L**T,  if UPLO = 'L',
     !! where U is an upper triangular matrix and L is lower triangular.
     !! This is the recursive version of the algorithm. It divides
     !! the matrix into four submatrices:
     !! [  A11 | A12  ]  where A11 is n1 by n1 and A22 is n2 by n2
     !! A = [ -----|----- ]  with n1 = n/2
     !! [  A21 | A22  ]       n2 = n-n1
     !! The subroutine calls itself to factor A11. Update and scale A21
     !! or A12, update A22 then call itself to factor A22.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: n1, n2, iinfo
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SPOTRF2', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! n=1 case
           if( n==1_ilp ) then
              ! test for non-positive-definiteness
              if( a( 1_ilp, 1_ilp )<=zero.or.stdlib_sisnan( a( 1_ilp, 1_ilp ) ) ) then
                 info = 1_ilp
                 return
              end if
              ! factor
              a( 1_ilp, 1_ilp ) = sqrt( a( 1_ilp, 1_ilp ) )
           ! use recursive code
           else
              n1 = n/2_ilp
              n2 = n-n1
              ! factor a11
              call stdlib_spotrf2( uplo, n1, a( 1_ilp, 1_ilp ), lda, iinfo )
              if ( iinfo/=0_ilp ) then
                 info = iinfo
                 return
              end if
              ! compute the cholesky factorization a = u**t*u
              if( upper ) then
                 ! update and scale a12
                 call stdlib_strsm( 'L', 'U', 'T', 'N', n1, n2, one,a( 1_ilp, 1_ilp ), lda, a( 1_ilp, n1+1 ), &
                           lda )
                 ! update and factor a22
                 call stdlib_ssyrk( uplo, 'T', n2, n1, -one, a( 1_ilp, n1+1 ), lda,one, a( n1+1, n1+1 &
                           ), lda )
                 call stdlib_spotrf2( uplo, n2, a( n1+1, n1+1 ), lda, iinfo )
                 if ( iinfo/=0_ilp ) then
                    info = iinfo + n1
                    return
                 end if
              ! compute the cholesky factorization a = l*l**t
              else
                 ! update and scale a21
                 call stdlib_strsm( 'R', 'L', 'T', 'N', n2, n1, one,a( 1_ilp, 1_ilp ), lda, a( n1+1, 1_ilp ), &
                           lda )
                 ! update and factor a22
                 call stdlib_ssyrk( uplo, 'N', n2, n1, -one, a( n1+1, 1_ilp ), lda,one, a( n1+1, n1+1 &
                           ), lda )
                 call stdlib_spotrf2( uplo, n2, a( n1+1, n1+1 ), lda, iinfo )
                 if ( iinfo/=0_ilp ) then
                    info = iinfo + n1
                    return
                 end if
              end if
           end if
           return
     end subroutine stdlib_spotrf2

     pure recursive module subroutine stdlib_dpotrf2( uplo, n, a, lda, info )
     !! DPOTRF2 computes the Cholesky factorization of a real symmetric
     !! positive definite matrix A using the recursive algorithm.
     !! The factorization has the form
     !! A = U**T * U,  if UPLO = 'U', or
     !! A = L  * L**T,  if UPLO = 'L',
     !! where U is an upper triangular matrix and L is lower triangular.
     !! This is the recursive version of the algorithm. It divides
     !! the matrix into four submatrices:
     !! [  A11 | A12  ]  where A11 is n1 by n1 and A22 is n2 by n2
     !! A = [ -----|----- ]  with n1 = n/2
     !! [  A21 | A22  ]       n2 = n-n1
     !! The subroutine calls itself to factor A11. Update and scale A21
     !! or A12, update A22 then calls itself to factor A22.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: n1, n2, iinfo
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DPOTRF2', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! n=1 case
           if( n==1_ilp ) then
              ! test for non-positive-definiteness
              if( a( 1_ilp, 1_ilp )<=zero.or.stdlib_disnan( a( 1_ilp, 1_ilp ) ) ) then
                 info = 1_ilp
                 return
              end if
              ! factor
              a( 1_ilp, 1_ilp ) = sqrt( a( 1_ilp, 1_ilp ) )
           ! use recursive code
           else
              n1 = n/2_ilp
              n2 = n-n1
              ! factor a11
              call stdlib_dpotrf2( uplo, n1, a( 1_ilp, 1_ilp ), lda, iinfo )
              if ( iinfo/=0_ilp ) then
                 info = iinfo
                 return
              end if
              ! compute the cholesky factorization a = u**t*u
              if( upper ) then
                 ! update and scale a12
                 call stdlib_dtrsm( 'L', 'U', 'T', 'N', n1, n2, one,a( 1_ilp, 1_ilp ), lda, a( 1_ilp, n1+1 ), &
                           lda )
                 ! update and factor a22
                 call stdlib_dsyrk( uplo, 'T', n2, n1, -one, a( 1_ilp, n1+1 ), lda,one, a( n1+1, n1+1 &
                           ), lda )
                 call stdlib_dpotrf2( uplo, n2, a( n1+1, n1+1 ), lda, iinfo )
                 if ( iinfo/=0_ilp ) then
                    info = iinfo + n1
                    return
                 end if
              ! compute the cholesky factorization a = l*l**t
              else
                 ! update and scale a21
                 call stdlib_dtrsm( 'R', 'L', 'T', 'N', n2, n1, one,a( 1_ilp, 1_ilp ), lda, a( n1+1, 1_ilp ), &
                           lda )
                 ! update and factor a22
                 call stdlib_dsyrk( uplo, 'N', n2, n1, -one, a( n1+1, 1_ilp ), lda,one, a( n1+1, n1+1 &
                           ), lda )
                 call stdlib_dpotrf2( uplo, n2, a( n1+1, n1+1 ), lda, iinfo )
                 if ( iinfo/=0_ilp ) then
                    info = iinfo + n1
                    return
                 end if
              end if
           end if
           return
     end subroutine stdlib_dpotrf2


     pure recursive module subroutine stdlib_cpotrf2( uplo, n, a, lda, info )
     !! CPOTRF2 computes the Cholesky factorization of a Hermitian
     !! positive definite matrix A using the recursive algorithm.
     !! The factorization has the form
     !! A = U**H * U,  if UPLO = 'U', or
     !! A = L  * L**H,  if UPLO = 'L',
     !! where U is an upper triangular matrix and L is lower triangular.
     !! This is the recursive version of the algorithm. It divides
     !! the matrix into four submatrices:
     !! [  A11 | A12  ]  where A11 is n1 by n1 and A22 is n2 by n2
     !! A = [ -----|----- ]  with n1 = n/2
     !! [  A21 | A22  ]       n2 = n-n1
     !! The subroutine calls itself to factor A11. Update and scale A21
     !! or A12, update A22 then calls itself to factor A22.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: n1, n2, iinfo
           real(sp) :: ajj
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CPOTRF2', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! n=1 case
           if( n==1_ilp ) then
              ! test for non-positive-definiteness
              ajj = real( a( 1_ilp, 1_ilp ),KIND=sp)
              if( ajj<=zero.or.stdlib_sisnan( ajj ) ) then
                 info = 1_ilp
                 return
              end if
              ! factor
              a( 1_ilp, 1_ilp ) = sqrt( ajj )
           ! use recursive code
           else
              n1 = n/2_ilp
              n2 = n-n1
              ! factor a11
              call stdlib_cpotrf2( uplo, n1, a( 1_ilp, 1_ilp ), lda, iinfo )
              if ( iinfo/=0_ilp ) then
                 info = iinfo
                 return
              end if
              ! compute the cholesky factorization a = u**h*u
              if( upper ) then
                 ! update and scale a12
                 call stdlib_ctrsm( 'L', 'U', 'C', 'N', n1, n2, cone,a( 1_ilp, 1_ilp ), lda, a( 1_ilp, n1+1 ),&
                            lda )
                 ! update and factor a22
                 call stdlib_cherk( uplo, 'C', n2, n1, -one, a( 1_ilp, n1+1 ), lda,one, a( n1+1, n1+1 &
                           ), lda )
                 call stdlib_cpotrf2( uplo, n2, a( n1+1, n1+1 ), lda, iinfo )
                 if ( iinfo/=0_ilp ) then
                    info = iinfo + n1
                    return
                 end if
              ! compute the cholesky factorization a = l*l**h
              else
                 ! update and scale a21
                 call stdlib_ctrsm( 'R', 'L', 'C', 'N', n2, n1, cone,a( 1_ilp, 1_ilp ), lda, a( n1+1, 1_ilp ),&
                            lda )
                 ! update and factor a22
                 call stdlib_cherk( uplo, 'N', n2, n1, -one, a( n1+1, 1_ilp ), lda,one, a( n1+1, n1+1 &
                           ), lda )
                 call stdlib_cpotrf2( uplo, n2, a( n1+1, n1+1 ), lda, iinfo )
                 if ( iinfo/=0_ilp ) then
                    info = iinfo + n1
                    return
                 end if
              end if
           end if
           return
     end subroutine stdlib_cpotrf2

     pure recursive module subroutine stdlib_zpotrf2( uplo, n, a, lda, info )
     !! ZPOTRF2 computes the Cholesky factorization of a Hermitian
     !! positive definite matrix A using the recursive algorithm.
     !! The factorization has the form
     !! A = U**H * U,  if UPLO = 'U', or
     !! A = L  * L**H,  if UPLO = 'L',
     !! where U is an upper triangular matrix and L is lower triangular.
     !! This is the recursive version of the algorithm. It divides
     !! the matrix into four submatrices:
     !! [  A11 | A12  ]  where A11 is n1 by n1 and A22 is n2 by n2
     !! A = [ -----|----- ]  with n1 = n/2
     !! [  A21 | A22  ]       n2 = n-n1
     !! The subroutine calls itself to factor A11. Update and scale A21
     !! or A12, update A22 then call itself to factor A22.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: n1, n2, iinfo
           real(dp) :: ajj
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZPOTRF2', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! n=1 case
           if( n==1_ilp ) then
              ! test for non-positive-definiteness
              ajj = real( a( 1_ilp, 1_ilp ),KIND=dp)
              if( ajj<=zero.or.stdlib_disnan( ajj ) ) then
                 info = 1_ilp
                 return
              end if
              ! factor
              a( 1_ilp, 1_ilp ) = sqrt( ajj )
           ! use recursive code
           else
              n1 = n/2_ilp
              n2 = n-n1
              ! factor a11
              call stdlib_zpotrf2( uplo, n1, a( 1_ilp, 1_ilp ), lda, iinfo )
              if ( iinfo/=0_ilp ) then
                 info = iinfo
                 return
              end if
              ! compute the cholesky factorization a = u**h*u
              if( upper ) then
                 ! update and scale a12
                 call stdlib_ztrsm( 'L', 'U', 'C', 'N', n1, n2, cone,a( 1_ilp, 1_ilp ), lda, a( 1_ilp, n1+1 ),&
                            lda )
                 ! update and factor a22
                 call stdlib_zherk( uplo, 'C', n2, n1, -one, a( 1_ilp, n1+1 ), lda,one, a( n1+1, n1+1 &
                           ), lda )
                 call stdlib_zpotrf2( uplo, n2, a( n1+1, n1+1 ), lda, iinfo )
                 if ( iinfo/=0_ilp ) then
                    info = iinfo + n1
                    return
                 end if
              ! compute the cholesky factorization a = l*l**h
              else
                 ! update and scale a21
                 call stdlib_ztrsm( 'R', 'L', 'C', 'N', n2, n1, cone,a( 1_ilp, 1_ilp ), lda, a( n1+1, 1_ilp ),&
                            lda )
                 ! update and factor a22
                 call stdlib_zherk( uplo, 'N', n2, n1, -one, a( n1+1, 1_ilp ), lda,one, a( n1+1, n1+1 &
                           ), lda )
                 call stdlib_zpotrf2( uplo, n2, a( n1+1, n1+1 ), lda, iinfo )
                 if ( iinfo/=0_ilp ) then
                    info = iinfo + n1
                    return
                 end if
              end if
           end if
           return
     end subroutine stdlib_zpotrf2




     pure module subroutine stdlib_spotf2( uplo, n, a, lda, info )
     !! SPOTF2 computes the Cholesky factorization of a real symmetric
     !! positive definite matrix A.
     !! The factorization has the form
     !! A = U**T * U ,  if UPLO = 'U', or
     !! A = L  * L**T,  if UPLO = 'L',
     !! where U is an upper triangular matrix and L is lower triangular.
     !! This is the unblocked version of the algorithm, calling Level 2 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j
           real(sp) :: ajj
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SPOTF2', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( upper ) then
              ! compute the cholesky factorization a = u**t *u.
              do j = 1, n
                 ! compute u(j,j) and test for non-positive-definiteness.
                 ajj = a( j, j ) - stdlib_sdot( j-1, a( 1_ilp, j ), 1_ilp, a( 1_ilp, j ), 1_ilp )
                 if( ajj<=zero.or.stdlib_sisnan( ajj ) ) then
                    a( j, j ) = ajj
                    go to 30
                 end if
                 ajj = sqrt( ajj )
                 a( j, j ) = ajj
                 ! compute elements j+1:n of row j.
                 if( j<n ) then
                    call stdlib_sgemv( 'TRANSPOSE', j-1, n-j, -one, a( 1_ilp, j+1 ),lda, a( 1_ilp, j ), 1_ilp,&
                               one, a( j, j+1 ), lda )
                    call stdlib_sscal( n-j, one / ajj, a( j, j+1 ), lda )
                 end if
              end do
           else
              ! compute the cholesky factorization a = l*l**t.
              do j = 1, n
                 ! compute l(j,j) and test for non-positive-definiteness.
                 ajj = a( j, j ) - stdlib_sdot( j-1, a( j, 1_ilp ), lda, a( j, 1_ilp ),lda )
                 if( ajj<=zero.or.stdlib_sisnan( ajj ) ) then
                    a( j, j ) = ajj
                    go to 30
                 end if
                 ajj = sqrt( ajj )
                 a( j, j ) = ajj
                 ! compute elements j+1:n of column j.
                 if( j<n ) then
                    call stdlib_sgemv( 'NO TRANSPOSE', n-j, j-1, -one, a( j+1, 1_ilp ),lda, a( j, 1_ilp ),&
                               lda, one, a( j+1, j ), 1_ilp )
                    call stdlib_sscal( n-j, one / ajj, a( j+1, j ), 1_ilp )
                 end if
              end do
           end if
           go to 40
           30 continue
           info = j
           40 continue
           return
     end subroutine stdlib_spotf2

     pure module subroutine stdlib_dpotf2( uplo, n, a, lda, info )
     !! DPOTF2 computes the Cholesky factorization of a real symmetric
     !! positive definite matrix A.
     !! The factorization has the form
     !! A = U**T * U ,  if UPLO = 'U', or
     !! A = L  * L**T,  if UPLO = 'L',
     !! where U is an upper triangular matrix and L is lower triangular.
     !! This is the unblocked version of the algorithm, calling Level 2 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j
           real(dp) :: ajj
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DPOTF2', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( upper ) then
              ! compute the cholesky factorization a = u**t *u.
              do j = 1, n
                 ! compute u(j,j) and test for non-positive-definiteness.
                 ajj = a( j, j ) - stdlib_ddot( j-1, a( 1_ilp, j ), 1_ilp, a( 1_ilp, j ), 1_ilp )
                 if( ajj<=zero.or.stdlib_disnan( ajj ) ) then
                    a( j, j ) = ajj
                    go to 30
                 end if
                 ajj = sqrt( ajj )
                 a( j, j ) = ajj
                 ! compute elements j+1:n of row j.
                 if( j<n ) then
                    call stdlib_dgemv( 'TRANSPOSE', j-1, n-j, -one, a( 1_ilp, j+1 ),lda, a( 1_ilp, j ), 1_ilp,&
                               one, a( j, j+1 ), lda )
                    call stdlib_dscal( n-j, one / ajj, a( j, j+1 ), lda )
                 end if
              end do
           else
              ! compute the cholesky factorization a = l*l**t.
              do j = 1, n
                 ! compute l(j,j) and test for non-positive-definiteness.
                 ajj = a( j, j ) - stdlib_ddot( j-1, a( j, 1_ilp ), lda, a( j, 1_ilp ),lda )
                 if( ajj<=zero.or.stdlib_disnan( ajj ) ) then
                    a( j, j ) = ajj
                    go to 30
                 end if
                 ajj = sqrt( ajj )
                 a( j, j ) = ajj
                 ! compute elements j+1:n of column j.
                 if( j<n ) then
                    call stdlib_dgemv( 'NO TRANSPOSE', n-j, j-1, -one, a( j+1, 1_ilp ),lda, a( j, 1_ilp ),&
                               lda, one, a( j+1, j ), 1_ilp )
                    call stdlib_dscal( n-j, one / ajj, a( j+1, j ), 1_ilp )
                 end if
              end do
           end if
           go to 40
           30 continue
           info = j
           40 continue
           return
     end subroutine stdlib_dpotf2


     pure module subroutine stdlib_cpotf2( uplo, n, a, lda, info )
     !! CPOTF2 computes the Cholesky factorization of a complex Hermitian
     !! positive definite matrix A.
     !! The factorization has the form
     !! A = U**H * U ,  if UPLO = 'U', or
     !! A = L  * L**H,  if UPLO = 'L',
     !! where U is an upper triangular matrix and L is lower triangular.
     !! This is the unblocked version of the algorithm, calling Level 2 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j
           real(sp) :: ajj
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CPOTF2', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( upper ) then
              ! compute the cholesky factorization a = u**h *u.
              do j = 1, n
                 ! compute u(j,j) and test for non-positive-definiteness.
                 ajj = real( real( a( j, j ),KIND=sp) - stdlib_cdotc( j-1, a( 1_ilp, j ), 1_ilp,a( 1_ilp, j ),&
                            1_ilp ),KIND=sp)
                 if( ajj<=zero.or.stdlib_sisnan( ajj ) ) then
                    a( j, j ) = ajj
                    go to 30
                 end if
                 ajj = sqrt( ajj )
                 a( j, j ) = ajj
                 ! compute elements j+1:n of row j.
                 if( j<n ) then
                    call stdlib_clacgv( j-1, a( 1_ilp, j ), 1_ilp )
                    call stdlib_cgemv( 'TRANSPOSE', j-1, n-j, -cone, a( 1_ilp, j+1 ),lda, a( 1_ilp, j ), &
                              1_ilp, cone, a( j, j+1 ), lda )
                    call stdlib_clacgv( j-1, a( 1_ilp, j ), 1_ilp )
                    call stdlib_csscal( n-j, one / ajj, a( j, j+1 ), lda )
                 end if
              end do
           else
              ! compute the cholesky factorization a = l*l**h.
              do j = 1, n
                 ! compute l(j,j) and test for non-positive-definiteness.
                 ajj = real( real( a( j, j ),KIND=sp) - stdlib_cdotc( j-1, a( j, 1_ilp ), lda,a( j, 1_ilp &
                           ), lda ),KIND=sp)
                 if( ajj<=zero.or.stdlib_sisnan( ajj ) ) then
                    a( j, j ) = ajj
                    go to 30
                 end if
                 ajj = sqrt( ajj )
                 a( j, j ) = ajj
                 ! compute elements j+1:n of column j.
                 if( j<n ) then
                    call stdlib_clacgv( j-1, a( j, 1_ilp ), lda )
                    call stdlib_cgemv( 'NO TRANSPOSE', n-j, j-1, -cone, a( j+1, 1_ilp ),lda, a( j, 1_ilp )&
                              , lda, cone, a( j+1, j ), 1_ilp )
                    call stdlib_clacgv( j-1, a( j, 1_ilp ), lda )
                    call stdlib_csscal( n-j, one / ajj, a( j+1, j ), 1_ilp )
                 end if
              end do
           end if
           go to 40
           30 continue
           info = j
           40 continue
           return
     end subroutine stdlib_cpotf2

     pure module subroutine stdlib_zpotf2( uplo, n, a, lda, info )
     !! ZPOTF2 computes the Cholesky factorization of a complex Hermitian
     !! positive definite matrix A.
     !! The factorization has the form
     !! A = U**H * U ,  if UPLO = 'U', or
     !! A = L  * L**H,  if UPLO = 'L',
     !! where U is an upper triangular matrix and L is lower triangular.
     !! This is the unblocked version of the algorithm, calling Level 2 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j
           real(dp) :: ajj
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZPOTF2', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( upper ) then
              ! compute the cholesky factorization a = u**h *u.
              do j = 1, n
                 ! compute u(j,j) and test for non-positive-definiteness.
                 ajj = real( a( j, j ),KIND=dp) - real( stdlib_zdotc( j-1, a( 1_ilp, j ), 1_ilp,a( 1_ilp, j ),&
                            1_ilp ),KIND=dp)
                 if( ajj<=zero.or.stdlib_disnan( ajj ) ) then
                    a( j, j ) = ajj
                    go to 30
                 end if
                 ajj = sqrt( ajj )
                 a( j, j ) = ajj
                 ! compute elements j+1:n of row j.
                 if( j<n ) then
                    call stdlib_zlacgv( j-1, a( 1_ilp, j ), 1_ilp )
                    call stdlib_zgemv( 'TRANSPOSE', j-1, n-j, -cone, a( 1_ilp, j+1 ),lda, a( 1_ilp, j ), &
                              1_ilp, cone, a( j, j+1 ), lda )
                    call stdlib_zlacgv( j-1, a( 1_ilp, j ), 1_ilp )
                    call stdlib_zdscal( n-j, one / ajj, a( j, j+1 ), lda )
                 end if
              end do
           else
              ! compute the cholesky factorization a = l*l**h.
              do j = 1, n
                 ! compute l(j,j) and test for non-positive-definiteness.
                 ajj = real( a( j, j ),KIND=dp) - real( stdlib_zdotc( j-1, a( j, 1_ilp ), lda,a( j, 1_ilp &
                           ), lda ),KIND=dp)
                 if( ajj<=zero.or.stdlib_disnan( ajj ) ) then
                    a( j, j ) = ajj
                    go to 30
                 end if
                 ajj = sqrt( ajj )
                 a( j, j ) = ajj
                 ! compute elements j+1:n of column j.
                 if( j<n ) then
                    call stdlib_zlacgv( j-1, a( j, 1_ilp ), lda )
                    call stdlib_zgemv( 'NO TRANSPOSE', n-j, j-1, -cone, a( j+1, 1_ilp ),lda, a( j, 1_ilp )&
                              , lda, cone, a( j+1, j ), 1_ilp )
                    call stdlib_zlacgv( j-1, a( j, 1_ilp ), lda )
                    call stdlib_zdscal( n-j, one / ajj, a( j+1, j ), 1_ilp )
                 end if
              end do
           end if
           go to 40
           30 continue
           info = j
           40 continue
           return
     end subroutine stdlib_zpotf2




     pure module subroutine stdlib_spstrf( uplo, n, a, lda, piv, rank, tol, work, info )
     !! SPSTRF computes the Cholesky factorization with complete
     !! pivoting of a real symmetric positive semidefinite matrix A.
     !! The factorization has the form
     !! P**T * A * P = U**T * U ,  if UPLO = 'U',
     !! P**T * A * P = L  * L**T,  if UPLO = 'L',
     !! where U is an upper triangular matrix and L is lower triangular, and
     !! P is stored as vector PIV.
     !! This algorithm does not attempt to check that A is positive
     !! semidefinite. This version of the algorithm calls level 3 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(sp), intent(in) :: tol
           integer(ilp), intent(out) :: info, rank
           integer(ilp), intent(in) :: lda, n
           character, intent(in) :: uplo
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: work(2_ilp*n)
           integer(ilp), intent(out) :: piv(n)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: ajj, sstop, stemp
           integer(ilp) :: i, itemp, j, jb, k, nb, pvt
           logical(lk) :: upper
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SPSTRF', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! get block size
           nb = stdlib_ilaenv( 1_ilp, 'SPOTRF', uplo, n, -1_ilp, -1_ilp, -1_ilp )
           if( nb<=1_ilp .or. nb>=n ) then
              ! use unblocked code
              call stdlib_spstf2( uplo, n, a( 1_ilp, 1_ilp ), lda, piv, rank, tol, work,info )
              go to 200
           else
           ! initialize piv
              do i = 1, n
                 piv( i ) = i
              end do
           ! compute stopping value
              pvt = 1_ilp
              ajj = a( pvt, pvt )
              do i = 2, n
                 if( a( i, i )>ajj ) then
                    pvt = i
                    ajj = a( pvt, pvt )
                 end if
              end do
              if( ajj<=zero.or.stdlib_sisnan( ajj ) ) then
                 rank = 0_ilp
                 info = 1_ilp
                 go to 200
              end if
           ! compute stopping value if not supplied
              if( tol<zero ) then
                 sstop = n * stdlib_slamch( 'EPSILON' ) * ajj
              else
                 sstop = tol
              end if
              if( upper ) then
                 ! compute the cholesky factorization p**t * a * p = u**t * u
                 loop_140: do k = 1, n, nb
                    ! account for last block not being nb wide
                    jb = min( nb, n-k+1 )
                    ! set relevant part of first half of work to zero,
                    ! holds dot products
                    do i = k, n
                       work( i ) = 0_ilp
                    end do
                    loop_130: do j = k, k + jb - 1
                    ! find pivot, test for exit, else swap rows and columns
                    ! update dot products, compute possible pivots which are
                    ! stored in the second half of work
                       do i = j, n
                          if( j>k ) then
                             work( i ) = work( i ) + a( j-1, i )**2_ilp
                          end if
                          work( n+i ) = a( i, i ) - work( i )
                       end do
                       if( j>1_ilp ) then
                          itemp = maxloc( work( (n+j):(2_ilp*n) ), 1_ilp )
                          pvt = itemp + j - 1_ilp
                          ajj = work( n+pvt )
                          if( ajj<=sstop.or.stdlib_sisnan( ajj ) ) then
                             a( j, j ) = ajj
                             go to 190
                          end if
                       end if
                       if( j/=pvt ) then
                          ! pivot ok, so can now swap pivot rows and columns
                          a( pvt, pvt ) = a( j, j )
                          call stdlib_sswap( j-1, a( 1_ilp, j ), 1_ilp, a( 1_ilp, pvt ), 1_ilp )
                          if( pvt<n )call stdlib_sswap( n-pvt, a( j, pvt+1 ), lda,a( pvt, pvt+1 ),&
                                     lda )
                          call stdlib_sswap( pvt-j-1, a( j, j+1 ), lda,a( j+1, pvt ), 1_ilp )
                          ! swap dot products and piv
                          stemp = work( j )
                          work( j ) = work( pvt )
                          work( pvt ) = stemp
                          itemp = piv( pvt )
                          piv( pvt ) = piv( j )
                          piv( j ) = itemp
                       end if
                       ajj = sqrt( ajj )
                       a( j, j ) = ajj
                       ! compute elements j+1:n of row j.
                       if( j<n ) then
                          call stdlib_sgemv( 'TRANS', j-k, n-j, -one, a( k, j+1 ),lda, a( k, j ), &
                                    1_ilp, one, a( j, j+1 ),lda )
                          call stdlib_sscal( n-j, one / ajj, a( j, j+1 ), lda )
                       end if
                    end do loop_130
                    ! update trailing matrix, j already incremented
                    if( k+jb<=n ) then
                       call stdlib_ssyrk( 'UPPER', 'TRANS', n-j+1, jb, -one,a( k, j ), lda, one, &
                                 a( j, j ), lda )
                    end if
                 end do loop_140
              else
              ! compute the cholesky factorization p**t * a * p = l * l**t
                 loop_180: do k = 1, n, nb
                    ! account for last block not being nb wide
                    jb = min( nb, n-k+1 )
                    ! set relevant part of first half of work to zero,
                    ! holds dot products
                    do i = k, n
                       work( i ) = 0_ilp
                    end do
                    loop_170: do j = k, k + jb - 1
                    ! find pivot, test for exit, else swap rows and columns
                    ! update dot products, compute possible pivots which are
                    ! stored in the second half of work
                       do i = j, n
                          if( j>k ) then
                             work( i ) = work( i ) + a( i, j-1 )**2_ilp
                          end if
                          work( n+i ) = a( i, i ) - work( i )
                       end do
                       if( j>1_ilp ) then
                          itemp = maxloc( work( (n+j):(2_ilp*n) ), 1_ilp )
                          pvt = itemp + j - 1_ilp
                          ajj = work( n+pvt )
                          if( ajj<=sstop.or.stdlib_sisnan( ajj ) ) then
                             a( j, j ) = ajj
                             go to 190
                          end if
                       end if
                       if( j/=pvt ) then
                          ! pivot ok, so can now swap pivot rows and columns
                          a( pvt, pvt ) = a( j, j )
                          call stdlib_sswap( j-1, a( j, 1_ilp ), lda, a( pvt, 1_ilp ), lda )
                          if( pvt<n )call stdlib_sswap( n-pvt, a( pvt+1, j ), 1_ilp,a( pvt+1, pvt ), &
                                    1_ilp )
                          call stdlib_sswap( pvt-j-1, a( j+1, j ), 1_ilp, a( pvt, j+1 ),lda )
                          ! swap dot products and piv
                          stemp = work( j )
                          work( j ) = work( pvt )
                          work( pvt ) = stemp
                          itemp = piv( pvt )
                          piv( pvt ) = piv( j )
                          piv( j ) = itemp
                       end if
                       ajj = sqrt( ajj )
                       a( j, j ) = ajj
                       ! compute elements j+1:n of column j.
                       if( j<n ) then
                          call stdlib_sgemv( 'NO TRANS', n-j, j-k, -one,a( j+1, k ), lda, a( j, k &
                                    ), lda, one,a( j+1, j ), 1_ilp )
                          call stdlib_sscal( n-j, one / ajj, a( j+1, j ), 1_ilp )
                       end if
                    end do loop_170
                    ! update trailing matrix, j already incremented
                    if( k+jb<=n ) then
                       call stdlib_ssyrk( 'LOWER', 'NO TRANS', n-j+1, jb, -one,a( j, k ), lda, &
                                 one, a( j, j ), lda )
                    end if
                 end do loop_180
              end if
           end if
           ! ran to completion, a has full rank
           rank = n
           go to 200
           190 continue
           ! rank is the number of steps completed.  set info = 1 to signal
           ! that the factorization cannot be used to solve a system.
           rank = j - 1_ilp
           info = 1_ilp
           200 continue
           return
     end subroutine stdlib_spstrf

     pure module subroutine stdlib_dpstrf( uplo, n, a, lda, piv, rank, tol, work, info )
     !! DPSTRF computes the Cholesky factorization with complete
     !! pivoting of a real symmetric positive semidefinite matrix A.
     !! The factorization has the form
     !! P**T * A * P = U**T * U ,  if UPLO = 'U',
     !! P**T * A * P = L  * L**T,  if UPLO = 'L',
     !! where U is an upper triangular matrix and L is lower triangular, and
     !! P is stored as vector PIV.
     !! This algorithm does not attempt to check that A is positive
     !! semidefinite. This version of the algorithm calls level 3 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(dp), intent(in) :: tol
           integer(ilp), intent(out) :: info, rank
           integer(ilp), intent(in) :: lda, n
           character, intent(in) :: uplo
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: work(2_ilp*n)
           integer(ilp), intent(out) :: piv(n)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: ajj, dstop, dtemp
           integer(ilp) :: i, itemp, j, jb, k, nb, pvt
           logical(lk) :: upper
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DPSTRF', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! get block size
           nb = stdlib_ilaenv( 1_ilp, 'DPOTRF', uplo, n, -1_ilp, -1_ilp, -1_ilp )
           if( nb<=1_ilp .or. nb>=n ) then
              ! use unblocked code
              call stdlib_dpstf2( uplo, n, a( 1_ilp, 1_ilp ), lda, piv, rank, tol, work,info )
              go to 200
           else
           ! initialize piv
              do i = 1, n
                 piv( i ) = i
              end do
           ! compute stopping value
              pvt = 1_ilp
              ajj = a( pvt, pvt )
              do i = 2, n
                 if( a( i, i )>ajj ) then
                    pvt = i
                    ajj = a( pvt, pvt )
                 end if
              end do
              if( ajj<=zero.or.stdlib_disnan( ajj ) ) then
                 rank = 0_ilp
                 info = 1_ilp
                 go to 200
              end if
           ! compute stopping value if not supplied
              if( tol<zero ) then
                 dstop = n * stdlib_dlamch( 'EPSILON' ) * ajj
              else
                 dstop = tol
              end if
              if( upper ) then
                 ! compute the cholesky factorization p**t * a * p = u**t * u
                 loop_140: do k = 1, n, nb
                    ! account for last block not being nb wide
                    jb = min( nb, n-k+1 )
                    ! set relevant part of first half of work to zero,
                    ! holds dot products
                    do i = k, n
                       work( i ) = 0_ilp
                    end do
                    loop_130: do j = k, k + jb - 1
                    ! find pivot, test for exit, else swap rows and columns
                    ! update dot products, compute possible pivots which are
                    ! stored in the second half of work
                       do i = j, n
                          if( j>k ) then
                             work( i ) = work( i ) + a( j-1, i )**2_ilp
                          end if
                          work( n+i ) = a( i, i ) - work( i )
                       end do
                       if( j>1_ilp ) then
                          itemp = maxloc( work( (n+j):(2_ilp*n) ), 1_ilp )
                          pvt = itemp + j - 1_ilp
                          ajj = work( n+pvt )
                          if( ajj<=dstop.or.stdlib_disnan( ajj ) ) then
                             a( j, j ) = ajj
                             go to 190
                          end if
                       end if
                       if( j/=pvt ) then
                          ! pivot ok, so can now swap pivot rows and columns
                          a( pvt, pvt ) = a( j, j )
                          call stdlib_dswap( j-1, a( 1_ilp, j ), 1_ilp, a( 1_ilp, pvt ), 1_ilp )
                          if( pvt<n )call stdlib_dswap( n-pvt, a( j, pvt+1 ), lda,a( pvt, pvt+1 ),&
                                     lda )
                          call stdlib_dswap( pvt-j-1, a( j, j+1 ), lda,a( j+1, pvt ), 1_ilp )
                          ! swap dot products and piv
                          dtemp = work( j )
                          work( j ) = work( pvt )
                          work( pvt ) = dtemp
                          itemp = piv( pvt )
                          piv( pvt ) = piv( j )
                          piv( j ) = itemp
                       end if
                       ajj = sqrt( ajj )
                       a( j, j ) = ajj
                       ! compute elements j+1:n of row j.
                       if( j<n ) then
                          call stdlib_dgemv( 'TRANS', j-k, n-j, -one, a( k, j+1 ),lda, a( k, j ), &
                                    1_ilp, one, a( j, j+1 ),lda )
                          call stdlib_dscal( n-j, one / ajj, a( j, j+1 ), lda )
                       end if
                    end do loop_130
                    ! update trailing matrix, j already incremented
                    if( k+jb<=n ) then
                       call stdlib_dsyrk( 'UPPER', 'TRANS', n-j+1, jb, -one,a( k, j ), lda, one, &
                                 a( j, j ), lda )
                    end if
                 end do loop_140
              else
              ! compute the cholesky factorization p**t * a * p = l * l**t
                 loop_180: do k = 1, n, nb
                    ! account for last block not being nb wide
                    jb = min( nb, n-k+1 )
                    ! set relevant part of first half of work to zero,
                    ! holds dot products
                    do i = k, n
                       work( i ) = 0_ilp
                    end do
                    loop_170: do j = k, k + jb - 1
                    ! find pivot, test for exit, else swap rows and columns
                    ! update dot products, compute possible pivots which are
                    ! stored in the second half of work
                       do i = j, n
                          if( j>k ) then
                             work( i ) = work( i ) + a( i, j-1 )**2_ilp
                          end if
                          work( n+i ) = a( i, i ) - work( i )
                       end do
                       if( j>1_ilp ) then
                          itemp = maxloc( work( (n+j):(2_ilp*n) ), 1_ilp )
                          pvt = itemp + j - 1_ilp
                          ajj = work( n+pvt )
                          if( ajj<=dstop.or.stdlib_disnan( ajj ) ) then
                             a( j, j ) = ajj
                             go to 190
                          end if
                       end if
                       if( j/=pvt ) then
                          ! pivot ok, so can now swap pivot rows and columns
                          a( pvt, pvt ) = a( j, j )
                          call stdlib_dswap( j-1, a( j, 1_ilp ), lda, a( pvt, 1_ilp ), lda )
                          if( pvt<n )call stdlib_dswap( n-pvt, a( pvt+1, j ), 1_ilp,a( pvt+1, pvt ), &
                                    1_ilp )
                          call stdlib_dswap( pvt-j-1, a( j+1, j ), 1_ilp, a( pvt, j+1 ),lda )
                          ! swap dot products and piv
                          dtemp = work( j )
                          work( j ) = work( pvt )
                          work( pvt ) = dtemp
                          itemp = piv( pvt )
                          piv( pvt ) = piv( j )
                          piv( j ) = itemp
                       end if
                       ajj = sqrt( ajj )
                       a( j, j ) = ajj
                       ! compute elements j+1:n of column j.
                       if( j<n ) then
                          call stdlib_dgemv( 'NO TRANS', n-j, j-k, -one,a( j+1, k ), lda, a( j, k &
                                    ), lda, one,a( j+1, j ), 1_ilp )
                          call stdlib_dscal( n-j, one / ajj, a( j+1, j ), 1_ilp )
                       end if
                    end do loop_170
                    ! update trailing matrix, j already incremented
                    if( k+jb<=n ) then
                       call stdlib_dsyrk( 'LOWER', 'NO TRANS', n-j+1, jb, -one,a( j, k ), lda, &
                                 one, a( j, j ), lda )
                    end if
                 end do loop_180
              end if
           end if
           ! ran to completion, a has full rank
           rank = n
           go to 200
           190 continue
           ! rank is the number of steps completed.  set info = 1 to signal
           ! that the factorization cannot be used to solve a system.
           rank = j - 1_ilp
           info = 1_ilp
           200 continue
           return
     end subroutine stdlib_dpstrf


     pure module subroutine stdlib_cpstrf( uplo, n, a, lda, piv, rank, tol, work, info )
     !! CPSTRF computes the Cholesky factorization with complete
     !! pivoting of a complex Hermitian positive semidefinite matrix A.
     !! The factorization has the form
     !! P**T * A * P = U**H * U ,  if UPLO = 'U',
     !! P**T * A * P = L  * L**H,  if UPLO = 'L',
     !! where U is an upper triangular matrix and L is lower triangular, and
     !! P is stored as vector PIV.
     !! This algorithm does not attempt to check that A is positive
     !! semidefinite. This version of the algorithm calls level 3 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(sp), intent(in) :: tol
           integer(ilp), intent(out) :: info, rank
           integer(ilp), intent(in) :: lda, n
           character, intent(in) :: uplo
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: work(2_ilp*n)
           integer(ilp), intent(out) :: piv(n)
        ! =====================================================================
           
           
           ! Local Scalars 
           complex(sp) :: ctemp
           real(sp) :: ajj, sstop, stemp
           integer(ilp) :: i, itemp, j, jb, k, nb, pvt
           logical(lk) :: upper
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CPSTRF', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! get block size
           nb = stdlib_ilaenv( 1_ilp, 'CPOTRF', uplo, n, -1_ilp, -1_ilp, -1_ilp )
           if( nb<=1_ilp .or. nb>=n ) then
              ! use unblocked code
              call stdlib_cpstf2( uplo, n, a( 1_ilp, 1_ilp ), lda, piv, rank, tol, work,info )
              go to 230
           else
           ! initialize piv
              do i = 1, n
                 piv( i ) = i
              end do
           ! compute stopping value
              do i = 1, n
                 work( i ) = real( a( i, i ),KIND=sp)
              end do
              pvt = maxloc( work( 1_ilp:n ), 1_ilp )
              ajj = real( a( pvt, pvt ),KIND=sp)
              if( ajj<=zero.or.stdlib_sisnan( ajj ) ) then
                 rank = 0_ilp
                 info = 1_ilp
                 go to 230
              end if
           ! compute stopping value if not supplied
              if( tol<zero ) then
                 sstop = n * stdlib_slamch( 'EPSILON' ) * ajj
              else
                 sstop = tol
              end if
              if( upper ) then
                 ! compute the cholesky factorization p**t * a * p = u**h * u
                 loop_160: do k = 1, n, nb
                    ! account for last block not being nb wide
                    jb = min( nb, n-k+1 )
                    ! set relevant part of first chalf of work to zero,
                    ! holds dot products
                    do i = k, n
                       work( i ) = 0_ilp
                    end do
                    loop_150: do j = k, k + jb - 1
                    ! find pivot, test for exit, else swap rows and columns
                    ! update dot products, compute possible pivots which are
                    ! stored in the second chalf of work
                       do i = j, n
                          if( j>k ) then
                             work( i ) = work( i ) +real( conjg( a( j-1, i ) )*a( j-1, i ),&
                                       KIND=sp)
                          end if
                          work( n+i ) = real( a( i, i ),KIND=sp) - work( i )
                       end do
                       if( j>1_ilp ) then
                          itemp = maxloc( work( (n+j):(2_ilp*n) ), 1_ilp )
                          pvt = itemp + j - 1_ilp
                          ajj = work( n+pvt )
                          if( ajj<=sstop.or.stdlib_sisnan( ajj ) ) then
                             a( j, j ) = ajj
                             go to 220
                          end if
                       end if
                       if( j/=pvt ) then
                          ! pivot ok, so can now swap pivot rows and columns
                          a( pvt, pvt ) = a( j, j )
                          call stdlib_cswap( j-1, a( 1_ilp, j ), 1_ilp, a( 1_ilp, pvt ), 1_ilp )
                          if( pvt<n )call stdlib_cswap( n-pvt, a( j, pvt+1 ), lda,a( pvt, pvt+1 ),&
                                     lda )
                          do i = j + 1, pvt - 1
                             ctemp = conjg( a( j, i ) )
                             a( j, i ) = conjg( a( i, pvt ) )
                             a( i, pvt ) = ctemp
                          end do
                          a( j, pvt ) = conjg( a( j, pvt ) )
                          ! swap dot products and piv
                          stemp = work( j )
                          work( j ) = work( pvt )
                          work( pvt ) = stemp
                          itemp = piv( pvt )
                          piv( pvt ) = piv( j )
                          piv( j ) = itemp
                       end if
                       ajj = sqrt( ajj )
                       a( j, j ) = ajj
                       ! compute elements j+1:n of row j.
                       if( j<n ) then
                          call stdlib_clacgv( j-1, a( 1_ilp, j ), 1_ilp )
                          call stdlib_cgemv( 'TRANS', j-k, n-j, -cone, a( k, j+1 ),lda, a( k, j ),&
                                     1_ilp, cone, a( j, j+1 ),lda )
                          call stdlib_clacgv( j-1, a( 1_ilp, j ), 1_ilp )
                          call stdlib_csscal( n-j, one / ajj, a( j, j+1 ), lda )
                       end if
                    end do loop_150
                    ! update trailing matrix, j already incremented
                    if( k+jb<=n ) then
                       call stdlib_cherk( 'UPPER', 'CONJ TRANS', n-j+1, jb, -one,a( k, j ), lda, &
                                 one, a( j, j ), lda )
                    end if
                 end do loop_160
              else
              ! compute the cholesky factorization p**t * a * p = l * l**h
                 loop_210: do k = 1, n, nb
                    ! account for last block not being nb wide
                    jb = min( nb, n-k+1 )
                    ! set relevant part of first chalf of work to zero,
                    ! holds dot products
                    do i = k, n
                       work( i ) = 0_ilp
                    end do
                    loop_200: do j = k, k + jb - 1
                    ! find pivot, test for exit, else swap rows and columns
                    ! update dot products, compute possible pivots which are
                    ! stored in the second chalf of work
                       do i = j, n
                          if( j>k ) then
                             work( i ) = work( i ) +real( conjg( a( i, j-1 ) )*a( i, j-1 ),&
                                       KIND=sp)
                          end if
                          work( n+i ) = real( a( i, i ),KIND=sp) - work( i )
                       end do
                       if( j>1_ilp ) then
                          itemp = maxloc( work( (n+j):(2_ilp*n) ), 1_ilp )
                          pvt = itemp + j - 1_ilp
                          ajj = work( n+pvt )
                          if( ajj<=sstop.or.stdlib_sisnan( ajj ) ) then
                             a( j, j ) = ajj
                             go to 220
                          end if
                       end if
                       if( j/=pvt ) then
                          ! pivot ok, so can now swap pivot rows and columns
                          a( pvt, pvt ) = a( j, j )
                          call stdlib_cswap( j-1, a( j, 1_ilp ), lda, a( pvt, 1_ilp ), lda )
                          if( pvt<n )call stdlib_cswap( n-pvt, a( pvt+1, j ), 1_ilp,a( pvt+1, pvt ), &
                                    1_ilp )
                          do i = j + 1, pvt - 1
                             ctemp = conjg( a( i, j ) )
                             a( i, j ) = conjg( a( pvt, i ) )
                             a( pvt, i ) = ctemp
                          end do
                          a( pvt, j ) = conjg( a( pvt, j ) )
                          ! swap dot products and piv
                          stemp = work( j )
                          work( j ) = work( pvt )
                          work( pvt ) = stemp
                          itemp = piv( pvt )
                          piv( pvt ) = piv( j )
                          piv( j ) = itemp
                       end if
                       ajj = sqrt( ajj )
                       a( j, j ) = ajj
                       ! compute elements j+1:n of column j.
                       if( j<n ) then
                          call stdlib_clacgv( j-1, a( j, 1_ilp ), lda )
                          call stdlib_cgemv( 'NO TRANS', n-j, j-k, -cone,a( j+1, k ), lda, a( j, &
                                    k ), lda, cone,a( j+1, j ), 1_ilp )
                          call stdlib_clacgv( j-1, a( j, 1_ilp ), lda )
                          call stdlib_csscal( n-j, one / ajj, a( j+1, j ), 1_ilp )
                       end if
                    end do loop_200
                    ! update trailing matrix, j already incremented
                    if( k+jb<=n ) then
                       call stdlib_cherk( 'LOWER', 'NO TRANS', n-j+1, jb, -one,a( j, k ), lda, &
                                 one, a( j, j ), lda )
                    end if
                 end do loop_210
              end if
           end if
           ! ran to completion, a has full rank
           rank = n
           go to 230
           220 continue
           ! rank is the number of steps completed.  set info = 1 to signal
           ! that the factorization cannot be used to solve a system.
           rank = j - 1_ilp
           info = 1_ilp
           230 continue
           return
     end subroutine stdlib_cpstrf

     pure module subroutine stdlib_zpstrf( uplo, n, a, lda, piv, rank, tol, work, info )
     !! ZPSTRF computes the Cholesky factorization with complete
     !! pivoting of a complex Hermitian positive semidefinite matrix A.
     !! The factorization has the form
     !! P**T * A * P = U**H * U ,  if UPLO = 'U',
     !! P**T * A * P = L  * L**H,  if UPLO = 'L',
     !! where U is an upper triangular matrix and L is lower triangular, and
     !! P is stored as vector PIV.
     !! This algorithm does not attempt to check that A is positive
     !! semidefinite. This version of the algorithm calls level 3 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(dp), intent(in) :: tol
           integer(ilp), intent(out) :: info, rank
           integer(ilp), intent(in) :: lda, n
           character, intent(in) :: uplo
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: work(2_ilp*n)
           integer(ilp), intent(out) :: piv(n)
        ! =====================================================================
           
           
           ! Local Scalars 
           complex(dp) :: ztemp
           real(dp) :: ajj, dstop, dtemp
           integer(ilp) :: i, itemp, j, jb, k, nb, pvt
           logical(lk) :: upper
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZPSTRF', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! get block size
           nb = stdlib_ilaenv( 1_ilp, 'ZPOTRF', uplo, n, -1_ilp, -1_ilp, -1_ilp )
           if( nb<=1_ilp .or. nb>=n ) then
              ! use unblocked code
              call stdlib_zpstf2( uplo, n, a( 1_ilp, 1_ilp ), lda, piv, rank, tol, work,info )
              go to 230
           else
           ! initialize piv
              do i = 1, n
                 piv( i ) = i
              end do
           ! compute stopping value
              do i = 1, n
                 work( i ) = real( a( i, i ),KIND=dp)
              end do
              pvt = maxloc( work( 1_ilp:n ), 1_ilp )
              ajj = real( a( pvt, pvt ),KIND=dp)
              if( ajj<=zero.or.stdlib_disnan( ajj ) ) then
                 rank = 0_ilp
                 info = 1_ilp
                 go to 230
              end if
           ! compute stopping value if not supplied
              if( tol<zero ) then
                 dstop = n * stdlib_dlamch( 'EPSILON' ) * ajj
              else
                 dstop = tol
              end if
              if( upper ) then
                 ! compute the cholesky factorization p**t * a * p = u**h * u
                 loop_160: do k = 1, n, nb
                    ! account for last block not being nb wide
                    jb = min( nb, n-k+1 )
                    ! set relevant part of first chalf of work to zero,
                    ! holds dot products
                    do i = k, n
                       work( i ) = 0_ilp
                    end do
                    loop_150: do j = k, k + jb - 1
                    ! find pivot, test for exit, else swap rows and columns
                    ! update dot products, compute possible pivots which are
                    ! stored in the second chalf of work
                       do i = j, n
                          if( j>k ) then
                             work( i ) = work( i ) +real( conjg( a( j-1, i ) )*a( j-1, i ),&
                                       KIND=dp)
                          end if
                          work( n+i ) = real( a( i, i ),KIND=dp) - work( i )
                       end do
                       if( j>1_ilp ) then
                          itemp = maxloc( work( (n+j):(2_ilp*n) ), 1_ilp )
                          pvt = itemp + j - 1_ilp
                          ajj = work( n+pvt )
                          if( ajj<=dstop.or.stdlib_disnan( ajj ) ) then
                             a( j, j ) = ajj
                             go to 220
                          end if
                       end if
                       if( j/=pvt ) then
                          ! pivot ok, so can now swap pivot rows and columns
                          a( pvt, pvt ) = a( j, j )
                          call stdlib_zswap( j-1, a( 1_ilp, j ), 1_ilp, a( 1_ilp, pvt ), 1_ilp )
                          if( pvt<n )call stdlib_zswap( n-pvt, a( j, pvt+1 ), lda,a( pvt, pvt+1 ),&
                                     lda )
                          do i = j + 1, pvt - 1
                             ztemp = conjg( a( j, i ) )
                             a( j, i ) = conjg( a( i, pvt ) )
                             a( i, pvt ) = ztemp
                          end do
                          a( j, pvt ) = conjg( a( j, pvt ) )
                          ! swap dot products and piv
                          dtemp = work( j )
                          work( j ) = work( pvt )
                          work( pvt ) = dtemp
                          itemp = piv( pvt )
                          piv( pvt ) = piv( j )
                          piv( j ) = itemp
                       end if
                       ajj = sqrt( ajj )
                       a( j, j ) = ajj
                       ! compute elements j+1:n of row j.
                       if( j<n ) then
                          call stdlib_zlacgv( j-1, a( 1_ilp, j ), 1_ilp )
                          call stdlib_zgemv( 'TRANS', j-k, n-j, -cone, a( k, j+1 ),lda, a( k, j ),&
                                     1_ilp, cone, a( j, j+1 ),lda )
                          call stdlib_zlacgv( j-1, a( 1_ilp, j ), 1_ilp )
                          call stdlib_zdscal( n-j, one / ajj, a( j, j+1 ), lda )
                       end if
                    end do loop_150
                    ! update trailing matrix, j already incremented
                    if( k+jb<=n ) then
                       call stdlib_zherk( 'UPPER', 'CONJ TRANS', n-j+1, jb, -one,a( k, j ), lda, &
                                 one, a( j, j ), lda )
                    end if
                 end do loop_160
              else
              ! compute the cholesky factorization p**t * a * p = l * l**h
                 loop_210: do k = 1, n, nb
                    ! account for last block not being nb wide
                    jb = min( nb, n-k+1 )
                    ! set relevant part of first chalf of work to zero,
                    ! holds dot products
                    do i = k, n
                       work( i ) = 0_ilp
                    end do
                    loop_200: do j = k, k + jb - 1
                    ! find pivot, test for exit, else swap rows and columns
                    ! update dot products, compute possible pivots which are
                    ! stored in the second chalf of work
                       do i = j, n
                          if( j>k ) then
                             work( i ) = work( i ) +real( conjg( a( i, j-1 ) )*a( i, j-1 ),&
                                       KIND=dp)
                          end if
                          work( n+i ) = real( a( i, i ),KIND=dp) - work( i )
                       end do
                       if( j>1_ilp ) then
                          itemp = maxloc( work( (n+j):(2_ilp*n) ), 1_ilp )
                          pvt = itemp + j - 1_ilp
                          ajj = work( n+pvt )
                          if( ajj<=dstop.or.stdlib_disnan( ajj ) ) then
                             a( j, j ) = ajj
                             go to 220
                          end if
                       end if
                       if( j/=pvt ) then
                          ! pivot ok, so can now swap pivot rows and columns
                          a( pvt, pvt ) = a( j, j )
                          call stdlib_zswap( j-1, a( j, 1_ilp ), lda, a( pvt, 1_ilp ), lda )
                          if( pvt<n )call stdlib_zswap( n-pvt, a( pvt+1, j ), 1_ilp,a( pvt+1, pvt ), &
                                    1_ilp )
                          do i = j + 1, pvt - 1
                             ztemp = conjg( a( i, j ) )
                             a( i, j ) = conjg( a( pvt, i ) )
                             a( pvt, i ) = ztemp
                          end do
                          a( pvt, j ) = conjg( a( pvt, j ) )
                          ! swap dot products and piv
                          dtemp = work( j )
                          work( j ) = work( pvt )
                          work( pvt ) = dtemp
                          itemp = piv( pvt )
                          piv( pvt ) = piv( j )
                          piv( j ) = itemp
                       end if
                       ajj = sqrt( ajj )
                       a( j, j ) = ajj
                       ! compute elements j+1:n of column j.
                       if( j<n ) then
                          call stdlib_zlacgv( j-1, a( j, 1_ilp ), lda )
                          call stdlib_zgemv( 'NO TRANS', n-j, j-k, -cone,a( j+1, k ), lda, a( j, &
                                    k ), lda, cone,a( j+1, j ), 1_ilp )
                          call stdlib_zlacgv( j-1, a( j, 1_ilp ), lda )
                          call stdlib_zdscal( n-j, one / ajj, a( j+1, j ), 1_ilp )
                       end if
                    end do loop_200
                    ! update trailing matrix, j already incremented
                    if( k+jb<=n ) then
                       call stdlib_zherk( 'LOWER', 'NO TRANS', n-j+1, jb, -one,a( j, k ), lda, &
                                 one, a( j, j ), lda )
                    end if
                 end do loop_210
              end if
           end if
           ! ran to completion, a has full rank
           rank = n
           go to 230
           220 continue
           ! rank is the number of steps completed.  set info = 1 to signal
           ! that the factorization cannot be used to solve a system.
           rank = j - 1_ilp
           info = 1_ilp
           230 continue
           return
     end subroutine stdlib_zpstrf




     pure module subroutine stdlib_spstf2( uplo, n, a, lda, piv, rank, tol, work, info )
     !! SPSTF2 computes the Cholesky factorization with complete
     !! pivoting of a real symmetric positive semidefinite matrix A.
     !! The factorization has the form
     !! P**T * A * P = U**T * U ,  if UPLO = 'U',
     !! P**T * A * P = L  * L**T,  if UPLO = 'L',
     !! where U is an upper triangular matrix and L is lower triangular, and
     !! P is stored as vector PIV.
     !! This algorithm does not attempt to check that A is positive
     !! semidefinite. This version of the algorithm calls level 2 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(sp), intent(in) :: tol
           integer(ilp), intent(out) :: info, rank
           integer(ilp), intent(in) :: lda, n
           character, intent(in) :: uplo
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: work(2_ilp*n)
           integer(ilp), intent(out) :: piv(n)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: ajj, sstop, stemp
           integer(ilp) :: i, itemp, j, pvt
           logical(lk) :: upper
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SPSTF2', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! initialize piv
           do i = 1, n
              piv( i ) = i
           end do
           ! compute stopping value
           pvt = 1_ilp
           ajj = a( pvt, pvt )
           do i = 2, n
              if( a( i, i )>ajj ) then
                 pvt = i
                 ajj = a( pvt, pvt )
              end if
           end do
           if( ajj<=zero.or.stdlib_sisnan( ajj ) ) then
              rank = 0_ilp
              info = 1_ilp
              go to 170
           end if
           ! compute stopping value if not supplied
           if( tol<zero ) then
              sstop = n * stdlib_slamch( 'EPSILON' ) * ajj
           else
              sstop = tol
           end if
           ! set first half of work to zero, holds dot products
           do i = 1, n
              work( i ) = 0_ilp
           end do
           if( upper ) then
              ! compute the cholesky factorization p**t * a * p = u**t * u
              loop_130: do j = 1, n
              ! find pivot, test for exit, else swap rows and columns
              ! update dot products, compute possible pivots which are
              ! stored in the second half of work
                 do i = j, n
                    if( j>1_ilp ) then
                       work( i ) = work( i ) + a( j-1, i )**2_ilp
                    end if
                    work( n+i ) = a( i, i ) - work( i )
                 end do
                 if( j>1_ilp ) then
                    itemp = maxloc( work( (n+j):(2_ilp*n) ), 1_ilp )
                    pvt = itemp + j - 1_ilp
                    ajj = work( n+pvt )
                    if( ajj<=sstop.or.stdlib_sisnan( ajj ) ) then
                       a( j, j ) = ajj
                       go to 160
                    end if
                 end if
                 if( j/=pvt ) then
                    ! pivot ok, so can now swap pivot rows and columns
                    a( pvt, pvt ) = a( j, j )
                    call stdlib_sswap( j-1, a( 1_ilp, j ), 1_ilp, a( 1_ilp, pvt ), 1_ilp )
                    if( pvt<n )call stdlib_sswap( n-pvt, a( j, pvt+1 ), lda,a( pvt, pvt+1 ), lda )
                              
                    call stdlib_sswap( pvt-j-1, a( j, j+1 ), lda, a( j+1, pvt ), 1_ilp )
                    ! swap dot products and piv
                    stemp = work( j )
                    work( j ) = work( pvt )
                    work( pvt ) = stemp
                    itemp = piv( pvt )
                    piv( pvt ) = piv( j )
                    piv( j ) = itemp
                 end if
                 ajj = sqrt( ajj )
                 a( j, j ) = ajj
                 ! compute elements j+1:n of row j
                 if( j<n ) then
                    call stdlib_sgemv( 'TRANS', j-1, n-j, -one, a( 1_ilp, j+1 ), lda,a( 1_ilp, j ), 1_ilp, &
                              one, a( j, j+1 ), lda )
                    call stdlib_sscal( n-j, one / ajj, a( j, j+1 ), lda )
                 end if
              end do loop_130
           else
              ! compute the cholesky factorization p**t * a * p = l * l**t
              loop_150: do j = 1, n
              ! find pivot, test for exit, else swap rows and columns
              ! update dot products, compute possible pivots which are
              ! stored in the second half of work
                 do i = j, n
                    if( j>1_ilp ) then
                       work( i ) = work( i ) + a( i, j-1 )**2_ilp
                    end if
                    work( n+i ) = a( i, i ) - work( i )
                 end do
                 if( j>1_ilp ) then
                    itemp = maxloc( work( (n+j):(2_ilp*n) ), 1_ilp )
                    pvt = itemp + j - 1_ilp
                    ajj = work( n+pvt )
                    if( ajj<=sstop.or.stdlib_sisnan( ajj ) ) then
                       a( j, j ) = ajj
                       go to 160
                    end if
                 end if
                 if( j/=pvt ) then
                    ! pivot ok, so can now swap pivot rows and columns
                    a( pvt, pvt ) = a( j, j )
                    call stdlib_sswap( j-1, a( j, 1_ilp ), lda, a( pvt, 1_ilp ), lda )
                    if( pvt<n )call stdlib_sswap( n-pvt, a( pvt+1, j ), 1_ilp, a( pvt+1, pvt ),1_ilp )
                              
                    call stdlib_sswap( pvt-j-1, a( j+1, j ), 1_ilp, a( pvt, j+1 ), lda )
                    ! swap dot products and piv
                    stemp = work( j )
                    work( j ) = work( pvt )
                    work( pvt ) = stemp
                    itemp = piv( pvt )
                    piv( pvt ) = piv( j )
                    piv( j ) = itemp
                 end if
                 ajj = sqrt( ajj )
                 a( j, j ) = ajj
                 ! compute elements j+1:n of column j
                 if( j<n ) then
                    call stdlib_sgemv( 'NO TRANS', n-j, j-1, -one, a( j+1, 1_ilp ), lda,a( j, 1_ilp ), &
                              lda, one, a( j+1, j ), 1_ilp )
                    call stdlib_sscal( n-j, one / ajj, a( j+1, j ), 1_ilp )
                 end if
              end do loop_150
           end if
           ! ran to completion, a has full rank
           rank = n
           go to 170
           160 continue
           ! rank is number of steps completed.  set info = 1 to signal
           ! that the factorization cannot be used to solve a system.
           rank = j - 1_ilp
           info = 1_ilp
           170 continue
           return
     end subroutine stdlib_spstf2

     pure module subroutine stdlib_dpstf2( uplo, n, a, lda, piv, rank, tol, work, info )
     !! DPSTF2 computes the Cholesky factorization with complete
     !! pivoting of a real symmetric positive semidefinite matrix A.
     !! The factorization has the form
     !! P**T * A * P = U**T * U ,  if UPLO = 'U',
     !! P**T * A * P = L  * L**T,  if UPLO = 'L',
     !! where U is an upper triangular matrix and L is lower triangular, and
     !! P is stored as vector PIV.
     !! This algorithm does not attempt to check that A is positive
     !! semidefinite. This version of the algorithm calls level 2 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(dp), intent(in) :: tol
           integer(ilp), intent(out) :: info, rank
           integer(ilp), intent(in) :: lda, n
           character, intent(in) :: uplo
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: work(2_ilp*n)
           integer(ilp), intent(out) :: piv(n)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: ajj, dstop, dtemp
           integer(ilp) :: i, itemp, j, pvt
           logical(lk) :: upper
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DPSTF2', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! initialize piv
           do i = 1, n
              piv( i ) = i
           end do
           ! compute stopping value
           pvt = 1_ilp
           ajj = a( pvt, pvt )
           do i = 2, n
              if( a( i, i )>ajj ) then
                 pvt = i
                 ajj = a( pvt, pvt )
              end if
           end do
           if( ajj<=zero.or.stdlib_disnan( ajj ) ) then
              rank = 0_ilp
              info = 1_ilp
              go to 170
           end if
           ! compute stopping value if not supplied
           if( tol<zero ) then
              dstop = n * stdlib_dlamch( 'EPSILON' ) * ajj
           else
              dstop = tol
           end if
           ! set first half of work to zero, holds dot products
           do i = 1, n
              work( i ) = 0_ilp
           end do
           if( upper ) then
              ! compute the cholesky factorization p**t * a * p = u**t * u
              loop_130: do j = 1, n
              ! find pivot, test for exit, else swap rows and columns
              ! update dot products, compute possible pivots which are
              ! stored in the second half of work
                 do i = j, n
                    if( j>1_ilp ) then
                       work( i ) = work( i ) + a( j-1, i )**2_ilp
                    end if
                    work( n+i ) = a( i, i ) - work( i )
                 end do
                 if( j>1_ilp ) then
                    itemp = maxloc( work( (n+j):(2_ilp*n) ), 1_ilp )
                    pvt = itemp + j - 1_ilp
                    ajj = work( n+pvt )
                    if( ajj<=dstop.or.stdlib_disnan( ajj ) ) then
                       a( j, j ) = ajj
                       go to 160
                    end if
                 end if
                 if( j/=pvt ) then
                    ! pivot ok, so can now swap pivot rows and columns
                    a( pvt, pvt ) = a( j, j )
                    call stdlib_dswap( j-1, a( 1_ilp, j ), 1_ilp, a( 1_ilp, pvt ), 1_ilp )
                    if( pvt<n )call stdlib_dswap( n-pvt, a( j, pvt+1 ), lda,a( pvt, pvt+1 ), lda )
                              
                    call stdlib_dswap( pvt-j-1, a( j, j+1 ), lda, a( j+1, pvt ), 1_ilp )
                    ! swap dot products and piv
                    dtemp = work( j )
                    work( j ) = work( pvt )
                    work( pvt ) = dtemp
                    itemp = piv( pvt )
                    piv( pvt ) = piv( j )
                    piv( j ) = itemp
                 end if
                 ajj = sqrt( ajj )
                 a( j, j ) = ajj
                 ! compute elements j+1:n of row j
                 if( j<n ) then
                    call stdlib_dgemv( 'TRANS', j-1, n-j, -one, a( 1_ilp, j+1 ), lda,a( 1_ilp, j ), 1_ilp, &
                              one, a( j, j+1 ), lda )
                    call stdlib_dscal( n-j, one / ajj, a( j, j+1 ), lda )
                 end if
              end do loop_130
           else
              ! compute the cholesky factorization p**t * a * p = l * l**t
              loop_150: do j = 1, n
              ! find pivot, test for exit, else swap rows and columns
              ! update dot products, compute possible pivots which are
              ! stored in the second half of work
                 do i = j, n
                    if( j>1_ilp ) then
                       work( i ) = work( i ) + a( i, j-1 )**2_ilp
                    end if
                    work( n+i ) = a( i, i ) - work( i )
                 end do
                 if( j>1_ilp ) then
                    itemp = maxloc( work( (n+j):(2_ilp*n) ), 1_ilp )
                    pvt = itemp + j - 1_ilp
                    ajj = work( n+pvt )
                    if( ajj<=dstop.or.stdlib_disnan( ajj ) ) then
                       a( j, j ) = ajj
                       go to 160
                    end if
                 end if
                 if( j/=pvt ) then
                    ! pivot ok, so can now swap pivot rows and columns
                    a( pvt, pvt ) = a( j, j )
                    call stdlib_dswap( j-1, a( j, 1_ilp ), lda, a( pvt, 1_ilp ), lda )
                    if( pvt<n )call stdlib_dswap( n-pvt, a( pvt+1, j ), 1_ilp, a( pvt+1, pvt ),1_ilp )
                              
                    call stdlib_dswap( pvt-j-1, a( j+1, j ), 1_ilp, a( pvt, j+1 ), lda )
                    ! swap dot products and piv
                    dtemp = work( j )
                    work( j ) = work( pvt )
                    work( pvt ) = dtemp
                    itemp = piv( pvt )
                    piv( pvt ) = piv( j )
                    piv( j ) = itemp
                 end if
                 ajj = sqrt( ajj )
                 a( j, j ) = ajj
                 ! compute elements j+1:n of column j
                 if( j<n ) then
                    call stdlib_dgemv( 'NO TRANS', n-j, j-1, -one, a( j+1, 1_ilp ), lda,a( j, 1_ilp ), &
                              lda, one, a( j+1, j ), 1_ilp )
                    call stdlib_dscal( n-j, one / ajj, a( j+1, j ), 1_ilp )
                 end if
              end do loop_150
           end if
           ! ran to completion, a has full rank
           rank = n
           go to 170
           160 continue
           ! rank is number of steps completed.  set info = 1 to signal
           ! that the factorization cannot be used to solve a system.
           rank = j - 1_ilp
           info = 1_ilp
           170 continue
           return
     end subroutine stdlib_dpstf2


     pure module subroutine stdlib_cpstf2( uplo, n, a, lda, piv, rank, tol, work, info )
     !! CPSTF2 computes the Cholesky factorization with complete
     !! pivoting of a complex Hermitian positive semidefinite matrix A.
     !! The factorization has the form
     !! P**T * A * P = U**H * U ,  if UPLO = 'U',
     !! P**T * A * P = L  * L**H,  if UPLO = 'L',
     !! where U is an upper triangular matrix and L is lower triangular, and
     !! P is stored as vector PIV.
     !! This algorithm does not attempt to check that A is positive
     !! semidefinite. This version of the algorithm calls level 2 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(sp), intent(in) :: tol
           integer(ilp), intent(out) :: info, rank
           integer(ilp), intent(in) :: lda, n
           character, intent(in) :: uplo
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: work(2_ilp*n)
           integer(ilp), intent(out) :: piv(n)
        ! =====================================================================
           
           
           ! Local Scalars 
           complex(sp) :: ctemp
           real(sp) :: ajj, sstop, stemp
           integer(ilp) :: i, itemp, j, pvt
           logical(lk) :: upper
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CPSTF2', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! initialize piv
           do i = 1, n
              piv( i ) = i
           end do
           ! compute stopping value
           do i = 1, n
              work( i ) = real( a( i, i ),KIND=sp)
           end do
           pvt = maxloc( work( 1_ilp:n ), 1_ilp )
           ajj = real( a( pvt, pvt ),KIND=sp)
           if( ajj<=zero.or.stdlib_sisnan( ajj ) ) then
              rank = 0_ilp
              info = 1_ilp
              go to 200
           end if
           ! compute stopping value if not supplied
           if( tol<zero ) then
              sstop = n * stdlib_slamch( 'EPSILON' ) * ajj
           else
              sstop = tol
           end if
           ! set first chalf of work to zero, holds dot products
           do i = 1, n
              work( i ) = 0_ilp
           end do
           if( upper ) then
              ! compute the cholesky factorization p**t * a * p = u**h * u
              loop_150: do j = 1, n
              ! find pivot, test for exit, else swap rows and columns
              ! update dot products, compute possible pivots which are
              ! stored in the second chalf of work
                 do i = j, n
                    if( j>1_ilp ) then
                       work( i ) = work( i ) +real( conjg( a( j-1, i ) )*a( j-1, i ),KIND=sp)
                                 
                    end if
                    work( n+i ) = real( a( i, i ),KIND=sp) - work( i )
                 end do
                 if( j>1_ilp ) then
                    itemp = maxloc( work( (n+j):(2_ilp*n) ), 1_ilp )
                    pvt = itemp + j - 1_ilp
                    ajj = work( n+pvt )
                    if( ajj<=sstop.or.stdlib_sisnan( ajj ) ) then
                       a( j, j ) = ajj
                       go to 190
                    end if
                 end if
                 if( j/=pvt ) then
                    ! pivot ok, so can now swap pivot rows and columns
                    a( pvt, pvt ) = a( j, j )
                    call stdlib_cswap( j-1, a( 1_ilp, j ), 1_ilp, a( 1_ilp, pvt ), 1_ilp )
                    if( pvt<n )call stdlib_cswap( n-pvt, a( j, pvt+1 ), lda,a( pvt, pvt+1 ), lda )
                              
                    do i = j + 1, pvt - 1
                       ctemp = conjg( a( j, i ) )
                       a( j, i ) = conjg( a( i, pvt ) )
                       a( i, pvt ) = ctemp
                    end do
                    a( j, pvt ) = conjg( a( j, pvt ) )
                    ! swap dot products and piv
                    stemp = work( j )
                    work( j ) = work( pvt )
                    work( pvt ) = stemp
                    itemp = piv( pvt )
                    piv( pvt ) = piv( j )
                    piv( j ) = itemp
                 end if
                 ajj = sqrt( ajj )
                 a( j, j ) = ajj
                 ! compute elements j+1:n of row j
                 if( j<n ) then
                    call stdlib_clacgv( j-1, a( 1_ilp, j ), 1_ilp )
                    call stdlib_cgemv( 'TRANS', j-1, n-j, -cone, a( 1_ilp, j+1 ), lda,a( 1_ilp, j ), 1_ilp, &
                              cone, a( j, j+1 ), lda )
                    call stdlib_clacgv( j-1, a( 1_ilp, j ), 1_ilp )
                    call stdlib_csscal( n-j, one / ajj, a( j, j+1 ), lda )
                 end if
              end do loop_150
           else
              ! compute the cholesky factorization p**t * a * p = l * l**h
              loop_180: do j = 1, n
              ! find pivot, test for exit, else swap rows and columns
              ! update dot products, compute possible pivots which are
              ! stored in the second chalf of work
                 do i = j, n
                    if( j>1_ilp ) then
                       work( i ) = work( i ) +real( conjg( a( i, j-1 ) )*a( i, j-1 ),KIND=sp)
                                 
                    end if
                    work( n+i ) = real( a( i, i ),KIND=sp) - work( i )
                 end do
                 if( j>1_ilp ) then
                    itemp = maxloc( work( (n+j):(2_ilp*n) ), 1_ilp )
                    pvt = itemp + j - 1_ilp
                    ajj = work( n+pvt )
                    if( ajj<=sstop.or.stdlib_sisnan( ajj ) ) then
                       a( j, j ) = ajj
                       go to 190
                    end if
                 end if
                 if( j/=pvt ) then
                    ! pivot ok, so can now swap pivot rows and columns
                    a( pvt, pvt ) = a( j, j )
                    call stdlib_cswap( j-1, a( j, 1_ilp ), lda, a( pvt, 1_ilp ), lda )
                    if( pvt<n )call stdlib_cswap( n-pvt, a( pvt+1, j ), 1_ilp, a( pvt+1, pvt ),1_ilp )
                              
                    do i = j + 1, pvt - 1
                       ctemp = conjg( a( i, j ) )
                       a( i, j ) = conjg( a( pvt, i ) )
                       a( pvt, i ) = ctemp
                    end do
                    a( pvt, j ) = conjg( a( pvt, j ) )
                    ! swap dot products and piv
                    stemp = work( j )
                    work( j ) = work( pvt )
                    work( pvt ) = stemp
                    itemp = piv( pvt )
                    piv( pvt ) = piv( j )
                    piv( j ) = itemp
                 end if
                 ajj = sqrt( ajj )
                 a( j, j ) = ajj
                 ! compute elements j+1:n of column j
                 if( j<n ) then
                    call stdlib_clacgv( j-1, a( j, 1_ilp ), lda )
                    call stdlib_cgemv( 'NO TRANS', n-j, j-1, -cone, a( j+1, 1_ilp ),lda, a( j, 1_ilp ), &
                              lda, cone, a( j+1, j ), 1_ilp )
                    call stdlib_clacgv( j-1, a( j, 1_ilp ), lda )
                    call stdlib_csscal( n-j, one / ajj, a( j+1, j ), 1_ilp )
                 end if
              end do loop_180
           end if
           ! ran to completion, a has full rank
           rank = n
           go to 200
           190 continue
           ! rank is number of steps completed.  set info = 1 to signal
           ! that the factorization cannot be used to solve a system.
           rank = j - 1_ilp
           info = 1_ilp
           200 continue
           return
     end subroutine stdlib_cpstf2

     pure module subroutine stdlib_zpstf2( uplo, n, a, lda, piv, rank, tol, work, info )
     !! ZPSTF2 computes the Cholesky factorization with complete
     !! pivoting of a complex Hermitian positive semidefinite matrix A.
     !! The factorization has the form
     !! P**T * A * P = U**H * U ,  if UPLO = 'U',
     !! P**T * A * P = L  * L**H,  if UPLO = 'L',
     !! where U is an upper triangular matrix and L is lower triangular, and
     !! P is stored as vector PIV.
     !! This algorithm does not attempt to check that A is positive
     !! semidefinite. This version of the algorithm calls level 2 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(dp), intent(in) :: tol
           integer(ilp), intent(out) :: info, rank
           integer(ilp), intent(in) :: lda, n
           character, intent(in) :: uplo
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: work(2_ilp*n)
           integer(ilp), intent(out) :: piv(n)
        ! =====================================================================
           
           
           ! Local Scalars 
           complex(dp) :: ztemp
           real(dp) :: ajj, dstop, dtemp
           integer(ilp) :: i, itemp, j, pvt
           logical(lk) :: upper
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZPSTF2', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! initialize piv
           do i = 1, n
              piv( i ) = i
           end do
           ! compute stopping value
           do i = 1, n
              work( i ) = real( a( i, i ),KIND=dp)
           end do
           pvt = maxloc( work( 1_ilp:n ), 1_ilp )
           ajj = real( a( pvt, pvt ),KIND=dp)
           if( ajj<=zero.or.stdlib_disnan( ajj ) ) then
              rank = 0_ilp
              info = 1_ilp
              go to 200
           end if
           ! compute stopping value if not supplied
           if( tol<zero ) then
              dstop = n * stdlib_dlamch( 'EPSILON' ) * ajj
           else
              dstop = tol
           end if
           ! set first chalf of work to zero, holds dot products
           do i = 1, n
              work( i ) = 0_ilp
           end do
           if( upper ) then
              ! compute the cholesky factorization p**t * a * p = u**h* u
              loop_150: do j = 1, n
              ! find pivot, test for exit, else swap rows and columns
              ! update dot products, compute possible pivots which are
              ! stored in the second chalf of work
                 do i = j, n
                    if( j>1_ilp ) then
                       work( i ) = work( i ) +real( conjg( a( j-1, i ) )*a( j-1, i ),KIND=dp)
                                 
                    end if
                    work( n+i ) = real( a( i, i ),KIND=dp) - work( i )
                 end do
                 if( j>1_ilp ) then
                    itemp = maxloc( work( (n+j):(2_ilp*n) ), 1_ilp )
                    pvt = itemp + j - 1_ilp
                    ajj = work( n+pvt )
                    if( ajj<=dstop.or.stdlib_disnan( ajj ) ) then
                       a( j, j ) = ajj
                       go to 190
                    end if
                 end if
                 if( j/=pvt ) then
                    ! pivot ok, so can now swap pivot rows and columns
                    a( pvt, pvt ) = a( j, j )
                    call stdlib_zswap( j-1, a( 1_ilp, j ), 1_ilp, a( 1_ilp, pvt ), 1_ilp )
                    if( pvt<n )call stdlib_zswap( n-pvt, a( j, pvt+1 ), lda,a( pvt, pvt+1 ), lda )
                              
                    do i = j + 1, pvt - 1
                       ztemp = conjg( a( j, i ) )
                       a( j, i ) = conjg( a( i, pvt ) )
                       a( i, pvt ) = ztemp
                    end do
                    a( j, pvt ) = conjg( a( j, pvt ) )
                    ! swap dot products and piv
                    dtemp = work( j )
                    work( j ) = work( pvt )
                    work( pvt ) = dtemp
                    itemp = piv( pvt )
                    piv( pvt ) = piv( j )
                    piv( j ) = itemp
                 end if
                 ajj = sqrt( ajj )
                 a( j, j ) = ajj
                 ! compute elements j+1:n of row j
                 if( j<n ) then
                    call stdlib_zlacgv( j-1, a( 1_ilp, j ), 1_ilp )
                    call stdlib_zgemv( 'TRANS', j-1, n-j, -cone, a( 1_ilp, j+1 ), lda,a( 1_ilp, j ), 1_ilp, &
                              cone, a( j, j+1 ), lda )
                    call stdlib_zlacgv( j-1, a( 1_ilp, j ), 1_ilp )
                    call stdlib_zdscal( n-j, one / ajj, a( j, j+1 ), lda )
                 end if
              end do loop_150
           else
              ! compute the cholesky factorization p**t * a * p = l * l**h
              loop_180: do j = 1, n
              ! find pivot, test for exit, else swap rows and columns
              ! update dot products, compute possible pivots which are
              ! stored in the second chalf of work
                 do i = j, n
                    if( j>1_ilp ) then
                       work( i ) = work( i ) +real( conjg( a( i, j-1 ) )*a( i, j-1 ),KIND=dp)
                                 
                    end if
                    work( n+i ) = real( a( i, i ),KIND=dp) - work( i )
                 end do
                 if( j>1_ilp ) then
                    itemp = maxloc( work( (n+j):(2_ilp*n) ), 1_ilp )
                    pvt = itemp + j - 1_ilp
                    ajj = work( n+pvt )
                    if( ajj<=dstop.or.stdlib_disnan( ajj ) ) then
                       a( j, j ) = ajj
                       go to 190
                    end if
                 end if
                 if( j/=pvt ) then
                    ! pivot ok, so can now swap pivot rows and columns
                    a( pvt, pvt ) = a( j, j )
                    call stdlib_zswap( j-1, a( j, 1_ilp ), lda, a( pvt, 1_ilp ), lda )
                    if( pvt<n )call stdlib_zswap( n-pvt, a( pvt+1, j ), 1_ilp, a( pvt+1, pvt ),1_ilp )
                              
                    do i = j + 1, pvt - 1
                       ztemp = conjg( a( i, j ) )
                       a( i, j ) = conjg( a( pvt, i ) )
                       a( pvt, i ) = ztemp
                    end do
                    a( pvt, j ) = conjg( a( pvt, j ) )
                    ! swap dot products and piv
                    dtemp = work( j )
                    work( j ) = work( pvt )
                    work( pvt ) = dtemp
                    itemp = piv( pvt )
                    piv( pvt ) = piv( j )
                    piv( j ) = itemp
                 end if
                 ajj = sqrt( ajj )
                 a( j, j ) = ajj
                 ! compute elements j+1:n of column j
                 if( j<n ) then
                    call stdlib_zlacgv( j-1, a( j, 1_ilp ), lda )
                    call stdlib_zgemv( 'NO TRANS', n-j, j-1, -cone, a( j+1, 1_ilp ),lda, a( j, 1_ilp ), &
                              lda, cone, a( j+1, j ), 1_ilp )
                    call stdlib_zlacgv( j-1, a( j, 1_ilp ), lda )
                    call stdlib_zdscal( n-j, one / ajj, a( j+1, j ), 1_ilp )
                 end if
              end do loop_180
           end if
           ! ran to completion, a has full rank
           rank = n
           go to 200
           190 continue
           ! rank is number of steps completed.  set info = 1 to signal
           ! that the factorization cannot be used to solve a system.
           rank = j - 1_ilp
           info = 1_ilp
           200 continue
           return
     end subroutine stdlib_zpstf2




     pure module subroutine stdlib_spotrs( uplo, n, nrhs, a, lda, b, ldb, info )
     !! SPOTRS solves a system of linear equations A*X = B with a symmetric
     !! positive definite matrix A using the Cholesky factorization
     !! A = U**T*U or A = L*L**T computed by SPOTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SPOTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( upper ) then
              ! solve a*x = b where a = u**t *u.
              ! solve u**t *x = b, overwriting b with x.
              call stdlib_strsm( 'LEFT', 'UPPER', 'TRANSPOSE', 'NON-UNIT', n, nrhs,one, a, lda, b,&
                         ldb )
              ! solve u*x = b, overwriting b with x.
              call stdlib_strsm( 'LEFT', 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', n,nrhs, one, a, lda,&
                         b, ldb )
           else
              ! solve a*x = b where a = l*l**t.
              ! solve l*x = b, overwriting b with x.
              call stdlib_strsm( 'LEFT', 'LOWER', 'NO TRANSPOSE', 'NON-UNIT', n,nrhs, one, a, lda,&
                         b, ldb )
              ! solve l**t *x = b, overwriting b with x.
              call stdlib_strsm( 'LEFT', 'LOWER', 'TRANSPOSE', 'NON-UNIT', n, nrhs,one, a, lda, b,&
                         ldb )
           end if
           return
     end subroutine stdlib_spotrs

     pure module subroutine stdlib_dpotrs( uplo, n, nrhs, a, lda, b, ldb, info )
     !! DPOTRS solves a system of linear equations A*X = B with a symmetric
     !! positive definite matrix A using the Cholesky factorization
     !! A = U**T*U or A = L*L**T computed by DPOTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DPOTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( upper ) then
              ! solve a*x = b where a = u**t *u.
              ! solve u**t *x = b, overwriting b with x.
              call stdlib_dtrsm( 'LEFT', 'UPPER', 'TRANSPOSE', 'NON-UNIT', n, nrhs,one, a, lda, b,&
                         ldb )
              ! solve u*x = b, overwriting b with x.
              call stdlib_dtrsm( 'LEFT', 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', n,nrhs, one, a, lda,&
                         b, ldb )
           else
              ! solve a*x = b where a = l*l**t.
              ! solve l*x = b, overwriting b with x.
              call stdlib_dtrsm( 'LEFT', 'LOWER', 'NO TRANSPOSE', 'NON-UNIT', n,nrhs, one, a, lda,&
                         b, ldb )
              ! solve l**t *x = b, overwriting b with x.
              call stdlib_dtrsm( 'LEFT', 'LOWER', 'TRANSPOSE', 'NON-UNIT', n, nrhs,one, a, lda, b,&
                         ldb )
           end if
           return
     end subroutine stdlib_dpotrs


     pure module subroutine stdlib_cpotrs( uplo, n, nrhs, a, lda, b, ldb, info )
     !! CPOTRS solves a system of linear equations A*X = B with a Hermitian
     !! positive definite matrix A using the Cholesky factorization
     !! A = U**H*U or A = L*L**H computed by CPOTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CPOTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( upper ) then
              ! solve a*x = b where a = u**h *u.
              ! solve u**h *x = b, overwriting b with x.
              call stdlib_ctrsm( 'LEFT', 'UPPER', 'CONJUGATE TRANSPOSE', 'NON-UNIT',n, nrhs, cone,&
                         a, lda, b, ldb )
              ! solve u*x = b, overwriting b with x.
              call stdlib_ctrsm( 'LEFT', 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', n,nrhs, cone, a, &
                        lda, b, ldb )
           else
              ! solve a*x = b where a = l*l**h.
              ! solve l*x = b, overwriting b with x.
              call stdlib_ctrsm( 'LEFT', 'LOWER', 'NO TRANSPOSE', 'NON-UNIT', n,nrhs, cone, a, &
                        lda, b, ldb )
              ! solve l**h *x = b, overwriting b with x.
              call stdlib_ctrsm( 'LEFT', 'LOWER', 'CONJUGATE TRANSPOSE', 'NON-UNIT',n, nrhs, cone,&
                         a, lda, b, ldb )
           end if
           return
     end subroutine stdlib_cpotrs

     pure module subroutine stdlib_zpotrs( uplo, n, nrhs, a, lda, b, ldb, info )
     !! ZPOTRS solves a system of linear equations A*X = B with a Hermitian
     !! positive definite matrix A using the Cholesky factorization
     !! A = U**H * U or A = L * L**H computed by ZPOTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZPOTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( upper ) then
              ! solve a*x = b where a = u**h *u.
              ! solve u**h *x = b, overwriting b with x.
              call stdlib_ztrsm( 'LEFT', 'UPPER', 'CONJUGATE TRANSPOSE', 'NON-UNIT',n, nrhs, cone,&
                         a, lda, b, ldb )
              ! solve u*x = b, overwriting b with x.
              call stdlib_ztrsm( 'LEFT', 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', n,nrhs, cone, a, &
                        lda, b, ldb )
           else
              ! solve a*x = b where a = l*l**h.
              ! solve l*x = b, overwriting b with x.
              call stdlib_ztrsm( 'LEFT', 'LOWER', 'NO TRANSPOSE', 'NON-UNIT', n,nrhs, cone, a, &
                        lda, b, ldb )
              ! solve l**h *x = b, overwriting b with x.
              call stdlib_ztrsm( 'LEFT', 'LOWER', 'CONJUGATE TRANSPOSE', 'NON-UNIT',n, nrhs, cone,&
                         a, lda, b, ldb )
           end if
           return
     end subroutine stdlib_zpotrs




     pure module subroutine stdlib_spotri( uplo, n, a, lda, info )
     !! SPOTRI computes the inverse of a real symmetric positive definite
     !! matrix A using the Cholesky factorization A = U**T*U or A = L*L**T
     !! computed by SPOTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SPOTRI', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! invert the triangular cholesky factor u or l.
           call stdlib_strtri( uplo, 'NON-UNIT', n, a, lda, info )
           if( info>0 )return
           ! form inv(u) * inv(u)**t or inv(l)**t * inv(l).
           call stdlib_slauum( uplo, n, a, lda, info )
           return
     end subroutine stdlib_spotri

     pure module subroutine stdlib_dpotri( uplo, n, a, lda, info )
     !! DPOTRI computes the inverse of a real symmetric positive definite
     !! matrix A using the Cholesky factorization A = U**T*U or A = L*L**T
     !! computed by DPOTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DPOTRI', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! invert the triangular cholesky factor u or l.
           call stdlib_dtrtri( uplo, 'NON-UNIT', n, a, lda, info )
           if( info>0 )return
           ! form inv(u) * inv(u)**t or inv(l)**t * inv(l).
           call stdlib_dlauum( uplo, n, a, lda, info )
           return
     end subroutine stdlib_dpotri


     pure module subroutine stdlib_cpotri( uplo, n, a, lda, info )
     !! CPOTRI computes the inverse of a complex Hermitian positive definite
     !! matrix A using the Cholesky factorization A = U**H*U or A = L*L**H
     !! computed by CPOTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CPOTRI', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! invert the triangular cholesky factor u or l.
           call stdlib_ctrtri( uplo, 'NON-UNIT', n, a, lda, info )
           if( info>0 )return
           ! form inv(u) * inv(u)**h or inv(l)**h * inv(l).
           call stdlib_clauum( uplo, n, a, lda, info )
           return
     end subroutine stdlib_cpotri

     pure module subroutine stdlib_zpotri( uplo, n, a, lda, info )
     !! ZPOTRI computes the inverse of a complex Hermitian positive definite
     !! matrix A using the Cholesky factorization A = U**H*U or A = L*L**H
     !! computed by ZPOTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZPOTRI', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! invert the triangular cholesky factor u or l.
           call stdlib_ztrtri( uplo, 'NON-UNIT', n, a, lda, info )
           if( info>0 )return
           ! form inv(u) * inv(u)**h or inv(l)**h * inv(l).
           call stdlib_zlauum( uplo, n, a, lda, info )
           return
     end subroutine stdlib_zpotri




     pure module subroutine stdlib_sporfs( uplo, n, nrhs, a, lda, af, ldaf, b, ldb, x,ldx, ferr, berr, &
     !! SPORFS improves the computed solution to a system of linear
     !! equations when the coefficient matrix is symmetric positive definite,
     !! and provides error bounds and backward error estimates for the
     !! solution.
               work, iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldaf, ldb, ldx, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(in) :: a(lda,*), af(ldaf,*), b(ldb,*)
           real(sp), intent(out) :: berr(*), ferr(*), work(*)
           real(sp), intent(inout) :: x(ldx,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: count, i, j, k, kase, nz
           real(sp) :: eps, lstres, s, safe1, safe2, safmin, xk
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldaf<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SPORFS', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp .or. nrhs==0_ilp ) then
              do j = 1, nrhs
                 ferr( j ) = zero
                 berr( j ) = zero
              end do
              return
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = n + 1_ilp
           eps = stdlib_slamch( 'EPSILON' )
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           safe1 = nz*safmin
           safe2 = safe1 / eps
           ! do for each right hand side
           loop_140: do j = 1, nrhs
              count = 1_ilp
              lstres = three
              20 continue
              ! loop until stopping criterion is satisfied.
              ! compute residual r = b - a * x
              call stdlib_scopy( n, b( 1_ilp, j ), 1_ilp, work( n+1 ), 1_ilp )
              call stdlib_ssymv( uplo, n, -one, a, lda, x( 1_ilp, j ), 1_ilp, one,work( n+1 ), 1_ilp )
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(a)*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              do i = 1, n
                 work( i ) = abs( b( i, j ) )
              end do
              ! compute abs(a)*abs(x) + abs(b).
              if( upper ) then
                 do k = 1, n
                    s = zero
                    xk = abs( x( k, j ) )
                    do i = 1, k - 1
                       work( i ) = work( i ) + abs( a( i, k ) )*xk
                       s = s + abs( a( i, k ) )*abs( x( i, j ) )
                    end do
                    work( k ) = work( k ) + abs( a( k, k ) )*xk + s
                 end do
              else
                 do k = 1, n
                    s = zero
                    xk = abs( x( k, j ) )
                    work( k ) = work( k ) + abs( a( k, k ) )*xk
                    do i = k + 1, n
                       work( i ) = work( i ) + abs( a( i, k ) )*xk
                       s = s + abs( a( i, k ) )*abs( x( i, j ) )
                    end do
                    work( k ) = work( k ) + s
                 end do
              end if
              s = zero
              do i = 1, n
                 if( work( i )>safe2 ) then
                    s = max( s, abs( work( n+i ) ) / work( i ) )
                 else
                    s = max( s, ( abs( work( n+i ) )+safe1 ) /( work( i )+safe1 ) )
                 end if
              end do
              berr( j ) = s
              ! test stopping criterion. continue iterating if
                 ! 1) the residual berr(j) is larger than machine epsilon, and
                 ! 2) berr(j) decreased by at least a factor of 2 during the
                    ! last iteration, and
                 ! 3) at most itmax iterations tried.
              if( berr( j )>eps .and. two*berr( j )<=lstres .and.count<=itmax ) then
                 ! update solution and try again.
                 call stdlib_spotrs( uplo, n, 1_ilp, af, ldaf, work( n+1 ), n, info )
                 call stdlib_saxpy( n, one, work( n+1 ), 1_ilp, x( 1_ilp, j ), 1_ilp )
                 lstres = berr( j )
                 count = count + 1_ilp
                 go to 20
              end if
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(a))*
                 ! ( abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(a) is the inverse of a
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(a)*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(a)*abs(x) + abs(b) is less than safe2.
              ! use stdlib_slacn2 to estimate the infinity-norm of the matrix
                 ! inv(a) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) )))
              do i = 1, n
                 if( work( i )>safe2 ) then
                    work( i ) = abs( work( n+i ) ) + nz*eps*work( i )
                 else
                    work( i ) = abs( work( n+i ) ) + nz*eps*work( i ) + safe1
                 end if
              end do
              kase = 0_ilp
              100 continue
              call stdlib_slacn2( n, work( 2_ilp*n+1 ), work( n+1 ), iwork, ferr( j ),kase, isave )
                        
              if( kase/=0_ilp ) then
                 if( kase==1_ilp ) then
                    ! multiply by diag(w)*inv(a**t).
                    call stdlib_spotrs( uplo, n, 1_ilp, af, ldaf, work( n+1 ), n, info )
                    do i = 1, n
                       work( n+i ) = work( i )*work( n+i )
                    end do
                 else if( kase==2_ilp ) then
                    ! multiply by inv(a)*diag(w).
                    do i = 1, n
                       work( n+i ) = work( i )*work( n+i )
                    end do
                    call stdlib_spotrs( uplo, n, 1_ilp, af, ldaf, work( n+1 ), n, info )
                 end if
                 go to 100
              end if
              ! normalize error.
              lstres = zero
              do i = 1, n
                 lstres = max( lstres, abs( x( i, j ) ) )
              end do
              if( lstres/=zero )ferr( j ) = ferr( j ) / lstres
           end do loop_140
           return
     end subroutine stdlib_sporfs

     pure module subroutine stdlib_dporfs( uplo, n, nrhs, a, lda, af, ldaf, b, ldb, x,ldx, ferr, berr, &
     !! DPORFS improves the computed solution to a system of linear
     !! equations when the coefficient matrix is symmetric positive definite,
     !! and provides error bounds and backward error estimates for the
     !! solution.
               work, iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldaf, ldb, ldx, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(in) :: a(lda,*), af(ldaf,*), b(ldb,*)
           real(dp), intent(out) :: berr(*), ferr(*), work(*)
           real(dp), intent(inout) :: x(ldx,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: count, i, j, k, kase, nz
           real(dp) :: eps, lstres, s, safe1, safe2, safmin, xk
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldaf<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DPORFS', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp .or. nrhs==0_ilp ) then
              do j = 1, nrhs
                 ferr( j ) = zero
                 berr( j ) = zero
              end do
              return
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = n + 1_ilp
           eps = stdlib_dlamch( 'EPSILON' )
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           safe1 = nz*safmin
           safe2 = safe1 / eps
           ! do for each right hand side
           loop_140: do j = 1, nrhs
              count = 1_ilp
              lstres = three
              20 continue
              ! loop until stopping criterion is satisfied.
              ! compute residual r = b - a * x
              call stdlib_dcopy( n, b( 1_ilp, j ), 1_ilp, work( n+1 ), 1_ilp )
              call stdlib_dsymv( uplo, n, -one, a, lda, x( 1_ilp, j ), 1_ilp, one,work( n+1 ), 1_ilp )
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(a)*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              do i = 1, n
                 work( i ) = abs( b( i, j ) )
              end do
              ! compute abs(a)*abs(x) + abs(b).
              if( upper ) then
                 do k = 1, n
                    s = zero
                    xk = abs( x( k, j ) )
                    do i = 1, k - 1
                       work( i ) = work( i ) + abs( a( i, k ) )*xk
                       s = s + abs( a( i, k ) )*abs( x( i, j ) )
                    end do
                    work( k ) = work( k ) + abs( a( k, k ) )*xk + s
                 end do
              else
                 do k = 1, n
                    s = zero
                    xk = abs( x( k, j ) )
                    work( k ) = work( k ) + abs( a( k, k ) )*xk
                    do i = k + 1, n
                       work( i ) = work( i ) + abs( a( i, k ) )*xk
                       s = s + abs( a( i, k ) )*abs( x( i, j ) )
                    end do
                    work( k ) = work( k ) + s
                 end do
              end if
              s = zero
              do i = 1, n
                 if( work( i )>safe2 ) then
                    s = max( s, abs( work( n+i ) ) / work( i ) )
                 else
                    s = max( s, ( abs( work( n+i ) )+safe1 ) /( work( i )+safe1 ) )
                 end if
              end do
              berr( j ) = s
              ! test stopping criterion. continue iterating if
                 ! 1) the residual berr(j) is larger than machine epsilon, and
                 ! 2) berr(j) decreased by at least a factor of 2 during the
                    ! last iteration, and
                 ! 3) at most itmax iterations tried.
              if( berr( j )>eps .and. two*berr( j )<=lstres .and.count<=itmax ) then
                 ! update solution and try again.
                 call stdlib_dpotrs( uplo, n, 1_ilp, af, ldaf, work( n+1 ), n, info )
                 call stdlib_daxpy( n, one, work( n+1 ), 1_ilp, x( 1_ilp, j ), 1_ilp )
                 lstres = berr( j )
                 count = count + 1_ilp
                 go to 20
              end if
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(a))*
                 ! ( abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(a) is the inverse of a
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(a)*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(a)*abs(x) + abs(b) is less than safe2.
              ! use stdlib_dlacn2 to estimate the infinity-norm of the matrix
                 ! inv(a) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) )))
              do i = 1, n
                 if( work( i )>safe2 ) then
                    work( i ) = abs( work( n+i ) ) + nz*eps*work( i )
                 else
                    work( i ) = abs( work( n+i ) ) + nz*eps*work( i ) + safe1
                 end if
              end do
              kase = 0_ilp
              100 continue
              call stdlib_dlacn2( n, work( 2_ilp*n+1 ), work( n+1 ), iwork, ferr( j ),kase, isave )
                        
              if( kase/=0_ilp ) then
                 if( kase==1_ilp ) then
                    ! multiply by diag(w)*inv(a**t).
                    call stdlib_dpotrs( uplo, n, 1_ilp, af, ldaf, work( n+1 ), n, info )
                    do i = 1, n
                       work( n+i ) = work( i )*work( n+i )
                    end do
                 else if( kase==2_ilp ) then
                    ! multiply by inv(a)*diag(w).
                    do i = 1, n
                       work( n+i ) = work( i )*work( n+i )
                    end do
                    call stdlib_dpotrs( uplo, n, 1_ilp, af, ldaf, work( n+1 ), n, info )
                 end if
                 go to 100
              end if
              ! normalize error.
              lstres = zero
              do i = 1, n
                 lstres = max( lstres, abs( x( i, j ) ) )
              end do
              if( lstres/=zero )ferr( j ) = ferr( j ) / lstres
           end do loop_140
           return
     end subroutine stdlib_dporfs


     pure module subroutine stdlib_cporfs( uplo, n, nrhs, a, lda, af, ldaf, b, ldb, x,ldx, ferr, berr, &
     !! CPORFS improves the computed solution to a system of linear
     !! equations when the coefficient matrix is Hermitian positive definite,
     !! and provides error bounds and backward error estimates for the
     !! solution.
               work, rwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldaf, ldb, ldx, n, nrhs
           ! Array Arguments 
           real(sp), intent(out) :: berr(*), ferr(*), rwork(*)
           complex(sp), intent(in) :: a(lda,*), af(ldaf,*), b(ldb,*)
           complex(sp), intent(out) :: work(*)
           complex(sp), intent(inout) :: x(ldx,*)
        ! ====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: count, i, j, k, kase, nz
           real(sp) :: eps, lstres, s, safe1, safe2, safmin, xk
           complex(sp) :: zdum
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldaf<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CPORFS', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp .or. nrhs==0_ilp ) then
              do j = 1, nrhs
                 ferr( j ) = zero
                 berr( j ) = zero
              end do
              return
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = n + 1_ilp
           eps = stdlib_slamch( 'EPSILON' )
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           safe1 = nz*safmin
           safe2 = safe1 / eps
           ! do for each right hand side
           loop_140: do j = 1, nrhs
              count = 1_ilp
              lstres = three
              20 continue
              ! loop until stopping criterion is satisfied.
              ! compute residual r = b - a * x
              call stdlib_ccopy( n, b( 1_ilp, j ), 1_ilp, work, 1_ilp )
              call stdlib_chemv( uplo, n, -cone, a, lda, x( 1_ilp, j ), 1_ilp, cone, work, 1_ilp )
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(a)*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              do i = 1, n
                 rwork( i ) = cabs1( b( i, j ) )
              end do
              ! compute abs(a)*abs(x) + abs(b).
              if( upper ) then
                 do k = 1, n
                    s = zero
                    xk = cabs1( x( k, j ) )
                    do i = 1, k - 1
                       rwork( i ) = rwork( i ) + cabs1( a( i, k ) )*xk
                       s = s + cabs1( a( i, k ) )*cabs1( x( i, j ) )
                    end do
                    rwork( k ) = rwork( k ) + abs( real( a( k, k ),KIND=sp) )*xk + s
                 end do
              else
                 do k = 1, n
                    s = zero
                    xk = cabs1( x( k, j ) )
                    rwork( k ) = rwork( k ) + abs( real( a( k, k ),KIND=sp) )*xk
                    do i = k + 1, n
                       rwork( i ) = rwork( i ) + cabs1( a( i, k ) )*xk
                       s = s + cabs1( a( i, k ) )*cabs1( x( i, j ) )
                    end do
                    rwork( k ) = rwork( k ) + s
                 end do
              end if
              s = zero
              do i = 1, n
                 if( rwork( i )>safe2 ) then
                    s = max( s, cabs1( work( i ) ) / rwork( i ) )
                 else
                    s = max( s, ( cabs1( work( i ) )+safe1 ) /( rwork( i )+safe1 ) )
                 end if
              end do
              berr( j ) = s
              ! test stopping criterion. continue iterating if
                 ! 1) the residual berr(j) is larger than machine epsilon, and
                 ! 2) berr(j) decreased by at least a factor of 2 during the
                    ! last iteration, and
                 ! 3) at most itmax iterations tried.
              if( berr( j )>eps .and. two*berr( j )<=lstres .and.count<=itmax ) then
                 ! update solution and try again.
                 call stdlib_cpotrs( uplo, n, 1_ilp, af, ldaf, work, n, info )
                 call stdlib_caxpy( n, cone, work, 1_ilp, x( 1_ilp, j ), 1_ilp )
                 lstres = berr( j )
                 count = count + 1_ilp
                 go to 20
              end if
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(a))*
                 ! ( abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(a) is the inverse of a
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(a)*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(a)*abs(x) + abs(b) is less than safe2.
              ! use stdlib_clacn2 to estimate the infinity-norm of the matrix
                 ! inv(a) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) )))
              do i = 1, n
                 if( rwork( i )>safe2 ) then
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i )
                 else
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i ) +safe1
                 end if
              end do
              kase = 0_ilp
              100 continue
              call stdlib_clacn2( n, work( n+1 ), work, ferr( j ), kase, isave )
              if( kase/=0_ilp ) then
                 if( kase==1_ilp ) then
                    ! multiply by diag(w)*inv(a**h).
                    call stdlib_cpotrs( uplo, n, 1_ilp, af, ldaf, work, n, info )
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                 else if( kase==2_ilp ) then
                    ! multiply by inv(a)*diag(w).
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                    call stdlib_cpotrs( uplo, n, 1_ilp, af, ldaf, work, n, info )
                 end if
                 go to 100
              end if
              ! normalize error.
              lstres = zero
              do i = 1, n
                 lstres = max( lstres, cabs1( x( i, j ) ) )
              end do
              if( lstres/=zero )ferr( j ) = ferr( j ) / lstres
           end do loop_140
           return
     end subroutine stdlib_cporfs

     pure module subroutine stdlib_zporfs( uplo, n, nrhs, a, lda, af, ldaf, b, ldb, x,ldx, ferr, berr, &
     !! ZPORFS improves the computed solution to a system of linear
     !! equations when the coefficient matrix is Hermitian positive definite,
     !! and provides error bounds and backward error estimates for the
     !! solution.
               work, rwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldaf, ldb, ldx, n, nrhs
           ! Array Arguments 
           real(dp), intent(out) :: berr(*), ferr(*), rwork(*)
           complex(dp), intent(in) :: a(lda,*), af(ldaf,*), b(ldb,*)
           complex(dp), intent(out) :: work(*)
           complex(dp), intent(inout) :: x(ldx,*)
        ! ====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: count, i, j, k, kase, nz
           real(dp) :: eps, lstres, s, safe1, safe2, safmin, xk
           complex(dp) :: zdum
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldaf<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZPORFS', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp .or. nrhs==0_ilp ) then
              do j = 1, nrhs
                 ferr( j ) = zero
                 berr( j ) = zero
              end do
              return
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = n + 1_ilp
           eps = stdlib_dlamch( 'EPSILON' )
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           safe1 = nz*safmin
           safe2 = safe1 / eps
           ! do for each right hand side
           loop_140: do j = 1, nrhs
              count = 1_ilp
              lstres = three
              20 continue
              ! loop until stopping criterion is satisfied.
              ! compute residual r = b - a * x
              call stdlib_zcopy( n, b( 1_ilp, j ), 1_ilp, work, 1_ilp )
              call stdlib_zhemv( uplo, n, -cone, a, lda, x( 1_ilp, j ), 1_ilp, cone, work, 1_ilp )
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(a)*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              do i = 1, n
                 rwork( i ) = cabs1( b( i, j ) )
              end do
              ! compute abs(a)*abs(x) + abs(b).
              if( upper ) then
                 do k = 1, n
                    s = zero
                    xk = cabs1( x( k, j ) )
                    do i = 1, k - 1
                       rwork( i ) = rwork( i ) + cabs1( a( i, k ) )*xk
                       s = s + cabs1( a( i, k ) )*cabs1( x( i, j ) )
                    end do
                    rwork( k ) = rwork( k ) + abs( real( a( k, k ),KIND=dp) )*xk + s
                 end do
              else
                 do k = 1, n
                    s = zero
                    xk = cabs1( x( k, j ) )
                    rwork( k ) = rwork( k ) + abs( real( a( k, k ),KIND=dp) )*xk
                    do i = k + 1, n
                       rwork( i ) = rwork( i ) + cabs1( a( i, k ) )*xk
                       s = s + cabs1( a( i, k ) )*cabs1( x( i, j ) )
                    end do
                    rwork( k ) = rwork( k ) + s
                 end do
              end if
              s = zero
              do i = 1, n
                 if( rwork( i )>safe2 ) then
                    s = max( s, cabs1( work( i ) ) / rwork( i ) )
                 else
                    s = max( s, ( cabs1( work( i ) )+safe1 ) /( rwork( i )+safe1 ) )
                 end if
              end do
              berr( j ) = s
              ! test stopping criterion. continue iterating if
                 ! 1) the residual berr(j) is larger than machine epsilon, and
                 ! 2) berr(j) decreased by at least a factor of 2 during the
                    ! last iteration, and
                 ! 3) at most itmax iterations tried.
              if( berr( j )>eps .and. two*berr( j )<=lstres .and.count<=itmax ) then
                 ! update solution and try again.
                 call stdlib_zpotrs( uplo, n, 1_ilp, af, ldaf, work, n, info )
                 call stdlib_zaxpy( n, cone, work, 1_ilp, x( 1_ilp, j ), 1_ilp )
                 lstres = berr( j )
                 count = count + 1_ilp
                 go to 20
              end if
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(a))*
                 ! ( abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(a) is the inverse of a
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(a)*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(a)*abs(x) + abs(b) is less than safe2.
              ! use stdlib_zlacn2 to estimate the infinity-norm of the matrix
                 ! inv(a) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) )))
              do i = 1, n
                 if( rwork( i )>safe2 ) then
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i )
                 else
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i ) +safe1
                 end if
              end do
              kase = 0_ilp
              100 continue
              call stdlib_zlacn2( n, work( n+1 ), work, ferr( j ), kase, isave )
              if( kase/=0_ilp ) then
                 if( kase==1_ilp ) then
                    ! multiply by diag(w)*inv(a**h).
                    call stdlib_zpotrs( uplo, n, 1_ilp, af, ldaf, work, n, info )
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                 else if( kase==2_ilp ) then
                    ! multiply by inv(a)*diag(w).
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                    call stdlib_zpotrs( uplo, n, 1_ilp, af, ldaf, work, n, info )
                 end if
                 go to 100
              end if
              ! normalize error.
              lstres = zero
              do i = 1, n
                 lstres = max( lstres, cabs1( x( i, j ) ) )
              end do
              if( lstres/=zero )ferr( j ) = ferr( j ) / lstres
           end do loop_140
           return
     end subroutine stdlib_zporfs




     pure module subroutine stdlib_spoequ( n, a, lda, s, scond, amax, info )
     !! SPOEQU computes row and column scalings intended to equilibrate a
     !! symmetric positive definite matrix A and reduce its condition number
     !! (with respect to the two-norm).  S contains the scale factors,
     !! S(i) = 1/sqrt(A(i,i)), chosen so that the scaled matrix B with
     !! elements B(i,j) = S(i)*A(i,j)*S(j) has ones on the diagonal.  This
     !! choice of S puts the condition number of B within a factor N of the
     !! smallest possible condition number over all possible diagonal
     !! scalings.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           real(sp), intent(out) :: amax, scond
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(out) :: s(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i
           real(sp) :: smin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -3_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SPOEQU', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              scond = one
              amax = zero
              return
           end if
           ! find the minimum and maximum diagonal elements.
           s( 1_ilp ) = a( 1_ilp, 1_ilp )
           smin = s( 1_ilp )
           amax = s( 1_ilp )
           do i = 2, n
              s( i ) = a( i, i )
              smin = min( smin, s( i ) )
              amax = max( amax, s( i ) )
           end do
           if( smin<=zero ) then
              ! find the first non-positive diagonal element and return.
              do i = 1, n
                 if( s( i )<=zero ) then
                    info = i
                    return
                 end if
              end do
           else
              ! set the scale factors to the reciprocals
              ! of the diagonal elements.
              do i = 1, n
                 s( i ) = one / sqrt( s( i ) )
              end do
              ! compute scond = min(s(i)) / max(s(i))
              scond = sqrt( smin ) / sqrt( amax )
           end if
           return
     end subroutine stdlib_spoequ

     pure module subroutine stdlib_dpoequ( n, a, lda, s, scond, amax, info )
     !! DPOEQU computes row and column scalings intended to equilibrate a
     !! symmetric positive definite matrix A and reduce its condition number
     !! (with respect to the two-norm).  S contains the scale factors,
     !! S(i) = 1/sqrt(A(i,i)), chosen so that the scaled matrix B with
     !! elements B(i,j) = S(i)*A(i,j)*S(j) has ones on the diagonal.  This
     !! choice of S puts the condition number of B within a factor N of the
     !! smallest possible condition number over all possible diagonal
     !! scalings.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           real(dp), intent(out) :: amax, scond
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(out) :: s(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i
           real(dp) :: smin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -3_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DPOEQU', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              scond = one
              amax = zero
              return
           end if
           ! find the minimum and maximum diagonal elements.
           s( 1_ilp ) = a( 1_ilp, 1_ilp )
           smin = s( 1_ilp )
           amax = s( 1_ilp )
           do i = 2, n
              s( i ) = a( i, i )
              smin = min( smin, s( i ) )
              amax = max( amax, s( i ) )
           end do
           if( smin<=zero ) then
              ! find the first non-positive diagonal element and return.
              do i = 1, n
                 if( s( i )<=zero ) then
                    info = i
                    return
                 end if
              end do
           else
              ! set the scale factors to the reciprocals
              ! of the diagonal elements.
              do i = 1, n
                 s( i ) = one / sqrt( s( i ) )
              end do
              ! compute scond = min(s(i)) / max(s(i))
              scond = sqrt( smin ) / sqrt( amax )
           end if
           return
     end subroutine stdlib_dpoequ


     pure module subroutine stdlib_cpoequ( n, a, lda, s, scond, amax, info )
     !! CPOEQU computes row and column scalings intended to equilibrate a
     !! Hermitian positive definite matrix A and reduce its condition number
     !! (with respect to the two-norm).  S contains the scale factors,
     !! S(i) = 1/sqrt(A(i,i)), chosen so that the scaled matrix B with
     !! elements B(i,j) = S(i)*A(i,j)*S(j) has ones on the diagonal.  This
     !! choice of S puts the condition number of B within a factor N of the
     !! smallest possible condition number over all possible diagonal
     !! scalings.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           real(sp), intent(out) :: amax, scond
           ! Array Arguments 
           real(sp), intent(out) :: s(*)
           complex(sp), intent(in) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i
           real(sp) :: smin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -3_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CPOEQU', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              scond = one
              amax = zero
              return
           end if
           ! find the minimum and maximum diagonal elements.
           s( 1_ilp ) = real( a( 1_ilp, 1_ilp ),KIND=sp)
           smin = s( 1_ilp )
           amax = s( 1_ilp )
           do i = 2, n
              s( i ) = real( a( i, i ),KIND=sp)
              smin = min( smin, s( i ) )
              amax = max( amax, s( i ) )
           end do
           if( smin<=zero ) then
              ! find the first non-positive diagonal element and return.
              do i = 1, n
                 if( s( i )<=zero ) then
                    info = i
                    return
                 end if
              end do
           else
              ! set the scale factors to the reciprocals
              ! of the diagonal elements.
              do i = 1, n
                 s( i ) = one / sqrt( s( i ) )
              end do
              ! compute scond = min(s(i)) / max(s(i))
              scond = sqrt( smin ) / sqrt( amax )
           end if
           return
     end subroutine stdlib_cpoequ

     pure module subroutine stdlib_zpoequ( n, a, lda, s, scond, amax, info )
     !! ZPOEQU computes row and column scalings intended to equilibrate a
     !! Hermitian positive definite matrix A and reduce its condition number
     !! (with respect to the two-norm).  S contains the scale factors,
     !! S(i) = 1/sqrt(A(i,i)), chosen so that the scaled matrix B with
     !! elements B(i,j) = S(i)*A(i,j)*S(j) has ones on the diagonal.  This
     !! choice of S puts the condition number of B within a factor N of the
     !! smallest possible condition number over all possible diagonal
     !! scalings.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           real(dp), intent(out) :: amax, scond
           ! Array Arguments 
           real(dp), intent(out) :: s(*)
           complex(dp), intent(in) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i
           real(dp) :: smin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -3_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZPOEQU', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              scond = one
              amax = zero
              return
           end if
           ! find the minimum and maximum diagonal elements.
           s( 1_ilp ) = real( a( 1_ilp, 1_ilp ),KIND=dp)
           smin = s( 1_ilp )
           amax = s( 1_ilp )
           do i = 2, n
              s( i ) = real( a( i, i ),KIND=dp)
              smin = min( smin, s( i ) )
              amax = max( amax, s( i ) )
           end do
           if( smin<=zero ) then
              ! find the first non-positive diagonal element and return.
              do i = 1, n
                 if( s( i )<=zero ) then
                    info = i
                    return
                 end if
              end do
           else
              ! set the scale factors to the reciprocals
              ! of the diagonal elements.
              do i = 1, n
                 s( i ) = one / sqrt( s( i ) )
              end do
              ! compute scond = min(s(i)) / max(s(i))
              scond = sqrt( smin ) / sqrt( amax )
           end if
           return
     end subroutine stdlib_zpoequ




     pure module subroutine stdlib_spoequb( n, a, lda, s, scond, amax, info )
     !! SPOEQUB computes row and column scalings intended to equilibrate a
     !! symmetric positive definite matrix A and reduce its condition number
     !! (with respect to the two-norm).  S contains the scale factors,
     !! S(i) = 1/sqrt(A(i,i)), chosen so that the scaled matrix B with
     !! elements B(i,j) = S(i)*A(i,j)*S(j) has ones on the diagonal.  This
     !! choice of S puts the condition number of B within a factor N of the
     !! smallest possible condition number over all possible diagonal
     !! scalings.
     !! This routine differs from SPOEQU by restricting the scaling factors
     !! to a power of the radix.  Barring over- and underflow, scaling by
     !! these factors introduces no additional rounding errors.  However, the
     !! scaled diagonal entries are no longer approximately 1 but lie
     !! between sqrt(radix) and 1/sqrt(radix).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           real(sp), intent(out) :: amax, scond
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(out) :: s(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i
           real(sp) :: smin, base, tmp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           ! positive definite only performs 1 pass of equilibration.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -3_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SPOEQUB', -info )
              return
           end if
           ! quick return if possible.
           if( n==0_ilp ) then
              scond = one
              amax = zero
              return
           end if
           base = stdlib_slamch( 'B' )
           tmp = -0.5_sp / log ( base )
           ! find the minimum and maximum diagonal elements.
           s( 1_ilp ) = a( 1_ilp, 1_ilp )
           smin = s( 1_ilp )
           amax = s( 1_ilp )
           do i = 2, n
              s( i ) = a( i, i )
              smin = min( smin, s( i ) )
              amax = max( amax, s( i ) )
           end do
           if( smin<=zero ) then
              ! find the first non-positive diagonal element and return.
              do i = 1, n
                 if( s( i )<=zero ) then
                    info = i
                    return
                 end if
              end do
           else
              ! set the scale factors to the reciprocals
              ! of the diagonal elements.
              do i = 1, n
                 s( i ) = base ** int( tmp * log( s( i ) ),KIND=ilp)
              end do
              ! compute scond = min(s(i)) / max(s(i)).
              scond = sqrt( smin ) / sqrt( amax )
           end if
           return
     end subroutine stdlib_spoequb

     pure module subroutine stdlib_dpoequb( n, a, lda, s, scond, amax, info )
     !! DPOEQUB computes row and column scalings intended to equilibrate a
     !! symmetric positive definite matrix A and reduce its condition number
     !! (with respect to the two-norm).  S contains the scale factors,
     !! S(i) = 1/sqrt(A(i,i)), chosen so that the scaled matrix B with
     !! elements B(i,j) = S(i)*A(i,j)*S(j) has ones on the diagonal.  This
     !! choice of S puts the condition number of B within a factor N of the
     !! smallest possible condition number over all possible diagonal
     !! scalings.
     !! This routine differs from DPOEQU by restricting the scaling factors
     !! to a power of the radix.  Barring over- and underflow, scaling by
     !! these factors introduces no additional rounding errors.  However, the
     !! scaled diagonal entries are no longer approximately 1 but lie
     !! between sqrt(radix) and 1/sqrt(radix).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           real(dp), intent(out) :: amax, scond
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(out) :: s(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i
           real(dp) :: smin, base, tmp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           ! positive definite only performs 1 pass of equilibration.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -3_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DPOEQUB', -info )
              return
           end if
           ! quick return if possible.
           if( n==0_ilp ) then
              scond = one
              amax = zero
              return
           end if
           base = stdlib_dlamch( 'B' )
           tmp = -0.5e+0_dp / log ( base )
           ! find the minimum and maximum diagonal elements.
           s( 1_ilp ) = a( 1_ilp, 1_ilp )
           smin = s( 1_ilp )
           amax = s( 1_ilp )
           do i = 2, n
              s( i ) = a( i, i )
              smin = min( smin, s( i ) )
              amax = max( amax, s( i ) )
           end do
           if( smin<=zero ) then
              ! find the first non-positive diagonal element and return.
              do i = 1, n
                 if( s( i )<=zero ) then
                    info = i
                    return
                 end if
              end do
           else
              ! set the scale factors to the reciprocals
              ! of the diagonal elements.
              do i = 1, n
                 s( i ) = base ** int( tmp * log( s( i ) ),KIND=ilp)
              end do
              ! compute scond = min(s(i)) / max(s(i)).
              scond = sqrt( smin ) / sqrt( amax )
           end if
           return
     end subroutine stdlib_dpoequb


     pure module subroutine stdlib_cpoequb( n, a, lda, s, scond, amax, info )
     !! CPOEQUB computes row and column scalings intended to equilibrate a
     !! Hermitian positive definite matrix A and reduce its condition number
     !! (with respect to the two-norm).  S contains the scale factors,
     !! S(i) = 1/sqrt(A(i,i)), chosen so that the scaled matrix B with
     !! elements B(i,j) = S(i)*A(i,j)*S(j) has ones on the diagonal.  This
     !! choice of S puts the condition number of B within a factor N of the
     !! smallest possible condition number over all possible diagonal
     !! scalings.
     !! This routine differs from CPOEQU by restricting the scaling factors
     !! to a power of the radix.  Barring over- and underflow, scaling by
     !! these factors introduces no additional rounding errors.  However, the
     !! scaled diagonal entries are no longer approximately 1 but lie
     !! between sqrt(radix) and 1/sqrt(radix).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           real(sp), intent(out) :: amax, scond
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*)
           real(sp), intent(out) :: s(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i
           real(sp) :: smin, base, tmp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           ! positive definite only performs 1 pass of equilibration.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -3_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CPOEQUB', -info )
              return
           end if
           ! quick return if possible.
           if( n==0_ilp ) then
              scond = one
              amax = zero
              return
           end if
           base = stdlib_slamch( 'B' )
           tmp = -0.5_sp / log ( base )
           ! find the minimum and maximum diagonal elements.
           s( 1_ilp ) = real( a( 1_ilp, 1_ilp ),KIND=sp)
           smin = s( 1_ilp )
           amax = s( 1_ilp )
           do i = 2, n
              s( i ) = real( a( i, i ),KIND=sp)
              smin = min( smin, s( i ) )
              amax = max( amax, s( i ) )
           end do
           if( smin<=zero ) then
              ! find the first non-positive diagonal element and return.
              do i = 1, n
                 if( s( i )<=zero ) then
                    info = i
                    return
                 end if
              end do
           else
              ! set the scale factors to the reciprocals
              ! of the diagonal elements.
              do i = 1, n
                 s( i ) = base ** int( tmp * log( s( i ) ),KIND=ilp)
              end do
              ! compute scond = min(s(i)) / max(s(i)).
              scond = sqrt( smin ) / sqrt( amax )
           end if
           return
     end subroutine stdlib_cpoequb

     pure module subroutine stdlib_zpoequb( n, a, lda, s, scond, amax, info )
     !! ZPOEQUB computes row and column scalings intended to equilibrate a
     !! Hermitian positive definite matrix A and reduce its condition number
     !! (with respect to the two-norm).  S contains the scale factors,
     !! S(i) = 1/sqrt(A(i,i)), chosen so that the scaled matrix B with
     !! elements B(i,j) = S(i)*A(i,j)*S(j) has ones on the diagonal.  This
     !! choice of S puts the condition number of B within a factor N of the
     !! smallest possible condition number over all possible diagonal
     !! scalings.
     !! This routine differs from ZPOEQU by restricting the scaling factors
     !! to a power of the radix.  Barring over- and underflow, scaling by
     !! these factors introduces no additional rounding errors.  However, the
     !! scaled diagonal entries are no longer approximately 1 but lie
     !! between sqrt(radix) and 1/sqrt(radix).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           real(dp), intent(out) :: amax, scond
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*)
           real(dp), intent(out) :: s(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i
           real(dp) :: smin, base, tmp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           ! positive definite only performs 1 pass of equilibration.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -3_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZPOEQUB', -info )
              return
           end if
           ! quick return if possible.
           if( n==0_ilp ) then
              scond = one
              amax = zero
              return
           end if
           base = stdlib_dlamch( 'B' )
           tmp = -0.5e+0_dp / log ( base )
           ! find the minimum and maximum diagonal elements.
           s( 1_ilp ) = real( a( 1_ilp, 1_ilp ),KIND=dp)
           smin = s( 1_ilp )
           amax = s( 1_ilp )
           do i = 2, n
              s( i ) = real( a( i, i ),KIND=dp)
              smin = min( smin, s( i ) )
              amax = max( amax, s( i ) )
           end do
           if( smin<=zero ) then
              ! find the first non-positive diagonal element and return.
              do i = 1, n
                 if( s( i )<=zero ) then
                    info = i
                    return
                 end if
              end do
           else
              ! set the scale factors to the reciprocals
              ! of the diagonal elements.
              do i = 1, n
                 s( i ) = base ** int( tmp * log( s( i ) ),KIND=ilp)
              end do
              ! compute scond = min(s(i)) / max(s(i)).
              scond = sqrt( smin ) / sqrt( amax )
           end if
           return
     end subroutine stdlib_zpoequb




     pure module subroutine stdlib_claqhe( uplo, n, a, lda, s, scond, amax, equed )
     !! CLAQHE equilibrates a Hermitian matrix A using the scaling factors
     !! in the vector S.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(out) :: equed
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, n
           real(sp), intent(in) :: amax, scond
           ! Array Arguments 
           real(sp), intent(in) :: s(*)
           complex(sp), intent(inout) :: a(lda,*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: thresh = 0.1e+0_sp
           
           ! Local Scalars 
           integer(ilp) :: i, j
           real(sp) :: cj, large, small
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( n<=0_ilp ) then
              equed = 'N'
              return
           end if
           ! initialize large and small.
           small = stdlib_slamch( 'SAFE MINIMUM' ) / stdlib_slamch( 'PRECISION' )
           large = one / small
           if( scond>=thresh .and. amax>=small .and. amax<=large ) then
              ! no equilibration
              equed = 'N'
           else
              ! replace a by diag(s) * a * diag(s).
              if( stdlib_lsame( uplo, 'U' ) ) then
                 ! upper triangle of a is stored.
                 do j = 1, n
                    cj = s( j )
                    do i = 1, j - 1
                       a( i, j ) = cj*s( i )*a( i, j )
                    end do
                    a( j, j ) = cj*cj*real( a( j, j ),KIND=sp)
                 end do
              else
                 ! lower triangle of a is stored.
                 do j = 1, n
                    cj = s( j )
                    a( j, j ) = cj*cj*real( a( j, j ),KIND=sp)
                    do i = j + 1, n
                       a( i, j ) = cj*s( i )*a( i, j )
                    end do
                 end do
              end if
              equed = 'Y'
           end if
           return
     end subroutine stdlib_claqhe

     pure module subroutine stdlib_zlaqhe( uplo, n, a, lda, s, scond, amax, equed )
     !! ZLAQHE equilibrates a Hermitian matrix A using the scaling factors
     !! in the vector S.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(out) :: equed
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, n
           real(dp), intent(in) :: amax, scond
           ! Array Arguments 
           real(dp), intent(in) :: s(*)
           complex(dp), intent(inout) :: a(lda,*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: thresh = 0.1e+0_dp
           
           ! Local Scalars 
           integer(ilp) :: i, j
           real(dp) :: cj, large, small
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( n<=0_ilp ) then
              equed = 'N'
              return
           end if
           ! initialize large and small.
           small = stdlib_dlamch( 'SAFE MINIMUM' ) / stdlib_dlamch( 'PRECISION' )
           large = one / small
           if( scond>=thresh .and. amax>=small .and. amax<=large ) then
              ! no equilibration
              equed = 'N'
           else
              ! replace a by diag(s) * a * diag(s).
              if( stdlib_lsame( uplo, 'U' ) ) then
                 ! upper triangle of a is stored.
                 do j = 1, n
                    cj = s( j )
                    do i = 1, j - 1
                       a( i, j ) = cj*s( i )*a( i, j )
                    end do
                    a( j, j ) = cj*cj*real( a( j, j ),KIND=dp)
                 end do
              else
                 ! lower triangle of a is stored.
                 do j = 1, n
                    cj = s( j )
                    a( j, j ) = cj*cj*real( a( j, j ),KIND=dp)
                    do i = j + 1, n
                       a( i, j ) = cj*s( i )*a( i, j )
                    end do
                 end do
              end if
              equed = 'Y'
           end if
           return
     end subroutine stdlib_zlaqhe




     real(sp) module function stdlib_sla_porcond( uplo, n, a, lda, af, ldaf, cmode, c,info, work, iwork )
     !! SLA_PORCOND Estimates the Skeel condition number of  op(A) * op2(C)
     !! where op2 is determined by CMODE as follows
     !! CMODE =  1    op2(C) = C
     !! CMODE =  0    op2(C) = I
     !! CMODE = -1    op2(C) = inv(C)
     !! The Skeel condition number  cond(A) = norminf( |inv(A)||A| )
     !! is computed by computing scaling factors R such that
     !! diag(R)*A*op2(C) is row equilibrated and computing the standard
     !! infinity-norm condition number.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: n, lda, ldaf, cmode
           integer(ilp), intent(out) :: info
           real(sp), intent(in) :: a(lda,*), af(ldaf,*), c(*)
           real(sp), intent(out) :: work(*)
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: kase, i, j
           real(sp) :: ainvnm, tmp
           logical(lk) :: up
           ! Array Arguments 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           stdlib_sla_porcond = zero
           info = 0_ilp
           if( n<0_ilp ) then
              info = -2_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SLA_PORCOND', -info )
              return
           end if
           if( n==0_ilp ) then
              stdlib_sla_porcond = one
              return
           end if
           up = .false.
           if ( stdlib_lsame( uplo, 'U' ) ) up = .true.
           ! compute the equilibration matrix r such that
           ! inv(r)*a*c has unit 1-norm.
           if ( up ) then
              do i = 1, n
                 tmp = zero
                 if ( cmode == 1_ilp ) then
                    do j = 1, i
                       tmp = tmp + abs( a( j, i ) * c( j ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + abs( a( i, j ) * c( j ) )
                    end do
                 else if ( cmode == 0_ilp ) then
                    do j = 1, i
                       tmp = tmp + abs( a( j, i ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + abs( a( i, j ) )
                    end do
                 else
                    do j = 1, i
                       tmp = tmp + abs( a( j ,i ) / c( j ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + abs( a( i, j ) / c( j ) )
                    end do
                 end if
                 work( 2_ilp*n+i ) = tmp
              end do
           else
              do i = 1, n
                 tmp = zero
                 if ( cmode == 1_ilp ) then
                    do j = 1, i
                       tmp = tmp + abs( a( i, j ) * c( j ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + abs( a( j, i ) * c( j ) )
                    end do
                 else if ( cmode == 0_ilp ) then
                    do j = 1, i
                       tmp = tmp + abs( a( i, j ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + abs( a( j, i ) )
                    end do
                 else
                    do j = 1, i
                       tmp = tmp + abs( a( i, j ) / c( j ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + abs( a( j, i ) / c( j ) )
                    end do
                 end if
                 work( 2_ilp*n+i ) = tmp
              end do
           endif
           ! estimate the norm of inv(op(a)).
           ainvnm = zero
           kase = 0_ilp
           10 continue
           call stdlib_slacn2( n, work( n+1 ), work, iwork, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              if( kase==2_ilp ) then
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * work( 2_ilp*n+i )
                 end do
                 if (up) then
                    call stdlib_spotrs( 'UPPER', n, 1_ilp, af, ldaf, work, n, info )
                 else
                    call stdlib_spotrs( 'LOWER', n, 1_ilp, af, ldaf, work, n, info )
                 endif
                 ! multiply by inv(c).
                 if ( cmode == 1_ilp ) then
                    do i = 1, n
                       work( i ) = work( i ) / c( i )
                    end do
                 else if ( cmode == -1_ilp ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
              else
                 ! multiply by inv(c**t).
                 if ( cmode == 1_ilp ) then
                    do i = 1, n
                       work( i ) = work( i ) / c( i )
                    end do
                 else if ( cmode == -1_ilp ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
                 if ( up ) then
                    call stdlib_spotrs( 'UPPER', n, 1_ilp, af, ldaf, work, n, info )
                 else
                    call stdlib_spotrs( 'LOWER', n, 1_ilp, af, ldaf, work, n, info )
                 endif
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * work( 2_ilp*n+i )
                 end do
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm /= 0.0_sp )stdlib_sla_porcond = ( one / ainvnm )
           return
     end function stdlib_sla_porcond

     real(dp) module function stdlib_dla_porcond( uplo, n, a, lda, af, ldaf,cmode, c, info, work,iwork )
     !! DLA_PORCOND Estimates the Skeel condition number of  op(A) * op2(C)
     !! where op2 is determined by CMODE as follows
     !! CMODE =  1    op2(C) = C
     !! CMODE =  0    op2(C) = I
     !! CMODE = -1    op2(C) = inv(C)
     !! The Skeel condition number  cond(A) = norminf( |inv(A)||A| )
     !! is computed by computing scaling factors R such that
     !! diag(R)*A*op2(C) is row equilibrated and computing the standard
     !! infinity-norm condition number.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: n, lda, ldaf, cmode
           integer(ilp), intent(out) :: info
           real(dp), intent(in) :: a(lda,*), af(ldaf,*), c(*)
           real(dp), intent(out) :: work(*)
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: kase, i, j
           real(dp) :: ainvnm, tmp
           logical(lk) :: up
           ! Array Arguments 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           stdlib_dla_porcond = zero
           info = 0_ilp
           if( n<0_ilp ) then
              info = -2_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DLA_PORCOND', -info )
              return
           end if
           if( n==0_ilp ) then
              stdlib_dla_porcond = one
              return
           end if
           up = .false.
           if ( stdlib_lsame( uplo, 'U' ) ) up = .true.
           ! compute the equilibration matrix r such that
           ! inv(r)*a*c has unit 1-norm.
           if ( up ) then
              do i = 1, n
                 tmp = zero
                 if ( cmode == 1_ilp ) then
                    do j = 1, i
                       tmp = tmp + abs( a( j, i ) * c( j ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + abs( a( i, j ) * c( j ) )
                    end do
                 else if ( cmode == 0_ilp ) then
                    do j = 1, i
                       tmp = tmp + abs( a( j, i ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + abs( a( i, j ) )
                    end do
                 else
                    do j = 1, i
                       tmp = tmp + abs( a( j ,i ) / c( j ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + abs( a( i, j ) / c( j ) )
                    end do
                 end if
                 work( 2_ilp*n+i ) = tmp
              end do
           else
              do i = 1, n
                 tmp = zero
                 if ( cmode == 1_ilp ) then
                    do j = 1, i
                       tmp = tmp + abs( a( i, j ) * c( j ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + abs( a( j, i ) * c( j ) )
                    end do
                 else if ( cmode == 0_ilp ) then
                    do j = 1, i
                       tmp = tmp + abs( a( i, j ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + abs( a( j, i ) )
                    end do
                 else
                    do j = 1, i
                       tmp = tmp + abs( a( i, j ) / c( j ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + abs( a( j, i ) / c( j ) )
                    end do
                 end if
                 work( 2_ilp*n+i ) = tmp
              end do
           endif
           ! estimate the norm of inv(op(a)).
           ainvnm = zero
           kase = 0_ilp
           10 continue
           call stdlib_dlacn2( n, work( n+1 ), work, iwork, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              if( kase==2_ilp ) then
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * work( 2_ilp*n+i )
                 end do
                 if (up) then
                    call stdlib_dpotrs( 'UPPER', n, 1_ilp, af, ldaf, work, n, info )
                 else
                    call stdlib_dpotrs( 'LOWER', n, 1_ilp, af, ldaf, work, n, info )
                 endif
                 ! multiply by inv(c).
                 if ( cmode == 1_ilp ) then
                    do i = 1, n
                       work( i ) = work( i ) / c( i )
                    end do
                 else if ( cmode == -1_ilp ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
              else
                 ! multiply by inv(c**t).
                 if ( cmode == 1_ilp ) then
                    do i = 1, n
                       work( i ) = work( i ) / c( i )
                    end do
                 else if ( cmode == -1_ilp ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
                 if ( up ) then
                    call stdlib_dpotrs( 'UPPER', n, 1_ilp, af, ldaf, work, n, info )
                 else
                    call stdlib_dpotrs( 'LOWER', n, 1_ilp, af, ldaf, work, n, info )
                 endif
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * work( 2_ilp*n+i )
                 end do
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm /= zero )stdlib_dla_porcond = ( one / ainvnm )
           return
     end function stdlib_dla_porcond




     real(sp) module function stdlib_sla_porpvgrw( uplo, ncols, a, lda, af, ldaf, work )
     !! SLA_PORPVGRW computes the reciprocal pivot growth factor
     !! norm(A)/norm(U). The "max absolute element" norm is used. If this is
     !! much less than 1, the stability of the LU factorization of the
     !! (equilibrated) matrix A could be poor. This also means that the
     !! solution X, estimated condition numbers, and error bounds could be
     !! unreliable.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: ncols, lda, ldaf
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*), af(ldaf,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, j
           real(sp) :: amax, umax, rpvgrw
           logical(lk) :: upper
           ! Intrinsic Functions 
           ! Executable Statements 
           upper = stdlib_lsame( 'UPPER', uplo )
           ! stdlib_spotrf will have factored only the ncolsxncols leading minor, so
           ! we restrict the growth search to that minor and use only the first
           ! 2*ncols workspace entries.
           rpvgrw = one
           do i = 1, 2*ncols
              work( i ) = zero
           end do
           ! find the max magnitude entry of each column.
           if ( upper ) then
              do j = 1, ncols
                 do i = 1, j
                    work( ncols+j ) =max( abs( a( i, j ) ), work( ncols+j ) )
                 end do
              end do
           else
              do j = 1, ncols
                 do i = j, ncols
                    work( ncols+j ) =max( abs( a( i, j ) ), work( ncols+j ) )
                 end do
              end do
           end if
           ! now find the max magnitude entry of each column of the factor in
           ! af.  no pivoting, so no permutations.
           if ( stdlib_lsame( 'UPPER', uplo ) ) then
              do j = 1, ncols
                 do i = 1, j
                    work( j ) = max( abs( af( i, j ) ), work( j ) )
                 end do
              end do
           else
              do j = 1, ncols
                 do i = j, ncols
                    work( j ) = max( abs( af( i, j ) ), work( j ) )
                 end do
              end do
           end if
           ! compute the *inverse* of the max element growth factor.  dividing
           ! by zero would imply the largest entry of the factor's column is
           ! zero.  than can happen when either the column of a is zero or
           ! massive pivots made the factor underflow to zero.  neither counts
           ! as growth in itself, so simply ignore terms with zero
           ! denominators.
           if ( stdlib_lsame( 'UPPER', uplo ) ) then
              do i = 1, ncols
                 umax = work( i )
                 amax = work( ncols+i )
                 if ( umax /= 0.0_sp ) then
                    rpvgrw = min( amax / umax, rpvgrw )
                 end if
              end do
           else
              do i = 1, ncols
                 umax = work( i )
                 amax = work( ncols+i )
                 if ( umax /= 0.0_sp ) then
                    rpvgrw = min( amax / umax, rpvgrw )
                 end if
              end do
           end if
           stdlib_sla_porpvgrw = rpvgrw
     end function stdlib_sla_porpvgrw

     real(dp) module function stdlib_dla_porpvgrw( uplo, ncols, a, lda, af,ldaf, work )
     !! DLA_PORPVGRW computes the reciprocal pivot growth factor
     !! norm(A)/norm(U). The "max absolute element" norm is used. If this is
     !! much less than 1, the stability of the LU factorization of the
     !! (equilibrated) matrix A could be poor. This also means that the
     !! solution X, estimated condition numbers, and error bounds could be
     !! unreliable.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: ncols, lda, ldaf
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*), af(ldaf,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, j
           real(dp) :: amax, umax, rpvgrw
           logical(lk) :: upper
           ! Intrinsic Functions 
           ! Executable Statements 
           upper = stdlib_lsame( 'UPPER', uplo )
           ! stdlib_dpotrf will have factored only the ncolsxncols leading minor, so
           ! we restrict the growth search to that minor and use only the first
           ! 2*ncols workspace entries.
           rpvgrw = one
           do i = 1, 2*ncols
              work( i ) = zero
           end do
           ! find the max magnitude entry of each column.
           if ( upper ) then
              do j = 1, ncols
                 do i = 1, j
                    work( ncols+j ) =max( abs( a( i, j ) ), work( ncols+j ) )
                 end do
              end do
           else
              do j = 1, ncols
                 do i = j, ncols
                    work( ncols+j ) =max( abs( a( i, j ) ), work( ncols+j ) )
                 end do
              end do
           end if
           ! now find the max magnitude entry of each column of the factor in
           ! af.  no pivoting, so no permutations.
           if ( stdlib_lsame( 'UPPER', uplo ) ) then
              do j = 1, ncols
                 do i = 1, j
                    work( j ) = max( abs( af( i, j ) ), work( j ) )
                 end do
              end do
           else
              do j = 1, ncols
                 do i = j, ncols
                    work( j ) = max( abs( af( i, j ) ), work( j ) )
                 end do
              end do
           end if
           ! compute the *inverse* of the max element growth factor.  dividing
           ! by zero would imply the largest entry of the factor's column is
           ! zero.  than can happen when either the column of a is zero or
           ! massive pivots made the factor underflow to zero.  neither counts
           ! as growth in itself, so simply ignore terms with zero
           ! denominators.
           if ( stdlib_lsame( 'UPPER', uplo ) ) then
              do i = 1, ncols
                 umax = work( i )
                 amax = work( ncols+i )
                 if ( umax /= zero ) then
                    rpvgrw = min( amax / umax, rpvgrw )
                 end if
              end do
           else
              do i = 1, ncols
                 umax = work( i )
                 amax = work( ncols+i )
                 if ( umax /= zero ) then
                    rpvgrw = min( amax / umax, rpvgrw )
                 end if
              end do
           end if
           stdlib_dla_porpvgrw = rpvgrw
     end function stdlib_dla_porpvgrw


     real(sp) module function stdlib_cla_porpvgrw( uplo, ncols, a, lda, af, ldaf, work )
     !! CLA_PORPVGRW computes the reciprocal pivot growth factor
     !! norm(A)/norm(U). The "max absolute element" norm is used. If this is
     !! much less than 1, the stability of the LU factorization of the
     !! (equilibrated) matrix A could be poor. This also means that the
     !! solution X, estimated condition numbers, and error bounds could be
     !! unreliable.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: ncols, lda, ldaf
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*), af(ldaf,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, j
           real(sp) :: amax, umax, rpvgrw
           logical(lk) :: upper
           complex(sp) :: zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           upper = stdlib_lsame( 'UPPER', uplo )
           ! stdlib_spotrf will have factored only the ncolsxncols leading minor, so
           ! we restrict the growth search to that minor and use only the first
           ! 2*ncols workspace entries.
           rpvgrw = one
           do i = 1, 2*ncols
              work( i ) = zero
           end do
           ! find the max magnitude entry of each column.
           if ( upper ) then
              do j = 1, ncols
                 do i = 1, j
                    work( ncols+j ) =max( cabs1( a( i, j ) ), work( ncols+j ) )
                 end do
              end do
           else
              do j = 1, ncols
                 do i = j, ncols
                    work( ncols+j ) =max( cabs1( a( i, j ) ), work( ncols+j ) )
                 end do
              end do
           end if
           ! now find the max magnitude entry of each column of the factor in
           ! af.  no pivoting, so no permutations.
           if ( stdlib_lsame( 'UPPER', uplo ) ) then
              do j = 1, ncols
                 do i = 1, j
                    work( j ) = max( cabs1( af( i, j ) ), work( j ) )
                 end do
              end do
           else
              do j = 1, ncols
                 do i = j, ncols
                    work( j ) = max( cabs1( af( i, j ) ), work( j ) )
                 end do
              end do
           end if
           ! compute the *inverse* of the max element growth factor.  dividing
           ! by zero would imply the largest entry of the factor's column is
           ! zero.  than can happen when either the column of a is zero or
           ! massive pivots made the factor underflow to zero.  neither counts
           ! as growth in itself, so simply ignore terms with zero
           ! denominators.
           if ( stdlib_lsame( 'UPPER', uplo ) ) then
              do i = 1, ncols
                 umax = work( i )
                 amax = work( ncols+i )
                 if ( umax /= 0.0_sp ) then
                    rpvgrw = min( amax / umax, rpvgrw )
                 end if
              end do
           else
              do i = 1, ncols
                 umax = work( i )
                 amax = work( ncols+i )
                 if ( umax /= 0.0_sp ) then
                    rpvgrw = min( amax / umax, rpvgrw )
                 end if
              end do
           end if
           stdlib_cla_porpvgrw = rpvgrw
     end function stdlib_cla_porpvgrw

     real(dp) module function stdlib_zla_porpvgrw( uplo, ncols, a, lda, af,ldaf, work )
     !! ZLA_PORPVGRW computes the reciprocal pivot growth factor
     !! norm(A)/norm(U). The "max absolute element" norm is used. If this is
     !! much less than 1, the stability of the LU factorization of the
     !! (equilibrated) matrix A could be poor. This also means that the
     !! solution X, estimated condition numbers, and error bounds could be
     !! unreliable.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: ncols, lda, ldaf
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*), af(ldaf,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, j
           real(dp) :: amax, umax, rpvgrw
           logical(lk) :: upper
           complex(dp) :: zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           upper = stdlib_lsame( 'UPPER', uplo )
           ! stdlib_dpotrf will have factored only the ncolsxncols leading minor, so
           ! we restrict the growth search to that minor and use only the first
           ! 2*ncols workspace entries.
           rpvgrw = one
           do i = 1, 2*ncols
              work( i ) = zero
           end do
           ! find the max magnitude entry of each column.
           if ( upper ) then
              do j = 1, ncols
                 do i = 1, j
                    work( ncols+j ) =max( cabs1( a( i, j ) ), work( ncols+j ) )
                 end do
              end do
           else
              do j = 1, ncols
                 do i = j, ncols
                    work( ncols+j ) =max( cabs1( a( i, j ) ), work( ncols+j ) )
                 end do
              end do
           end if
           ! now find the max magnitude entry of each column of the factor in
           ! af.  no pivoting, so no permutations.
           if ( stdlib_lsame( 'UPPER', uplo ) ) then
              do j = 1, ncols
                 do i = 1, j
                    work( j ) = max( cabs1( af( i, j ) ), work( j ) )
                 end do
              end do
           else
              do j = 1, ncols
                 do i = j, ncols
                    work( j ) = max( cabs1( af( i, j ) ), work( j ) )
                 end do
              end do
           end if
           ! compute the *inverse* of the max element growth factor.  dividing
           ! by zero would imply the largest entry of the factor's column is
           ! zero.  than can happen when either the column of a is zero or
           ! massive pivots made the factor underflow to zero.  neither counts
           ! as growth in itself, so simply ignore terms with zero
           ! denominators.
           if ( stdlib_lsame( 'UPPER', uplo ) ) then
              do i = 1, ncols
                 umax = work( i )
                 amax = work( ncols+i )
                 if ( umax /= zero ) then
                    rpvgrw = min( amax / umax, rpvgrw )
                 end if
              end do
           else
              do i = 1, ncols
                 umax = work( i )
                 amax = work( ncols+i )
                 if ( umax /= zero ) then
                    rpvgrw = min( amax / umax, rpvgrw )
                 end if
              end do
           end if
           stdlib_zla_porpvgrw = rpvgrw
     end function stdlib_zla_porpvgrw




     pure module subroutine stdlib_sppcon( uplo, n, ap, anorm, rcond, work, iwork, info )
     !! SPPCON estimates the reciprocal of the condition number (in the
     !! 1-norm) of a real symmetric positive definite packed matrix using
     !! the Cholesky factorization A = U**T*U or A = L*L**T computed by
     !! SPPTRF.
     !! An estimate is obtained for norm(inv(A)), and the reciprocal of the
     !! condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(sp), intent(in) :: anorm
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(in) :: ap(*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           character :: normin
           integer(ilp) :: ix, kase
           real(sp) :: ainvnm, scale, scalel, scaleu, smlnum
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( anorm<zero ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SPPCON', -info )
              return
           end if
           ! quick return if possible
           rcond = zero
           if( n==0_ilp ) then
              rcond = one
              return
           else if( anorm==zero ) then
              return
           end if
           smlnum = stdlib_slamch( 'SAFE MINIMUM' )
           ! estimate the 1-norm of the inverse.
           kase = 0_ilp
           normin = 'N'
           10 continue
           call stdlib_slacn2( n, work( n+1 ), work, iwork, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              if( upper ) then
                 ! multiply by inv(u**t).
                 call stdlib_slatps( 'UPPER', 'TRANSPOSE', 'NON-UNIT', normin, n,ap, work, scalel,&
                            work( 2_ilp*n+1 ), info )
                 normin = 'Y'
                 ! multiply by inv(u).
                 call stdlib_slatps( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', normin, n,ap, work, &
                           scaleu, work( 2_ilp*n+1 ), info )
              else
                 ! multiply by inv(l).
                 call stdlib_slatps( 'LOWER', 'NO TRANSPOSE', 'NON-UNIT', normin, n,ap, work, &
                           scalel, work( 2_ilp*n+1 ), info )
                 normin = 'Y'
                 ! multiply by inv(l**t).
                 call stdlib_slatps( 'LOWER', 'TRANSPOSE', 'NON-UNIT', normin, n,ap, work, scaleu,&
                            work( 2_ilp*n+1 ), info )
              end if
              ! multiply by 1/scale if doing so will not cause overflow.
              scale = scalel*scaleu
              if( scale/=one ) then
                 ix = stdlib_isamax( n, work, 1_ilp )
                 if( scale<abs( work( ix ) )*smlnum .or. scale==zero )go to 20
                 call stdlib_srscl( n, scale, work, 1_ilp )
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm/=zero )rcond = ( one / ainvnm ) / anorm
           20 continue
           return
     end subroutine stdlib_sppcon

     pure module subroutine stdlib_dppcon( uplo, n, ap, anorm, rcond, work, iwork, info )
     !! DPPCON estimates the reciprocal of the condition number (in the
     !! 1-norm) of a real symmetric positive definite packed matrix using
     !! the Cholesky factorization A = U**T*U or A = L*L**T computed by
     !! DPPTRF.
     !! An estimate is obtained for norm(inv(A)), and the reciprocal of the
     !! condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(dp), intent(in) :: anorm
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(in) :: ap(*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           character :: normin
           integer(ilp) :: ix, kase
           real(dp) :: ainvnm, scale, scalel, scaleu, smlnum
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( anorm<zero ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DPPCON', -info )
              return
           end if
           ! quick return if possible
           rcond = zero
           if( n==0_ilp ) then
              rcond = one
              return
           else if( anorm==zero ) then
              return
           end if
           smlnum = stdlib_dlamch( 'SAFE MINIMUM' )
           ! estimate the 1-norm of the inverse.
           kase = 0_ilp
           normin = 'N'
           10 continue
           call stdlib_dlacn2( n, work( n+1 ), work, iwork, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              if( upper ) then
                 ! multiply by inv(u**t).
                 call stdlib_dlatps( 'UPPER', 'TRANSPOSE', 'NON-UNIT', normin, n,ap, work, scalel,&
                            work( 2_ilp*n+1 ), info )
                 normin = 'Y'
                 ! multiply by inv(u).
                 call stdlib_dlatps( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', normin, n,ap, work, &
                           scaleu, work( 2_ilp*n+1 ), info )
              else
                 ! multiply by inv(l).
                 call stdlib_dlatps( 'LOWER', 'NO TRANSPOSE', 'NON-UNIT', normin, n,ap, work, &
                           scalel, work( 2_ilp*n+1 ), info )
                 normin = 'Y'
                 ! multiply by inv(l**t).
                 call stdlib_dlatps( 'LOWER', 'TRANSPOSE', 'NON-UNIT', normin, n,ap, work, scaleu,&
                            work( 2_ilp*n+1 ), info )
              end if
              ! multiply by 1/scale if doing so will not cause overflow.
              scale = scalel*scaleu
              if( scale/=one ) then
                 ix = stdlib_idamax( n, work, 1_ilp )
                 if( scale<abs( work( ix ) )*smlnum .or. scale==zero )go to 20
                 call stdlib_drscl( n, scale, work, 1_ilp )
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm/=zero )rcond = ( one / ainvnm ) / anorm
           20 continue
           return
     end subroutine stdlib_dppcon


     pure module subroutine stdlib_cppcon( uplo, n, ap, anorm, rcond, work, rwork, info )
     !! CPPCON estimates the reciprocal of the condition number (in the
     !! 1-norm) of a complex Hermitian positive definite packed matrix using
     !! the Cholesky factorization A = U**H*U or A = L*L**H computed by
     !! CPPTRF.
     !! An estimate is obtained for norm(inv(A)), and the reciprocal of the
     !! condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(sp), intent(in) :: anorm
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(in) :: ap(*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           character :: normin
           integer(ilp) :: ix, kase
           real(sp) :: ainvnm, scale, scalel, scaleu, smlnum
           complex(sp) :: zdum
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( anorm<zero ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CPPCON', -info )
              return
           end if
           ! quick return if possible
           rcond = zero
           if( n==0_ilp ) then
              rcond = one
              return
           else if( anorm==zero ) then
              return
           end if
           smlnum = stdlib_slamch( 'SAFE MINIMUM' )
           ! estimate the 1-norm of the inverse.
           kase = 0_ilp
           normin = 'N'
           10 continue
           call stdlib_clacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              if( upper ) then
                 ! multiply by inv(u**h).
                 call stdlib_clatps( 'UPPER', 'CONJUGATE TRANSPOSE', 'NON-UNIT',normin, n, ap, &
                           work, scalel, rwork, info )
                 normin = 'Y'
                 ! multiply by inv(u).
                 call stdlib_clatps( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', normin, n,ap, work, &
                           scaleu, rwork, info )
              else
                 ! multiply by inv(l).
                 call stdlib_clatps( 'LOWER', 'NO TRANSPOSE', 'NON-UNIT', normin, n,ap, work, &
                           scalel, rwork, info )
                 normin = 'Y'
                 ! multiply by inv(l**h).
                 call stdlib_clatps( 'LOWER', 'CONJUGATE TRANSPOSE', 'NON-UNIT',normin, n, ap, &
                           work, scaleu, rwork, info )
              end if
              ! multiply by 1/scale if doing so will not cause overflow.
              scale = scalel*scaleu
              if( scale/=one ) then
                 ix = stdlib_icamax( n, work, 1_ilp )
                 if( scale<cabs1( work( ix ) )*smlnum .or. scale==zero )go to 20
                 call stdlib_csrscl( n, scale, work, 1_ilp )
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm/=zero )rcond = ( one / ainvnm ) / anorm
           20 continue
           return
     end subroutine stdlib_cppcon

     pure module subroutine stdlib_zppcon( uplo, n, ap, anorm, rcond, work, rwork, info )
     !! ZPPCON estimates the reciprocal of the condition number (in the
     !! 1-norm) of a complex Hermitian positive definite packed matrix using
     !! the Cholesky factorization A = U**H*U or A = L*L**H computed by
     !! ZPPTRF.
     !! An estimate is obtained for norm(inv(A)), and the reciprocal of the
     !! condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(dp), intent(in) :: anorm
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(in) :: ap(*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           character :: normin
           integer(ilp) :: ix, kase
           real(dp) :: ainvnm, scale, scalel, scaleu, smlnum
           complex(dp) :: zdum
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( anorm<zero ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZPPCON', -info )
              return
           end if
           ! quick return if possible
           rcond = zero
           if( n==0_ilp ) then
              rcond = one
              return
           else if( anorm==zero ) then
              return
           end if
           smlnum = stdlib_dlamch( 'SAFE MINIMUM' )
           ! estimate the 1-norm of the inverse.
           kase = 0_ilp
           normin = 'N'
           10 continue
           call stdlib_zlacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              if( upper ) then
                 ! multiply by inv(u**h).
                 call stdlib_zlatps( 'UPPER', 'CONJUGATE TRANSPOSE', 'NON-UNIT',normin, n, ap, &
                           work, scalel, rwork, info )
                 normin = 'Y'
                 ! multiply by inv(u).
                 call stdlib_zlatps( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', normin, n,ap, work, &
                           scaleu, rwork, info )
              else
                 ! multiply by inv(l).
                 call stdlib_zlatps( 'LOWER', 'NO TRANSPOSE', 'NON-UNIT', normin, n,ap, work, &
                           scalel, rwork, info )
                 normin = 'Y'
                 ! multiply by inv(l**h).
                 call stdlib_zlatps( 'LOWER', 'CONJUGATE TRANSPOSE', 'NON-UNIT',normin, n, ap, &
                           work, scaleu, rwork, info )
              end if
              ! multiply by 1/scale if doing so will not cause overflow.
              scale = scalel*scaleu
              if( scale/=one ) then
                 ix = stdlib_izamax( n, work, 1_ilp )
                 if( scale<cabs1( work( ix ) )*smlnum .or. scale==zero )go to 20
                 call stdlib_zdrscl( n, scale, work, 1_ilp )
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm/=zero )rcond = ( one / ainvnm ) / anorm
           20 continue
           return
     end subroutine stdlib_zppcon




     pure module subroutine stdlib_spptrf( uplo, n, ap, info )
     !! SPPTRF computes the Cholesky factorization of a real symmetric
     !! positive definite matrix A stored in packed format.
     !! The factorization has the form
     !! A = U**T * U,  if UPLO = 'U', or
     !! A = L  * L**T,  if UPLO = 'L',
     !! where U is an upper triangular matrix and L is lower triangular.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(sp), intent(inout) :: ap(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, jc, jj
           real(sp) :: ajj
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SPPTRF', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( upper ) then
              ! compute the cholesky factorization a = u**t*u.
              jj = 0_ilp
              do j = 1, n
                 jc = jj + 1_ilp
                 jj = jj + j
                 ! compute elements 1:j-1 of column j.
                 if( j>1_ilp )call stdlib_stpsv( 'UPPER', 'TRANSPOSE', 'NON-UNIT', j-1, ap,ap( jc ), &
                           1_ilp )
                 ! compute u(j,j) and test for non-positive-definiteness.
                 ajj = ap( jj ) - stdlib_sdot( j-1, ap( jc ), 1_ilp, ap( jc ), 1_ilp )
                 if( ajj<=zero ) then
                    ap( jj ) = ajj
                    go to 30
                 end if
                 ap( jj ) = sqrt( ajj )
              end do
           else
              ! compute the cholesky factorization a = l*l**t.
              jj = 1_ilp
              do j = 1, n
                 ! compute l(j,j) and test for non-positive-definiteness.
                 ajj = ap( jj )
                 if( ajj<=zero ) then
                    ap( jj ) = ajj
                    go to 30
                 end if
                 ajj = sqrt( ajj )
                 ap( jj ) = ajj
                 ! compute elements j+1:n of column j and update the trailing
                 ! submatrix.
                 if( j<n ) then
                    call stdlib_sscal( n-j, one / ajj, ap( jj+1 ), 1_ilp )
                    call stdlib_sspr( 'LOWER', n-j, -one, ap( jj+1 ), 1_ilp,ap( jj+n-j+1 ) )
                    jj = jj + n - j + 1_ilp
                 end if
              end do
           end if
           go to 40
           30 continue
           info = j
           40 continue
           return
     end subroutine stdlib_spptrf

     pure module subroutine stdlib_dpptrf( uplo, n, ap, info )
     !! DPPTRF computes the Cholesky factorization of a real symmetric
     !! positive definite matrix A stored in packed format.
     !! The factorization has the form
     !! A = U**T * U,  if UPLO = 'U', or
     !! A = L  * L**T,  if UPLO = 'L',
     !! where U is an upper triangular matrix and L is lower triangular.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(dp), intent(inout) :: ap(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, jc, jj
           real(dp) :: ajj
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DPPTRF', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( upper ) then
              ! compute the cholesky factorization a = u**t*u.
              jj = 0_ilp
              do j = 1, n
                 jc = jj + 1_ilp
                 jj = jj + j
                 ! compute elements 1:j-1 of column j.
                 if( j>1_ilp )call stdlib_dtpsv( 'UPPER', 'TRANSPOSE', 'NON-UNIT', j-1, ap,ap( jc ), &
                           1_ilp )
                 ! compute u(j,j) and test for non-positive-definiteness.
                 ajj = ap( jj ) - stdlib_ddot( j-1, ap( jc ), 1_ilp, ap( jc ), 1_ilp )
                 if( ajj<=zero ) then
                    ap( jj ) = ajj
                    go to 30
                 end if
                 ap( jj ) = sqrt( ajj )
              end do
           else
              ! compute the cholesky factorization a = l*l**t.
              jj = 1_ilp
              do j = 1, n
                 ! compute l(j,j) and test for non-positive-definiteness.
                 ajj = ap( jj )
                 if( ajj<=zero ) then
                    ap( jj ) = ajj
                    go to 30
                 end if
                 ajj = sqrt( ajj )
                 ap( jj ) = ajj
                 ! compute elements j+1:n of column j and update the trailing
                 ! submatrix.
                 if( j<n ) then
                    call stdlib_dscal( n-j, one / ajj, ap( jj+1 ), 1_ilp )
                    call stdlib_dspr( 'LOWER', n-j, -one, ap( jj+1 ), 1_ilp,ap( jj+n-j+1 ) )
                    jj = jj + n - j + 1_ilp
                 end if
              end do
           end if
           go to 40
           30 continue
           info = j
           40 continue
           return
     end subroutine stdlib_dpptrf


     pure module subroutine stdlib_cpptrf( uplo, n, ap, info )
     !! CPPTRF computes the Cholesky factorization of a complex Hermitian
     !! positive definite matrix A stored in packed format.
     !! The factorization has the form
     !! A = U**H * U,  if UPLO = 'U', or
     !! A = L  * L**H,  if UPLO = 'L',
     !! where U is an upper triangular matrix and L is lower triangular.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           complex(sp), intent(inout) :: ap(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, jc, jj
           real(sp) :: ajj
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CPPTRF', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( upper ) then
              ! compute the cholesky factorization a = u**h * u.
              jj = 0_ilp
              do j = 1, n
                 jc = jj + 1_ilp
                 jj = jj + j
                 ! compute elements 1:j-1 of column j.
                 if( j>1_ilp )call stdlib_ctpsv( 'UPPER', 'CONJUGATE TRANSPOSE', 'NON-UNIT',j-1, ap, &
                           ap( jc ), 1_ilp )
                 ! compute u(j,j) and test for non-positive-definiteness.
                 ajj = real( real( ap( jj ),KIND=sp) - stdlib_cdotc( j-1,ap( jc ), 1_ilp, ap( jc ), 1_ilp &
                           ),KIND=sp)
                 if( ajj<=zero ) then
                    ap( jj ) = ajj
                    go to 30
                 end if
                 ap( jj ) = sqrt( ajj )
              end do
           else
              ! compute the cholesky factorization a = l * l**h.
              jj = 1_ilp
              do j = 1, n
                 ! compute l(j,j) and test for non-positive-definiteness.
                 ajj = real( ap( jj ),KIND=sp)
                 if( ajj<=zero ) then
                    ap( jj ) = ajj
                    go to 30
                 end if
                 ajj = sqrt( ajj )
                 ap( jj ) = ajj
                 ! compute elements j+1:n of column j and update the trailing
                 ! submatrix.
                 if( j<n ) then
                    call stdlib_csscal( n-j, one / ajj, ap( jj+1 ), 1_ilp )
                    call stdlib_chpr( 'LOWER', n-j, -one, ap( jj+1 ), 1_ilp,ap( jj+n-j+1 ) )
                    jj = jj + n - j + 1_ilp
                 end if
              end do
           end if
           go to 40
           30 continue
           info = j
           40 continue
           return
     end subroutine stdlib_cpptrf

     pure module subroutine stdlib_zpptrf( uplo, n, ap, info )
     !! ZPPTRF computes the Cholesky factorization of a complex Hermitian
     !! positive definite matrix A stored in packed format.
     !! The factorization has the form
     !! A = U**H * U,  if UPLO = 'U', or
     !! A = L  * L**H,  if UPLO = 'L',
     !! where U is an upper triangular matrix and L is lower triangular.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           complex(dp), intent(inout) :: ap(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, jc, jj
           real(dp) :: ajj
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZPPTRF', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( upper ) then
              ! compute the cholesky factorization a = u**h * u.
              jj = 0_ilp
              do j = 1, n
                 jc = jj + 1_ilp
                 jj = jj + j
                 ! compute elements 1:j-1 of column j.
                 if( j>1_ilp )call stdlib_ztpsv( 'UPPER', 'CONJUGATE TRANSPOSE', 'NON-UNIT',j-1, ap, &
                           ap( jc ), 1_ilp )
                 ! compute u(j,j) and test for non-positive-definiteness.
                 ajj = real( ap( jj ),KIND=dp) - real( stdlib_zdotc( j-1,ap( jc ), 1_ilp, ap( jc ), 1_ilp &
                           ),KIND=dp)
                 if( ajj<=zero ) then
                    ap( jj ) = ajj
                    go to 30
                 end if
                 ap( jj ) = sqrt( ajj )
              end do
           else
              ! compute the cholesky factorization a = l * l**h.
              jj = 1_ilp
              do j = 1, n
                 ! compute l(j,j) and test for non-positive-definiteness.
                 ajj = real( ap( jj ),KIND=dp)
                 if( ajj<=zero ) then
                    ap( jj ) = ajj
                    go to 30
                 end if
                 ajj = sqrt( ajj )
                 ap( jj ) = ajj
                 ! compute elements j+1:n of column j and update the trailing
                 ! submatrix.
                 if( j<n ) then
                    call stdlib_zdscal( n-j, one / ajj, ap( jj+1 ), 1_ilp )
                    call stdlib_zhpr( 'LOWER', n-j, -one, ap( jj+1 ), 1_ilp,ap( jj+n-j+1 ) )
                    jj = jj + n - j + 1_ilp
                 end if
              end do
           end if
           go to 40
           30 continue
           info = j
           40 continue
           return
     end subroutine stdlib_zpptrf




     pure module subroutine stdlib_spptrs( uplo, n, nrhs, ap, b, ldb, info )
     !! SPPTRS solves a system of linear equations A*X = B with a symmetric
     !! positive definite matrix A in packed storage using the Cholesky
     !! factorization A = U**T*U or A = L*L**T computed by SPPTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           real(sp), intent(in) :: ap(*)
           real(sp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SPPTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( upper ) then
              ! solve a*x = b where a = u**t * u.
              do i = 1, nrhs
                 ! solve u**t *x = b, overwriting b with x.
                 call stdlib_stpsv( 'UPPER', 'TRANSPOSE', 'NON-UNIT', n, ap,b( 1_ilp, i ), 1_ilp )
                 ! solve u*x = b, overwriting b with x.
                 call stdlib_stpsv( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', n, ap,b( 1_ilp, i ), 1_ilp )
                           
              end do
           else
              ! solve a*x = b where a = l * l**t.
              do i = 1, nrhs
                 ! solve l*y = b, overwriting b with x.
                 call stdlib_stpsv( 'LOWER', 'NO TRANSPOSE', 'NON-UNIT', n, ap,b( 1_ilp, i ), 1_ilp )
                           
                 ! solve l**t *x = y, overwriting b with x.
                 call stdlib_stpsv( 'LOWER', 'TRANSPOSE', 'NON-UNIT', n, ap,b( 1_ilp, i ), 1_ilp )
              end do
           end if
           return
     end subroutine stdlib_spptrs

     pure module subroutine stdlib_dpptrs( uplo, n, nrhs, ap, b, ldb, info )
     !! DPPTRS solves a system of linear equations A*X = B with a symmetric
     !! positive definite matrix A in packed storage using the Cholesky
     !! factorization A = U**T*U or A = L*L**T computed by DPPTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           real(dp), intent(in) :: ap(*)
           real(dp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DPPTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( upper ) then
              ! solve a*x = b where a = u**t * u.
              do i = 1, nrhs
                 ! solve u**t *x = b, overwriting b with x.
                 call stdlib_dtpsv( 'UPPER', 'TRANSPOSE', 'NON-UNIT', n, ap,b( 1_ilp, i ), 1_ilp )
                 ! solve u*x = b, overwriting b with x.
                 call stdlib_dtpsv( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', n, ap,b( 1_ilp, i ), 1_ilp )
                           
              end do
           else
              ! solve a*x = b where a = l * l**t.
              do i = 1, nrhs
                 ! solve l*y = b, overwriting b with x.
                 call stdlib_dtpsv( 'LOWER', 'NO TRANSPOSE', 'NON-UNIT', n, ap,b( 1_ilp, i ), 1_ilp )
                           
                 ! solve l**t *x = y, overwriting b with x.
                 call stdlib_dtpsv( 'LOWER', 'TRANSPOSE', 'NON-UNIT', n, ap,b( 1_ilp, i ), 1_ilp )
              end do
           end if
           return
     end subroutine stdlib_dpptrs


     pure module subroutine stdlib_cpptrs( uplo, n, nrhs, ap, b, ldb, info )
     !! CPPTRS solves a system of linear equations A*X = B with a Hermitian
     !! positive definite matrix A in packed storage using the Cholesky
     !! factorization A = U**H*U or A = L*L**H computed by CPPTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           complex(sp), intent(in) :: ap(*)
           complex(sp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CPPTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( upper ) then
              ! solve a*x = b where a = u**h * u.
              do i = 1, nrhs
                 ! solve u**h *x = b, overwriting b with x.
                 call stdlib_ctpsv( 'UPPER', 'CONJUGATE TRANSPOSE', 'NON-UNIT', n,ap, b( 1_ilp, i ), &
                           1_ilp )
                 ! solve u*x = b, overwriting b with x.
                 call stdlib_ctpsv( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', n, ap,b( 1_ilp, i ), 1_ilp )
                           
              end do
           else
              ! solve a*x = b where a = l * l**h.
              do i = 1, nrhs
                 ! solve l*y = b, overwriting b with x.
                 call stdlib_ctpsv( 'LOWER', 'NO TRANSPOSE', 'NON-UNIT', n, ap,b( 1_ilp, i ), 1_ilp )
                           
                 ! solve l**h *x = y, overwriting b with x.
                 call stdlib_ctpsv( 'LOWER', 'CONJUGATE TRANSPOSE', 'NON-UNIT', n,ap, b( 1_ilp, i ), &
                           1_ilp )
              end do
           end if
           return
     end subroutine stdlib_cpptrs

     pure module subroutine stdlib_zpptrs( uplo, n, nrhs, ap, b, ldb, info )
     !! ZPPTRS solves a system of linear equations A*X = B with a Hermitian
     !! positive definite matrix A in packed storage using the Cholesky
     !! factorization A = U**H * U or A = L * L**H computed by ZPPTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           complex(dp), intent(in) :: ap(*)
           complex(dp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZPPTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( upper ) then
              ! solve a*x = b where a = u**h * u.
              do i = 1, nrhs
                 ! solve u**h *x = b, overwriting b with x.
                 call stdlib_ztpsv( 'UPPER', 'CONJUGATE TRANSPOSE', 'NON-UNIT', n,ap, b( 1_ilp, i ), &
                           1_ilp )
                 ! solve u*x = b, overwriting b with x.
                 call stdlib_ztpsv( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', n, ap,b( 1_ilp, i ), 1_ilp )
                           
              end do
           else
              ! solve a*x = b where a = l * l**h.
              do i = 1, nrhs
                 ! solve l*y = b, overwriting b with x.
                 call stdlib_ztpsv( 'LOWER', 'NO TRANSPOSE', 'NON-UNIT', n, ap,b( 1_ilp, i ), 1_ilp )
                           
                 ! solve l**h *x = y, overwriting b with x.
                 call stdlib_ztpsv( 'LOWER', 'CONJUGATE TRANSPOSE', 'NON-UNIT', n,ap, b( 1_ilp, i ), &
                           1_ilp )
              end do
           end if
           return
     end subroutine stdlib_zpptrs




     pure module subroutine stdlib_spptri( uplo, n, ap, info )
     !! SPPTRI computes the inverse of a real symmetric positive definite
     !! matrix A using the Cholesky factorization A = U**T*U or A = L*L**T
     !! computed by SPPTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(sp), intent(inout) :: ap(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, jc, jj, jjn
           real(sp) :: ajj
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SPPTRI', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! invert the triangular cholesky factor u or l.
           call stdlib_stptri( uplo, 'NON-UNIT', n, ap, info )
           if( info>0 )return
           if( upper ) then
              ! compute the product inv(u) * inv(u)**t.
              jj = 0_ilp
              do j = 1, n
                 jc = jj + 1_ilp
                 jj = jj + j
                 if( j>1_ilp )call stdlib_sspr( 'UPPER', j-1, one, ap( jc ), 1_ilp, ap )
                 ajj = ap( jj )
                 call stdlib_sscal( j, ajj, ap( jc ), 1_ilp )
              end do
           else
              ! compute the product inv(l)**t * inv(l).
              jj = 1_ilp
              do j = 1, n
                 jjn = jj + n - j + 1_ilp
                 ap( jj ) = stdlib_sdot( n-j+1, ap( jj ), 1_ilp, ap( jj ), 1_ilp )
                 if( j<n )call stdlib_stpmv( 'LOWER', 'TRANSPOSE', 'NON-UNIT', n-j,ap( jjn ), ap( &
                           jj+1 ), 1_ilp )
                 jj = jjn
              end do
           end if
           return
     end subroutine stdlib_spptri

     pure module subroutine stdlib_dpptri( uplo, n, ap, info )
     !! DPPTRI computes the inverse of a real symmetric positive definite
     !! matrix A using the Cholesky factorization A = U**T*U or A = L*L**T
     !! computed by DPPTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(dp), intent(inout) :: ap(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, jc, jj, jjn
           real(dp) :: ajj
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DPPTRI', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! invert the triangular cholesky factor u or l.
           call stdlib_dtptri( uplo, 'NON-UNIT', n, ap, info )
           if( info>0 )return
           if( upper ) then
              ! compute the product inv(u) * inv(u)**t.
              jj = 0_ilp
              do j = 1, n
                 jc = jj + 1_ilp
                 jj = jj + j
                 if( j>1_ilp )call stdlib_dspr( 'UPPER', j-1, one, ap( jc ), 1_ilp, ap )
                 ajj = ap( jj )
                 call stdlib_dscal( j, ajj, ap( jc ), 1_ilp )
              end do
           else
              ! compute the product inv(l)**t * inv(l).
              jj = 1_ilp
              do j = 1, n
                 jjn = jj + n - j + 1_ilp
                 ap( jj ) = stdlib_ddot( n-j+1, ap( jj ), 1_ilp, ap( jj ), 1_ilp )
                 if( j<n )call stdlib_dtpmv( 'LOWER', 'TRANSPOSE', 'NON-UNIT', n-j,ap( jjn ), ap( &
                           jj+1 ), 1_ilp )
                 jj = jjn
              end do
           end if
           return
     end subroutine stdlib_dpptri


     pure module subroutine stdlib_cpptri( uplo, n, ap, info )
     !! CPPTRI computes the inverse of a complex Hermitian positive definite
     !! matrix A using the Cholesky factorization A = U**H*U or A = L*L**H
     !! computed by CPPTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           complex(sp), intent(inout) :: ap(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, jc, jj, jjn
           real(sp) :: ajj
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CPPTRI', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! invert the triangular cholesky factor u or l.
           call stdlib_ctptri( uplo, 'NON-UNIT', n, ap, info )
           if( info>0 )return
           if( upper ) then
              ! compute the product inv(u) * inv(u)**h.
              jj = 0_ilp
              do j = 1, n
                 jc = jj + 1_ilp
                 jj = jj + j
                 if( j>1_ilp )call stdlib_chpr( 'UPPER', j-1, one, ap( jc ), 1_ilp, ap )
                 ajj = real( ap( jj ),KIND=sp)
                 call stdlib_csscal( j, ajj, ap( jc ), 1_ilp )
              end do
           else
              ! compute the product inv(l)**h * inv(l).
              jj = 1_ilp
              do j = 1, n
                 jjn = jj + n - j + 1_ilp
                 ap( jj ) = real( stdlib_cdotc( n-j+1, ap( jj ), 1_ilp, ap( jj ), 1_ilp ),KIND=sp)
                 if( j<n )call stdlib_ctpmv( 'LOWER', 'CONJUGATE TRANSPOSE', 'NON-UNIT',n-j, ap( &
                           jjn ), ap( jj+1 ), 1_ilp )
                 jj = jjn
              end do
           end if
           return
     end subroutine stdlib_cpptri

     pure module subroutine stdlib_zpptri( uplo, n, ap, info )
     !! ZPPTRI computes the inverse of a complex Hermitian positive definite
     !! matrix A using the Cholesky factorization A = U**H*U or A = L*L**H
     !! computed by ZPPTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           complex(dp), intent(inout) :: ap(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, jc, jj, jjn
           real(dp) :: ajj
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZPPTRI', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! invert the triangular cholesky factor u or l.
           call stdlib_ztptri( uplo, 'NON-UNIT', n, ap, info )
           if( info>0 )return
           if( upper ) then
              ! compute the product inv(u) * inv(u)**h.
              jj = 0_ilp
              do j = 1, n
                 jc = jj + 1_ilp
                 jj = jj + j
                 if( j>1_ilp )call stdlib_zhpr( 'UPPER', j-1, one, ap( jc ), 1_ilp, ap )
                 ajj = real( ap( jj ),KIND=dp)
                 call stdlib_zdscal( j, ajj, ap( jc ), 1_ilp )
              end do
           else
              ! compute the product inv(l)**h * inv(l).
              jj = 1_ilp
              do j = 1, n
                 jjn = jj + n - j + 1_ilp
                 ap( jj ) = real( stdlib_zdotc( n-j+1, ap( jj ), 1_ilp, ap( jj ), 1_ilp ),KIND=dp)
                 if( j<n )call stdlib_ztpmv( 'LOWER', 'CONJUGATE TRANSPOSE', 'NON-UNIT',n-j, ap( &
                           jjn ), ap( jj+1 ), 1_ilp )
                 jj = jjn
              end do
           end if
           return
     end subroutine stdlib_zpptri




     pure module subroutine stdlib_spprfs( uplo, n, nrhs, ap, afp, b, ldb, x, ldx, ferr,berr, work, &
     !! SPPRFS improves the computed solution to a system of linear
     !! equations when the coefficient matrix is symmetric positive definite
     !! and packed, and provides error bounds and backward error estimates
     !! for the solution.
               iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(in) :: afp(*), ap(*), b(ldb,*)
           real(sp), intent(out) :: berr(*), ferr(*), work(*)
           real(sp), intent(inout) :: x(ldx,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: count, i, ik, j, k, kase, kk, nz
           real(sp) :: eps, lstres, s, safe1, safe2, safmin, xk
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SPPRFS', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp .or. nrhs==0_ilp ) then
              do j = 1, nrhs
                 ferr( j ) = zero
                 berr( j ) = zero
              end do
              return
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = n + 1_ilp
           eps = stdlib_slamch( 'EPSILON' )
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           safe1 = nz*safmin
           safe2 = safe1 / eps
           ! do for each right hand side
           loop_140: do j = 1, nrhs
              count = 1_ilp
              lstres = three
              20 continue
              ! loop until stopping criterion is satisfied.
              ! compute residual r = b - a * x
              call stdlib_scopy( n, b( 1_ilp, j ), 1_ilp, work( n+1 ), 1_ilp )
              call stdlib_sspmv( uplo, n, -one, ap, x( 1_ilp, j ), 1_ilp, one, work( n+1 ),1_ilp )
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(a)*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              do i = 1, n
                 work( i ) = abs( b( i, j ) )
              end do
              ! compute abs(a)*abs(x) + abs(b).
              kk = 1_ilp
              if( upper ) then
                 do k = 1, n
                    s = zero
                    xk = abs( x( k, j ) )
                    ik = kk
                    do i = 1, k - 1
                       work( i ) = work( i ) + abs( ap( ik ) )*xk
                       s = s + abs( ap( ik ) )*abs( x( i, j ) )
                       ik = ik + 1_ilp
                    end do
                    work( k ) = work( k ) + abs( ap( kk+k-1 ) )*xk + s
                    kk = kk + k
                 end do
              else
                 do k = 1, n
                    s = zero
                    xk = abs( x( k, j ) )
                    work( k ) = work( k ) + abs( ap( kk ) )*xk
                    ik = kk + 1_ilp
                    do i = k + 1, n
                       work( i ) = work( i ) + abs( ap( ik ) )*xk
                       s = s + abs( ap( ik ) )*abs( x( i, j ) )
                       ik = ik + 1_ilp
                    end do
                    work( k ) = work( k ) + s
                    kk = kk + ( n-k+1 )
                 end do
              end if
              s = zero
              do i = 1, n
                 if( work( i )>safe2 ) then
                    s = max( s, abs( work( n+i ) ) / work( i ) )
                 else
                    s = max( s, ( abs( work( n+i ) )+safe1 ) /( work( i )+safe1 ) )
                 end if
              end do
              berr( j ) = s
              ! test stopping criterion. continue iterating if
                 ! 1) the residual berr(j) is larger than machine epsilon, and
                 ! 2) berr(j) decreased by at least a factor of 2 during the
                    ! last iteration, and
                 ! 3) at most itmax iterations tried.
              if( berr( j )>eps .and. two*berr( j )<=lstres .and.count<=itmax ) then
                 ! update solution and try again.
                 call stdlib_spptrs( uplo, n, 1_ilp, afp, work( n+1 ), n, info )
                 call stdlib_saxpy( n, one, work( n+1 ), 1_ilp, x( 1_ilp, j ), 1_ilp )
                 lstres = berr( j )
                 count = count + 1_ilp
                 go to 20
              end if
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(a))*
                 ! ( abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(a) is the inverse of a
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(a)*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(a)*abs(x) + abs(b) is less than safe2.
              ! use stdlib_slacn2 to estimate the infinity-norm of the matrix
                 ! inv(a) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) )))
              do i = 1, n
                 if( work( i )>safe2 ) then
                    work( i ) = abs( work( n+i ) ) + nz*eps*work( i )
                 else
                    work( i ) = abs( work( n+i ) ) + nz*eps*work( i ) + safe1
                 end if
              end do
              kase = 0_ilp
              100 continue
              call stdlib_slacn2( n, work( 2_ilp*n+1 ), work( n+1 ), iwork, ferr( j ),kase, isave )
                        
              if( kase/=0_ilp ) then
                 if( kase==1_ilp ) then
                    ! multiply by diag(w)*inv(a**t).
                    call stdlib_spptrs( uplo, n, 1_ilp, afp, work( n+1 ), n, info )
                    do i = 1, n
                       work( n+i ) = work( i )*work( n+i )
                    end do
                 else if( kase==2_ilp ) then
                    ! multiply by inv(a)*diag(w).
                    do i = 1, n
                       work( n+i ) = work( i )*work( n+i )
                    end do
                    call stdlib_spptrs( uplo, n, 1_ilp, afp, work( n+1 ), n, info )
                 end if
                 go to 100
              end if
              ! normalize error.
              lstres = zero
              do i = 1, n
                 lstres = max( lstres, abs( x( i, j ) ) )
              end do
              if( lstres/=zero )ferr( j ) = ferr( j ) / lstres
           end do loop_140
           return
     end subroutine stdlib_spprfs

     pure module subroutine stdlib_dpprfs( uplo, n, nrhs, ap, afp, b, ldb, x, ldx, ferr,berr, work, &
     !! DPPRFS improves the computed solution to a system of linear
     !! equations when the coefficient matrix is symmetric positive definite
     !! and packed, and provides error bounds and backward error estimates
     !! for the solution.
               iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(in) :: afp(*), ap(*), b(ldb,*)
           real(dp), intent(out) :: berr(*), ferr(*), work(*)
           real(dp), intent(inout) :: x(ldx,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: count, i, ik, j, k, kase, kk, nz
           real(dp) :: eps, lstres, s, safe1, safe2, safmin, xk
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DPPRFS', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp .or. nrhs==0_ilp ) then
              do j = 1, nrhs
                 ferr( j ) = zero
                 berr( j ) = zero
              end do
              return
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = n + 1_ilp
           eps = stdlib_dlamch( 'EPSILON' )
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           safe1 = nz*safmin
           safe2 = safe1 / eps
           ! do for each right hand side
           loop_140: do j = 1, nrhs
              count = 1_ilp
              lstres = three
              20 continue
              ! loop until stopping criterion is satisfied.
              ! compute residual r = b - a * x
              call stdlib_dcopy( n, b( 1_ilp, j ), 1_ilp, work( n+1 ), 1_ilp )
              call stdlib_dspmv( uplo, n, -one, ap, x( 1_ilp, j ), 1_ilp, one, work( n+1 ),1_ilp )
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(a)*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              do i = 1, n
                 work( i ) = abs( b( i, j ) )
              end do
              ! compute abs(a)*abs(x) + abs(b).
              kk = 1_ilp
              if( upper ) then
                 do k = 1, n
                    s = zero
                    xk = abs( x( k, j ) )
                    ik = kk
                    do i = 1, k - 1
                       work( i ) = work( i ) + abs( ap( ik ) )*xk
                       s = s + abs( ap( ik ) )*abs( x( i, j ) )
                       ik = ik + 1_ilp
                    end do
                    work( k ) = work( k ) + abs( ap( kk+k-1 ) )*xk + s
                    kk = kk + k
                 end do
              else
                 do k = 1, n
                    s = zero
                    xk = abs( x( k, j ) )
                    work( k ) = work( k ) + abs( ap( kk ) )*xk
                    ik = kk + 1_ilp
                    do i = k + 1, n
                       work( i ) = work( i ) + abs( ap( ik ) )*xk
                       s = s + abs( ap( ik ) )*abs( x( i, j ) )
                       ik = ik + 1_ilp
                    end do
                    work( k ) = work( k ) + s
                    kk = kk + ( n-k+1 )
                 end do
              end if
              s = zero
              do i = 1, n
                 if( work( i )>safe2 ) then
                    s = max( s, abs( work( n+i ) ) / work( i ) )
                 else
                    s = max( s, ( abs( work( n+i ) )+safe1 ) /( work( i )+safe1 ) )
                 end if
              end do
              berr( j ) = s
              ! test stopping criterion. continue iterating if
                 ! 1) the residual berr(j) is larger than machine epsilon, and
                 ! 2) berr(j) decreased by at least a factor of 2 during the
                    ! last iteration, and
                 ! 3) at most itmax iterations tried.
              if( berr( j )>eps .and. two*berr( j )<=lstres .and.count<=itmax ) then
                 ! update solution and try again.
                 call stdlib_dpptrs( uplo, n, 1_ilp, afp, work( n+1 ), n, info )
                 call stdlib_daxpy( n, one, work( n+1 ), 1_ilp, x( 1_ilp, j ), 1_ilp )
                 lstres = berr( j )
                 count = count + 1_ilp
                 go to 20
              end if
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(a))*
                 ! ( abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(a) is the inverse of a
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(a)*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(a)*abs(x) + abs(b) is less than safe2.
              ! use stdlib_dlacn2 to estimate the infinity-norm of the matrix
                 ! inv(a) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) )))
              do i = 1, n
                 if( work( i )>safe2 ) then
                    work( i ) = abs( work( n+i ) ) + nz*eps*work( i )
                 else
                    work( i ) = abs( work( n+i ) ) + nz*eps*work( i ) + safe1
                 end if
              end do
              kase = 0_ilp
              100 continue
              call stdlib_dlacn2( n, work( 2_ilp*n+1 ), work( n+1 ), iwork, ferr( j ),kase, isave )
                        
              if( kase/=0_ilp ) then
                 if( kase==1_ilp ) then
                    ! multiply by diag(w)*inv(a**t).
                    call stdlib_dpptrs( uplo, n, 1_ilp, afp, work( n+1 ), n, info )
                    do i = 1, n
                       work( n+i ) = work( i )*work( n+i )
                    end do
                 else if( kase==2_ilp ) then
                    ! multiply by inv(a)*diag(w).
                    do i = 1, n
                       work( n+i ) = work( i )*work( n+i )
                    end do
                    call stdlib_dpptrs( uplo, n, 1_ilp, afp, work( n+1 ), n, info )
                 end if
                 go to 100
              end if
              ! normalize error.
              lstres = zero
              do i = 1, n
                 lstres = max( lstres, abs( x( i, j ) ) )
              end do
              if( lstres/=zero )ferr( j ) = ferr( j ) / lstres
           end do loop_140
           return
     end subroutine stdlib_dpprfs


     pure module subroutine stdlib_cpprfs( uplo, n, nrhs, ap, afp, b, ldb, x, ldx, ferr,berr, work, &
     !! CPPRFS improves the computed solution to a system of linear
     !! equations when the coefficient matrix is Hermitian positive definite
     !! and packed, and provides error bounds and backward error estimates
     !! for the solution.
               rwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           ! Array Arguments 
           real(sp), intent(out) :: berr(*), ferr(*), rwork(*)
           complex(sp), intent(in) :: afp(*), ap(*), b(ldb,*)
           complex(sp), intent(out) :: work(*)
           complex(sp), intent(inout) :: x(ldx,*)
        ! ====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: count, i, ik, j, k, kase, kk, nz
           real(sp) :: eps, lstres, s, safe1, safe2, safmin, xk
           complex(sp) :: zdum
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CPPRFS', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp .or. nrhs==0_ilp ) then
              do j = 1, nrhs
                 ferr( j ) = zero
                 berr( j ) = zero
              end do
              return
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = n + 1_ilp
           eps = stdlib_slamch( 'EPSILON' )
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           safe1 = nz*safmin
           safe2 = safe1 / eps
           ! do for each right hand side
           loop_140: do j = 1, nrhs
              count = 1_ilp
              lstres = three
              20 continue
              ! loop until stopping criterion is satisfied.
              ! compute residual r = b - a * x
              call stdlib_ccopy( n, b( 1_ilp, j ), 1_ilp, work, 1_ilp )
              call stdlib_chpmv( uplo, n, -cone, ap, x( 1_ilp, j ), 1_ilp, cone, work, 1_ilp )
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(a)*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              do i = 1, n
                 rwork( i ) = cabs1( b( i, j ) )
              end do
              ! compute abs(a)*abs(x) + abs(b).
              kk = 1_ilp
              if( upper ) then
                 do k = 1, n
                    s = zero
                    xk = cabs1( x( k, j ) )
                    ik = kk
                    do i = 1, k - 1
                       rwork( i ) = rwork( i ) + cabs1( ap( ik ) )*xk
                       s = s + cabs1( ap( ik ) )*cabs1( x( i, j ) )
                       ik = ik + 1_ilp
                    end do
                    rwork( k ) = rwork( k ) + abs( real( ap( kk+k-1 ),KIND=sp) )*xk + s
                    kk = kk + k
                 end do
              else
                 do k = 1, n
                    s = zero
                    xk = cabs1( x( k, j ) )
                    rwork( k ) = rwork( k ) + abs( real( ap( kk ),KIND=sp) )*xk
                    ik = kk + 1_ilp
                    do i = k + 1, n
                       rwork( i ) = rwork( i ) + cabs1( ap( ik ) )*xk
                       s = s + cabs1( ap( ik ) )*cabs1( x( i, j ) )
                       ik = ik + 1_ilp
                    end do
                    rwork( k ) = rwork( k ) + s
                    kk = kk + ( n-k+1 )
                 end do
              end if
              s = zero
              do i = 1, n
                 if( rwork( i )>safe2 ) then
                    s = max( s, cabs1( work( i ) ) / rwork( i ) )
                 else
                    s = max( s, ( cabs1( work( i ) )+safe1 ) /( rwork( i )+safe1 ) )
                 end if
              end do
              berr( j ) = s
              ! test stopping criterion. continue iterating if
                 ! 1) the residual berr(j) is larger than machine epsilon, and
                 ! 2) berr(j) decreased by at least a factor of 2 during the
                    ! last iteration, and
                 ! 3) at most itmax iterations tried.
              if( berr( j )>eps .and. two*berr( j )<=lstres .and.count<=itmax ) then
                 ! update solution and try again.
                 call stdlib_cpptrs( uplo, n, 1_ilp, afp, work, n, info )
                 call stdlib_caxpy( n, cone, work, 1_ilp, x( 1_ilp, j ), 1_ilp )
                 lstres = berr( j )
                 count = count + 1_ilp
                 go to 20
              end if
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(a))*
                 ! ( abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(a) is the inverse of a
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(a)*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(a)*abs(x) + abs(b) is less than safe2.
              ! use stdlib_clacn2 to estimate the infinity-norm of the matrix
                 ! inv(a) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) )))
              do i = 1, n
                 if( rwork( i )>safe2 ) then
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i )
                 else
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i ) +safe1
                 end if
              end do
              kase = 0_ilp
              100 continue
              call stdlib_clacn2( n, work( n+1 ), work, ferr( j ), kase, isave )
              if( kase/=0_ilp ) then
                 if( kase==1_ilp ) then
                    ! multiply by diag(w)*inv(a**h).
                    call stdlib_cpptrs( uplo, n, 1_ilp, afp, work, n, info )
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                 else if( kase==2_ilp ) then
                    ! multiply by inv(a)*diag(w).
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                    call stdlib_cpptrs( uplo, n, 1_ilp, afp, work, n, info )
                 end if
                 go to 100
              end if
              ! normalize error.
              lstres = zero
              do i = 1, n
                 lstres = max( lstres, cabs1( x( i, j ) ) )
              end do
              if( lstres/=zero )ferr( j ) = ferr( j ) / lstres
           end do loop_140
           return
     end subroutine stdlib_cpprfs

     pure module subroutine stdlib_zpprfs( uplo, n, nrhs, ap, afp, b, ldb, x, ldx, ferr,berr, work, &
     !! ZPPRFS improves the computed solution to a system of linear
     !! equations when the coefficient matrix is Hermitian positive definite
     !! and packed, and provides error bounds and backward error estimates
     !! for the solution.
               rwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           ! Array Arguments 
           real(dp), intent(out) :: berr(*), ferr(*), rwork(*)
           complex(dp), intent(in) :: afp(*), ap(*), b(ldb,*)
           complex(dp), intent(out) :: work(*)
           complex(dp), intent(inout) :: x(ldx,*)
        ! ====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: count, i, ik, j, k, kase, kk, nz
           real(dp) :: eps, lstres, s, safe1, safe2, safmin, xk
           complex(dp) :: zdum
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZPPRFS', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp .or. nrhs==0_ilp ) then
              do j = 1, nrhs
                 ferr( j ) = zero
                 berr( j ) = zero
              end do
              return
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = n + 1_ilp
           eps = stdlib_dlamch( 'EPSILON' )
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           safe1 = nz*safmin
           safe2 = safe1 / eps
           ! do for each right hand side
           loop_140: do j = 1, nrhs
              count = 1_ilp
              lstres = three
              20 continue
              ! loop until stopping criterion is satisfied.
              ! compute residual r = b - a * x
              call stdlib_zcopy( n, b( 1_ilp, j ), 1_ilp, work, 1_ilp )
              call stdlib_zhpmv( uplo, n, -cone, ap, x( 1_ilp, j ), 1_ilp, cone, work, 1_ilp )
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(a)*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              do i = 1, n
                 rwork( i ) = cabs1( b( i, j ) )
              end do
              ! compute abs(a)*abs(x) + abs(b).
              kk = 1_ilp
              if( upper ) then
                 do k = 1, n
                    s = zero
                    xk = cabs1( x( k, j ) )
                    ik = kk
                    do i = 1, k - 1
                       rwork( i ) = rwork( i ) + cabs1( ap( ik ) )*xk
                       s = s + cabs1( ap( ik ) )*cabs1( x( i, j ) )
                       ik = ik + 1_ilp
                    end do
                    rwork( k ) = rwork( k ) + abs( real( ap( kk+k-1 ),KIND=dp) )*xk + s
                    kk = kk + k
                 end do
              else
                 do k = 1, n
                    s = zero
                    xk = cabs1( x( k, j ) )
                    rwork( k ) = rwork( k ) + abs( real( ap( kk ),KIND=dp) )*xk
                    ik = kk + 1_ilp
                    do i = k + 1, n
                       rwork( i ) = rwork( i ) + cabs1( ap( ik ) )*xk
                       s = s + cabs1( ap( ik ) )*cabs1( x( i, j ) )
                       ik = ik + 1_ilp
                    end do
                    rwork( k ) = rwork( k ) + s
                    kk = kk + ( n-k+1 )
                 end do
              end if
              s = zero
              do i = 1, n
                 if( rwork( i )>safe2 ) then
                    s = max( s, cabs1( work( i ) ) / rwork( i ) )
                 else
                    s = max( s, ( cabs1( work( i ) )+safe1 ) /( rwork( i )+safe1 ) )
                 end if
              end do
              berr( j ) = s
              ! test stopping criterion. continue iterating if
                 ! 1) the residual berr(j) is larger than machine epsilon, and
                 ! 2) berr(j) decreased by at least a factor of 2 during the
                    ! last iteration, and
                 ! 3) at most itmax iterations tried.
              if( berr( j )>eps .and. two*berr( j )<=lstres .and.count<=itmax ) then
                 ! update solution and try again.
                 call stdlib_zpptrs( uplo, n, 1_ilp, afp, work, n, info )
                 call stdlib_zaxpy( n, cone, work, 1_ilp, x( 1_ilp, j ), 1_ilp )
                 lstres = berr( j )
                 count = count + 1_ilp
                 go to 20
              end if
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(a))*
                 ! ( abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(a) is the inverse of a
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(a)*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(a)*abs(x) + abs(b) is less than safe2.
              ! use stdlib_zlacn2 to estimate the infinity-norm of the matrix
                 ! inv(a) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) )))
              do i = 1, n
                 if( rwork( i )>safe2 ) then
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i )
                 else
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i ) +safe1
                 end if
              end do
              kase = 0_ilp
              100 continue
              call stdlib_zlacn2( n, work( n+1 ), work, ferr( j ), kase, isave )
              if( kase/=0_ilp ) then
                 if( kase==1_ilp ) then
                    ! multiply by diag(w)*inv(a**h).
                    call stdlib_zpptrs( uplo, n, 1_ilp, afp, work, n, info )
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                 else if( kase==2_ilp ) then
                    ! multiply by inv(a)*diag(w).
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                    call stdlib_zpptrs( uplo, n, 1_ilp, afp, work, n, info )
                 end if
                 go to 100
              end if
              ! normalize error.
              lstres = zero
              do i = 1, n
                 lstres = max( lstres, cabs1( x( i, j ) ) )
              end do
              if( lstres/=zero )ferr( j ) = ferr( j ) / lstres
           end do loop_140
           return
     end subroutine stdlib_zpprfs




     pure module subroutine stdlib_sppequ( uplo, n, ap, s, scond, amax, info )
     !! SPPEQU computes row and column scalings intended to equilibrate a
     !! symmetric positive definite matrix A in packed storage and reduce
     !! its condition number (with respect to the two-norm).  S contains the
     !! scale factors, S(i)=1/sqrt(A(i,i)), chosen so that the scaled matrix
     !! B with elements B(i,j)=S(i)*A(i,j)*S(j) has ones on the diagonal.
     !! This choice of S puts the condition number of B within a factor N of
     !! the smallest possible condition number over all possible diagonal
     !! scalings.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(sp), intent(out) :: amax, scond
           ! Array Arguments 
           real(sp), intent(in) :: ap(*)
           real(sp), intent(out) :: s(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, jj
           real(sp) :: smin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SPPEQU', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              scond = one
              amax = zero
              return
           end if
           ! initialize smin and amax.
           s( 1_ilp ) = ap( 1_ilp )
           smin = s( 1_ilp )
           amax = s( 1_ilp )
           if( upper ) then
              ! uplo = 'u':  upper triangle of a is stored.
              ! find the minimum and maximum diagonal elements.
              jj = 1_ilp
              do i = 2, n
                 jj = jj + i
                 s( i ) = ap( jj )
                 smin = min( smin, s( i ) )
                 amax = max( amax, s( i ) )
              end do
           else
              ! uplo = 'l':  lower triangle of a is stored.
              ! find the minimum and maximum diagonal elements.
              jj = 1_ilp
              do i = 2, n
                 jj = jj + n - i + 2_ilp
                 s( i ) = ap( jj )
                 smin = min( smin, s( i ) )
                 amax = max( amax, s( i ) )
              end do
           end if
           if( smin<=zero ) then
              ! find the first non-positive diagonal element and return.
              do i = 1, n
                 if( s( i )<=zero ) then
                    info = i
                    return
                 end if
              end do
           else
              ! set the scale factors to the reciprocals
              ! of the diagonal elements.
              do i = 1, n
                 s( i ) = one / sqrt( s( i ) )
              end do
              ! compute scond = min(s(i)) / max(s(i))
              scond = sqrt( smin ) / sqrt( amax )
           end if
           return
     end subroutine stdlib_sppequ

     pure module subroutine stdlib_dppequ( uplo, n, ap, s, scond, amax, info )
     !! DPPEQU computes row and column scalings intended to equilibrate a
     !! symmetric positive definite matrix A in packed storage and reduce
     !! its condition number (with respect to the two-norm).  S contains the
     !! scale factors, S(i)=1/sqrt(A(i,i)), chosen so that the scaled matrix
     !! B with elements B(i,j)=S(i)*A(i,j)*S(j) has ones on the diagonal.
     !! This choice of S puts the condition number of B within a factor N of
     !! the smallest possible condition number over all possible diagonal
     !! scalings.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(dp), intent(out) :: amax, scond
           ! Array Arguments 
           real(dp), intent(in) :: ap(*)
           real(dp), intent(out) :: s(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, jj
           real(dp) :: smin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DPPEQU', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              scond = one
              amax = zero
              return
           end if
           ! initialize smin and amax.
           s( 1_ilp ) = ap( 1_ilp )
           smin = s( 1_ilp )
           amax = s( 1_ilp )
           if( upper ) then
              ! uplo = 'u':  upper triangle of a is stored.
              ! find the minimum and maximum diagonal elements.
              jj = 1_ilp
              do i = 2, n
                 jj = jj + i
                 s( i ) = ap( jj )
                 smin = min( smin, s( i ) )
                 amax = max( amax, s( i ) )
              end do
           else
              ! uplo = 'l':  lower triangle of a is stored.
              ! find the minimum and maximum diagonal elements.
              jj = 1_ilp
              do i = 2, n
                 jj = jj + n - i + 2_ilp
                 s( i ) = ap( jj )
                 smin = min( smin, s( i ) )
                 amax = max( amax, s( i ) )
              end do
           end if
           if( smin<=zero ) then
              ! find the first non-positive diagonal element and return.
              do i = 1, n
                 if( s( i )<=zero ) then
                    info = i
                    return
                 end if
              end do
           else
              ! set the scale factors to the reciprocals
              ! of the diagonal elements.
              do i = 1, n
                 s( i ) = one / sqrt( s( i ) )
              end do
              ! compute scond = min(s(i)) / max(s(i))
              scond = sqrt( smin ) / sqrt( amax )
           end if
           return
     end subroutine stdlib_dppequ


     pure module subroutine stdlib_cppequ( uplo, n, ap, s, scond, amax, info )
     !! CPPEQU computes row and column scalings intended to equilibrate a
     !! Hermitian positive definite matrix A in packed storage and reduce
     !! its condition number (with respect to the two-norm).  S contains the
     !! scale factors, S(i)=1/sqrt(A(i,i)), chosen so that the scaled matrix
     !! B with elements B(i,j)=S(i)*A(i,j)*S(j) has ones on the diagonal.
     !! This choice of S puts the condition number of B within a factor N of
     !! the smallest possible condition number over all possible diagonal
     !! scalings.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(sp), intent(out) :: amax, scond
           ! Array Arguments 
           real(sp), intent(out) :: s(*)
           complex(sp), intent(in) :: ap(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, jj
           real(sp) :: smin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CPPEQU', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              scond = one
              amax = zero
              return
           end if
           ! initialize smin and amax.
           s( 1_ilp ) = real( ap( 1_ilp ),KIND=sp)
           smin = s( 1_ilp )
           amax = s( 1_ilp )
           if( upper ) then
              ! uplo = 'u':  upper triangle of a is stored.
              ! find the minimum and maximum diagonal elements.
              jj = 1_ilp
              do i = 2, n
                 jj = jj + i
                 s( i ) = real( ap( jj ),KIND=sp)
                 smin = min( smin, s( i ) )
                 amax = max( amax, s( i ) )
              end do
           else
              ! uplo = 'l':  lower triangle of a is stored.
              ! find the minimum and maximum diagonal elements.
              jj = 1_ilp
              do i = 2, n
                 jj = jj + n - i + 2_ilp
                 s( i ) = real( ap( jj ),KIND=sp)
                 smin = min( smin, s( i ) )
                 amax = max( amax, s( i ) )
              end do
           end if
           if( smin<=zero ) then
              ! find the first non-positive diagonal element and return.
              do i = 1, n
                 if( s( i )<=zero ) then
                    info = i
                    return
                 end if
              end do
           else
              ! set the scale factors to the reciprocals
              ! of the diagonal elements.
              do i = 1, n
                 s( i ) = one / sqrt( s( i ) )
              end do
              ! compute scond = min(s(i)) / max(s(i))
              scond = sqrt( smin ) / sqrt( amax )
           end if
           return
     end subroutine stdlib_cppequ

     pure module subroutine stdlib_zppequ( uplo, n, ap, s, scond, amax, info )
     !! ZPPEQU computes row and column scalings intended to equilibrate a
     !! Hermitian positive definite matrix A in packed storage and reduce
     !! its condition number (with respect to the two-norm).  S contains the
     !! scale factors, S(i)=1/sqrt(A(i,i)), chosen so that the scaled matrix
     !! B with elements B(i,j)=S(i)*A(i,j)*S(j) has ones on the diagonal.
     !! This choice of S puts the condition number of B within a factor N of
     !! the smallest possible condition number over all possible diagonal
     !! scalings.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(dp), intent(out) :: amax, scond
           ! Array Arguments 
           real(dp), intent(out) :: s(*)
           complex(dp), intent(in) :: ap(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, jj
           real(dp) :: smin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZPPEQU', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              scond = one
              amax = zero
              return
           end if
           ! initialize smin and amax.
           s( 1_ilp ) = real( ap( 1_ilp ),KIND=dp)
           smin = s( 1_ilp )
           amax = s( 1_ilp )
           if( upper ) then
              ! uplo = 'u':  upper triangle of a is stored.
              ! find the minimum and maximum diagonal elements.
              jj = 1_ilp
              do i = 2, n
                 jj = jj + i
                 s( i ) = real( ap( jj ),KIND=dp)
                 smin = min( smin, s( i ) )
                 amax = max( amax, s( i ) )
              end do
           else
              ! uplo = 'l':  lower triangle of a is stored.
              ! find the minimum and maximum diagonal elements.
              jj = 1_ilp
              do i = 2, n
                 jj = jj + n - i + 2_ilp
                 s( i ) = real( ap( jj ),KIND=dp)
                 smin = min( smin, s( i ) )
                 amax = max( amax, s( i ) )
              end do
           end if
           if( smin<=zero ) then
              ! find the first non-positive diagonal element and return.
              do i = 1, n
                 if( s( i )<=zero ) then
                    info = i
                    return
                 end if
              end do
           else
              ! set the scale factors to the reciprocals
              ! of the diagonal elements.
              do i = 1, n
                 s( i ) = one / sqrt( s( i ) )
              end do
              ! compute scond = min(s(i)) / max(s(i))
              scond = sqrt( smin ) / sqrt( amax )
           end if
           return
     end subroutine stdlib_zppequ




     pure module subroutine stdlib_claqhp( uplo, n, ap, s, scond, amax, equed )
     !! CLAQHP equilibrates a Hermitian matrix A using the scaling factors
     !! in the vector S.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(out) :: equed
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: n
           real(sp), intent(in) :: amax, scond
           ! Array Arguments 
           real(sp), intent(in) :: s(*)
           complex(sp), intent(inout) :: ap(*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: thresh = 0.1e+0_sp
           
           ! Local Scalars 
           integer(ilp) :: i, j, jc
           real(sp) :: cj, large, small
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( n<=0_ilp ) then
              equed = 'N'
              return
           end if
           ! initialize large and small.
           small = stdlib_slamch( 'SAFE MINIMUM' ) / stdlib_slamch( 'PRECISION' )
           large = one / small
           if( scond>=thresh .and. amax>=small .and. amax<=large ) then
              ! no equilibration
              equed = 'N'
           else
              ! replace a by diag(s) * a * diag(s).
              if( stdlib_lsame( uplo, 'U' ) ) then
                 ! upper triangle of a is stored.
                 jc = 1_ilp
                 do j = 1, n
                    cj = s( j )
                    do i = 1, j - 1
                       ap( jc+i-1 ) = cj*s( i )*ap( jc+i-1 )
                    end do
                    ap( jc+j-1 ) = cj*cj*real( ap( jc+j-1 ),KIND=sp)
                    jc = jc + j
                 end do
              else
                 ! lower triangle of a is stored.
                 jc = 1_ilp
                 do j = 1, n
                    cj = s( j )
                    ap( jc ) = cj*cj*real( ap( jc ),KIND=sp)
                    do i = j + 1, n
                       ap( jc+i-j ) = cj*s( i )*ap( jc+i-j )
                    end do
                    jc = jc + n - j + 1_ilp
                 end do
              end if
              equed = 'Y'
           end if
           return
     end subroutine stdlib_claqhp

     pure module subroutine stdlib_zlaqhp( uplo, n, ap, s, scond, amax, equed )
     !! ZLAQHP equilibrates a Hermitian matrix A using the scaling factors
     !! in the vector S.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(out) :: equed
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: n
           real(dp), intent(in) :: amax, scond
           ! Array Arguments 
           real(dp), intent(in) :: s(*)
           complex(dp), intent(inout) :: ap(*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: thresh = 0.1e+0_dp
           
           ! Local Scalars 
           integer(ilp) :: i, j, jc
           real(dp) :: cj, large, small
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( n<=0_ilp ) then
              equed = 'N'
              return
           end if
           ! initialize large and small.
           small = stdlib_dlamch( 'SAFE MINIMUM' ) / stdlib_dlamch( 'PRECISION' )
           large = one / small
           if( scond>=thresh .and. amax>=small .and. amax<=large ) then
              ! no equilibration
              equed = 'N'
           else
              ! replace a by diag(s) * a * diag(s).
              if( stdlib_lsame( uplo, 'U' ) ) then
                 ! upper triangle of a is stored.
                 jc = 1_ilp
                 do j = 1, n
                    cj = s( j )
                    do i = 1, j - 1
                       ap( jc+i-1 ) = cj*s( i )*ap( jc+i-1 )
                    end do
                    ap( jc+j-1 ) = cj*cj*real( ap( jc+j-1 ),KIND=dp)
                    jc = jc + j
                 end do
              else
                 ! lower triangle of a is stored.
                 jc = 1_ilp
                 do j = 1, n
                    cj = s( j )
                    ap( jc ) = cj*cj*real( ap( jc ),KIND=dp)
                    do i = j + 1, n
                       ap( jc+i-j ) = cj*s( i )*ap( jc+i-j )
                    end do
                    jc = jc + n - j + 1_ilp
                 end do
              end if
              equed = 'Y'
           end if
           return
     end subroutine stdlib_zlaqhp




     pure module subroutine stdlib_spftrf( transr, uplo, n, a, info )
     !! SPFTRF computes the Cholesky factorization of a real symmetric
     !! positive definite matrix A.
     !! The factorization has the form
     !! A = U**T * U,  if UPLO = 'U', or
     !! A = L  * L**T,  if UPLO = 'L',
     !! where U is an upper triangular matrix and L is lower triangular.
     !! This is the block version of the algorithm, calling Level 3 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo
           integer(ilp), intent(in) :: n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           real(sp), intent(inout) :: a(0_ilp:*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lower, nisodd, normaltransr
           integer(ilp) :: n1, n2, k
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'T' ) ) then
              info = -1_ilp
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SPFTRF', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! if n is odd, set nisodd = .true.
           ! if n is even, set k = n/2 and nisodd = .false.
           if( mod( n, 2_ilp )==0_ilp ) then
              k = n / 2_ilp
              nisodd = .false.
           else
              nisodd = .true.
           end if
           ! set n1 and n2 depending on lower
           if( lower ) then
              n2 = n / 2_ilp
              n1 = n - n2
           else
              n1 = n / 2_ilp
              n2 = n - n1
           end if
           ! start execution: there are eight cases
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                   ! srpa for lower, normal and n is odd ( a(0:n-1,0:n1-1) )
                   ! t1 -> a(0,0), t2 -> a(0,1), s -> a(n1,0)
                   ! t1 -> a(0), t2 -> a(n), s -> a(n1)
                    call stdlib_spotrf( 'L', n1, a( 0_ilp ), n, info )
                    if( info>0 )return
                    call stdlib_strsm( 'R', 'L', 'T', 'N', n2, n1, one, a( 0_ilp ), n,a( n1 ), n )
                              
                    call stdlib_ssyrk( 'U', 'N', n2, n1, -one, a( n1 ), n, one,a( n ), n )
                    call stdlib_spotrf( 'U', n2, a( n ), n, info )
                    if( info>0_ilp )info = info + n1
                 else
                   ! srpa for upper, normal and n is odd ( a(0:n-1,0:n2-1)
                   ! t1 -> a(n1+1,0), t2 -> a(n1,0), s -> a(0,0)
                   ! t1 -> a(n2), t2 -> a(n1), s -> a(0)
                    call stdlib_spotrf( 'L', n1, a( n2 ), n, info )
                    if( info>0 )return
                    call stdlib_strsm( 'L', 'L', 'N', 'N', n1, n2, one, a( n2 ), n,a( 0_ilp ), n )
                              
                    call stdlib_ssyrk( 'U', 'T', n2, n1, -one, a( 0_ilp ), n, one,a( n1 ), n )
                    call stdlib_spotrf( 'U', n2, a( n1 ), n, info )
                    if( info>0_ilp )info = info + n1
                 end if
              else
                 ! n is odd and transr = 't'
                 if( lower ) then
                    ! srpa for lower, transpose and n is odd
                    ! t1 -> a(0,0) , t2 -> a(1,0) , s -> a(0,n1)
                    ! t1 -> a(0+0) , t2 -> a(1+0) , s -> a(0+n1*n1); lda=n1
                    call stdlib_spotrf( 'U', n1, a( 0_ilp ), n1, info )
                    if( info>0 )return
                    call stdlib_strsm( 'L', 'U', 'T', 'N', n1, n2, one, a( 0_ilp ), n1,a( n1*n1 ), n1 &
                              )
                    call stdlib_ssyrk( 'L', 'T', n2, n1, -one, a( n1*n1 ), n1, one,a( 1_ilp ), n1 )
                              
                    call stdlib_spotrf( 'L', n2, a( 1_ilp ), n1, info )
                    if( info>0_ilp )info = info + n1
                 else
                    ! srpa for upper, transpose and n is odd
                    ! t1 -> a(0,n1+1), t2 -> a(0,n1), s -> a(0,0)
                    ! t1 -> a(n2*n2), t2 -> a(n1*n2), s -> a(0); lda = n2
                    call stdlib_spotrf( 'U', n1, a( n2*n2 ), n2, info )
                    if( info>0 )return
                    call stdlib_strsm( 'R', 'U', 'N', 'N', n2, n1, one, a( n2*n2 ),n2, a( 0_ilp ), n2 &
                              )
                    call stdlib_ssyrk( 'L', 'N', n2, n1, -one, a( 0_ilp ), n2, one,a( n1*n2 ), n2 )
                              
                    call stdlib_spotrf( 'L', n2, a( n1*n2 ), n2, info )
                    if( info>0_ilp )info = info + n1
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! srpa for lower, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(1,0), t2 -> a(0,0), s -> a(k+1,0)
                    ! t1 -> a(1), t2 -> a(0), s -> a(k+1)
                    call stdlib_spotrf( 'L', k, a( 1_ilp ), n+1, info )
                    if( info>0 )return
                    call stdlib_strsm( 'R', 'L', 'T', 'N', k, k, one, a( 1_ilp ), n+1,a( k+1 ), n+1 )
                              
                    call stdlib_ssyrk( 'U', 'N', k, k, -one, a( k+1 ), n+1, one,a( 0_ilp ), n+1 )
                              
                    call stdlib_spotrf( 'U', k, a( 0_ilp ), n+1, info )
                    if( info>0_ilp )info = info + k
                 else
                    ! srpa for upper, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(k+1,0) ,  t2 -> a(k,0),   s -> a(0,0)
                    ! t1 -> a(k+1), t2 -> a(k), s -> a(0)
                    call stdlib_spotrf( 'L', k, a( k+1 ), n+1, info )
                    if( info>0 )return
                    call stdlib_strsm( 'L', 'L', 'N', 'N', k, k, one, a( k+1 ),n+1, a( 0_ilp ), n+1 )
                              
                    call stdlib_ssyrk( 'U', 'T', k, k, -one, a( 0_ilp ), n+1, one,a( k ), n+1 )
                              
                    call stdlib_spotrf( 'U', k, a( k ), n+1, info )
                    if( info>0_ilp )info = info + k
                 end if
              else
                 ! n is even and transr = 't'
                 if( lower ) then
                    ! srpa for lower, transpose and n is even (see paper)
                    ! t1 -> b(0,1), t2 -> b(0,0), s -> b(0,k+1)
                    ! t1 -> a(0+k), t2 -> a(0+0), s -> a(0+k*(k+1)); lda=k
                    call stdlib_spotrf( 'U', k, a( 0_ilp+k ), k, info )
                    if( info>0 )return
                    call stdlib_strsm( 'L', 'U', 'T', 'N', k, k, one, a( k ), n1,a( k*( k+1 ) ), &
                              k )
                    call stdlib_ssyrk( 'L', 'T', k, k, -one, a( k*( k+1 ) ), k, one,a( 0_ilp ), k )
                              
                    call stdlib_spotrf( 'L', k, a( 0_ilp ), k, info )
                    if( info>0_ilp )info = info + k
                 else
                    ! srpa for upper, transpose and n is even (see paper)
                    ! t1 -> b(0,k+1),     t2 -> b(0,k),   s -> b(0,0)
                    ! t1 -> a(0+k*(k+1)), t2 -> a(0+k*k), s -> a(0+0)); lda=k
                    call stdlib_spotrf( 'U', k, a( k*( k+1 ) ), k, info )
                    if( info>0 )return
                    call stdlib_strsm( 'R', 'U', 'N', 'N', k, k, one,a( k*( k+1 ) ), k, a( 0_ilp ), k &
                              )
                    call stdlib_ssyrk( 'L', 'N', k, k, -one, a( 0_ilp ), k, one,a( k*k ), k )
                    call stdlib_spotrf( 'L', k, a( k*k ), k, info )
                    if( info>0_ilp )info = info + k
                 end if
              end if
           end if
           return
     end subroutine stdlib_spftrf

     pure module subroutine stdlib_dpftrf( transr, uplo, n, a, info )
     !! DPFTRF computes the Cholesky factorization of a real symmetric
     !! positive definite matrix A.
     !! The factorization has the form
     !! A = U**T * U,  if UPLO = 'U', or
     !! A = L  * L**T,  if UPLO = 'L',
     !! where U is an upper triangular matrix and L is lower triangular.
     !! This is the block version of the algorithm, calling Level 3 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo
           integer(ilp), intent(in) :: n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           real(dp), intent(inout) :: a(0_ilp:*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lower, nisodd, normaltransr
           integer(ilp) :: n1, n2, k
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'T' ) ) then
              info = -1_ilp
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DPFTRF', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! if n is odd, set nisodd = .true.
           ! if n is even, set k = n/2 and nisodd = .false.
           if( mod( n, 2_ilp )==0_ilp ) then
              k = n / 2_ilp
              nisodd = .false.
           else
              nisodd = .true.
           end if
           ! set n1 and n2 depending on lower
           if( lower ) then
              n2 = n / 2_ilp
              n1 = n - n2
           else
              n1 = n / 2_ilp
              n2 = n - n1
           end if
           ! start execution: there are eight cases
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                   ! srpa for lower, normal and n is odd ( a(0:n-1,0:n1-1) )
                   ! t1 -> a(0,0), t2 -> a(0,1), s -> a(n1,0)
                   ! t1 -> a(0), t2 -> a(n), s -> a(n1)
                    call stdlib_dpotrf( 'L', n1, a( 0_ilp ), n, info )
                    if( info>0 )return
                    call stdlib_dtrsm( 'R', 'L', 'T', 'N', n2, n1, one, a( 0_ilp ), n,a( n1 ), n )
                              
                    call stdlib_dsyrk( 'U', 'N', n2, n1, -one, a( n1 ), n, one,a( n ), n )
                    call stdlib_dpotrf( 'U', n2, a( n ), n, info )
                    if( info>0_ilp )info = info + n1
                 else
                   ! srpa for upper, normal and n is odd ( a(0:n-1,0:n2-1)
                   ! t1 -> a(n1+1,0), t2 -> a(n1,0), s -> a(0,0)
                   ! t1 -> a(n2), t2 -> a(n1), s -> a(0)
                    call stdlib_dpotrf( 'L', n1, a( n2 ), n, info )
                    if( info>0 )return
                    call stdlib_dtrsm( 'L', 'L', 'N', 'N', n1, n2, one, a( n2 ), n,a( 0_ilp ), n )
                              
                    call stdlib_dsyrk( 'U', 'T', n2, n1, -one, a( 0_ilp ), n, one,a( n1 ), n )
                    call stdlib_dpotrf( 'U', n2, a( n1 ), n, info )
                    if( info>0_ilp )info = info + n1
                 end if
              else
                 ! n is odd and transr = 't'
                 if( lower ) then
                    ! srpa for lower, transpose and n is odd
                    ! t1 -> a(0,0) , t2 -> a(1,0) , s -> a(0,n1)
                    ! t1 -> a(0+0) , t2 -> a(1+0) , s -> a(0+n1*n1); lda=n1
                    call stdlib_dpotrf( 'U', n1, a( 0_ilp ), n1, info )
                    if( info>0 )return
                    call stdlib_dtrsm( 'L', 'U', 'T', 'N', n1, n2, one, a( 0_ilp ), n1,a( n1*n1 ), n1 &
                              )
                    call stdlib_dsyrk( 'L', 'T', n2, n1, -one, a( n1*n1 ), n1, one,a( 1_ilp ), n1 )
                              
                    call stdlib_dpotrf( 'L', n2, a( 1_ilp ), n1, info )
                    if( info>0_ilp )info = info + n1
                 else
                    ! srpa for upper, transpose and n is odd
                    ! t1 -> a(0,n1+1), t2 -> a(0,n1), s -> a(0,0)
                    ! t1 -> a(n2*n2), t2 -> a(n1*n2), s -> a(0); lda = n2
                    call stdlib_dpotrf( 'U', n1, a( n2*n2 ), n2, info )
                    if( info>0 )return
                    call stdlib_dtrsm( 'R', 'U', 'N', 'N', n2, n1, one, a( n2*n2 ),n2, a( 0_ilp ), n2 &
                              )
                    call stdlib_dsyrk( 'L', 'N', n2, n1, -one, a( 0_ilp ), n2, one,a( n1*n2 ), n2 )
                              
                    call stdlib_dpotrf( 'L', n2, a( n1*n2 ), n2, info )
                    if( info>0_ilp )info = info + n1
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! srpa for lower, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(1,0), t2 -> a(0,0), s -> a(k+1,0)
                    ! t1 -> a(1), t2 -> a(0), s -> a(k+1)
                    call stdlib_dpotrf( 'L', k, a( 1_ilp ), n+1, info )
                    if( info>0 )return
                    call stdlib_dtrsm( 'R', 'L', 'T', 'N', k, k, one, a( 1_ilp ), n+1,a( k+1 ), n+1 )
                              
                    call stdlib_dsyrk( 'U', 'N', k, k, -one, a( k+1 ), n+1, one,a( 0_ilp ), n+1 )
                              
                    call stdlib_dpotrf( 'U', k, a( 0_ilp ), n+1, info )
                    if( info>0_ilp )info = info + k
                 else
                    ! srpa for upper, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(k+1,0) ,  t2 -> a(k,0),   s -> a(0,0)
                    ! t1 -> a(k+1), t2 -> a(k), s -> a(0)
                    call stdlib_dpotrf( 'L', k, a( k+1 ), n+1, info )
                    if( info>0 )return
                    call stdlib_dtrsm( 'L', 'L', 'N', 'N', k, k, one, a( k+1 ),n+1, a( 0_ilp ), n+1 )
                              
                    call stdlib_dsyrk( 'U', 'T', k, k, -one, a( 0_ilp ), n+1, one,a( k ), n+1 )
                              
                    call stdlib_dpotrf( 'U', k, a( k ), n+1, info )
                    if( info>0_ilp )info = info + k
                 end if
              else
                 ! n is even and transr = 't'
                 if( lower ) then
                    ! srpa for lower, transpose and n is even (see paper)
                    ! t1 -> b(0,1), t2 -> b(0,0), s -> b(0,k+1)
                    ! t1 -> a(0+k), t2 -> a(0+0), s -> a(0+k*(k+1)); lda=k
                    call stdlib_dpotrf( 'U', k, a( 0_ilp+k ), k, info )
                    if( info>0 )return
                    call stdlib_dtrsm( 'L', 'U', 'T', 'N', k, k, one, a( k ), n1,a( k*( k+1 ) ), &
                              k )
                    call stdlib_dsyrk( 'L', 'T', k, k, -one, a( k*( k+1 ) ), k, one,a( 0_ilp ), k )
                              
                    call stdlib_dpotrf( 'L', k, a( 0_ilp ), k, info )
                    if( info>0_ilp )info = info + k
                 else
                    ! srpa for upper, transpose and n is even (see paper)
                    ! t1 -> b(0,k+1),     t2 -> b(0,k),   s -> b(0,0)
                    ! t1 -> a(0+k*(k+1)), t2 -> a(0+k*k), s -> a(0+0)); lda=k
                    call stdlib_dpotrf( 'U', k, a( k*( k+1 ) ), k, info )
                    if( info>0 )return
                    call stdlib_dtrsm( 'R', 'U', 'N', 'N', k, k, one,a( k*( k+1 ) ), k, a( 0_ilp ), k &
                              )
                    call stdlib_dsyrk( 'L', 'N', k, k, -one, a( 0_ilp ), k, one,a( k*k ), k )
                    call stdlib_dpotrf( 'L', k, a( k*k ), k, info )
                    if( info>0_ilp )info = info + k
                 end if
              end if
           end if
           return
     end subroutine stdlib_dpftrf


     pure module subroutine stdlib_cpftrf( transr, uplo, n, a, info )
     !! CPFTRF computes the Cholesky factorization of a complex Hermitian
     !! positive definite matrix A.
     !! The factorization has the form
     !! A = U**H * U,  if UPLO = 'U', or
     !! A = L  * L**H,  if UPLO = 'L',
     !! where U is an upper triangular matrix and L is lower triangular.
     !! This is the block version of the algorithm, calling Level 3 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo
           integer(ilp), intent(in) :: n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           complex(sp), intent(inout) :: a(0_ilp:*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: lower, nisodd, normaltransr
           integer(ilp) :: n1, n2, k
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'C' ) ) then
              info = -1_ilp
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CPFTRF', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! if n is odd, set nisodd = .true.
           ! if n is even, set k = n/2 and nisodd = .false.
           if( mod( n, 2_ilp )==0_ilp ) then
              k = n / 2_ilp
              nisodd = .false.
           else
              nisodd = .true.
           end if
           ! set n1 and n2 depending on lower
           if( lower ) then
              n2 = n / 2_ilp
              n1 = n - n2
           else
              n1 = n / 2_ilp
              n2 = n - n1
           end if
           ! start execution: there are eight cases
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                   ! srpa for lower, normal and n is odd ( a(0:n-1,0:n1-1) )
                   ! t1 -> a(0,0), t2 -> a(0,1), s -> a(n1,0)
                   ! t1 -> a(0), t2 -> a(n), s -> a(n1)
                    call stdlib_cpotrf( 'L', n1, a( 0_ilp ), n, info )
                    if( info>0 )return
                    call stdlib_ctrsm( 'R', 'L', 'C', 'N', n2, n1, cone, a( 0_ilp ), n,a( n1 ), n )
                              
                    call stdlib_cherk( 'U', 'N', n2, n1, -one, a( n1 ), n, one,a( n ), n )
                    call stdlib_cpotrf( 'U', n2, a( n ), n, info )
                    if( info>0_ilp )info = info + n1
                 else
                   ! srpa for upper, normal and n is odd ( a(0:n-1,0:n2-1)
                   ! t1 -> a(n1+1,0), t2 -> a(n1,0), s -> a(0,0)
                   ! t1 -> a(n2), t2 -> a(n1), s -> a(0)
                    call stdlib_cpotrf( 'L', n1, a( n2 ), n, info )
                    if( info>0 )return
                    call stdlib_ctrsm( 'L', 'L', 'N', 'N', n1, n2, cone, a( n2 ), n,a( 0_ilp ), n )
                              
                    call stdlib_cherk( 'U', 'C', n2, n1, -one, a( 0_ilp ), n, one,a( n1 ), n )
                    call stdlib_cpotrf( 'U', n2, a( n1 ), n, info )
                    if( info>0_ilp )info = info + n1
                 end if
              else
                 ! n is odd and transr = 'c'
                 if( lower ) then
                    ! srpa for lower, transpose and n is odd
                    ! t1 -> a(0,0) , t2 -> a(1,0) , s -> a(0,n1)
                    ! t1 -> a(0+0) , t2 -> a(1+0) , s -> a(0+n1*n1); lda=n1
                    call stdlib_cpotrf( 'U', n1, a( 0_ilp ), n1, info )
                    if( info>0 )return
                    call stdlib_ctrsm( 'L', 'U', 'C', 'N', n1, n2, cone, a( 0_ilp ), n1,a( n1*n1 ), &
                              n1 )
                    call stdlib_cherk( 'L', 'C', n2, n1, -one, a( n1*n1 ), n1, one,a( 1_ilp ), n1 )
                              
                    call stdlib_cpotrf( 'L', n2, a( 1_ilp ), n1, info )
                    if( info>0_ilp )info = info + n1
                 else
                    ! srpa for upper, transpose and n is odd
                    ! t1 -> a(0,n1+1), t2 -> a(0,n1), s -> a(0,0)
                    ! t1 -> a(n2*n2), t2 -> a(n1*n2), s -> a(0); lda = n2
                    call stdlib_cpotrf( 'U', n1, a( n2*n2 ), n2, info )
                    if( info>0 )return
                    call stdlib_ctrsm( 'R', 'U', 'N', 'N', n2, n1, cone, a( n2*n2 ),n2, a( 0_ilp ), &
                              n2 )
                    call stdlib_cherk( 'L', 'N', n2, n1, -one, a( 0_ilp ), n2, one,a( n1*n2 ), n2 )
                              
                    call stdlib_cpotrf( 'L', n2, a( n1*n2 ), n2, info )
                    if( info>0_ilp )info = info + n1
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! srpa for lower, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(1,0), t2 -> a(0,0), s -> a(k+1,0)
                    ! t1 -> a(1), t2 -> a(0), s -> a(k+1)
                    call stdlib_cpotrf( 'L', k, a( 1_ilp ), n+1, info )
                    if( info>0 )return
                    call stdlib_ctrsm( 'R', 'L', 'C', 'N', k, k, cone, a( 1_ilp ), n+1,a( k+1 ), n+1 )
                              
                    call stdlib_cherk( 'U', 'N', k, k, -one, a( k+1 ), n+1, one,a( 0_ilp ), n+1 )
                              
                    call stdlib_cpotrf( 'U', k, a( 0_ilp ), n+1, info )
                    if( info>0_ilp )info = info + k
                 else
                    ! srpa for upper, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(k+1,0) ,  t2 -> a(k,0),   s -> a(0,0)
                    ! t1 -> a(k+1), t2 -> a(k), s -> a(0)
                    call stdlib_cpotrf( 'L', k, a( k+1 ), n+1, info )
                    if( info>0 )return
                    call stdlib_ctrsm( 'L', 'L', 'N', 'N', k, k, cone, a( k+1 ),n+1, a( 0_ilp ), n+1 )
                              
                    call stdlib_cherk( 'U', 'C', k, k, -one, a( 0_ilp ), n+1, one,a( k ), n+1 )
                              
                    call stdlib_cpotrf( 'U', k, a( k ), n+1, info )
                    if( info>0_ilp )info = info + k
                 end if
              else
                 ! n is even and transr = 'c'
                 if( lower ) then
                    ! srpa for lower, transpose and n is even (see paper)
                    ! t1 -> b(0,1), t2 -> b(0,0), s -> b(0,k+1)
                    ! t1 -> a(0+k), t2 -> a(0+0), s -> a(0+k*(k+1)); lda=k
                    call stdlib_cpotrf( 'U', k, a( 0_ilp+k ), k, info )
                    if( info>0 )return
                    call stdlib_ctrsm( 'L', 'U', 'C', 'N', k, k, cone, a( k ), n1,a( k*( k+1 ) ), &
                              k )
                    call stdlib_cherk( 'L', 'C', k, k, -one, a( k*( k+1 ) ), k, one,a( 0_ilp ), k )
                              
                    call stdlib_cpotrf( 'L', k, a( 0_ilp ), k, info )
                    if( info>0_ilp )info = info + k
                 else
                    ! srpa for upper, transpose and n is even (see paper)
                    ! t1 -> b(0,k+1),     t2 -> b(0,k),   s -> b(0,0)
                    ! t1 -> a(0+k*(k+1)), t2 -> a(0+k*k), s -> a(0+0)); lda=k
                    call stdlib_cpotrf( 'U', k, a( k*( k+1 ) ), k, info )
                    if( info>0 )return
                    call stdlib_ctrsm( 'R', 'U', 'N', 'N', k, k, cone,a( k*( k+1 ) ), k, a( 0_ilp ), &
                              k )
                    call stdlib_cherk( 'L', 'N', k, k, -one, a( 0_ilp ), k, one,a( k*k ), k )
                    call stdlib_cpotrf( 'L', k, a( k*k ), k, info )
                    if( info>0_ilp )info = info + k
                 end if
              end if
           end if
           return
     end subroutine stdlib_cpftrf

     pure module subroutine stdlib_zpftrf( transr, uplo, n, a, info )
     !! ZPFTRF computes the Cholesky factorization of a complex Hermitian
     !! positive definite matrix A.
     !! The factorization has the form
     !! A = U**H * U,  if UPLO = 'U', or
     !! A = L  * L**H,  if UPLO = 'L',
     !! where U is an upper triangular matrix and L is lower triangular.
     !! This is the block version of the algorithm, calling Level 3 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo
           integer(ilp), intent(in) :: n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           complex(dp), intent(inout) :: a(0_ilp:*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: lower, nisodd, normaltransr
           integer(ilp) :: n1, n2, k
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'C' ) ) then
              info = -1_ilp
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZPFTRF', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! if n is odd, set nisodd = .true.
           ! if n is even, set k = n/2 and nisodd = .false.
           if( mod( n, 2_ilp )==0_ilp ) then
              k = n / 2_ilp
              nisodd = .false.
           else
              nisodd = .true.
           end if
           ! set n1 and n2 depending on lower
           if( lower ) then
              n2 = n / 2_ilp
              n1 = n - n2
           else
              n1 = n / 2_ilp
              n2 = n - n1
           end if
           ! start execution: there are eight cases
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                   ! srpa for lower, normal and n is odd ( a(0:n-1,0:n1-1) )
                   ! t1 -> a(0,0), t2 -> a(0,1), s -> a(n1,0)
                   ! t1 -> a(0), t2 -> a(n), s -> a(n1)
                    call stdlib_zpotrf( 'L', n1, a( 0_ilp ), n, info )
                    if( info>0 )return
                    call stdlib_ztrsm( 'R', 'L', 'C', 'N', n2, n1, cone, a( 0_ilp ), n,a( n1 ), n )
                              
                    call stdlib_zherk( 'U', 'N', n2, n1, -one, a( n1 ), n, one,a( n ), n )
                    call stdlib_zpotrf( 'U', n2, a( n ), n, info )
                    if( info>0_ilp )info = info + n1
                 else
                   ! srpa for upper, normal and n is odd ( a(0:n-1,0:n2-1)
                   ! t1 -> a(n1+1,0), t2 -> a(n1,0), s -> a(0,0)
                   ! t1 -> a(n2), t2 -> a(n1), s -> a(0)
                    call stdlib_zpotrf( 'L', n1, a( n2 ), n, info )
                    if( info>0 )return
                    call stdlib_ztrsm( 'L', 'L', 'N', 'N', n1, n2, cone, a( n2 ), n,a( 0_ilp ), n )
                              
                    call stdlib_zherk( 'U', 'C', n2, n1, -one, a( 0_ilp ), n, one,a( n1 ), n )
                    call stdlib_zpotrf( 'U', n2, a( n1 ), n, info )
                    if( info>0_ilp )info = info + n1
                 end if
              else
                 ! n is odd and transr = 'c'
                 if( lower ) then
                    ! srpa for lower, transpose and n is odd
                    ! t1 -> a(0,0) , t2 -> a(1,0) , s -> a(0,n1)
                    ! t1 -> a(0+0) , t2 -> a(1+0) , s -> a(0+n1*n1); lda=n1
                    call stdlib_zpotrf( 'U', n1, a( 0_ilp ), n1, info )
                    if( info>0 )return
                    call stdlib_ztrsm( 'L', 'U', 'C', 'N', n1, n2, cone, a( 0_ilp ), n1,a( n1*n1 ), &
                              n1 )
                    call stdlib_zherk( 'L', 'C', n2, n1, -one, a( n1*n1 ), n1, one,a( 1_ilp ), n1 )
                              
                    call stdlib_zpotrf( 'L', n2, a( 1_ilp ), n1, info )
                    if( info>0_ilp )info = info + n1
                 else
                    ! srpa for upper, transpose and n is odd
                    ! t1 -> a(0,n1+1), t2 -> a(0,n1), s -> a(0,0)
                    ! t1 -> a(n2*n2), t2 -> a(n1*n2), s -> a(0); lda = n2
                    call stdlib_zpotrf( 'U', n1, a( n2*n2 ), n2, info )
                    if( info>0 )return
                    call stdlib_ztrsm( 'R', 'U', 'N', 'N', n2, n1, cone, a( n2*n2 ),n2, a( 0_ilp ), &
                              n2 )
                    call stdlib_zherk( 'L', 'N', n2, n1, -one, a( 0_ilp ), n2, one,a( n1*n2 ), n2 )
                              
                    call stdlib_zpotrf( 'L', n2, a( n1*n2 ), n2, info )
                    if( info>0_ilp )info = info + n1
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! srpa for lower, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(1,0), t2 -> a(0,0), s -> a(k+1,0)
                    ! t1 -> a(1), t2 -> a(0), s -> a(k+1)
                    call stdlib_zpotrf( 'L', k, a( 1_ilp ), n+1, info )
                    if( info>0 )return
                    call stdlib_ztrsm( 'R', 'L', 'C', 'N', k, k, cone, a( 1_ilp ), n+1,a( k+1 ), n+1 )
                              
                    call stdlib_zherk( 'U', 'N', k, k, -one, a( k+1 ), n+1, one,a( 0_ilp ), n+1 )
                              
                    call stdlib_zpotrf( 'U', k, a( 0_ilp ), n+1, info )
                    if( info>0_ilp )info = info + k
                 else
                    ! srpa for upper, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(k+1,0) ,  t2 -> a(k,0),   s -> a(0,0)
                    ! t1 -> a(k+1), t2 -> a(k), s -> a(0)
                    call stdlib_zpotrf( 'L', k, a( k+1 ), n+1, info )
                    if( info>0 )return
                    call stdlib_ztrsm( 'L', 'L', 'N', 'N', k, k, cone, a( k+1 ),n+1, a( 0_ilp ), n+1 )
                              
                    call stdlib_zherk( 'U', 'C', k, k, -one, a( 0_ilp ), n+1, one,a( k ), n+1 )
                              
                    call stdlib_zpotrf( 'U', k, a( k ), n+1, info )
                    if( info>0_ilp )info = info + k
                 end if
              else
                 ! n is even and transr = 'c'
                 if( lower ) then
                    ! srpa for lower, transpose and n is even (see paper)
                    ! t1 -> b(0,1), t2 -> b(0,0), s -> b(0,k+1)
                    ! t1 -> a(0+k), t2 -> a(0+0), s -> a(0+k*(k+1)); lda=k
                    call stdlib_zpotrf( 'U', k, a( 0_ilp+k ), k, info )
                    if( info>0 )return
                    call stdlib_ztrsm( 'L', 'U', 'C', 'N', k, k, cone, a( k ), n1,a( k*( k+1 ) ), &
                              k )
                    call stdlib_zherk( 'L', 'C', k, k, -one, a( k*( k+1 ) ), k, one,a( 0_ilp ), k )
                              
                    call stdlib_zpotrf( 'L', k, a( 0_ilp ), k, info )
                    if( info>0_ilp )info = info + k
                 else
                    ! srpa for upper, transpose and n is even (see paper)
                    ! t1 -> b(0,k+1),     t2 -> b(0,k),   s -> b(0,0)
                    ! t1 -> a(0+k*(k+1)), t2 -> a(0+k*k), s -> a(0+0)); lda=k
                    call stdlib_zpotrf( 'U', k, a( k*( k+1 ) ), k, info )
                    if( info>0 )return
                    call stdlib_ztrsm( 'R', 'U', 'N', 'N', k, k, cone,a( k*( k+1 ) ), k, a( 0_ilp ), &
                              k )
                    call stdlib_zherk( 'L', 'N', k, k, -one, a( 0_ilp ), k, one,a( k*k ), k )
                    call stdlib_zpotrf( 'L', k, a( k*k ), k, info )
                    if( info>0_ilp )info = info + k
                 end if
              end if
           end if
           return
     end subroutine stdlib_zpftrf




     pure module subroutine stdlib_spftrs( transr, uplo, n, nrhs, a, b, ldb, info )
     !! SPFTRS solves a system of linear equations A*X = B with a symmetric
     !! positive definite matrix A using the Cholesky factorization
     !! A = U**T*U or A = L*L**T computed by SPFTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           real(sp), intent(in) :: a(0_ilp:*)
           real(sp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lower, normaltransr
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'T' ) ) then
              info = -1_ilp
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SPFTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           ! start execution: there are two triangular solves
           if( lower ) then
              call stdlib_stfsm( transr, 'L', uplo, 'N', 'N', n, nrhs, one, a, b,ldb )
              call stdlib_stfsm( transr, 'L', uplo, 'T', 'N', n, nrhs, one, a, b,ldb )
           else
              call stdlib_stfsm( transr, 'L', uplo, 'T', 'N', n, nrhs, one, a, b,ldb )
              call stdlib_stfsm( transr, 'L', uplo, 'N', 'N', n, nrhs, one, a, b,ldb )
           end if
           return
     end subroutine stdlib_spftrs

     pure module subroutine stdlib_dpftrs( transr, uplo, n, nrhs, a, b, ldb, info )
     !! DPFTRS solves a system of linear equations A*X = B with a symmetric
     !! positive definite matrix A using the Cholesky factorization
     !! A = U**T*U or A = L*L**T computed by DPFTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           real(dp), intent(in) :: a(0_ilp:*)
           real(dp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lower, normaltransr
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'T' ) ) then
              info = -1_ilp
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DPFTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           ! start execution: there are two triangular solves
           if( lower ) then
              call stdlib_dtfsm( transr, 'L', uplo, 'N', 'N', n, nrhs, one, a, b,ldb )
              call stdlib_dtfsm( transr, 'L', uplo, 'T', 'N', n, nrhs, one, a, b,ldb )
           else
              call stdlib_dtfsm( transr, 'L', uplo, 'T', 'N', n, nrhs, one, a, b,ldb )
              call stdlib_dtfsm( transr, 'L', uplo, 'N', 'N', n, nrhs, one, a, b,ldb )
           end if
           return
     end subroutine stdlib_dpftrs


     pure module subroutine stdlib_cpftrs( transr, uplo, n, nrhs, a, b, ldb, info )
     !! CPFTRS solves a system of linear equations A*X = B with a Hermitian
     !! positive definite matrix A using the Cholesky factorization
     !! A = U**H*U or A = L*L**H computed by CPFTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           complex(sp), intent(in) :: a(0_ilp:*)
           complex(sp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lower, normaltransr
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'C' ) ) then
              info = -1_ilp
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CPFTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           ! start execution: there are two triangular solves
           if( lower ) then
              call stdlib_ctfsm( transr, 'L', uplo, 'N', 'N', n, nrhs, cone, a, b,ldb )
              call stdlib_ctfsm( transr, 'L', uplo, 'C', 'N', n, nrhs, cone, a, b,ldb )
           else
              call stdlib_ctfsm( transr, 'L', uplo, 'C', 'N', n, nrhs, cone, a, b,ldb )
              call stdlib_ctfsm( transr, 'L', uplo, 'N', 'N', n, nrhs, cone, a, b,ldb )
           end if
           return
     end subroutine stdlib_cpftrs

     pure module subroutine stdlib_zpftrs( transr, uplo, n, nrhs, a, b, ldb, info )
     !! ZPFTRS solves a system of linear equations A*X = B with a Hermitian
     !! positive definite matrix A using the Cholesky factorization
     !! A = U**H*U or A = L*L**H computed by ZPFTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           complex(dp), intent(in) :: a(0_ilp:*)
           complex(dp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lower, normaltransr
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'C' ) ) then
              info = -1_ilp
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZPFTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           ! start execution: there are two triangular solves
           if( lower ) then
              call stdlib_ztfsm( transr, 'L', uplo, 'N', 'N', n, nrhs, cone, a, b,ldb )
              call stdlib_ztfsm( transr, 'L', uplo, 'C', 'N', n, nrhs, cone, a, b,ldb )
           else
              call stdlib_ztfsm( transr, 'L', uplo, 'C', 'N', n, nrhs, cone, a, b,ldb )
              call stdlib_ztfsm( transr, 'L', uplo, 'N', 'N', n, nrhs, cone, a, b,ldb )
           end if
           return
     end subroutine stdlib_zpftrs




     pure module subroutine stdlib_spftri( transr, uplo, n, a, info )
     !! SPFTRI computes the inverse of a real (symmetric) positive definite
     !! matrix A using the Cholesky factorization A = U**T*U or A = L*L**T
     !! computed by SPFTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(sp), intent(inout) :: a(0_ilp:*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lower, nisodd, normaltransr
           integer(ilp) :: n1, n2, k
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'T' ) ) then
              info = -1_ilp
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SPFTRI', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! invert the triangular cholesky factor u or l.
           call stdlib_stftri( transr, uplo, 'N', n, a, info )
           if( info>0 )return
           ! if n is odd, set nisodd = .true.
           ! if n is even, set k = n/2 and nisodd = .false.
           if( mod( n, 2_ilp )==0_ilp ) then
              k = n / 2_ilp
              nisodd = .false.
           else
              nisodd = .true.
           end if
           ! set n1 and n2 depending on lower
           if( lower ) then
              n2 = n / 2_ilp
              n1 = n - n2
           else
              n1 = n / 2_ilp
              n2 = n - n1
           end if
           ! start execution of triangular matrix multiply: inv(u)*inv(u)^c or
           ! inv(l)^c*inv(l). there are eight cases.
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                    ! srpa for lower, normal and n is odd ( a(0:n-1,0:n1-1) )
                    ! t1 -> a(0,0), t2 -> a(0,1), s -> a(n1,0)
                    ! t1 -> a(0), t2 -> a(n), s -> a(n1)
                    call stdlib_slauum( 'L', n1, a( 0_ilp ), n, info )
                    call stdlib_ssyrk( 'L', 'T', n1, n2, one, a( n1 ), n, one,a( 0_ilp ), n )
                    call stdlib_strmm( 'L', 'U', 'N', 'N', n2, n1, one, a( n ), n,a( n1 ), n )
                              
                    call stdlib_slauum( 'U', n2, a( n ), n, info )
                 else
                    ! srpa for upper, normal and n is odd ( a(0:n-1,0:n2-1)
                    ! t1 -> a(n1+1,0), t2 -> a(n1,0), s -> a(0,0)
                    ! t1 -> a(n2), t2 -> a(n1), s -> a(0)
                    call stdlib_slauum( 'L', n1, a( n2 ), n, info )
                    call stdlib_ssyrk( 'L', 'N', n1, n2, one, a( 0_ilp ), n, one,a( n2 ), n )
                    call stdlib_strmm( 'R', 'U', 'T', 'N', n1, n2, one, a( n1 ), n,a( 0_ilp ), n )
                              
                    call stdlib_slauum( 'U', n2, a( n1 ), n, info )
                 end if
              else
                 ! n is odd and transr = 't'
                 if( lower ) then
                    ! srpa for lower, transpose, and n is odd
                    ! t1 -> a(0), t2 -> a(1), s -> a(0+n1*n1)
                    call stdlib_slauum( 'U', n1, a( 0_ilp ), n1, info )
                    call stdlib_ssyrk( 'U', 'N', n1, n2, one, a( n1*n1 ), n1, one,a( 0_ilp ), n1 )
                              
                    call stdlib_strmm( 'R', 'L', 'N', 'N', n1, n2, one, a( 1_ilp ), n1,a( n1*n1 ), n1 &
                              )
                    call stdlib_slauum( 'L', n2, a( 1_ilp ), n1, info )
                 else
                    ! srpa for upper, transpose, and n is odd
                    ! t1 -> a(0+n2*n2), t2 -> a(0+n1*n2), s -> a(0)
                    call stdlib_slauum( 'U', n1, a( n2*n2 ), n2, info )
                    call stdlib_ssyrk( 'U', 'T', n1, n2, one, a( 0_ilp ), n2, one,a( n2*n2 ), n2 )
                              
                    call stdlib_strmm( 'L', 'L', 'T', 'N', n2, n1, one, a( n1*n2 ),n2, a( 0_ilp ), n2 &
                              )
                    call stdlib_slauum( 'L', n2, a( n1*n2 ), n2, info )
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! srpa for lower, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(1,0), t2 -> a(0,0), s -> a(k+1,0)
                    ! t1 -> a(1), t2 -> a(0), s -> a(k+1)
                    call stdlib_slauum( 'L', k, a( 1_ilp ), n+1, info )
                    call stdlib_ssyrk( 'L', 'T', k, k, one, a( k+1 ), n+1, one,a( 1_ilp ), n+1 )
                              
                    call stdlib_strmm( 'L', 'U', 'N', 'N', k, k, one, a( 0_ilp ), n+1,a( k+1 ), n+1 )
                              
                    call stdlib_slauum( 'U', k, a( 0_ilp ), n+1, info )
                 else
                    ! srpa for upper, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(k+1,0) ,  t2 -> a(k,0),   s -> a(0,0)
                    ! t1 -> a(k+1), t2 -> a(k), s -> a(0)
                    call stdlib_slauum( 'L', k, a( k+1 ), n+1, info )
                    call stdlib_ssyrk( 'L', 'N', k, k, one, a( 0_ilp ), n+1, one,a( k+1 ), n+1 )
                              
                    call stdlib_strmm( 'R', 'U', 'T', 'N', k, k, one, a( k ), n+1,a( 0_ilp ), n+1 )
                              
                    call stdlib_slauum( 'U', k, a( k ), n+1, info )
                 end if
              else
                 ! n is even and transr = 't'
                 if( lower ) then
                    ! srpa for lower, transpose, and n is even (see paper)
                    ! t1 -> b(0,1), t2 -> b(0,0), s -> b(0,k+1),
                    ! t1 -> a(0+k), t2 -> a(0+0), s -> a(0+k*(k+1)); lda=k
                    call stdlib_slauum( 'U', k, a( k ), k, info )
                    call stdlib_ssyrk( 'U', 'N', k, k, one, a( k*( k+1 ) ), k, one,a( k ), k )
                              
                    call stdlib_strmm( 'R', 'L', 'N', 'N', k, k, one, a( 0_ilp ), k,a( k*( k+1 ) ), k &
                              )
                    call stdlib_slauum( 'L', k, a( 0_ilp ), k, info )
                 else
                    ! srpa for upper, transpose, and n is even (see paper)
                    ! t1 -> b(0,k+1),     t2 -> b(0,k),   s -> b(0,0),
                    ! t1 -> a(0+k*(k+1)), t2 -> a(0+k*k), s -> a(0+0)); lda=k
                    call stdlib_slauum( 'U', k, a( k*( k+1 ) ), k, info )
                    call stdlib_ssyrk( 'U', 'T', k, k, one, a( 0_ilp ), k, one,a( k*( k+1 ) ), k )
                              
                    call stdlib_strmm( 'L', 'L', 'T', 'N', k, k, one, a( k*k ), k,a( 0_ilp ), k )
                              
                    call stdlib_slauum( 'L', k, a( k*k ), k, info )
                 end if
              end if
           end if
           return
     end subroutine stdlib_spftri

     pure module subroutine stdlib_dpftri( transr, uplo, n, a, info )
     !! DPFTRI computes the inverse of a (real) symmetric positive definite
     !! matrix A using the Cholesky factorization A = U**T*U or A = L*L**T
     !! computed by DPFTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(dp), intent(inout) :: a(0_ilp:*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lower, nisodd, normaltransr
           integer(ilp) :: n1, n2, k
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'T' ) ) then
              info = -1_ilp
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DPFTRI', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! invert the triangular cholesky factor u or l.
           call stdlib_dtftri( transr, uplo, 'N', n, a, info )
           if( info>0 )return
           ! if n is odd, set nisodd = .true.
           ! if n is even, set k = n/2 and nisodd = .false.
           if( mod( n, 2_ilp )==0_ilp ) then
              k = n / 2_ilp
              nisodd = .false.
           else
              nisodd = .true.
           end if
           ! set n1 and n2 depending on lower
           if( lower ) then
              n2 = n / 2_ilp
              n1 = n - n2
           else
              n1 = n / 2_ilp
              n2 = n - n1
           end if
           ! start execution of triangular matrix multiply: inv(u)*inv(u)^c or
           ! inv(l)^c*inv(l). there are eight cases.
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                    ! srpa for lower, normal and n is odd ( a(0:n-1,0:n1-1) )
                    ! t1 -> a(0,0), t2 -> a(0,1), s -> a(n1,0)
                    ! t1 -> a(0), t2 -> a(n), s -> a(n1)
                    call stdlib_dlauum( 'L', n1, a( 0_ilp ), n, info )
                    call stdlib_dsyrk( 'L', 'T', n1, n2, one, a( n1 ), n, one,a( 0_ilp ), n )
                    call stdlib_dtrmm( 'L', 'U', 'N', 'N', n2, n1, one, a( n ), n,a( n1 ), n )
                              
                    call stdlib_dlauum( 'U', n2, a( n ), n, info )
                 else
                    ! srpa for upper, normal and n is odd ( a(0:n-1,0:n2-1)
                    ! t1 -> a(n1+1,0), t2 -> a(n1,0), s -> a(0,0)
                    ! t1 -> a(n2), t2 -> a(n1), s -> a(0)
                    call stdlib_dlauum( 'L', n1, a( n2 ), n, info )
                    call stdlib_dsyrk( 'L', 'N', n1, n2, one, a( 0_ilp ), n, one,a( n2 ), n )
                    call stdlib_dtrmm( 'R', 'U', 'T', 'N', n1, n2, one, a( n1 ), n,a( 0_ilp ), n )
                              
                    call stdlib_dlauum( 'U', n2, a( n1 ), n, info )
                 end if
              else
                 ! n is odd and transr = 't'
                 if( lower ) then
                    ! srpa for lower, transpose, and n is odd
                    ! t1 -> a(0), t2 -> a(1), s -> a(0+n1*n1)
                    call stdlib_dlauum( 'U', n1, a( 0_ilp ), n1, info )
                    call stdlib_dsyrk( 'U', 'N', n1, n2, one, a( n1*n1 ), n1, one,a( 0_ilp ), n1 )
                              
                    call stdlib_dtrmm( 'R', 'L', 'N', 'N', n1, n2, one, a( 1_ilp ), n1,a( n1*n1 ), n1 &
                              )
                    call stdlib_dlauum( 'L', n2, a( 1_ilp ), n1, info )
                 else
                    ! srpa for upper, transpose, and n is odd
                    ! t1 -> a(0+n2*n2), t2 -> a(0+n1*n2), s -> a(0)
                    call stdlib_dlauum( 'U', n1, a( n2*n2 ), n2, info )
                    call stdlib_dsyrk( 'U', 'T', n1, n2, one, a( 0_ilp ), n2, one,a( n2*n2 ), n2 )
                              
                    call stdlib_dtrmm( 'L', 'L', 'T', 'N', n2, n1, one, a( n1*n2 ),n2, a( 0_ilp ), n2 &
                              )
                    call stdlib_dlauum( 'L', n2, a( n1*n2 ), n2, info )
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! srpa for lower, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(1,0), t2 -> a(0,0), s -> a(k+1,0)
                    ! t1 -> a(1), t2 -> a(0), s -> a(k+1)
                    call stdlib_dlauum( 'L', k, a( 1_ilp ), n+1, info )
                    call stdlib_dsyrk( 'L', 'T', k, k, one, a( k+1 ), n+1, one,a( 1_ilp ), n+1 )
                              
                    call stdlib_dtrmm( 'L', 'U', 'N', 'N', k, k, one, a( 0_ilp ), n+1,a( k+1 ), n+1 )
                              
                    call stdlib_dlauum( 'U', k, a( 0_ilp ), n+1, info )
                 else
                    ! srpa for upper, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(k+1,0) ,  t2 -> a(k,0),   s -> a(0,0)
                    ! t1 -> a(k+1), t2 -> a(k), s -> a(0)
                    call stdlib_dlauum( 'L', k, a( k+1 ), n+1, info )
                    call stdlib_dsyrk( 'L', 'N', k, k, one, a( 0_ilp ), n+1, one,a( k+1 ), n+1 )
                              
                    call stdlib_dtrmm( 'R', 'U', 'T', 'N', k, k, one, a( k ), n+1,a( 0_ilp ), n+1 )
                              
                    call stdlib_dlauum( 'U', k, a( k ), n+1, info )
                 end if
              else
                 ! n is even and transr = 't'
                 if( lower ) then
                    ! srpa for lower, transpose, and n is even (see paper)
                    ! t1 -> b(0,1), t2 -> b(0,0), s -> b(0,k+1),
                    ! t1 -> a(0+k), t2 -> a(0+0), s -> a(0+k*(k+1)); lda=k
                    call stdlib_dlauum( 'U', k, a( k ), k, info )
                    call stdlib_dsyrk( 'U', 'N', k, k, one, a( k*( k+1 ) ), k, one,a( k ), k )
                              
                    call stdlib_dtrmm( 'R', 'L', 'N', 'N', k, k, one, a( 0_ilp ), k,a( k*( k+1 ) ), k &
                              )
                    call stdlib_dlauum( 'L', k, a( 0_ilp ), k, info )
                 else
                    ! srpa for upper, transpose, and n is even (see paper)
                    ! t1 -> b(0,k+1),     t2 -> b(0,k),   s -> b(0,0),
                    ! t1 -> a(0+k*(k+1)), t2 -> a(0+k*k), s -> a(0+0)); lda=k
                    call stdlib_dlauum( 'U', k, a( k*( k+1 ) ), k, info )
                    call stdlib_dsyrk( 'U', 'T', k, k, one, a( 0_ilp ), k, one,a( k*( k+1 ) ), k )
                              
                    call stdlib_dtrmm( 'L', 'L', 'T', 'N', k, k, one, a( k*k ), k,a( 0_ilp ), k )
                              
                    call stdlib_dlauum( 'L', k, a( k*k ), k, info )
                 end if
              end if
           end if
           return
     end subroutine stdlib_dpftri


     pure module subroutine stdlib_cpftri( transr, uplo, n, a, info )
     !! CPFTRI computes the inverse of a complex Hermitian positive definite
     !! matrix A using the Cholesky factorization A = U**H*U or A = L*L**H
     !! computed by CPFTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(0_ilp:*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: lower, nisodd, normaltransr
           integer(ilp) :: n1, n2, k
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'C' ) ) then
              info = -1_ilp
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CPFTRI', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! invert the triangular cholesky factor u or l.
           call stdlib_ctftri( transr, uplo, 'N', n, a, info )
           if( info>0 )return
           ! if n is odd, set nisodd = .true.
           ! if n is even, set k = n/2 and nisodd = .false.
           if( mod( n, 2_ilp )==0_ilp ) then
              k = n / 2_ilp
              nisodd = .false.
           else
              nisodd = .true.
           end if
           ! set n1 and n2 depending on lower
           if( lower ) then
              n2 = n / 2_ilp
              n1 = n - n2
           else
              n1 = n / 2_ilp
              n2 = n - n1
           end if
           ! start execution of triangular matrix multiply: inv(u)*inv(u)^c or
           ! inv(l)^c*inv(l). there are eight cases.
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                    ! srpa for lower, normal and n is odd ( a(0:n-1,0:n1-1) )
                    ! t1 -> a(0,0), t2 -> a(0,1), s -> a(n1,0)
                    ! t1 -> a(0), t2 -> a(n), s -> a(n1)
                    call stdlib_clauum( 'L', n1, a( 0_ilp ), n, info )
                    call stdlib_cherk( 'L', 'C', n1, n2, one, a( n1 ), n, one,a( 0_ilp ), n )
                    call stdlib_ctrmm( 'L', 'U', 'N', 'N', n2, n1, cone, a( n ), n,a( n1 ), n )
                              
                    call stdlib_clauum( 'U', n2, a( n ), n, info )
                 else
                    ! srpa for upper, normal and n is odd ( a(0:n-1,0:n2-1)
                    ! t1 -> a(n1+1,0), t2 -> a(n1,0), s -> a(0,0)
                    ! t1 -> a(n2), t2 -> a(n1), s -> a(0)
                    call stdlib_clauum( 'L', n1, a( n2 ), n, info )
                    call stdlib_cherk( 'L', 'N', n1, n2, one, a( 0_ilp ), n, one,a( n2 ), n )
                    call stdlib_ctrmm( 'R', 'U', 'C', 'N', n1, n2, cone, a( n1 ), n,a( 0_ilp ), n )
                              
                    call stdlib_clauum( 'U', n2, a( n1 ), n, info )
                 end if
              else
                 ! n is odd and transr = 'c'
                 if( lower ) then
                    ! srpa for lower, transpose, and n is odd
                    ! t1 -> a(0), t2 -> a(1), s -> a(0+n1*n1)
                    call stdlib_clauum( 'U', n1, a( 0_ilp ), n1, info )
                    call stdlib_cherk( 'U', 'N', n1, n2, one, a( n1*n1 ), n1, one,a( 0_ilp ), n1 )
                              
                    call stdlib_ctrmm( 'R', 'L', 'N', 'N', n1, n2, cone, a( 1_ilp ), n1,a( n1*n1 ), &
                              n1 )
                    call stdlib_clauum( 'L', n2, a( 1_ilp ), n1, info )
                 else
                    ! srpa for upper, transpose, and n is odd
                    ! t1 -> a(0+n2*n2), t2 -> a(0+n1*n2), s -> a(0)
                    call stdlib_clauum( 'U', n1, a( n2*n2 ), n2, info )
                    call stdlib_cherk( 'U', 'C', n1, n2, one, a( 0_ilp ), n2, one,a( n2*n2 ), n2 )
                              
                    call stdlib_ctrmm( 'L', 'L', 'C', 'N', n2, n1, cone, a( n1*n2 ),n2, a( 0_ilp ), &
                              n2 )
                    call stdlib_clauum( 'L', n2, a( n1*n2 ), n2, info )
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! srpa for lower, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(1,0), t2 -> a(0,0), s -> a(k+1,0)
                    ! t1 -> a(1), t2 -> a(0), s -> a(k+1)
                    call stdlib_clauum( 'L', k, a( 1_ilp ), n+1, info )
                    call stdlib_cherk( 'L', 'C', k, k, one, a( k+1 ), n+1, one,a( 1_ilp ), n+1 )
                              
                    call stdlib_ctrmm( 'L', 'U', 'N', 'N', k, k, cone, a( 0_ilp ), n+1,a( k+1 ), n+1 )
                              
                    call stdlib_clauum( 'U', k, a( 0_ilp ), n+1, info )
                 else
                    ! srpa for upper, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(k+1,0) ,  t2 -> a(k,0),   s -> a(0,0)
                    ! t1 -> a(k+1), t2 -> a(k), s -> a(0)
                    call stdlib_clauum( 'L', k, a( k+1 ), n+1, info )
                    call stdlib_cherk( 'L', 'N', k, k, one, a( 0_ilp ), n+1, one,a( k+1 ), n+1 )
                              
                    call stdlib_ctrmm( 'R', 'U', 'C', 'N', k, k, cone, a( k ), n+1,a( 0_ilp ), n+1 )
                              
                    call stdlib_clauum( 'U', k, a( k ), n+1, info )
                 end if
              else
                 ! n is even and transr = 'c'
                 if( lower ) then
                    ! srpa for lower, transpose, and n is even (see paper)
                    ! t1 -> b(0,1), t2 -> b(0,0), s -> b(0,k+1),
                    ! t1 -> a(0+k), t2 -> a(0+0), s -> a(0+k*(k+1)); lda=k
                    call stdlib_clauum( 'U', k, a( k ), k, info )
                    call stdlib_cherk( 'U', 'N', k, k, one, a( k*( k+1 ) ), k, one,a( k ), k )
                              
                    call stdlib_ctrmm( 'R', 'L', 'N', 'N', k, k, cone, a( 0_ilp ), k,a( k*( k+1 ) ), &
                              k )
                    call stdlib_clauum( 'L', k, a( 0_ilp ), k, info )
                 else
                    ! srpa for upper, transpose, and n is even (see paper)
                    ! t1 -> b(0,k+1),     t2 -> b(0,k),   s -> b(0,0),
                    ! t1 -> a(0+k*(k+1)), t2 -> a(0+k*k), s -> a(0+0)); lda=k
                    call stdlib_clauum( 'U', k, a( k*( k+1 ) ), k, info )
                    call stdlib_cherk( 'U', 'C', k, k, one, a( 0_ilp ), k, one,a( k*( k+1 ) ), k )
                              
                    call stdlib_ctrmm( 'L', 'L', 'C', 'N', k, k, cone, a( k*k ), k,a( 0_ilp ), k )
                              
                    call stdlib_clauum( 'L', k, a( k*k ), k, info )
                 end if
              end if
           end if
           return
     end subroutine stdlib_cpftri

     pure module subroutine stdlib_zpftri( transr, uplo, n, a, info )
     !! ZPFTRI computes the inverse of a complex Hermitian positive definite
     !! matrix A using the Cholesky factorization A = U**H*U or A = L*L**H
     !! computed by ZPFTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(0_ilp:*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: lower, nisodd, normaltransr
           integer(ilp) :: n1, n2, k
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'C' ) ) then
              info = -1_ilp
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZPFTRI', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! invert the triangular cholesky factor u or l.
           call stdlib_ztftri( transr, uplo, 'N', n, a, info )
           if( info>0 )return
           ! if n is odd, set nisodd = .true.
           ! if n is even, set k = n/2 and nisodd = .false.
           if( mod( n, 2_ilp )==0_ilp ) then
              k = n / 2_ilp
              nisodd = .false.
           else
              nisodd = .true.
           end if
           ! set n1 and n2 depending on lower
           if( lower ) then
              n2 = n / 2_ilp
              n1 = n - n2
           else
              n1 = n / 2_ilp
              n2 = n - n1
           end if
           ! start execution of triangular matrix multiply: inv(u)*inv(u)^c or
           ! inv(l)^c*inv(l). there are eight cases.
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                    ! srpa for lower, normal and n is odd ( a(0:n-1,0:n1-1) )
                    ! t1 -> a(0,0), t2 -> a(0,1), s -> a(n1,0)
                    ! t1 -> a(0), t2 -> a(n), s -> a(n1)
                    call stdlib_zlauum( 'L', n1, a( 0_ilp ), n, info )
                    call stdlib_zherk( 'L', 'C', n1, n2, one, a( n1 ), n, one,a( 0_ilp ), n )
                    call stdlib_ztrmm( 'L', 'U', 'N', 'N', n2, n1, cone, a( n ), n,a( n1 ), n )
                              
                    call stdlib_zlauum( 'U', n2, a( n ), n, info )
                 else
                    ! srpa for upper, normal and n is odd ( a(0:n-1,0:n2-1)
                    ! t1 -> a(n1+1,0), t2 -> a(n1,0), s -> a(0,0)
                    ! t1 -> a(n2), t2 -> a(n1), s -> a(0)
                    call stdlib_zlauum( 'L', n1, a( n2 ), n, info )
                    call stdlib_zherk( 'L', 'N', n1, n2, one, a( 0_ilp ), n, one,a( n2 ), n )
                    call stdlib_ztrmm( 'R', 'U', 'C', 'N', n1, n2, cone, a( n1 ), n,a( 0_ilp ), n )
                              
                    call stdlib_zlauum( 'U', n2, a( n1 ), n, info )
                 end if
              else
                 ! n is odd and transr = 'c'
                 if( lower ) then
                    ! srpa for lower, transpose, and n is odd
                    ! t1 -> a(0), t2 -> a(1), s -> a(0+n1*n1)
                    call stdlib_zlauum( 'U', n1, a( 0_ilp ), n1, info )
                    call stdlib_zherk( 'U', 'N', n1, n2, one, a( n1*n1 ), n1, one,a( 0_ilp ), n1 )
                              
                    call stdlib_ztrmm( 'R', 'L', 'N', 'N', n1, n2, cone, a( 1_ilp ), n1,a( n1*n1 ), &
                              n1 )
                    call stdlib_zlauum( 'L', n2, a( 1_ilp ), n1, info )
                 else
                    ! srpa for upper, transpose, and n is odd
                    ! t1 -> a(0+n2*n2), t2 -> a(0+n1*n2), s -> a(0)
                    call stdlib_zlauum( 'U', n1, a( n2*n2 ), n2, info )
                    call stdlib_zherk( 'U', 'C', n1, n2, one, a( 0_ilp ), n2, one,a( n2*n2 ), n2 )
                              
                    call stdlib_ztrmm( 'L', 'L', 'C', 'N', n2, n1, cone, a( n1*n2 ),n2, a( 0_ilp ), &
                              n2 )
                    call stdlib_zlauum( 'L', n2, a( n1*n2 ), n2, info )
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! srpa for lower, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(1,0), t2 -> a(0,0), s -> a(k+1,0)
                    ! t1 -> a(1), t2 -> a(0), s -> a(k+1)
                    call stdlib_zlauum( 'L', k, a( 1_ilp ), n+1, info )
                    call stdlib_zherk( 'L', 'C', k, k, one, a( k+1 ), n+1, one,a( 1_ilp ), n+1 )
                              
                    call stdlib_ztrmm( 'L', 'U', 'N', 'N', k, k, cone, a( 0_ilp ), n+1,a( k+1 ), n+1 )
                              
                    call stdlib_zlauum( 'U', k, a( 0_ilp ), n+1, info )
                 else
                    ! srpa for upper, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(k+1,0) ,  t2 -> a(k,0),   s -> a(0,0)
                    ! t1 -> a(k+1), t2 -> a(k), s -> a(0)
                    call stdlib_zlauum( 'L', k, a( k+1 ), n+1, info )
                    call stdlib_zherk( 'L', 'N', k, k, one, a( 0_ilp ), n+1, one,a( k+1 ), n+1 )
                              
                    call stdlib_ztrmm( 'R', 'U', 'C', 'N', k, k, cone, a( k ), n+1,a( 0_ilp ), n+1 )
                              
                    call stdlib_zlauum( 'U', k, a( k ), n+1, info )
                 end if
              else
                 ! n is even and transr = 'c'
                 if( lower ) then
                    ! srpa for lower, transpose, and n is even (see paper)
                    ! t1 -> b(0,1), t2 -> b(0,0), s -> b(0,k+1),
                    ! t1 -> a(0+k), t2 -> a(0+0), s -> a(0+k*(k+1)); lda=k
                    call stdlib_zlauum( 'U', k, a( k ), k, info )
                    call stdlib_zherk( 'U', 'N', k, k, one, a( k*( k+1 ) ), k, one,a( k ), k )
                              
                    call stdlib_ztrmm( 'R', 'L', 'N', 'N', k, k, cone, a( 0_ilp ), k,a( k*( k+1 ) ), &
                              k )
                    call stdlib_zlauum( 'L', k, a( 0_ilp ), k, info )
                 else
                    ! srpa for upper, transpose, and n is even (see paper)
                    ! t1 -> b(0,k+1),     t2 -> b(0,k),   s -> b(0,0),
                    ! t1 -> a(0+k*(k+1)), t2 -> a(0+k*k), s -> a(0+0)); lda=k
                    call stdlib_zlauum( 'U', k, a( k*( k+1 ) ), k, info )
                    call stdlib_zherk( 'U', 'C', k, k, one, a( 0_ilp ), k, one,a( k*( k+1 ) ), k )
                              
                    call stdlib_ztrmm( 'L', 'L', 'C', 'N', k, k, cone, a( k*k ), k,a( 0_ilp ), k )
                              
                    call stdlib_zlauum( 'L', k, a( k*k ), k, info )
                 end if
              end if
           end if
           return
     end subroutine stdlib_zpftri




     pure module subroutine stdlib_spbcon( uplo, n, kd, ab, ldab, anorm, rcond, work,iwork, info )
     !! SPBCON estimates the reciprocal of the condition number (in the
     !! 1-norm) of a real symmetric positive definite band matrix using the
     !! Cholesky factorization A = U**T*U or A = L*L**T computed by SPBTRF.
     !! An estimate is obtained for norm(inv(A)), and the reciprocal of the
     !! condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, n
           real(sp), intent(in) :: anorm
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(in) :: ab(ldab,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           character :: normin
           integer(ilp) :: ix, kase
           real(sp) :: ainvnm, scale, scalel, scaleu, smlnum
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kd<0_ilp ) then
              info = -3_ilp
           else if( ldab<kd+1 ) then
              info = -5_ilp
           else if( anorm<zero ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SPBCON', -info )
              return
           end if
           ! quick return if possible
           rcond = zero
           if( n==0_ilp ) then
              rcond = one
              return
           else if( anorm==zero ) then
              return
           end if
           smlnum = stdlib_slamch( 'SAFE MINIMUM' )
           ! estimate the 1-norm of the inverse.
           kase = 0_ilp
           normin = 'N'
           10 continue
           call stdlib_slacn2( n, work( n+1 ), work, iwork, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              if( upper ) then
                 ! multiply by inv(u**t).
                 call stdlib_slatbs( 'UPPER', 'TRANSPOSE', 'NON-UNIT', normin, n,kd, ab, ldab, &
                           work, scalel, work( 2_ilp*n+1 ),info )
                 normin = 'Y'
                 ! multiply by inv(u).
                 call stdlib_slatbs( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', normin, n,kd, ab, ldab, &
                           work, scaleu, work( 2_ilp*n+1 ),info )
              else
                 ! multiply by inv(l).
                 call stdlib_slatbs( 'LOWER', 'NO TRANSPOSE', 'NON-UNIT', normin, n,kd, ab, ldab, &
                           work, scalel, work( 2_ilp*n+1 ),info )
                 normin = 'Y'
                 ! multiply by inv(l**t).
                 call stdlib_slatbs( 'LOWER', 'TRANSPOSE', 'NON-UNIT', normin, n,kd, ab, ldab, &
                           work, scaleu, work( 2_ilp*n+1 ),info )
              end if
              ! multiply by 1/scale if doing so will not cause overflow.
              scale = scalel*scaleu
              if( scale/=one ) then
                 ix = stdlib_isamax( n, work, 1_ilp )
                 if( scale<abs( work( ix ) )*smlnum .or. scale==zero )go to 20
                 call stdlib_srscl( n, scale, work, 1_ilp )
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm/=zero )rcond = ( one / ainvnm ) / anorm
           20 continue
           return
     end subroutine stdlib_spbcon

     pure module subroutine stdlib_dpbcon( uplo, n, kd, ab, ldab, anorm, rcond, work,iwork, info )
     !! DPBCON estimates the reciprocal of the condition number (in the
     !! 1-norm) of a real symmetric positive definite band matrix using the
     !! Cholesky factorization A = U**T*U or A = L*L**T computed by DPBTRF.
     !! An estimate is obtained for norm(inv(A)), and the reciprocal of the
     !! condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, n
           real(dp), intent(in) :: anorm
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(in) :: ab(ldab,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           character :: normin
           integer(ilp) :: ix, kase
           real(dp) :: ainvnm, scale, scalel, scaleu, smlnum
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kd<0_ilp ) then
              info = -3_ilp
           else if( ldab<kd+1 ) then
              info = -5_ilp
           else if( anorm<zero ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DPBCON', -info )
              return
           end if
           ! quick return if possible
           rcond = zero
           if( n==0_ilp ) then
              rcond = one
              return
           else if( anorm==zero ) then
              return
           end if
           smlnum = stdlib_dlamch( 'SAFE MINIMUM' )
           ! estimate the 1-norm of the inverse.
           kase = 0_ilp
           normin = 'N'
           10 continue
           call stdlib_dlacn2( n, work( n+1 ), work, iwork, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              if( upper ) then
                 ! multiply by inv(u**t).
                 call stdlib_dlatbs( 'UPPER', 'TRANSPOSE', 'NON-UNIT', normin, n,kd, ab, ldab, &
                           work, scalel, work( 2_ilp*n+1 ),info )
                 normin = 'Y'
                 ! multiply by inv(u).
                 call stdlib_dlatbs( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', normin, n,kd, ab, ldab, &
                           work, scaleu, work( 2_ilp*n+1 ),info )
              else
                 ! multiply by inv(l).
                 call stdlib_dlatbs( 'LOWER', 'NO TRANSPOSE', 'NON-UNIT', normin, n,kd, ab, ldab, &
                           work, scalel, work( 2_ilp*n+1 ),info )
                 normin = 'Y'
                 ! multiply by inv(l**t).
                 call stdlib_dlatbs( 'LOWER', 'TRANSPOSE', 'NON-UNIT', normin, n,kd, ab, ldab, &
                           work, scaleu, work( 2_ilp*n+1 ),info )
              end if
              ! multiply by 1/scale if doing so will not cause overflow.
              scale = scalel*scaleu
              if( scale/=one ) then
                 ix = stdlib_idamax( n, work, 1_ilp )
                 if( scale<abs( work( ix ) )*smlnum .or. scale==zero )go to 20
                 call stdlib_drscl( n, scale, work, 1_ilp )
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm/=zero )rcond = ( one / ainvnm ) / anorm
           20 continue
           return
     end subroutine stdlib_dpbcon


     pure module subroutine stdlib_cpbcon( uplo, n, kd, ab, ldab, anorm, rcond, work,rwork, info )
     !! CPBCON estimates the reciprocal of the condition number (in the
     !! 1-norm) of a complex Hermitian positive definite band matrix using
     !! the Cholesky factorization A = U**H*U or A = L*L**H computed by
     !! CPBTRF.
     !! An estimate is obtained for norm(inv(A)), and the reciprocal of the
     !! condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, n
           real(sp), intent(in) :: anorm
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(in) :: ab(ldab,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           character :: normin
           integer(ilp) :: ix, kase
           real(sp) :: ainvnm, scale, scalel, scaleu, smlnum
           complex(sp) :: zdum
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kd<0_ilp ) then
              info = -3_ilp
           else if( ldab<kd+1 ) then
              info = -5_ilp
           else if( anorm<zero ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CPBCON', -info )
              return
           end if
           ! quick return if possible
           rcond = zero
           if( n==0_ilp ) then
              rcond = one
              return
           else if( anorm==zero ) then
              return
           end if
           smlnum = stdlib_slamch( 'SAFE MINIMUM' )
           ! estimate the 1-norm of the inverse.
           kase = 0_ilp
           normin = 'N'
           10 continue
           call stdlib_clacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              if( upper ) then
                 ! multiply by inv(u**h).
                 call stdlib_clatbs( 'UPPER', 'CONJUGATE TRANSPOSE', 'NON-UNIT',normin, n, kd, ab,&
                            ldab, work, scalel, rwork,info )
                 normin = 'Y'
                 ! multiply by inv(u).
                 call stdlib_clatbs( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', normin, n,kd, ab, ldab, &
                           work, scaleu, rwork, info )
              else
                 ! multiply by inv(l).
                 call stdlib_clatbs( 'LOWER', 'NO TRANSPOSE', 'NON-UNIT', normin, n,kd, ab, ldab, &
                           work, scalel, rwork, info )
                 normin = 'Y'
                 ! multiply by inv(l**h).
                 call stdlib_clatbs( 'LOWER', 'CONJUGATE TRANSPOSE', 'NON-UNIT',normin, n, kd, ab,&
                            ldab, work, scaleu, rwork,info )
              end if
              ! multiply by 1/scale if doing so will not cause overflow.
              scale = scalel*scaleu
              if( scale/=one ) then
                 ix = stdlib_icamax( n, work, 1_ilp )
                 if( scale<cabs1( work( ix ) )*smlnum .or. scale==zero )go to 20
                 call stdlib_csrscl( n, scale, work, 1_ilp )
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm/=zero )rcond = ( one / ainvnm ) / anorm
           20 continue
           return
     end subroutine stdlib_cpbcon

     pure module subroutine stdlib_zpbcon( uplo, n, kd, ab, ldab, anorm, rcond, work,rwork, info )
     !! ZPBCON estimates the reciprocal of the condition number (in the
     !! 1-norm) of a complex Hermitian positive definite band matrix using
     !! the Cholesky factorization A = U**H*U or A = L*L**H computed by
     !! ZPBTRF.
     !! An estimate is obtained for norm(inv(A)), and the reciprocal of the
     !! condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, n
           real(dp), intent(in) :: anorm
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(in) :: ab(ldab,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           character :: normin
           integer(ilp) :: ix, kase
           real(dp) :: ainvnm, scale, scalel, scaleu, smlnum
           complex(dp) :: zdum
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kd<0_ilp ) then
              info = -3_ilp
           else if( ldab<kd+1 ) then
              info = -5_ilp
           else if( anorm<zero ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZPBCON', -info )
              return
           end if
           ! quick return if possible
           rcond = zero
           if( n==0_ilp ) then
              rcond = one
              return
           else if( anorm==zero ) then
              return
           end if
           smlnum = stdlib_dlamch( 'SAFE MINIMUM' )
           ! estimate the 1-norm of the inverse.
           kase = 0_ilp
           normin = 'N'
           10 continue
           call stdlib_zlacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              if( upper ) then
                 ! multiply by inv(u**h).
                 call stdlib_zlatbs( 'UPPER', 'CONJUGATE TRANSPOSE', 'NON-UNIT',normin, n, kd, ab,&
                            ldab, work, scalel, rwork,info )
                 normin = 'Y'
                 ! multiply by inv(u).
                 call stdlib_zlatbs( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', normin, n,kd, ab, ldab, &
                           work, scaleu, rwork, info )
              else
                 ! multiply by inv(l).
                 call stdlib_zlatbs( 'LOWER', 'NO TRANSPOSE', 'NON-UNIT', normin, n,kd, ab, ldab, &
                           work, scalel, rwork, info )
                 normin = 'Y'
                 ! multiply by inv(l**h).
                 call stdlib_zlatbs( 'LOWER', 'CONJUGATE TRANSPOSE', 'NON-UNIT',normin, n, kd, ab,&
                            ldab, work, scaleu, rwork,info )
              end if
              ! multiply by 1/scale if doing so will not cause overflow.
              scale = scalel*scaleu
              if( scale/=one ) then
                 ix = stdlib_izamax( n, work, 1_ilp )
                 if( scale<cabs1( work( ix ) )*smlnum .or. scale==zero )go to 20
                 call stdlib_zdrscl( n, scale, work, 1_ilp )
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm/=zero )rcond = ( one / ainvnm ) / anorm
           20 continue
           return
     end subroutine stdlib_zpbcon




     pure module subroutine stdlib_spbtrf( uplo, n, kd, ab, ldab, info )
     !! SPBTRF computes the Cholesky factorization of a real symmetric
     !! positive definite band matrix A.
     !! The factorization has the form
     !! A = U**T * U,  if UPLO = 'U', or
     !! A = L  * L**T,  if UPLO = 'L',
     !! where U is an upper triangular matrix and L is lower triangular.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, n
           ! Array Arguments 
           real(sp), intent(inout) :: ab(ldab,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: nbmax = 32_ilp
           integer(ilp), parameter :: ldwork = nbmax+1
           
           
           ! Local Scalars 
           integer(ilp) :: i, i2, i3, ib, ii, j, jj, nb
           ! Local Arrays 
           real(sp) :: work(ldwork,nbmax)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( ( .not.stdlib_lsame( uplo, 'U' ) ) .and.( .not.stdlib_lsame( uplo, 'L' ) ) ) &
                     then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kd<0_ilp ) then
              info = -3_ilp
           else if( ldab<kd+1 ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SPBTRF', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! determine the block size for this environment
           nb = stdlib_ilaenv( 1_ilp, 'SPBTRF', uplo, n, kd, -1_ilp, -1_ilp )
           ! the block size must not exceed the semi-bandwidth kd, and must not
           ! exceed the limit set by the size of the local array work.
           nb = min( nb, nbmax )
           if( nb<=1_ilp .or. nb>kd ) then
              ! use unblocked code
              call stdlib_spbtf2( uplo, n, kd, ab, ldab, info )
           else
              ! use blocked code
              if( stdlib_lsame( uplo, 'U' ) ) then
                 ! compute the cholesky factorization of a symmetric band
                 ! matrix, given the upper triangle of the matrix in band
                 ! storage.
                 ! zero the upper triangle of the work array.
                 do j = 1, nb
                    do i = 1, j - 1
                       work( i, j ) = zero
                    end do
                 end do
                 ! process the band matrix one diagonal block at a time.
                 loop_70: do i = 1, n, nb
                    ib = min( nb, n-i+1 )
                    ! factorize the diagonal block
                    call stdlib_spotf2( uplo, ib, ab( kd+1, i ), ldab-1, ii )
                    if( ii/=0_ilp ) then
                       info = i + ii - 1_ilp
                       go to 150
                    end if
                    if( i+ib<=n ) then
                       ! update the relevant part of the trailing submatrix.
                       ! if a11 denotes the diagonal block which has just been
                       ! factorized, then we need to update the remaining
                       ! blocks in the diagram:
                          ! a11   a12   a13
                                ! a22   a23
                                      ! a33
                       ! the numbers of rows and columns in the partitioning
                       ! are ib, i2, i3 respectively. the blocks a12, a22 and
                       ! a23 are empty if ib = kd. the upper triangle of a13
                       ! lies outside the band.
                       i2 = min( kd-ib, n-i-ib+1 )
                       i3 = min( ib, n-i-kd+1 )
                       if( i2>0_ilp ) then
                          ! update a12
                          call stdlib_strsm( 'LEFT', 'UPPER', 'TRANSPOSE','NON-UNIT', ib, i2, one,&
                                     ab( kd+1, i ),ldab-1, ab( kd+1-ib, i+ib ), ldab-1 )
                          ! update a22
                          call stdlib_ssyrk( 'UPPER', 'TRANSPOSE', i2, ib, -one,ab( kd+1-ib, i+ib &
                                    ), ldab-1, one,ab( kd+1, i+ib ), ldab-1 )
                       end if
                       if( i3>0_ilp ) then
                          ! copy the lower triangle of a13 into the work array.
                          do jj = 1, i3
                             do ii = jj, ib
                                work( ii, jj ) = ab( ii-jj+1, jj+i+kd-1 )
                             end do
                          end do
                          ! update a13 (in the work array).
                          call stdlib_strsm( 'LEFT', 'UPPER', 'TRANSPOSE','NON-UNIT', ib, i3, one,&
                                     ab( kd+1, i ),ldab-1, work, ldwork )
                          ! update a23
                          if( i2>0_ilp )call stdlib_sgemm( 'TRANSPOSE', 'NO TRANSPOSE', i2, i3,ib, -&
                          one, ab( kd+1-ib, i+ib ),ldab-1, work, ldwork, one,ab( 1_ilp+ib, i+kd ), &
                                    ldab-1 )
                          ! update a33
                          call stdlib_ssyrk( 'UPPER', 'TRANSPOSE', i3, ib, -one,work, ldwork, one,&
                                     ab( kd+1, i+kd ),ldab-1 )
                          ! copy the lower triangle of a13 back into place.
                          do jj = 1, i3
                             do ii = jj, ib
                                ab( ii-jj+1, jj+i+kd-1 ) = work( ii, jj )
                             end do
                          end do
                       end if
                    end if
                 end do loop_70
              else
                 ! compute the cholesky factorization of a symmetric band
                 ! matrix, given the lower triangle of the matrix in band
                 ! storage.
                 ! zero the lower triangle of the work array.
                 do j = 1, nb
                    do i = j + 1, nb
                       work( i, j ) = zero
                    end do
                 end do
                 ! process the band matrix one diagonal block at a time.
                 loop_140: do i = 1, n, nb
                    ib = min( nb, n-i+1 )
                    ! factorize the diagonal block
                    call stdlib_spotf2( uplo, ib, ab( 1_ilp, i ), ldab-1, ii )
                    if( ii/=0_ilp ) then
                       info = i + ii - 1_ilp
                       go to 150
                    end if
                    if( i+ib<=n ) then
                       ! update the relevant part of the trailing submatrix.
                       ! if a11 denotes the diagonal block which has just been
                       ! factorized, then we need to update the remaining
                       ! blocks in the diagram:
                          ! a11
                          ! a21   a22
                          ! a31   a32   a33
                       ! the numbers of rows and columns in the partitioning
                       ! are ib, i2, i3 respectively. the blocks a21, a22 and
                       ! a32 are empty if ib = kd. the lower triangle of a31
                       ! lies outside the band.
                       i2 = min( kd-ib, n-i-ib+1 )
                       i3 = min( ib, n-i-kd+1 )
                       if( i2>0_ilp ) then
                          ! update a21
                          call stdlib_strsm( 'RIGHT', 'LOWER', 'TRANSPOSE','NON-UNIT', i2, ib, &
                                    one, ab( 1_ilp, i ),ldab-1, ab( 1_ilp+ib, i ), ldab-1 )
                          ! update a22
                          call stdlib_ssyrk( 'LOWER', 'NO TRANSPOSE', i2, ib, -one,ab( 1_ilp+ib, i ), &
                                    ldab-1, one,ab( 1_ilp, i+ib ), ldab-1 )
                       end if
                       if( i3>0_ilp ) then
                          ! copy the upper triangle of a31 into the work array.
                          do jj = 1, ib
                             do ii = 1, min( jj, i3 )
                                work( ii, jj ) = ab( kd+1-jj+ii, jj+i-1 )
                             end do
                          end do
                          ! update a31 (in the work array).
                          call stdlib_strsm( 'RIGHT', 'LOWER', 'TRANSPOSE','NON-UNIT', i3, ib, &
                                    one, ab( 1_ilp, i ),ldab-1, work, ldwork )
                          ! update a32
                          if( i2>0_ilp )call stdlib_sgemm( 'NO TRANSPOSE', 'TRANSPOSE', i3, i2,ib, -&
                          one, work, ldwork,ab( 1_ilp+ib, i ), ldab-1, one,ab( 1_ilp+kd-ib, i+ib ), ldab-&
                                    1_ilp )
                          ! update a33
                          call stdlib_ssyrk( 'LOWER', 'NO TRANSPOSE', i3, ib, -one,work, ldwork, &
                                    one, ab( 1_ilp, i+kd ),ldab-1 )
                          ! copy the upper triangle of a31 back into place.
                          do jj = 1, ib
                             do ii = 1, min( jj, i3 )
                                ab( kd+1-jj+ii, jj+i-1 ) = work( ii, jj )
                             end do
                          end do
                       end if
                    end if
                 end do loop_140
              end if
           end if
           return
           150 continue
           return
     end subroutine stdlib_spbtrf

     pure module subroutine stdlib_dpbtrf( uplo, n, kd, ab, ldab, info )
     !! DPBTRF computes the Cholesky factorization of a real symmetric
     !! positive definite band matrix A.
     !! The factorization has the form
     !! A = U**T * U,  if UPLO = 'U', or
     !! A = L  * L**T,  if UPLO = 'L',
     !! where U is an upper triangular matrix and L is lower triangular.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, n
           ! Array Arguments 
           real(dp), intent(inout) :: ab(ldab,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: nbmax = 32_ilp
           integer(ilp), parameter :: ldwork = nbmax+1
           
           
           ! Local Scalars 
           integer(ilp) :: i, i2, i3, ib, ii, j, jj, nb
           ! Local Arrays 
           real(dp) :: work(ldwork,nbmax)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( ( .not.stdlib_lsame( uplo, 'U' ) ) .and.( .not.stdlib_lsame( uplo, 'L' ) ) ) &
                     then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kd<0_ilp ) then
              info = -3_ilp
           else if( ldab<kd+1 ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DPBTRF', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! determine the block size for this environment
           nb = stdlib_ilaenv( 1_ilp, 'DPBTRF', uplo, n, kd, -1_ilp, -1_ilp )
           ! the block size must not exceed the semi-bandwidth kd, and must not
           ! exceed the limit set by the size of the local array work.
           nb = min( nb, nbmax )
           if( nb<=1_ilp .or. nb>kd ) then
              ! use unblocked code
              call stdlib_dpbtf2( uplo, n, kd, ab, ldab, info )
           else
              ! use blocked code
              if( stdlib_lsame( uplo, 'U' ) ) then
                 ! compute the cholesky factorization of a symmetric band
                 ! matrix, given the upper triangle of the matrix in band
                 ! storage.
                 ! zero the upper triangle of the work array.
                 do j = 1, nb
                    do i = 1, j - 1
                       work( i, j ) = zero
                    end do
                 end do
                 ! process the band matrix one diagonal block at a time.
                 loop_70: do i = 1, n, nb
                    ib = min( nb, n-i+1 )
                    ! factorize the diagonal block
                    call stdlib_dpotf2( uplo, ib, ab( kd+1, i ), ldab-1, ii )
                    if( ii/=0_ilp ) then
                       info = i + ii - 1_ilp
                       go to 150
                    end if
                    if( i+ib<=n ) then
                       ! update the relevant part of the trailing submatrix.
                       ! if a11 denotes the diagonal block which has just been
                       ! factorized, then we need to update the remaining
                       ! blocks in the diagram:
                          ! a11   a12   a13
                                ! a22   a23
                                      ! a33
                       ! the numbers of rows and columns in the partitioning
                       ! are ib, i2, i3 respectively. the blocks a12, a22 and
                       ! a23 are empty if ib = kd. the upper triangle of a13
                       ! lies outside the band.
                       i2 = min( kd-ib, n-i-ib+1 )
                       i3 = min( ib, n-i-kd+1 )
                       if( i2>0_ilp ) then
                          ! update a12
                          call stdlib_dtrsm( 'LEFT', 'UPPER', 'TRANSPOSE','NON-UNIT', ib, i2, one,&
                                     ab( kd+1, i ),ldab-1, ab( kd+1-ib, i+ib ), ldab-1 )
                          ! update a22
                          call stdlib_dsyrk( 'UPPER', 'TRANSPOSE', i2, ib, -one,ab( kd+1-ib, i+ib &
                                    ), ldab-1, one,ab( kd+1, i+ib ), ldab-1 )
                       end if
                       if( i3>0_ilp ) then
                          ! copy the lower triangle of a13 into the work array.
                          do jj = 1, i3
                             do ii = jj, ib
                                work( ii, jj ) = ab( ii-jj+1, jj+i+kd-1 )
                             end do
                          end do
                          ! update a13 (in the work array).
                          call stdlib_dtrsm( 'LEFT', 'UPPER', 'TRANSPOSE','NON-UNIT', ib, i3, one,&
                                     ab( kd+1, i ),ldab-1, work, ldwork )
                          ! update a23
                          if( i2>0_ilp )call stdlib_dgemm( 'TRANSPOSE', 'NO TRANSPOSE', i2, i3,ib, -&
                          one, ab( kd+1-ib, i+ib ),ldab-1, work, ldwork, one,ab( 1_ilp+ib, i+kd ), &
                                    ldab-1 )
                          ! update a33
                          call stdlib_dsyrk( 'UPPER', 'TRANSPOSE', i3, ib, -one,work, ldwork, one,&
                                     ab( kd+1, i+kd ),ldab-1 )
                          ! copy the lower triangle of a13 back into place.
                          do jj = 1, i3
                             do ii = jj, ib
                                ab( ii-jj+1, jj+i+kd-1 ) = work( ii, jj )
                             end do
                          end do
                       end if
                    end if
                 end do loop_70
              else
                 ! compute the cholesky factorization of a symmetric band
                 ! matrix, given the lower triangle of the matrix in band
                 ! storage.
                 ! zero the lower triangle of the work array.
                 do j = 1, nb
                    do i = j + 1, nb
                       work( i, j ) = zero
                    end do
                 end do
                 ! process the band matrix one diagonal block at a time.
                 loop_140: do i = 1, n, nb
                    ib = min( nb, n-i+1 )
                    ! factorize the diagonal block
                    call stdlib_dpotf2( uplo, ib, ab( 1_ilp, i ), ldab-1, ii )
                    if( ii/=0_ilp ) then
                       info = i + ii - 1_ilp
                       go to 150
                    end if
                    if( i+ib<=n ) then
                       ! update the relevant part of the trailing submatrix.
                       ! if a11 denotes the diagonal block which has just been
                       ! factorized, then we need to update the remaining
                       ! blocks in the diagram:
                          ! a11
                          ! a21   a22
                          ! a31   a32   a33
                       ! the numbers of rows and columns in the partitioning
                       ! are ib, i2, i3 respectively. the blocks a21, a22 and
                       ! a32 are empty if ib = kd. the lower triangle of a31
                       ! lies outside the band.
                       i2 = min( kd-ib, n-i-ib+1 )
                       i3 = min( ib, n-i-kd+1 )
                       if( i2>0_ilp ) then
                          ! update a21
                          call stdlib_dtrsm( 'RIGHT', 'LOWER', 'TRANSPOSE','NON-UNIT', i2, ib, &
                                    one, ab( 1_ilp, i ),ldab-1, ab( 1_ilp+ib, i ), ldab-1 )
                          ! update a22
                          call stdlib_dsyrk( 'LOWER', 'NO TRANSPOSE', i2, ib, -one,ab( 1_ilp+ib, i ), &
                                    ldab-1, one,ab( 1_ilp, i+ib ), ldab-1 )
                       end if
                       if( i3>0_ilp ) then
                          ! copy the upper triangle of a31 into the work array.
                          do jj = 1, ib
                             do ii = 1, min( jj, i3 )
                                work( ii, jj ) = ab( kd+1-jj+ii, jj+i-1 )
                             end do
                          end do
                          ! update a31 (in the work array).
                          call stdlib_dtrsm( 'RIGHT', 'LOWER', 'TRANSPOSE','NON-UNIT', i3, ib, &
                                    one, ab( 1_ilp, i ),ldab-1, work, ldwork )
                          ! update a32
                          if( i2>0_ilp )call stdlib_dgemm( 'NO TRANSPOSE', 'TRANSPOSE', i3, i2,ib, -&
                          one, work, ldwork,ab( 1_ilp+ib, i ), ldab-1, one,ab( 1_ilp+kd-ib, i+ib ), ldab-&
                                    1_ilp )
                          ! update a33
                          call stdlib_dsyrk( 'LOWER', 'NO TRANSPOSE', i3, ib, -one,work, ldwork, &
                                    one, ab( 1_ilp, i+kd ),ldab-1 )
                          ! copy the upper triangle of a31 back into place.
                          do jj = 1, ib
                             do ii = 1, min( jj, i3 )
                                ab( kd+1-jj+ii, jj+i-1 ) = work( ii, jj )
                             end do
                          end do
                       end if
                    end if
                 end do loop_140
              end if
           end if
           return
           150 continue
           return
     end subroutine stdlib_dpbtrf


     pure module subroutine stdlib_cpbtrf( uplo, n, kd, ab, ldab, info )
     !! CPBTRF computes the Cholesky factorization of a complex Hermitian
     !! positive definite band matrix A.
     !! The factorization has the form
     !! A = U**H * U,  if UPLO = 'U', or
     !! A = L  * L**H,  if UPLO = 'L',
     !! where U is an upper triangular matrix and L is lower triangular.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, n
           ! Array Arguments 
           complex(sp), intent(inout) :: ab(ldab,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: nbmax = 32_ilp
           integer(ilp), parameter :: ldwork = nbmax+1
           
           
           
           ! Local Scalars 
           integer(ilp) :: i, i2, i3, ib, ii, j, jj, nb
           ! Local Arrays 
           complex(sp) :: work(ldwork,nbmax)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( ( .not.stdlib_lsame( uplo, 'U' ) ) .and.( .not.stdlib_lsame( uplo, 'L' ) ) ) &
                     then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kd<0_ilp ) then
              info = -3_ilp
           else if( ldab<kd+1 ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CPBTRF', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! determine the block size for this environment
           nb = stdlib_ilaenv( 1_ilp, 'CPBTRF', uplo, n, kd, -1_ilp, -1_ilp )
           ! the block size must not exceed the semi-bandwidth kd, and must not
           ! exceed the limit set by the size of the local array work.
           nb = min( nb, nbmax )
           if( nb<=1_ilp .or. nb>kd ) then
              ! use unblocked code
              call stdlib_cpbtf2( uplo, n, kd, ab, ldab, info )
           else
              ! use blocked code
              if( stdlib_lsame( uplo, 'U' ) ) then
                 ! compute the cholesky factorization of a hermitian band
                 ! matrix, given the upper triangle of the matrix in band
                 ! storage.
                 ! zero the upper triangle of the work array.
                 do j = 1, nb
                    do i = 1, j - 1
                       work( i, j ) = zero
                    end do
                 end do
                 ! process the band matrix one diagonal block at a time.
                 loop_70: do i = 1, n, nb
                    ib = min( nb, n-i+1 )
                    ! factorize the diagonal block
                    call stdlib_cpotf2( uplo, ib, ab( kd+1, i ), ldab-1, ii )
                    if( ii/=0_ilp ) then
                       info = i + ii - 1_ilp
                       go to 150
                    end if
                    if( i+ib<=n ) then
                       ! update the relevant part of the trailing submatrix.
                       ! if a11 denotes the diagonal block which has just been
                       ! factorized, then we need to update the remaining
                       ! blocks in the diagram:
                          ! a11   a12   a13
                                ! a22   a23
                                      ! a33
                       ! the numbers of rows and columns in the partitioning
                       ! are ib, i2, i3 respectively. the blocks a12, a22 and
                       ! a23 are empty if ib = kd. the upper triangle of a13
                       ! lies outside the band.
                       i2 = min( kd-ib, n-i-ib+1 )
                       i3 = min( ib, n-i-kd+1 )
                       if( i2>0_ilp ) then
                          ! update a12
                          call stdlib_ctrsm( 'LEFT', 'UPPER', 'CONJUGATE TRANSPOSE','NON-UNIT', &
                                    ib, i2, cone,ab( kd+1, i ), ldab-1,ab( kd+1-ib, i+ib ), ldab-1 )
                          ! update a22
                          call stdlib_cherk( 'UPPER', 'CONJUGATE TRANSPOSE', i2, ib,-one, ab( kd+&
                                    1_ilp-ib, i+ib ), ldab-1, one,ab( kd+1, i+ib ), ldab-1 )
                       end if
                       if( i3>0_ilp ) then
                          ! copy the lower triangle of a13 into the work array.
                          do jj = 1, i3
                             do ii = jj, ib
                                work( ii, jj ) = ab( ii-jj+1, jj+i+kd-1 )
                             end do
                          end do
                          ! update a13 (in the work array).
                          call stdlib_ctrsm( 'LEFT', 'UPPER', 'CONJUGATE TRANSPOSE','NON-UNIT', &
                                    ib, i3, cone,ab( kd+1, i ), ldab-1, work, ldwork )
                          ! update a23
                          if( i2>0_ilp )call stdlib_cgemm( 'CONJUGATE TRANSPOSE','NO TRANSPOSE', i2, &
                          i3, ib, -cone,ab( kd+1-ib, i+ib ), ldab-1, work,ldwork, cone, ab( 1_ilp+ib, &
                                    i+kd ),ldab-1 )
                          ! update a33
                          call stdlib_cherk( 'UPPER', 'CONJUGATE TRANSPOSE', i3, ib,-one, work, &
                                    ldwork, one,ab( kd+1, i+kd ), ldab-1 )
                          ! copy the lower triangle of a13 back into place.
                          do jj = 1, i3
                             do ii = jj, ib
                                ab( ii-jj+1, jj+i+kd-1 ) = work( ii, jj )
                             end do
                          end do
                       end if
                    end if
                 end do loop_70
              else
                 ! compute the cholesky factorization of a hermitian band
                 ! matrix, given the lower triangle of the matrix in band
                 ! storage.
                 ! zero the lower triangle of the work array.
                 do j = 1, nb
                    do i = j + 1, nb
                       work( i, j ) = zero
                    end do
                 end do
                 ! process the band matrix one diagonal block at a time.
                 loop_140: do i = 1, n, nb
                    ib = min( nb, n-i+1 )
                    ! factorize the diagonal block
                    call stdlib_cpotf2( uplo, ib, ab( 1_ilp, i ), ldab-1, ii )
                    if( ii/=0_ilp ) then
                       info = i + ii - 1_ilp
                       go to 150
                    end if
                    if( i+ib<=n ) then
                       ! update the relevant part of the trailing submatrix.
                       ! if a11 denotes the diagonal block which has just been
                       ! factorized, then we need to update the remaining
                       ! blocks in the diagram:
                          ! a11
                          ! a21   a22
                          ! a31   a32   a33
                       ! the numbers of rows and columns in the partitioning
                       ! are ib, i2, i3 respectively. the blocks a21, a22 and
                       ! a32 are empty if ib = kd. the lower triangle of a31
                       ! lies outside the band.
                       i2 = min( kd-ib, n-i-ib+1 )
                       i3 = min( ib, n-i-kd+1 )
                       if( i2>0_ilp ) then
                          ! update a21
                          call stdlib_ctrsm( 'RIGHT', 'LOWER','CONJUGATE TRANSPOSE', 'NON-UNIT', &
                                    i2,ib, cone, ab( 1_ilp, i ), ldab-1,ab( 1_ilp+ib, i ), ldab-1 )
                          ! update a22
                          call stdlib_cherk( 'LOWER', 'NO TRANSPOSE', i2, ib, -one,ab( 1_ilp+ib, i ), &
                                    ldab-1, one,ab( 1_ilp, i+ib ), ldab-1 )
                       end if
                       if( i3>0_ilp ) then
                          ! copy the upper triangle of a31 into the work array.
                          do jj = 1, ib
                             do ii = 1, min( jj, i3 )
                                work( ii, jj ) = ab( kd+1-jj+ii, jj+i-1 )
                             end do
                          end do
                          ! update a31 (in the work array).
                          call stdlib_ctrsm( 'RIGHT', 'LOWER','CONJUGATE TRANSPOSE', 'NON-UNIT', &
                                    i3,ib, cone, ab( 1_ilp, i ), ldab-1, work,ldwork )
                          ! update a32
                          if( i2>0_ilp )call stdlib_cgemm( 'NO TRANSPOSE','CONJUGATE TRANSPOSE', i3, &
                          i2, ib,-cone, work, ldwork, ab( 1_ilp+ib, i ),ldab-1, cone, ab( 1_ilp+kd-ib, i+&
                                    ib ),ldab-1 )
                          ! update a33
                          call stdlib_cherk( 'LOWER', 'NO TRANSPOSE', i3, ib, -one,work, ldwork, &
                                    one, ab( 1_ilp, i+kd ),ldab-1 )
                          ! copy the upper triangle of a31 back into place.
                          do jj = 1, ib
                             do ii = 1, min( jj, i3 )
                                ab( kd+1-jj+ii, jj+i-1 ) = work( ii, jj )
                             end do
                          end do
                       end if
                    end if
                 end do loop_140
              end if
           end if
           return
           150 continue
           return
     end subroutine stdlib_cpbtrf

     pure module subroutine stdlib_zpbtrf( uplo, n, kd, ab, ldab, info )
     !! ZPBTRF computes the Cholesky factorization of a complex Hermitian
     !! positive definite band matrix A.
     !! The factorization has the form
     !! A = U**H * U,  if UPLO = 'U', or
     !! A = L  * L**H,  if UPLO = 'L',
     !! where U is an upper triangular matrix and L is lower triangular.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, n
           ! Array Arguments 
           complex(dp), intent(inout) :: ab(ldab,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: nbmax = 32_ilp
           integer(ilp), parameter :: ldwork = nbmax+1
           
           
           
           ! Local Scalars 
           integer(ilp) :: i, i2, i3, ib, ii, j, jj, nb
           ! Local Arrays 
           complex(dp) :: work(ldwork,nbmax)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( ( .not.stdlib_lsame( uplo, 'U' ) ) .and.( .not.stdlib_lsame( uplo, 'L' ) ) ) &
                     then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kd<0_ilp ) then
              info = -3_ilp
           else if( ldab<kd+1 ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZPBTRF', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! determine the block size for this environment
           nb = stdlib_ilaenv( 1_ilp, 'ZPBTRF', uplo, n, kd, -1_ilp, -1_ilp )
           ! the block size must not exceed the semi-bandwidth kd, and must not
           ! exceed the limit set by the size of the local array work.
           nb = min( nb, nbmax )
           if( nb<=1_ilp .or. nb>kd ) then
              ! use unblocked code
              call stdlib_zpbtf2( uplo, n, kd, ab, ldab, info )
           else
              ! use blocked code
              if( stdlib_lsame( uplo, 'U' ) ) then
                 ! compute the cholesky factorization of a hermitian band
                 ! matrix, given the upper triangle of the matrix in band
                 ! storage.
                 ! zero the upper triangle of the work array.
                 do j = 1, nb
                    do i = 1, j - 1
                       work( i, j ) = zero
                    end do
                 end do
                 ! process the band matrix one diagonal block at a time.
                 loop_70: do i = 1, n, nb
                    ib = min( nb, n-i+1 )
                    ! factorize the diagonal block
                    call stdlib_zpotf2( uplo, ib, ab( kd+1, i ), ldab-1, ii )
                    if( ii/=0_ilp ) then
                       info = i + ii - 1_ilp
                       go to 150
                    end if
                    if( i+ib<=n ) then
                       ! update the relevant part of the trailing submatrix.
                       ! if a11 denotes the diagonal block which has just been
                       ! factorized, then we need to update the remaining
                       ! blocks in the diagram:
                          ! a11   a12   a13
                                ! a22   a23
                                      ! a33
                       ! the numbers of rows and columns in the partitioning
                       ! are ib, i2, i3 respectively. the blocks a12, a22 and
                       ! a23 are empty if ib = kd. the upper triangle of a13
                       ! lies outside the band.
                       i2 = min( kd-ib, n-i-ib+1 )
                       i3 = min( ib, n-i-kd+1 )
                       if( i2>0_ilp ) then
                          ! update a12
                          call stdlib_ztrsm( 'LEFT', 'UPPER', 'CONJUGATE TRANSPOSE','NON-UNIT', &
                                    ib, i2, cone,ab( kd+1, i ), ldab-1,ab( kd+1-ib, i+ib ), ldab-1 )
                          ! update a22
                          call stdlib_zherk( 'UPPER', 'CONJUGATE TRANSPOSE', i2, ib,-one, ab( kd+&
                                    1_ilp-ib, i+ib ), ldab-1, one,ab( kd+1, i+ib ), ldab-1 )
                       end if
                       if( i3>0_ilp ) then
                          ! copy the lower triangle of a13 into the work array.
                          do jj = 1, i3
                             do ii = jj, ib
                                work( ii, jj ) = ab( ii-jj+1, jj+i+kd-1 )
                             end do
                          end do
                          ! update a13 (in the work array).
                          call stdlib_ztrsm( 'LEFT', 'UPPER', 'CONJUGATE TRANSPOSE','NON-UNIT', &
                                    ib, i3, cone,ab( kd+1, i ), ldab-1, work, ldwork )
                          ! update a23
                          if( i2>0_ilp )call stdlib_zgemm( 'CONJUGATE TRANSPOSE','NO TRANSPOSE', i2, &
                          i3, ib, -cone,ab( kd+1-ib, i+ib ), ldab-1, work,ldwork, cone, ab( 1_ilp+ib, &
                                    i+kd ),ldab-1 )
                          ! update a33
                          call stdlib_zherk( 'UPPER', 'CONJUGATE TRANSPOSE', i3, ib,-one, work, &
                                    ldwork, one,ab( kd+1, i+kd ), ldab-1 )
                          ! copy the lower triangle of a13 back into place.
                          do jj = 1, i3
                             do ii = jj, ib
                                ab( ii-jj+1, jj+i+kd-1 ) = work( ii, jj )
                             end do
                          end do
                       end if
                    end if
                 end do loop_70
              else
                 ! compute the cholesky factorization of a hermitian band
                 ! matrix, given the lower triangle of the matrix in band
                 ! storage.
                 ! zero the lower triangle of the work array.
                 do j = 1, nb
                    do i = j + 1, nb
                       work( i, j ) = zero
                    end do
                 end do
                 ! process the band matrix one diagonal block at a time.
                 loop_140: do i = 1, n, nb
                    ib = min( nb, n-i+1 )
                    ! factorize the diagonal block
                    call stdlib_zpotf2( uplo, ib, ab( 1_ilp, i ), ldab-1, ii )
                    if( ii/=0_ilp ) then
                       info = i + ii - 1_ilp
                       go to 150
                    end if
                    if( i+ib<=n ) then
                       ! update the relevant part of the trailing submatrix.
                       ! if a11 denotes the diagonal block which has just been
                       ! factorized, then we need to update the remaining
                       ! blocks in the diagram:
                          ! a11
                          ! a21   a22
                          ! a31   a32   a33
                       ! the numbers of rows and columns in the partitioning
                       ! are ib, i2, i3 respectively. the blocks a21, a22 and
                       ! a32 are empty if ib = kd. the lower triangle of a31
                       ! lies outside the band.
                       i2 = min( kd-ib, n-i-ib+1 )
                       i3 = min( ib, n-i-kd+1 )
                       if( i2>0_ilp ) then
                          ! update a21
                          call stdlib_ztrsm( 'RIGHT', 'LOWER','CONJUGATE TRANSPOSE', 'NON-UNIT', &
                                    i2,ib, cone, ab( 1_ilp, i ), ldab-1,ab( 1_ilp+ib, i ), ldab-1 )
                          ! update a22
                          call stdlib_zherk( 'LOWER', 'NO TRANSPOSE', i2, ib, -one,ab( 1_ilp+ib, i ), &
                                    ldab-1, one,ab( 1_ilp, i+ib ), ldab-1 )
                       end if
                       if( i3>0_ilp ) then
                          ! copy the upper triangle of a31 into the work array.
                          do jj = 1, ib
                             do ii = 1, min( jj, i3 )
                                work( ii, jj ) = ab( kd+1-jj+ii, jj+i-1 )
                             end do
                          end do
                          ! update a31 (in the work array).
                          call stdlib_ztrsm( 'RIGHT', 'LOWER','CONJUGATE TRANSPOSE', 'NON-UNIT', &
                                    i3,ib, cone, ab( 1_ilp, i ), ldab-1, work,ldwork )
                          ! update a32
                          if( i2>0_ilp )call stdlib_zgemm( 'NO TRANSPOSE','CONJUGATE TRANSPOSE', i3, &
                          i2, ib,-cone, work, ldwork, ab( 1_ilp+ib, i ),ldab-1, cone, ab( 1_ilp+kd-ib, i+&
                                    ib ),ldab-1 )
                          ! update a33
                          call stdlib_zherk( 'LOWER', 'NO TRANSPOSE', i3, ib, -one,work, ldwork, &
                                    one, ab( 1_ilp, i+kd ),ldab-1 )
                          ! copy the upper triangle of a31 back into place.
                          do jj = 1, ib
                             do ii = 1, min( jj, i3 )
                                ab( kd+1-jj+ii, jj+i-1 ) = work( ii, jj )
                             end do
                          end do
                       end if
                    end if
                 end do loop_140
              end if
           end if
           return
           150 continue
           return
     end subroutine stdlib_zpbtrf




     pure module subroutine stdlib_spbtf2( uplo, n, kd, ab, ldab, info )
     !! SPBTF2 computes the Cholesky factorization of a real symmetric
     !! positive definite band matrix A.
     !! The factorization has the form
     !! A = U**T * U ,  if UPLO = 'U', or
     !! A = L  * L**T,  if UPLO = 'L',
     !! where U is an upper triangular matrix, U**T is the transpose of U, and
     !! L is lower triangular.
     !! This is the unblocked version of the algorithm, calling Level 2 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, n
           ! Array Arguments 
           real(sp), intent(inout) :: ab(ldab,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, kld, kn
           real(sp) :: ajj
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kd<0_ilp ) then
              info = -3_ilp
           else if( ldab<kd+1 ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SPBTF2', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           kld = max( 1_ilp, ldab-1 )
           if( upper ) then
              ! compute the cholesky factorization a = u**t*u.
              do j = 1, n
                 ! compute u(j,j) and test for non-positive-definiteness.
                 ajj = ab( kd+1, j )
                 if( ajj<=zero )go to 30
                 ajj = sqrt( ajj )
                 ab( kd+1, j ) = ajj
                 ! compute elements j+1:j+kn of row j and update the
                 ! trailing submatrix within the band.
                 kn = min( kd, n-j )
                 if( kn>0_ilp ) then
                    call stdlib_sscal( kn, one / ajj, ab( kd, j+1 ), kld )
                    call stdlib_ssyr( 'UPPER', kn, -one, ab( kd, j+1 ), kld,ab( kd+1, j+1 ), kld )
                              
                 end if
              end do
           else
              ! compute the cholesky factorization a = l*l**t.
              do j = 1, n
                 ! compute l(j,j) and test for non-positive-definiteness.
                 ajj = ab( 1_ilp, j )
                 if( ajj<=zero )go to 30
                 ajj = sqrt( ajj )
                 ab( 1_ilp, j ) = ajj
                 ! compute elements j+1:j+kn of column j and update the
                 ! trailing submatrix within the band.
                 kn = min( kd, n-j )
                 if( kn>0_ilp ) then
                    call stdlib_sscal( kn, one / ajj, ab( 2_ilp, j ), 1_ilp )
                    call stdlib_ssyr( 'LOWER', kn, -one, ab( 2_ilp, j ), 1_ilp,ab( 1_ilp, j+1 ), kld )
                 end if
              end do
           end if
           return
           30 continue
           info = j
           return
     end subroutine stdlib_spbtf2

     pure module subroutine stdlib_dpbtf2( uplo, n, kd, ab, ldab, info )
     !! DPBTF2 computes the Cholesky factorization of a real symmetric
     !! positive definite band matrix A.
     !! The factorization has the form
     !! A = U**T * U ,  if UPLO = 'U', or
     !! A = L  * L**T,  if UPLO = 'L',
     !! where U is an upper triangular matrix, U**T is the transpose of U, and
     !! L is lower triangular.
     !! This is the unblocked version of the algorithm, calling Level 2 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, n
           ! Array Arguments 
           real(dp), intent(inout) :: ab(ldab,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, kld, kn
           real(dp) :: ajj
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kd<0_ilp ) then
              info = -3_ilp
           else if( ldab<kd+1 ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DPBTF2', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           kld = max( 1_ilp, ldab-1 )
           if( upper ) then
              ! compute the cholesky factorization a = u**t*u.
              do j = 1, n
                 ! compute u(j,j) and test for non-positive-definiteness.
                 ajj = ab( kd+1, j )
                 if( ajj<=zero )go to 30
                 ajj = sqrt( ajj )
                 ab( kd+1, j ) = ajj
                 ! compute elements j+1:j+kn of row j and update the
                 ! trailing submatrix within the band.
                 kn = min( kd, n-j )
                 if( kn>0_ilp ) then
                    call stdlib_dscal( kn, one / ajj, ab( kd, j+1 ), kld )
                    call stdlib_dsyr( 'UPPER', kn, -one, ab( kd, j+1 ), kld,ab( kd+1, j+1 ), kld )
                              
                 end if
              end do
           else
              ! compute the cholesky factorization a = l*l**t.
              do j = 1, n
                 ! compute l(j,j) and test for non-positive-definiteness.
                 ajj = ab( 1_ilp, j )
                 if( ajj<=zero )go to 30
                 ajj = sqrt( ajj )
                 ab( 1_ilp, j ) = ajj
                 ! compute elements j+1:j+kn of column j and update the
                 ! trailing submatrix within the band.
                 kn = min( kd, n-j )
                 if( kn>0_ilp ) then
                    call stdlib_dscal( kn, one / ajj, ab( 2_ilp, j ), 1_ilp )
                    call stdlib_dsyr( 'LOWER', kn, -one, ab( 2_ilp, j ), 1_ilp,ab( 1_ilp, j+1 ), kld )
                 end if
              end do
           end if
           return
           30 continue
           info = j
           return
     end subroutine stdlib_dpbtf2


     pure module subroutine stdlib_cpbtf2( uplo, n, kd, ab, ldab, info )
     !! CPBTF2 computes the Cholesky factorization of a complex Hermitian
     !! positive definite band matrix A.
     !! The factorization has the form
     !! A = U**H * U ,  if UPLO = 'U', or
     !! A = L  * L**H,  if UPLO = 'L',
     !! where U is an upper triangular matrix, U**H is the conjugate transpose
     !! of U, and L is lower triangular.
     !! This is the unblocked version of the algorithm, calling Level 2 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, n
           ! Array Arguments 
           complex(sp), intent(inout) :: ab(ldab,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, kld, kn
           real(sp) :: ajj
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kd<0_ilp ) then
              info = -3_ilp
           else if( ldab<kd+1 ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CPBTF2', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           kld = max( 1_ilp, ldab-1 )
           if( upper ) then
              ! compute the cholesky factorization a = u**h * u.
              do j = 1, n
                 ! compute u(j,j) and test for non-positive-definiteness.
                 ajj = real( ab( kd+1, j ),KIND=sp)
                 if( ajj<=zero ) then
                    ab( kd+1, j ) = ajj
                    go to 30
                 end if
                 ajj = sqrt( ajj )
                 ab( kd+1, j ) = ajj
                 ! compute elements j+1:j+kn of row j and update the
                 ! trailing submatrix within the band.
                 kn = min( kd, n-j )
                 if( kn>0_ilp ) then
                    call stdlib_csscal( kn, one / ajj, ab( kd, j+1 ), kld )
                    call stdlib_clacgv( kn, ab( kd, j+1 ), kld )
                    call stdlib_cher( 'UPPER', kn, -one, ab( kd, j+1 ), kld,ab( kd+1, j+1 ), kld )
                              
                    call stdlib_clacgv( kn, ab( kd, j+1 ), kld )
                 end if
              end do
           else
              ! compute the cholesky factorization a = l*l**h.
              do j = 1, n
                 ! compute l(j,j) and test for non-positive-definiteness.
                 ajj = real( ab( 1_ilp, j ),KIND=sp)
                 if( ajj<=zero ) then
                    ab( 1_ilp, j ) = ajj
                    go to 30
                 end if
                 ajj = sqrt( ajj )
                 ab( 1_ilp, j ) = ajj
                 ! compute elements j+1:j+kn of column j and update the
                 ! trailing submatrix within the band.
                 kn = min( kd, n-j )
                 if( kn>0_ilp ) then
                    call stdlib_csscal( kn, one / ajj, ab( 2_ilp, j ), 1_ilp )
                    call stdlib_cher( 'LOWER', kn, -one, ab( 2_ilp, j ), 1_ilp,ab( 1_ilp, j+1 ), kld )
                 end if
              end do
           end if
           return
           30 continue
           info = j
           return
     end subroutine stdlib_cpbtf2

     pure module subroutine stdlib_zpbtf2( uplo, n, kd, ab, ldab, info )
     !! ZPBTF2 computes the Cholesky factorization of a complex Hermitian
     !! positive definite band matrix A.
     !! The factorization has the form
     !! A = U**H * U ,  if UPLO = 'U', or
     !! A = L  * L**H,  if UPLO = 'L',
     !! where U is an upper triangular matrix, U**H is the conjugate transpose
     !! of U, and L is lower triangular.
     !! This is the unblocked version of the algorithm, calling Level 2 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, n
           ! Array Arguments 
           complex(dp), intent(inout) :: ab(ldab,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, kld, kn
           real(dp) :: ajj
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kd<0_ilp ) then
              info = -3_ilp
           else if( ldab<kd+1 ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZPBTF2', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           kld = max( 1_ilp, ldab-1 )
           if( upper ) then
              ! compute the cholesky factorization a = u**h * u.
              do j = 1, n
                 ! compute u(j,j) and test for non-positive-definiteness.
                 ajj = real( ab( kd+1, j ),KIND=dp)
                 if( ajj<=zero ) then
                    ab( kd+1, j ) = ajj
                    go to 30
                 end if
                 ajj = sqrt( ajj )
                 ab( kd+1, j ) = ajj
                 ! compute elements j+1:j+kn of row j and update the
                 ! trailing submatrix within the band.
                 kn = min( kd, n-j )
                 if( kn>0_ilp ) then
                    call stdlib_zdscal( kn, one / ajj, ab( kd, j+1 ), kld )
                    call stdlib_zlacgv( kn, ab( kd, j+1 ), kld )
                    call stdlib_zher( 'UPPER', kn, -one, ab( kd, j+1 ), kld,ab( kd+1, j+1 ), kld )
                              
                    call stdlib_zlacgv( kn, ab( kd, j+1 ), kld )
                 end if
              end do
           else
              ! compute the cholesky factorization a = l*l**h.
              do j = 1, n
                 ! compute l(j,j) and test for non-positive-definiteness.
                 ajj = real( ab( 1_ilp, j ),KIND=dp)
                 if( ajj<=zero ) then
                    ab( 1_ilp, j ) = ajj
                    go to 30
                 end if
                 ajj = sqrt( ajj )
                 ab( 1_ilp, j ) = ajj
                 ! compute elements j+1:j+kn of column j and update the
                 ! trailing submatrix within the band.
                 kn = min( kd, n-j )
                 if( kn>0_ilp ) then
                    call stdlib_zdscal( kn, one / ajj, ab( 2_ilp, j ), 1_ilp )
                    call stdlib_zher( 'LOWER', kn, -one, ab( 2_ilp, j ), 1_ilp,ab( 1_ilp, j+1 ), kld )
                 end if
              end do
           end if
           return
           30 continue
           info = j
           return
     end subroutine stdlib_zpbtf2




     pure module subroutine stdlib_spbtrs( uplo, n, kd, nrhs, ab, ldab, b, ldb, info )
     !! SPBTRS solves a system of linear equations A*X = B with a symmetric
     !! positive definite band matrix A using the Cholesky factorization
     !! A = U**T*U or A = L*L**T computed by SPBTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldb, n, nrhs
           ! Array Arguments 
           real(sp), intent(in) :: ab(ldab,*)
           real(sp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kd<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( ldab<kd+1 ) then
              info = -6_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SPBTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( upper ) then
              ! solve a*x = b where a = u**t *u.
              do j = 1, nrhs
                 ! solve u**t *x = b, overwriting b with x.
                 call stdlib_stbsv( 'UPPER', 'TRANSPOSE', 'NON-UNIT', n, kd, ab,ldab, b( 1_ilp, j ), &
                           1_ilp )
                 ! solve u*x = b, overwriting b with x.
                 call stdlib_stbsv( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', n, kd, ab,ldab, b( 1_ilp, j )&
                           , 1_ilp )
              end do
           else
              ! solve a*x = b where a = l*l**t.
              do j = 1, nrhs
                 ! solve l*x = b, overwriting b with x.
                 call stdlib_stbsv( 'LOWER', 'NO TRANSPOSE', 'NON-UNIT', n, kd, ab,ldab, b( 1_ilp, j )&
                           , 1_ilp )
                 ! solve l**t *x = b, overwriting b with x.
                 call stdlib_stbsv( 'LOWER', 'TRANSPOSE', 'NON-UNIT', n, kd, ab,ldab, b( 1_ilp, j ), &
                           1_ilp )
              end do
           end if
           return
     end subroutine stdlib_spbtrs

     pure module subroutine stdlib_dpbtrs( uplo, n, kd, nrhs, ab, ldab, b, ldb, info )
     !! DPBTRS solves a system of linear equations A*X = B with a symmetric
     !! positive definite band matrix A using the Cholesky factorization
     !! A = U**T*U or A = L*L**T computed by DPBTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldb, n, nrhs
           ! Array Arguments 
           real(dp), intent(in) :: ab(ldab,*)
           real(dp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kd<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( ldab<kd+1 ) then
              info = -6_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DPBTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( upper ) then
              ! solve a*x = b where a = u**t *u.
              do j = 1, nrhs
                 ! solve u**t *x = b, overwriting b with x.
                 call stdlib_dtbsv( 'UPPER', 'TRANSPOSE', 'NON-UNIT', n, kd, ab,ldab, b( 1_ilp, j ), &
                           1_ilp )
                 ! solve u*x = b, overwriting b with x.
                 call stdlib_dtbsv( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', n, kd, ab,ldab, b( 1_ilp, j )&
                           , 1_ilp )
              end do
           else
              ! solve a*x = b where a = l*l**t.
              do j = 1, nrhs
                 ! solve l*x = b, overwriting b with x.
                 call stdlib_dtbsv( 'LOWER', 'NO TRANSPOSE', 'NON-UNIT', n, kd, ab,ldab, b( 1_ilp, j )&
                           , 1_ilp )
                 ! solve l**t *x = b, overwriting b with x.
                 call stdlib_dtbsv( 'LOWER', 'TRANSPOSE', 'NON-UNIT', n, kd, ab,ldab, b( 1_ilp, j ), &
                           1_ilp )
              end do
           end if
           return
     end subroutine stdlib_dpbtrs


     pure module subroutine stdlib_cpbtrs( uplo, n, kd, nrhs, ab, ldab, b, ldb, info )
     !! CPBTRS solves a system of linear equations A*X = B with a Hermitian
     !! positive definite band matrix A using the Cholesky factorization
     !! A = U**H*U or A = L*L**H computed by CPBTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldb, n, nrhs
           ! Array Arguments 
           complex(sp), intent(in) :: ab(ldab,*)
           complex(sp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kd<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( ldab<kd+1 ) then
              info = -6_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CPBTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( upper ) then
              ! solve a*x = b where a = u**h *u.
              do j = 1, nrhs
                 ! solve u**h *x = b, overwriting b with x.
                 call stdlib_ctbsv( 'UPPER', 'CONJUGATE TRANSPOSE', 'NON-UNIT', n,kd, ab, ldab, b(&
                            1_ilp, j ), 1_ilp )
                 ! solve u*x = b, overwriting b with x.
                 call stdlib_ctbsv( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', n, kd, ab,ldab, b( 1_ilp, j )&
                           , 1_ilp )
              end do
           else
              ! solve a*x = b where a = l*l**h.
              do j = 1, nrhs
                 ! solve l*x = b, overwriting b with x.
                 call stdlib_ctbsv( 'LOWER', 'NO TRANSPOSE', 'NON-UNIT', n, kd, ab,ldab, b( 1_ilp, j )&
                           , 1_ilp )
                 ! solve l**h *x = b, overwriting b with x.
                 call stdlib_ctbsv( 'LOWER', 'CONJUGATE TRANSPOSE', 'NON-UNIT', n,kd, ab, ldab, b(&
                            1_ilp, j ), 1_ilp )
              end do
           end if
           return
     end subroutine stdlib_cpbtrs

     pure module subroutine stdlib_zpbtrs( uplo, n, kd, nrhs, ab, ldab, b, ldb, info )
     !! ZPBTRS solves a system of linear equations A*X = B with a Hermitian
     !! positive definite band matrix A using the Cholesky factorization
     !! A = U**H *U or A = L*L**H computed by ZPBTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldb, n, nrhs
           ! Array Arguments 
           complex(dp), intent(in) :: ab(ldab,*)
           complex(dp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kd<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( ldab<kd+1 ) then
              info = -6_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZPBTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( upper ) then
              ! solve a*x = b where a = u**h *u.
              do j = 1, nrhs
                 ! solve u**h *x = b, overwriting b with x.
                 call stdlib_ztbsv( 'UPPER', 'CONJUGATE TRANSPOSE', 'NON-UNIT', n,kd, ab, ldab, b(&
                            1_ilp, j ), 1_ilp )
                 ! solve u*x = b, overwriting b with x.
                 call stdlib_ztbsv( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', n, kd, ab,ldab, b( 1_ilp, j )&
                           , 1_ilp )
              end do
           else
              ! solve a*x = b where a = l*l**h.
              do j = 1, nrhs
                 ! solve l*x = b, overwriting b with x.
                 call stdlib_ztbsv( 'LOWER', 'NO TRANSPOSE', 'NON-UNIT', n, kd, ab,ldab, b( 1_ilp, j )&
                           , 1_ilp )
                 ! solve l**h *x = b, overwriting b with x.
                 call stdlib_ztbsv( 'LOWER', 'CONJUGATE TRANSPOSE', 'NON-UNIT', n,kd, ab, ldab, b(&
                            1_ilp, j ), 1_ilp )
              end do
           end if
           return
     end subroutine stdlib_zpbtrs




     pure module subroutine stdlib_spbrfs( uplo, n, kd, nrhs, ab, ldab, afb, ldafb, b,ldb, x, ldx, ferr, &
     !! SPBRFS improves the computed solution to a system of linear
     !! equations when the coefficient matrix is symmetric positive definite
     !! and banded, and provides error bounds and backward error estimates
     !! for the solution.
               berr, work, iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldafb, ldb, ldx, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(in) :: ab(ldab,*), afb(ldafb,*), b(ldb,*)
           real(sp), intent(out) :: berr(*), ferr(*), work(*)
           real(sp), intent(inout) :: x(ldx,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: count, i, j, k, kase, l, nz
           real(sp) :: eps, lstres, s, safe1, safe2, safmin, xk
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kd<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( ldab<kd+1 ) then
              info = -6_ilp
           else if( ldafb<kd+1 ) then
              info = -8_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -10_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -12_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SPBRFS', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp .or. nrhs==0_ilp ) then
              do j = 1, nrhs
                 ferr( j ) = zero
                 berr( j ) = zero
              end do
              return
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = min( n+1, 2_ilp*kd+2 )
           eps = stdlib_slamch( 'EPSILON' )
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           safe1 = nz*safmin
           safe2 = safe1 / eps
           ! do for each right hand side
           loop_140: do j = 1, nrhs
              count = 1_ilp
              lstres = three
              20 continue
              ! loop until stopping criterion is satisfied.
              ! compute residual r = b - a * x
              call stdlib_scopy( n, b( 1_ilp, j ), 1_ilp, work( n+1 ), 1_ilp )
              call stdlib_ssbmv( uplo, n, kd, -one, ab, ldab, x( 1_ilp, j ), 1_ilp, one,work( n+1 ), 1_ilp )
                        
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(a)*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              do i = 1, n
                 work( i ) = abs( b( i, j ) )
              end do
              ! compute abs(a)*abs(x) + abs(b).
              if( upper ) then
                 do k = 1, n
                    s = zero
                    xk = abs( x( k, j ) )
                    l = kd + 1_ilp - k
                    do i = max( 1, k-kd ), k - 1
                       work( i ) = work( i ) + abs( ab( l+i, k ) )*xk
                       s = s + abs( ab( l+i, k ) )*abs( x( i, j ) )
                    end do
                    work( k ) = work( k ) + abs( ab( kd+1, k ) )*xk + s
                 end do
              else
                 do k = 1, n
                    s = zero
                    xk = abs( x( k, j ) )
                    work( k ) = work( k ) + abs( ab( 1_ilp, k ) )*xk
                    l = 1_ilp - k
                    do i = k + 1, min( n, k+kd )
                       work( i ) = work( i ) + abs( ab( l+i, k ) )*xk
                       s = s + abs( ab( l+i, k ) )*abs( x( i, j ) )
                    end do
                    work( k ) = work( k ) + s
                 end do
              end if
              s = zero
              do i = 1, n
                 if( work( i )>safe2 ) then
                    s = max( s, abs( work( n+i ) ) / work( i ) )
                 else
                    s = max( s, ( abs( work( n+i ) )+safe1 ) /( work( i )+safe1 ) )
                 end if
              end do
              berr( j ) = s
              ! test stopping criterion. continue iterating if
                 ! 1) the residual berr(j) is larger than machine epsilon, and
                 ! 2) berr(j) decreased by at least a factor of 2 during the
                    ! last iteration, and
                 ! 3) at most itmax iterations tried.
              if( berr( j )>eps .and. two*berr( j )<=lstres .and.count<=itmax ) then
                 ! update solution and try again.
                 call stdlib_spbtrs( uplo, n, kd, 1_ilp, afb, ldafb, work( n+1 ), n,info )
                 call stdlib_saxpy( n, one, work( n+1 ), 1_ilp, x( 1_ilp, j ), 1_ilp )
                 lstres = berr( j )
                 count = count + 1_ilp
                 go to 20
              end if
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(a))*
                 ! ( abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(a) is the inverse of a
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(a)*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(a)*abs(x) + abs(b) is less than safe2.
              ! use stdlib_slacn2 to estimate the infinity-norm of the matrix
                 ! inv(a) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) )))
              do i = 1, n
                 if( work( i )>safe2 ) then
                    work( i ) = abs( work( n+i ) ) + nz*eps*work( i )
                 else
                    work( i ) = abs( work( n+i ) ) + nz*eps*work( i ) + safe1
                 end if
              end do
              kase = 0_ilp
              100 continue
              call stdlib_slacn2( n, work( 2_ilp*n+1 ), work( n+1 ), iwork, ferr( j ),kase, isave )
                        
              if( kase/=0_ilp ) then
                 if( kase==1_ilp ) then
                    ! multiply by diag(w)*inv(a**t).
                    call stdlib_spbtrs( uplo, n, kd, 1_ilp, afb, ldafb, work( n+1 ), n,info )
                    do i = 1, n
                       work( n+i ) = work( n+i )*work( i )
                    end do
                 else if( kase==2_ilp ) then
                    ! multiply by inv(a)*diag(w).
                    do i = 1, n
                       work( n+i ) = work( n+i )*work( i )
                    end do
                    call stdlib_spbtrs( uplo, n, kd, 1_ilp, afb, ldafb, work( n+1 ), n,info )
                 end if
                 go to 100
              end if
              ! normalize error.
              lstres = zero
              do i = 1, n
                 lstres = max( lstres, abs( x( i, j ) ) )
              end do
              if( lstres/=zero )ferr( j ) = ferr( j ) / lstres
           end do loop_140
           return
     end subroutine stdlib_spbrfs

     pure module subroutine stdlib_dpbrfs( uplo, n, kd, nrhs, ab, ldab, afb, ldafb, b,ldb, x, ldx, ferr, &
     !! DPBRFS improves the computed solution to a system of linear
     !! equations when the coefficient matrix is symmetric positive definite
     !! and banded, and provides error bounds and backward error estimates
     !! for the solution.
               berr, work, iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldafb, ldb, ldx, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(in) :: ab(ldab,*), afb(ldafb,*), b(ldb,*)
           real(dp), intent(out) :: berr(*), ferr(*), work(*)
           real(dp), intent(inout) :: x(ldx,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: count, i, j, k, kase, l, nz
           real(dp) :: eps, lstres, s, safe1, safe2, safmin, xk
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kd<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( ldab<kd+1 ) then
              info = -6_ilp
           else if( ldafb<kd+1 ) then
              info = -8_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -10_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -12_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DPBRFS', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp .or. nrhs==0_ilp ) then
              do j = 1, nrhs
                 ferr( j ) = zero
                 berr( j ) = zero
              end do
              return
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = min( n+1, 2_ilp*kd+2 )
           eps = stdlib_dlamch( 'EPSILON' )
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           safe1 = nz*safmin
           safe2 = safe1 / eps
           ! do for each right hand side
           loop_140: do j = 1, nrhs
              count = 1_ilp
              lstres = three
              20 continue
              ! loop until stopping criterion is satisfied.
              ! compute residual r = b - a * x
              call stdlib_dcopy( n, b( 1_ilp, j ), 1_ilp, work( n+1 ), 1_ilp )
              call stdlib_dsbmv( uplo, n, kd, -one, ab, ldab, x( 1_ilp, j ), 1_ilp, one,work( n+1 ), 1_ilp )
                        
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(a)*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              do i = 1, n
                 work( i ) = abs( b( i, j ) )
              end do
              ! compute abs(a)*abs(x) + abs(b).
              if( upper ) then
                 do k = 1, n
                    s = zero
                    xk = abs( x( k, j ) )
                    l = kd + 1_ilp - k
                    do i = max( 1, k-kd ), k - 1
                       work( i ) = work( i ) + abs( ab( l+i, k ) )*xk
                       s = s + abs( ab( l+i, k ) )*abs( x( i, j ) )
                    end do
                    work( k ) = work( k ) + abs( ab( kd+1, k ) )*xk + s
                 end do
              else
                 do k = 1, n
                    s = zero
                    xk = abs( x( k, j ) )
                    work( k ) = work( k ) + abs( ab( 1_ilp, k ) )*xk
                    l = 1_ilp - k
                    do i = k + 1, min( n, k+kd )
                       work( i ) = work( i ) + abs( ab( l+i, k ) )*xk
                       s = s + abs( ab( l+i, k ) )*abs( x( i, j ) )
                    end do
                    work( k ) = work( k ) + s
                 end do
              end if
              s = zero
              do i = 1, n
                 if( work( i )>safe2 ) then
                    s = max( s, abs( work( n+i ) ) / work( i ) )
                 else
                    s = max( s, ( abs( work( n+i ) )+safe1 ) /( work( i )+safe1 ) )
                 end if
              end do
              berr( j ) = s
              ! test stopping criterion. continue iterating if
                 ! 1) the residual berr(j) is larger than machine epsilon, and
                 ! 2) berr(j) decreased by at least a factor of 2 during the
                    ! last iteration, and
                 ! 3) at most itmax iterations tried.
              if( berr( j )>eps .and. two*berr( j )<=lstres .and.count<=itmax ) then
                 ! update solution and try again.
                 call stdlib_dpbtrs( uplo, n, kd, 1_ilp, afb, ldafb, work( n+1 ), n,info )
                 call stdlib_daxpy( n, one, work( n+1 ), 1_ilp, x( 1_ilp, j ), 1_ilp )
                 lstres = berr( j )
                 count = count + 1_ilp
                 go to 20
              end if
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(a))*
                 ! ( abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(a) is the inverse of a
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(a)*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(a)*abs(x) + abs(b) is less than safe2.
              ! use stdlib_dlacn2 to estimate the infinity-norm of the matrix
                 ! inv(a) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) )))
              do i = 1, n
                 if( work( i )>safe2 ) then
                    work( i ) = abs( work( n+i ) ) + nz*eps*work( i )
                 else
                    work( i ) = abs( work( n+i ) ) + nz*eps*work( i ) + safe1
                 end if
              end do
              kase = 0_ilp
              100 continue
              call stdlib_dlacn2( n, work( 2_ilp*n+1 ), work( n+1 ), iwork, ferr( j ),kase, isave )
                        
              if( kase/=0_ilp ) then
                 if( kase==1_ilp ) then
                    ! multiply by diag(w)*inv(a**t).
                    call stdlib_dpbtrs( uplo, n, kd, 1_ilp, afb, ldafb, work( n+1 ), n,info )
                    do i = 1, n
                       work( n+i ) = work( n+i )*work( i )
                    end do
                 else if( kase==2_ilp ) then
                    ! multiply by inv(a)*diag(w).
                    do i = 1, n
                       work( n+i ) = work( n+i )*work( i )
                    end do
                    call stdlib_dpbtrs( uplo, n, kd, 1_ilp, afb, ldafb, work( n+1 ), n,info )
                 end if
                 go to 100
              end if
              ! normalize error.
              lstres = zero
              do i = 1, n
                 lstres = max( lstres, abs( x( i, j ) ) )
              end do
              if( lstres/=zero )ferr( j ) = ferr( j ) / lstres
           end do loop_140
           return
     end subroutine stdlib_dpbrfs


     pure module subroutine stdlib_cpbrfs( uplo, n, kd, nrhs, ab, ldab, afb, ldafb, b,ldb, x, ldx, ferr, &
     !! CPBRFS improves the computed solution to a system of linear
     !! equations when the coefficient matrix is Hermitian positive definite
     !! and banded, and provides error bounds and backward error estimates
     !! for the solution.
               berr, work, rwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldafb, ldb, ldx, n, nrhs
           ! Array Arguments 
           real(sp), intent(out) :: berr(*), ferr(*), rwork(*)
           complex(sp), intent(in) :: ab(ldab,*), afb(ldafb,*), b(ldb,*)
           complex(sp), intent(out) :: work(*)
           complex(sp), intent(inout) :: x(ldx,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: count, i, j, k, kase, l, nz
           real(sp) :: eps, lstres, s, safe1, safe2, safmin, xk
           complex(sp) :: zdum
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kd<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( ldab<kd+1 ) then
              info = -6_ilp
           else if( ldafb<kd+1 ) then
              info = -8_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -10_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -12_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CPBRFS', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp .or. nrhs==0_ilp ) then
              do j = 1, nrhs
                 ferr( j ) = zero
                 berr( j ) = zero
              end do
              return
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = min( n+1, 2_ilp*kd+2 )
           eps = stdlib_slamch( 'EPSILON' )
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           safe1 = nz*safmin
           safe2 = safe1 / eps
           ! do for each right hand side
           loop_140: do j = 1, nrhs
              count = 1_ilp
              lstres = three
              20 continue
              ! loop until stopping criterion is satisfied.
              ! compute residual r = b - a * x
              call stdlib_ccopy( n, b( 1_ilp, j ), 1_ilp, work, 1_ilp )
              call stdlib_chbmv( uplo, n, kd, -cone, ab, ldab, x( 1_ilp, j ), 1_ilp, cone,work, 1_ilp )
                        
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(a)*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              do i = 1, n
                 rwork( i ) = cabs1( b( i, j ) )
              end do
              ! compute abs(a)*abs(x) + abs(b).
              if( upper ) then
                 do k = 1, n
                    s = zero
                    xk = cabs1( x( k, j ) )
                    l = kd + 1_ilp - k
                    do i = max( 1, k-kd ), k - 1
                       rwork( i ) = rwork( i ) + cabs1( ab( l+i, k ) )*xk
                       s = s + cabs1( ab( l+i, k ) )*cabs1( x( i, j ) )
                    end do
                    rwork( k ) = rwork( k ) + abs( real( ab( kd+1, k ),KIND=sp) )*xk + s
                 end do
              else
                 do k = 1, n
                    s = zero
                    xk = cabs1( x( k, j ) )
                    rwork( k ) = rwork( k ) + abs( real( ab( 1_ilp, k ),KIND=sp) )*xk
                    l = 1_ilp - k
                    do i = k + 1, min( n, k+kd )
                       rwork( i ) = rwork( i ) + cabs1( ab( l+i, k ) )*xk
                       s = s + cabs1( ab( l+i, k ) )*cabs1( x( i, j ) )
                    end do
                    rwork( k ) = rwork( k ) + s
                 end do
              end if
              s = zero
              do i = 1, n
                 if( rwork( i )>safe2 ) then
                    s = max( s, cabs1( work( i ) ) / rwork( i ) )
                 else
                    s = max( s, ( cabs1( work( i ) )+safe1 ) /( rwork( i )+safe1 ) )
                 end if
              end do
              berr( j ) = s
              ! test stopping criterion. continue iterating if
                 ! 1) the residual berr(j) is larger than machine epsilon, and
                 ! 2) berr(j) decreased by at least a factor of 2 during the
                    ! last iteration, and
                 ! 3) at most itmax iterations tried.
              if( berr( j )>eps .and. two*berr( j )<=lstres .and.count<=itmax ) then
                 ! update solution and try again.
                 call stdlib_cpbtrs( uplo, n, kd, 1_ilp, afb, ldafb, work, n, info )
                 call stdlib_caxpy( n, cone, work, 1_ilp, x( 1_ilp, j ), 1_ilp )
                 lstres = berr( j )
                 count = count + 1_ilp
                 go to 20
              end if
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(a))*
                 ! ( abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(a) is the inverse of a
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(a)*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(a)*abs(x) + abs(b) is less than safe2.
              ! use stdlib_clacn2 to estimate the infinity-norm of the matrix
                 ! inv(a) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) )))
              do i = 1, n
                 if( rwork( i )>safe2 ) then
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i )
                 else
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i ) +safe1
                 end if
              end do
              kase = 0_ilp
              100 continue
              call stdlib_clacn2( n, work( n+1 ), work, ferr( j ), kase, isave )
              if( kase/=0_ilp ) then
                 if( kase==1_ilp ) then
                    ! multiply by diag(w)*inv(a**h).
                    call stdlib_cpbtrs( uplo, n, kd, 1_ilp, afb, ldafb, work, n, info )
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                 else if( kase==2_ilp ) then
                    ! multiply by inv(a)*diag(w).
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                    call stdlib_cpbtrs( uplo, n, kd, 1_ilp, afb, ldafb, work, n, info )
                 end if
                 go to 100
              end if
              ! normalize error.
              lstres = zero
              do i = 1, n
                 lstres = max( lstres, cabs1( x( i, j ) ) )
              end do
              if( lstres/=zero )ferr( j ) = ferr( j ) / lstres
           end do loop_140
           return
     end subroutine stdlib_cpbrfs

     pure module subroutine stdlib_zpbrfs( uplo, n, kd, nrhs, ab, ldab, afb, ldafb, b,ldb, x, ldx, ferr, &
     !! ZPBRFS improves the computed solution to a system of linear
     !! equations when the coefficient matrix is Hermitian positive definite
     !! and banded, and provides error bounds and backward error estimates
     !! for the solution.
               berr, work, rwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldafb, ldb, ldx, n, nrhs
           ! Array Arguments 
           real(dp), intent(out) :: berr(*), ferr(*), rwork(*)
           complex(dp), intent(in) :: ab(ldab,*), afb(ldafb,*), b(ldb,*)
           complex(dp), intent(out) :: work(*)
           complex(dp), intent(inout) :: x(ldx,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: count, i, j, k, kase, l, nz
           real(dp) :: eps, lstres, s, safe1, safe2, safmin, xk
           complex(dp) :: zdum
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kd<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( ldab<kd+1 ) then
              info = -6_ilp
           else if( ldafb<kd+1 ) then
              info = -8_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -10_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -12_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZPBRFS', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp .or. nrhs==0_ilp ) then
              do j = 1, nrhs
                 ferr( j ) = zero
                 berr( j ) = zero
              end do
              return
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = min( n+1, 2_ilp*kd+2 )
           eps = stdlib_dlamch( 'EPSILON' )
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           safe1 = nz*safmin
           safe2 = safe1 / eps
           ! do for each right hand side
           loop_140: do j = 1, nrhs
              count = 1_ilp
              lstres = three
              20 continue
              ! loop until stopping criterion is satisfied.
              ! compute residual r = b - a * x
              call stdlib_zcopy( n, b( 1_ilp, j ), 1_ilp, work, 1_ilp )
              call stdlib_zhbmv( uplo, n, kd, -cone, ab, ldab, x( 1_ilp, j ), 1_ilp, cone,work, 1_ilp )
                        
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(a)*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              do i = 1, n
                 rwork( i ) = cabs1( b( i, j ) )
              end do
              ! compute abs(a)*abs(x) + abs(b).
              if( upper ) then
                 do k = 1, n
                    s = zero
                    xk = cabs1( x( k, j ) )
                    l = kd + 1_ilp - k
                    do i = max( 1, k-kd ), k - 1
                       rwork( i ) = rwork( i ) + cabs1( ab( l+i, k ) )*xk
                       s = s + cabs1( ab( l+i, k ) )*cabs1( x( i, j ) )
                    end do
                    rwork( k ) = rwork( k ) + abs( real( ab( kd+1, k ),KIND=dp) )*xk + s
                 end do
              else
                 do k = 1, n
                    s = zero
                    xk = cabs1( x( k, j ) )
                    rwork( k ) = rwork( k ) + abs( real( ab( 1_ilp, k ),KIND=dp) )*xk
                    l = 1_ilp - k
                    do i = k + 1, min( n, k+kd )
                       rwork( i ) = rwork( i ) + cabs1( ab( l+i, k ) )*xk
                       s = s + cabs1( ab( l+i, k ) )*cabs1( x( i, j ) )
                    end do
                    rwork( k ) = rwork( k ) + s
                 end do
              end if
              s = zero
              do i = 1, n
                 if( rwork( i )>safe2 ) then
                    s = max( s, cabs1( work( i ) ) / rwork( i ) )
                 else
                    s = max( s, ( cabs1( work( i ) )+safe1 ) /( rwork( i )+safe1 ) )
                 end if
              end do
              berr( j ) = s
              ! test stopping criterion. continue iterating if
                 ! 1) the residual berr(j) is larger than machine epsilon, and
                 ! 2) berr(j) decreased by at least a factor of 2 during the
                    ! last iteration, and
                 ! 3) at most itmax iterations tried.
              if( berr( j )>eps .and. two*berr( j )<=lstres .and.count<=itmax ) then
                 ! update solution and try again.
                 call stdlib_zpbtrs( uplo, n, kd, 1_ilp, afb, ldafb, work, n, info )
                 call stdlib_zaxpy( n, cone, work, 1_ilp, x( 1_ilp, j ), 1_ilp )
                 lstres = berr( j )
                 count = count + 1_ilp
                 go to 20
              end if
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(a))*
                 ! ( abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(a) is the inverse of a
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(a)*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(a)*abs(x) + abs(b) is less than safe2.
              ! use stdlib_zlacn2 to estimate the infinity-norm of the matrix
                 ! inv(a) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) )))
              do i = 1, n
                 if( rwork( i )>safe2 ) then
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i )
                 else
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i ) +safe1
                 end if
              end do
              kase = 0_ilp
              100 continue
              call stdlib_zlacn2( n, work( n+1 ), work, ferr( j ), kase, isave )
              if( kase/=0_ilp ) then
                 if( kase==1_ilp ) then
                    ! multiply by diag(w)*inv(a**h).
                    call stdlib_zpbtrs( uplo, n, kd, 1_ilp, afb, ldafb, work, n, info )
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                 else if( kase==2_ilp ) then
                    ! multiply by inv(a)*diag(w).
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                    call stdlib_zpbtrs( uplo, n, kd, 1_ilp, afb, ldafb, work, n, info )
                 end if
                 go to 100
              end if
              ! normalize error.
              lstres = zero
              do i = 1, n
                 lstres = max( lstres, cabs1( x( i, j ) ) )
              end do
              if( lstres/=zero )ferr( j ) = ferr( j ) / lstres
           end do loop_140
           return
     end subroutine stdlib_zpbrfs




     pure module subroutine stdlib_spbequ( uplo, n, kd, ab, ldab, s, scond, amax, info )
     !! SPBEQU computes row and column scalings intended to equilibrate a
     !! symmetric positive definite band matrix A and reduce its condition
     !! number (with respect to the two-norm).  S contains the scale factors,
     !! S(i) = 1/sqrt(A(i,i)), chosen so that the scaled matrix B with
     !! elements B(i,j) = S(i)*A(i,j)*S(j) has ones on the diagonal.  This
     !! choice of S puts the condition number of B within a factor N of the
     !! smallest possible condition number over all possible diagonal
     !! scalings.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, n
           real(sp), intent(out) :: amax, scond
           ! Array Arguments 
           real(sp), intent(in) :: ab(ldab,*)
           real(sp), intent(out) :: s(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, j
           real(sp) :: smin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kd<0_ilp ) then
              info = -3_ilp
           else if( ldab<kd+1 ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SPBEQU', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              scond = one
              amax = zero
              return
           end if
           if( upper ) then
              j = kd + 1_ilp
           else
              j = 1_ilp
           end if
           ! initialize smin and amax.
           s( 1_ilp ) = ab( j, 1_ilp )
           smin = s( 1_ilp )
           amax = s( 1_ilp )
           ! find the minimum and maximum diagonal elements.
           do i = 2, n
              s( i ) = ab( j, i )
              smin = min( smin, s( i ) )
              amax = max( amax, s( i ) )
           end do
           if( smin<=zero ) then
              ! find the first non-positive diagonal element and return.
              do i = 1, n
                 if( s( i )<=zero ) then
                    info = i
                    return
                 end if
              end do
           else
              ! set the scale factors to the reciprocals
              ! of the diagonal elements.
              do i = 1, n
                 s( i ) = one / sqrt( s( i ) )
              end do
              ! compute scond = min(s(i)) / max(s(i))
              scond = sqrt( smin ) / sqrt( amax )
           end if
           return
     end subroutine stdlib_spbequ

     pure module subroutine stdlib_dpbequ( uplo, n, kd, ab, ldab, s, scond, amax, info )
     !! DPBEQU computes row and column scalings intended to equilibrate a
     !! symmetric positive definite band matrix A and reduce its condition
     !! number (with respect to the two-norm).  S contains the scale factors,
     !! S(i) = 1/sqrt(A(i,i)), chosen so that the scaled matrix B with
     !! elements B(i,j) = S(i)*A(i,j)*S(j) has ones on the diagonal.  This
     !! choice of S puts the condition number of B within a factor N of the
     !! smallest possible condition number over all possible diagonal
     !! scalings.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, n
           real(dp), intent(out) :: amax, scond
           ! Array Arguments 
           real(dp), intent(in) :: ab(ldab,*)
           real(dp), intent(out) :: s(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, j
           real(dp) :: smin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kd<0_ilp ) then
              info = -3_ilp
           else if( ldab<kd+1 ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DPBEQU', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              scond = one
              amax = zero
              return
           end if
           if( upper ) then
              j = kd + 1_ilp
           else
              j = 1_ilp
           end if
           ! initialize smin and amax.
           s( 1_ilp ) = ab( j, 1_ilp )
           smin = s( 1_ilp )
           amax = s( 1_ilp )
           ! find the minimum and maximum diagonal elements.
           do i = 2, n
              s( i ) = ab( j, i )
              smin = min( smin, s( i ) )
              amax = max( amax, s( i ) )
           end do
           if( smin<=zero ) then
              ! find the first non-positive diagonal element and return.
              do i = 1, n
                 if( s( i )<=zero ) then
                    info = i
                    return
                 end if
              end do
           else
              ! set the scale factors to the reciprocals
              ! of the diagonal elements.
              do i = 1, n
                 s( i ) = one / sqrt( s( i ) )
              end do
              ! compute scond = min(s(i)) / max(s(i))
              scond = sqrt( smin ) / sqrt( amax )
           end if
           return
     end subroutine stdlib_dpbequ


     pure module subroutine stdlib_cpbequ( uplo, n, kd, ab, ldab, s, scond, amax, info )
     !! CPBEQU computes row and column scalings intended to equilibrate a
     !! Hermitian positive definite band matrix A and reduce its condition
     !! number (with respect to the two-norm).  S contains the scale factors,
     !! S(i) = 1/sqrt(A(i,i)), chosen so that the scaled matrix B with
     !! elements B(i,j) = S(i)*A(i,j)*S(j) has ones on the diagonal.  This
     !! choice of S puts the condition number of B within a factor N of the
     !! smallest possible condition number over all possible diagonal
     !! scalings.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, n
           real(sp), intent(out) :: amax, scond
           ! Array Arguments 
           real(sp), intent(out) :: s(*)
           complex(sp), intent(in) :: ab(ldab,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, j
           real(sp) :: smin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kd<0_ilp ) then
              info = -3_ilp
           else if( ldab<kd+1 ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CPBEQU', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              scond = one
              amax = zero
              return
           end if
           if( upper ) then
              j = kd + 1_ilp
           else
              j = 1_ilp
           end if
           ! initialize smin and amax.
           s( 1_ilp ) = real( ab( j, 1_ilp ),KIND=sp)
           smin = s( 1_ilp )
           amax = s( 1_ilp )
           ! find the minimum and maximum diagonal elements.
           do i = 2, n
              s( i ) = real( ab( j, i ),KIND=sp)
              smin = min( smin, s( i ) )
              amax = max( amax, s( i ) )
           end do
           if( smin<=zero ) then
              ! find the first non-positive diagonal element and return.
              do i = 1, n
                 if( s( i )<=zero ) then
                    info = i
                    return
                 end if
              end do
           else
              ! set the scale factors to the reciprocals
              ! of the diagonal elements.
              do i = 1, n
                 s( i ) = one / sqrt( s( i ) )
              end do
              ! compute scond = min(s(i)) / max(s(i))
              scond = sqrt( smin ) / sqrt( amax )
           end if
           return
     end subroutine stdlib_cpbequ

     pure module subroutine stdlib_zpbequ( uplo, n, kd, ab, ldab, s, scond, amax, info )
     !! ZPBEQU computes row and column scalings intended to equilibrate a
     !! Hermitian positive definite band matrix A and reduce its condition
     !! number (with respect to the two-norm).  S contains the scale factors,
     !! S(i) = 1/sqrt(A(i,i)), chosen so that the scaled matrix B with
     !! elements B(i,j) = S(i)*A(i,j)*S(j) has ones on the diagonal.  This
     !! choice of S puts the condition number of B within a factor N of the
     !! smallest possible condition number over all possible diagonal
     !! scalings.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, n
           real(dp), intent(out) :: amax, scond
           ! Array Arguments 
           real(dp), intent(out) :: s(*)
           complex(dp), intent(in) :: ab(ldab,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, j
           real(dp) :: smin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kd<0_ilp ) then
              info = -3_ilp
           else if( ldab<kd+1 ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZPBEQU', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              scond = one
              amax = zero
              return
           end if
           if( upper ) then
              j = kd + 1_ilp
           else
              j = 1_ilp
           end if
           ! initialize smin and amax.
           s( 1_ilp ) = real( ab( j, 1_ilp ),KIND=dp)
           smin = s( 1_ilp )
           amax = s( 1_ilp )
           ! find the minimum and maximum diagonal elements.
           do i = 2, n
              s( i ) = real( ab( j, i ),KIND=dp)
              smin = min( smin, s( i ) )
              amax = max( amax, s( i ) )
           end do
           if( smin<=zero ) then
              ! find the first non-positive diagonal element and return.
              do i = 1, n
                 if( s( i )<=zero ) then
                    info = i
                    return
                 end if
              end do
           else
              ! set the scale factors to the reciprocals
              ! of the diagonal elements.
              do i = 1, n
                 s( i ) = one / sqrt( s( i ) )
              end do
              ! compute scond = min(s(i)) / max(s(i))
              scond = sqrt( smin ) / sqrt( amax )
           end if
           return
     end subroutine stdlib_zpbequ




     pure module subroutine stdlib_claqhb( uplo, n, kd, ab, ldab, s, scond, amax, equed )
     !! CLAQHB equilibrates an Hermitian band matrix A using the scaling
     !! factors in the vector S.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(out) :: equed
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: kd, ldab, n
           real(sp), intent(in) :: amax, scond
           ! Array Arguments 
           real(sp), intent(out) :: s(*)
           complex(sp), intent(inout) :: ab(ldab,*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: thresh = 0.1e+0_sp
           
           ! Local Scalars 
           integer(ilp) :: i, j
           real(sp) :: cj, large, small
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( n<=0_ilp ) then
              equed = 'N'
              return
           end if
           ! initialize large and small.
           small = stdlib_slamch( 'SAFE MINIMUM' ) / stdlib_slamch( 'PRECISION' )
           large = one / small
           if( scond>=thresh .and. amax>=small .and. amax<=large ) then
              ! no equilibration
              equed = 'N'
           else
              ! replace a by diag(s) * a * diag(s).
              if( stdlib_lsame( uplo, 'U' ) ) then
                 ! upper triangle of a is stored in band format.
                 do j = 1, n
                    cj = s( j )
                    do i = max( 1, j-kd ), j - 1
                       ab( kd+1+i-j, j ) = cj*s( i )*ab( kd+1+i-j, j )
                    end do
                    ab( kd+1, j ) = cj*cj*real( ab( kd+1, j ),KIND=sp)
                 end do
              else
                 ! lower triangle of a is stored.
                 do j = 1, n
                    cj = s( j )
                    ab( 1_ilp, j ) = cj*cj*real( ab( 1_ilp, j ),KIND=sp)
                    do i = j + 1, min( n, j+kd )
                       ab( 1_ilp+i-j, j ) = cj*s( i )*ab( 1_ilp+i-j, j )
                    end do
                 end do
              end if
              equed = 'Y'
           end if
           return
     end subroutine stdlib_claqhb

     pure module subroutine stdlib_zlaqhb( uplo, n, kd, ab, ldab, s, scond, amax, equed )
     !! ZLAQHB equilibrates a Hermitian band matrix A
     !! using the scaling factors in the vector S.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(out) :: equed
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: kd, ldab, n
           real(dp), intent(in) :: amax, scond
           ! Array Arguments 
           real(dp), intent(out) :: s(*)
           complex(dp), intent(inout) :: ab(ldab,*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: thresh = 0.1e+0_dp
           
           ! Local Scalars 
           integer(ilp) :: i, j
           real(dp) :: cj, large, small
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( n<=0_ilp ) then
              equed = 'N'
              return
           end if
           ! initialize large and small.
           small = stdlib_dlamch( 'SAFE MINIMUM' ) / stdlib_dlamch( 'PRECISION' )
           large = one / small
           if( scond>=thresh .and. amax>=small .and. amax<=large ) then
              ! no equilibration
              equed = 'N'
           else
              ! replace a by diag(s) * a * diag(s).
              if( stdlib_lsame( uplo, 'U' ) ) then
                 ! upper triangle of a is stored in band format.
                 do j = 1, n
                    cj = s( j )
                    do i = max( 1, j-kd ), j - 1
                       ab( kd+1+i-j, j ) = cj*s( i )*ab( kd+1+i-j, j )
                    end do
                    ab( kd+1, j ) = cj*cj*real( ab( kd+1, j ),KIND=dp)
                 end do
              else
                 ! lower triangle of a is stored.
                 do j = 1, n
                    cj = s( j )
                    ab( 1_ilp, j ) = cj*cj*real( ab( 1_ilp, j ),KIND=dp)
                    do i = j + 1, min( n, j+kd )
                       ab( 1_ilp+i-j, j ) = cj*s( i )*ab( 1_ilp+i-j, j )
                    end do
                 end do
              end if
              equed = 'Y'
           end if
           return
     end subroutine stdlib_zlaqhb




     pure module subroutine stdlib_sptcon( n, d, e, anorm, rcond, work, info )
     !! SPTCON computes the reciprocal of the condition number (in the
     !! 1-norm) of a real symmetric positive definite tridiagonal matrix
     !! using the factorization A = L*D*L**T or A = U**T*D*U computed by
     !! SPTTRF.
     !! Norm(inv(A)) is computed by a direct method, and the reciprocal of
     !! the condition number is computed as
     !! RCOND = 1 / (ANORM * norm(inv(A))).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(sp), intent(in) :: anorm
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           real(sp), intent(in) :: d(*), e(*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, ix
           real(sp) :: ainvnm
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
           else if( anorm<zero ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SPTCON', -info )
              return
           end if
           ! quick return if possible
           rcond = zero
           if( n==0_ilp ) then
              rcond = one
              return
           else if( anorm==zero ) then
              return
           end if
           ! check that d(1:n) is positive.
           do i = 1, n
              if( d( i )<=zero )return
           end do
           ! solve m(a) * x = e, where m(a) = (m(i,j)) is given by
              ! m(i,j) =  abs(a(i,j)), i = j,
              ! m(i,j) = -abs(a(i,j)), i .ne. j,
           ! and e = [ 1, 1, ..., 1 ]**t.  note m(a) = m(l)*d*m(l)**t.
           ! solve m(l) * x = e.
           work( 1_ilp ) = one
           do i = 2, n
              work( i ) = one + work( i-1 )*abs( e( i-1 ) )
           end do
           ! solve d * m(l)**t * x = b.
           work( n ) = work( n ) / d( n )
           do i = n - 1, 1, -1
              work( i ) = work( i ) / d( i ) + work( i+1 )*abs( e( i ) )
           end do
           ! compute ainvnm = max(x(i)), 1<=i<=n.
           ix = stdlib_isamax( n, work, 1_ilp )
           ainvnm = abs( work( ix ) )
           ! compute the reciprocal condition number.
           if( ainvnm/=zero )rcond = ( one / ainvnm ) / anorm
           return
     end subroutine stdlib_sptcon

     pure module subroutine stdlib_dptcon( n, d, e, anorm, rcond, work, info )
     !! DPTCON computes the reciprocal of the condition number (in the
     !! 1-norm) of a real symmetric positive definite tridiagonal matrix
     !! using the factorization A = L*D*L**T or A = U**T*D*U computed by
     !! DPTTRF.
     !! Norm(inv(A)) is computed by a direct method, and the reciprocal of
     !! the condition number is computed as
     !! RCOND = 1 / (ANORM * norm(inv(A))).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(dp), intent(in) :: anorm
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           real(dp), intent(in) :: d(*), e(*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, ix
           real(dp) :: ainvnm
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
           else if( anorm<zero ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DPTCON', -info )
              return
           end if
           ! quick return if possible
           rcond = zero
           if( n==0_ilp ) then
              rcond = one
              return
           else if( anorm==zero ) then
              return
           end if
           ! check that d(1:n) is positive.
           do i = 1, n
              if( d( i )<=zero )return
           end do
           ! solve m(a) * x = e, where m(a) = (m(i,j)) is given by
              ! m(i,j) =  abs(a(i,j)), i = j,
              ! m(i,j) = -abs(a(i,j)), i .ne. j,
           ! and e = [ 1, 1, ..., 1 ]**t.  note m(a) = m(l)*d*m(l)**t.
           ! solve m(l) * x = e.
           work( 1_ilp ) = one
           do i = 2, n
              work( i ) = one + work( i-1 )*abs( e( i-1 ) )
           end do
           ! solve d * m(l)**t * x = b.
           work( n ) = work( n ) / d( n )
           do i = n - 1, 1, -1
              work( i ) = work( i ) / d( i ) + work( i+1 )*abs( e( i ) )
           end do
           ! compute ainvnm = max(x(i)), 1<=i<=n.
           ix = stdlib_idamax( n, work, 1_ilp )
           ainvnm = abs( work( ix ) )
           ! compute the reciprocal condition number.
           if( ainvnm/=zero )rcond = ( one / ainvnm ) / anorm
           return
     end subroutine stdlib_dptcon


     pure module subroutine stdlib_cptcon( n, d, e, anorm, rcond, rwork, info )
     !! CPTCON computes the reciprocal of the condition number (in the
     !! 1-norm) of a complex Hermitian positive definite tridiagonal matrix
     !! using the factorization A = L*D*L**H or A = U**H*D*U computed by
     !! CPTTRF.
     !! Norm(inv(A)) is computed by a direct method, and the reciprocal of
     !! the condition number is computed as
     !! RCOND = 1 / (ANORM * norm(inv(A))).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(sp), intent(in) :: anorm
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           real(sp), intent(in) :: d(*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(in) :: e(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, ix
           real(sp) :: ainvnm
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
           else if( anorm<zero ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CPTCON', -info )
              return
           end if
           ! quick return if possible
           rcond = zero
           if( n==0_ilp ) then
              rcond = one
              return
           else if( anorm==zero ) then
              return
           end if
           ! check that d(1:n) is positive.
           do i = 1, n
              if( d( i )<=zero )return
           end do
           ! solve m(a) * x = e, where m(a) = (m(i,j)) is given by
              ! m(i,j) =  abs(a(i,j)), i = j,
              ! m(i,j) = -abs(a(i,j)), i .ne. j,
           ! and e = [ 1, 1, ..., 1 ]**t.  note m(a) = m(l)*d*m(l)**h.
           ! solve m(l) * x = e.
           rwork( 1_ilp ) = one
           do i = 2, n
              rwork( i ) = one + rwork( i-1 )*abs( e( i-1 ) )
           end do
           ! solve d * m(l)**h * x = b.
           rwork( n ) = rwork( n ) / d( n )
           do i = n - 1, 1, -1
              rwork( i ) = rwork( i ) / d( i ) + rwork( i+1 )*abs( e( i ) )
           end do
           ! compute ainvnm = max(x(i)), 1<=i<=n.
           ix = stdlib_isamax( n, rwork, 1_ilp )
           ainvnm = abs( rwork( ix ) )
           ! compute the reciprocal condition number.
           if( ainvnm/=zero )rcond = ( one / ainvnm ) / anorm
           return
     end subroutine stdlib_cptcon

     pure module subroutine stdlib_zptcon( n, d, e, anorm, rcond, rwork, info )
     !! ZPTCON computes the reciprocal of the condition number (in the
     !! 1-norm) of a complex Hermitian positive definite tridiagonal matrix
     !! using the factorization A = L*D*L**H or A = U**H*D*U computed by
     !! ZPTTRF.
     !! Norm(inv(A)) is computed by a direct method, and the reciprocal of
     !! the condition number is computed as
     !! RCOND = 1 / (ANORM * norm(inv(A))).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(dp), intent(in) :: anorm
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           real(dp), intent(in) :: d(*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(in) :: e(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, ix
           real(dp) :: ainvnm
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
           else if( anorm<zero ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZPTCON', -info )
              return
           end if
           ! quick return if possible
           rcond = zero
           if( n==0_ilp ) then
              rcond = one
              return
           else if( anorm==zero ) then
              return
           end if
           ! check that d(1:n) is positive.
           do i = 1, n
              if( d( i )<=zero )return
           end do
           ! solve m(a) * x = e, where m(a) = (m(i,j)) is given by
              ! m(i,j) =  abs(a(i,j)), i = j,
              ! m(i,j) = -abs(a(i,j)), i .ne. j,
           ! and e = [ 1, 1, ..., 1 ]**t.  note m(a) = m(l)*d*m(l)**h.
           ! solve m(l) * x = e.
           rwork( 1_ilp ) = one
           do i = 2, n
              rwork( i ) = one + rwork( i-1 )*abs( e( i-1 ) )
           end do
           ! solve d * m(l)**h * x = b.
           rwork( n ) = rwork( n ) / d( n )
           do i = n - 1, 1, -1
              rwork( i ) = rwork( i ) / d( i ) + rwork( i+1 )*abs( e( i ) )
           end do
           ! compute ainvnm = max(x(i)), 1<=i<=n.
           ix = stdlib_idamax( n, rwork, 1_ilp )
           ainvnm = abs( rwork( ix ) )
           ! compute the reciprocal condition number.
           if( ainvnm/=zero )rcond = ( one / ainvnm ) / anorm
           return
     end subroutine stdlib_zptcon




     pure module subroutine stdlib_spttrf( n, d, e, info )
     !! SPTTRF computes the L*D*L**T factorization of a real symmetric
     !! positive definite tridiagonal matrix A.  The factorization may also
     !! be regarded as having the form A = U**T*D*U.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(sp), intent(inout) :: d(*), e(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, i4
           real(sp) :: ei
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
              call stdlib_xerbla( 'SPTTRF', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! compute the l*d*l**t (or u**t*d*u) factorization of a.
           i4 = mod( n-1, 4_ilp )
           do i = 1, i4
              if( d( i )<=zero ) then
                 info = i
                 go to 30
              end if
              ei = e( i )
              e( i ) = ei / d( i )
              d( i+1 ) = d( i+1 ) - e( i )*ei
           end do
           loop_20: do i = i4 + 1, n - 4, 4
              ! drop out of the loop if d(i) <= 0: the matrix is not positive
              ! definite.
              if( d( i )<=zero ) then
                 info = i
                 go to 30
              end if
              ! solve for e(i) and d(i+1).
              ei = e( i )
              e( i ) = ei / d( i )
              d( i+1 ) = d( i+1 ) - e( i )*ei
              if( d( i+1 )<=zero ) then
                 info = i + 1_ilp
                 go to 30
              end if
              ! solve for e(i+1) and d(i+2).
              ei = e( i+1 )
              e( i+1 ) = ei / d( i+1 )
              d( i+2 ) = d( i+2 ) - e( i+1 )*ei
              if( d( i+2 )<=zero ) then
                 info = i + 2_ilp
                 go to 30
              end if
              ! solve for e(i+2) and d(i+3).
              ei = e( i+2 )
              e( i+2 ) = ei / d( i+2 )
              d( i+3 ) = d( i+3 ) - e( i+2 )*ei
              if( d( i+3 )<=zero ) then
                 info = i + 3_ilp
                 go to 30
              end if
              ! solve for e(i+3) and d(i+4).
              ei = e( i+3 )
              e( i+3 ) = ei / d( i+3 )
              d( i+4 ) = d( i+4 ) - e( i+3 )*ei
           end do loop_20
           ! check d(n) for positive definiteness.
           if( d( n )<=zero )info = n
           30 continue
           return
     end subroutine stdlib_spttrf

     pure module subroutine stdlib_dpttrf( n, d, e, info )
     !! DPTTRF computes the L*D*L**T factorization of a real symmetric
     !! positive definite tridiagonal matrix A.  The factorization may also
     !! be regarded as having the form A = U**T*D*U.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(dp), intent(inout) :: d(*), e(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, i4
           real(dp) :: ei
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
              call stdlib_xerbla( 'DPTTRF', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! compute the l*d*l**t (or u**t*d*u) factorization of a.
           i4 = mod( n-1, 4_ilp )
           do i = 1, i4
              if( d( i )<=zero ) then
                 info = i
                 go to 30
              end if
              ei = e( i )
              e( i ) = ei / d( i )
              d( i+1 ) = d( i+1 ) - e( i )*ei
           end do
           loop_20: do i = i4 + 1, n - 4, 4
              ! drop out of the loop if d(i) <= 0: the matrix is not positive
              ! definite.
              if( d( i )<=zero ) then
                 info = i
                 go to 30
              end if
              ! solve for e(i) and d(i+1).
              ei = e( i )
              e( i ) = ei / d( i )
              d( i+1 ) = d( i+1 ) - e( i )*ei
              if( d( i+1 )<=zero ) then
                 info = i + 1_ilp
                 go to 30
              end if
              ! solve for e(i+1) and d(i+2).
              ei = e( i+1 )
              e( i+1 ) = ei / d( i+1 )
              d( i+2 ) = d( i+2 ) - e( i+1 )*ei
              if( d( i+2 )<=zero ) then
                 info = i + 2_ilp
                 go to 30
              end if
              ! solve for e(i+2) and d(i+3).
              ei = e( i+2 )
              e( i+2 ) = ei / d( i+2 )
              d( i+3 ) = d( i+3 ) - e( i+2 )*ei
              if( d( i+3 )<=zero ) then
                 info = i + 3_ilp
                 go to 30
              end if
              ! solve for e(i+3) and d(i+4).
              ei = e( i+3 )
              e( i+3 ) = ei / d( i+3 )
              d( i+4 ) = d( i+4 ) - e( i+3 )*ei
           end do loop_20
           ! check d(n) for positive definiteness.
           if( d( n )<=zero )info = n
           30 continue
           return
     end subroutine stdlib_dpttrf


     pure module subroutine stdlib_cpttrf( n, d, e, info )
     !! CPTTRF computes the L*D*L**H factorization of a complex Hermitian
     !! positive definite tridiagonal matrix A.  The factorization may also
     !! be regarded as having the form A = U**H *D*U.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(sp), intent(inout) :: d(*)
           complex(sp), intent(inout) :: e(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, i4
           real(sp) :: eii, eir, f, g
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
              call stdlib_xerbla( 'CPTTRF', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! compute the l*d*l**h (or u**h *d*u) factorization of a.
           i4 = mod( n-1, 4_ilp )
           do i = 1, i4
              if( d( i )<=zero ) then
                 info = i
                 go to 20
              end if
              eir = real( e( i ),KIND=sp)
              eii = aimag( e( i ) )
              f = eir / d( i )
              g = eii / d( i )
              e( i ) = cmplx( f, g,KIND=sp)
              d( i+1 ) = d( i+1 ) - f*eir - g*eii
           end do
           loop_110: do i = i4+1, n - 4, 4
              ! drop out of the loop if d(i) <= 0: the matrix is not positive
              ! definite.
              if( d( i )<=zero ) then
                 info = i
                 go to 20
              end if
              ! solve for e(i) and d(i+1).
              eir = real( e( i ),KIND=sp)
              eii = aimag( e( i ) )
              f = eir / d( i )
              g = eii / d( i )
              e( i ) = cmplx( f, g,KIND=sp)
              d( i+1 ) = d( i+1 ) - f*eir - g*eii
              if( d( i+1 )<=zero ) then
                 info = i+1
                 go to 20
              end if
              ! solve for e(i+1) and d(i+2).
              eir = real( e( i+1 ),KIND=sp)
              eii = aimag( e( i+1 ) )
              f = eir / d( i+1 )
              g = eii / d( i+1 )
              e( i+1 ) = cmplx( f, g,KIND=sp)
              d( i+2 ) = d( i+2 ) - f*eir - g*eii
              if( d( i+2 )<=zero ) then
                 info = i+2
                 go to 20
              end if
              ! solve for e(i+2) and d(i+3).
              eir = real( e( i+2 ),KIND=sp)
              eii = aimag( e( i+2 ) )
              f = eir / d( i+2 )
              g = eii / d( i+2 )
              e( i+2 ) = cmplx( f, g,KIND=sp)
              d( i+3 ) = d( i+3 ) - f*eir - g*eii
              if( d( i+3 )<=zero ) then
                 info = i+3
                 go to 20
              end if
              ! solve for e(i+3) and d(i+4).
              eir = real( e( i+3 ),KIND=sp)
              eii = aimag( e( i+3 ) )
              f = eir / d( i+3 )
              g = eii / d( i+3 )
              e( i+3 ) = cmplx( f, g,KIND=sp)
              d( i+4 ) = d( i+4 ) - f*eir - g*eii
           end do loop_110
           ! check d(n) for positive definiteness.
           if( d( n )<=zero )info = n
           20 continue
           return
     end subroutine stdlib_cpttrf

     pure module subroutine stdlib_zpttrf( n, d, e, info )
     !! ZPTTRF computes the L*D*L**H factorization of a complex Hermitian
     !! positive definite tridiagonal matrix A.  The factorization may also
     !! be regarded as having the form A = U**H *D*U.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(dp), intent(inout) :: d(*)
           complex(dp), intent(inout) :: e(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, i4
           real(dp) :: eii, eir, f, g
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
              call stdlib_xerbla( 'ZPTTRF', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! compute the l*d*l**h (or u**h *d*u) factorization of a.
           i4 = mod( n-1, 4_ilp )
           do i = 1, i4
              if( d( i )<=zero ) then
                 info = i
                 go to 30
              end if
              eir = real( e( i ),KIND=dp)
              eii = aimag( e( i ) )
              f = eir / d( i )
              g = eii / d( i )
              e( i ) = cmplx( f, g,KIND=dp)
              d( i+1 ) = d( i+1 ) - f*eir - g*eii
           end do
           loop_20: do i = i4 + 1, n - 4, 4
              ! drop out of the loop if d(i) <= 0: the matrix is not positive
              ! definite.
              if( d( i )<=zero ) then
                 info = i
                 go to 30
              end if
              ! solve for e(i) and d(i+1).
              eir = real( e( i ),KIND=dp)
              eii = aimag( e( i ) )
              f = eir / d( i )
              g = eii / d( i )
              e( i ) = cmplx( f, g,KIND=dp)
              d( i+1 ) = d( i+1 ) - f*eir - g*eii
              if( d( i+1 )<=zero ) then
                 info = i + 1_ilp
                 go to 30
              end if
              ! solve for e(i+1) and d(i+2).
              eir = real( e( i+1 ),KIND=dp)
              eii = aimag( e( i+1 ) )
              f = eir / d( i+1 )
              g = eii / d( i+1 )
              e( i+1 ) = cmplx( f, g,KIND=dp)
              d( i+2 ) = d( i+2 ) - f*eir - g*eii
              if( d( i+2 )<=zero ) then
                 info = i + 2_ilp
                 go to 30
              end if
              ! solve for e(i+2) and d(i+3).
              eir = real( e( i+2 ),KIND=dp)
              eii = aimag( e( i+2 ) )
              f = eir / d( i+2 )
              g = eii / d( i+2 )
              e( i+2 ) = cmplx( f, g,KIND=dp)
              d( i+3 ) = d( i+3 ) - f*eir - g*eii
              if( d( i+3 )<=zero ) then
                 info = i + 3_ilp
                 go to 30
              end if
              ! solve for e(i+3) and d(i+4).
              eir = real( e( i+3 ),KIND=dp)
              eii = aimag( e( i+3 ) )
              f = eir / d( i+3 )
              g = eii / d( i+3 )
              e( i+3 ) = cmplx( f, g,KIND=dp)
              d( i+4 ) = d( i+4 ) - f*eir - g*eii
           end do loop_20
           ! check d(n) for positive definiteness.
           if( d( n )<=zero )info = n
           30 continue
           return
     end subroutine stdlib_zpttrf




     pure module subroutine stdlib_spttrs( n, nrhs, d, e, b, ldb, info )
     !! SPTTRS solves a tridiagonal system of the form
     !! A * X = B
     !! using the L*D*L**T factorization of A computed by SPTTRF.  D is a
     !! diagonal matrix specified in the vector D, L is a unit bidiagonal
     !! matrix whose subdiagonal is specified in the vector E, and X and B
     !! are N by NRHS matrices.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           real(sp), intent(inout) :: b(ldb,*)
           real(sp), intent(in) :: d(*), e(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: j, jb, nb
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
           else if( nrhs<0_ilp ) then
              info = -2_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SPTTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           ! determine the number of right-hand sides to solve at a time.
           if( nrhs==1_ilp ) then
              nb = 1_ilp
           else
              nb = max( 1_ilp, stdlib_ilaenv( 1_ilp, 'SPTTRS', ' ', n, nrhs, -1_ilp, -1_ilp ) )
           end if
           if( nb>=nrhs ) then
              call stdlib_sptts2( n, nrhs, d, e, b, ldb )
           else
              do j = 1, nrhs, nb
                 jb = min( nrhs-j+1, nb )
                 call stdlib_sptts2( n, jb, d, e, b( 1_ilp, j ), ldb )
              end do
           end if
           return
     end subroutine stdlib_spttrs

     pure module subroutine stdlib_dpttrs( n, nrhs, d, e, b, ldb, info )
     !! DPTTRS solves a tridiagonal system of the form
     !! A * X = B
     !! using the L*D*L**T factorization of A computed by DPTTRF.  D is a
     !! diagonal matrix specified in the vector D, L is a unit bidiagonal
     !! matrix whose subdiagonal is specified in the vector E, and X and B
     !! are N by NRHS matrices.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           real(dp), intent(inout) :: b(ldb,*)
           real(dp), intent(in) :: d(*), e(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: j, jb, nb
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
           else if( nrhs<0_ilp ) then
              info = -2_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DPTTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           ! determine the number of right-hand sides to solve at a time.
           if( nrhs==1_ilp ) then
              nb = 1_ilp
           else
              nb = max( 1_ilp, stdlib_ilaenv( 1_ilp, 'DPTTRS', ' ', n, nrhs, -1_ilp, -1_ilp ) )
           end if
           if( nb>=nrhs ) then
              call stdlib_dptts2( n, nrhs, d, e, b, ldb )
           else
              do j = 1, nrhs, nb
                 jb = min( nrhs-j+1, nb )
                 call stdlib_dptts2( n, jb, d, e, b( 1_ilp, j ), ldb )
              end do
           end if
           return
     end subroutine stdlib_dpttrs


     pure module subroutine stdlib_cpttrs( uplo, n, nrhs, d, e, b, ldb, info )
     !! CPTTRS solves a tridiagonal system of the form
     !! A * X = B
     !! using the factorization A = U**H*D*U or A = L*D*L**H computed by CPTTRF.
     !! D is a diagonal matrix specified in the vector D, U (or L) is a unit
     !! bidiagonal matrix whose superdiagonal (subdiagonal) is specified in
     !! the vector E, and X and B are N by NRHS matrices.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           real(sp), intent(in) :: d(*)
           complex(sp), intent(inout) :: b(ldb,*)
           complex(sp), intent(in) :: e(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: iuplo, j, jb, nb
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments.
           info = 0_ilp
           upper = ( uplo=='U' .or. uplo=='U' )
           if( .not.upper .and. .not.( uplo=='L' .or. uplo=='L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CPTTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           ! determine the number of right-hand sides to solve at a time.
           if( nrhs==1_ilp ) then
              nb = 1_ilp
           else
              nb = max( 1_ilp, stdlib_ilaenv( 1_ilp, 'CPTTRS', uplo, n, nrhs, -1_ilp, -1_ilp ) )
           end if
           ! decode uplo
           if( upper ) then
              iuplo = 1_ilp
           else
              iuplo = 0_ilp
           end if
           if( nb>=nrhs ) then
              call stdlib_cptts2( iuplo, n, nrhs, d, e, b, ldb )
           else
              do j = 1, nrhs, nb
                 jb = min( nrhs-j+1, nb )
                 call stdlib_cptts2( iuplo, n, jb, d, e, b( 1_ilp, j ), ldb )
              end do
           end if
           return
     end subroutine stdlib_cpttrs

     pure module subroutine stdlib_zpttrs( uplo, n, nrhs, d, e, b, ldb, info )
     !! ZPTTRS solves a tridiagonal system of the form
     !! A * X = B
     !! using the factorization A = U**H *D* U or A = L*D*L**H computed by ZPTTRF.
     !! D is a diagonal matrix specified in the vector D, U (or L) is a unit
     !! bidiagonal matrix whose superdiagonal (subdiagonal) is specified in
     !! the vector E, and X and B are N by NRHS matrices.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           real(dp), intent(in) :: d(*)
           complex(dp), intent(inout) :: b(ldb,*)
           complex(dp), intent(in) :: e(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: iuplo, j, jb, nb
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments.
           info = 0_ilp
           upper = ( uplo=='U' .or. uplo=='U' )
           if( .not.upper .and. .not.( uplo=='L' .or. uplo=='L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZPTTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           ! determine the number of right-hand sides to solve at a time.
           if( nrhs==1_ilp ) then
              nb = 1_ilp
           else
              nb = max( 1_ilp, stdlib_ilaenv( 1_ilp, 'ZPTTRS', uplo, n, nrhs, -1_ilp, -1_ilp ) )
           end if
           ! decode uplo
           if( upper ) then
              iuplo = 1_ilp
           else
              iuplo = 0_ilp
           end if
           if( nb>=nrhs ) then
              call stdlib_zptts2( iuplo, n, nrhs, d, e, b, ldb )
           else
              do j = 1, nrhs, nb
                 jb = min( nrhs-j+1, nb )
                 call stdlib_zptts2( iuplo, n, jb, d, e, b( 1_ilp, j ), ldb )
              end do
           end if
           return
     end subroutine stdlib_zpttrs




     pure module subroutine stdlib_sptts2( n, nrhs, d, e, b, ldb )
     !! SPTTS2 solves a tridiagonal system of the form
     !! A * X = B
     !! using the L*D*L**T factorization of A computed by SPTTRF.  D is a
     !! diagonal matrix specified in the vector D, L is a unit bidiagonal
     !! matrix whose subdiagonal is specified in the vector E, and X and B
     !! are N by NRHS matrices.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           real(sp), intent(inout) :: b(ldb,*)
           real(sp), intent(in) :: d(*), e(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, j
           ! Executable Statements 
           ! quick return if possible
           if( n<=1_ilp ) then
              if( n==1_ilp )call stdlib_sscal( nrhs, 1. / d( 1_ilp ), b, ldb )
              return
           end if
           ! solve a * x = b using the factorization a = l*d*l**t,
           ! overwriting each right hand side vector with its solution.
           do j = 1, nrhs
                 ! solve l * x = b.
              do i = 2, n
                 b( i, j ) = b( i, j ) - b( i-1, j )*e( i-1 )
              end do
                 ! solve d * l**t * x = b.
              b( n, j ) = b( n, j ) / d( n )
              do i = n - 1, 1, -1
                 b( i, j ) = b( i, j ) / d( i ) - b( i+1, j )*e( i )
              end do
           end do
           return
     end subroutine stdlib_sptts2

     pure module subroutine stdlib_dptts2( n, nrhs, d, e, b, ldb )
     !! DPTTS2 solves a tridiagonal system of the form
     !! A * X = B
     !! using the L*D*L**T factorization of A computed by DPTTRF.  D is a
     !! diagonal matrix specified in the vector D, L is a unit bidiagonal
     !! matrix whose subdiagonal is specified in the vector E, and X and B
     !! are N by NRHS matrices.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           real(dp), intent(inout) :: b(ldb,*)
           real(dp), intent(in) :: d(*), e(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, j
           ! Executable Statements 
           ! quick return if possible
           if( n<=1_ilp ) then
              if( n==1_ilp )call stdlib_dscal( nrhs, 1._dp / d( 1_ilp ), b, ldb )
              return
           end if
           ! solve a * x = b using the factorization a = l*d*l**t,
           ! overwriting each right hand side vector with its solution.
           do j = 1, nrhs
                 ! solve l * x = b.
              do i = 2, n
                 b( i, j ) = b( i, j ) - b( i-1, j )*e( i-1 )
              end do
                 ! solve d * l**t * x = b.
              b( n, j ) = b( n, j ) / d( n )
              do i = n - 1, 1, -1
                 b( i, j ) = b( i, j ) / d( i ) - b( i+1, j )*e( i )
              end do
           end do
           return
     end subroutine stdlib_dptts2


     pure module subroutine stdlib_cptts2( iuplo, n, nrhs, d, e, b, ldb )
     !! CPTTS2 solves a tridiagonal system of the form
     !! A * X = B
     !! using the factorization A = U**H*D*U or A = L*D*L**H computed by CPTTRF.
     !! D is a diagonal matrix specified in the vector D, U (or L) is a unit
     !! bidiagonal matrix whose superdiagonal (subdiagonal) is specified in
     !! the vector E, and X and B are N by NRHS matrices.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: iuplo, ldb, n, nrhs
           ! Array Arguments 
           real(sp), intent(in) :: d(*)
           complex(sp), intent(inout) :: b(ldb,*)
           complex(sp), intent(in) :: e(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, j
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( n<=1_ilp ) then
              if( n==1_ilp )call stdlib_csscal( nrhs, 1. / d( 1_ilp ), b, ldb )
              return
           end if
           if( iuplo==1_ilp ) then
              ! solve a * x = b using the factorization a = u**h *d*u,
              ! overwriting each right hand side vector with its solution.
              if( nrhs<=2_ilp ) then
                 j = 1_ilp
                 5 continue
                 ! solve u**h * x = b.
                 do i = 2, n
                    b( i, j ) = b( i, j ) - b( i-1, j )*conjg( e( i-1 ) )
                 end do
                 ! solve d * u * x = b.
                 do i = 1, n
                    b( i, j ) = b( i, j ) / d( i )
                 end do
                 do i = n - 1, 1, -1
                    b( i, j ) = b( i, j ) - b( i+1, j )*e( i )
                 end do
                 if( j<nrhs ) then
                    j = j + 1_ilp
                    go to 5
                 end if
              else
                 do j = 1, nrhs
                    ! solve u**h * x = b.
                    do i = 2, n
                       b( i, j ) = b( i, j ) - b( i-1, j )*conjg( e( i-1 ) )
                    end do
                    ! solve d * u * x = b.
                    b( n, j ) = b( n, j ) / d( n )
                    do i = n - 1, 1, -1
                       b( i, j ) = b( i, j ) / d( i ) - b( i+1, j )*e( i )
                    end do
                 end do
              end if
           else
              ! solve a * x = b using the factorization a = l*d*l**h,
              ! overwriting each right hand side vector with its solution.
              if( nrhs<=2_ilp ) then
                 j = 1_ilp
                 65 continue
                 ! solve l * x = b.
                 do i = 2, n
                    b( i, j ) = b( i, j ) - b( i-1, j )*e( i-1 )
                 end do
                 ! solve d * l**h * x = b.
                 do i = 1, n
                    b( i, j ) = b( i, j ) / d( i )
                 end do
                 do i = n - 1, 1, -1
                    b( i, j ) = b( i, j ) - b( i+1, j )*conjg( e( i ) )
                 end do
                 if( j<nrhs ) then
                    j = j + 1_ilp
                    go to 65
                 end if
              else
                 do j = 1, nrhs
                    ! solve l * x = b.
                    do i = 2, n
                       b( i, j ) = b( i, j ) - b( i-1, j )*e( i-1 )
                    end do
                    ! solve d * l**h * x = b.
                    b( n, j ) = b( n, j ) / d( n )
                    do i = n - 1, 1, -1
                       b( i, j ) = b( i, j ) / d( i ) -b( i+1, j )*conjg( e( i ) )
                    end do
                 end do
              end if
           end if
           return
     end subroutine stdlib_cptts2

     pure module subroutine stdlib_zptts2( iuplo, n, nrhs, d, e, b, ldb )
     !! ZPTTS2 solves a tridiagonal system of the form
     !! A * X = B
     !! using the factorization A = U**H *D*U or A = L*D*L**H computed by ZPTTRF.
     !! D is a diagonal matrix specified in the vector D, U (or L) is a unit
     !! bidiagonal matrix whose superdiagonal (subdiagonal) is specified in
     !! the vector E, and X and B are N by NRHS matrices.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: iuplo, ldb, n, nrhs
           ! Array Arguments 
           real(dp), intent(in) :: d(*)
           complex(dp), intent(inout) :: b(ldb,*)
           complex(dp), intent(in) :: e(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, j
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( n<=1_ilp ) then
              if( n==1_ilp )call stdlib_zdscal( nrhs, 1._dp / d( 1_ilp ), b, ldb )
              return
           end if
           if( iuplo==1_ilp ) then
              ! solve a * x = b using the factorization a = u**h *d*u,
              ! overwriting each right hand side vector with its solution.
              if( nrhs<=2_ilp ) then
                 j = 1_ilp
                 10 continue
                 ! solve u**h * x = b.
                 do i = 2, n
                    b( i, j ) = b( i, j ) - b( i-1, j )*conjg( e( i-1 ) )
                 end do
                 ! solve d * u * x = b.
                 do i = 1, n
                    b( i, j ) = b( i, j ) / d( i )
                 end do
                 do i = n - 1, 1, -1
                    b( i, j ) = b( i, j ) - b( i+1, j )*e( i )
                 end do
                 if( j<nrhs ) then
                    j = j + 1_ilp
                    go to 10
                 end if
              else
                 do j = 1, nrhs
                    ! solve u**h * x = b.
                    do i = 2, n
                       b( i, j ) = b( i, j ) - b( i-1, j )*conjg( e( i-1 ) )
                    end do
                    ! solve d * u * x = b.
                    b( n, j ) = b( n, j ) / d( n )
                    do i = n - 1, 1, -1
                       b( i, j ) = b( i, j ) / d( i ) - b( i+1, j )*e( i )
                    end do
                 end do
              end if
           else
              ! solve a * x = b using the factorization a = l*d*l**h,
              ! overwriting each right hand side vector with its solution.
              if( nrhs<=2_ilp ) then
                 j = 1_ilp
                 80 continue
                 ! solve l * x = b.
                 do i = 2, n
                    b( i, j ) = b( i, j ) - b( i-1, j )*e( i-1 )
                 end do
                 ! solve d * l**h * x = b.
                 do i = 1, n
                    b( i, j ) = b( i, j ) / d( i )
                 end do
                 do i = n - 1, 1, -1
                    b( i, j ) = b( i, j ) - b( i+1, j )*conjg( e( i ) )
                 end do
                 if( j<nrhs ) then
                    j = j + 1_ilp
                    go to 80
                 end if
              else
                 do j = 1, nrhs
                    ! solve l * x = b.
                    do i = 2, n
                       b( i, j ) = b( i, j ) - b( i-1, j )*e( i-1 )
                    end do
                    ! solve d * l**h * x = b.
                    b( n, j ) = b( n, j ) / d( n )
                    do i = n - 1, 1, -1
                       b( i, j ) = b( i, j ) / d( i ) -b( i+1, j )*conjg( e( i ) )
                    end do
                 end do
              end if
           end if
           return
     end subroutine stdlib_zptts2




     pure module subroutine stdlib_sptrfs( n, nrhs, d, e, df, ef, b, ldb, x, ldx, ferr,berr, work, info )
     !! SPTRFS improves the computed solution to a system of linear
     !! equations when the coefficient matrix is symmetric positive definite
     !! and tridiagonal, and provides error bounds and backward error
     !! estimates for the solution.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           ! Array Arguments 
           real(sp), intent(in) :: b(ldb,*), d(*), df(*), e(*), ef(*)
           real(sp), intent(out) :: berr(*), ferr(*), work(*)
           real(sp), intent(inout) :: x(ldx,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           
           
           
           ! Local Scalars 
           integer(ilp) :: count, i, ix, j, nz
           real(sp) :: bi, cx, dx, eps, ex, lstres, s, safe1, safe2, safmin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
           else if( nrhs<0_ilp ) then
              info = -2_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SPTRFS', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp .or. nrhs==0_ilp ) then
              do j = 1, nrhs
                 ferr( j ) = zero
                 berr( j ) = zero
              end do
              return
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = 4_ilp
           eps = stdlib_slamch( 'EPSILON' )
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           safe1 = nz*safmin
           safe2 = safe1 / eps
           ! do for each right hand side
           loop_90: do j = 1, nrhs
              count = 1_ilp
              lstres = three
              20 continue
              ! loop until stopping criterion is satisfied.
              ! compute residual r = b - a * x.  also compute
              ! abs(a)*abs(x) + abs(b) for use in the backward error bound.
              if( n==1_ilp ) then
                 bi = b( 1_ilp, j )
                 dx = d( 1_ilp )*x( 1_ilp, j )
                 work( n+1 ) = bi - dx
                 work( 1_ilp ) = abs( bi ) + abs( dx )
              else
                 bi = b( 1_ilp, j )
                 dx = d( 1_ilp )*x( 1_ilp, j )
                 ex = e( 1_ilp )*x( 2_ilp, j )
                 work( n+1 ) = bi - dx - ex
                 work( 1_ilp ) = abs( bi ) + abs( dx ) + abs( ex )
                 do i = 2, n - 1
                    bi = b( i, j )
                    cx = e( i-1 )*x( i-1, j )
                    dx = d( i )*x( i, j )
                    ex = e( i )*x( i+1, j )
                    work( n+i ) = bi - cx - dx - ex
                    work( i ) = abs( bi ) + abs( cx ) + abs( dx ) + abs( ex )
                 end do
                 bi = b( n, j )
                 cx = e( n-1 )*x( n-1, j )
                 dx = d( n )*x( n, j )
                 work( n+n ) = bi - cx - dx
                 work( n ) = abs( bi ) + abs( cx ) + abs( dx )
              end if
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(a)*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              s = zero
              do i = 1, n
                 if( work( i )>safe2 ) then
                    s = max( s, abs( work( n+i ) ) / work( i ) )
                 else
                    s = max( s, ( abs( work( n+i ) )+safe1 ) /( work( i )+safe1 ) )
                 end if
              end do
              berr( j ) = s
              ! test stopping criterion. continue iterating if
                 ! 1) the residual berr(j) is larger than machine epsilon, and
                 ! 2) berr(j) decreased by at least a factor of 2 during the
                    ! last iteration, and
                 ! 3) at most itmax iterations tried.
              if( berr( j )>eps .and. two*berr( j )<=lstres .and.count<=itmax ) then
                 ! update solution and try again.
                 call stdlib_spttrs( n, 1_ilp, df, ef, work( n+1 ), n, info )
                 call stdlib_saxpy( n, one, work( n+1 ), 1_ilp, x( 1_ilp, j ), 1_ilp )
                 lstres = berr( j )
                 count = count + 1_ilp
                 go to 20
              end if
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(a))*
                 ! ( abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(a) is the inverse of a
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(a)*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(a)*abs(x) + abs(b) is less than safe2.
              do i = 1, n
                 if( work( i )>safe2 ) then
                    work( i ) = abs( work( n+i ) ) + nz*eps*work( i )
                 else
                    work( i ) = abs( work( n+i ) ) + nz*eps*work( i ) + safe1
                 end if
              end do
              ix = stdlib_isamax( n, work, 1_ilp )
              ferr( j ) = work( ix )
              ! estimate the norm of inv(a).
              ! solve m(a) * x = e, where m(a) = (m(i,j)) is given by
                 ! m(i,j) =  abs(a(i,j)), i = j,
                 ! m(i,j) = -abs(a(i,j)), i .ne. j,
              ! and e = [ 1, 1, ..., 1 ]**t.  note m(a) = m(l)*d*m(l)**t.
              ! solve m(l) * x = e.
              work( 1_ilp ) = one
              do i = 2, n
                 work( i ) = one + work( i-1 )*abs( ef( i-1 ) )
              end do
              ! solve d * m(l)**t * x = b.
              work( n ) = work( n ) / df( n )
              do i = n - 1, 1, -1
                 work( i ) = work( i ) / df( i ) + work( i+1 )*abs( ef( i ) )
              end do
              ! compute norm(inv(a)) = max(x(i)), 1<=i<=n.
              ix = stdlib_isamax( n, work, 1_ilp )
              ferr( j ) = ferr( j )*abs( work( ix ) )
              ! normalize error.
              lstres = zero
              do i = 1, n
                 lstres = max( lstres, abs( x( i, j ) ) )
              end do
              if( lstres/=zero )ferr( j ) = ferr( j ) / lstres
           end do loop_90
           return
     end subroutine stdlib_sptrfs

     pure module subroutine stdlib_dptrfs( n, nrhs, d, e, df, ef, b, ldb, x, ldx, ferr,berr, work, info )
     !! DPTRFS improves the computed solution to a system of linear
     !! equations when the coefficient matrix is symmetric positive definite
     !! and tridiagonal, and provides error bounds and backward error
     !! estimates for the solution.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           ! Array Arguments 
           real(dp), intent(in) :: b(ldb,*), d(*), df(*), e(*), ef(*)
           real(dp), intent(out) :: berr(*), ferr(*), work(*)
           real(dp), intent(inout) :: x(ldx,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           
           
           
           ! Local Scalars 
           integer(ilp) :: count, i, ix, j, nz
           real(dp) :: bi, cx, dx, eps, ex, lstres, s, safe1, safe2, safmin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
           else if( nrhs<0_ilp ) then
              info = -2_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DPTRFS', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp .or. nrhs==0_ilp ) then
              do j = 1, nrhs
                 ferr( j ) = zero
                 berr( j ) = zero
              end do
              return
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = 4_ilp
           eps = stdlib_dlamch( 'EPSILON' )
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           safe1 = nz*safmin
           safe2 = safe1 / eps
           ! do for each right hand side
           loop_90: do j = 1, nrhs
              count = 1_ilp
              lstres = three
              20 continue
              ! loop until stopping criterion is satisfied.
              ! compute residual r = b - a * x.  also compute
              ! abs(a)*abs(x) + abs(b) for use in the backward error bound.
              if( n==1_ilp ) then
                 bi = b( 1_ilp, j )
                 dx = d( 1_ilp )*x( 1_ilp, j )
                 work( n+1 ) = bi - dx
                 work( 1_ilp ) = abs( bi ) + abs( dx )
              else
                 bi = b( 1_ilp, j )
                 dx = d( 1_ilp )*x( 1_ilp, j )
                 ex = e( 1_ilp )*x( 2_ilp, j )
                 work( n+1 ) = bi - dx - ex
                 work( 1_ilp ) = abs( bi ) + abs( dx ) + abs( ex )
                 do i = 2, n - 1
                    bi = b( i, j )
                    cx = e( i-1 )*x( i-1, j )
                    dx = d( i )*x( i, j )
                    ex = e( i )*x( i+1, j )
                    work( n+i ) = bi - cx - dx - ex
                    work( i ) = abs( bi ) + abs( cx ) + abs( dx ) + abs( ex )
                 end do
                 bi = b( n, j )
                 cx = e( n-1 )*x( n-1, j )
                 dx = d( n )*x( n, j )
                 work( n+n ) = bi - cx - dx
                 work( n ) = abs( bi ) + abs( cx ) + abs( dx )
              end if
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(a)*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              s = zero
              do i = 1, n
                 if( work( i )>safe2 ) then
                    s = max( s, abs( work( n+i ) ) / work( i ) )
                 else
                    s = max( s, ( abs( work( n+i ) )+safe1 ) /( work( i )+safe1 ) )
                 end if
              end do
              berr( j ) = s
              ! test stopping criterion. continue iterating if
                 ! 1) the residual berr(j) is larger than machine epsilon, and
                 ! 2) berr(j) decreased by at least a factor of 2 during the
                    ! last iteration, and
                 ! 3) at most itmax iterations tried.
              if( berr( j )>eps .and. two*berr( j )<=lstres .and.count<=itmax ) then
                 ! update solution and try again.
                 call stdlib_dpttrs( n, 1_ilp, df, ef, work( n+1 ), n, info )
                 call stdlib_daxpy( n, one, work( n+1 ), 1_ilp, x( 1_ilp, j ), 1_ilp )
                 lstres = berr( j )
                 count = count + 1_ilp
                 go to 20
              end if
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(a))*
                 ! ( abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(a) is the inverse of a
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(a)*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(a)*abs(x) + abs(b) is less than safe2.
              do i = 1, n
                 if( work( i )>safe2 ) then
                    work( i ) = abs( work( n+i ) ) + nz*eps*work( i )
                 else
                    work( i ) = abs( work( n+i ) ) + nz*eps*work( i ) + safe1
                 end if
              end do
              ix = stdlib_idamax( n, work, 1_ilp )
              ferr( j ) = work( ix )
              ! estimate the norm of inv(a).
              ! solve m(a) * x = e, where m(a) = (m(i,j)) is given by
                 ! m(i,j) =  abs(a(i,j)), i = j,
                 ! m(i,j) = -abs(a(i,j)), i .ne. j,
              ! and e = [ 1, 1, ..., 1 ]**t.  note m(a) = m(l)*d*m(l)**t.
              ! solve m(l) * x = e.
              work( 1_ilp ) = one
              do i = 2, n
                 work( i ) = one + work( i-1 )*abs( ef( i-1 ) )
              end do
              ! solve d * m(l)**t * x = b.
              work( n ) = work( n ) / df( n )
              do i = n - 1, 1, -1
                 work( i ) = work( i ) / df( i ) + work( i+1 )*abs( ef( i ) )
              end do
              ! compute norm(inv(a)) = max(x(i)), 1<=i<=n.
              ix = stdlib_idamax( n, work, 1_ilp )
              ferr( j ) = ferr( j )*abs( work( ix ) )
              ! normalize error.
              lstres = zero
              do i = 1, n
                 lstres = max( lstres, abs( x( i, j ) ) )
              end do
              if( lstres/=zero )ferr( j ) = ferr( j ) / lstres
           end do loop_90
           return
     end subroutine stdlib_dptrfs


     pure module subroutine stdlib_cptrfs( uplo, n, nrhs, d, e, df, ef, b, ldb, x, ldx,ferr, berr, work, &
     !! CPTRFS improves the computed solution to a system of linear
     !! equations when the coefficient matrix is Hermitian positive definite
     !! and tridiagonal, and provides error bounds and backward error
     !! estimates for the solution.
               rwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           ! Array Arguments 
           real(sp), intent(out) :: berr(*), ferr(*), rwork(*)
           real(sp), intent(in) :: d(*), df(*)
           complex(sp), intent(in) :: b(ldb,*), e(*), ef(*)
           complex(sp), intent(out) :: work(*)
           complex(sp), intent(inout) :: x(ldx,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: count, i, ix, j, nz
           real(sp) :: eps, lstres, s, safe1, safe2, safmin
           complex(sp) :: bi, cx, dx, ex, zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CPTRFS', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp .or. nrhs==0_ilp ) then
              do j = 1, nrhs
                 ferr( j ) = zero
                 berr( j ) = zero
              end do
              return
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = 4_ilp
           eps = stdlib_slamch( 'EPSILON' )
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           safe1 = nz*safmin
           safe2 = safe1 / eps
           ! do for each right hand side
           loop_100: do j = 1, nrhs
              count = 1_ilp
              lstres = three
              20 continue
              ! loop until stopping criterion is satisfied.
              ! compute residual r = b - a * x.  also compute
              ! abs(a)*abs(x) + abs(b) for use in the backward error bound.
              if( upper ) then
                 if( n==1_ilp ) then
                    bi = b( 1_ilp, j )
                    dx = d( 1_ilp )*x( 1_ilp, j )
                    work( 1_ilp ) = bi - dx
                    rwork( 1_ilp ) = cabs1( bi ) + cabs1( dx )
                 else
                    bi = b( 1_ilp, j )
                    dx = d( 1_ilp )*x( 1_ilp, j )
                    ex = e( 1_ilp )*x( 2_ilp, j )
                    work( 1_ilp ) = bi - dx - ex
                    rwork( 1_ilp ) = cabs1( bi ) + cabs1( dx ) +cabs1( e( 1_ilp ) )*cabs1( x( 2_ilp, j ) )
                              
                    do i = 2, n - 1
                       bi = b( i, j )
                       cx = conjg( e( i-1 ) )*x( i-1, j )
                       dx = d( i )*x( i, j )
                       ex = e( i )*x( i+1, j )
                       work( i ) = bi - cx - dx - ex
                       rwork( i ) = cabs1( bi ) +cabs1( e( i-1 ) )*cabs1( x( i-1, j ) ) +cabs1( &
                                 dx ) + cabs1( e( i ) )*cabs1( x( i+1, j ) )
                    end do
                    bi = b( n, j )
                    cx = conjg( e( n-1 ) )*x( n-1, j )
                    dx = d( n )*x( n, j )
                    work( n ) = bi - cx - dx
                    rwork( n ) = cabs1( bi ) + cabs1( e( n-1 ) )*cabs1( x( n-1, j ) ) + cabs1( dx &
                              )
                 end if
              else
                 if( n==1_ilp ) then
                    bi = b( 1_ilp, j )
                    dx = d( 1_ilp )*x( 1_ilp, j )
                    work( 1_ilp ) = bi - dx
                    rwork( 1_ilp ) = cabs1( bi ) + cabs1( dx )
                 else
                    bi = b( 1_ilp, j )
                    dx = d( 1_ilp )*x( 1_ilp, j )
                    ex = conjg( e( 1_ilp ) )*x( 2_ilp, j )
                    work( 1_ilp ) = bi - dx - ex
                    rwork( 1_ilp ) = cabs1( bi ) + cabs1( dx ) +cabs1( e( 1_ilp ) )*cabs1( x( 2_ilp, j ) )
                              
                    do i = 2, n - 1
                       bi = b( i, j )
                       cx = e( i-1 )*x( i-1, j )
                       dx = d( i )*x( i, j )
                       ex = conjg( e( i ) )*x( i+1, j )
                       work( i ) = bi - cx - dx - ex
                       rwork( i ) = cabs1( bi ) +cabs1( e( i-1 ) )*cabs1( x( i-1, j ) ) +cabs1( &
                                 dx ) + cabs1( e( i ) )*cabs1( x( i+1, j ) )
                    end do
                    bi = b( n, j )
                    cx = e( n-1 )*x( n-1, j )
                    dx = d( n )*x( n, j )
                    work( n ) = bi - cx - dx
                    rwork( n ) = cabs1( bi ) + cabs1( e( n-1 ) )*cabs1( x( n-1, j ) ) + cabs1( dx &
                              )
                 end if
              end if
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(a)*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              s = zero
              do i = 1, n
                 if( rwork( i )>safe2 ) then
                    s = max( s, cabs1( work( i ) ) / rwork( i ) )
                 else
                    s = max( s, ( cabs1( work( i ) )+safe1 ) /( rwork( i )+safe1 ) )
                 end if
              end do
              berr( j ) = s
              ! test stopping criterion. continue iterating if
                 ! 1) the residual berr(j) is larger than machine epsilon, and
                 ! 2) berr(j) decreased by at least a factor of 2 during the
                    ! last iteration, and
                 ! 3) at most itmax iterations tried.
              if( berr( j )>eps .and. two*berr( j )<=lstres .and.count<=itmax ) then
                 ! update solution and try again.
                 call stdlib_cpttrs( uplo, n, 1_ilp, df, ef, work, n, info )
                 call stdlib_caxpy( n, cmplx( one,KIND=sp), work, 1_ilp, x( 1_ilp, j ), 1_ilp )
                 lstres = berr( j )
                 count = count + 1_ilp
                 go to 20
              end if
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(a))*
                 ! ( abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(a) is the inverse of a
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(a)*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(a)*abs(x) + abs(b) is less than safe2.
              do i = 1, n
                 if( rwork( i )>safe2 ) then
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i )
                 else
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i ) +safe1
                 end if
              end do
              ix = stdlib_isamax( n, rwork, 1_ilp )
              ferr( j ) = rwork( ix )
              ! estimate the norm of inv(a).
              ! solve m(a) * x = e, where m(a) = (m(i,j)) is given by
                 ! m(i,j) =  abs(a(i,j)), i = j,
                 ! m(i,j) = -abs(a(i,j)), i .ne. j,
              ! and e = [ 1, 1, ..., 1 ]**t.  note m(a) = m(l)*d*m(l)**h.
              ! solve m(l) * x = e.
              rwork( 1_ilp ) = one
              do i = 2, n
                 rwork( i ) = one + rwork( i-1 )*abs( ef( i-1 ) )
              end do
              ! solve d * m(l)**h * x = b.
              rwork( n ) = rwork( n ) / df( n )
              do i = n - 1, 1, -1
                 rwork( i ) = rwork( i ) / df( i ) +rwork( i+1 )*abs( ef( i ) )
              end do
              ! compute norm(inv(a)) = max(x(i)), 1<=i<=n.
              ix = stdlib_isamax( n, rwork, 1_ilp )
              ferr( j ) = ferr( j )*abs( rwork( ix ) )
              ! normalize error.
              lstres = zero
              do i = 1, n
                 lstres = max( lstres, abs( x( i, j ) ) )
              end do
              if( lstres/=zero )ferr( j ) = ferr( j ) / lstres
           end do loop_100
           return
     end subroutine stdlib_cptrfs

     pure module subroutine stdlib_zptrfs( uplo, n, nrhs, d, e, df, ef, b, ldb, x, ldx,ferr, berr, work, &
     !! ZPTRFS improves the computed solution to a system of linear
     !! equations when the coefficient matrix is Hermitian positive definite
     !! and tridiagonal, and provides error bounds and backward error
     !! estimates for the solution.
               rwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           ! Array Arguments 
           real(dp), intent(out) :: berr(*), ferr(*), rwork(*)
           real(dp), intent(in) :: d(*), df(*)
           complex(dp), intent(in) :: b(ldb,*), e(*), ef(*)
           complex(dp), intent(out) :: work(*)
           complex(dp), intent(inout) :: x(ldx,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: count, i, ix, j, nz
           real(dp) :: eps, lstres, s, safe1, safe2, safmin
           complex(dp) :: bi, cx, dx, ex, zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZPTRFS', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp .or. nrhs==0_ilp ) then
              do j = 1, nrhs
                 ferr( j ) = zero
                 berr( j ) = zero
              end do
              return
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = 4_ilp
           eps = stdlib_dlamch( 'EPSILON' )
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           safe1 = nz*safmin
           safe2 = safe1 / eps
           ! do for each right hand side
           loop_100: do j = 1, nrhs
              count = 1_ilp
              lstres = three
              20 continue
              ! loop until stopping criterion is satisfied.
              ! compute residual r = b - a * x.  also compute
              ! abs(a)*abs(x) + abs(b) for use in the backward error bound.
              if( upper ) then
                 if( n==1_ilp ) then
                    bi = b( 1_ilp, j )
                    dx = d( 1_ilp )*x( 1_ilp, j )
                    work( 1_ilp ) = bi - dx
                    rwork( 1_ilp ) = cabs1( bi ) + cabs1( dx )
                 else
                    bi = b( 1_ilp, j )
                    dx = d( 1_ilp )*x( 1_ilp, j )
                    ex = e( 1_ilp )*x( 2_ilp, j )
                    work( 1_ilp ) = bi - dx - ex
                    rwork( 1_ilp ) = cabs1( bi ) + cabs1( dx ) +cabs1( e( 1_ilp ) )*cabs1( x( 2_ilp, j ) )
                              
                    do i = 2, n - 1
                       bi = b( i, j )
                       cx = conjg( e( i-1 ) )*x( i-1, j )
                       dx = d( i )*x( i, j )
                       ex = e( i )*x( i+1, j )
                       work( i ) = bi - cx - dx - ex
                       rwork( i ) = cabs1( bi ) +cabs1( e( i-1 ) )*cabs1( x( i-1, j ) ) +cabs1( &
                                 dx ) + cabs1( e( i ) )*cabs1( x( i+1, j ) )
                    end do
                    bi = b( n, j )
                    cx = conjg( e( n-1 ) )*x( n-1, j )
                    dx = d( n )*x( n, j )
                    work( n ) = bi - cx - dx
                    rwork( n ) = cabs1( bi ) + cabs1( e( n-1 ) )*cabs1( x( n-1, j ) ) + cabs1( dx &
                              )
                 end if
              else
                 if( n==1_ilp ) then
                    bi = b( 1_ilp, j )
                    dx = d( 1_ilp )*x( 1_ilp, j )
                    work( 1_ilp ) = bi - dx
                    rwork( 1_ilp ) = cabs1( bi ) + cabs1( dx )
                 else
                    bi = b( 1_ilp, j )
                    dx = d( 1_ilp )*x( 1_ilp, j )
                    ex = conjg( e( 1_ilp ) )*x( 2_ilp, j )
                    work( 1_ilp ) = bi - dx - ex
                    rwork( 1_ilp ) = cabs1( bi ) + cabs1( dx ) +cabs1( e( 1_ilp ) )*cabs1( x( 2_ilp, j ) )
                              
                    do i = 2, n - 1
                       bi = b( i, j )
                       cx = e( i-1 )*x( i-1, j )
                       dx = d( i )*x( i, j )
                       ex = conjg( e( i ) )*x( i+1, j )
                       work( i ) = bi - cx - dx - ex
                       rwork( i ) = cabs1( bi ) +cabs1( e( i-1 ) )*cabs1( x( i-1, j ) ) +cabs1( &
                                 dx ) + cabs1( e( i ) )*cabs1( x( i+1, j ) )
                    end do
                    bi = b( n, j )
                    cx = e( n-1 )*x( n-1, j )
                    dx = d( n )*x( n, j )
                    work( n ) = bi - cx - dx
                    rwork( n ) = cabs1( bi ) + cabs1( e( n-1 ) )*cabs1( x( n-1, j ) ) + cabs1( dx &
                              )
                 end if
              end if
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(a)*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              s = zero
              do i = 1, n
                 if( rwork( i )>safe2 ) then
                    s = max( s, cabs1( work( i ) ) / rwork( i ) )
                 else
                    s = max( s, ( cabs1( work( i ) )+safe1 ) /( rwork( i )+safe1 ) )
                 end if
              end do
              berr( j ) = s
              ! test stopping criterion. continue iterating if
                 ! 1) the residual berr(j) is larger than machine epsilon, and
                 ! 2) berr(j) decreased by at least a factor of 2 during the
                    ! last iteration, and
                 ! 3) at most itmax iterations tried.
              if( berr( j )>eps .and. two*berr( j )<=lstres .and.count<=itmax ) then
                 ! update solution and try again.
                 call stdlib_zpttrs( uplo, n, 1_ilp, df, ef, work, n, info )
                 call stdlib_zaxpy( n, cmplx( one,KIND=dp), work, 1_ilp, x( 1_ilp, j ), 1_ilp )
                 lstres = berr( j )
                 count = count + 1_ilp
                 go to 20
              end if
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(a))*
                 ! ( abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(a) is the inverse of a
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(a)*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(a)*abs(x) + abs(b) is less than safe2.
              do i = 1, n
                 if( rwork( i )>safe2 ) then
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i )
                 else
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i ) +safe1
                 end if
              end do
              ix = stdlib_idamax( n, rwork, 1_ilp )
              ferr( j ) = rwork( ix )
              ! estimate the norm of inv(a).
              ! solve m(a) * x = e, where m(a) = (m(i,j)) is given by
                 ! m(i,j) =  abs(a(i,j)), i = j,
                 ! m(i,j) = -abs(a(i,j)), i .ne. j,
              ! and e = [ 1, 1, ..., 1 ]**t.  note m(a) = m(l)*d*m(l)**h.
              ! solve m(l) * x = e.
              rwork( 1_ilp ) = one
              do i = 2, n
                 rwork( i ) = one + rwork( i-1 )*abs( ef( i-1 ) )
              end do
              ! solve d * m(l)**h * x = b.
              rwork( n ) = rwork( n ) / df( n )
              do i = n - 1, 1, -1
                 rwork( i ) = rwork( i ) / df( i ) +rwork( i+1 )*abs( ef( i ) )
              end do
              ! compute norm(inv(a)) = max(x(i)), 1<=i<=n.
              ix = stdlib_idamax( n, rwork, 1_ilp )
              ferr( j ) = ferr( j )*abs( rwork( ix ) )
              ! normalize error.
              lstres = zero
              do i = 1, n
                 lstres = max( lstres, abs( x( i, j ) ) )
              end do
              if( lstres/=zero )ferr( j ) = ferr( j ) / lstres
           end do loop_100
           return
     end subroutine stdlib_zptrfs




     pure module subroutine stdlib_slaqsp( uplo, n, ap, s, scond, amax, equed )
     !! SLAQSP equilibrates a symmetric matrix A using the scaling factors
     !! in the vector S.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(out) :: equed
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: n
           real(sp), intent(in) :: amax, scond
           ! Array Arguments 
           real(sp), intent(inout) :: ap(*)
           real(sp), intent(in) :: s(*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: thresh = 0.1e+0_sp
           
           ! Local Scalars 
           integer(ilp) :: i, j, jc
           real(sp) :: cj, large, small
           ! Executable Statements 
           ! quick return if possible
           if( n<=0_ilp ) then
              equed = 'N'
              return
           end if
           ! initialize large and small.
           small = stdlib_slamch( 'SAFE MINIMUM' ) / stdlib_slamch( 'PRECISION' )
           large = one / small
           if( scond>=thresh .and. amax>=small .and. amax<=large ) then
              ! no equilibration
              equed = 'N'
           else
              ! replace a by diag(s) * a * diag(s).
              if( stdlib_lsame( uplo, 'U' ) ) then
                 ! upper triangle of a is stored.
                 jc = 1_ilp
                 do j = 1, n
                    cj = s( j )
                    do i = 1, j
                       ap( jc+i-1 ) = cj*s( i )*ap( jc+i-1 )
                    end do
                    jc = jc + j
                 end do
              else
                 ! lower triangle of a is stored.
                 jc = 1_ilp
                 do j = 1, n
                    cj = s( j )
                    do i = j, n
                       ap( jc+i-j ) = cj*s( i )*ap( jc+i-j )
                    end do
                    jc = jc + n - j + 1_ilp
                 end do
              end if
              equed = 'Y'
           end if
           return
     end subroutine stdlib_slaqsp

     pure module subroutine stdlib_dlaqsp( uplo, n, ap, s, scond, amax, equed )
     !! DLAQSP equilibrates a symmetric matrix A using the scaling factors
     !! in the vector S.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(out) :: equed
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: n
           real(dp), intent(in) :: amax, scond
           ! Array Arguments 
           real(dp), intent(inout) :: ap(*)
           real(dp), intent(in) :: s(*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: thresh = 0.1e+0_dp
           
           ! Local Scalars 
           integer(ilp) :: i, j, jc
           real(dp) :: cj, large, small
           ! Executable Statements 
           ! quick return if possible
           if( n<=0_ilp ) then
              equed = 'N'
              return
           end if
           ! initialize large and small.
           small = stdlib_dlamch( 'SAFE MINIMUM' ) / stdlib_dlamch( 'PRECISION' )
           large = one / small
           if( scond>=thresh .and. amax>=small .and. amax<=large ) then
              ! no equilibration
              equed = 'N'
           else
              ! replace a by diag(s) * a * diag(s).
              if( stdlib_lsame( uplo, 'U' ) ) then
                 ! upper triangle of a is stored.
                 jc = 1_ilp
                 do j = 1, n
                    cj = s( j )
                    do i = 1, j
                       ap( jc+i-1 ) = cj*s( i )*ap( jc+i-1 )
                    end do
                    jc = jc + j
                 end do
              else
                 ! lower triangle of a is stored.
                 jc = 1_ilp
                 do j = 1, n
                    cj = s( j )
                    do i = j, n
                       ap( jc+i-j ) = cj*s( i )*ap( jc+i-j )
                    end do
                    jc = jc + n - j + 1_ilp
                 end do
              end if
              equed = 'Y'
           end if
           return
     end subroutine stdlib_dlaqsp


     pure module subroutine stdlib_claqsp( uplo, n, ap, s, scond, amax, equed )
     !! CLAQSP equilibrates a symmetric matrix A using the scaling factors
     !! in the vector S.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(out) :: equed
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: n
           real(sp), intent(in) :: amax, scond
           ! Array Arguments 
           real(sp), intent(in) :: s(*)
           complex(sp), intent(inout) :: ap(*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: thresh = 0.1e+0_sp
           
           ! Local Scalars 
           integer(ilp) :: i, j, jc
           real(sp) :: cj, large, small
           ! Executable Statements 
           ! quick return if possible
           if( n<=0_ilp ) then
              equed = 'N'
              return
           end if
           ! initialize large and small.
           small = stdlib_slamch( 'SAFE MINIMUM' ) / stdlib_slamch( 'PRECISION' )
           large = one / small
           if( scond>=thresh .and. amax>=small .and. amax<=large ) then
              ! no equilibration
              equed = 'N'
           else
              ! replace a by diag(s) * a * diag(s).
              if( stdlib_lsame( uplo, 'U' ) ) then
                 ! upper triangle of a is stored.
                 jc = 1_ilp
                 do j = 1, n
                    cj = s( j )
                    do i = 1, j
                       ap( jc+i-1 ) = cj*s( i )*ap( jc+i-1 )
                    end do
                    jc = jc + j
                 end do
              else
                 ! lower triangle of a is stored.
                 jc = 1_ilp
                 do j = 1, n
                    cj = s( j )
                    do i = j, n
                       ap( jc+i-j ) = cj*s( i )*ap( jc+i-j )
                    end do
                    jc = jc + n - j + 1_ilp
                 end do
              end if
              equed = 'Y'
           end if
           return
     end subroutine stdlib_claqsp

     pure module subroutine stdlib_zlaqsp( uplo, n, ap, s, scond, amax, equed )
     !! ZLAQSP equilibrates a symmetric matrix A using the scaling factors
     !! in the vector S.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(out) :: equed
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: n
           real(dp), intent(in) :: amax, scond
           ! Array Arguments 
           real(dp), intent(in) :: s(*)
           complex(dp), intent(inout) :: ap(*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: thresh = 0.1e+0_dp
           
           ! Local Scalars 
           integer(ilp) :: i, j, jc
           real(dp) :: cj, large, small
           ! Executable Statements 
           ! quick return if possible
           if( n<=0_ilp ) then
              equed = 'N'
              return
           end if
           ! initialize large and small.
           small = stdlib_dlamch( 'SAFE MINIMUM' ) / stdlib_dlamch( 'PRECISION' )
           large = one / small
           if( scond>=thresh .and. amax>=small .and. amax<=large ) then
              ! no equilibration
              equed = 'N'
           else
              ! replace a by diag(s) * a * diag(s).
              if( stdlib_lsame( uplo, 'U' ) ) then
                 ! upper triangle of a is stored.
                 jc = 1_ilp
                 do j = 1, n
                    cj = s( j )
                    do i = 1, j
                       ap( jc+i-1 ) = cj*s( i )*ap( jc+i-1 )
                    end do
                    jc = jc + j
                 end do
              else
                 ! lower triangle of a is stored.
                 jc = 1_ilp
                 do j = 1, n
                    cj = s( j )
                    do i = j, n
                       ap( jc+i-j ) = cj*s( i )*ap( jc+i-j )
                    end do
                    jc = jc + n - j + 1_ilp
                 end do
              end if
              equed = 'Y'
           end if
           return
     end subroutine stdlib_zlaqsp



end submodule stdlib_lapack_solve_chol_comp
