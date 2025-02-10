submodule(stdlib_lapack_solve) stdlib_lapack_solve_lu_comp
  implicit none


  contains

     pure module subroutine stdlib_sgecon( norm, n, a, lda, anorm, rcond, work, iwork,info )
     !! SGECON estimates the reciprocal of the condition number of a general
     !! real matrix A, in either the 1-norm or the infinity-norm, using
     !! the LU factorization computed by SGETRF.
     !! An estimate is obtained for norm(inv(A)), and the reciprocal of the
     !! condition number is computed as
     !! RCOND = 1 / ( norm(A) * norm(inv(A)) ).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm
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
           logical(lk) :: onenrm
           character :: normin
           integer(ilp) :: ix, kase, kase1
           real(sp) :: ainvnm, scale, sl, smlnum, su
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           onenrm = norm=='1' .or. stdlib_lsame( norm, 'O' )
           if( .not.onenrm .and. .not.stdlib_lsame( norm, 'I' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( anorm<zero ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGECON', -info )
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
           ! estimate the norm of inv(a).
           ainvnm = zero
           normin = 'N'
           if( onenrm ) then
              kase1 = 1_ilp
           else
              kase1 = 2_ilp
           end if
           kase = 0_ilp
           10 continue
           call stdlib_slacn2( n, work( n+1 ), work, iwork, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              if( kase==kase1 ) then
                 ! multiply by inv(l).
                 call stdlib_slatrs( 'LOWER', 'NO TRANSPOSE', 'UNIT', normin, n, a,lda, work, sl, &
                           work( 2_ilp*n+1 ), info )
                 ! multiply by inv(u).
                 call stdlib_slatrs( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', normin, n,a, lda, work, &
                           su, work( 3_ilp*n+1 ), info )
              else
                 ! multiply by inv(u**t).
                 call stdlib_slatrs( 'UPPER', 'TRANSPOSE', 'NON-UNIT', normin, n, a,lda, work, su,&
                            work( 3_ilp*n+1 ), info )
                 ! multiply by inv(l**t).
                 call stdlib_slatrs( 'LOWER', 'TRANSPOSE', 'UNIT', normin, n, a,lda, work, sl, &
                           work( 2_ilp*n+1 ), info )
              end if
              ! divide x by 1/(sl*su) if doing so will not cause overflow.
              scale = sl*su
              normin = 'Y'
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
     end subroutine stdlib_sgecon

     pure module subroutine stdlib_dgecon( norm, n, a, lda, anorm, rcond, work, iwork,info )
     !! DGECON estimates the reciprocal of the condition number of a general
     !! real matrix A, in either the 1-norm or the infinity-norm, using
     !! the LU factorization computed by DGETRF.
     !! An estimate is obtained for norm(inv(A)), and the reciprocal of the
     !! condition number is computed as
     !! RCOND = 1 / ( norm(A) * norm(inv(A)) ).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm
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
           logical(lk) :: onenrm
           character :: normin
           integer(ilp) :: ix, kase, kase1
           real(dp) :: ainvnm, scale, sl, smlnum, su
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           onenrm = norm=='1' .or. stdlib_lsame( norm, 'O' )
           if( .not.onenrm .and. .not.stdlib_lsame( norm, 'I' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( anorm<zero ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGECON', -info )
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
           ! estimate the norm of inv(a).
           ainvnm = zero
           normin = 'N'
           if( onenrm ) then
              kase1 = 1_ilp
           else
              kase1 = 2_ilp
           end if
           kase = 0_ilp
           10 continue
           call stdlib_dlacn2( n, work( n+1 ), work, iwork, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              if( kase==kase1 ) then
                 ! multiply by inv(l).
                 call stdlib_dlatrs( 'LOWER', 'NO TRANSPOSE', 'UNIT', normin, n, a,lda, work, sl, &
                           work( 2_ilp*n+1 ), info )
                 ! multiply by inv(u).
                 call stdlib_dlatrs( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', normin, n,a, lda, work, &
                           su, work( 3_ilp*n+1 ), info )
              else
                 ! multiply by inv(u**t).
                 call stdlib_dlatrs( 'UPPER', 'TRANSPOSE', 'NON-UNIT', normin, n, a,lda, work, su,&
                            work( 3_ilp*n+1 ), info )
                 ! multiply by inv(l**t).
                 call stdlib_dlatrs( 'LOWER', 'TRANSPOSE', 'UNIT', normin, n, a,lda, work, sl, &
                           work( 2_ilp*n+1 ), info )
              end if
              ! divide x by 1/(sl*su) if doing so will not cause overflow.
              scale = sl*su
              normin = 'Y'
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
     end subroutine stdlib_dgecon


     pure module subroutine stdlib_cgecon( norm, n, a, lda, anorm, rcond, work, rwork,info )
     !! CGECON estimates the reciprocal of the condition number of a general
     !! complex matrix A, in either the 1-norm or the infinity-norm, using
     !! the LU factorization computed by CGETRF.
     !! An estimate is obtained for norm(inv(A)), and the reciprocal of the
     !! condition number is computed as
     !! RCOND = 1 / ( norm(A) * norm(inv(A)) ).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm
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
           logical(lk) :: onenrm
           character :: normin
           integer(ilp) :: ix, kase, kase1
           real(sp) :: ainvnm, scale, sl, smlnum, su
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
           onenrm = norm=='1' .or. stdlib_lsame( norm, 'O' )
           if( .not.onenrm .and. .not.stdlib_lsame( norm, 'I' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( anorm<zero ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGECON', -info )
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
           ! estimate the norm of inv(a).
           ainvnm = zero
           normin = 'N'
           if( onenrm ) then
              kase1 = 1_ilp
           else
              kase1 = 2_ilp
           end if
           kase = 0_ilp
           10 continue
           call stdlib_clacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              if( kase==kase1 ) then
                 ! multiply by inv(l).
                 call stdlib_clatrs( 'LOWER', 'NO TRANSPOSE', 'UNIT', normin, n, a,lda, work, sl, &
                           rwork, info )
                 ! multiply by inv(u).
                 call stdlib_clatrs( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', normin, n,a, lda, work, &
                           su, rwork( n+1 ), info )
              else
                 ! multiply by inv(u**h).
                 call stdlib_clatrs( 'UPPER', 'CONJUGATE TRANSPOSE', 'NON-UNIT',normin, n, a, lda,&
                            work, su, rwork( n+1 ),info )
                 ! multiply by inv(l**h).
                 call stdlib_clatrs( 'LOWER', 'CONJUGATE TRANSPOSE', 'UNIT', normin,n, a, lda, &
                           work, sl, rwork, info )
              end if
              ! divide x by 1/(sl*su) if doing so will not cause overflow.
              scale = sl*su
              normin = 'Y'
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
     end subroutine stdlib_cgecon

     pure module subroutine stdlib_zgecon( norm, n, a, lda, anorm, rcond, work, rwork,info )
     !! ZGECON estimates the reciprocal of the condition number of a general
     !! complex matrix A, in either the 1-norm or the infinity-norm, using
     !! the LU factorization computed by ZGETRF.
     !! An estimate is obtained for norm(inv(A)), and the reciprocal of the
     !! condition number is computed as
     !! RCOND = 1 / ( norm(A) * norm(inv(A)) ).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm
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
           logical(lk) :: onenrm
           character :: normin
           integer(ilp) :: ix, kase, kase1
           real(dp) :: ainvnm, scale, sl, smlnum, su
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
           onenrm = norm=='1' .or. stdlib_lsame( norm, 'O' )
           if( .not.onenrm .and. .not.stdlib_lsame( norm, 'I' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( anorm<zero ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGECON', -info )
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
           ! estimate the norm of inv(a).
           ainvnm = zero
           normin = 'N'
           if( onenrm ) then
              kase1 = 1_ilp
           else
              kase1 = 2_ilp
           end if
           kase = 0_ilp
           10 continue
           call stdlib_zlacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              if( kase==kase1 ) then
                 ! multiply by inv(l).
                 call stdlib_zlatrs( 'LOWER', 'NO TRANSPOSE', 'UNIT', normin, n, a,lda, work, sl, &
                           rwork, info )
                 ! multiply by inv(u).
                 call stdlib_zlatrs( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', normin, n,a, lda, work, &
                           su, rwork( n+1 ), info )
              else
                 ! multiply by inv(u**h).
                 call stdlib_zlatrs( 'UPPER', 'CONJUGATE TRANSPOSE', 'NON-UNIT',normin, n, a, lda,&
                            work, su, rwork( n+1 ),info )
                 ! multiply by inv(l**h).
                 call stdlib_zlatrs( 'LOWER', 'CONJUGATE TRANSPOSE', 'UNIT', normin,n, a, lda, &
                           work, sl, rwork, info )
              end if
              ! divide x by 1/(sl*su) if doing so will not cause overflow.
              scale = sl*su
              normin = 'Y'
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
     end subroutine stdlib_zgecon




     pure module subroutine stdlib_sgetrf( m, n, a, lda, ipiv, info )
     !! SGETRF computes an LU factorization of a general M-by-N matrix A
     !! using partial pivoting with row interchanges.
     !! The factorization has the form
     !! A = P * L * U
     !! where P is a permutation matrix, L is lower triangular with unit
     !! diagonal elements (lower trapezoidal if m > n), and U is upper
     !! triangular (upper trapezoidal if m < n).
     !! This is the right-looking Level 3 BLAS version of the algorithm.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(sp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, iinfo, j, jb, nb
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGETRF', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 )return
           ! determine the block size for this environment.
           nb = stdlib_ilaenv( 1_ilp, 'SGETRF', ' ', m, n, -1_ilp, -1_ilp )
           if( nb<=1_ilp .or. nb>=min( m, n ) ) then
              ! use unblocked code.
              call stdlib_sgetrf2( m, n, a, lda, ipiv, info )
           else
              ! use blocked code.
              do j = 1, min( m, n ), nb
                 jb = min( min( m, n )-j+1, nb )
                 ! factor diagonal and subdiagonal blocks and test for exact
                 ! singularity.
                 call stdlib_sgetrf2( m-j+1, jb, a( j, j ), lda, ipiv( j ), iinfo )
                 ! adjust info and the pivot indices.
                 if( info==0_ilp .and. iinfo>0_ilp )info = iinfo + j - 1_ilp
                 do i = j, min( m, j+jb-1 )
                    ipiv( i ) = j - 1_ilp + ipiv( i )
                 end do
                 ! apply interchanges to columns 1:j-1.
                 call stdlib_slaswp( j-1, a, lda, j, j+jb-1, ipiv, 1_ilp )
                 if( j+jb<=n ) then
                    ! apply interchanges to columns j+jb:n.
                    call stdlib_slaswp( n-j-jb+1, a( 1_ilp, j+jb ), lda, j, j+jb-1,ipiv, 1_ilp )
                    ! compute block row of u.
                    call stdlib_strsm( 'LEFT', 'LOWER', 'NO TRANSPOSE', 'UNIT', jb,n-j-jb+1, one, &
                              a( j, j ), lda, a( j, j+jb ),lda )
                    if( j+jb<=m ) then
                       ! update trailing submatrix.
                       call stdlib_sgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m-j-jb+1,n-j-jb+1, jb, -&
                       one, a( j+jb, j ), lda,a( j, j+jb ), lda, one, a( j+jb, j+jb ),lda )
                                 
                    end if
                 end if
              end do
           end if
           return
     end subroutine stdlib_sgetrf

     pure module subroutine stdlib_dgetrf( m, n, a, lda, ipiv, info )
     !! DGETRF computes an LU factorization of a general M-by-N matrix A
     !! using partial pivoting with row interchanges.
     !! The factorization has the form
     !! A = P * L * U
     !! where P is a permutation matrix, L is lower triangular with unit
     !! diagonal elements (lower trapezoidal if m > n), and U is upper
     !! triangular (upper trapezoidal if m < n).
     !! This is the right-looking Level 3 BLAS version of the algorithm.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(dp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, iinfo, j, jb, nb
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGETRF', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 )return
           ! determine the block size for this environment.
           nb = stdlib_ilaenv( 1_ilp, 'DGETRF', ' ', m, n, -1_ilp, -1_ilp )
           if( nb<=1_ilp .or. nb>=min( m, n ) ) then
              ! use unblocked code.
              call stdlib_dgetrf2( m, n, a, lda, ipiv, info )
           else
              ! use blocked code.
              do j = 1, min( m, n ), nb
                 jb = min( min( m, n )-j+1, nb )
                 ! factor diagonal and subdiagonal blocks and test for exact
                 ! singularity.
                 call stdlib_dgetrf2( m-j+1, jb, a( j, j ), lda, ipiv( j ), iinfo )
                 ! adjust info and the pivot indices.
                 if( info==0_ilp .and. iinfo>0_ilp )info = iinfo + j - 1_ilp
                 do i = j, min( m, j+jb-1 )
                    ipiv( i ) = j - 1_ilp + ipiv( i )
                 end do
                 ! apply interchanges to columns 1:j-1.
                 call stdlib_dlaswp( j-1, a, lda, j, j+jb-1, ipiv, 1_ilp )
                 if( j+jb<=n ) then
                    ! apply interchanges to columns j+jb:n.
                    call stdlib_dlaswp( n-j-jb+1, a( 1_ilp, j+jb ), lda, j, j+jb-1,ipiv, 1_ilp )
                    ! compute block row of u.
                    call stdlib_dtrsm( 'LEFT', 'LOWER', 'NO TRANSPOSE', 'UNIT', jb,n-j-jb+1, one, &
                              a( j, j ), lda, a( j, j+jb ),lda )
                    if( j+jb<=m ) then
                       ! update trailing submatrix.
                       call stdlib_dgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m-j-jb+1,n-j-jb+1, jb, -&
                       one, a( j+jb, j ), lda,a( j, j+jb ), lda, one, a( j+jb, j+jb ),lda )
                                 
                    end if
                 end if
              end do
           end if
           return
     end subroutine stdlib_dgetrf


     pure module subroutine stdlib_cgetrf( m, n, a, lda, ipiv, info )
     !! CGETRF computes an LU factorization of a general M-by-N matrix A
     !! using partial pivoting with row interchanges.
     !! The factorization has the form
     !! A = P * L * U
     !! where P is a permutation matrix, L is lower triangular with unit
     !! diagonal elements (lower trapezoidal if m > n), and U is upper
     !! triangular (upper trapezoidal if m < n).
     !! This is the right-looking Level 3 BLAS version of the algorithm.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(sp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, iinfo, j, jb, nb
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGETRF', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 )return
           ! determine the block size for this environment.
           nb = stdlib_ilaenv( 1_ilp, 'CGETRF', ' ', m, n, -1_ilp, -1_ilp )
           if( nb<=1_ilp .or. nb>=min( m, n ) ) then
              ! use unblocked code.
              call stdlib_cgetrf2( m, n, a, lda, ipiv, info )
           else
              ! use blocked code.
              do j = 1, min( m, n ), nb
                 jb = min( min( m, n )-j+1, nb )
                 ! factor diagonal and subdiagonal blocks and test for exact
                 ! singularity.
                 call stdlib_cgetrf2( m-j+1, jb, a( j, j ), lda, ipiv( j ), iinfo )
                 ! adjust info and the pivot indices.
                 if( info==0_ilp .and. iinfo>0_ilp )info = iinfo + j - 1_ilp
                 do i = j, min( m, j+jb-1 )
                    ipiv( i ) = j - 1_ilp + ipiv( i )
                 end do
                 ! apply interchanges to columns 1:j-1.
                 call stdlib_claswp( j-1, a, lda, j, j+jb-1, ipiv, 1_ilp )
                 if( j+jb<=n ) then
                    ! apply interchanges to columns j+jb:n.
                    call stdlib_claswp( n-j-jb+1, a( 1_ilp, j+jb ), lda, j, j+jb-1,ipiv, 1_ilp )
                    ! compute block row of u.
                    call stdlib_ctrsm( 'LEFT', 'LOWER', 'NO TRANSPOSE', 'UNIT', jb,n-j-jb+1, cone,&
                               a( j, j ), lda, a( j, j+jb ),lda )
                    if( j+jb<=m ) then
                       ! update trailing submatrix.
                       call stdlib_cgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m-j-jb+1,n-j-jb+1, jb, -&
                       cone, a( j+jb, j ), lda,a( j, j+jb ), lda, cone, a( j+jb, j+jb ),lda )
                                 
                    end if
                 end if
              end do
           end if
           return
     end subroutine stdlib_cgetrf

     pure module subroutine stdlib_zgetrf( m, n, a, lda, ipiv, info )
     !! ZGETRF computes an LU factorization of a general M-by-N matrix A
     !! using partial pivoting with row interchanges.
     !! The factorization has the form
     !! A = P * L * U
     !! where P is a permutation matrix, L is lower triangular with unit
     !! diagonal elements (lower trapezoidal if m > n), and U is upper
     !! triangular (upper trapezoidal if m < n).
     !! This is the right-looking Level 3 BLAS version of the algorithm.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(dp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, iinfo, j, jb, nb
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGETRF', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 )return
           ! determine the block size for this environment.
           nb = stdlib_ilaenv( 1_ilp, 'ZGETRF', ' ', m, n, -1_ilp, -1_ilp )
           if( nb<=1_ilp .or. nb>=min( m, n ) ) then
              ! use unblocked code.
              call stdlib_zgetrf2( m, n, a, lda, ipiv, info )
           else
              ! use blocked code.
              do j = 1, min( m, n ), nb
                 jb = min( min( m, n )-j+1, nb )
                 ! factor diagonal and subdiagonal blocks and test for exact
                 ! singularity.
                 call stdlib_zgetrf2( m-j+1, jb, a( j, j ), lda, ipiv( j ), iinfo )
                 ! adjust info and the pivot indices.
                 if( info==0_ilp .and. iinfo>0_ilp )info = iinfo + j - 1_ilp
                 do i = j, min( m, j+jb-1 )
                    ipiv( i ) = j - 1_ilp + ipiv( i )
                 end do
                 ! apply interchanges to columns 1:j-1.
                 call stdlib_zlaswp( j-1, a, lda, j, j+jb-1, ipiv, 1_ilp )
                 if( j+jb<=n ) then
                    ! apply interchanges to columns j+jb:n.
                    call stdlib_zlaswp( n-j-jb+1, a( 1_ilp, j+jb ), lda, j, j+jb-1,ipiv, 1_ilp )
                    ! compute block row of u.
                    call stdlib_ztrsm( 'LEFT', 'LOWER', 'NO TRANSPOSE', 'UNIT', jb,n-j-jb+1, cone,&
                               a( j, j ), lda, a( j, j+jb ),lda )
                    if( j+jb<=m ) then
                       ! update trailing submatrix.
                       call stdlib_zgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m-j-jb+1,n-j-jb+1, jb, -&
                       cone, a( j+jb, j ), lda,a( j, j+jb ), lda, cone, a( j+jb, j+jb ),lda )
                                 
                    end if
                 end if
              end do
           end if
           return
     end subroutine stdlib_zgetrf




     pure recursive module subroutine stdlib_sgetrf2( m, n, a, lda, ipiv, info )
     !! SGETRF2 computes an LU factorization of a general M-by-N matrix A
     !! using partial pivoting with row interchanges.
     !! The factorization has the form
     !! A = P * L * U
     !! where P is a permutation matrix, L is lower triangular with unit
     !! diagonal elements (lower trapezoidal if m > n), and U is upper
     !! triangular (upper trapezoidal if m < n).
     !! This is the recursive version of the algorithm. It divides
     !! the matrix into four submatrices:
     !! [  A11 | A12  ]  where A11 is n1 by n1 and A22 is n2 by n2
     !! A = [ -----|----- ]  with n1 = min(m,n)/2
     !! [  A21 | A22  ]       n2 = n-n1
     !! [ A11 ]
     !! The subroutine calls itself to factor [ --- ],
     !! [ A12 ]
     !! [ A12 ]
     !! do the swaps on [ --- ], solve A12, update A22,
     !! [ A22 ]
     !! then calls itself to factor A22 and do the swaps on A21.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(sp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: sfmin, temp
           integer(ilp) :: i, iinfo, n1, n2
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGETRF2', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 )return
           if ( m==1_ilp ) then
              ! use unblocked code for one row case
              ! just need to handle ipiv and info
              ipiv( 1_ilp ) = 1_ilp
              if ( a(1_ilp,1_ilp)==zero )info = 1_ilp
           else if( n==1_ilp ) then
              ! use unblocked code for one column case
              ! compute machine safe minimum
              sfmin = stdlib_slamch('S')
              ! find pivot and test for singularity
              i = stdlib_isamax( m, a( 1_ilp, 1_ilp ), 1_ilp )
              ipiv( 1_ilp ) = i
              if( a( i, 1_ilp )/=zero ) then
                 ! apply the interchange
                 if( i/=1_ilp ) then
                    temp = a( 1_ilp, 1_ilp )
                    a( 1_ilp, 1_ilp ) = a( i, 1_ilp )
                    a( i, 1_ilp ) = temp
                 end if
                 ! compute elements 2:m of the column
                 if( abs(a( 1_ilp, 1_ilp )) >= sfmin ) then
                    call stdlib_sscal( m-1, one / a( 1_ilp, 1_ilp ), a( 2_ilp, 1_ilp ), 1_ilp )
                 else
                    do i = 1, m-1
                       a( 1_ilp+i, 1_ilp ) = a( 1_ilp+i, 1_ilp ) / a( 1_ilp, 1_ilp )
                    end do
                 end if
              else
                 info = 1_ilp
              end if
           else
              ! use recursive code
              n1 = min( m, n ) / 2_ilp
              n2 = n-n1
                     ! [ a11 ]
              ! factor [ --- ]
                     ! [ a21 ]
              call stdlib_sgetrf2( m, n1, a, lda, ipiv, iinfo )
              if ( info==0_ilp .and. iinfo>0_ilp )info = iinfo
                                    ! [ a12 ]
              ! apply interchanges to [ --- ]
                                    ! [ a22 ]
              call stdlib_slaswp( n2, a( 1_ilp, n1+1 ), lda, 1_ilp, n1, ipiv, 1_ilp )
              ! solve a12
              call stdlib_strsm( 'L', 'L', 'N', 'U', n1, n2, one, a, lda,a( 1_ilp, n1+1 ), lda )
                        
              ! update a22
              call stdlib_sgemm( 'N', 'N', m-n1, n2, n1, -one, a( n1+1, 1_ilp ), lda,a( 1_ilp, n1+1 ), &
                        lda, one, a( n1+1, n1+1 ), lda )
              ! factor a22
              call stdlib_sgetrf2( m-n1, n2, a( n1+1, n1+1 ), lda, ipiv( n1+1 ),iinfo )
              ! adjust info and the pivot indices
              if ( info==0_ilp .and. iinfo>0_ilp )info = iinfo + n1
              do i = n1+1, min( m, n )
                 ipiv( i ) = ipiv( i ) + n1
              end do
              ! apply interchanges to a21
              call stdlib_slaswp( n1, a( 1_ilp, 1_ilp ), lda, n1+1, min( m, n), ipiv, 1_ilp )
           end if
           return
     end subroutine stdlib_sgetrf2

     pure recursive module subroutine stdlib_dgetrf2( m, n, a, lda, ipiv, info )
     !! DGETRF2 computes an LU factorization of a general M-by-N matrix A
     !! using partial pivoting with row interchanges.
     !! The factorization has the form
     !! A = P * L * U
     !! where P is a permutation matrix, L is lower triangular with unit
     !! diagonal elements (lower trapezoidal if m > n), and U is upper
     !! triangular (upper trapezoidal if m < n).
     !! This is the recursive version of the algorithm. It divides
     !! the matrix into four submatrices:
     !! [  A11 | A12  ]  where A11 is n1 by n1 and A22 is n2 by n2
     !! A = [ -----|----- ]  with n1 = min(m,n)/2
     !! [  A21 | A22  ]       n2 = n-n1
     !! [ A11 ]
     !! The subroutine calls itself to factor [ --- ],
     !! [ A12 ]
     !! [ A12 ]
     !! do the swaps on [ --- ], solve A12, update A22,
     !! [ A22 ]
     !! then calls itself to factor A22 and do the swaps on A21.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(dp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: sfmin, temp
           integer(ilp) :: i, iinfo, n1, n2
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGETRF2', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 )return
           if ( m==1_ilp ) then
              ! use unblocked code for one row case
              ! just need to handle ipiv and info
              ipiv( 1_ilp ) = 1_ilp
              if ( a(1_ilp,1_ilp)==zero )info = 1_ilp
           else if( n==1_ilp ) then
              ! use unblocked code for one column case
              ! compute machine safe minimum
              sfmin = stdlib_dlamch('S')
              ! find pivot and test for singularity
              i = stdlib_idamax( m, a( 1_ilp, 1_ilp ), 1_ilp )
              ipiv( 1_ilp ) = i
              if( a( i, 1_ilp )/=zero ) then
                 ! apply the interchange
                 if( i/=1_ilp ) then
                    temp = a( 1_ilp, 1_ilp )
                    a( 1_ilp, 1_ilp ) = a( i, 1_ilp )
                    a( i, 1_ilp ) = temp
                 end if
                 ! compute elements 2:m of the column
                 if( abs(a( 1_ilp, 1_ilp )) >= sfmin ) then
                    call stdlib_dscal( m-1, one / a( 1_ilp, 1_ilp ), a( 2_ilp, 1_ilp ), 1_ilp )
                 else
                    do i = 1, m-1
                       a( 1_ilp+i, 1_ilp ) = a( 1_ilp+i, 1_ilp ) / a( 1_ilp, 1_ilp )
                    end do
                 end if
              else
                 info = 1_ilp
              end if
           else
              ! use recursive code
              n1 = min( m, n ) / 2_ilp
              n2 = n-n1
                     ! [ a11 ]
              ! factor [ --- ]
                     ! [ a21 ]
              call stdlib_dgetrf2( m, n1, a, lda, ipiv, iinfo )
              if ( info==0_ilp .and. iinfo>0_ilp )info = iinfo
                                    ! [ a12 ]
              ! apply interchanges to [ --- ]
                                    ! [ a22 ]
              call stdlib_dlaswp( n2, a( 1_ilp, n1+1 ), lda, 1_ilp, n1, ipiv, 1_ilp )
              ! solve a12
              call stdlib_dtrsm( 'L', 'L', 'N', 'U', n1, n2, one, a, lda,a( 1_ilp, n1+1 ), lda )
                        
              ! update a22
              call stdlib_dgemm( 'N', 'N', m-n1, n2, n1, -one, a( n1+1, 1_ilp ), lda,a( 1_ilp, n1+1 ), &
                        lda, one, a( n1+1, n1+1 ), lda )
              ! factor a22
              call stdlib_dgetrf2( m-n1, n2, a( n1+1, n1+1 ), lda, ipiv( n1+1 ),iinfo )
              ! adjust info and the pivot indices
              if ( info==0_ilp .and. iinfo>0_ilp )info = iinfo + n1
              do i = n1+1, min( m, n )
                 ipiv( i ) = ipiv( i ) + n1
              end do
              ! apply interchanges to a21
              call stdlib_dlaswp( n1, a( 1_ilp, 1_ilp ), lda, n1+1, min( m, n), ipiv, 1_ilp )
           end if
           return
     end subroutine stdlib_dgetrf2


     pure recursive module subroutine stdlib_cgetrf2( m, n, a, lda, ipiv, info )
     !! CGETRF2 computes an LU factorization of a general M-by-N matrix A
     !! using partial pivoting with row interchanges.
     !! The factorization has the form
     !! A = P * L * U
     !! where P is a permutation matrix, L is lower triangular with unit
     !! diagonal elements (lower trapezoidal if m > n), and U is upper
     !! triangular (upper trapezoidal if m < n).
     !! This is the recursive version of the algorithm. It divides
     !! the matrix into four submatrices:
     !! [  A11 | A12  ]  where A11 is n1 by n1 and A22 is n2 by n2
     !! A = [ -----|----- ]  with n1 = min(m,n)/2
     !! [  A21 | A22  ]       n2 = n-n1
     !! [ A11 ]
     !! The subroutine calls itself to factor [ --- ],
     !! [ A12 ]
     !! [ A12 ]
     !! do the swaps on [ --- ], solve A12, update A22,
     !! [ A22 ]
     !! then calls itself to factor A22 and do the swaps on A21.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(sp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: sfmin
           complex(sp) :: temp
           integer(ilp) :: i, iinfo, n1, n2
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGETRF2', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 )return
           if ( m==1_ilp ) then
              ! use unblocked code for cone row case
              ! just need to handle ipiv and info
              ipiv( 1_ilp ) = 1_ilp
              if ( a(1_ilp,1_ilp)==czero )info = 1_ilp
           else if( n==1_ilp ) then
              ! use unblocked code for cone column case
              ! compute machine safe minimum
              sfmin = stdlib_slamch('S')
              ! find pivot and test for singularity
              i = stdlib_icamax( m, a( 1_ilp, 1_ilp ), 1_ilp )
              ipiv( 1_ilp ) = i
              if( a( i, 1_ilp )/=czero ) then
                 ! apply the interchange
                 if( i/=1_ilp ) then
                    temp = a( 1_ilp, 1_ilp )
                    a( 1_ilp, 1_ilp ) = a( i, 1_ilp )
                    a( i, 1_ilp ) = temp
                 end if
                 ! compute elements 2:m of the column
                 if( abs(a( 1_ilp, 1_ilp )) >= sfmin ) then
                    call stdlib_cscal( m-1, cone / a( 1_ilp, 1_ilp ), a( 2_ilp, 1_ilp ), 1_ilp )
                 else
                    do i = 1, m-1
                       a( 1_ilp+i, 1_ilp ) = a( 1_ilp+i, 1_ilp ) / a( 1_ilp, 1_ilp )
                    end do
                 end if
              else
                 info = 1_ilp
              end if
           else
              ! use recursive code
              n1 = min( m, n ) / 2_ilp
              n2 = n-n1
                     ! [ a11 ]
              ! factor [ --- ]
                     ! [ a21 ]
              call stdlib_cgetrf2( m, n1, a, lda, ipiv, iinfo )
              if ( info==0_ilp .and. iinfo>0_ilp )info = iinfo
                                    ! [ a12 ]
              ! apply interchanges to [ --- ]
                                    ! [ a22 ]
              call stdlib_claswp( n2, a( 1_ilp, n1+1 ), lda, 1_ilp, n1, ipiv, 1_ilp )
              ! solve a12
              call stdlib_ctrsm( 'L', 'L', 'N', 'U', n1, n2, cone, a, lda,a( 1_ilp, n1+1 ), lda )
                        
              ! update a22
              call stdlib_cgemm( 'N', 'N', m-n1, n2, n1, -cone, a( n1+1, 1_ilp ), lda,a( 1_ilp, n1+1 ), &
                        lda, cone, a( n1+1, n1+1 ), lda )
              ! factor a22
              call stdlib_cgetrf2( m-n1, n2, a( n1+1, n1+1 ), lda, ipiv( n1+1 ),iinfo )
              ! adjust info and the pivot indices
              if ( info==0_ilp .and. iinfo>0_ilp )info = iinfo + n1
              do i = n1+1, min( m, n )
                 ipiv( i ) = ipiv( i ) + n1
              end do
              ! apply interchanges to a21
              call stdlib_claswp( n1, a( 1_ilp, 1_ilp ), lda, n1+1, min( m, n), ipiv, 1_ilp )
           end if
           return
     end subroutine stdlib_cgetrf2

     pure recursive module subroutine stdlib_zgetrf2( m, n, a, lda, ipiv, info )
     !! ZGETRF2 computes an LU factorization of a general M-by-N matrix A
     !! using partial pivoting with row interchanges.
     !! The factorization has the form
     !! A = P * L * U
     !! where P is a permutation matrix, L is lower triangular with unit
     !! diagonal elements (lower trapezoidal if m > n), and U is upper
     !! triangular (upper trapezoidal if m < n).
     !! This is the recursive version of the algorithm. It divides
     !! the matrix into four submatrices:
     !! [  A11 | A12  ]  where A11 is n1 by n1 and A22 is n2 by n2
     !! A = [ -----|----- ]  with n1 = min(m,n)/2
     !! [  A21 | A22  ]       n2 = n-n1
     !! [ A11 ]
     !! The subroutine calls itself to factor [ --- ],
     !! [ A12 ]
     !! [ A12 ]
     !! do the swaps on [ --- ], solve A12, update A22,
     !! [ A22 ]
     !! then calls itself to factor A22 and do the swaps on A21.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(dp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: sfmin
           complex(dp) :: temp
           integer(ilp) :: i, iinfo, n1, n2
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGETRF2', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 )return
           if ( m==1_ilp ) then
              ! use unblocked code for cone row case
              ! just need to handle ipiv and info
              ipiv( 1_ilp ) = 1_ilp
              if ( a(1_ilp,1_ilp)==czero )info = 1_ilp
           else if( n==1_ilp ) then
              ! use unblocked code for cone column case
              ! compute machine safe minimum
              sfmin = stdlib_dlamch('S')
              ! find pivot and test for singularity
              i = stdlib_izamax( m, a( 1_ilp, 1_ilp ), 1_ilp )
              ipiv( 1_ilp ) = i
              if( a( i, 1_ilp )/=czero ) then
                 ! apply the interchange
                 if( i/=1_ilp ) then
                    temp = a( 1_ilp, 1_ilp )
                    a( 1_ilp, 1_ilp ) = a( i, 1_ilp )
                    a( i, 1_ilp ) = temp
                 end if
                 ! compute elements 2:m of the column
                 if( abs(a( 1_ilp, 1_ilp )) >= sfmin ) then
                    call stdlib_zscal( m-1, cone / a( 1_ilp, 1_ilp ), a( 2_ilp, 1_ilp ), 1_ilp )
                 else
                    do i = 1, m-1
                       a( 1_ilp+i, 1_ilp ) = a( 1_ilp+i, 1_ilp ) / a( 1_ilp, 1_ilp )
                    end do
                 end if
              else
                 info = 1_ilp
              end if
           else
              ! use recursive code
              n1 = min( m, n ) / 2_ilp
              n2 = n-n1
                     ! [ a11 ]
              ! factor [ --- ]
                     ! [ a21 ]
              call stdlib_zgetrf2( m, n1, a, lda, ipiv, iinfo )
              if ( info==0_ilp .and. iinfo>0_ilp )info = iinfo
                                    ! [ a12 ]
              ! apply interchanges to [ --- ]
                                    ! [ a22 ]
              call stdlib_zlaswp( n2, a( 1_ilp, n1+1 ), lda, 1_ilp, n1, ipiv, 1_ilp )
              ! solve a12
              call stdlib_ztrsm( 'L', 'L', 'N', 'U', n1, n2, cone, a, lda,a( 1_ilp, n1+1 ), lda )
                        
              ! update a22
              call stdlib_zgemm( 'N', 'N', m-n1, n2, n1, -cone, a( n1+1, 1_ilp ), lda,a( 1_ilp, n1+1 ), &
                        lda, cone, a( n1+1, n1+1 ), lda )
              ! factor a22
              call stdlib_zgetrf2( m-n1, n2, a( n1+1, n1+1 ), lda, ipiv( n1+1 ),iinfo )
              ! adjust info and the pivot indices
              if ( info==0_ilp .and. iinfo>0_ilp )info = iinfo + n1
              do i = n1+1, min( m, n )
                 ipiv( i ) = ipiv( i ) + n1
              end do
              ! apply interchanges to a21
              call stdlib_zlaswp( n1, a( 1_ilp, 1_ilp ), lda, n1+1, min( m, n), ipiv, 1_ilp )
           end if
           return
     end subroutine stdlib_zgetrf2




     pure module subroutine stdlib_sgetf2( m, n, a, lda, ipiv, info )
     !! SGETF2 computes an LU factorization of a general m-by-n matrix A
     !! using partial pivoting with row interchanges.
     !! The factorization has the form
     !! A = P * L * U
     !! where P is a permutation matrix, L is lower triangular with unit
     !! diagonal elements (lower trapezoidal if m > n), and U is upper
     !! triangular (upper trapezoidal if m < n).
     !! This is the right-looking Level 2 BLAS version of the algorithm.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(sp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: sfmin
           integer(ilp) :: i, j, jp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGETF2', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 )return
           ! compute machine safe minimum
           sfmin = stdlib_slamch('S')
           do j = 1, min( m, n )
              ! find pivot and test for singularity.
              jp = j - 1_ilp + stdlib_isamax( m-j+1, a( j, j ), 1_ilp )
              ipiv( j ) = jp
              if( a( jp, j )/=zero ) then
                 ! apply the interchange to columns 1:n.
                 if( jp/=j )call stdlib_sswap( n, a( j, 1_ilp ), lda, a( jp, 1_ilp ), lda )
                 ! compute elements j+1:m of j-th column.
                 if( j<m ) then
                    if( abs(a( j, j )) >= sfmin ) then
                       call stdlib_sscal( m-j, one / a( j, j ), a( j+1, j ), 1_ilp )
                    else
                      do i = 1, m-j
                         a( j+i, j ) = a( j+i, j ) / a( j, j )
                      end do
                    end if
                 end if
              else if( info==0_ilp ) then
                 info = j
              end if
              if( j<min( m, n ) ) then
                 ! update trailing submatrix.
                 call stdlib_sger( m-j, n-j, -one, a( j+1, j ), 1_ilp, a( j, j+1 ), lda,a( j+1, j+1 ),&
                            lda )
              end if
           end do
           return
     end subroutine stdlib_sgetf2

     pure module subroutine stdlib_dgetf2( m, n, a, lda, ipiv, info )
     !! DGETF2 computes an LU factorization of a general m-by-n matrix A
     !! using partial pivoting with row interchanges.
     !! The factorization has the form
     !! A = P * L * U
     !! where P is a permutation matrix, L is lower triangular with unit
     !! diagonal elements (lower trapezoidal if m > n), and U is upper
     !! triangular (upper trapezoidal if m < n).
     !! This is the right-looking Level 2 BLAS version of the algorithm.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(dp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: sfmin
           integer(ilp) :: i, j, jp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGETF2', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 )return
           ! compute machine safe minimum
           sfmin = stdlib_dlamch('S')
           do j = 1, min( m, n )
              ! find pivot and test for singularity.
              jp = j - 1_ilp + stdlib_idamax( m-j+1, a( j, j ), 1_ilp )
              ipiv( j ) = jp
              if( a( jp, j )/=zero ) then
                 ! apply the interchange to columns 1:n.
                 if( jp/=j )call stdlib_dswap( n, a( j, 1_ilp ), lda, a( jp, 1_ilp ), lda )
                 ! compute elements j+1:m of j-th column.
                 if( j<m ) then
                    if( abs(a( j, j )) >= sfmin ) then
                       call stdlib_dscal( m-j, one / a( j, j ), a( j+1, j ), 1_ilp )
                    else
                      do i = 1, m-j
                         a( j+i, j ) = a( j+i, j ) / a( j, j )
                      end do
                    end if
                 end if
              else if( info==0_ilp ) then
                 info = j
              end if
              if( j<min( m, n ) ) then
                 ! update trailing submatrix.
                 call stdlib_dger( m-j, n-j, -one, a( j+1, j ), 1_ilp, a( j, j+1 ), lda,a( j+1, j+1 ),&
                            lda )
              end if
           end do
           return
     end subroutine stdlib_dgetf2


     pure module subroutine stdlib_cgetf2( m, n, a, lda, ipiv, info )
     !! CGETF2 computes an LU factorization of a general m-by-n matrix A
     !! using partial pivoting with row interchanges.
     !! The factorization has the form
     !! A = P * L * U
     !! where P is a permutation matrix, L is lower triangular with unit
     !! diagonal elements (lower trapezoidal if m > n), and U is upper
     !! triangular (upper trapezoidal if m < n).
     !! This is the right-looking Level 2 BLAS version of the algorithm.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(sp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: sfmin
           integer(ilp) :: i, j, jp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGETF2', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 )return
           ! compute machine safe minimum
           sfmin = stdlib_slamch('S')
           do j = 1, min( m, n )
              ! find pivot and test for singularity.
              jp = j - 1_ilp + stdlib_icamax( m-j+1, a( j, j ), 1_ilp )
              ipiv( j ) = jp
              if( a( jp, j )/=czero ) then
                 ! apply the interchange to columns 1:n.
                 if( jp/=j )call stdlib_cswap( n, a( j, 1_ilp ), lda, a( jp, 1_ilp ), lda )
                 ! compute elements j+1:m of j-th column.
                 if( j<m ) then
                    if( abs(a( j, j )) >= sfmin ) then
                       call stdlib_cscal( m-j, cone / a( j, j ), a( j+1, j ), 1_ilp )
                    else
                       do i = 1, m-j
                          a( j+i, j ) = a( j+i, j ) / a( j, j )
                       end do
                    end if
                 end if
              else if( info==0_ilp ) then
                 info = j
              end if
              if( j<min( m, n ) ) then
                 ! update trailing submatrix.
                 call stdlib_cgeru( m-j, n-j, -cone, a( j+1, j ), 1_ilp, a( j, j+1 ),lda, a( j+1, j+1 &
                           ), lda )
              end if
           end do
           return
     end subroutine stdlib_cgetf2

     pure module subroutine stdlib_zgetf2( m, n, a, lda, ipiv, info )
     !! ZGETF2 computes an LU factorization of a general m-by-n matrix A
     !! using partial pivoting with row interchanges.
     !! The factorization has the form
     !! A = P * L * U
     !! where P is a permutation matrix, L is lower triangular with unit
     !! diagonal elements (lower trapezoidal if m > n), and U is upper
     !! triangular (upper trapezoidal if m < n).
     !! This is the right-looking Level 2 BLAS version of the algorithm.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(dp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: sfmin
           integer(ilp) :: i, j, jp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGETF2', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 )return
           ! compute machine safe minimum
           sfmin = stdlib_dlamch('S')
           do j = 1, min( m, n )
              ! find pivot and test for singularity.
              jp = j - 1_ilp + stdlib_izamax( m-j+1, a( j, j ), 1_ilp )
              ipiv( j ) = jp
              if( a( jp, j )/=czero ) then
                 ! apply the interchange to columns 1:n.
                 if( jp/=j )call stdlib_zswap( n, a( j, 1_ilp ), lda, a( jp, 1_ilp ), lda )
                 ! compute elements j+1:m of j-th column.
                 if( j<m ) then
                    if( abs(a( j, j )) >= sfmin ) then
                       call stdlib_zscal( m-j, cone / a( j, j ), a( j+1, j ), 1_ilp )
                    else
                       do i = 1, m-j
                          a( j+i, j ) = a( j+i, j ) / a( j, j )
                       end do
                    end if
                 end if
              else if( info==0_ilp ) then
                 info = j
              end if
              if( j<min( m, n ) ) then
                 ! update trailing submatrix.
                 call stdlib_zgeru( m-j, n-j, -cone, a( j+1, j ), 1_ilp, a( j, j+1 ),lda, a( j+1, j+1 &
                           ), lda )
              end if
           end do
           return
     end subroutine stdlib_zgetf2




     pure module subroutine stdlib_sgetrs( trans, n, nrhs, a, lda, ipiv, b, ldb, info )
     !! SGETRS solves a system of linear equations
     !! A * X = B  or  A**T * X = B
     !! with a general N-by-N matrix A using the LU factorization computed
     !! by SGETRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: notran
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           notran = stdlib_lsame( trans, 'N' )
           if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( trans, &
                     'C' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGETRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( notran ) then
              ! solve a * x = b.
              ! apply row interchanges to the right hand sides.
              call stdlib_slaswp( nrhs, b, ldb, 1_ilp, n, ipiv, 1_ilp )
              ! solve l*x = b, overwriting b with x.
              call stdlib_strsm( 'LEFT', 'LOWER', 'NO TRANSPOSE', 'UNIT', n, nrhs,one, a, lda, b, &
                        ldb )
              ! solve u*x = b, overwriting b with x.
              call stdlib_strsm( 'LEFT', 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', n,nrhs, one, a, lda,&
                         b, ldb )
           else
              ! solve a**t * x = b.
              ! solve u**t *x = b, overwriting b with x.
              call stdlib_strsm( 'LEFT', 'UPPER', 'TRANSPOSE', 'NON-UNIT', n, nrhs,one, a, lda, b,&
                         ldb )
              ! solve l**t *x = b, overwriting b with x.
              call stdlib_strsm( 'LEFT', 'LOWER', 'TRANSPOSE', 'UNIT', n, nrhs, one,a, lda, b, &
                        ldb )
              ! apply row interchanges to the solution vectors.
              call stdlib_slaswp( nrhs, b, ldb, 1_ilp, n, ipiv, -1_ilp )
           end if
           return
     end subroutine stdlib_sgetrs

     pure module subroutine stdlib_dgetrs( trans, n, nrhs, a, lda, ipiv, b, ldb, info )
     !! DGETRS solves a system of linear equations
     !! A * X = B  or  A**T * X = B
     !! with a general N-by-N matrix A using the LU factorization computed
     !! by DGETRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: notran
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           notran = stdlib_lsame( trans, 'N' )
           if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( trans, &
                     'C' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGETRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( notran ) then
              ! solve a * x = b.
              ! apply row interchanges to the right hand sides.
              call stdlib_dlaswp( nrhs, b, ldb, 1_ilp, n, ipiv, 1_ilp )
              ! solve l*x = b, overwriting b with x.
              call stdlib_dtrsm( 'LEFT', 'LOWER', 'NO TRANSPOSE', 'UNIT', n, nrhs,one, a, lda, b, &
                        ldb )
              ! solve u*x = b, overwriting b with x.
              call stdlib_dtrsm( 'LEFT', 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', n,nrhs, one, a, lda,&
                         b, ldb )
           else
              ! solve a**t * x = b.
              ! solve u**t *x = b, overwriting b with x.
              call stdlib_dtrsm( 'LEFT', 'UPPER', 'TRANSPOSE', 'NON-UNIT', n, nrhs,one, a, lda, b,&
                         ldb )
              ! solve l**t *x = b, overwriting b with x.
              call stdlib_dtrsm( 'LEFT', 'LOWER', 'TRANSPOSE', 'UNIT', n, nrhs, one,a, lda, b, &
                        ldb )
              ! apply row interchanges to the solution vectors.
              call stdlib_dlaswp( nrhs, b, ldb, 1_ilp, n, ipiv, -1_ilp )
           end if
           return
     end subroutine stdlib_dgetrs


     pure module subroutine stdlib_cgetrs( trans, n, nrhs, a, lda, ipiv, b, ldb, info )
     !! CGETRS solves a system of linear equations
     !! A * X = B,  A**T * X = B,  or  A**H * X = B
     !! with a general N-by-N matrix A using the LU factorization computed
     !! by CGETRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: notran
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           notran = stdlib_lsame( trans, 'N' )
           if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( trans, &
                     'C' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGETRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( notran ) then
              ! solve a * x = b.
              ! apply row interchanges to the right hand sides.
              call stdlib_claswp( nrhs, b, ldb, 1_ilp, n, ipiv, 1_ilp )
              ! solve l*x = b, overwriting b with x.
              call stdlib_ctrsm( 'LEFT', 'LOWER', 'NO TRANSPOSE', 'UNIT', n, nrhs,cone, a, lda, b,&
                         ldb )
              ! solve u*x = b, overwriting b with x.
              call stdlib_ctrsm( 'LEFT', 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', n,nrhs, cone, a, &
                        lda, b, ldb )
           else
              ! solve a**t * x = b  or a**h * x = b.
              ! solve u**t *x = b or u**h *x = b, overwriting b with x.
              call stdlib_ctrsm( 'LEFT', 'UPPER', trans, 'NON-UNIT', n, nrhs, cone,a, lda, b, ldb &
                        )
              ! solve l**t *x = b, or l**h *x = b overwriting b with x.
              call stdlib_ctrsm( 'LEFT', 'LOWER', trans, 'UNIT', n, nrhs, cone, a,lda, b, ldb )
                        
              ! apply row interchanges to the solution vectors.
              call stdlib_claswp( nrhs, b, ldb, 1_ilp, n, ipiv, -1_ilp )
           end if
           return
     end subroutine stdlib_cgetrs

     pure module subroutine stdlib_zgetrs( trans, n, nrhs, a, lda, ipiv, b, ldb, info )
     !! ZGETRS solves a system of linear equations
     !! A * X = B,  A**T * X = B,  or  A**H * X = B
     !! with a general N-by-N matrix A using the LU factorization computed
     !! by ZGETRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: notran
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           notran = stdlib_lsame( trans, 'N' )
           if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( trans, &
                     'C' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGETRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( notran ) then
              ! solve a * x = b.
              ! apply row interchanges to the right hand sides.
              call stdlib_zlaswp( nrhs, b, ldb, 1_ilp, n, ipiv, 1_ilp )
              ! solve l*x = b, overwriting b with x.
              call stdlib_ztrsm( 'LEFT', 'LOWER', 'NO TRANSPOSE', 'UNIT', n, nrhs,cone, a, lda, b,&
                         ldb )
              ! solve u*x = b, overwriting b with x.
              call stdlib_ztrsm( 'LEFT', 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', n,nrhs, cone, a, &
                        lda, b, ldb )
           else
              ! solve a**t * x = b  or a**h * x = b.
              ! solve u**t *x = b or u**h *x = b, overwriting b with x.
              call stdlib_ztrsm( 'LEFT', 'UPPER', trans, 'NON-UNIT', n, nrhs, cone,a, lda, b, ldb &
                        )
              ! solve l**t *x = b, or l**h *x = b overwriting b with x.
              call stdlib_ztrsm( 'LEFT', 'LOWER', trans, 'UNIT', n, nrhs, cone, a,lda, b, ldb )
                        
              ! apply row interchanges to the solution vectors.
              call stdlib_zlaswp( nrhs, b, ldb, 1_ilp, n, ipiv, -1_ilp )
           end if
           return
     end subroutine stdlib_zgetrs




     pure module subroutine stdlib_sgetri( n, a, lda, ipiv, work, lwork, info )
     !! SGETRI computes the inverse of a matrix using the LU factorization
     !! computed by SGETRF.
     !! This method inverts U and then computes inv(A) by solving the system
     !! inv(A)*L = inv(U) for inv(A).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, n
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, iws, j, jb, jj, jp, ldwork, lwkopt, nb, nbmin, nn
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           nb = stdlib_ilaenv( 1_ilp, 'SGETRI', ' ', n, -1_ilp, -1_ilp, -1_ilp )
           lwkopt = n*nb
           work( 1_ilp ) = lwkopt
           lquery = ( lwork==-1_ilp )
           if( n<0_ilp ) then
              info = -1_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -3_ilp
           else if( lwork<max( 1_ilp, n ) .and. .not.lquery ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGETRI', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form inv(u).  if info > 0 from stdlib_strtri, then u is singular,
           ! and the inverse is not computed.
           call stdlib_strtri( 'UPPER', 'NON-UNIT', n, a, lda, info )
           if( info>0 )return
           nbmin = 2_ilp
           ldwork = n
           if( nb>1_ilp .and. nb<n ) then
              iws = max( ldwork*nb, 1_ilp )
              if( lwork<iws ) then
                 nb = lwork / ldwork
                 nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'SGETRI', ' ', n, -1_ilp, -1_ilp, -1_ilp ) )
              end if
           else
              iws = n
           end if
           ! solve the equation inv(a)*l = inv(u) for inv(a).
           if( nb<nbmin .or. nb>=n ) then
              ! use unblocked code.
              do j = n, 1, -1
                 ! copy current column of l to work and replace with zeros.
                 do i = j + 1, n
                    work( i ) = a( i, j )
                    a( i, j ) = zero
                 end do
                 ! compute current column of inv(a).
                 if( j<n )call stdlib_sgemv( 'NO TRANSPOSE', n, n-j, -one, a( 1_ilp, j+1 ),lda, work( &
                           j+1 ), 1_ilp, one, a( 1_ilp, j ), 1_ilp )
              end do
           else
              ! use blocked code.
              nn = ( ( n-1 ) / nb )*nb + 1_ilp
              do j = nn, 1, -nb
                 jb = min( nb, n-j+1 )
                 ! copy current block column of l to work and replace with
                 ! zeros.
                 do jj = j, j + jb - 1
                    do i = jj + 1, n
                       work( i+( jj-j )*ldwork ) = a( i, jj )
                       a( i, jj ) = zero
                    end do
                 end do
                 ! compute current block column of inv(a).
                 if( j+jb<=n )call stdlib_sgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', n, jb,n-j-jb+1, -&
                           one, a( 1_ilp, j+jb ), lda,work( j+jb ), ldwork, one, a( 1_ilp, j ), lda )
                 call stdlib_strsm( 'RIGHT', 'LOWER', 'NO TRANSPOSE', 'UNIT', n, jb,one, work( j )&
                           , ldwork, a( 1_ilp, j ), lda )
              end do
           end if
           ! apply column interchanges.
           do j = n - 1, 1, -1
              jp = ipiv( j )
              if( jp/=j )call stdlib_sswap( n, a( 1_ilp, j ), 1_ilp, a( 1_ilp, jp ), 1_ilp )
           end do
           work( 1_ilp ) = iws
           return
     end subroutine stdlib_sgetri

     pure module subroutine stdlib_dgetri( n, a, lda, ipiv, work, lwork, info )
     !! DGETRI computes the inverse of a matrix using the LU factorization
     !! computed by DGETRF.
     !! This method inverts U and then computes inv(A) by solving the system
     !! inv(A)*L = inv(U) for inv(A).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, n
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, iws, j, jb, jj, jp, ldwork, lwkopt, nb, nbmin, nn
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           nb = stdlib_ilaenv( 1_ilp, 'DGETRI', ' ', n, -1_ilp, -1_ilp, -1_ilp )
           lwkopt = n*nb
           work( 1_ilp ) = lwkopt
           lquery = ( lwork==-1_ilp )
           if( n<0_ilp ) then
              info = -1_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -3_ilp
           else if( lwork<max( 1_ilp, n ) .and. .not.lquery ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGETRI', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form inv(u).  if info > 0 from stdlib_dtrtri, then u is singular,
           ! and the inverse is not computed.
           call stdlib_dtrtri( 'UPPER', 'NON-UNIT', n, a, lda, info )
           if( info>0 )return
           nbmin = 2_ilp
           ldwork = n
           if( nb>1_ilp .and. nb<n ) then
              iws = max( ldwork*nb, 1_ilp )
              if( lwork<iws ) then
                 nb = lwork / ldwork
                 nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'DGETRI', ' ', n, -1_ilp, -1_ilp, -1_ilp ) )
              end if
           else
              iws = n
           end if
           ! solve the equation inv(a)*l = inv(u) for inv(a).
           if( nb<nbmin .or. nb>=n ) then
              ! use unblocked code.
              do j = n, 1, -1
                 ! copy current column of l to work and replace with zeros.
                 do i = j + 1, n
                    work( i ) = a( i, j )
                    a( i, j ) = zero
                 end do
                 ! compute current column of inv(a).
                 if( j<n )call stdlib_dgemv( 'NO TRANSPOSE', n, n-j, -one, a( 1_ilp, j+1 ),lda, work( &
                           j+1 ), 1_ilp, one, a( 1_ilp, j ), 1_ilp )
              end do
           else
              ! use blocked code.
              nn = ( ( n-1 ) / nb )*nb + 1_ilp
              do j = nn, 1, -nb
                 jb = min( nb, n-j+1 )
                 ! copy current block column of l to work and replace with
                 ! zeros.
                 do jj = j, j + jb - 1
                    do i = jj + 1, n
                       work( i+( jj-j )*ldwork ) = a( i, jj )
                       a( i, jj ) = zero
                    end do
                 end do
                 ! compute current block column of inv(a).
                 if( j+jb<=n )call stdlib_dgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', n, jb,n-j-jb+1, -&
                           one, a( 1_ilp, j+jb ), lda,work( j+jb ), ldwork, one, a( 1_ilp, j ), lda )
                 call stdlib_dtrsm( 'RIGHT', 'LOWER', 'NO TRANSPOSE', 'UNIT', n, jb,one, work( j )&
                           , ldwork, a( 1_ilp, j ), lda )
              end do
           end if
           ! apply column interchanges.
           do j = n - 1, 1, -1
              jp = ipiv( j )
              if( jp/=j )call stdlib_dswap( n, a( 1_ilp, j ), 1_ilp, a( 1_ilp, jp ), 1_ilp )
           end do
           work( 1_ilp ) = iws
           return
     end subroutine stdlib_dgetri


     pure module subroutine stdlib_cgetri( n, a, lda, ipiv, work, lwork, info )
     !! CGETRI computes the inverse of a matrix using the LU factorization
     !! computed by CGETRF.
     !! This method inverts U and then computes inv(A) by solving the system
     !! inv(A)*L = inv(U) for inv(A).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, n
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, iws, j, jb, jj, jp, ldwork, lwkopt, nb, nbmin, nn
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           nb = stdlib_ilaenv( 1_ilp, 'CGETRI', ' ', n, -1_ilp, -1_ilp, -1_ilp )
           lwkopt = n*nb
           work( 1_ilp ) = lwkopt
           lquery = ( lwork==-1_ilp )
           if( n<0_ilp ) then
              info = -1_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -3_ilp
           else if( lwork<max( 1_ilp, n ) .and. .not.lquery ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGETRI', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form inv(u).  if info > 0 from stdlib_ctrtri, then u is singular,
           ! and the inverse is not computed.
           call stdlib_ctrtri( 'UPPER', 'NON-UNIT', n, a, lda, info )
           if( info>0 )return
           nbmin = 2_ilp
           ldwork = n
           if( nb>1_ilp .and. nb<n ) then
              iws = max( ldwork*nb, 1_ilp )
              if( lwork<iws ) then
                 nb = lwork / ldwork
                 nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'CGETRI', ' ', n, -1_ilp, -1_ilp, -1_ilp ) )
              end if
           else
              iws = n
           end if
           ! solve the equation inv(a)*l = inv(u) for inv(a).
           if( nb<nbmin .or. nb>=n ) then
              ! use unblocked code.
              do j = n, 1, -1
                 ! copy current column of l to work and replace with zeros.
                 do i = j + 1, n
                    work( i ) = a( i, j )
                    a( i, j ) = czero
                 end do
                 ! compute current column of inv(a).
                 if( j<n )call stdlib_cgemv( 'NO TRANSPOSE', n, n-j, -cone, a( 1_ilp, j+1 ),lda, work(&
                            j+1 ), 1_ilp, cone, a( 1_ilp, j ), 1_ilp )
              end do
           else
              ! use blocked code.
              nn = ( ( n-1 ) / nb )*nb + 1_ilp
              do j = nn, 1, -nb
                 jb = min( nb, n-j+1 )
                 ! copy current block column of l to work and replace with
                 ! zeros.
                 do jj = j, j + jb - 1
                    do i = jj + 1, n
                       work( i+( jj-j )*ldwork ) = a( i, jj )
                       a( i, jj ) = czero
                    end do
                 end do
                 ! compute current block column of inv(a).
                 if( j+jb<=n )call stdlib_cgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', n, jb,n-j-jb+1, -&
                           cone, a( 1_ilp, j+jb ), lda,work( j+jb ), ldwork, cone, a( 1_ilp, j ), lda )
                 call stdlib_ctrsm( 'RIGHT', 'LOWER', 'NO TRANSPOSE', 'UNIT', n, jb,cone, work( j &
                           ), ldwork, a( 1_ilp, j ), lda )
              end do
           end if
           ! apply column interchanges.
           do j = n - 1, 1, -1
              jp = ipiv( j )
              if( jp/=j )call stdlib_cswap( n, a( 1_ilp, j ), 1_ilp, a( 1_ilp, jp ), 1_ilp )
           end do
           work( 1_ilp ) = iws
           return
     end subroutine stdlib_cgetri

     pure module subroutine stdlib_zgetri( n, a, lda, ipiv, work, lwork, info )
     !! ZGETRI computes the inverse of a matrix using the LU factorization
     !! computed by ZGETRF.
     !! This method inverts U and then computes inv(A) by solving the system
     !! inv(A)*L = inv(U) for inv(A).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, n
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, iws, j, jb, jj, jp, ldwork, lwkopt, nb, nbmin, nn
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           nb = stdlib_ilaenv( 1_ilp, 'ZGETRI', ' ', n, -1_ilp, -1_ilp, -1_ilp )
           lwkopt = n*nb
           work( 1_ilp ) = lwkopt
           lquery = ( lwork==-1_ilp )
           if( n<0_ilp ) then
              info = -1_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -3_ilp
           else if( lwork<max( 1_ilp, n ) .and. .not.lquery ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGETRI', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form inv(u).  if info > 0 from stdlib_ztrtri, then u is singular,
           ! and the inverse is not computed.
           call stdlib_ztrtri( 'UPPER', 'NON-UNIT', n, a, lda, info )
           if( info>0 )return
           nbmin = 2_ilp
           ldwork = n
           if( nb>1_ilp .and. nb<n ) then
              iws = max( ldwork*nb, 1_ilp )
              if( lwork<iws ) then
                 nb = lwork / ldwork
                 nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'ZGETRI', ' ', n, -1_ilp, -1_ilp, -1_ilp ) )
              end if
           else
              iws = n
           end if
           ! solve the equation inv(a)*l = inv(u) for inv(a).
           if( nb<nbmin .or. nb>=n ) then
              ! use unblocked code.
              do j = n, 1, -1
                 ! copy current column of l to work and replace with zeros.
                 do i = j + 1, n
                    work( i ) = a( i, j )
                    a( i, j ) = czero
                 end do
                 ! compute current column of inv(a).
                 if( j<n )call stdlib_zgemv( 'NO TRANSPOSE', n, n-j, -cone, a( 1_ilp, j+1 ),lda, work(&
                            j+1 ), 1_ilp, cone, a( 1_ilp, j ), 1_ilp )
              end do
           else
              ! use blocked code.
              nn = ( ( n-1 ) / nb )*nb + 1_ilp
              do j = nn, 1, -nb
                 jb = min( nb, n-j+1 )
                 ! copy current block column of l to work and replace with
                 ! zeros.
                 do jj = j, j + jb - 1
                    do i = jj + 1, n
                       work( i+( jj-j )*ldwork ) = a( i, jj )
                       a( i, jj ) = czero
                    end do
                 end do
                 ! compute current block column of inv(a).
                 if( j+jb<=n )call stdlib_zgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', n, jb,n-j-jb+1, -&
                           cone, a( 1_ilp, j+jb ), lda,work( j+jb ), ldwork, cone, a( 1_ilp, j ), lda )
                 call stdlib_ztrsm( 'RIGHT', 'LOWER', 'NO TRANSPOSE', 'UNIT', n, jb,cone, work( j &
                           ), ldwork, a( 1_ilp, j ), lda )
              end do
           end if
           ! apply column interchanges.
           do j = n - 1, 1, -1
              jp = ipiv( j )
              if( jp/=j )call stdlib_zswap( n, a( 1_ilp, j ), 1_ilp, a( 1_ilp, jp ), 1_ilp )
           end do
           work( 1_ilp ) = iws
           return
     end subroutine stdlib_zgetri




     pure module subroutine stdlib_sgerfs( trans, n, nrhs, a, lda, af, ldaf, ipiv, b, ldb,x, ldx, ferr, &
     !! SGERFS improves the computed solution to a system of linear
     !! equations and provides error bounds and backward error estimates for
     !! the solution.
               berr, work, iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldaf, ldb, ldx, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(in) :: a(lda,*), af(ldaf,*), b(ldb,*)
           real(sp), intent(out) :: berr(*), ferr(*), work(*)
           real(sp), intent(inout) :: x(ldx,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           
           
           
           ! Local Scalars 
           logical(lk) :: notran
           character :: transt
           integer(ilp) :: count, i, j, k, kase, nz
           real(sp) :: eps, lstres, s, safe1, safe2, safmin, xk
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           notran = stdlib_lsame( trans, 'N' )
           if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( trans, &
                     'C' ) ) then
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
              info = -10_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -12_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGERFS', -info )
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
           if( notran ) then
              transt = 'T'
           else
              transt = 'N'
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
              ! compute residual r = b - op(a) * x,
              ! where op(a) = a, a**t, or a**h, depending on trans.
              call stdlib_scopy( n, b( 1_ilp, j ), 1_ilp, work( n+1 ), 1_ilp )
              call stdlib_sgemv( trans, n, n, -one, a, lda, x( 1_ilp, j ), 1_ilp, one,work( n+1 ), 1_ilp )
                        
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(op(a))*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              do i = 1, n
                 work( i ) = abs( b( i, j ) )
              end do
              ! compute abs(op(a))*abs(x) + abs(b).
              if( notran ) then
                 do k = 1, n
                    xk = abs( x( k, j ) )
                    do i = 1, n
                       work( i ) = work( i ) + abs( a( i, k ) )*xk
                    end do
                 end do
              else
                 do k = 1, n
                    s = zero
                    do i = 1, n
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
                 call stdlib_sgetrs( trans, n, 1_ilp, af, ldaf, ipiv, work( n+1 ), n,info )
                 call stdlib_saxpy( n, one, work( n+1 ), 1_ilp, x( 1_ilp, j ), 1_ilp )
                 lstres = berr( j )
                 count = count + 1_ilp
                 go to 20
              end if
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(op(a)))*
                 ! ( abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(op(a)) is the inverse of op(a)
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(op(a))*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(op(a))*abs(x) + abs(b) is less than safe2.
              ! use stdlib_slacn2 to estimate the infinity-norm of the matrix
                 ! inv(op(a)) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) )))
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
                    ! multiply by diag(w)*inv(op(a)**t).
                    call stdlib_sgetrs( transt, n, 1_ilp, af, ldaf, ipiv, work( n+1 ),n, info )
                              
                    do i = 1, n
                       work( n+i ) = work( i )*work( n+i )
                    end do
                 else
                    ! multiply by inv(op(a))*diag(w).
                    do i = 1, n
                       work( n+i ) = work( i )*work( n+i )
                    end do
                    call stdlib_sgetrs( trans, n, 1_ilp, af, ldaf, ipiv, work( n+1 ), n,info )
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
     end subroutine stdlib_sgerfs

     pure module subroutine stdlib_dgerfs( trans, n, nrhs, a, lda, af, ldaf, ipiv, b, ldb,x, ldx, ferr, &
     !! DGERFS improves the computed solution to a system of linear
     !! equations and provides error bounds and backward error estimates for
     !! the solution.
               berr, work, iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldaf, ldb, ldx, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(in) :: a(lda,*), af(ldaf,*), b(ldb,*)
           real(dp), intent(out) :: berr(*), ferr(*), work(*)
           real(dp), intent(inout) :: x(ldx,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           
           
           
           ! Local Scalars 
           logical(lk) :: notran
           character :: transt
           integer(ilp) :: count, i, j, k, kase, nz
           real(dp) :: eps, lstres, s, safe1, safe2, safmin, xk
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           notran = stdlib_lsame( trans, 'N' )
           if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( trans, &
                     'C' ) ) then
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
              info = -10_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -12_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGERFS', -info )
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
           if( notran ) then
              transt = 'T'
           else
              transt = 'N'
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
              ! compute residual r = b - op(a) * x,
              ! where op(a) = a, a**t, or a**h, depending on trans.
              call stdlib_dcopy( n, b( 1_ilp, j ), 1_ilp, work( n+1 ), 1_ilp )
              call stdlib_dgemv( trans, n, n, -one, a, lda, x( 1_ilp, j ), 1_ilp, one,work( n+1 ), 1_ilp )
                        
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(op(a))*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              do i = 1, n
                 work( i ) = abs( b( i, j ) )
              end do
              ! compute abs(op(a))*abs(x) + abs(b).
              if( notran ) then
                 do k = 1, n
                    xk = abs( x( k, j ) )
                    do i = 1, n
                       work( i ) = work( i ) + abs( a( i, k ) )*xk
                    end do
                 end do
              else
                 do k = 1, n
                    s = zero
                    do i = 1, n
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
                 call stdlib_dgetrs( trans, n, 1_ilp, af, ldaf, ipiv, work( n+1 ), n,info )
                 call stdlib_daxpy( n, one, work( n+1 ), 1_ilp, x( 1_ilp, j ), 1_ilp )
                 lstres = berr( j )
                 count = count + 1_ilp
                 go to 20
              end if
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(op(a)))*
                 ! ( abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(op(a)) is the inverse of op(a)
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(op(a))*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(op(a))*abs(x) + abs(b) is less than safe2.
              ! use stdlib_dlacn2 to estimate the infinity-norm of the matrix
                 ! inv(op(a)) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) )))
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
                    ! multiply by diag(w)*inv(op(a)**t).
                    call stdlib_dgetrs( transt, n, 1_ilp, af, ldaf, ipiv, work( n+1 ),n, info )
                              
                    do i = 1, n
                       work( n+i ) = work( i )*work( n+i )
                    end do
                 else
                    ! multiply by inv(op(a))*diag(w).
                    do i = 1, n
                       work( n+i ) = work( i )*work( n+i )
                    end do
                    call stdlib_dgetrs( trans, n, 1_ilp, af, ldaf, ipiv, work( n+1 ), n,info )
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
     end subroutine stdlib_dgerfs


     pure module subroutine stdlib_cgerfs( trans, n, nrhs, a, lda, af, ldaf, ipiv, b, ldb,x, ldx, ferr, &
     !! CGERFS improves the computed solution to a system of linear
     !! equations and provides error bounds and backward error estimates for
     !! the solution.
               berr, work, rwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldaf, ldb, ldx, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(sp), intent(out) :: berr(*), ferr(*), rwork(*)
           complex(sp), intent(in) :: a(lda,*), af(ldaf,*), b(ldb,*)
           complex(sp), intent(out) :: work(*)
           complex(sp), intent(inout) :: x(ldx,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           
           
           
           ! Local Scalars 
           logical(lk) :: notran
           character :: transn, transt
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
           notran = stdlib_lsame( trans, 'N' )
           if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( trans, &
                     'C' ) ) then
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
              info = -10_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -12_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGERFS', -info )
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
           if( notran ) then
              transn = 'N'
              transt = 'C'
           else
              transn = 'C'
              transt = 'N'
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
              ! compute residual r = b - op(a) * x,
              ! where op(a) = a, a**t, or a**h, depending on trans.
              call stdlib_ccopy( n, b( 1_ilp, j ), 1_ilp, work, 1_ilp )
              call stdlib_cgemv( trans, n, n, -cone, a, lda, x( 1_ilp, j ), 1_ilp, cone, work,1_ilp )
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(op(a))*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              do i = 1, n
                 rwork( i ) = cabs1( b( i, j ) )
              end do
              ! compute abs(op(a))*abs(x) + abs(b).
              if( notran ) then
                 do k = 1, n
                    xk = cabs1( x( k, j ) )
                    do i = 1, n
                       rwork( i ) = rwork( i ) + cabs1( a( i, k ) )*xk
                    end do
                 end do
              else
                 do k = 1, n
                    s = zero
                    do i = 1, n
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
                 call stdlib_cgetrs( trans, n, 1_ilp, af, ldaf, ipiv, work, n, info )
                 call stdlib_caxpy( n, cone, work, 1_ilp, x( 1_ilp, j ), 1_ilp )
                 lstres = berr( j )
                 count = count + 1_ilp
                 go to 20
              end if
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(op(a)))*
                 ! ( abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(op(a)) is the inverse of op(a)
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(op(a))*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(op(a))*abs(x) + abs(b) is less than safe2.
              ! use stdlib_clacn2 to estimate the infinity-norm of the matrix
                 ! inv(op(a)) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) )))
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
                    ! multiply by diag(w)*inv(op(a)**h).
                    call stdlib_cgetrs( transt, n, 1_ilp, af, ldaf, ipiv, work, n,info )
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                 else
                    ! multiply by inv(op(a))*diag(w).
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                    call stdlib_cgetrs( transn, n, 1_ilp, af, ldaf, ipiv, work, n,info )
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
     end subroutine stdlib_cgerfs

     pure module subroutine stdlib_zgerfs( trans, n, nrhs, a, lda, af, ldaf, ipiv, b, ldb,x, ldx, ferr, &
     !! ZGERFS improves the computed solution to a system of linear
     !! equations and provides error bounds and backward error estimates for
     !! the solution.
               berr, work, rwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldaf, ldb, ldx, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(dp), intent(out) :: berr(*), ferr(*), rwork(*)
           complex(dp), intent(in) :: a(lda,*), af(ldaf,*), b(ldb,*)
           complex(dp), intent(out) :: work(*)
           complex(dp), intent(inout) :: x(ldx,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           
           
           
           ! Local Scalars 
           logical(lk) :: notran
           character :: transn, transt
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
           notran = stdlib_lsame( trans, 'N' )
           if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( trans, &
                     'C' ) ) then
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
              info = -10_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -12_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGERFS', -info )
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
           if( notran ) then
              transn = 'N'
              transt = 'C'
           else
              transn = 'C'
              transt = 'N'
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
              ! compute residual r = b - op(a) * x,
              ! where op(a) = a, a**t, or a**h, depending on trans.
              call stdlib_zcopy( n, b( 1_ilp, j ), 1_ilp, work, 1_ilp )
              call stdlib_zgemv( trans, n, n, -cone, a, lda, x( 1_ilp, j ), 1_ilp, cone, work,1_ilp )
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(op(a))*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              do i = 1, n
                 rwork( i ) = cabs1( b( i, j ) )
              end do
              ! compute abs(op(a))*abs(x) + abs(b).
              if( notran ) then
                 do k = 1, n
                    xk = cabs1( x( k, j ) )
                    do i = 1, n
                       rwork( i ) = rwork( i ) + cabs1( a( i, k ) )*xk
                    end do
                 end do
              else
                 do k = 1, n
                    s = zero
                    do i = 1, n
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
                 call stdlib_zgetrs( trans, n, 1_ilp, af, ldaf, ipiv, work, n, info )
                 call stdlib_zaxpy( n, cone, work, 1_ilp, x( 1_ilp, j ), 1_ilp )
                 lstres = berr( j )
                 count = count + 1_ilp
                 go to 20
              end if
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(op(a)))*
                 ! ( abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(op(a)) is the inverse of op(a)
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(op(a))*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(op(a))*abs(x) + abs(b) is less than safe2.
              ! use stdlib_zlacn2 to estimate the infinity-norm of the matrix
                 ! inv(op(a)) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) )))
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
                    ! multiply by diag(w)*inv(op(a)**h).
                    call stdlib_zgetrs( transt, n, 1_ilp, af, ldaf, ipiv, work, n,info )
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                 else
                    ! multiply by inv(op(a))*diag(w).
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                    call stdlib_zgetrs( transn, n, 1_ilp, af, ldaf, ipiv, work, n,info )
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
     end subroutine stdlib_zgerfs




     pure module subroutine stdlib_sgeequ( m, n, a, lda, r, c, rowcnd, colcnd, amax,info )
     !! SGEEQU computes row and column scalings intended to equilibrate an
     !! M-by-N matrix A and reduce its condition number.  R returns the row
     !! scale factors and C the column scale factors, chosen to try to make
     !! the largest element in each row and column of the matrix B with
     !! elements B(i,j)=R(i)*A(i,j)*C(j) have absolute value 1.
     !! R(i) and C(j) are restricted to be between SMLNUM = smallest safe
     !! number and BIGNUM = largest safe number.  Use of these scaling
     !! factors is not guaranteed to reduce the condition number of A but
     !! works well in practice.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           real(sp), intent(out) :: amax, colcnd, rowcnd
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(out) :: c(*), r(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j
           real(sp) :: bignum, rcmax, rcmin, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGEEQU', -info )
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp ) then
              rowcnd = one
              colcnd = one
              amax = zero
              return
           end if
           ! get machine constants.
           smlnum = stdlib_slamch( 'S' )
           bignum = one / smlnum
           ! compute row scale factors.
           do i = 1, m
              r( i ) = zero
           end do
           ! find the maximum element in each row.
           do j = 1, n
              do i = 1, m
                 r( i ) = max( r( i ), abs( a( i, j ) ) )
              end do
           end do
           ! find the maximum and minimum scale factors.
           rcmin = bignum
           rcmax = zero
           do i = 1, m
              rcmax = max( rcmax, r( i ) )
              rcmin = min( rcmin, r( i ) )
           end do
           amax = rcmax
           if( rcmin==zero ) then
              ! find the first zero scale factor and return an error code.
              do i = 1, m
                 if( r( i )==zero ) then
                    info = i
                    return
                 end if
              end do
           else
              ! invert the scale factors.
              do i = 1, m
                 r( i ) = one / min( max( r( i ), smlnum ), bignum )
              end do
              ! compute rowcnd = min(r(i)) / max(r(i))
              rowcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
           end if
           ! compute column scale factors
           do j = 1, n
              c( j ) = zero
           end do
           ! find the maximum element in each column,
           ! assuming the row scaling computed above.
           do j = 1, n
              do i = 1, m
                 c( j ) = max( c( j ), abs( a( i, j ) )*r( i ) )
              end do
           end do
           ! find the maximum and minimum scale factors.
           rcmin = bignum
           rcmax = zero
           do j = 1, n
              rcmin = min( rcmin, c( j ) )
              rcmax = max( rcmax, c( j ) )
           end do
           if( rcmin==zero ) then
              ! find the first zero scale factor and return an error code.
              do j = 1, n
                 if( c( j )==zero ) then
                    info = m + j
                    return
                 end if
              end do
           else
              ! invert the scale factors.
              do j = 1, n
                 c( j ) = one / min( max( c( j ), smlnum ), bignum )
              end do
              ! compute colcnd = min(c(j)) / max(c(j))
              colcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
           end if
           return
     end subroutine stdlib_sgeequ

     pure module subroutine stdlib_dgeequ( m, n, a, lda, r, c, rowcnd, colcnd, amax,info )
     !! DGEEQU computes row and column scalings intended to equilibrate an
     !! M-by-N matrix A and reduce its condition number.  R returns the row
     !! scale factors and C the column scale factors, chosen to try to make
     !! the largest element in each row and column of the matrix B with
     !! elements B(i,j)=R(i)*A(i,j)*C(j) have absolute value 1.
     !! R(i) and C(j) are restricted to be between SMLNUM = smallest safe
     !! number and BIGNUM = largest safe number.  Use of these scaling
     !! factors is not guaranteed to reduce the condition number of A but
     !! works well in practice.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           real(dp), intent(out) :: amax, colcnd, rowcnd
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(out) :: c(*), r(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j
           real(dp) :: bignum, rcmax, rcmin, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGEEQU', -info )
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp ) then
              rowcnd = one
              colcnd = one
              amax = zero
              return
           end if
           ! get machine constants.
           smlnum = stdlib_dlamch( 'S' )
           bignum = one / smlnum
           ! compute row scale factors.
           do i = 1, m
              r( i ) = zero
           end do
           ! find the maximum element in each row.
           do j = 1, n
              do i = 1, m
                 r( i ) = max( r( i ), abs( a( i, j ) ) )
              end do
           end do
           ! find the maximum and minimum scale factors.
           rcmin = bignum
           rcmax = zero
           do i = 1, m
              rcmax = max( rcmax, r( i ) )
              rcmin = min( rcmin, r( i ) )
           end do
           amax = rcmax
           if( rcmin==zero ) then
              ! find the first zero scale factor and return an error code.
              do i = 1, m
                 if( r( i )==zero ) then
                    info = i
                    return
                 end if
              end do
           else
              ! invert the scale factors.
              do i = 1, m
                 r( i ) = one / min( max( r( i ), smlnum ), bignum )
              end do
              ! compute rowcnd = min(r(i)) / max(r(i))
              rowcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
           end if
           ! compute column scale factors
           do j = 1, n
              c( j ) = zero
           end do
           ! find the maximum element in each column,
           ! assuming the row scaling computed above.
           do j = 1, n
              do i = 1, m
                 c( j ) = max( c( j ), abs( a( i, j ) )*r( i ) )
              end do
           end do
           ! find the maximum and minimum scale factors.
           rcmin = bignum
           rcmax = zero
           do j = 1, n
              rcmin = min( rcmin, c( j ) )
              rcmax = max( rcmax, c( j ) )
           end do
           if( rcmin==zero ) then
              ! find the first zero scale factor and return an error code.
              do j = 1, n
                 if( c( j )==zero ) then
                    info = m + j
                    return
                 end if
              end do
           else
              ! invert the scale factors.
              do j = 1, n
                 c( j ) = one / min( max( c( j ), smlnum ), bignum )
              end do
              ! compute colcnd = min(c(j)) / max(c(j))
              colcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
           end if
           return
     end subroutine stdlib_dgeequ


     pure module subroutine stdlib_cgeequ( m, n, a, lda, r, c, rowcnd, colcnd, amax,info )
     !! CGEEQU computes row and column scalings intended to equilibrate an
     !! M-by-N matrix A and reduce its condition number.  R returns the row
     !! scale factors and C the column scale factors, chosen to try to make
     !! the largest element in each row and column of the matrix B with
     !! elements B(i,j)=R(i)*A(i,j)*C(j) have absolute value 1.
     !! R(i) and C(j) are restricted to be between SMLNUM = smallest safe
     !! number and BIGNUM = largest safe number.  Use of these scaling
     !! factors is not guaranteed to reduce the condition number of A but
     !! works well in practice.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           real(sp), intent(out) :: amax, colcnd, rowcnd
           ! Array Arguments 
           real(sp), intent(out) :: c(*), r(*)
           complex(sp), intent(in) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j
           real(sp) :: bignum, rcmax, rcmin, smlnum
           complex(sp) :: zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGEEQU', -info )
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp ) then
              rowcnd = one
              colcnd = one
              amax = zero
              return
           end if
           ! get machine constants.
           smlnum = stdlib_slamch( 'S' )
           bignum = one / smlnum
           ! compute row scale factors.
           do i = 1, m
              r( i ) = zero
           end do
           ! find the maximum element in each row.
           do j = 1, n
              do i = 1, m
                 r( i ) = max( r( i ), cabs1( a( i, j ) ) )
              end do
           end do
           ! find the maximum and minimum scale factors.
           rcmin = bignum
           rcmax = zero
           do i = 1, m
              rcmax = max( rcmax, r( i ) )
              rcmin = min( rcmin, r( i ) )
           end do
           amax = rcmax
           if( rcmin==zero ) then
              ! find the first zero scale factor and return an error code.
              do i = 1, m
                 if( r( i )==zero ) then
                    info = i
                    return
                 end if
              end do
           else
              ! invert the scale factors.
              do i = 1, m
                 r( i ) = one / min( max( r( i ), smlnum ), bignum )
              end do
              ! compute rowcnd = min(r(i)) / max(r(i))
              rowcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
           end if
           ! compute column scale factors
           do j = 1, n
              c( j ) = zero
           end do
           ! find the maximum element in each column,
           ! assuming the row scaling computed above.
           do j = 1, n
              do i = 1, m
                 c( j ) = max( c( j ), cabs1( a( i, j ) )*r( i ) )
              end do
           end do
           ! find the maximum and minimum scale factors.
           rcmin = bignum
           rcmax = zero
           do j = 1, n
              rcmin = min( rcmin, c( j ) )
              rcmax = max( rcmax, c( j ) )
           end do
           if( rcmin==zero ) then
              ! find the first zero scale factor and return an error code.
              do j = 1, n
                 if( c( j )==zero ) then
                    info = m + j
                    return
                 end if
              end do
           else
              ! invert the scale factors.
              do j = 1, n
                 c( j ) = one / min( max( c( j ), smlnum ), bignum )
              end do
              ! compute colcnd = min(c(j)) / max(c(j))
              colcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
           end if
           return
     end subroutine stdlib_cgeequ

     pure module subroutine stdlib_zgeequ( m, n, a, lda, r, c, rowcnd, colcnd, amax,info )
     !! ZGEEQU computes row and column scalings intended to equilibrate an
     !! M-by-N matrix A and reduce its condition number.  R returns the row
     !! scale factors and C the column scale factors, chosen to try to make
     !! the largest element in each row and column of the matrix B with
     !! elements B(i,j)=R(i)*A(i,j)*C(j) have absolute value 1.
     !! R(i) and C(j) are restricted to be between SMLNUM = smallest safe
     !! number and BIGNUM = largest safe number.  Use of these scaling
     !! factors is not guaranteed to reduce the condition number of A but
     !! works well in practice.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           real(dp), intent(out) :: amax, colcnd, rowcnd
           ! Array Arguments 
           real(dp), intent(out) :: c(*), r(*)
           complex(dp), intent(in) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j
           real(dp) :: bignum, rcmax, rcmin, smlnum
           complex(dp) :: zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGEEQU', -info )
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp ) then
              rowcnd = one
              colcnd = one
              amax = zero
              return
           end if
           ! get machine constants.
           smlnum = stdlib_dlamch( 'S' )
           bignum = one / smlnum
           ! compute row scale factors.
           do i = 1, m
              r( i ) = zero
           end do
           ! find the maximum element in each row.
           do j = 1, n
              do i = 1, m
                 r( i ) = max( r( i ), cabs1( a( i, j ) ) )
              end do
           end do
           ! find the maximum and minimum scale factors.
           rcmin = bignum
           rcmax = zero
           do i = 1, m
              rcmax = max( rcmax, r( i ) )
              rcmin = min( rcmin, r( i ) )
           end do
           amax = rcmax
           if( rcmin==zero ) then
              ! find the first zero scale factor and return an error code.
              do i = 1, m
                 if( r( i )==zero ) then
                    info = i
                    return
                 end if
              end do
           else
              ! invert the scale factors.
              do i = 1, m
                 r( i ) = one / min( max( r( i ), smlnum ), bignum )
              end do
              ! compute rowcnd = min(r(i)) / max(r(i))
              rowcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
           end if
           ! compute column scale factors
           do j = 1, n
              c( j ) = zero
           end do
           ! find the maximum element in each column,
           ! assuming the row scaling computed above.
           do j = 1, n
              do i = 1, m
                 c( j ) = max( c( j ), cabs1( a( i, j ) )*r( i ) )
              end do
           end do
           ! find the maximum and minimum scale factors.
           rcmin = bignum
           rcmax = zero
           do j = 1, n
              rcmin = min( rcmin, c( j ) )
              rcmax = max( rcmax, c( j ) )
           end do
           if( rcmin==zero ) then
              ! find the first zero scale factor and return an error code.
              do j = 1, n
                 if( c( j )==zero ) then
                    info = m + j
                    return
                 end if
              end do
           else
              ! invert the scale factors.
              do j = 1, n
                 c( j ) = one / min( max( c( j ), smlnum ), bignum )
              end do
              ! compute colcnd = min(c(j)) / max(c(j))
              colcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
           end if
           return
     end subroutine stdlib_zgeequ




     pure module subroutine stdlib_sgeequb( m, n, a, lda, r, c, rowcnd, colcnd, amax,info )
     !! SGEEQUB computes row and column scalings intended to equilibrate an
     !! M-by-N matrix A and reduce its condition number.  R returns the row
     !! scale factors and C the column scale factors, chosen to try to make
     !! the largest element in each row and column of the matrix B with
     !! elements B(i,j)=R(i)*A(i,j)*C(j) have an absolute value of at most
     !! the radix.
     !! R(i) and C(j) are restricted to be a power of the radix between
     !! SMLNUM = smallest safe number and BIGNUM = largest safe number.  Use
     !! of these scaling factors is not guaranteed to reduce the condition
     !! number of A but works well in practice.
     !! This routine differs from SGEEQU by restricting the scaling factors
     !! to a power of the radix.  Barring over- and underflow, scaling by
     !! these factors introduces no additional rounding errors.  However, the
     !! scaled entries' magnitudes are no longer approximately 1 but lie
     !! between sqrt(radix) and 1/sqrt(radix).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           real(sp), intent(out) :: amax, colcnd, rowcnd
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(out) :: c(*), r(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j
           real(sp) :: bignum, rcmax, rcmin, smlnum, radix, logrdx
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGEEQUB', -info )
              return
           end if
           ! quick return if possible.
           if( m==0_ilp .or. n==0_ilp ) then
              rowcnd = one
              colcnd = one
              amax = zero
              return
           end if
           ! get machine constants.  assume smlnum is a power of the radix.
           smlnum = stdlib_slamch( 'S' )
           bignum = one / smlnum
           radix = stdlib_slamch( 'B' )
           logrdx = log( radix )
           ! compute row scale factors.
           do i = 1, m
              r( i ) = zero
           end do
           ! find the maximum element in each row.
           do j = 1, n
              do i = 1, m
                 r( i ) = max( r( i ), abs( a( i, j ) ) )
              end do
           end do
           do i = 1, m
              if( r( i )>zero ) then
                 r( i ) = radix**int( log( r( i ) ) / logrdx,KIND=ilp)
              end if
           end do
           ! find the maximum and minimum scale factors.
           rcmin = bignum
           rcmax = zero
           do i = 1, m
              rcmax = max( rcmax, r( i ) )
              rcmin = min( rcmin, r( i ) )
           end do
           amax = rcmax
           if( rcmin==zero ) then
              ! find the first zero scale factor and return an error code.
              do i = 1, m
                 if( r( i )==zero ) then
                    info = i
                    return
                 end if
              end do
           else
              ! invert the scale factors.
              do i = 1, m
                 r( i ) = one / min( max( r( i ), smlnum ), bignum )
              end do
              ! compute rowcnd = min(r(i)) / max(r(i)).
              rowcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
           end if
           ! compute column scale factors
           do j = 1, n
              c( j ) = zero
           end do
           ! find the maximum element in each column,
           ! assuming the row scaling computed above.
           do j = 1, n
              do i = 1, m
                 c( j ) = max( c( j ), abs( a( i, j ) )*r( i ) )
              end do
              if( c( j )>zero ) then
                 c( j ) = radix**int( log( c( j ) ) / logrdx,KIND=ilp)
              end if
           end do
           ! find the maximum and minimum scale factors.
           rcmin = bignum
           rcmax = zero
           do j = 1, n
              rcmin = min( rcmin, c( j ) )
              rcmax = max( rcmax, c( j ) )
           end do
           if( rcmin==zero ) then
              ! find the first zero scale factor and return an error code.
              do j = 1, n
                 if( c( j )==zero ) then
                    info = m + j
                    return
                 end if
              end do
           else
              ! invert the scale factors.
              do j = 1, n
                 c( j ) = one / min( max( c( j ), smlnum ), bignum )
              end do
              ! compute colcnd = min(c(j)) / max(c(j)).
              colcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
           end if
           return
     end subroutine stdlib_sgeequb

     pure module subroutine stdlib_dgeequb( m, n, a, lda, r, c, rowcnd, colcnd, amax,info )
     !! DGEEQUB computes row and column scalings intended to equilibrate an
     !! M-by-N matrix A and reduce its condition number.  R returns the row
     !! scale factors and C the column scale factors, chosen to try to make
     !! the largest element in each row and column of the matrix B with
     !! elements B(i,j)=R(i)*A(i,j)*C(j) have an absolute value of at most
     !! the radix.
     !! R(i) and C(j) are restricted to be a power of the radix between
     !! SMLNUM = smallest safe number and BIGNUM = largest safe number.  Use
     !! of these scaling factors is not guaranteed to reduce the condition
     !! number of A but works well in practice.
     !! This routine differs from DGEEQU by restricting the scaling factors
     !! to a power of the radix.  Barring over- and underflow, scaling by
     !! these factors introduces no additional rounding errors.  However, the
     !! scaled entries' magnitudes are no longer approximately 1 but lie
     !! between sqrt(radix) and 1/sqrt(radix).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           real(dp), intent(out) :: amax, colcnd, rowcnd
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(out) :: c(*), r(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j
           real(dp) :: bignum, rcmax, rcmin, smlnum, radix, logrdx
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGEEQUB', -info )
              return
           end if
           ! quick return if possible.
           if( m==0_ilp .or. n==0_ilp ) then
              rowcnd = one
              colcnd = one
              amax = zero
              return
           end if
           ! get machine constants.  assume smlnum is a power of the radix.
           smlnum = stdlib_dlamch( 'S' )
           bignum = one / smlnum
           radix = stdlib_dlamch( 'B' )
           logrdx = log( radix )
           ! compute row scale factors.
           do i = 1, m
              r( i ) = zero
           end do
           ! find the maximum element in each row.
           do j = 1, n
              do i = 1, m
                 r( i ) = max( r( i ), abs( a( i, j ) ) )
              end do
           end do
           do i = 1, m
              if( r( i )>zero ) then
                 r( i ) = radix**int( log( r( i ) ) / logrdx,KIND=ilp)
              end if
           end do
           ! find the maximum and minimum scale factors.
           rcmin = bignum
           rcmax = zero
           do i = 1, m
              rcmax = max( rcmax, r( i ) )
              rcmin = min( rcmin, r( i ) )
           end do
           amax = rcmax
           if( rcmin==zero ) then
              ! find the first zero scale factor and return an error code.
              do i = 1, m
                 if( r( i )==zero ) then
                    info = i
                    return
                 end if
              end do
           else
              ! invert the scale factors.
              do i = 1, m
                 r( i ) = one / min( max( r( i ), smlnum ), bignum )
              end do
              ! compute rowcnd = min(r(i)) / max(r(i)).
              rowcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
           end if
           ! compute column scale factors
           do j = 1, n
              c( j ) = zero
           end do
           ! find the maximum element in each column,
           ! assuming the row scaling computed above.
           do j = 1, n
              do i = 1, m
                 c( j ) = max( c( j ), abs( a( i, j ) )*r( i ) )
              end do
              if( c( j )>zero ) then
                 c( j ) = radix**int( log( c( j ) ) / logrdx,KIND=ilp)
              end if
           end do
           ! find the maximum and minimum scale factors.
           rcmin = bignum
           rcmax = zero
           do j = 1, n
              rcmin = min( rcmin, c( j ) )
              rcmax = max( rcmax, c( j ) )
           end do
           if( rcmin==zero ) then
              ! find the first zero scale factor and return an error code.
              do j = 1, n
                 if( c( j )==zero ) then
                    info = m + j
                    return
                 end if
              end do
           else
              ! invert the scale factors.
              do j = 1, n
                 c( j ) = one / min( max( c( j ), smlnum ), bignum )
              end do
              ! compute colcnd = min(c(j)) / max(c(j)).
              colcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
           end if
           return
     end subroutine stdlib_dgeequb


     pure module subroutine stdlib_cgeequb( m, n, a, lda, r, c, rowcnd, colcnd, amax,info )
     !! CGEEQUB computes row and column scalings intended to equilibrate an
     !! M-by-N matrix A and reduce its condition number.  R returns the row
     !! scale factors and C the column scale factors, chosen to try to make
     !! the largest element in each row and column of the matrix B with
     !! elements B(i,j)=R(i)*A(i,j)*C(j) have an absolute value of at most
     !! the radix.
     !! R(i) and C(j) are restricted to be a power of the radix between
     !! SMLNUM = smallest safe number and BIGNUM = largest safe number.  Use
     !! of these scaling factors is not guaranteed to reduce the condition
     !! number of A but works well in practice.
     !! This routine differs from CGEEQU by restricting the scaling factors
     !! to a power of the radix.  Barring over- and underflow, scaling by
     !! these factors introduces no additional rounding errors.  However, the
     !! scaled entries' magnitudes are no longer approximately 1 but lie
     !! between sqrt(radix) and 1/sqrt(radix).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           real(sp), intent(out) :: amax, colcnd, rowcnd
           ! Array Arguments 
           real(sp), intent(out) :: c(*), r(*)
           complex(sp), intent(in) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j
           real(sp) :: bignum, rcmax, rcmin, smlnum, radix, logrdx
           complex(sp) :: zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGEEQUB', -info )
              return
           end if
           ! quick return if possible.
           if( m==0_ilp .or. n==0_ilp ) then
              rowcnd = one
              colcnd = one
              amax = zero
              return
           end if
           ! get machine constants.  assume smlnum is a power of the radix.
           smlnum = stdlib_slamch( 'S' )
           bignum = one / smlnum
           radix = stdlib_slamch( 'B' )
           logrdx = log( radix )
           ! compute row scale factors.
           do i = 1, m
              r( i ) = zero
           end do
           ! find the maximum element in each row.
           do j = 1, n
              do i = 1, m
                 r( i ) = max( r( i ), cabs1( a( i, j ) ) )
              end do
           end do
           do i = 1, m
              if( r( i )>zero ) then
                 r( i ) = radix**int( log(r( i ) ) / logrdx,KIND=ilp)
              end if
           end do
           ! find the maximum and minimum scale factors.
           rcmin = bignum
           rcmax = zero
           do i = 1, m
              rcmax = max( rcmax, r( i ) )
              rcmin = min( rcmin, r( i ) )
           end do
           amax = rcmax
           if( rcmin==zero ) then
              ! find the first zero scale factor and return an error code.
              do i = 1, m
                 if( r( i )==zero ) then
                    info = i
                    return
                 end if
              end do
           else
              ! invert the scale factors.
              do i = 1, m
                 r( i ) = one / min( max( r( i ), smlnum ), bignum )
              end do
              ! compute rowcnd = min(r(i)) / max(r(i)).
              rowcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
           end if
           ! compute column scale factors.
           do j = 1, n
              c( j ) = zero
           end do
           ! find the maximum element in each column,
           ! assuming the row scaling computed above.
           do j = 1, n
              do i = 1, m
                 c( j ) = max( c( j ), cabs1( a( i, j ) )*r( i ) )
              end do
              if( c( j )>zero ) then
                 c( j ) = radix**int( log( c( j ) ) / logrdx,KIND=ilp)
              end if
           end do
           ! find the maximum and minimum scale factors.
           rcmin = bignum
           rcmax = zero
           do j = 1, n
              rcmin = min( rcmin, c( j ) )
              rcmax = max( rcmax, c( j ) )
           end do
           if( rcmin==zero ) then
              ! find the first zero scale factor and return an error code.
              do j = 1, n
                 if( c( j )==zero ) then
                    info = m + j
                    return
                 end if
              end do
           else
              ! invert the scale factors.
              do j = 1, n
                 c( j ) = one / min( max( c( j ), smlnum ), bignum )
              end do
              ! compute colcnd = min(c(j)) / max(c(j)).
              colcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
           end if
           return
     end subroutine stdlib_cgeequb

     pure module subroutine stdlib_zgeequb( m, n, a, lda, r, c, rowcnd, colcnd, amax,info )
     !! ZGEEQUB computes row and column scalings intended to equilibrate an
     !! M-by-N matrix A and reduce its condition number.  R returns the row
     !! scale factors and C the column scale factors, chosen to try to make
     !! the largest element in each row and column of the matrix B with
     !! elements B(i,j)=R(i)*A(i,j)*C(j) have an absolute value of at most
     !! the radix.
     !! R(i) and C(j) are restricted to be a power of the radix between
     !! SMLNUM = smallest safe number and BIGNUM = largest safe number.  Use
     !! of these scaling factors is not guaranteed to reduce the condition
     !! number of A but works well in practice.
     !! This routine differs from ZGEEQU by restricting the scaling factors
     !! to a power of the radix.  Barring over- and underflow, scaling by
     !! these factors introduces no additional rounding errors.  However, the
     !! scaled entries' magnitudes are no longer approximately 1 but lie
     !! between sqrt(radix) and 1/sqrt(radix).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           real(dp), intent(out) :: amax, colcnd, rowcnd
           ! Array Arguments 
           real(dp), intent(out) :: c(*), r(*)
           complex(dp), intent(in) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j
           real(dp) :: bignum, rcmax, rcmin, smlnum, radix, logrdx
           complex(dp) :: zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGEEQUB', -info )
              return
           end if
           ! quick return if possible.
           if( m==0_ilp .or. n==0_ilp ) then
              rowcnd = one
              colcnd = one
              amax = zero
              return
           end if
           ! get machine constants.  assume smlnum is a power of the radix.
           smlnum = stdlib_dlamch( 'S' )
           bignum = one / smlnum
           radix = stdlib_dlamch( 'B' )
           logrdx = log( radix )
           ! compute row scale factors.
           do i = 1, m
              r( i ) = zero
           end do
           ! find the maximum element in each row.
           do j = 1, n
              do i = 1, m
                 r( i ) = max( r( i ), cabs1( a( i, j ) ) )
              end do
           end do
           do i = 1, m
              if( r( i )>zero ) then
                 r( i ) = radix**int( log(r( i ) ) / logrdx,KIND=ilp)
              end if
           end do
           ! find the maximum and minimum scale factors.
           rcmin = bignum
           rcmax = zero
           do i = 1, m
              rcmax = max( rcmax, r( i ) )
              rcmin = min( rcmin, r( i ) )
           end do
           amax = rcmax
           if( rcmin==zero ) then
              ! find the first zero scale factor and return an error code.
              do i = 1, m
                 if( r( i )==zero ) then
                    info = i
                    return
                 end if
              end do
           else
              ! invert the scale factors.
              do i = 1, m
                 r( i ) = one / min( max( r( i ), smlnum ), bignum )
              end do
              ! compute rowcnd = min(r(i)) / max(r(i)).
              rowcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
           end if
           ! compute column scale factors.
           do j = 1, n
              c( j ) = zero
           end do
           ! find the maximum element in each column,
           ! assuming the row scaling computed above.
           do j = 1, n
              do i = 1, m
                 c( j ) = max( c( j ), cabs1( a( i, j ) )*r( i ) )
              end do
              if( c( j )>zero ) then
                 c( j ) = radix**int( log( c( j ) ) / logrdx,KIND=ilp)
              end if
           end do
           ! find the maximum and minimum scale factors.
           rcmin = bignum
           rcmax = zero
           do j = 1, n
              rcmin = min( rcmin, c( j ) )
              rcmax = max( rcmax, c( j ) )
           end do
           if( rcmin==zero ) then
              ! find the first zero scale factor and return an error code.
              do j = 1, n
                 if( c( j )==zero ) then
                    info = m + j
                    return
                 end if
              end do
           else
              ! invert the scale factors.
              do j = 1, n
                 c( j ) = one / min( max( c( j ), smlnum ), bignum )
              end do
              ! compute colcnd = min(c(j)) / max(c(j)).
              colcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
           end if
           return
     end subroutine stdlib_zgeequb




     pure module subroutine stdlib_slaqge( m, n, a, lda, r, c, rowcnd, colcnd, amax,equed )
     !! SLAQGE equilibrates a general M by N matrix A using the row and
     !! column scaling factors in the vectors R and C.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(out) :: equed
           integer(ilp), intent(in) :: lda, m, n
           real(sp), intent(in) :: amax, colcnd, rowcnd
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: c(*), r(*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: thresh = 0.1e+0_sp
           
           ! Local Scalars 
           integer(ilp) :: i, j
           real(sp) :: cj, large, small
           ! Executable Statements 
           ! quick return if possible
           if( m<=0_ilp .or. n<=0_ilp ) then
              equed = 'N'
              return
           end if
           ! initialize large and small.
           small = stdlib_slamch( 'SAFE MINIMUM' ) / stdlib_slamch( 'PRECISION' )
           large = one / small
           if( rowcnd>=thresh .and. amax>=small .and. amax<=large )then
              ! no row scaling
              if( colcnd>=thresh ) then
                 ! no column scaling
                 equed = 'N'
              else
                 ! column scaling
                 do j = 1, n
                    cj = c( j )
                    do i = 1, m
                       a( i, j ) = cj*a( i, j )
                    end do
                 end do
                 equed = 'C'
              end if
           else if( colcnd>=thresh ) then
              ! row scaling, no column scaling
              do j = 1, n
                 do i = 1, m
                    a( i, j ) = r( i )*a( i, j )
                 end do
              end do
              equed = 'R'
           else
              ! row and column scaling
              do j = 1, n
                 cj = c( j )
                 do i = 1, m
                    a( i, j ) = cj*r( i )*a( i, j )
                 end do
              end do
              equed = 'B'
           end if
           return
     end subroutine stdlib_slaqge

     pure module subroutine stdlib_dlaqge( m, n, a, lda, r, c, rowcnd, colcnd, amax,equed )
     !! DLAQGE equilibrates a general M by N matrix A using the row and
     !! column scaling factors in the vectors R and C.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(out) :: equed
           integer(ilp), intent(in) :: lda, m, n
           real(dp), intent(in) :: amax, colcnd, rowcnd
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: c(*), r(*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: thresh = 0.1e+0_dp
           
           ! Local Scalars 
           integer(ilp) :: i, j
           real(dp) :: cj, large, small
           ! Executable Statements 
           ! quick return if possible
           if( m<=0_ilp .or. n<=0_ilp ) then
              equed = 'N'
              return
           end if
           ! initialize large and small.
           small = stdlib_dlamch( 'SAFE MINIMUM' ) / stdlib_dlamch( 'PRECISION' )
           large = one / small
           if( rowcnd>=thresh .and. amax>=small .and. amax<=large )then
              ! no row scaling
              if( colcnd>=thresh ) then
                 ! no column scaling
                 equed = 'N'
              else
                 ! column scaling
                 do j = 1, n
                    cj = c( j )
                    do i = 1, m
                       a( i, j ) = cj*a( i, j )
                    end do
                 end do
                 equed = 'C'
              end if
           else if( colcnd>=thresh ) then
              ! row scaling, no column scaling
              do j = 1, n
                 do i = 1, m
                    a( i, j ) = r( i )*a( i, j )
                 end do
              end do
              equed = 'R'
           else
              ! row and column scaling
              do j = 1, n
                 cj = c( j )
                 do i = 1, m
                    a( i, j ) = cj*r( i )*a( i, j )
                 end do
              end do
              equed = 'B'
           end if
           return
     end subroutine stdlib_dlaqge


     pure module subroutine stdlib_claqge( m, n, a, lda, r, c, rowcnd, colcnd, amax,equed )
     !! CLAQGE equilibrates a general M by N matrix A using the row and
     !! column scaling factors in the vectors R and C.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(out) :: equed
           integer(ilp), intent(in) :: lda, m, n
           real(sp), intent(in) :: amax, colcnd, rowcnd
           ! Array Arguments 
           real(sp), intent(in) :: c(*), r(*)
           complex(sp), intent(inout) :: a(lda,*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: thresh = 0.1e+0_sp
           
           ! Local Scalars 
           integer(ilp) :: i, j
           real(sp) :: cj, large, small
           ! Executable Statements 
           ! quick return if possible
           if( m<=0_ilp .or. n<=0_ilp ) then
              equed = 'N'
              return
           end if
           ! initialize large and small.
           small = stdlib_slamch( 'SAFE MINIMUM' ) / stdlib_slamch( 'PRECISION' )
           large = one / small
           if( rowcnd>=thresh .and. amax>=small .and. amax<=large )then
              ! no row scaling
              if( colcnd>=thresh ) then
                 ! no column scaling
                 equed = 'N'
              else
                 ! column scaling
                 do j = 1, n
                    cj = c( j )
                    do i = 1, m
                       a( i, j ) = cj*a( i, j )
                    end do
                 end do
                 equed = 'C'
              end if
           else if( colcnd>=thresh ) then
              ! row scaling, no column scaling
              do j = 1, n
                 do i = 1, m
                    a( i, j ) = r( i )*a( i, j )
                 end do
              end do
              equed = 'R'
           else
              ! row and column scaling
              do j = 1, n
                 cj = c( j )
                 do i = 1, m
                    a( i, j ) = cj*r( i )*a( i, j )
                 end do
              end do
              equed = 'B'
           end if
           return
     end subroutine stdlib_claqge

     pure module subroutine stdlib_zlaqge( m, n, a, lda, r, c, rowcnd, colcnd, amax,equed )
     !! ZLAQGE equilibrates a general M by N matrix A using the row and
     !! column scaling factors in the vectors R and C.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(out) :: equed
           integer(ilp), intent(in) :: lda, m, n
           real(dp), intent(in) :: amax, colcnd, rowcnd
           ! Array Arguments 
           real(dp), intent(in) :: c(*), r(*)
           complex(dp), intent(inout) :: a(lda,*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: thresh = 0.1e+0_dp
           
           ! Local Scalars 
           integer(ilp) :: i, j
           real(dp) :: cj, large, small
           ! Executable Statements 
           ! quick return if possible
           if( m<=0_ilp .or. n<=0_ilp ) then
              equed = 'N'
              return
           end if
           ! initialize large and small.
           small = stdlib_dlamch( 'SAFE MINIMUM' ) / stdlib_dlamch( 'PRECISION' )
           large = one / small
           if( rowcnd>=thresh .and. amax>=small .and. amax<=large )then
              ! no row scaling
              if( colcnd>=thresh ) then
                 ! no column scaling
                 equed = 'N'
              else
                 ! column scaling
                 do j = 1, n
                    cj = c( j )
                    do i = 1, m
                       a( i, j ) = cj*a( i, j )
                    end do
                 end do
                 equed = 'C'
              end if
           else if( colcnd>=thresh ) then
              ! row scaling, no column scaling
              do j = 1, n
                 do i = 1, m
                    a( i, j ) = r( i )*a( i, j )
                 end do
              end do
              equed = 'R'
           else
              ! row and column scaling
              do j = 1, n
                 cj = c( j )
                 do i = 1, m
                    a( i, j ) = cj*r( i )*a( i, j )
                 end do
              end do
              equed = 'B'
           end if
           return
     end subroutine stdlib_zlaqge




     pure module subroutine stdlib_slaswp( n, a, lda, k1, k2, ipiv, incx )
     !! SLASWP performs a series of row interchanges on the matrix A.
     !! One row interchange is initiated for each of rows K1 through K2 of A.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, k1, k2, lda, n
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(sp), intent(inout) :: a(lda,*)
       ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, i1, i2, inc, ip, ix, ix0, j, k, n32
           real(sp) :: temp
           ! Executable Statements 
           ! interchange row i with row ipiv(k1+(i-k1)*abs(incx)) for each of rows
           ! k1 through k2.
           if( incx>0_ilp ) then
              ix0 = k1
              i1 = k1
              i2 = k2
              inc = 1_ilp
           else if( incx<0_ilp ) then
              ix0 = k1 + ( k1-k2 )*incx
              i1 = k2
              i2 = k1
              inc = -1_ilp
           else
              return
           end if
           n32 = ( n / 32_ilp )*32_ilp
           if( n32/=0_ilp ) then
              do j = 1, n32, 32
                 ix = ix0
                 do i = i1, i2, inc
                    ip = ipiv( ix )
                    if( ip/=i ) then
                       do k = j, j + 31
                          temp = a( i, k )
                          a( i, k ) = a( ip, k )
                          a( ip, k ) = temp
                       end do
                    end if
                    ix = ix + incx
                 end do
              end do
           end if
           if( n32/=n ) then
              n32 = n32 + 1_ilp
              ix = ix0
              do i = i1, i2, inc
                 ip = ipiv( ix )
                 if( ip/=i ) then
                    do k = n32, n
                       temp = a( i, k )
                       a( i, k ) = a( ip, k )
                       a( ip, k ) = temp
                    end do
                 end if
                 ix = ix + incx
              end do
           end if
           return
     end subroutine stdlib_slaswp

     pure module subroutine stdlib_dlaswp( n, a, lda, k1, k2, ipiv, incx )
     !! DLASWP performs a series of row interchanges on the matrix A.
     !! One row interchange is initiated for each of rows K1 through K2 of A.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, k1, k2, lda, n
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(dp), intent(inout) :: a(lda,*)
       ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, i1, i2, inc, ip, ix, ix0, j, k, n32
           real(dp) :: temp
           ! Executable Statements 
           ! interchange row i with row ipiv(k1+(i-k1)*abs(incx)) for each of rows
           ! k1 through k2.
           if( incx>0_ilp ) then
              ix0 = k1
              i1 = k1
              i2 = k2
              inc = 1_ilp
           else if( incx<0_ilp ) then
              ix0 = k1 + ( k1-k2 )*incx
              i1 = k2
              i2 = k1
              inc = -1_ilp
           else
              return
           end if
           n32 = ( n / 32_ilp )*32_ilp
           if( n32/=0_ilp ) then
              do j = 1, n32, 32
                 ix = ix0
                 do i = i1, i2, inc
                    ip = ipiv( ix )
                    if( ip/=i ) then
                       do k = j, j + 31
                          temp = a( i, k )
                          a( i, k ) = a( ip, k )
                          a( ip, k ) = temp
                       end do
                    end if
                    ix = ix + incx
                 end do
              end do
           end if
           if( n32/=n ) then
              n32 = n32 + 1_ilp
              ix = ix0
              do i = i1, i2, inc
                 ip = ipiv( ix )
                 if( ip/=i ) then
                    do k = n32, n
                       temp = a( i, k )
                       a( i, k ) = a( ip, k )
                       a( ip, k ) = temp
                    end do
                 end if
                 ix = ix + incx
              end do
           end if
           return
     end subroutine stdlib_dlaswp


     pure module subroutine stdlib_claswp( n, a, lda, k1, k2, ipiv, incx )
     !! CLASWP performs a series of row interchanges on the matrix A.
     !! One row interchange is initiated for each of rows K1 through K2 of A.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, k1, k2, lda, n
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(sp), intent(inout) :: a(lda,*)
       ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, i1, i2, inc, ip, ix, ix0, j, k, n32
           complex(sp) :: temp
           ! Executable Statements 
           ! interchange row i with row ipiv(k1+(i-k1)*abs(incx)) for each of rows
           ! k1 through k2.
           if( incx>0_ilp ) then
              ix0 = k1
              i1 = k1
              i2 = k2
              inc = 1_ilp
           else if( incx<0_ilp ) then
              ix0 = k1 + ( k1-k2 )*incx
              i1 = k2
              i2 = k1
              inc = -1_ilp
           else
              return
           end if
           n32 = ( n / 32_ilp )*32_ilp
           if( n32/=0_ilp ) then
              do j = 1, n32, 32
                 ix = ix0
                 do i = i1, i2, inc
                    ip = ipiv( ix )
                    if( ip/=i ) then
                       do k = j, j + 31
                          temp = a( i, k )
                          a( i, k ) = a( ip, k )
                          a( ip, k ) = temp
                       end do
                    end if
                    ix = ix + incx
                 end do
              end do
           end if
           if( n32/=n ) then
              n32 = n32 + 1_ilp
              ix = ix0
              do i = i1, i2, inc
                 ip = ipiv( ix )
                 if( ip/=i ) then
                    do k = n32, n
                       temp = a( i, k )
                       a( i, k ) = a( ip, k )
                       a( ip, k ) = temp
                    end do
                 end if
                 ix = ix + incx
              end do
           end if
           return
     end subroutine stdlib_claswp

     pure module subroutine stdlib_zlaswp( n, a, lda, k1, k2, ipiv, incx )
     !! ZLASWP performs a series of row interchanges on the matrix A.
     !! One row interchange is initiated for each of rows K1 through K2 of A.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, k1, k2, lda, n
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(dp), intent(inout) :: a(lda,*)
       ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, i1, i2, inc, ip, ix, ix0, j, k, n32
           complex(dp) :: temp
           ! Executable Statements 
           ! interchange row i with row ipiv(k1+(i-k1)*abs(incx)) for each of rows
           ! k1 through k2.
           if( incx>0_ilp ) then
              ix0 = k1
              i1 = k1
              i2 = k2
              inc = 1_ilp
           else if( incx<0_ilp ) then
              ix0 = k1 + ( k1-k2 )*incx
              i1 = k2
              i2 = k1
              inc = -1_ilp
           else
              return
           end if
           n32 = ( n / 32_ilp )*32_ilp
           if( n32/=0_ilp ) then
              do j = 1, n32, 32
                 ix = ix0
                 do i = i1, i2, inc
                    ip = ipiv( ix )
                    if( ip/=i ) then
                       do k = j, j + 31
                          temp = a( i, k )
                          a( i, k ) = a( ip, k )
                          a( ip, k ) = temp
                       end do
                    end if
                    ix = ix + incx
                 end do
              end do
           end if
           if( n32/=n ) then
              n32 = n32 + 1_ilp
              ix = ix0
              do i = i1, i2, inc
                 ip = ipiv( ix )
                 if( ip/=i ) then
                    do k = n32, n
                       temp = a( i, k )
                       a( i, k ) = a( ip, k )
                       a( ip, k ) = temp
                    end do
                 end if
                 ix = ix + incx
              end do
           end if
           return
     end subroutine stdlib_zlaswp




     pure module subroutine stdlib_sgetc2( n, a, lda, ipiv, jpiv, info )
     !! SGETC2 computes an LU factorization with complete pivoting of the
     !! n-by-n matrix A. The factorization has the form A = P * L * U * Q,
     !! where P and Q are permutation matrices, L is lower triangular with
     !! unit diagonal elements and U is upper triangular.
     !! This is the Level 2 BLAS algorithm.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*), jpiv(*)
           real(sp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, ip, ipv, j, jp, jpv
           real(sp) :: bignum, eps, smin, smlnum, xmax
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           ! quick return if possible
           if( n==0 )return
           ! set constants to control overflow
           eps = stdlib_slamch( 'P' )
           smlnum = stdlib_slamch( 'S' ) / eps
           bignum = one / smlnum
           call stdlib_slabad( smlnum, bignum )
           ! handle the case n=1 by itself
           if( n==1_ilp ) then
              ipiv( 1_ilp ) = 1_ilp
              jpiv( 1_ilp ) = 1_ilp
              if( abs( a( 1_ilp, 1_ilp ) )<smlnum ) then
                 info = 1_ilp
                 a( 1_ilp, 1_ilp ) = smlnum
              end if
              return
           end if
           ! factorize a using complete pivoting.
           ! set pivots less than smin to smin.
           loop_40: do i = 1, n - 1
              ! find max element in matrix a
              xmax = zero
              do ip = i, n
                 do jp = i, n
                    if( abs( a( ip, jp ) )>=xmax ) then
                       xmax = abs( a( ip, jp ) )
                       ipv = ip
                       jpv = jp
                    end if
                 end do
              end do
              if( i==1_ilp )smin = max( eps*xmax, smlnum )
              ! swap rows
              if( ipv/=i )call stdlib_sswap( n, a( ipv, 1_ilp ), lda, a( i, 1_ilp ), lda )
              ipiv( i ) = ipv
              ! swap columns
              if( jpv/=i )call stdlib_sswap( n, a( 1_ilp, jpv ), 1_ilp, a( 1_ilp, i ), 1_ilp )
              jpiv( i ) = jpv
              ! check for singularity
              if( abs( a( i, i ) )<smin ) then
                 info = i
                 a( i, i ) = smin
              end if
              do j = i + 1, n
                 a( j, i ) = a( j, i ) / a( i, i )
              end do
              call stdlib_sger( n-i, n-i, -one, a( i+1, i ), 1_ilp, a( i, i+1 ), lda,a( i+1, i+1 ), &
                        lda )
           end do loop_40
           if( abs( a( n, n ) )<smin ) then
              info = n
              a( n, n ) = smin
           end if
           ! set last pivots to n
           ipiv( n ) = n
           jpiv( n ) = n
           return
     end subroutine stdlib_sgetc2

     pure module subroutine stdlib_dgetc2( n, a, lda, ipiv, jpiv, info )
     !! DGETC2 computes an LU factorization with complete pivoting of the
     !! n-by-n matrix A. The factorization has the form A = P * L * U * Q,
     !! where P and Q are permutation matrices, L is lower triangular with
     !! unit diagonal elements and U is upper triangular.
     !! This is the Level 2 BLAS algorithm.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*), jpiv(*)
           real(dp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, ip, ipv, j, jp, jpv
           real(dp) :: bignum, eps, smin, smlnum, xmax
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           ! quick return if possible
           if( n==0 )return
           ! set constants to control overflow
           eps = stdlib_dlamch( 'P' )
           smlnum = stdlib_dlamch( 'S' ) / eps
           bignum = one / smlnum
           call stdlib_dlabad( smlnum, bignum )
           ! handle the case n=1 by itself
           if( n==1_ilp ) then
              ipiv( 1_ilp ) = 1_ilp
              jpiv( 1_ilp ) = 1_ilp
              if( abs( a( 1_ilp, 1_ilp ) )<smlnum ) then
                 info = 1_ilp
                 a( 1_ilp, 1_ilp ) = smlnum
              end if
              return
           end if
           ! factorize a using complete pivoting.
           ! set pivots less than smin to smin.
           loop_40: do i = 1, n - 1
              ! find max element in matrix a
              xmax = zero
              do ip = i, n
                 do jp = i, n
                    if( abs( a( ip, jp ) )>=xmax ) then
                       xmax = abs( a( ip, jp ) )
                       ipv = ip
                       jpv = jp
                    end if
                 end do
              end do
              if( i==1_ilp )smin = max( eps*xmax, smlnum )
              ! swap rows
              if( ipv/=i )call stdlib_dswap( n, a( ipv, 1_ilp ), lda, a( i, 1_ilp ), lda )
              ipiv( i ) = ipv
              ! swap columns
              if( jpv/=i )call stdlib_dswap( n, a( 1_ilp, jpv ), 1_ilp, a( 1_ilp, i ), 1_ilp )
              jpiv( i ) = jpv
              ! check for singularity
              if( abs( a( i, i ) )<smin ) then
                 info = i
                 a( i, i ) = smin
              end if
              do j = i + 1, n
                 a( j, i ) = a( j, i ) / a( i, i )
              end do
              call stdlib_dger( n-i, n-i, -one, a( i+1, i ), 1_ilp, a( i, i+1 ), lda,a( i+1, i+1 ), &
                        lda )
           end do loop_40
           if( abs( a( n, n ) )<smin ) then
              info = n
              a( n, n ) = smin
           end if
           ! set last pivots to n
           ipiv( n ) = n
           jpiv( n ) = n
           return
     end subroutine stdlib_dgetc2


     pure module subroutine stdlib_cgetc2( n, a, lda, ipiv, jpiv, info )
     !! CGETC2 computes an LU factorization, using complete pivoting, of the
     !! n-by-n matrix A. The factorization has the form A = P * L * U * Q,
     !! where P and Q are permutation matrices, L is lower triangular with
     !! unit diagonal elements and U is upper triangular.
     !! This is a level 1 BLAS version of the algorithm.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*), jpiv(*)
           complex(sp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, ip, ipv, j, jp, jpv
           real(sp) :: bignum, eps, smin, smlnum, xmax
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           ! quick return if possible
           if( n==0 )return
           ! set constants to control overflow
           eps = stdlib_slamch( 'P' )
           smlnum = stdlib_slamch( 'S' ) / eps
           bignum = one / smlnum
           call stdlib_slabad( smlnum, bignum )
           ! handle the case n=1 by itself
           if( n==1_ilp ) then
              ipiv( 1_ilp ) = 1_ilp
              jpiv( 1_ilp ) = 1_ilp
              if( abs( a( 1_ilp, 1_ilp ) )<smlnum ) then
                 info = 1_ilp
                 a( 1_ilp, 1_ilp ) = cmplx( smlnum, zero,KIND=sp)
              end if
              return
           end if
           ! factorize a using complete pivoting.
           ! set pivots less than smin to smin
           loop_40: do i = 1, n - 1
              ! find max element in matrix a
              xmax = zero
              do ip = i, n
                 do jp = i, n
                    if( abs( a( ip, jp ) )>=xmax ) then
                       xmax = abs( a( ip, jp ) )
                       ipv = ip
                       jpv = jp
                    end if
                 end do
              end do
              if( i==1_ilp )smin = max( eps*xmax, smlnum )
              ! swap rows
              if( ipv/=i )call stdlib_cswap( n, a( ipv, 1_ilp ), lda, a( i, 1_ilp ), lda )
              ipiv( i ) = ipv
              ! swap columns
              if( jpv/=i )call stdlib_cswap( n, a( 1_ilp, jpv ), 1_ilp, a( 1_ilp, i ), 1_ilp )
              jpiv( i ) = jpv
              ! check for singularity
              if( abs( a( i, i ) )<smin ) then
                 info = i
                 a( i, i ) = cmplx( smin, zero,KIND=sp)
              end if
              do j = i + 1, n
                 a( j, i ) = a( j, i ) / a( i, i )
              end do
              call stdlib_cgeru( n-i, n-i, -cmplx( one,KIND=sp), a( i+1, i ), 1_ilp,a( i, i+1 ), lda, &
                        a( i+1, i+1 ), lda )
           end do loop_40
           if( abs( a( n, n ) )<smin ) then
              info = n
              a( n, n ) = cmplx( smin, zero,KIND=sp)
           end if
           ! set last pivots to n
           ipiv( n ) = n
           jpiv( n ) = n
           return
     end subroutine stdlib_cgetc2

     pure module subroutine stdlib_zgetc2( n, a, lda, ipiv, jpiv, info )
     !! ZGETC2 computes an LU factorization, using complete pivoting, of the
     !! n-by-n matrix A. The factorization has the form A = P * L * U * Q,
     !! where P and Q are permutation matrices, L is lower triangular with
     !! unit diagonal elements and U is upper triangular.
     !! This is a level 1 BLAS version of the algorithm.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*), jpiv(*)
           complex(dp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, ip, ipv, j, jp, jpv
           real(dp) :: bignum, eps, smin, smlnum, xmax
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           ! quick return if possible
           if( n==0 )return
           ! set constants to control overflow
           eps = stdlib_dlamch( 'P' )
           smlnum = stdlib_dlamch( 'S' ) / eps
           bignum = one / smlnum
           call stdlib_dlabad( smlnum, bignum )
           ! handle the case n=1 by itself
           if( n==1_ilp ) then
              ipiv( 1_ilp ) = 1_ilp
              jpiv( 1_ilp ) = 1_ilp
              if( abs( a( 1_ilp, 1_ilp ) )<smlnum ) then
                 info = 1_ilp
                 a( 1_ilp, 1_ilp ) = cmplx( smlnum, zero,KIND=dp)
              end if
              return
           end if
           ! factorize a using complete pivoting.
           ! set pivots less than smin to smin
           loop_40: do i = 1, n - 1
              ! find max element in matrix a
              xmax = zero
              do ip = i, n
                 do jp = i, n
                    if( abs( a( ip, jp ) )>=xmax ) then
                       xmax = abs( a( ip, jp ) )
                       ipv = ip
                       jpv = jp
                    end if
                 end do
              end do
              if( i==1_ilp )smin = max( eps*xmax, smlnum )
              ! swap rows
              if( ipv/=i )call stdlib_zswap( n, a( ipv, 1_ilp ), lda, a( i, 1_ilp ), lda )
              ipiv( i ) = ipv
              ! swap columns
              if( jpv/=i )call stdlib_zswap( n, a( 1_ilp, jpv ), 1_ilp, a( 1_ilp, i ), 1_ilp )
              jpiv( i ) = jpv
              ! check for singularity
              if( abs( a( i, i ) )<smin ) then
                 info = i
                 a( i, i ) = cmplx( smin, zero,KIND=dp)
              end if
              do j = i + 1, n
                 a( j, i ) = a( j, i ) / a( i, i )
              end do
              call stdlib_zgeru( n-i, n-i, -cmplx( one,KIND=dp), a( i+1, i ), 1_ilp,a( i, i+1 ), lda, &
                        a( i+1, i+1 ), lda )
           end do loop_40
           if( abs( a( n, n ) )<smin ) then
              info = n
              a( n, n ) = cmplx( smin, zero,KIND=dp)
           end if
           ! set last pivots to n
           ipiv( n ) = n
           jpiv( n ) = n
           return
     end subroutine stdlib_zgetc2




     pure module subroutine stdlib_sgesc2( n, a, lda, rhs, ipiv, jpiv, scale )
     !! SGESC2 solves a system of linear equations
     !! A * X = scale* RHS
     !! with a general N-by-N matrix A using the LU factorization with
     !! complete pivoting computed by SGETC2.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: lda, n
           real(sp), intent(out) :: scale
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*), jpiv(*)
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(inout) :: rhs(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j
           real(sp) :: bignum, eps, smlnum, temp
           ! Intrinsic Functions 
           ! Executable Statements 
            ! set constant to control overflow
           eps = stdlib_slamch( 'P' )
           smlnum = stdlib_slamch( 'S' ) / eps
           bignum = one / smlnum
           call stdlib_slabad( smlnum, bignum )
           ! apply permutations ipiv to rhs
           call stdlib_slaswp( 1_ilp, rhs, lda, 1_ilp, n-1, ipiv, 1_ilp )
           ! solve for l part
           do i = 1, n - 1
              do j = i + 1, n
                 rhs( j ) = rhs( j ) - a( j, i )*rhs( i )
              end do
           end do
           ! solve for u part
           scale = one
           ! check for scaling
           i = stdlib_isamax( n, rhs, 1_ilp )
           if( two*smlnum*abs( rhs( i ) )>abs( a( n, n ) ) ) then
              temp = ( one / two ) / abs( rhs( i ) )
              call stdlib_sscal( n, temp, rhs( 1_ilp ), 1_ilp )
              scale = scale*temp
           end if
           do i = n, 1, -1
              temp = one / a( i, i )
              rhs( i ) = rhs( i )*temp
              do j = i + 1, n
                 rhs( i ) = rhs( i ) - rhs( j )*( a( i, j )*temp )
              end do
           end do
           ! apply permutations jpiv to the solution (rhs)
           call stdlib_slaswp( 1_ilp, rhs, lda, 1_ilp, n-1, jpiv, -1_ilp )
           return
     end subroutine stdlib_sgesc2

     pure module subroutine stdlib_dgesc2( n, a, lda, rhs, ipiv, jpiv, scale )
     !! DGESC2 solves a system of linear equations
     !! A * X = scale* RHS
     !! with a general N-by-N matrix A using the LU factorization with
     !! complete pivoting computed by DGETC2.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: lda, n
           real(dp), intent(out) :: scale
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*), jpiv(*)
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(inout) :: rhs(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j
           real(dp) :: bignum, eps, smlnum, temp
           ! Intrinsic Functions 
           ! Executable Statements 
            ! set constant to control overflow
           eps = stdlib_dlamch( 'P' )
           smlnum = stdlib_dlamch( 'S' ) / eps
           bignum = one / smlnum
           call stdlib_dlabad( smlnum, bignum )
           ! apply permutations ipiv to rhs
           call stdlib_dlaswp( 1_ilp, rhs, lda, 1_ilp, n-1, ipiv, 1_ilp )
           ! solve for l part
           do i = 1, n - 1
              do j = i + 1, n
                 rhs( j ) = rhs( j ) - a( j, i )*rhs( i )
              end do
           end do
           ! solve for u part
           scale = one
           ! check for scaling
           i = stdlib_idamax( n, rhs, 1_ilp )
           if( two*smlnum*abs( rhs( i ) )>abs( a( n, n ) ) ) then
              temp = ( one / two ) / abs( rhs( i ) )
              call stdlib_dscal( n, temp, rhs( 1_ilp ), 1_ilp )
              scale = scale*temp
           end if
           do i = n, 1, -1
              temp = one / a( i, i )
              rhs( i ) = rhs( i )*temp
              do j = i + 1, n
                 rhs( i ) = rhs( i ) - rhs( j )*( a( i, j )*temp )
              end do
           end do
           ! apply permutations jpiv to the solution (rhs)
           call stdlib_dlaswp( 1_ilp, rhs, lda, 1_ilp, n-1, jpiv, -1_ilp )
           return
     end subroutine stdlib_dgesc2


     pure module subroutine stdlib_cgesc2( n, a, lda, rhs, ipiv, jpiv, scale )
     !! CGESC2 solves a system of linear equations
     !! A * X = scale* RHS
     !! with a general N-by-N matrix A using the LU factorization with
     !! complete pivoting computed by CGETC2.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: lda, n
           real(sp), intent(out) :: scale
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*), jpiv(*)
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(inout) :: rhs(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j
           real(sp) :: bignum, eps, smlnum
           complex(sp) :: temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! set constant to control overflow
           eps = stdlib_slamch( 'P' )
           smlnum = stdlib_slamch( 'S' ) / eps
           bignum = one / smlnum
           call stdlib_slabad( smlnum, bignum )
           ! apply permutations ipiv to rhs
           call stdlib_claswp( 1_ilp, rhs, lda, 1_ilp, n-1, ipiv, 1_ilp )
           ! solve for l part
           do i = 1, n - 1
              do j = i + 1, n
                 rhs( j ) = rhs( j ) - a( j, i )*rhs( i )
              end do
           end do
           ! solve for u part
           scale = one
           ! check for scaling
           i = stdlib_icamax( n, rhs, 1_ilp )
           if( two*smlnum*abs( rhs( i ) )>abs( a( n, n ) ) ) then
              temp = cmplx( one / two, zero,KIND=sp) / abs( rhs( i ) )
              call stdlib_cscal( n, temp, rhs( 1_ilp ), 1_ilp )
              scale = scale*real( temp,KIND=sp)
           end if
           do i = n, 1, -1
              temp = cmplx( one, zero,KIND=sp) / a( i, i )
              rhs( i ) = rhs( i )*temp
              do j = i + 1, n
                 rhs( i ) = rhs( i ) - rhs( j )*( a( i, j )*temp )
              end do
           end do
           ! apply permutations jpiv to the solution (rhs)
           call stdlib_claswp( 1_ilp, rhs, lda, 1_ilp, n-1, jpiv, -1_ilp )
           return
     end subroutine stdlib_cgesc2

     pure module subroutine stdlib_zgesc2( n, a, lda, rhs, ipiv, jpiv, scale )
     !! ZGESC2 solves a system of linear equations
     !! A * X = scale* RHS
     !! with a general N-by-N matrix A using the LU factorization with
     !! complete pivoting computed by ZGETC2.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: lda, n
           real(dp), intent(out) :: scale
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*), jpiv(*)
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(inout) :: rhs(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j
           real(dp) :: bignum, eps, smlnum
           complex(dp) :: temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! set constant to control overflow
           eps = stdlib_dlamch( 'P' )
           smlnum = stdlib_dlamch( 'S' ) / eps
           bignum = one / smlnum
           call stdlib_dlabad( smlnum, bignum )
           ! apply permutations ipiv to rhs
           call stdlib_zlaswp( 1_ilp, rhs, lda, 1_ilp, n-1, ipiv, 1_ilp )
           ! solve for l part
           do i = 1, n - 1
              do j = i + 1, n
                 rhs( j ) = rhs( j ) - a( j, i )*rhs( i )
              end do
           end do
           ! solve for u part
           scale = one
           ! check for scaling
           i = stdlib_izamax( n, rhs, 1_ilp )
           if( two*smlnum*abs( rhs( i ) )>abs( a( n, n ) ) ) then
              temp = cmplx( one / two, zero,KIND=dp) / abs( rhs( i ) )
              call stdlib_zscal( n, temp, rhs( 1_ilp ), 1_ilp )
              scale = scale*real( temp,KIND=dp)
           end if
           do i = n, 1, -1
              temp = cmplx( one, zero,KIND=dp) / a( i, i )
              rhs( i ) = rhs( i )*temp
              do j = i + 1, n
                 rhs( i ) = rhs( i ) - rhs( j )*( a( i, j )*temp )
              end do
           end do
           ! apply permutations jpiv to the solution (rhs)
           call stdlib_zlaswp( 1_ilp, rhs, lda, 1_ilp, n-1, jpiv, -1_ilp )
           return
     end subroutine stdlib_zgesc2




     pure module subroutine stdlib_slatdf( ijob, n, z, ldz, rhs, rdsum, rdscal, ipiv,jpiv )
     !! SLATDF uses the LU factorization of the n-by-n matrix Z computed by
     !! SGETC2 and computes a contribution to the reciprocal Dif-estimate
     !! by solving Z * x = b for x, and choosing the r.h.s. b such that
     !! the norm of x is as large as possible. On entry RHS = b holds the
     !! contribution from earlier solved sub-systems, and on return RHS = x.
     !! The factorization of Z returned by SGETC2 has the form Z = P*L*U*Q,
     !! where P and Q are permutation matrices. L is lower triangular with
     !! unit diagonal elements and U is upper triangular.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ijob, ldz, n
           real(sp), intent(inout) :: rdscal, rdsum
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*), jpiv(*)
           real(sp), intent(inout) :: rhs(*), z(ldz,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: maxdim = 8_ilp
           
           
           ! Local Scalars 
           integer(ilp) :: i, info, j, k
           real(sp) :: bm, bp, pmone, sminu, splus, temp
           ! Local Arrays 
           integer(ilp) :: iwork(maxdim)
           real(sp) :: work(4_ilp*maxdim), xm(maxdim), xp(maxdim)
           ! Intrinsic Functions 
           ! Executable Statements 
           if( ijob/=2_ilp ) then
              ! apply permutations ipiv to rhs
              call stdlib_slaswp( 1_ilp, rhs, ldz, 1_ilp, n-1, ipiv, 1_ilp )
              ! solve for l-part choosing rhs either to +1 or -1.
              pmone = -one
              loop_10: do j = 1, n - 1
                 bp = rhs( j ) + one
                 bm = rhs( j ) - one
                 splus = one
                 ! look-ahead for l-part rhs(1:n-1) = + or -1, splus and
                 ! smin computed more efficiently than in bsolve [1].
                 splus = splus + stdlib_sdot( n-j, z( j+1, j ), 1_ilp, z( j+1, j ), 1_ilp )
                 sminu = stdlib_sdot( n-j, z( j+1, j ), 1_ilp, rhs( j+1 ), 1_ilp )
                 splus = splus*rhs( j )
                 if( splus>sminu ) then
                    rhs( j ) = bp
                 else if( sminu>splus ) then
                    rhs( j ) = bm
                 else
                    ! in this case the updating sums are equal and we can
                    ! choose rhs(j) +1 or -1. the first time this happens
                    ! we choose -1, thereafter +1. this is a simple way to
                    ! get good estimates of matrices like byers well-known
                    ! example (see [1]). (not done in bsolve.)
                    rhs( j ) = rhs( j ) + pmone
                    pmone = one
                 end if
                 ! compute the remaining r.h.s.
                 temp = -rhs( j )
                 call stdlib_saxpy( n-j, temp, z( j+1, j ), 1_ilp, rhs( j+1 ), 1_ilp )
              end do loop_10
              ! solve for u-part, look-ahead for rhs(n) = +-1. this is not done
              ! in bsolve and will hopefully give us a better estimate because
              ! any ill-conditioning of the original matrix is transferred to u
              ! and not to l. u(n, n) is an approximation to sigma_min(lu).
              call stdlib_scopy( n-1, rhs, 1_ilp, xp, 1_ilp )
              xp( n ) = rhs( n ) + one
              rhs( n ) = rhs( n ) - one
              splus = zero
              sminu = zero
              do i = n, 1, -1
                 temp = one / z( i, i )
                 xp( i ) = xp( i )*temp
                 rhs( i ) = rhs( i )*temp
                 do k = i + 1, n
                    xp( i ) = xp( i ) - xp( k )*( z( i, k )*temp )
                    rhs( i ) = rhs( i ) - rhs( k )*( z( i, k )*temp )
                 end do
                 splus = splus + abs( xp( i ) )
                 sminu = sminu + abs( rhs( i ) )
              end do
              if( splus>sminu )call stdlib_scopy( n, xp, 1_ilp, rhs, 1_ilp )
              ! apply the permutations jpiv to the computed solution (rhs)
              call stdlib_slaswp( 1_ilp, rhs, ldz, 1_ilp, n-1, jpiv, -1_ilp )
              ! compute the sum of squares
              call stdlib_slassq( n, rhs, 1_ilp, rdscal, rdsum )
           else
              ! ijob = 2, compute approximate nullvector xm of z
              call stdlib_sgecon( 'I', n, z, ldz, one, temp, work, iwork, info )
              call stdlib_scopy( n, work( n+1 ), 1_ilp, xm, 1_ilp )
              ! compute rhs
              call stdlib_slaswp( 1_ilp, xm, ldz, 1_ilp, n-1, ipiv, -1_ilp )
              temp = one / sqrt( stdlib_sdot( n, xm, 1_ilp, xm, 1_ilp ) )
              call stdlib_sscal( n, temp, xm, 1_ilp )
              call stdlib_scopy( n, xm, 1_ilp, xp, 1_ilp )
              call stdlib_saxpy( n, one, rhs, 1_ilp, xp, 1_ilp )
              call stdlib_saxpy( n, -one, xm, 1_ilp, rhs, 1_ilp )
              call stdlib_sgesc2( n, z, ldz, rhs, ipiv, jpiv, temp )
              call stdlib_sgesc2( n, z, ldz, xp, ipiv, jpiv, temp )
              if( stdlib_sasum( n, xp, 1_ilp )>stdlib_sasum( n, rhs, 1_ilp ) )call stdlib_scopy( n, xp, 1_ilp,&
                         rhs, 1_ilp )
              ! compute the sum of squares
              call stdlib_slassq( n, rhs, 1_ilp, rdscal, rdsum )
           end if
           return
     end subroutine stdlib_slatdf

     pure module subroutine stdlib_dlatdf( ijob, n, z, ldz, rhs, rdsum, rdscal, ipiv,jpiv )
     !! DLATDF uses the LU factorization of the n-by-n matrix Z computed by
     !! DGETC2 and computes a contribution to the reciprocal Dif-estimate
     !! by solving Z * x = b for x, and choosing the r.h.s. b such that
     !! the norm of x is as large as possible. On entry RHS = b holds the
     !! contribution from earlier solved sub-systems, and on return RHS = x.
     !! The factorization of Z returned by DGETC2 has the form Z = P*L*U*Q,
     !! where P and Q are permutation matrices. L is lower triangular with
     !! unit diagonal elements and U is upper triangular.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ijob, ldz, n
           real(dp), intent(inout) :: rdscal, rdsum
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*), jpiv(*)
           real(dp), intent(inout) :: rhs(*), z(ldz,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: maxdim = 8_ilp
           
           
           ! Local Scalars 
           integer(ilp) :: i, info, j, k
           real(dp) :: bm, bp, pmone, sminu, splus, temp
           ! Local Arrays 
           integer(ilp) :: iwork(maxdim)
           real(dp) :: work(4_ilp*maxdim), xm(maxdim), xp(maxdim)
           ! Intrinsic Functions 
           ! Executable Statements 
           if( ijob/=2_ilp ) then
              ! apply permutations ipiv to rhs
              call stdlib_dlaswp( 1_ilp, rhs, ldz, 1_ilp, n-1, ipiv, 1_ilp )
              ! solve for l-part choosing rhs either to +1 or -1.
              pmone = -one
              loop_10: do j = 1, n - 1
                 bp = rhs( j ) + one
                 bm = rhs( j ) - one
                 splus = one
                 ! look-ahead for l-part rhs(1:n-1) = + or -1, splus and
                 ! smin computed more efficiently than in bsolve [1].
                 splus = splus + stdlib_ddot( n-j, z( j+1, j ), 1_ilp, z( j+1, j ), 1_ilp )
                 sminu = stdlib_ddot( n-j, z( j+1, j ), 1_ilp, rhs( j+1 ), 1_ilp )
                 splus = splus*rhs( j )
                 if( splus>sminu ) then
                    rhs( j ) = bp
                 else if( sminu>splus ) then
                    rhs( j ) = bm
                 else
                    ! in this case the updating sums are equal and we can
                    ! choose rhs(j) +1 or -1. the first time this happens
                    ! we choose -1, thereafter +1. this is a simple way to
                    ! get good estimates of matrices like byers well-known
                    ! example (see [1]). (not done in bsolve.)
                    rhs( j ) = rhs( j ) + pmone
                    pmone = one
                 end if
                 ! compute the remaining r.h.s.
                 temp = -rhs( j )
                 call stdlib_daxpy( n-j, temp, z( j+1, j ), 1_ilp, rhs( j+1 ), 1_ilp )
              end do loop_10
              ! solve for u-part, look-ahead for rhs(n) = +-1. this is not done
              ! in bsolve and will hopefully give us a better estimate because
              ! any ill-conditioning of the original matrix is transferred to u
              ! and not to l. u(n, n) is an approximation to sigma_min(lu).
              call stdlib_dcopy( n-1, rhs, 1_ilp, xp, 1_ilp )
              xp( n ) = rhs( n ) + one
              rhs( n ) = rhs( n ) - one
              splus = zero
              sminu = zero
              do i = n, 1, -1
                 temp = one / z( i, i )
                 xp( i ) = xp( i )*temp
                 rhs( i ) = rhs( i )*temp
                 do k = i + 1, n
                    xp( i ) = xp( i ) - xp( k )*( z( i, k )*temp )
                    rhs( i ) = rhs( i ) - rhs( k )*( z( i, k )*temp )
                 end do
                 splus = splus + abs( xp( i ) )
                 sminu = sminu + abs( rhs( i ) )
              end do
              if( splus>sminu )call stdlib_dcopy( n, xp, 1_ilp, rhs, 1_ilp )
              ! apply the permutations jpiv to the computed solution (rhs)
              call stdlib_dlaswp( 1_ilp, rhs, ldz, 1_ilp, n-1, jpiv, -1_ilp )
              ! compute the sum of squares
              call stdlib_dlassq( n, rhs, 1_ilp, rdscal, rdsum )
           else
              ! ijob = 2, compute approximate nullvector xm of z
              call stdlib_dgecon( 'I', n, z, ldz, one, temp, work, iwork, info )
              call stdlib_dcopy( n, work( n+1 ), 1_ilp, xm, 1_ilp )
              ! compute rhs
              call stdlib_dlaswp( 1_ilp, xm, ldz, 1_ilp, n-1, ipiv, -1_ilp )
              temp = one / sqrt( stdlib_ddot( n, xm, 1_ilp, xm, 1_ilp ) )
              call stdlib_dscal( n, temp, xm, 1_ilp )
              call stdlib_dcopy( n, xm, 1_ilp, xp, 1_ilp )
              call stdlib_daxpy( n, one, rhs, 1_ilp, xp, 1_ilp )
              call stdlib_daxpy( n, -one, xm, 1_ilp, rhs, 1_ilp )
              call stdlib_dgesc2( n, z, ldz, rhs, ipiv, jpiv, temp )
              call stdlib_dgesc2( n, z, ldz, xp, ipiv, jpiv, temp )
              if( stdlib_dasum( n, xp, 1_ilp )>stdlib_dasum( n, rhs, 1_ilp ) )call stdlib_dcopy( n, xp, 1_ilp,&
                         rhs, 1_ilp )
              ! compute the sum of squares
              call stdlib_dlassq( n, rhs, 1_ilp, rdscal, rdsum )
           end if
           return
     end subroutine stdlib_dlatdf


     pure module subroutine stdlib_clatdf( ijob, n, z, ldz, rhs, rdsum, rdscal, ipiv,jpiv )
     !! CLATDF computes the contribution to the reciprocal Dif-estimate
     !! by solving for x in Z * x = b, where b is chosen such that the norm
     !! of x is as large as possible. It is assumed that LU decomposition
     !! of Z has been computed by CGETC2. On entry RHS = f holds the
     !! contribution from earlier solved sub-systems, and on return RHS = x.
     !! The factorization of Z returned by CGETC2 has the form
     !! Z = P * L * U * Q, where P and Q are permutation matrices. L is lower
     !! triangular with unit diagonal elements and U is upper triangular.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ijob, ldz, n
           real(sp), intent(inout) :: rdscal, rdsum
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*), jpiv(*)
           complex(sp), intent(inout) :: rhs(*), z(ldz,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: maxdim = 2_ilp
           
           
           
           ! Local Scalars 
           integer(ilp) :: i, info, j, k
           real(sp) :: rtemp, scale, sminu, splus
           complex(sp) :: bm, bp, pmone, temp
           ! Local Arrays 
           real(sp) :: rwork(maxdim)
           complex(sp) :: work(4_ilp*maxdim), xm(maxdim), xp(maxdim)
           ! Intrinsic Functions 
           ! Executable Statements 
           if( ijob/=2_ilp ) then
              ! apply permutations ipiv to rhs
              call stdlib_claswp( 1_ilp, rhs, ldz, 1_ilp, n-1, ipiv, 1_ilp )
              ! solve for l-part choosing rhs either to +1 or -1.
              pmone = -cone
              loop_10: do j = 1, n - 1
                 bp = rhs( j ) + cone
                 bm = rhs( j ) - cone
                 splus = one
                 ! lockahead for l- part rhs(1:n-1) = +-1
                 ! splus and smin computed more efficiently than in bsolve[1].
                 splus = splus + real( stdlib_cdotc( n-j, z( j+1, j ), 1_ilp, z( j+1,j ), 1_ilp ),KIND=sp)
                           
                 sminu = real( stdlib_cdotc( n-j, z( j+1, j ), 1_ilp, rhs( j+1 ), 1_ilp ),KIND=sp)
                 splus = splus*real( rhs( j ),KIND=sp)
                 if( splus>sminu ) then
                    rhs( j ) = bp
                 else if( sminu>splus ) then
                    rhs( j ) = bm
                 else
                    ! in this case the updating sums are equal and we can
                    ! choose rhs(j) +1 or -1. the first time this happens we
                    ! choose -1, thereafter +1. this is a simple way to get
                    ! good estimates of matrices like byers well-known example
                    ! (see [1]). (not done in bsolve.)
                    rhs( j ) = rhs( j ) + pmone
                    pmone = cone
                 end if
                 ! compute the remaining r.h.s.
                 temp = -rhs( j )
                 call stdlib_caxpy( n-j, temp, z( j+1, j ), 1_ilp, rhs( j+1 ), 1_ilp )
              end do loop_10
              ! solve for u- part, lockahead for rhs(n) = +-1. this is not done
              ! in bsolve and will hopefully give us a better estimate because
              ! any ill-conditioning of the original matrix is transferred to u
              ! and not to l. u(n, n) is an approximation to sigma_min(lu).
              call stdlib_ccopy( n-1, rhs, 1_ilp, work, 1_ilp )
              work( n ) = rhs( n ) + cone
              rhs( n ) = rhs( n ) - cone
              splus = zero
              sminu = zero
              do i = n, 1, -1
                 temp = cone / z( i, i )
                 work( i ) = work( i )*temp
                 rhs( i ) = rhs( i )*temp
                 do k = i + 1, n
                    work( i ) = work( i ) - work( k )*( z( i, k )*temp )
                    rhs( i ) = rhs( i ) - rhs( k )*( z( i, k )*temp )
                 end do
                 splus = splus + abs( work( i ) )
                 sminu = sminu + abs( rhs( i ) )
              end do
              if( splus>sminu )call stdlib_ccopy( n, work, 1_ilp, rhs, 1_ilp )
              ! apply the permutations jpiv to the computed solution (rhs)
              call stdlib_claswp( 1_ilp, rhs, ldz, 1_ilp, n-1, jpiv, -1_ilp )
              ! compute the sum of squares
              call stdlib_classq( n, rhs, 1_ilp, rdscal, rdsum )
              return
           end if
           ! entry ijob = 2
           ! compute approximate nullvector xm of z
           call stdlib_cgecon( 'I', n, z, ldz, one, rtemp, work, rwork, info )
           call stdlib_ccopy( n, work( n+1 ), 1_ilp, xm, 1_ilp )
           ! compute rhs
           call stdlib_claswp( 1_ilp, xm, ldz, 1_ilp, n-1, ipiv, -1_ilp )
           temp = cone / sqrt( stdlib_cdotc( n, xm, 1_ilp, xm, 1_ilp ) )
           call stdlib_cscal( n, temp, xm, 1_ilp )
           call stdlib_ccopy( n, xm, 1_ilp, xp, 1_ilp )
           call stdlib_caxpy( n, cone, rhs, 1_ilp, xp, 1_ilp )
           call stdlib_caxpy( n, -cone, xm, 1_ilp, rhs, 1_ilp )
           call stdlib_cgesc2( n, z, ldz, rhs, ipiv, jpiv, scale )
           call stdlib_cgesc2( n, z, ldz, xp, ipiv, jpiv, scale )
           if( stdlib_scasum( n, xp, 1_ilp )>stdlib_scasum( n, rhs, 1_ilp ) )call stdlib_ccopy( n, xp, 1_ilp, &
                     rhs, 1_ilp )
           ! compute the sum of squares
           call stdlib_classq( n, rhs, 1_ilp, rdscal, rdsum )
           return
     end subroutine stdlib_clatdf

     pure module subroutine stdlib_zlatdf( ijob, n, z, ldz, rhs, rdsum, rdscal, ipiv,jpiv )
     !! ZLATDF computes the contribution to the reciprocal Dif-estimate
     !! by solving for x in Z * x = b, where b is chosen such that the norm
     !! of x is as large as possible. It is assumed that LU decomposition
     !! of Z has been computed by ZGETC2. On entry RHS = f holds the
     !! contribution from earlier solved sub-systems, and on return RHS = x.
     !! The factorization of Z returned by ZGETC2 has the form
     !! Z = P * L * U * Q, where P and Q are permutation matrices. L is lower
     !! triangular with unit diagonal elements and U is upper triangular.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ijob, ldz, n
           real(dp), intent(inout) :: rdscal, rdsum
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*), jpiv(*)
           complex(dp), intent(inout) :: rhs(*), z(ldz,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: maxdim = 2_ilp
           
           
           
           ! Local Scalars 
           integer(ilp) :: i, info, j, k
           real(dp) :: rtemp, scale, sminu, splus
           complex(dp) :: bm, bp, pmone, temp
           ! Local Arrays 
           real(dp) :: rwork(maxdim)
           complex(dp) :: work(4_ilp*maxdim), xm(maxdim), xp(maxdim)
           ! Intrinsic Functions 
           ! Executable Statements 
           if( ijob/=2_ilp ) then
              ! apply permutations ipiv to rhs
              call stdlib_zlaswp( 1_ilp, rhs, ldz, 1_ilp, n-1, ipiv, 1_ilp )
              ! solve for l-part choosing rhs either to +1 or -1.
              pmone = -cone
              loop_10: do j = 1, n - 1
                 bp = rhs( j ) + cone
                 bm = rhs( j ) - cone
                 splus = one
                 ! lockahead for l- part rhs(1:n-1) = +-1
                 ! splus and smin computed more efficiently than in bsolve[1].
                 splus = splus + real( stdlib_zdotc( n-j, z( j+1, j ), 1_ilp, z( j+1,j ), 1_ilp ),KIND=dp)
                           
                 sminu = real( stdlib_zdotc( n-j, z( j+1, j ), 1_ilp, rhs( j+1 ), 1_ilp ),KIND=dp)
                 splus = splus*real( rhs( j ),KIND=dp)
                 if( splus>sminu ) then
                    rhs( j ) = bp
                 else if( sminu>splus ) then
                    rhs( j ) = bm
                 else
                    ! in this case the updating sums are equal and we can
                    ! choose rhs(j) +1 or -1. the first time this happens we
                    ! choose -1, thereafter +1. this is a simple way to get
                    ! good estimates of matrices like byers well-known example
                    ! (see [1]). (not done in bsolve.)
                    rhs( j ) = rhs( j ) + pmone
                    pmone = cone
                 end if
                 ! compute the remaining r.h.s.
                 temp = -rhs( j )
                 call stdlib_zaxpy( n-j, temp, z( j+1, j ), 1_ilp, rhs( j+1 ), 1_ilp )
              end do loop_10
              ! solve for u- part, lockahead for rhs(n) = +-1. this is not done
              ! in bsolve and will hopefully give us a better estimate because
              ! any ill-conditioning of the original matrix is transferred to u
              ! and not to l. u(n, n) is an approximation to sigma_min(lu).
              call stdlib_zcopy( n-1, rhs, 1_ilp, work, 1_ilp )
              work( n ) = rhs( n ) + cone
              rhs( n ) = rhs( n ) - cone
              splus = zero
              sminu = zero
              do i = n, 1, -1
                 temp = cone / z( i, i )
                 work( i ) = work( i )*temp
                 rhs( i ) = rhs( i )*temp
                 do k = i + 1, n
                    work( i ) = work( i ) - work( k )*( z( i, k )*temp )
                    rhs( i ) = rhs( i ) - rhs( k )*( z( i, k )*temp )
                 end do
                 splus = splus + abs( work( i ) )
                 sminu = sminu + abs( rhs( i ) )
              end do
              if( splus>sminu )call stdlib_zcopy( n, work, 1_ilp, rhs, 1_ilp )
              ! apply the permutations jpiv to the computed solution (rhs)
              call stdlib_zlaswp( 1_ilp, rhs, ldz, 1_ilp, n-1, jpiv, -1_ilp )
              ! compute the sum of squares
              call stdlib_zlassq( n, rhs, 1_ilp, rdscal, rdsum )
              return
           end if
           ! entry ijob = 2
           ! compute approximate nullvector xm of z
           call stdlib_zgecon( 'I', n, z, ldz, one, rtemp, work, rwork, info )
           call stdlib_zcopy( n, work( n+1 ), 1_ilp, xm, 1_ilp )
           ! compute rhs
           call stdlib_zlaswp( 1_ilp, xm, ldz, 1_ilp, n-1, ipiv, -1_ilp )
           temp = cone / sqrt( stdlib_zdotc( n, xm, 1_ilp, xm, 1_ilp ) )
           call stdlib_zscal( n, temp, xm, 1_ilp )
           call stdlib_zcopy( n, xm, 1_ilp, xp, 1_ilp )
           call stdlib_zaxpy( n, cone, rhs, 1_ilp, xp, 1_ilp )
           call stdlib_zaxpy( n, -cone, xm, 1_ilp, rhs, 1_ilp )
           call stdlib_zgesc2( n, z, ldz, rhs, ipiv, jpiv, scale )
           call stdlib_zgesc2( n, z, ldz, xp, ipiv, jpiv, scale )
           if( stdlib_dzasum( n, xp, 1_ilp )>stdlib_dzasum( n, rhs, 1_ilp ) )call stdlib_zcopy( n, xp, 1_ilp, &
                     rhs, 1_ilp )
           ! compute the sum of squares
           call stdlib_zlassq( n, rhs, 1_ilp, rdscal, rdsum )
           return
     end subroutine stdlib_zlatdf




     real(sp) module function stdlib_sla_gercond( trans, n, a, lda, af, ldaf, ipiv,cmode, c, info, work, &
     !! SLA_GERCOND estimates the Skeel condition number of op(A) * op2(C)
     !! where op2 is determined by CMODE as follows
     !! CMODE =  1    op2(C) = C
     !! CMODE =  0    op2(C) = I
     !! CMODE = -1    op2(C) = inv(C)
     !! The Skeel condition number cond(A) = norminf( |inv(A)||A| )
     !! is computed by computing scaling factors R such that
     !! diag(R)*A*op2(C) is row equilibrated and computing the standard
     !! infinity-norm condition number.
               iwork )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(in) :: n, lda, ldaf, cmode
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(in) :: a(lda,*), af(ldaf,*), c(*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: notrans
           integer(ilp) :: kase, i, j
           real(sp) :: ainvnm, tmp
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           stdlib_sla_gercond = zero
           info = 0_ilp
           notrans = stdlib_lsame( trans, 'N' )
           if ( .not. notrans .and. .not. stdlib_lsame(trans, 'T').and. .not. stdlib_lsame(trans, &
                     'C') ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( ldaf<max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SLA_GERCOND', -info )
              return
           end if
           if( n==0_ilp ) then
              stdlib_sla_gercond = one
              return
           end if
           ! compute the equilibration matrix r such that
           ! inv(r)*a*c has unit 1-norm.
           if (notrans) then
              do i = 1, n
                 tmp = zero
                 if ( cmode == 1_ilp ) then
                    do j = 1, n
                       tmp = tmp + abs( a( i, j ) * c( j ) )
                    end do
                 else if ( cmode == 0_ilp ) then
                    do j = 1, n
                       tmp = tmp + abs( a( i, j ) )
                    end do
                 else
                    do j = 1, n
                       tmp = tmp + abs( a( i, j ) / c( j ) )
                    end do
                 end if
                 work( 2_ilp*n+i ) = tmp
              end do
           else
              do i = 1, n
                 tmp = zero
                 if ( cmode == 1_ilp ) then
                    do j = 1, n
                       tmp = tmp + abs( a( j, i ) * c( j ) )
                    end do
                 else if ( cmode == 0_ilp ) then
                    do j = 1, n
                       tmp = tmp + abs( a( j, i ) )
                    end do
                 else
                    do j = 1, n
                       tmp = tmp + abs( a( j, i ) / c( j ) )
                    end do
                 end if
                 work( 2_ilp*n+i ) = tmp
              end do
           end if
           ! estimate the norm of inv(op(a)).
           ainvnm = zero
           kase = 0_ilp
           10 continue
           call stdlib_slacn2( n, work( n+1 ), work, iwork, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              if( kase==2_ilp ) then
                 ! multiply by r.
                 do i = 1, n
                    work(i) = work(i) * work(2_ilp*n+i)
                 end do
                 if (notrans) then
                    call stdlib_sgetrs( 'NO TRANSPOSE', n, 1_ilp, af, ldaf, ipiv,work, n, info )
                              
                 else
                    call stdlib_sgetrs( 'TRANSPOSE', n, 1_ilp, af, ldaf, ipiv,work, n, info )
                 end if
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
                 if (notrans) then
                    call stdlib_sgetrs( 'TRANSPOSE', n, 1_ilp, af, ldaf, ipiv,work, n, info )
                 else
                    call stdlib_sgetrs( 'NO TRANSPOSE', n, 1_ilp, af, ldaf, ipiv,work, n, info )
                              
                 end if
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * work( 2_ilp*n+i )
                 end do
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm /= 0.0_sp )stdlib_sla_gercond = ( one / ainvnm )
           return
     end function stdlib_sla_gercond

     real(dp) module function stdlib_dla_gercond( trans, n, a, lda, af,ldaf, ipiv, cmode, c,info, work, &
     !! DLA_GERCOND estimates the Skeel condition number of op(A) * op2(C)
     !! where op2 is determined by CMODE as follows
     !! CMODE =  1    op2(C) = C
     !! CMODE =  0    op2(C) = I
     !! CMODE = -1    op2(C) = inv(C)
     !! The Skeel condition number cond(A) = norminf( |inv(A)||A| )
     !! is computed by computing scaling factors R such that
     !! diag(R)*A*op2(C) is row equilibrated and computing the standard
     !! infinity-norm condition number.
               iwork )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(in) :: n, lda, ldaf, cmode
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(in) :: a(lda,*), af(ldaf,*), c(*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: notrans
           integer(ilp) :: kase, i, j
           real(dp) :: ainvnm, tmp
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           stdlib_dla_gercond = zero
           info = 0_ilp
           notrans = stdlib_lsame( trans, 'N' )
           if ( .not. notrans .and. .not. stdlib_lsame(trans, 'T').and. .not. stdlib_lsame(trans, &
                     'C') ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( ldaf<max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DLA_GERCOND', -info )
              return
           end if
           if( n==0_ilp ) then
              stdlib_dla_gercond = one
              return
           end if
           ! compute the equilibration matrix r such that
           ! inv(r)*a*c has unit 1-norm.
           if (notrans) then
              do i = 1, n
                 tmp = zero
                 if ( cmode == 1_ilp ) then
                    do j = 1, n
                       tmp = tmp + abs( a( i, j ) * c( j ) )
                    end do
                 else if ( cmode == 0_ilp ) then
                    do j = 1, n
                       tmp = tmp + abs( a( i, j ) )
                    end do
                 else
                    do j = 1, n
                       tmp = tmp + abs( a( i, j ) / c( j ) )
                    end do
                 end if
                 work( 2_ilp*n+i ) = tmp
              end do
           else
              do i = 1, n
                 tmp = zero
                 if ( cmode == 1_ilp ) then
                    do j = 1, n
                       tmp = tmp + abs( a( j, i ) * c( j ) )
                    end do
                 else if ( cmode == 0_ilp ) then
                    do j = 1, n
                       tmp = tmp + abs( a( j, i ) )
                    end do
                 else
                    do j = 1, n
                       tmp = tmp + abs( a( j, i ) / c( j ) )
                    end do
                 end if
                 work( 2_ilp*n+i ) = tmp
              end do
           end if
           ! estimate the norm of inv(op(a)).
           ainvnm = zero
           kase = 0_ilp
           10 continue
           call stdlib_dlacn2( n, work( n+1 ), work, iwork, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              if( kase==2_ilp ) then
                 ! multiply by r.
                 do i = 1, n
                    work(i) = work(i) * work(2_ilp*n+i)
                 end do
                 if (notrans) then
                    call stdlib_dgetrs( 'NO TRANSPOSE', n, 1_ilp, af, ldaf, ipiv,work, n, info )
                              
                 else
                    call stdlib_dgetrs( 'TRANSPOSE', n, 1_ilp, af, ldaf, ipiv,work, n, info )
                 end if
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
                 if (notrans) then
                    call stdlib_dgetrs( 'TRANSPOSE', n, 1_ilp, af, ldaf, ipiv,work, n, info )
                 else
                    call stdlib_dgetrs( 'NO TRANSPOSE', n, 1_ilp, af, ldaf, ipiv,work, n, info )
                              
                 end if
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * work( 2_ilp*n+i )
                 end do
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm /= zero )stdlib_dla_gercond = ( one / ainvnm )
           return
     end function stdlib_dla_gercond




     pure module subroutine stdlib_sgbcon( norm, n, kl, ku, ab, ldab, ipiv, anorm, rcond,work, iwork, &
     !! SGBCON estimates the reciprocal of the condition number of a real
     !! general band matrix A, in either the 1-norm or the infinity-norm,
     !! using the LU factorization computed by SGBTRF.
     !! An estimate is obtained for norm(inv(A)), and the reciprocal of the
     !! condition number is computed as
     !! RCOND = 1 / ( norm(A) * norm(inv(A)) ).
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, ldab, n
           real(sp), intent(in) :: anorm
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(in) :: ab(ldab,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lnoti, onenrm
           character :: normin
           integer(ilp) :: ix, j, jp, kase, kase1, kd, lm
           real(sp) :: ainvnm, scale, smlnum, t
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           onenrm = norm=='1' .or. stdlib_lsame( norm, 'O' )
           if( .not.onenrm .and. .not.stdlib_lsame( norm, 'I' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kl<0_ilp ) then
              info = -3_ilp
           else if( ku<0_ilp ) then
              info = -4_ilp
           else if( ldab<2_ilp*kl+ku+1 ) then
              info = -6_ilp
           else if( anorm<zero ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGBCON', -info )
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
           ! estimate the norm of inv(a).
           ainvnm = zero
           normin = 'N'
           if( onenrm ) then
              kase1 = 1_ilp
           else
              kase1 = 2_ilp
           end if
           kd = kl + ku + 1_ilp
           lnoti = kl>0_ilp
           kase = 0_ilp
           10 continue
           call stdlib_slacn2( n, work( n+1 ), work, iwork, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              if( kase==kase1 ) then
                 ! multiply by inv(l).
                 if( lnoti ) then
                    do j = 1, n - 1
                       lm = min( kl, n-j )
                       jp = ipiv( j )
                       t = work( jp )
                       if( jp/=j ) then
                          work( jp ) = work( j )
                          work( j ) = t
                       end if
                       call stdlib_saxpy( lm, -t, ab( kd+1, j ), 1_ilp, work( j+1 ), 1_ilp )
                    end do
                 end if
                 ! multiply by inv(u).
                 call stdlib_slatbs( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', normin, n,kl+ku, ab, &
                           ldab, work, scale, work( 2_ilp*n+1 ),info )
              else
                 ! multiply by inv(u**t).
                 call stdlib_slatbs( 'UPPER', 'TRANSPOSE', 'NON-UNIT', normin, n,kl+ku, ab, ldab, &
                           work, scale, work( 2_ilp*n+1 ),info )
                 ! multiply by inv(l**t).
                 if( lnoti ) then
                    do j = n - 1, 1, -1
                       lm = min( kl, n-j )
                       work( j ) = work( j ) - stdlib_sdot( lm, ab( kd+1, j ), 1_ilp,work( j+1 ), 1_ilp )
                                 
                       jp = ipiv( j )
                       if( jp/=j ) then
                          t = work( jp )
                          work( jp ) = work( j )
                          work( j ) = t
                       end if
                    end do
                 end if
              end if
              ! divide x by 1/scale if doing so will not cause overflow.
              normin = 'Y'
              if( scale/=one ) then
                 ix = stdlib_isamax( n, work, 1_ilp )
                 if( scale<abs( work( ix ) )*smlnum .or. scale==zero )go to 40
                 call stdlib_srscl( n, scale, work, 1_ilp )
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm/=zero )rcond = ( one / ainvnm ) / anorm
           40 continue
           return
     end subroutine stdlib_sgbcon

     pure module subroutine stdlib_dgbcon( norm, n, kl, ku, ab, ldab, ipiv, anorm, rcond,work, iwork, &
     !! DGBCON estimates the reciprocal of the condition number of a real
     !! general band matrix A, in either the 1-norm or the infinity-norm,
     !! using the LU factorization computed by DGBTRF.
     !! An estimate is obtained for norm(inv(A)), and the reciprocal of the
     !! condition number is computed as
     !! RCOND = 1 / ( norm(A) * norm(inv(A)) ).
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, ldab, n
           real(dp), intent(in) :: anorm
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(in) :: ab(ldab,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lnoti, onenrm
           character :: normin
           integer(ilp) :: ix, j, jp, kase, kase1, kd, lm
           real(dp) :: ainvnm, scale, smlnum, t
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           onenrm = norm=='1' .or. stdlib_lsame( norm, 'O' )
           if( .not.onenrm .and. .not.stdlib_lsame( norm, 'I' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kl<0_ilp ) then
              info = -3_ilp
           else if( ku<0_ilp ) then
              info = -4_ilp
           else if( ldab<2_ilp*kl+ku+1 ) then
              info = -6_ilp
           else if( anorm<zero ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGBCON', -info )
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
           ! estimate the norm of inv(a).
           ainvnm = zero
           normin = 'N'
           if( onenrm ) then
              kase1 = 1_ilp
           else
              kase1 = 2_ilp
           end if
           kd = kl + ku + 1_ilp
           lnoti = kl>0_ilp
           kase = 0_ilp
           10 continue
           call stdlib_dlacn2( n, work( n+1 ), work, iwork, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              if( kase==kase1 ) then
                 ! multiply by inv(l).
                 if( lnoti ) then
                    do j = 1, n - 1
                       lm = min( kl, n-j )
                       jp = ipiv( j )
                       t = work( jp )
                       if( jp/=j ) then
                          work( jp ) = work( j )
                          work( j ) = t
                       end if
                       call stdlib_daxpy( lm, -t, ab( kd+1, j ), 1_ilp, work( j+1 ), 1_ilp )
                    end do
                 end if
                 ! multiply by inv(u).
                 call stdlib_dlatbs( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', normin, n,kl+ku, ab, &
                           ldab, work, scale, work( 2_ilp*n+1 ),info )
              else
                 ! multiply by inv(u**t).
                 call stdlib_dlatbs( 'UPPER', 'TRANSPOSE', 'NON-UNIT', normin, n,kl+ku, ab, ldab, &
                           work, scale, work( 2_ilp*n+1 ),info )
                 ! multiply by inv(l**t).
                 if( lnoti ) then
                    do j = n - 1, 1, -1
                       lm = min( kl, n-j )
                       work( j ) = work( j ) - stdlib_ddot( lm, ab( kd+1, j ), 1_ilp,work( j+1 ), 1_ilp )
                                 
                       jp = ipiv( j )
                       if( jp/=j ) then
                          t = work( jp )
                          work( jp ) = work( j )
                          work( j ) = t
                       end if
                    end do
                 end if
              end if
              ! divide x by 1/scale if doing so will not cause overflow.
              normin = 'Y'
              if( scale/=one ) then
                 ix = stdlib_idamax( n, work, 1_ilp )
                 if( scale<abs( work( ix ) )*smlnum .or. scale==zero )go to 40
                 call stdlib_drscl( n, scale, work, 1_ilp )
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm/=zero )rcond = ( one / ainvnm ) / anorm
           40 continue
           return
     end subroutine stdlib_dgbcon


     pure module subroutine stdlib_cgbcon( norm, n, kl, ku, ab, ldab, ipiv, anorm, rcond,work, rwork, &
     !! CGBCON estimates the reciprocal of the condition number of a complex
     !! general band matrix A, in either the 1-norm or the infinity-norm,
     !! using the LU factorization computed by CGBTRF.
     !! An estimate is obtained for norm(inv(A)), and the reciprocal of the
     !! condition number is computed as
     !! RCOND = 1 / ( norm(A) * norm(inv(A)) ).
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, ldab, n
           real(sp), intent(in) :: anorm
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(in) :: ab(ldab,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lnoti, onenrm
           character :: normin
           integer(ilp) :: ix, j, jp, kase, kase1, kd, lm
           real(sp) :: ainvnm, scale, smlnum
           complex(sp) :: t, zdum
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
           onenrm = norm=='1' .or. stdlib_lsame( norm, 'O' )
           if( .not.onenrm .and. .not.stdlib_lsame( norm, 'I' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kl<0_ilp ) then
              info = -3_ilp
           else if( ku<0_ilp ) then
              info = -4_ilp
           else if( ldab<2_ilp*kl+ku+1 ) then
              info = -6_ilp
           else if( anorm<zero ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGBCON', -info )
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
           ! estimate the norm of inv(a).
           ainvnm = zero
           normin = 'N'
           if( onenrm ) then
              kase1 = 1_ilp
           else
              kase1 = 2_ilp
           end if
           kd = kl + ku + 1_ilp
           lnoti = kl>0_ilp
           kase = 0_ilp
           10 continue
           call stdlib_clacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              if( kase==kase1 ) then
                 ! multiply by inv(l).
                 if( lnoti ) then
                    do j = 1, n - 1
                       lm = min( kl, n-j )
                       jp = ipiv( j )
                       t = work( jp )
                       if( jp/=j ) then
                          work( jp ) = work( j )
                          work( j ) = t
                       end if
                       call stdlib_caxpy( lm, -t, ab( kd+1, j ), 1_ilp, work( j+1 ), 1_ilp )
                    end do
                 end if
                 ! multiply by inv(u).
                 call stdlib_clatbs( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', normin, n,kl+ku, ab, &
                           ldab, work, scale, rwork, info )
              else
                 ! multiply by inv(u**h).
                 call stdlib_clatbs( 'UPPER', 'CONJUGATE TRANSPOSE', 'NON-UNIT',normin, n, kl+ku, &
                           ab, ldab, work, scale, rwork,info )
                 ! multiply by inv(l**h).
                 if( lnoti ) then
                    do j = n - 1, 1, -1
                       lm = min( kl, n-j )
                       work( j ) = work( j ) - stdlib_cdotc( lm, ab( kd+1, j ), 1_ilp,work( j+1 ), 1_ilp )
                                 
                       jp = ipiv( j )
                       if( jp/=j ) then
                          t = work( jp )
                          work( jp ) = work( j )
                          work( j ) = t
                       end if
                    end do
                 end if
              end if
              ! divide x by 1/scale if doing so will not cause overflow.
              normin = 'Y'
              if( scale/=one ) then
                 ix = stdlib_icamax( n, work, 1_ilp )
                 if( scale<cabs1( work( ix ) )*smlnum .or. scale==zero )go to 40
                 call stdlib_csrscl( n, scale, work, 1_ilp )
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm/=zero )rcond = ( one / ainvnm ) / anorm
           40 continue
           return
     end subroutine stdlib_cgbcon

     pure module subroutine stdlib_zgbcon( norm, n, kl, ku, ab, ldab, ipiv, anorm, rcond,work, rwork, &
     !! ZGBCON estimates the reciprocal of the condition number of a complex
     !! general band matrix A, in either the 1-norm or the infinity-norm,
     !! using the LU factorization computed by ZGBTRF.
     !! An estimate is obtained for norm(inv(A)), and the reciprocal of the
     !! condition number is computed as
     !! RCOND = 1 / ( norm(A) * norm(inv(A)) ).
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, ldab, n
           real(dp), intent(in) :: anorm
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(in) :: ab(ldab,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lnoti, onenrm
           character :: normin
           integer(ilp) :: ix, j, jp, kase, kase1, kd, lm
           real(dp) :: ainvnm, scale, smlnum
           complex(dp) :: t, zdum
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
           onenrm = norm=='1' .or. stdlib_lsame( norm, 'O' )
           if( .not.onenrm .and. .not.stdlib_lsame( norm, 'I' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kl<0_ilp ) then
              info = -3_ilp
           else if( ku<0_ilp ) then
              info = -4_ilp
           else if( ldab<2_ilp*kl+ku+1 ) then
              info = -6_ilp
           else if( anorm<zero ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGBCON', -info )
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
           ! estimate the norm of inv(a).
           ainvnm = zero
           normin = 'N'
           if( onenrm ) then
              kase1 = 1_ilp
           else
              kase1 = 2_ilp
           end if
           kd = kl + ku + 1_ilp
           lnoti = kl>0_ilp
           kase = 0_ilp
           10 continue
           call stdlib_zlacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              if( kase==kase1 ) then
                 ! multiply by inv(l).
                 if( lnoti ) then
                    do j = 1, n - 1
                       lm = min( kl, n-j )
                       jp = ipiv( j )
                       t = work( jp )
                       if( jp/=j ) then
                          work( jp ) = work( j )
                          work( j ) = t
                       end if
                       call stdlib_zaxpy( lm, -t, ab( kd+1, j ), 1_ilp, work( j+1 ), 1_ilp )
                    end do
                 end if
                 ! multiply by inv(u).
                 call stdlib_zlatbs( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', normin, n,kl+ku, ab, &
                           ldab, work, scale, rwork, info )
              else
                 ! multiply by inv(u**h).
                 call stdlib_zlatbs( 'UPPER', 'CONJUGATE TRANSPOSE', 'NON-UNIT',normin, n, kl+ku, &
                           ab, ldab, work, scale, rwork,info )
                 ! multiply by inv(l**h).
                 if( lnoti ) then
                    do j = n - 1, 1, -1
                       lm = min( kl, n-j )
                       work( j ) = work( j ) - stdlib_zdotc( lm, ab( kd+1, j ), 1_ilp,work( j+1 ), 1_ilp )
                                 
                       jp = ipiv( j )
                       if( jp/=j ) then
                          t = work( jp )
                          work( jp ) = work( j )
                          work( j ) = t
                       end if
                    end do
                 end if
              end if
              ! divide x by 1/scale if doing so will not cause overflow.
              normin = 'Y'
              if( scale/=one ) then
                 ix = stdlib_izamax( n, work, 1_ilp )
                 if( scale<cabs1( work( ix ) )*smlnum .or. scale==zero )go to 40
                 call stdlib_zdrscl( n, scale, work, 1_ilp )
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm/=zero )rcond = ( one / ainvnm ) / anorm
           40 continue
           return
     end subroutine stdlib_zgbcon




     pure module subroutine stdlib_sgbtrf( m, n, kl, ku, ab, ldab, ipiv, info )
     !! SGBTRF computes an LU factorization of a real m-by-n band matrix A
     !! using partial pivoting with row interchanges.
     !! This is the blocked version of the algorithm, calling Level 3 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, ldab, m, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(sp), intent(inout) :: ab(ldab,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: nbmax = 64_ilp
           integer(ilp), parameter :: ldwork = nbmax+1
           
           
           ! Local Scalars 
           integer(ilp) :: i, i2, i3, ii, ip, j, j2, j3, jb, jj, jm, jp, ju, k2, km, kv, nb, &
                     nw
           real(sp) :: temp
           ! Local Arrays 
           real(sp) :: work13(ldwork,nbmax), work31(ldwork,nbmax)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! kv is the number of superdiagonals in the factor u, allowing for
           ! fill-in
           kv = ku + kl
           ! test the input parameters.
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kl<0_ilp ) then
              info = -3_ilp
           else if( ku<0_ilp ) then
              info = -4_ilp
           else if( ldab<kl+kv+1 ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGBTRF', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 )return
           ! determine the block size for this environment
           nb = stdlib_ilaenv( 1_ilp, 'SGBTRF', ' ', m, n, kl, ku )
           ! the block size must not exceed the limit set by the size of the
           ! local arrays work13 and work31.
           nb = min( nb, nbmax )
           if( nb<=1_ilp .or. nb>kl ) then
              ! use unblocked code
              call stdlib_sgbtf2( m, n, kl, ku, ab, ldab, ipiv, info )
           else
              ! use blocked code
              ! zero the superdiagonal elements of the work array work13
              do j = 1, nb
                 do i = 1, j - 1
                    work13( i, j ) = zero
                 end do
              end do
              ! zero the subdiagonal elements of the work array work31
              do j = 1, nb
                 do i = j + 1, nb
                    work31( i, j ) = zero
                 end do
              end do
              ! gaussian elimination with partial pivoting
              ! set fill-in elements in columns ku+2 to kv to zero
              do j = ku + 2, min( kv, n )
                 do i = kv - j + 2, kl
                    ab( i, j ) = zero
                 end do
              end do
              ! ju is the index of the last column affected by the current
              ! stage of the factorization
              ju = 1_ilp
              loop_180: do j = 1, min( m, n ), nb
                 jb = min( nb, min( m, n )-j+1 )
                 ! the active part of the matrix is partitioned
                    ! a11   a12   a13
                    ! a21   a22   a23
                    ! a31   a32   a33
                 ! here a11, a21 and a31 denote the current block of jb columns
                 ! which is about to be factorized. the number of rows in the
                 ! partitioning are jb, i2, i3 respectively, and the numbers
                 ! of columns are jb, j2, j3. the superdiagonal elements of a13
                 ! and the subdiagonal elements of a31 lie outside the band.
                 i2 = min( kl-jb, m-j-jb+1 )
                 i3 = min( jb, m-j-kl+1 )
                 ! j2 and j3 are computed after ju has been updated.
                 ! factorize the current block of jb columns
                 loop_80: do jj = j, j + jb - 1
                    ! set fill-in elements in column jj+kv to zero
                    if( jj+kv<=n ) then
                       do i = 1, kl
                          ab( i, jj+kv ) = zero
                       end do
                    end if
                    ! find pivot and test for singularity. km is the number of
                    ! subdiagonal elements in the current column.
                    km = min( kl, m-jj )
                    jp = stdlib_isamax( km+1, ab( kv+1, jj ), 1_ilp )
                    ipiv( jj ) = jp + jj - j
                    if( ab( kv+jp, jj )/=zero ) then
                       ju = max( ju, min( jj+ku+jp-1, n ) )
                       if( jp/=1_ilp ) then
                          ! apply interchange to columns j to j+jb-1
                          if( jp+jj-1<j+kl ) then
                             call stdlib_sswap( jb, ab( kv+1+jj-j, j ), ldab-1,ab( kv+jp+jj-j, j )&
                                       , ldab-1 )
                          else
                             ! the interchange affects columns j to jj-1 of a31
                             ! which are stored in the work array work31
                             call stdlib_sswap( jj-j, ab( kv+1+jj-j, j ), ldab-1,work31( jp+jj-j-&
                                       kl, 1_ilp ), ldwork )
                             call stdlib_sswap( j+jb-jj, ab( kv+1, jj ), ldab-1,ab( kv+jp, jj ), &
                                       ldab-1 )
                          end if
                       end if
                       ! compute multipliers
                       call stdlib_sscal( km, one / ab( kv+1, jj ), ab( kv+2, jj ),1_ilp )
                       ! update trailing submatrix within the band and within
                       ! the current block. jm is the index of the last column
                       ! which needs to be updated.
                       jm = min( ju, j+jb-1 )
                       if( jm>jj )call stdlib_sger( km, jm-jj, -one, ab( kv+2, jj ), 1_ilp,ab( kv, jj+&
                                 1_ilp ), ldab-1,ab( kv+1, jj+1 ), ldab-1 )
                    else
                       ! if pivot is zero, set info to the index of the pivot
                       ! unless a zero pivot has already been found.
                       if( info==0_ilp )info = jj
                    end if
                    ! copy current column of a31 into the work array work31
                    nw = min( jj-j+1, i3 )
                    if( nw>0_ilp )call stdlib_scopy( nw, ab( kv+kl+1-jj+j, jj ), 1_ilp,work31( 1_ilp, jj-j+1 )&
                              , 1_ilp )
                 end do loop_80
                 if( j+jb<=n ) then
                    ! apply the row interchanges to the other blocks.
                    j2 = min( ju-j+1, kv ) - jb
                    j3 = max( 0_ilp, ju-j-kv+1 )
                    ! use stdlib_slaswp to apply the row interchanges to a12, a22, and
                    ! a32.
                    call stdlib_slaswp( j2, ab( kv+1-jb, j+jb ), ldab-1, 1_ilp, jb,ipiv( j ), 1_ilp )
                              
                    ! adjust the pivot indices.
                    do i = j, j + jb - 1
                       ipiv( i ) = ipiv( i ) + j - 1_ilp
                    end do
                    ! apply the row interchanges to a13, a23, and a33
                    ! columnwise.
                    k2 = j - 1_ilp + jb + j2
                    do i = 1, j3
                       jj = k2 + i
                       do ii = j + i - 1, j + jb - 1
                          ip = ipiv( ii )
                          if( ip/=ii ) then
                             temp = ab( kv+1+ii-jj, jj )
                             ab( kv+1+ii-jj, jj ) = ab( kv+1+ip-jj, jj )
                             ab( kv+1+ip-jj, jj ) = temp
                          end if
                       end do
                    end do
                    ! update the relevant part of the trailing submatrix
                    if( j2>0_ilp ) then
                       ! update a12
                       call stdlib_strsm( 'LEFT', 'LOWER', 'NO TRANSPOSE', 'UNIT',jb, j2, one, ab(&
                                  kv+1, j ), ldab-1,ab( kv+1-jb, j+jb ), ldab-1 )
                       if( i2>0_ilp ) then
                          ! update a22
                          call stdlib_sgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', i2, j2,jb, -one, ab( &
                          kv+1+jb, j ), ldab-1,ab( kv+1-jb, j+jb ), ldab-1, one,ab( kv+1, j+jb ), &
                                    ldab-1 )
                       end if
                       if( i3>0_ilp ) then
                          ! update a32
                          call stdlib_sgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', i3, j2,jb, -one, &
                          work31, ldwork,ab( kv+1-jb, j+jb ), ldab-1, one,ab( kv+kl+1-jb, j+jb ), &
                                    ldab-1 )
                       end if
                    end if
                    if( j3>0_ilp ) then
                       ! copy the lower triangle of a13 into the work array
                       ! work13
                       do jj = 1, j3
                          do ii = jj, jb
                             work13( ii, jj ) = ab( ii-jj+1, jj+j+kv-1 )
                          end do
                       end do
                       ! update a13 in the work array
                       call stdlib_strsm( 'LEFT', 'LOWER', 'NO TRANSPOSE', 'UNIT',jb, j3, one, ab(&
                                  kv+1, j ), ldab-1,work13, ldwork )
                       if( i2>0_ilp ) then
                          ! update a23
                          call stdlib_sgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', i2, j3,jb, -one, ab( &
                          kv+1+jb, j ), ldab-1,work13, ldwork, one, ab( 1_ilp+jb, j+kv ),ldab-1 )
                                    
                       end if
                       if( i3>0_ilp ) then
                          ! update a33
                          call stdlib_sgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', i3, j3,jb, -one, &
                                    work31, ldwork, work13,ldwork, one, ab( 1_ilp+kl, j+kv ), ldab-1 )
                       end if
                       ! copy the lower triangle of a13 back into place
                       do jj = 1, j3
                          do ii = jj, jb
                             ab( ii-jj+1, jj+j+kv-1 ) = work13( ii, jj )
                          end do
                       end do
                    end if
                 else
                    ! adjust the pivot indices.
                    do i = j, j + jb - 1
                       ipiv( i ) = ipiv( i ) + j - 1_ilp
                    end do
                 end if
                 ! partially undo the interchanges in the current block to
                 ! restore the upper triangular form of a31 and copy the upper
                 ! triangle of a31 back into place
                 do jj = j + jb - 1, j, -1
                    jp = ipiv( jj ) - jj + 1_ilp
                    if( jp/=1_ilp ) then
                       ! apply interchange to columns j to jj-1
                       if( jp+jj-1<j+kl ) then
                          ! the interchange does not affect a31
                          call stdlib_sswap( jj-j, ab( kv+1+jj-j, j ), ldab-1,ab( kv+jp+jj-j, j ),&
                                     ldab-1 )
                       else
                          ! the interchange does affect a31
                          call stdlib_sswap( jj-j, ab( kv+1+jj-j, j ), ldab-1,work31( jp+jj-j-kl, &
                                    1_ilp ), ldwork )
                       end if
                    end if
                    ! copy the current column of a31 back into place
                    nw = min( i3, jj-j+1 )
                    if( nw>0_ilp )call stdlib_scopy( nw, work31( 1_ilp, jj-j+1 ), 1_ilp,ab( kv+kl+1-jj+j, jj )&
                              , 1_ilp )
                 end do
              end do loop_180
           end if
           return
     end subroutine stdlib_sgbtrf

     pure module subroutine stdlib_dgbtrf( m, n, kl, ku, ab, ldab, ipiv, info )
     !! DGBTRF computes an LU factorization of a real m-by-n band matrix A
     !! using partial pivoting with row interchanges.
     !! This is the blocked version of the algorithm, calling Level 3 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, ldab, m, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(dp), intent(inout) :: ab(ldab,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: nbmax = 64_ilp
           integer(ilp), parameter :: ldwork = nbmax+1
           
           
           ! Local Scalars 
           integer(ilp) :: i, i2, i3, ii, ip, j, j2, j3, jb, jj, jm, jp, ju, k2, km, kv, nb, &
                     nw
           real(dp) :: temp
           ! Local Arrays 
           real(dp) :: work13(ldwork,nbmax), work31(ldwork,nbmax)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! kv is the number of superdiagonals in the factor u, allowing for
           ! fill-in
           kv = ku + kl
           ! test the input parameters.
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kl<0_ilp ) then
              info = -3_ilp
           else if( ku<0_ilp ) then
              info = -4_ilp
           else if( ldab<kl+kv+1 ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGBTRF', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 )return
           ! determine the block size for this environment
           nb = stdlib_ilaenv( 1_ilp, 'DGBTRF', ' ', m, n, kl, ku )
           ! the block size must not exceed the limit set by the size of the
           ! local arrays work13 and work31.
           nb = min( nb, nbmax )
           if( nb<=1_ilp .or. nb>kl ) then
              ! use unblocked code
              call stdlib_dgbtf2( m, n, kl, ku, ab, ldab, ipiv, info )
           else
              ! use blocked code
              ! zero the superdiagonal elements of the work array work13
              do j = 1, nb
                 do i = 1, j - 1
                    work13( i, j ) = zero
                 end do
              end do
              ! zero the subdiagonal elements of the work array work31
              do j = 1, nb
                 do i = j + 1, nb
                    work31( i, j ) = zero
                 end do
              end do
              ! gaussian elimination with partial pivoting
              ! set fill-in elements in columns ku+2 to kv to zero
              do j = ku + 2, min( kv, n )
                 do i = kv - j + 2, kl
                    ab( i, j ) = zero
                 end do
              end do
              ! ju is the index of the last column affected by the current
              ! stage of the factorization
              ju = 1_ilp
              loop_180: do j = 1, min( m, n ), nb
                 jb = min( nb, min( m, n )-j+1 )
                 ! the active part of the matrix is partitioned
                    ! a11   a12   a13
                    ! a21   a22   a23
                    ! a31   a32   a33
                 ! here a11, a21 and a31 denote the current block of jb columns
                 ! which is about to be factorized. the number of rows in the
                 ! partitioning are jb, i2, i3 respectively, and the numbers
                 ! of columns are jb, j2, j3. the superdiagonal elements of a13
                 ! and the subdiagonal elements of a31 lie outside the band.
                 i2 = min( kl-jb, m-j-jb+1 )
                 i3 = min( jb, m-j-kl+1 )
                 ! j2 and j3 are computed after ju has been updated.
                 ! factorize the current block of jb columns
                 loop_80: do jj = j, j + jb - 1
                    ! set fill-in elements in column jj+kv to zero
                    if( jj+kv<=n ) then
                       do i = 1, kl
                          ab( i, jj+kv ) = zero
                       end do
                    end if
                    ! find pivot and test for singularity. km is the number of
                    ! subdiagonal elements in the current column.
                    km = min( kl, m-jj )
                    jp = stdlib_idamax( km+1, ab( kv+1, jj ), 1_ilp )
                    ipiv( jj ) = jp + jj - j
                    if( ab( kv+jp, jj )/=zero ) then
                       ju = max( ju, min( jj+ku+jp-1, n ) )
                       if( jp/=1_ilp ) then
                          ! apply interchange to columns j to j+jb-1
                          if( jp+jj-1<j+kl ) then
                             call stdlib_dswap( jb, ab( kv+1+jj-j, j ), ldab-1,ab( kv+jp+jj-j, j )&
                                       , ldab-1 )
                          else
                             ! the interchange affects columns j to jj-1 of a31
                             ! which are stored in the work array work31
                             call stdlib_dswap( jj-j, ab( kv+1+jj-j, j ), ldab-1,work31( jp+jj-j-&
                                       kl, 1_ilp ), ldwork )
                             call stdlib_dswap( j+jb-jj, ab( kv+1, jj ), ldab-1,ab( kv+jp, jj ), &
                                       ldab-1 )
                          end if
                       end if
                       ! compute multipliers
                       call stdlib_dscal( km, one / ab( kv+1, jj ), ab( kv+2, jj ),1_ilp )
                       ! update trailing submatrix within the band and within
                       ! the current block. jm is the index of the last column
                       ! which needs to be updated.
                       jm = min( ju, j+jb-1 )
                       if( jm>jj )call stdlib_dger( km, jm-jj, -one, ab( kv+2, jj ), 1_ilp,ab( kv, jj+&
                                 1_ilp ), ldab-1,ab( kv+1, jj+1 ), ldab-1 )
                    else
                       ! if pivot is zero, set info to the index of the pivot
                       ! unless a zero pivot has already been found.
                       if( info==0_ilp )info = jj
                    end if
                    ! copy current column of a31 into the work array work31
                    nw = min( jj-j+1, i3 )
                    if( nw>0_ilp )call stdlib_dcopy( nw, ab( kv+kl+1-jj+j, jj ), 1_ilp,work31( 1_ilp, jj-j+1 )&
                              , 1_ilp )
                 end do loop_80
                 if( j+jb<=n ) then
                    ! apply the row interchanges to the other blocks.
                    j2 = min( ju-j+1, kv ) - jb
                    j3 = max( 0_ilp, ju-j-kv+1 )
                    ! use stdlib_dlaswp to apply the row interchanges to a12, a22, and
                    ! a32.
                    call stdlib_dlaswp( j2, ab( kv+1-jb, j+jb ), ldab-1, 1_ilp, jb,ipiv( j ), 1_ilp )
                              
                    ! adjust the pivot indices.
                    do i = j, j + jb - 1
                       ipiv( i ) = ipiv( i ) + j - 1_ilp
                    end do
                    ! apply the row interchanges to a13, a23, and a33
                    ! columnwise.
                    k2 = j - 1_ilp + jb + j2
                    do i = 1, j3
                       jj = k2 + i
                       do ii = j + i - 1, j + jb - 1
                          ip = ipiv( ii )
                          if( ip/=ii ) then
                             temp = ab( kv+1+ii-jj, jj )
                             ab( kv+1+ii-jj, jj ) = ab( kv+1+ip-jj, jj )
                             ab( kv+1+ip-jj, jj ) = temp
                          end if
                       end do
                    end do
                    ! update the relevant part of the trailing submatrix
                    if( j2>0_ilp ) then
                       ! update a12
                       call stdlib_dtrsm( 'LEFT', 'LOWER', 'NO TRANSPOSE', 'UNIT',jb, j2, one, ab(&
                                  kv+1, j ), ldab-1,ab( kv+1-jb, j+jb ), ldab-1 )
                       if( i2>0_ilp ) then
                          ! update a22
                          call stdlib_dgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', i2, j2,jb, -one, ab( &
                          kv+1+jb, j ), ldab-1,ab( kv+1-jb, j+jb ), ldab-1, one,ab( kv+1, j+jb ), &
                                    ldab-1 )
                       end if
                       if( i3>0_ilp ) then
                          ! update a32
                          call stdlib_dgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', i3, j2,jb, -one, &
                          work31, ldwork,ab( kv+1-jb, j+jb ), ldab-1, one,ab( kv+kl+1-jb, j+jb ), &
                                    ldab-1 )
                       end if
                    end if
                    if( j3>0_ilp ) then
                       ! copy the lower triangle of a13 into the work array
                       ! work13
                       do jj = 1, j3
                          do ii = jj, jb
                             work13( ii, jj ) = ab( ii-jj+1, jj+j+kv-1 )
                          end do
                       end do
                       ! update a13 in the work array
                       call stdlib_dtrsm( 'LEFT', 'LOWER', 'NO TRANSPOSE', 'UNIT',jb, j3, one, ab(&
                                  kv+1, j ), ldab-1,work13, ldwork )
                       if( i2>0_ilp ) then
                          ! update a23
                          call stdlib_dgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', i2, j3,jb, -one, ab( &
                          kv+1+jb, j ), ldab-1,work13, ldwork, one, ab( 1_ilp+jb, j+kv ),ldab-1 )
                                    
                       end if
                       if( i3>0_ilp ) then
                          ! update a33
                          call stdlib_dgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', i3, j3,jb, -one, &
                                    work31, ldwork, work13,ldwork, one, ab( 1_ilp+kl, j+kv ), ldab-1 )
                       end if
                       ! copy the lower triangle of a13 back into place
                       do jj = 1, j3
                          do ii = jj, jb
                             ab( ii-jj+1, jj+j+kv-1 ) = work13( ii, jj )
                          end do
                       end do
                    end if
                 else
                    ! adjust the pivot indices.
                    do i = j, j + jb - 1
                       ipiv( i ) = ipiv( i ) + j - 1_ilp
                    end do
                 end if
                 ! partially undo the interchanges in the current block to
                 ! restore the upper triangular form of a31 and copy the upper
                 ! triangle of a31 back into place
                 do jj = j + jb - 1, j, -1
                    jp = ipiv( jj ) - jj + 1_ilp
                    if( jp/=1_ilp ) then
                       ! apply interchange to columns j to jj-1
                       if( jp+jj-1<j+kl ) then
                          ! the interchange does not affect a31
                          call stdlib_dswap( jj-j, ab( kv+1+jj-j, j ), ldab-1,ab( kv+jp+jj-j, j ),&
                                     ldab-1 )
                       else
                          ! the interchange does affect a31
                          call stdlib_dswap( jj-j, ab( kv+1+jj-j, j ), ldab-1,work31( jp+jj-j-kl, &
                                    1_ilp ), ldwork )
                       end if
                    end if
                    ! copy the current column of a31 back into place
                    nw = min( i3, jj-j+1 )
                    if( nw>0_ilp )call stdlib_dcopy( nw, work31( 1_ilp, jj-j+1 ), 1_ilp,ab( kv+kl+1-jj+j, jj )&
                              , 1_ilp )
                 end do
              end do loop_180
           end if
           return
     end subroutine stdlib_dgbtrf


     pure module subroutine stdlib_cgbtrf( m, n, kl, ku, ab, ldab, ipiv, info )
     !! CGBTRF computes an LU factorization of a complex m-by-n band matrix A
     !! using partial pivoting with row interchanges.
     !! This is the blocked version of the algorithm, calling Level 3 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, ldab, m, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(sp), intent(inout) :: ab(ldab,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: nbmax = 64_ilp
           integer(ilp), parameter :: ldwork = nbmax+1
           
           
           ! Local Scalars 
           integer(ilp) :: i, i2, i3, ii, ip, j, j2, j3, jb, jj, jm, jp, ju, k2, km, kv, nb, &
                     nw
           complex(sp) :: temp
           ! Local Arrays 
           complex(sp) :: work13(ldwork,nbmax), work31(ldwork,nbmax)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! kv is the number of superdiagonals in the factor u, allowing for
           ! fill-in
           kv = ku + kl
           ! test the input parameters.
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kl<0_ilp ) then
              info = -3_ilp
           else if( ku<0_ilp ) then
              info = -4_ilp
           else if( ldab<kl+kv+1 ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGBTRF', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 )return
           ! determine the block size for this environment
           nb = stdlib_ilaenv( 1_ilp, 'CGBTRF', ' ', m, n, kl, ku )
           ! the block size must not exceed the limit set by the size of the
           ! local arrays work13 and work31.
           nb = min( nb, nbmax )
           if( nb<=1_ilp .or. nb>kl ) then
              ! use unblocked code
              call stdlib_cgbtf2( m, n, kl, ku, ab, ldab, ipiv, info )
           else
              ! use blocked code
              ! czero the superdiagonal elements of the work array work13
              do j = 1, nb
                 do i = 1, j - 1
                    work13( i, j ) = czero
                 end do
              end do
              ! czero the subdiagonal elements of the work array work31
              do j = 1, nb
                 do i = j + 1, nb
                    work31( i, j ) = czero
                 end do
              end do
              ! gaussian elimination with partial pivoting
              ! set fill-in elements in columns ku+2 to kv to czero
              do j = ku + 2, min( kv, n )
                 do i = kv - j + 2, kl
                    ab( i, j ) = czero
                 end do
              end do
              ! ju is the index of the last column affected by the current
              ! stage of the factorization
              ju = 1_ilp
              loop_180: do j = 1, min( m, n ), nb
                 jb = min( nb, min( m, n )-j+1 )
                 ! the active part of the matrix is partitioned
                    ! a11   a12   a13
                    ! a21   a22   a23
                    ! a31   a32   a33
                 ! here a11, a21 and a31 denote the current block of jb columns
                 ! which is about to be factorized. the number of rows in the
                 ! partitioning are jb, i2, i3 respectively, and the numbers
                 ! of columns are jb, j2, j3. the superdiagonal elements of a13
                 ! and the subdiagonal elements of a31 lie outside the band.
                 i2 = min( kl-jb, m-j-jb+1 )
                 i3 = min( jb, m-j-kl+1 )
                 ! j2 and j3 are computed after ju has been updated.
                 ! factorize the current block of jb columns
                 loop_80: do jj = j, j + jb - 1
                    ! set fill-in elements in column jj+kv to czero
                    if( jj+kv<=n ) then
                       do i = 1, kl
                          ab( i, jj+kv ) = czero
                       end do
                    end if
                    ! find pivot and test for singularity. km is the number of
                    ! subdiagonal elements in the current column.
                    km = min( kl, m-jj )
                    jp = stdlib_icamax( km+1, ab( kv+1, jj ), 1_ilp )
                    ipiv( jj ) = jp + jj - j
                    if( ab( kv+jp, jj )/=czero ) then
                       ju = max( ju, min( jj+ku+jp-1, n ) )
                       if( jp/=1_ilp ) then
                          ! apply interchange to columns j to j+jb-1
                          if( jp+jj-1<j+kl ) then
                             call stdlib_cswap( jb, ab( kv+1+jj-j, j ), ldab-1,ab( kv+jp+jj-j, j )&
                                       , ldab-1 )
                          else
                             ! the interchange affects columns j to jj-1 of a31
                             ! which are stored in the work array work31
                             call stdlib_cswap( jj-j, ab( kv+1+jj-j, j ), ldab-1,work31( jp+jj-j-&
                                       kl, 1_ilp ), ldwork )
                             call stdlib_cswap( j+jb-jj, ab( kv+1, jj ), ldab-1,ab( kv+jp, jj ), &
                                       ldab-1 )
                          end if
                       end if
                       ! compute multipliers
                       call stdlib_cscal( km, cone / ab( kv+1, jj ), ab( kv+2, jj ),1_ilp )
                       ! update trailing submatrix within the band and within
                       ! the current block. jm is the index of the last column
                       ! which needs to be updated.
                       jm = min( ju, j+jb-1 )
                       if( jm>jj )call stdlib_cgeru( km, jm-jj, -cone, ab( kv+2, jj ), 1_ilp,ab( kv, &
                                 jj+1 ), ldab-1,ab( kv+1, jj+1 ), ldab-1 )
                    else
                       ! if pivot is czero, set info to the index of the pivot
                       ! unless a czero pivot has already been found.
                       if( info==0_ilp )info = jj
                    end if
                    ! copy current column of a31 into the work array work31
                    nw = min( jj-j+1, i3 )
                    if( nw>0_ilp )call stdlib_ccopy( nw, ab( kv+kl+1-jj+j, jj ), 1_ilp,work31( 1_ilp, jj-j+1 )&
                              , 1_ilp )
                 end do loop_80
                 if( j+jb<=n ) then
                    ! apply the row interchanges to the other blocks.
                    j2 = min( ju-j+1, kv ) - jb
                    j3 = max( 0_ilp, ju-j-kv+1 )
                    ! use stdlib_claswp to apply the row interchanges to a12, a22, and
                    ! a32.
                    call stdlib_claswp( j2, ab( kv+1-jb, j+jb ), ldab-1, 1_ilp, jb,ipiv( j ), 1_ilp )
                              
                    ! adjust the pivot indices.
                    do i = j, j + jb - 1
                       ipiv( i ) = ipiv( i ) + j - 1_ilp
                    end do
                    ! apply the row interchanges to a13, a23, and a33
                    ! columnwise.
                    k2 = j - 1_ilp + jb + j2
                    do i = 1, j3
                       jj = k2 + i
                       do ii = j + i - 1, j + jb - 1
                          ip = ipiv( ii )
                          if( ip/=ii ) then
                             temp = ab( kv+1+ii-jj, jj )
                             ab( kv+1+ii-jj, jj ) = ab( kv+1+ip-jj, jj )
                             ab( kv+1+ip-jj, jj ) = temp
                          end if
                       end do
                    end do
                    ! update the relevant part of the trailing submatrix
                    if( j2>0_ilp ) then
                       ! update a12
                       call stdlib_ctrsm( 'LEFT', 'LOWER', 'NO TRANSPOSE', 'UNIT',jb, j2, cone, &
                                 ab( kv+1, j ), ldab-1,ab( kv+1-jb, j+jb ), ldab-1 )
                       if( i2>0_ilp ) then
                          ! update a22
                          call stdlib_cgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', i2, j2,jb, -cone, ab(&
                           kv+1+jb, j ), ldab-1,ab( kv+1-jb, j+jb ), ldab-1, cone,ab( kv+1, j+jb )&
                                     , ldab-1 )
                       end if
                       if( i3>0_ilp ) then
                          ! update a32
                          call stdlib_cgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', i3, j2,jb, -cone, &
                          work31, ldwork,ab( kv+1-jb, j+jb ), ldab-1, cone,ab( kv+kl+1-jb, j+jb ),&
                                     ldab-1 )
                       end if
                    end if
                    if( j3>0_ilp ) then
                       ! copy the lower triangle of a13 into the work array
                       ! work13
                       do jj = 1, j3
                          do ii = jj, jb
                             work13( ii, jj ) = ab( ii-jj+1, jj+j+kv-1 )
                          end do
                       end do
                       ! update a13 in the work array
                       call stdlib_ctrsm( 'LEFT', 'LOWER', 'NO TRANSPOSE', 'UNIT',jb, j3, cone, &
                                 ab( kv+1, j ), ldab-1,work13, ldwork )
                       if( i2>0_ilp ) then
                          ! update a23
                          call stdlib_cgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', i2, j3,jb, -cone, ab(&
                           kv+1+jb, j ), ldab-1,work13, ldwork, cone, ab( 1_ilp+jb, j+kv ),ldab-1 )
                                     
                       end if
                       if( i3>0_ilp ) then
                          ! update a33
                          call stdlib_cgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', i3, j3,jb, -cone, &
                                    work31, ldwork, work13,ldwork, cone, ab( 1_ilp+kl, j+kv ), ldab-1 )
                       end if
                       ! copy the lower triangle of a13 back into place
                       do jj = 1, j3
                          do ii = jj, jb
                             ab( ii-jj+1, jj+j+kv-1 ) = work13( ii, jj )
                          end do
                       end do
                    end if
                 else
                    ! adjust the pivot indices.
                    do i = j, j + jb - 1
                       ipiv( i ) = ipiv( i ) + j - 1_ilp
                    end do
                 end if
                 ! partially undo the interchanges in the current block to
                 ! restore the upper triangular form of a31 and copy the upper
                 ! triangle of a31 back into place
                 do jj = j + jb - 1, j, -1
                    jp = ipiv( jj ) - jj + 1_ilp
                    if( jp/=1_ilp ) then
                       ! apply interchange to columns j to jj-1
                       if( jp+jj-1<j+kl ) then
                          ! the interchange does not affect a31
                          call stdlib_cswap( jj-j, ab( kv+1+jj-j, j ), ldab-1,ab( kv+jp+jj-j, j ),&
                                     ldab-1 )
                       else
                          ! the interchange does affect a31
                          call stdlib_cswap( jj-j, ab( kv+1+jj-j, j ), ldab-1,work31( jp+jj-j-kl, &
                                    1_ilp ), ldwork )
                       end if
                    end if
                    ! copy the current column of a31 back into place
                    nw = min( i3, jj-j+1 )
                    if( nw>0_ilp )call stdlib_ccopy( nw, work31( 1_ilp, jj-j+1 ), 1_ilp,ab( kv+kl+1-jj+j, jj )&
                              , 1_ilp )
                 end do
              end do loop_180
           end if
           return
     end subroutine stdlib_cgbtrf

     pure module subroutine stdlib_zgbtrf( m, n, kl, ku, ab, ldab, ipiv, info )
     !! ZGBTRF computes an LU factorization of a complex m-by-n band matrix A
     !! using partial pivoting with row interchanges.
     !! This is the blocked version of the algorithm, calling Level 3 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, ldab, m, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(dp), intent(inout) :: ab(ldab,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: nbmax = 64_ilp
           integer(ilp), parameter :: ldwork = nbmax+1
           
           
           ! Local Scalars 
           integer(ilp) :: i, i2, i3, ii, ip, j, j2, j3, jb, jj, jm, jp, ju, k2, km, kv, nb, &
                     nw
           complex(dp) :: temp
           ! Local Arrays 
           complex(dp) :: work13(ldwork,nbmax), work31(ldwork,nbmax)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! kv is the number of superdiagonals in the factor u, allowing for
           ! fill-in
           kv = ku + kl
           ! test the input parameters.
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kl<0_ilp ) then
              info = -3_ilp
           else if( ku<0_ilp ) then
              info = -4_ilp
           else if( ldab<kl+kv+1 ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGBTRF', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 )return
           ! determine the block size for this environment
           nb = stdlib_ilaenv( 1_ilp, 'ZGBTRF', ' ', m, n, kl, ku )
           ! the block size must not exceed the limit set by the size of the
           ! local arrays work13 and work31.
           nb = min( nb, nbmax )
           if( nb<=1_ilp .or. nb>kl ) then
              ! use unblocked code
              call stdlib_zgbtf2( m, n, kl, ku, ab, ldab, ipiv, info )
           else
              ! use blocked code
              ! czero the superdiagonal elements of the work array work13
              do j = 1, nb
                 do i = 1, j - 1
                    work13( i, j ) = czero
                 end do
              end do
              ! czero the subdiagonal elements of the work array work31
              do j = 1, nb
                 do i = j + 1, nb
                    work31( i, j ) = czero
                 end do
              end do
              ! gaussian elimination with partial pivoting
              ! set fill-in elements in columns ku+2 to kv to czero
              do j = ku + 2, min( kv, n )
                 do i = kv - j + 2, kl
                    ab( i, j ) = czero
                 end do
              end do
              ! ju is the index of the last column affected by the current
              ! stage of the factorization
              ju = 1_ilp
              loop_180: do j = 1, min( m, n ), nb
                 jb = min( nb, min( m, n )-j+1 )
                 ! the active part of the matrix is partitioned
                    ! a11   a12   a13
                    ! a21   a22   a23
                    ! a31   a32   a33
                 ! here a11, a21 and a31 denote the current block of jb columns
                 ! which is about to be factorized. the number of rows in the
                 ! partitioning are jb, i2, i3 respectively, and the numbers
                 ! of columns are jb, j2, j3. the superdiagonal elements of a13
                 ! and the subdiagonal elements of a31 lie outside the band.
                 i2 = min( kl-jb, m-j-jb+1 )
                 i3 = min( jb, m-j-kl+1 )
                 ! j2 and j3 are computed after ju has been updated.
                 ! factorize the current block of jb columns
                 loop_80: do jj = j, j + jb - 1
                    ! set fill-in elements in column jj+kv to czero
                    if( jj+kv<=n ) then
                       do i = 1, kl
                          ab( i, jj+kv ) = czero
                       end do
                    end if
                    ! find pivot and test for singularity. km is the number of
                    ! subdiagonal elements in the current column.
                    km = min( kl, m-jj )
                    jp = stdlib_izamax( km+1, ab( kv+1, jj ), 1_ilp )
                    ipiv( jj ) = jp + jj - j
                    if( ab( kv+jp, jj )/=czero ) then
                       ju = max( ju, min( jj+ku+jp-1, n ) )
                       if( jp/=1_ilp ) then
                          ! apply interchange to columns j to j+jb-1
                          if( jp+jj-1<j+kl ) then
                             call stdlib_zswap( jb, ab( kv+1+jj-j, j ), ldab-1,ab( kv+jp+jj-j, j )&
                                       , ldab-1 )
                          else
                             ! the interchange affects columns j to jj-1 of a31
                             ! which are stored in the work array work31
                             call stdlib_zswap( jj-j, ab( kv+1+jj-j, j ), ldab-1,work31( jp+jj-j-&
                                       kl, 1_ilp ), ldwork )
                             call stdlib_zswap( j+jb-jj, ab( kv+1, jj ), ldab-1,ab( kv+jp, jj ), &
                                       ldab-1 )
                          end if
                       end if
                       ! compute multipliers
                       call stdlib_zscal( km, cone / ab( kv+1, jj ), ab( kv+2, jj ),1_ilp )
                       ! update trailing submatrix within the band and within
                       ! the current block. jm is the index of the last column
                       ! which needs to be updated.
                       jm = min( ju, j+jb-1 )
                       if( jm>jj )call stdlib_zgeru( km, jm-jj, -cone, ab( kv+2, jj ), 1_ilp,ab( kv, &
                                 jj+1 ), ldab-1,ab( kv+1, jj+1 ), ldab-1 )
                    else
                       ! if pivot is czero, set info to the index of the pivot
                       ! unless a czero pivot has already been found.
                       if( info==0_ilp )info = jj
                    end if
                    ! copy current column of a31 into the work array work31
                    nw = min( jj-j+1, i3 )
                    if( nw>0_ilp )call stdlib_zcopy( nw, ab( kv+kl+1-jj+j, jj ), 1_ilp,work31( 1_ilp, jj-j+1 )&
                              , 1_ilp )
                 end do loop_80
                 if( j+jb<=n ) then
                    ! apply the row interchanges to the other blocks.
                    j2 = min( ju-j+1, kv ) - jb
                    j3 = max( 0_ilp, ju-j-kv+1 )
                    ! use stdlib_zlaswp to apply the row interchanges to a12, a22, and
                    ! a32.
                    call stdlib_zlaswp( j2, ab( kv+1-jb, j+jb ), ldab-1, 1_ilp, jb,ipiv( j ), 1_ilp )
                              
                    ! adjust the pivot indices.
                    do i = j, j + jb - 1
                       ipiv( i ) = ipiv( i ) + j - 1_ilp
                    end do
                    ! apply the row interchanges to a13, a23, and a33
                    ! columnwise.
                    k2 = j - 1_ilp + jb + j2
                    do i = 1, j3
                       jj = k2 + i
                       do ii = j + i - 1, j + jb - 1
                          ip = ipiv( ii )
                          if( ip/=ii ) then
                             temp = ab( kv+1+ii-jj, jj )
                             ab( kv+1+ii-jj, jj ) = ab( kv+1+ip-jj, jj )
                             ab( kv+1+ip-jj, jj ) = temp
                          end if
                       end do
                    end do
                    ! update the relevant part of the trailing submatrix
                    if( j2>0_ilp ) then
                       ! update a12
                       call stdlib_ztrsm( 'LEFT', 'LOWER', 'NO TRANSPOSE', 'UNIT',jb, j2, cone, &
                                 ab( kv+1, j ), ldab-1,ab( kv+1-jb, j+jb ), ldab-1 )
                       if( i2>0_ilp ) then
                          ! update a22
                          call stdlib_zgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', i2, j2,jb, -cone, ab(&
                           kv+1+jb, j ), ldab-1,ab( kv+1-jb, j+jb ), ldab-1, cone,ab( kv+1, j+jb )&
                                     , ldab-1 )
                       end if
                       if( i3>0_ilp ) then
                          ! update a32
                          call stdlib_zgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', i3, j2,jb, -cone, &
                          work31, ldwork,ab( kv+1-jb, j+jb ), ldab-1, cone,ab( kv+kl+1-jb, j+jb ),&
                                     ldab-1 )
                       end if
                    end if
                    if( j3>0_ilp ) then
                       ! copy the lower triangle of a13 into the work array
                       ! work13
                       do jj = 1, j3
                          do ii = jj, jb
                             work13( ii, jj ) = ab( ii-jj+1, jj+j+kv-1 )
                          end do
                       end do
                       ! update a13 in the work array
                       call stdlib_ztrsm( 'LEFT', 'LOWER', 'NO TRANSPOSE', 'UNIT',jb, j3, cone, &
                                 ab( kv+1, j ), ldab-1,work13, ldwork )
                       if( i2>0_ilp ) then
                          ! update a23
                          call stdlib_zgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', i2, j3,jb, -cone, ab(&
                           kv+1+jb, j ), ldab-1,work13, ldwork, cone, ab( 1_ilp+jb, j+kv ),ldab-1 )
                                     
                       end if
                       if( i3>0_ilp ) then
                          ! update a33
                          call stdlib_zgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', i3, j3,jb, -cone, &
                                    work31, ldwork, work13,ldwork, cone, ab( 1_ilp+kl, j+kv ), ldab-1 )
                       end if
                       ! copy the lower triangle of a13 back into place
                       do jj = 1, j3
                          do ii = jj, jb
                             ab( ii-jj+1, jj+j+kv-1 ) = work13( ii, jj )
                          end do
                       end do
                    end if
                 else
                    ! adjust the pivot indices.
                    do i = j, j + jb - 1
                       ipiv( i ) = ipiv( i ) + j - 1_ilp
                    end do
                 end if
                 ! partially undo the interchanges in the current block to
                 ! restore the upper triangular form of a31 and copy the upper
                 ! triangle of a31 back into place
                 do jj = j + jb - 1, j, -1
                    jp = ipiv( jj ) - jj + 1_ilp
                    if( jp/=1_ilp ) then
                       ! apply interchange to columns j to jj-1
                       if( jp+jj-1<j+kl ) then
                          ! the interchange does not affect a31
                          call stdlib_zswap( jj-j, ab( kv+1+jj-j, j ), ldab-1,ab( kv+jp+jj-j, j ),&
                                     ldab-1 )
                       else
                          ! the interchange does affect a31
                          call stdlib_zswap( jj-j, ab( kv+1+jj-j, j ), ldab-1,work31( jp+jj-j-kl, &
                                    1_ilp ), ldwork )
                       end if
                    end if
                    ! copy the current column of a31 back into place
                    nw = min( i3, jj-j+1 )
                    if( nw>0_ilp )call stdlib_zcopy( nw, work31( 1_ilp, jj-j+1 ), 1_ilp,ab( kv+kl+1-jj+j, jj )&
                              , 1_ilp )
                 end do
              end do loop_180
           end if
           return
     end subroutine stdlib_zgbtrf




     pure module subroutine stdlib_sgbtf2( m, n, kl, ku, ab, ldab, ipiv, info )
     !! SGBTF2 computes an LU factorization of a real m-by-n band matrix A
     !! using partial pivoting with row interchanges.
     !! This is the unblocked version of the algorithm, calling Level 2 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, ldab, m, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(sp), intent(inout) :: ab(ldab,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, jp, ju, km, kv
           ! Intrinsic Functions 
           ! Executable Statements 
           ! kv is the number of superdiagonals in the factor u, allowing for
           ! fill-in.
           kv = ku + kl
           ! test the input parameters.
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kl<0_ilp ) then
              info = -3_ilp
           else if( ku<0_ilp ) then
              info = -4_ilp
           else if( ldab<kl+kv+1 ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGBTF2', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 )return
           ! gaussian elimination with partial pivoting
           ! set fill-in elements in columns ku+2 to kv to zero.
           do j = ku + 2, min( kv, n )
              do i = kv - j + 2, kl
                 ab( i, j ) = zero
              end do
           end do
           ! ju is the index of the last column affected by the current stage
           ! of the factorization.
           ju = 1_ilp
           loop_40: do j = 1, min( m, n )
              ! set fill-in elements in column j+kv to zero.
              if( j+kv<=n ) then
                 do i = 1, kl
                    ab( i, j+kv ) = zero
                 end do
              end if
              ! find pivot and test for singularity. km is the number of
              ! subdiagonal elements in the current column.
              km = min( kl, m-j )
              jp = stdlib_isamax( km+1, ab( kv+1, j ), 1_ilp )
              ipiv( j ) = jp + j - 1_ilp
              if( ab( kv+jp, j )/=zero ) then
                 ju = max( ju, min( j+ku+jp-1, n ) )
                 ! apply interchange to columns j to ju.
                 if( jp/=1_ilp )call stdlib_sswap( ju-j+1, ab( kv+jp, j ), ldab-1,ab( kv+1, j ), ldab-&
                           1_ilp )
                 if( km>0_ilp ) then
                    ! compute multipliers.
                    call stdlib_sscal( km, one / ab( kv+1, j ), ab( kv+2, j ), 1_ilp )
                    ! update trailing submatrix within the band.
                    if( ju>j )call stdlib_sger( km, ju-j, -one, ab( kv+2, j ), 1_ilp,ab( kv, j+1 ), &
                              ldab-1, ab( kv+1, j+1 ),ldab-1 )
                 end if
              else
                 ! if pivot is zero, set info to the index of the pivot
                 ! unless a zero pivot has already been found.
                 if( info==0_ilp )info = j
              end if
           end do loop_40
           return
     end subroutine stdlib_sgbtf2

     pure module subroutine stdlib_dgbtf2( m, n, kl, ku, ab, ldab, ipiv, info )
     !! DGBTF2 computes an LU factorization of a real m-by-n band matrix A
     !! using partial pivoting with row interchanges.
     !! This is the unblocked version of the algorithm, calling Level 2 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, ldab, m, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(dp), intent(inout) :: ab(ldab,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, jp, ju, km, kv
           ! Intrinsic Functions 
           ! Executable Statements 
           ! kv is the number of superdiagonals in the factor u, allowing for
           ! fill-in.
           kv = ku + kl
           ! test the input parameters.
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kl<0_ilp ) then
              info = -3_ilp
           else if( ku<0_ilp ) then
              info = -4_ilp
           else if( ldab<kl+kv+1 ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGBTF2', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 )return
           ! gaussian elimination with partial pivoting
           ! set fill-in elements in columns ku+2 to kv to zero.
           do j = ku + 2, min( kv, n )
              do i = kv - j + 2, kl
                 ab( i, j ) = zero
              end do
           end do
           ! ju is the index of the last column affected by the current stage
           ! of the factorization.
           ju = 1_ilp
           loop_40: do j = 1, min( m, n )
              ! set fill-in elements in column j+kv to zero.
              if( j+kv<=n ) then
                 do i = 1, kl
                    ab( i, j+kv ) = zero
                 end do
              end if
              ! find pivot and test for singularity. km is the number of
              ! subdiagonal elements in the current column.
              km = min( kl, m-j )
              jp = stdlib_idamax( km+1, ab( kv+1, j ), 1_ilp )
              ipiv( j ) = jp + j - 1_ilp
              if( ab( kv+jp, j )/=zero ) then
                 ju = max( ju, min( j+ku+jp-1, n ) )
                 ! apply interchange to columns j to ju.
                 if( jp/=1_ilp )call stdlib_dswap( ju-j+1, ab( kv+jp, j ), ldab-1,ab( kv+1, j ), ldab-&
                           1_ilp )
                 if( km>0_ilp ) then
                    ! compute multipliers.
                    call stdlib_dscal( km, one / ab( kv+1, j ), ab( kv+2, j ), 1_ilp )
                    ! update trailing submatrix within the band.
                    if( ju>j )call stdlib_dger( km, ju-j, -one, ab( kv+2, j ), 1_ilp,ab( kv, j+1 ), &
                              ldab-1, ab( kv+1, j+1 ),ldab-1 )
                 end if
              else
                 ! if pivot is zero, set info to the index of the pivot
                 ! unless a zero pivot has already been found.
                 if( info==0_ilp )info = j
              end if
           end do loop_40
           return
     end subroutine stdlib_dgbtf2


     pure module subroutine stdlib_cgbtf2( m, n, kl, ku, ab, ldab, ipiv, info )
     !! CGBTF2 computes an LU factorization of a complex m-by-n band matrix
     !! A using partial pivoting with row interchanges.
     !! This is the unblocked version of the algorithm, calling Level 2 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, ldab, m, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(sp), intent(inout) :: ab(ldab,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, jp, ju, km, kv
           ! Intrinsic Functions 
           ! Executable Statements 
           ! kv is the number of superdiagonals in the factor u, allowing for
           ! fill-in.
           kv = ku + kl
           ! test the input parameters.
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kl<0_ilp ) then
              info = -3_ilp
           else if( ku<0_ilp ) then
              info = -4_ilp
           else if( ldab<kl+kv+1 ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGBTF2', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 )return
           ! gaussian elimination with partial pivoting
           ! set fill-in elements in columns ku+2 to kv to czero.
           do j = ku + 2, min( kv, n )
              do i = kv - j + 2, kl
                 ab( i, j ) = czero
              end do
           end do
           ! ju is the index of the last column affected by the current stage
           ! of the factorization.
           ju = 1_ilp
           loop_40: do j = 1, min( m, n )
              ! set fill-in elements in column j+kv to czero.
              if( j+kv<=n ) then
                 do i = 1, kl
                    ab( i, j+kv ) = czero
                 end do
              end if
              ! find pivot and test for singularity. km is the number of
              ! subdiagonal elements in the current column.
              km = min( kl, m-j )
              jp = stdlib_icamax( km+1, ab( kv+1, j ), 1_ilp )
              ipiv( j ) = jp + j - 1_ilp
              if( ab( kv+jp, j )/=czero ) then
                 ju = max( ju, min( j+ku+jp-1, n ) )
                 ! apply interchange to columns j to ju.
                 if( jp/=1_ilp )call stdlib_cswap( ju-j+1, ab( kv+jp, j ), ldab-1,ab( kv+1, j ), ldab-&
                           1_ilp )
                 if( km>0_ilp ) then
                    ! compute multipliers.
                    call stdlib_cscal( km, cone / ab( kv+1, j ), ab( kv+2, j ), 1_ilp )
                    ! update trailing submatrix within the band.
                    if( ju>j )call stdlib_cgeru( km, ju-j, -cone, ab( kv+2, j ), 1_ilp,ab( kv, j+1 ), &
                              ldab-1, ab( kv+1, j+1 ),ldab-1 )
                 end if
              else
                 ! if pivot is czero, set info to the index of the pivot
                 ! unless a czero pivot has already been found.
                 if( info==0_ilp )info = j
              end if
           end do loop_40
           return
     end subroutine stdlib_cgbtf2

     pure module subroutine stdlib_zgbtf2( m, n, kl, ku, ab, ldab, ipiv, info )
     !! ZGBTF2 computes an LU factorization of a complex m-by-n band matrix
     !! A using partial pivoting with row interchanges.
     !! This is the unblocked version of the algorithm, calling Level 2 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, ldab, m, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(dp), intent(inout) :: ab(ldab,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, jp, ju, km, kv
           ! Intrinsic Functions 
           ! Executable Statements 
           ! kv is the number of superdiagonals in the factor u, allowing for
           ! fill-in.
           kv = ku + kl
           ! test the input parameters.
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kl<0_ilp ) then
              info = -3_ilp
           else if( ku<0_ilp ) then
              info = -4_ilp
           else if( ldab<kl+kv+1 ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGBTF2', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 )return
           ! gaussian elimination with partial pivoting
           ! set fill-in elements in columns ku+2 to kv to czero.
           do j = ku + 2, min( kv, n )
              do i = kv - j + 2, kl
                 ab( i, j ) = czero
              end do
           end do
           ! ju is the index of the last column affected by the current stage
           ! of the factorization.
           ju = 1_ilp
           loop_40: do j = 1, min( m, n )
              ! set fill-in elements in column j+kv to czero.
              if( j+kv<=n ) then
                 do i = 1, kl
                    ab( i, j+kv ) = czero
                 end do
              end if
              ! find pivot and test for singularity. km is the number of
              ! subdiagonal elements in the current column.
              km = min( kl, m-j )
              jp = stdlib_izamax( km+1, ab( kv+1, j ), 1_ilp )
              ipiv( j ) = jp + j - 1_ilp
              if( ab( kv+jp, j )/=czero ) then
                 ju = max( ju, min( j+ku+jp-1, n ) )
                 ! apply interchange to columns j to ju.
                 if( jp/=1_ilp )call stdlib_zswap( ju-j+1, ab( kv+jp, j ), ldab-1,ab( kv+1, j ), ldab-&
                           1_ilp )
                 if( km>0_ilp ) then
                    ! compute multipliers.
                    call stdlib_zscal( km, cone / ab( kv+1, j ), ab( kv+2, j ), 1_ilp )
                    ! update trailing submatrix within the band.
                    if( ju>j )call stdlib_zgeru( km, ju-j, -cone, ab( kv+2, j ), 1_ilp,ab( kv, j+1 ), &
                              ldab-1, ab( kv+1, j+1 ),ldab-1 )
                 end if
              else
                 ! if pivot is czero, set info to the index of the pivot
                 ! unless a czero pivot has already been found.
                 if( info==0_ilp )info = j
              end if
           end do loop_40
           return
     end subroutine stdlib_zgbtf2




     pure module subroutine stdlib_sgbtrs( trans, n, kl, ku, nrhs, ab, ldab, ipiv, b, ldb,info )
     !! SGBTRS solves a system of linear equations
     !! A * X = B  or  A**T * X = B
     !! with a general band matrix A using the LU factorization computed
     !! by SGBTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, ldab, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(sp), intent(in) :: ab(ldab,*)
           real(sp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lnoti, notran
           integer(ilp) :: i, j, kd, l, lm
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           notran = stdlib_lsame( trans, 'N' )
           if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( trans, &
                     'C' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kl<0_ilp ) then
              info = -3_ilp
           else if( ku<0_ilp ) then
              info = -4_ilp
           else if( nrhs<0_ilp ) then
              info = -5_ilp
           else if( ldab<( 2_ilp*kl+ku+1 ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGBTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           kd = ku + kl + 1_ilp
           lnoti = kl>0_ilp
           if( notran ) then
              ! solve  a*x = b.
              ! solve l*x = b, overwriting b with x.
              ! l is represented as a product of permutations and unit lower
              ! triangular matrices l = p(1) * l(1) * ... * p(n-1) * l(n-1),
              ! where each transformation l(i) is a rank-one modification of
              ! the identity matrix.
              if( lnoti ) then
                 do j = 1, n - 1
                    lm = min( kl, n-j )
                    l = ipiv( j )
                    if( l/=j )call stdlib_sswap( nrhs, b( l, 1_ilp ), ldb, b( j, 1_ilp ), ldb )
                    call stdlib_sger( lm, nrhs, -one, ab( kd+1, j ), 1_ilp, b( j, 1_ilp ),ldb, b( j+1, 1_ilp )&
                              , ldb )
                 end do
              end if
              do i = 1, nrhs
                 ! solve u*x = b, overwriting b with x.
                 call stdlib_stbsv( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', n, kl+ku,ab, ldab, b( 1_ilp, &
                           i ), 1_ilp )
              end do
           else
              ! solve a**t*x = b.
              do i = 1, nrhs
                 ! solve u**t*x = b, overwriting b with x.
                 call stdlib_stbsv( 'UPPER', 'TRANSPOSE', 'NON-UNIT', n, kl+ku, ab,ldab, b( 1_ilp, i )&
                           , 1_ilp )
              end do
              ! solve l**t*x = b, overwriting b with x.
              if( lnoti ) then
                 do j = n - 1, 1, -1
                    lm = min( kl, n-j )
                    call stdlib_sgemv( 'TRANSPOSE', lm, nrhs, -one, b( j+1, 1_ilp ),ldb, ab( kd+1, j )&
                              , 1_ilp, one, b( j, 1_ilp ), ldb )
                    l = ipiv( j )
                    if( l/=j )call stdlib_sswap( nrhs, b( l, 1_ilp ), ldb, b( j, 1_ilp ), ldb )
                 end do
              end if
           end if
           return
     end subroutine stdlib_sgbtrs

     pure module subroutine stdlib_dgbtrs( trans, n, kl, ku, nrhs, ab, ldab, ipiv, b, ldb,info )
     !! DGBTRS solves a system of linear equations
     !! A * X = B  or  A**T * X = B
     !! with a general band matrix A using the LU factorization computed
     !! by DGBTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, ldab, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(dp), intent(in) :: ab(ldab,*)
           real(dp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lnoti, notran
           integer(ilp) :: i, j, kd, l, lm
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           notran = stdlib_lsame( trans, 'N' )
           if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( trans, &
                     'C' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kl<0_ilp ) then
              info = -3_ilp
           else if( ku<0_ilp ) then
              info = -4_ilp
           else if( nrhs<0_ilp ) then
              info = -5_ilp
           else if( ldab<( 2_ilp*kl+ku+1 ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGBTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           kd = ku + kl + 1_ilp
           lnoti = kl>0_ilp
           if( notran ) then
              ! solve  a*x = b.
              ! solve l*x = b, overwriting b with x.
              ! l is represented as a product of permutations and unit lower
              ! triangular matrices l = p(1) * l(1) * ... * p(n-1) * l(n-1),
              ! where each transformation l(i) is a rank-one modification of
              ! the identity matrix.
              if( lnoti ) then
                 do j = 1, n - 1
                    lm = min( kl, n-j )
                    l = ipiv( j )
                    if( l/=j )call stdlib_dswap( nrhs, b( l, 1_ilp ), ldb, b( j, 1_ilp ), ldb )
                    call stdlib_dger( lm, nrhs, -one, ab( kd+1, j ), 1_ilp, b( j, 1_ilp ),ldb, b( j+1, 1_ilp )&
                              , ldb )
                 end do
              end if
              do i = 1, nrhs
                 ! solve u*x = b, overwriting b with x.
                 call stdlib_dtbsv( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', n, kl+ku,ab, ldab, b( 1_ilp, &
                           i ), 1_ilp )
              end do
           else
              ! solve a**t*x = b.
              do i = 1, nrhs
                 ! solve u**t*x = b, overwriting b with x.
                 call stdlib_dtbsv( 'UPPER', 'TRANSPOSE', 'NON-UNIT', n, kl+ku, ab,ldab, b( 1_ilp, i )&
                           , 1_ilp )
              end do
              ! solve l**t*x = b, overwriting b with x.
              if( lnoti ) then
                 do j = n - 1, 1, -1
                    lm = min( kl, n-j )
                    call stdlib_dgemv( 'TRANSPOSE', lm, nrhs, -one, b( j+1, 1_ilp ),ldb, ab( kd+1, j )&
                              , 1_ilp, one, b( j, 1_ilp ), ldb )
                    l = ipiv( j )
                    if( l/=j )call stdlib_dswap( nrhs, b( l, 1_ilp ), ldb, b( j, 1_ilp ), ldb )
                 end do
              end if
           end if
           return
     end subroutine stdlib_dgbtrs


     pure module subroutine stdlib_cgbtrs( trans, n, kl, ku, nrhs, ab, ldab, ipiv, b, ldb,info )
     !! CGBTRS solves a system of linear equations
     !! A * X = B,  A**T * X = B,  or  A**H * X = B
     !! with a general band matrix A using the LU factorization computed
     !! by CGBTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, ldab, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(sp), intent(in) :: ab(ldab,*)
           complex(sp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lnoti, notran
           integer(ilp) :: i, j, kd, l, lm
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           notran = stdlib_lsame( trans, 'N' )
           if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( trans, &
                     'C' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kl<0_ilp ) then
              info = -3_ilp
           else if( ku<0_ilp ) then
              info = -4_ilp
           else if( nrhs<0_ilp ) then
              info = -5_ilp
           else if( ldab<( 2_ilp*kl+ku+1 ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGBTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           kd = ku + kl + 1_ilp
           lnoti = kl>0_ilp
           if( notran ) then
              ! solve  a*x = b.
              ! solve l*x = b, overwriting b with x.
              ! l is represented as a product of permutations and unit lower
              ! triangular matrices l = p(1) * l(1) * ... * p(n-1) * l(n-1),
              ! where each transformation l(i) is a rank-cone modification of
              ! the identity matrix.
              if( lnoti ) then
                 do j = 1, n - 1
                    lm = min( kl, n-j )
                    l = ipiv( j )
                    if( l/=j )call stdlib_cswap( nrhs, b( l, 1_ilp ), ldb, b( j, 1_ilp ), ldb )
                    call stdlib_cgeru( lm, nrhs, -cone, ab( kd+1, j ), 1_ilp, b( j, 1_ilp ),ldb, b( j+1, &
                              1_ilp ), ldb )
                 end do
              end if
              do i = 1, nrhs
                 ! solve u*x = b, overwriting b with x.
                 call stdlib_ctbsv( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', n, kl+ku,ab, ldab, b( 1_ilp, &
                           i ), 1_ilp )
              end do
           else if( stdlib_lsame( trans, 'T' ) ) then
              ! solve a**t * x = b.
              do i = 1, nrhs
                 ! solve u**t * x = b, overwriting b with x.
                 call stdlib_ctbsv( 'UPPER', 'TRANSPOSE', 'NON-UNIT', n, kl+ku, ab,ldab, b( 1_ilp, i )&
                           , 1_ilp )
              end do
              ! solve l**t * x = b, overwriting b with x.
              if( lnoti ) then
                 do j = n - 1, 1, -1
                    lm = min( kl, n-j )
                    call stdlib_cgemv( 'TRANSPOSE', lm, nrhs, -cone, b( j+1, 1_ilp ),ldb, ab( kd+1, j &
                              ), 1_ilp, cone, b( j, 1_ilp ), ldb )
                    l = ipiv( j )
                    if( l/=j )call stdlib_cswap( nrhs, b( l, 1_ilp ), ldb, b( j, 1_ilp ), ldb )
                 end do
              end if
           else
              ! solve a**h * x = b.
              do i = 1, nrhs
                 ! solve u**h * x = b, overwriting b with x.
                 call stdlib_ctbsv( 'UPPER', 'CONJUGATE TRANSPOSE', 'NON-UNIT', n,kl+ku, ab, ldab,&
                            b( 1_ilp, i ), 1_ilp )
              end do
              ! solve l**h * x = b, overwriting b with x.
              if( lnoti ) then
                 do j = n - 1, 1, -1
                    lm = min( kl, n-j )
                    call stdlib_clacgv( nrhs, b( j, 1_ilp ), ldb )
                    call stdlib_cgemv( 'CONJUGATE TRANSPOSE', lm, nrhs, -cone,b( j+1, 1_ilp ), ldb, &
                              ab( kd+1, j ), 1_ilp, cone,b( j, 1_ilp ), ldb )
                    call stdlib_clacgv( nrhs, b( j, 1_ilp ), ldb )
                    l = ipiv( j )
                    if( l/=j )call stdlib_cswap( nrhs, b( l, 1_ilp ), ldb, b( j, 1_ilp ), ldb )
                 end do
              end if
           end if
           return
     end subroutine stdlib_cgbtrs

     pure module subroutine stdlib_zgbtrs( trans, n, kl, ku, nrhs, ab, ldab, ipiv, b, ldb,info )
     !! ZGBTRS solves a system of linear equations
     !! A * X = B,  A**T * X = B,  or  A**H * X = B
     !! with a general band matrix A using the LU factorization computed
     !! by ZGBTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, ldab, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(dp), intent(in) :: ab(ldab,*)
           complex(dp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lnoti, notran
           integer(ilp) :: i, j, kd, l, lm
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           notran = stdlib_lsame( trans, 'N' )
           if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( trans, &
                     'C' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kl<0_ilp ) then
              info = -3_ilp
           else if( ku<0_ilp ) then
              info = -4_ilp
           else if( nrhs<0_ilp ) then
              info = -5_ilp
           else if( ldab<( 2_ilp*kl+ku+1 ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGBTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           kd = ku + kl + 1_ilp
           lnoti = kl>0_ilp
           if( notran ) then
              ! solve  a*x = b.
              ! solve l*x = b, overwriting b with x.
              ! l is represented as a product of permutations and unit lower
              ! triangular matrices l = p(1) * l(1) * ... * p(n-1) * l(n-1),
              ! where each transformation l(i) is a rank-cone modification of
              ! the identity matrix.
              if( lnoti ) then
                 do j = 1, n - 1
                    lm = min( kl, n-j )
                    l = ipiv( j )
                    if( l/=j )call stdlib_zswap( nrhs, b( l, 1_ilp ), ldb, b( j, 1_ilp ), ldb )
                    call stdlib_zgeru( lm, nrhs, -cone, ab( kd+1, j ), 1_ilp, b( j, 1_ilp ),ldb, b( j+1, &
                              1_ilp ), ldb )
                 end do
              end if
              do i = 1, nrhs
                 ! solve u*x = b, overwriting b with x.
                 call stdlib_ztbsv( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', n, kl+ku,ab, ldab, b( 1_ilp, &
                           i ), 1_ilp )
              end do
           else if( stdlib_lsame( trans, 'T' ) ) then
              ! solve a**t * x = b.
              do i = 1, nrhs
                 ! solve u**t * x = b, overwriting b with x.
                 call stdlib_ztbsv( 'UPPER', 'TRANSPOSE', 'NON-UNIT', n, kl+ku, ab,ldab, b( 1_ilp, i )&
                           , 1_ilp )
              end do
              ! solve l**t * x = b, overwriting b with x.
              if( lnoti ) then
                 do j = n - 1, 1, -1
                    lm = min( kl, n-j )
                    call stdlib_zgemv( 'TRANSPOSE', lm, nrhs, -cone, b( j+1, 1_ilp ),ldb, ab( kd+1, j &
                              ), 1_ilp, cone, b( j, 1_ilp ), ldb )
                    l = ipiv( j )
                    if( l/=j )call stdlib_zswap( nrhs, b( l, 1_ilp ), ldb, b( j, 1_ilp ), ldb )
                 end do
              end if
           else
              ! solve a**h * x = b.
              do i = 1, nrhs
                 ! solve u**h * x = b, overwriting b with x.
                 call stdlib_ztbsv( 'UPPER', 'CONJUGATE TRANSPOSE', 'NON-UNIT', n,kl+ku, ab, ldab,&
                            b( 1_ilp, i ), 1_ilp )
              end do
              ! solve l**h * x = b, overwriting b with x.
              if( lnoti ) then
                 do j = n - 1, 1, -1
                    lm = min( kl, n-j )
                    call stdlib_zlacgv( nrhs, b( j, 1_ilp ), ldb )
                    call stdlib_zgemv( 'CONJUGATE TRANSPOSE', lm, nrhs, -cone,b( j+1, 1_ilp ), ldb, &
                              ab( kd+1, j ), 1_ilp, cone,b( j, 1_ilp ), ldb )
                    call stdlib_zlacgv( nrhs, b( j, 1_ilp ), ldb )
                    l = ipiv( j )
                    if( l/=j )call stdlib_zswap( nrhs, b( l, 1_ilp ), ldb, b( j, 1_ilp ), ldb )
                 end do
              end if
           end if
           return
     end subroutine stdlib_zgbtrs




     pure module subroutine stdlib_sgbrfs( trans, n, kl, ku, nrhs, ab, ldab, afb, ldafb,ipiv, b, ldb, x, &
     !! SGBRFS improves the computed solution to a system of linear
     !! equations when the coefficient matrix is banded, and provides
     !! error bounds and backward error estimates for the solution.
               ldx, ferr, berr, work, iwork,info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, ldab, ldafb, ldb, ldx, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(in) :: ab(ldab,*), afb(ldafb,*), b(ldb,*)
           real(sp), intent(out) :: berr(*), ferr(*), work(*)
           real(sp), intent(inout) :: x(ldx,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           
           
           
           ! Local Scalars 
           logical(lk) :: notran
           character :: transt
           integer(ilp) :: count, i, j, k, kase, kk, nz
           real(sp) :: eps, lstres, s, safe1, safe2, safmin, xk
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           notran = stdlib_lsame( trans, 'N' )
           if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( trans, &
                     'C' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kl<0_ilp ) then
              info = -3_ilp
           else if( ku<0_ilp ) then
              info = -4_ilp
           else if( nrhs<0_ilp ) then
              info = -5_ilp
           else if( ldab<kl+ku+1 ) then
              info = -7_ilp
           else if( ldafb<2_ilp*kl+ku+1 ) then
              info = -9_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -12_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -14_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGBRFS', -info )
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
           if( notran ) then
              transt = 'T'
           else
              transt = 'N'
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = min( kl+ku+2, n+1 )
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
              ! compute residual r = b - op(a) * x,
              ! where op(a) = a, a**t, or a**h, depending on trans.
              call stdlib_scopy( n, b( 1_ilp, j ), 1_ilp, work( n+1 ), 1_ilp )
              call stdlib_sgbmv( trans, n, n, kl, ku, -one, ab, ldab, x( 1_ilp, j ), 1_ilp,one, work( n+1 &
                        ), 1_ilp )
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(op(a))*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              do i = 1, n
                 work( i ) = abs( b( i, j ) )
              end do
              ! compute abs(op(a))*abs(x) + abs(b).
              if( notran ) then
                 do k = 1, n
                    kk = ku + 1_ilp - k
                    xk = abs( x( k, j ) )
                    do i = max( 1, k-ku ), min( n, k+kl )
                       work( i ) = work( i ) + abs( ab( kk+i, k ) )*xk
                    end do
                 end do
              else
                 do k = 1, n
                    s = zero
                    kk = ku + 1_ilp - k
                    do i = max( 1, k-ku ), min( n, k+kl )
                       s = s + abs( ab( kk+i, k ) )*abs( x( i, j ) )
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
                 call stdlib_sgbtrs( trans, n, kl, ku, 1_ilp, afb, ldafb, ipiv,work( n+1 ), n, info )
                           
                 call stdlib_saxpy( n, one, work( n+1 ), 1_ilp, x( 1_ilp, j ), 1_ilp )
                 lstres = berr( j )
                 count = count + 1_ilp
                 go to 20
              end if
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(op(a)))*
                 ! ( abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(op(a)) is the inverse of op(a)
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(op(a))*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(op(a))*abs(x) + abs(b) is less than safe2.
              ! use stdlib_slacn2 to estimate the infinity-norm of the matrix
                 ! inv(op(a)) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) )))
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
                    ! multiply by diag(w)*inv(op(a)**t).
                    call stdlib_sgbtrs( transt, n, kl, ku, 1_ilp, afb, ldafb, ipiv,work( n+1 ), n, &
                              info )
                    do i = 1, n
                       work( n+i ) = work( n+i )*work( i )
                    end do
                 else
                    ! multiply by inv(op(a))*diag(w).
                    do i = 1, n
                       work( n+i ) = work( n+i )*work( i )
                    end do
                    call stdlib_sgbtrs( trans, n, kl, ku, 1_ilp, afb, ldafb, ipiv,work( n+1 ), n, &
                              info )
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
     end subroutine stdlib_sgbrfs

     pure module subroutine stdlib_dgbrfs( trans, n, kl, ku, nrhs, ab, ldab, afb, ldafb,ipiv, b, ldb, x, &
     !! DGBRFS improves the computed solution to a system of linear
     !! equations when the coefficient matrix is banded, and provides
     !! error bounds and backward error estimates for the solution.
               ldx, ferr, berr, work, iwork,info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, ldab, ldafb, ldb, ldx, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(in) :: ab(ldab,*), afb(ldafb,*), b(ldb,*)
           real(dp), intent(out) :: berr(*), ferr(*), work(*)
           real(dp), intent(inout) :: x(ldx,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           
           
           
           ! Local Scalars 
           logical(lk) :: notran
           character :: transt
           integer(ilp) :: count, i, j, k, kase, kk, nz
           real(dp) :: eps, lstres, s, safe1, safe2, safmin, xk
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           notran = stdlib_lsame( trans, 'N' )
           if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( trans, &
                     'C' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kl<0_ilp ) then
              info = -3_ilp
           else if( ku<0_ilp ) then
              info = -4_ilp
           else if( nrhs<0_ilp ) then
              info = -5_ilp
           else if( ldab<kl+ku+1 ) then
              info = -7_ilp
           else if( ldafb<2_ilp*kl+ku+1 ) then
              info = -9_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -12_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -14_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGBRFS', -info )
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
           if( notran ) then
              transt = 'T'
           else
              transt = 'N'
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = min( kl+ku+2, n+1 )
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
              ! compute residual r = b - op(a) * x,
              ! where op(a) = a, a**t, or a**h, depending on trans.
              call stdlib_dcopy( n, b( 1_ilp, j ), 1_ilp, work( n+1 ), 1_ilp )
              call stdlib_dgbmv( trans, n, n, kl, ku, -one, ab, ldab, x( 1_ilp, j ), 1_ilp,one, work( n+1 &
                        ), 1_ilp )
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(op(a))*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              do i = 1, n
                 work( i ) = abs( b( i, j ) )
              end do
              ! compute abs(op(a))*abs(x) + abs(b).
              if( notran ) then
                 do k = 1, n
                    kk = ku + 1_ilp - k
                    xk = abs( x( k, j ) )
                    do i = max( 1, k-ku ), min( n, k+kl )
                       work( i ) = work( i ) + abs( ab( kk+i, k ) )*xk
                    end do
                 end do
              else
                 do k = 1, n
                    s = zero
                    kk = ku + 1_ilp - k
                    do i = max( 1, k-ku ), min( n, k+kl )
                       s = s + abs( ab( kk+i, k ) )*abs( x( i, j ) )
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
                 call stdlib_dgbtrs( trans, n, kl, ku, 1_ilp, afb, ldafb, ipiv,work( n+1 ), n, info )
                           
                 call stdlib_daxpy( n, one, work( n+1 ), 1_ilp, x( 1_ilp, j ), 1_ilp )
                 lstres = berr( j )
                 count = count + 1_ilp
                 go to 20
              end if
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(op(a)))*
                 ! ( abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(op(a)) is the inverse of op(a)
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(op(a))*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(op(a))*abs(x) + abs(b) is less than safe2.
              ! use stdlib_dlacn2 to estimate the infinity-norm of the matrix
                 ! inv(op(a)) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) )))
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
                    ! multiply by diag(w)*inv(op(a)**t).
                    call stdlib_dgbtrs( transt, n, kl, ku, 1_ilp, afb, ldafb, ipiv,work( n+1 ), n, &
                              info )
                    do i = 1, n
                       work( n+i ) = work( n+i )*work( i )
                    end do
                 else
                    ! multiply by inv(op(a))*diag(w).
                    do i = 1, n
                       work( n+i ) = work( n+i )*work( i )
                    end do
                    call stdlib_dgbtrs( trans, n, kl, ku, 1_ilp, afb, ldafb, ipiv,work( n+1 ), n, &
                              info )
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
     end subroutine stdlib_dgbrfs


     pure module subroutine stdlib_cgbrfs( trans, n, kl, ku, nrhs, ab, ldab, afb, ldafb,ipiv, b, ldb, x, &
     !! CGBRFS improves the computed solution to a system of linear
     !! equations when the coefficient matrix is banded, and provides
     !! error bounds and backward error estimates for the solution.
               ldx, ferr, berr, work, rwork,info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, ldab, ldafb, ldb, ldx, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(sp), intent(out) :: berr(*), ferr(*), rwork(*)
           complex(sp), intent(in) :: ab(ldab,*), afb(ldafb,*), b(ldb,*)
           complex(sp), intent(out) :: work(*)
           complex(sp), intent(inout) :: x(ldx,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           
           
           
           ! Local Scalars 
           logical(lk) :: notran
           character :: transn, transt
           integer(ilp) :: count, i, j, k, kase, kk, nz
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
           notran = stdlib_lsame( trans, 'N' )
           if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( trans, &
                     'C' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kl<0_ilp ) then
              info = -3_ilp
           else if( ku<0_ilp ) then
              info = -4_ilp
           else if( nrhs<0_ilp ) then
              info = -5_ilp
           else if( ldab<kl+ku+1 ) then
              info = -7_ilp
           else if( ldafb<2_ilp*kl+ku+1 ) then
              info = -9_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -12_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -14_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGBRFS', -info )
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
           if( notran ) then
              transn = 'N'
              transt = 'C'
           else
              transn = 'C'
              transt = 'N'
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = min( kl+ku+2, n+1 )
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
              ! compute residual r = b - op(a) * x,
              ! where op(a) = a, a**t, or a**h, depending on trans.
              call stdlib_ccopy( n, b( 1_ilp, j ), 1_ilp, work, 1_ilp )
              call stdlib_cgbmv( trans, n, n, kl, ku, -cone, ab, ldab, x( 1_ilp, j ), 1_ilp,cone, work, 1_ilp &
                        )
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(op(a))*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              do i = 1, n
                 rwork( i ) = cabs1( b( i, j ) )
              end do
              ! compute abs(op(a))*abs(x) + abs(b).
              if( notran ) then
                 do k = 1, n
                    kk = ku + 1_ilp - k
                    xk = cabs1( x( k, j ) )
                    do i = max( 1, k-ku ), min( n, k+kl )
                       rwork( i ) = rwork( i ) + cabs1( ab( kk+i, k ) )*xk
                    end do
                 end do
              else
                 do k = 1, n
                    s = zero
                    kk = ku + 1_ilp - k
                    do i = max( 1, k-ku ), min( n, k+kl )
                       s = s + cabs1( ab( kk+i, k ) )*cabs1( x( i, j ) )
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
                 call stdlib_cgbtrs( trans, n, kl, ku, 1_ilp, afb, ldafb, ipiv, work, n,info )
                 call stdlib_caxpy( n, cone, work, 1_ilp, x( 1_ilp, j ), 1_ilp )
                 lstres = berr( j )
                 count = count + 1_ilp
                 go to 20
              end if
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(op(a)))*
                 ! ( abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(op(a)) is the inverse of op(a)
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(op(a))*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(op(a))*abs(x) + abs(b) is less than safe2.
              ! use stdlib_clacn2 to estimate the infinity-norm of the matrix
                 ! inv(op(a)) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) )))
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
                    ! multiply by diag(w)*inv(op(a)**h).
                    call stdlib_cgbtrs( transt, n, kl, ku, 1_ilp, afb, ldafb, ipiv,work, n, info )
                              
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                 else
                    ! multiply by inv(op(a))*diag(w).
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                    call stdlib_cgbtrs( transn, n, kl, ku, 1_ilp, afb, ldafb, ipiv,work, n, info )
                              
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
     end subroutine stdlib_cgbrfs

     pure module subroutine stdlib_zgbrfs( trans, n, kl, ku, nrhs, ab, ldab, afb, ldafb,ipiv, b, ldb, x, &
     !! ZGBRFS improves the computed solution to a system of linear
     !! equations when the coefficient matrix is banded, and provides
     !! error bounds and backward error estimates for the solution.
               ldx, ferr, berr, work, rwork,info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, ldab, ldafb, ldb, ldx, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(dp), intent(out) :: berr(*), ferr(*), rwork(*)
           complex(dp), intent(in) :: ab(ldab,*), afb(ldafb,*), b(ldb,*)
           complex(dp), intent(out) :: work(*)
           complex(dp), intent(inout) :: x(ldx,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           
           
           
           ! Local Scalars 
           logical(lk) :: notran
           character :: transn, transt
           integer(ilp) :: count, i, j, k, kase, kk, nz
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
           notran = stdlib_lsame( trans, 'N' )
           if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( trans, &
                     'C' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kl<0_ilp ) then
              info = -3_ilp
           else if( ku<0_ilp ) then
              info = -4_ilp
           else if( nrhs<0_ilp ) then
              info = -5_ilp
           else if( ldab<kl+ku+1 ) then
              info = -7_ilp
           else if( ldafb<2_ilp*kl+ku+1 ) then
              info = -9_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -12_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -14_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGBRFS', -info )
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
           if( notran ) then
              transn = 'N'
              transt = 'C'
           else
              transn = 'C'
              transt = 'N'
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = min( kl+ku+2, n+1 )
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
              ! compute residual r = b - op(a) * x,
              ! where op(a) = a, a**t, or a**h, depending on trans.
              call stdlib_zcopy( n, b( 1_ilp, j ), 1_ilp, work, 1_ilp )
              call stdlib_zgbmv( trans, n, n, kl, ku, -cone, ab, ldab, x( 1_ilp, j ), 1_ilp,cone, work, 1_ilp &
                        )
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(op(a))*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              do i = 1, n
                 rwork( i ) = cabs1( b( i, j ) )
              end do
              ! compute abs(op(a))*abs(x) + abs(b).
              if( notran ) then
                 do k = 1, n
                    kk = ku + 1_ilp - k
                    xk = cabs1( x( k, j ) )
                    do i = max( 1, k-ku ), min( n, k+kl )
                       rwork( i ) = rwork( i ) + cabs1( ab( kk+i, k ) )*xk
                    end do
                 end do
              else
                 do k = 1, n
                    s = zero
                    kk = ku + 1_ilp - k
                    do i = max( 1, k-ku ), min( n, k+kl )
                       s = s + cabs1( ab( kk+i, k ) )*cabs1( x( i, j ) )
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
                 call stdlib_zgbtrs( trans, n, kl, ku, 1_ilp, afb, ldafb, ipiv, work, n,info )
                 call stdlib_zaxpy( n, cone, work, 1_ilp, x( 1_ilp, j ), 1_ilp )
                 lstres = berr( j )
                 count = count + 1_ilp
                 go to 20
              end if
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(op(a)))*
                 ! ( abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(op(a)) is the inverse of op(a)
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(op(a))*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(op(a))*abs(x) + abs(b) is less than safe2.
              ! use stdlib_zlacn2 to estimate the infinity-norm of the matrix
                 ! inv(op(a)) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) )))
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
                    ! multiply by diag(w)*inv(op(a)**h).
                    call stdlib_zgbtrs( transt, n, kl, ku, 1_ilp, afb, ldafb, ipiv,work, n, info )
                              
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                 else
                    ! multiply by inv(op(a))*diag(w).
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                    call stdlib_zgbtrs( transn, n, kl, ku, 1_ilp, afb, ldafb, ipiv,work, n, info )
                              
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
     end subroutine stdlib_zgbrfs




     pure module subroutine stdlib_sgbequ( m, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd,amax, info )
     !! SGBEQU computes row and column scalings intended to equilibrate an
     !! M-by-N band matrix A and reduce its condition number.  R returns the
     !! row scale factors and C the column scale factors, chosen to try to
     !! make the largest element in each row and column of the matrix B with
     !! elements B(i,j)=R(i)*A(i,j)*C(j) have absolute value 1.
     !! R(i) and C(j) are restricted to be between SMLNUM = smallest safe
     !! number and BIGNUM = largest safe number.  Use of these scaling
     !! factors is not guaranteed to reduce the condition number of A but
     !! works well in practice.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, ldab, m, n
           real(sp), intent(out) :: amax, colcnd, rowcnd
           ! Array Arguments 
           real(sp), intent(in) :: ab(ldab,*)
           real(sp), intent(out) :: c(*), r(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, kd
           real(sp) :: bignum, rcmax, rcmin, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kl<0_ilp ) then
              info = -3_ilp
           else if( ku<0_ilp ) then
              info = -4_ilp
           else if( ldab<kl+ku+1 ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGBEQU', -info )
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp ) then
              rowcnd = one
              colcnd = one
              amax = zero
              return
           end if
           ! get machine constants.
           smlnum = stdlib_slamch( 'S' )
           bignum = one / smlnum
           ! compute row scale factors.
           do i = 1, m
              r( i ) = zero
           end do
           ! find the maximum element in each row.
           kd = ku + 1_ilp
           do j = 1, n
              do i = max( j-ku, 1 ), min( j+kl, m )
                 r( i ) = max( r( i ), abs( ab( kd+i-j, j ) ) )
              end do
           end do
           ! find the maximum and minimum scale factors.
           rcmin = bignum
           rcmax = zero
           do i = 1, m
              rcmax = max( rcmax, r( i ) )
              rcmin = min( rcmin, r( i ) )
           end do
           amax = rcmax
           if( rcmin==zero ) then
              ! find the first zero scale factor and return an error code.
              do i = 1, m
                 if( r( i )==zero ) then
                    info = i
                    return
                 end if
              end do
           else
              ! invert the scale factors.
              do i = 1, m
                 r( i ) = one / min( max( r( i ), smlnum ), bignum )
              end do
              ! compute rowcnd = min(r(i)) / max(r(i))
              rowcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
           end if
           ! compute column scale factors
           do j = 1, n
              c( j ) = zero
           end do
           ! find the maximum element in each column,
           ! assuming the row scaling computed above.
           kd = ku + 1_ilp
           do j = 1, n
              do i = max( j-ku, 1 ), min( j+kl, m )
                 c( j ) = max( c( j ), abs( ab( kd+i-j, j ) )*r( i ) )
              end do
           end do
           ! find the maximum and minimum scale factors.
           rcmin = bignum
           rcmax = zero
           do j = 1, n
              rcmin = min( rcmin, c( j ) )
              rcmax = max( rcmax, c( j ) )
           end do
           if( rcmin==zero ) then
              ! find the first zero scale factor and return an error code.
              do j = 1, n
                 if( c( j )==zero ) then
                    info = m + j
                    return
                 end if
              end do
           else
              ! invert the scale factors.
              do j = 1, n
                 c( j ) = one / min( max( c( j ), smlnum ), bignum )
              end do
              ! compute colcnd = min(c(j)) / max(c(j))
              colcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
           end if
           return
     end subroutine stdlib_sgbequ

     pure module subroutine stdlib_dgbequ( m, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd,amax, info )
     !! DGBEQU computes row and column scalings intended to equilibrate an
     !! M-by-N band matrix A and reduce its condition number.  R returns the
     !! row scale factors and C the column scale factors, chosen to try to
     !! make the largest element in each row and column of the matrix B with
     !! elements B(i,j)=R(i)*A(i,j)*C(j) have absolute value 1.
     !! R(i) and C(j) are restricted to be between SMLNUM = smallest safe
     !! number and BIGNUM = largest safe number.  Use of these scaling
     !! factors is not guaranteed to reduce the condition number of A but
     !! works well in practice.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, ldab, m, n
           real(dp), intent(out) :: amax, colcnd, rowcnd
           ! Array Arguments 
           real(dp), intent(in) :: ab(ldab,*)
           real(dp), intent(out) :: c(*), r(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, kd
           real(dp) :: bignum, rcmax, rcmin, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kl<0_ilp ) then
              info = -3_ilp
           else if( ku<0_ilp ) then
              info = -4_ilp
           else if( ldab<kl+ku+1 ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGBEQU', -info )
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp ) then
              rowcnd = one
              colcnd = one
              amax = zero
              return
           end if
           ! get machine constants.
           smlnum = stdlib_dlamch( 'S' )
           bignum = one / smlnum
           ! compute row scale factors.
           do i = 1, m
              r( i ) = zero
           end do
           ! find the maximum element in each row.
           kd = ku + 1_ilp
           do j = 1, n
              do i = max( j-ku, 1 ), min( j+kl, m )
                 r( i ) = max( r( i ), abs( ab( kd+i-j, j ) ) )
              end do
           end do
           ! find the maximum and minimum scale factors.
           rcmin = bignum
           rcmax = zero
           do i = 1, m
              rcmax = max( rcmax, r( i ) )
              rcmin = min( rcmin, r( i ) )
           end do
           amax = rcmax
           if( rcmin==zero ) then
              ! find the first zero scale factor and return an error code.
              do i = 1, m
                 if( r( i )==zero ) then
                    info = i
                    return
                 end if
              end do
           else
              ! invert the scale factors.
              do i = 1, m
                 r( i ) = one / min( max( r( i ), smlnum ), bignum )
              end do
              ! compute rowcnd = min(r(i)) / max(r(i))
              rowcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
           end if
           ! compute column scale factors
           do j = 1, n
              c( j ) = zero
           end do
           ! find the maximum element in each column,
           ! assuming the row scaling computed above.
           kd = ku + 1_ilp
           do j = 1, n
              do i = max( j-ku, 1 ), min( j+kl, m )
                 c( j ) = max( c( j ), abs( ab( kd+i-j, j ) )*r( i ) )
              end do
           end do
           ! find the maximum and minimum scale factors.
           rcmin = bignum
           rcmax = zero
           do j = 1, n
              rcmin = min( rcmin, c( j ) )
              rcmax = max( rcmax, c( j ) )
           end do
           if( rcmin==zero ) then
              ! find the first zero scale factor and return an error code.
              do j = 1, n
                 if( c( j )==zero ) then
                    info = m + j
                    return
                 end if
              end do
           else
              ! invert the scale factors.
              do j = 1, n
                 c( j ) = one / min( max( c( j ), smlnum ), bignum )
              end do
              ! compute colcnd = min(c(j)) / max(c(j))
              colcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
           end if
           return
     end subroutine stdlib_dgbequ


     pure module subroutine stdlib_cgbequ( m, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd,amax, info )
     !! CGBEQU computes row and column scalings intended to equilibrate an
     !! M-by-N band matrix A and reduce its condition number.  R returns the
     !! row scale factors and C the column scale factors, chosen to try to
     !! make the largest element in each row and column of the matrix B with
     !! elements B(i,j)=R(i)*A(i,j)*C(j) have absolute value 1.
     !! R(i) and C(j) are restricted to be between SMLNUM = smallest safe
     !! number and BIGNUM = largest safe number.  Use of these scaling
     !! factors is not guaranteed to reduce the condition number of A but
     !! works well in practice.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, ldab, m, n
           real(sp), intent(out) :: amax, colcnd, rowcnd
           ! Array Arguments 
           real(sp), intent(out) :: c(*), r(*)
           complex(sp), intent(in) :: ab(ldab,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, kd
           real(sp) :: bignum, rcmax, rcmin, smlnum
           complex(sp) :: zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kl<0_ilp ) then
              info = -3_ilp
           else if( ku<0_ilp ) then
              info = -4_ilp
           else if( ldab<kl+ku+1 ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGBEQU', -info )
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp ) then
              rowcnd = one
              colcnd = one
              amax = zero
              return
           end if
           ! get machine constants.
           smlnum = stdlib_slamch( 'S' )
           bignum = one / smlnum
           ! compute row scale factors.
           do i = 1, m
              r( i ) = zero
           end do
           ! find the maximum element in each row.
           kd = ku + 1_ilp
           do j = 1, n
              do i = max( j-ku, 1 ), min( j+kl, m )
                 r( i ) = max( r( i ), cabs1( ab( kd+i-j, j ) ) )
              end do
           end do
           ! find the maximum and minimum scale factors.
           rcmin = bignum
           rcmax = zero
           do i = 1, m
              rcmax = max( rcmax, r( i ) )
              rcmin = min( rcmin, r( i ) )
           end do
           amax = rcmax
           if( rcmin==zero ) then
              ! find the first zero scale factor and return an error code.
              do i = 1, m
                 if( r( i )==zero ) then
                    info = i
                    return
                 end if
              end do
           else
              ! invert the scale factors.
              do i = 1, m
                 r( i ) = one / min( max( r( i ), smlnum ), bignum )
              end do
              ! compute rowcnd = min(r(i)) / max(r(i))
              rowcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
           end if
           ! compute column scale factors
           do j = 1, n
              c( j ) = zero
           end do
           ! find the maximum element in each column,
           ! assuming the row scaling computed above.
           kd = ku + 1_ilp
           do j = 1, n
              do i = max( j-ku, 1 ), min( j+kl, m )
                 c( j ) = max( c( j ), cabs1( ab( kd+i-j, j ) )*r( i ) )
              end do
           end do
           ! find the maximum and minimum scale factors.
           rcmin = bignum
           rcmax = zero
           do j = 1, n
              rcmin = min( rcmin, c( j ) )
              rcmax = max( rcmax, c( j ) )
           end do
           if( rcmin==zero ) then
              ! find the first zero scale factor and return an error code.
              do j = 1, n
                 if( c( j )==zero ) then
                    info = m + j
                    return
                 end if
              end do
           else
              ! invert the scale factors.
              do j = 1, n
                 c( j ) = one / min( max( c( j ), smlnum ), bignum )
              end do
              ! compute colcnd = min(c(j)) / max(c(j))
              colcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
           end if
           return
     end subroutine stdlib_cgbequ

     pure module subroutine stdlib_zgbequ( m, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd,amax, info )
     !! ZGBEQU computes row and column scalings intended to equilibrate an
     !! M-by-N band matrix A and reduce its condition number.  R returns the
     !! row scale factors and C the column scale factors, chosen to try to
     !! make the largest element in each row and column of the matrix B with
     !! elements B(i,j)=R(i)*A(i,j)*C(j) have absolute value 1.
     !! R(i) and C(j) are restricted to be between SMLNUM = smallest safe
     !! number and BIGNUM = largest safe number.  Use of these scaling
     !! factors is not guaranteed to reduce the condition number of A but
     !! works well in practice.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, ldab, m, n
           real(dp), intent(out) :: amax, colcnd, rowcnd
           ! Array Arguments 
           real(dp), intent(out) :: c(*), r(*)
           complex(dp), intent(in) :: ab(ldab,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, kd
           real(dp) :: bignum, rcmax, rcmin, smlnum
           complex(dp) :: zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kl<0_ilp ) then
              info = -3_ilp
           else if( ku<0_ilp ) then
              info = -4_ilp
           else if( ldab<kl+ku+1 ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGBEQU', -info )
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp ) then
              rowcnd = one
              colcnd = one
              amax = zero
              return
           end if
           ! get machine constants.
           smlnum = stdlib_dlamch( 'S' )
           bignum = one / smlnum
           ! compute row scale factors.
           do i = 1, m
              r( i ) = zero
           end do
           ! find the maximum element in each row.
           kd = ku + 1_ilp
           do j = 1, n
              do i = max( j-ku, 1 ), min( j+kl, m )
                 r( i ) = max( r( i ), cabs1( ab( kd+i-j, j ) ) )
              end do
           end do
           ! find the maximum and minimum scale factors.
           rcmin = bignum
           rcmax = zero
           do i = 1, m
              rcmax = max( rcmax, r( i ) )
              rcmin = min( rcmin, r( i ) )
           end do
           amax = rcmax
           if( rcmin==zero ) then
              ! find the first zero scale factor and return an error code.
              do i = 1, m
                 if( r( i )==zero ) then
                    info = i
                    return
                 end if
              end do
           else
              ! invert the scale factors.
              do i = 1, m
                 r( i ) = one / min( max( r( i ), smlnum ), bignum )
              end do
              ! compute rowcnd = min(r(i)) / max(r(i))
              rowcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
           end if
           ! compute column scale factors
           do j = 1, n
              c( j ) = zero
           end do
           ! find the maximum element in each column,
           ! assuming the row scaling computed above.
           kd = ku + 1_ilp
           do j = 1, n
              do i = max( j-ku, 1 ), min( j+kl, m )
                 c( j ) = max( c( j ), cabs1( ab( kd+i-j, j ) )*r( i ) )
              end do
           end do
           ! find the maximum and minimum scale factors.
           rcmin = bignum
           rcmax = zero
           do j = 1, n
              rcmin = min( rcmin, c( j ) )
              rcmax = max( rcmax, c( j ) )
           end do
           if( rcmin==zero ) then
              ! find the first zero scale factor and return an error code.
              do j = 1, n
                 if( c( j )==zero ) then
                    info = m + j
                    return
                 end if
              end do
           else
              ! invert the scale factors.
              do j = 1, n
                 c( j ) = one / min( max( c( j ), smlnum ), bignum )
              end do
              ! compute colcnd = min(c(j)) / max(c(j))
              colcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
           end if
           return
     end subroutine stdlib_zgbequ




     pure module subroutine stdlib_sgbequb( m, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd,amax, info )
     !! SGBEQUB computes row and column scalings intended to equilibrate an
     !! M-by-N matrix A and reduce its condition number.  R returns the row
     !! scale factors and C the column scale factors, chosen to try to make
     !! the largest element in each row and column of the matrix B with
     !! elements B(i,j)=R(i)*A(i,j)*C(j) have an absolute value of at most
     !! the radix.
     !! R(i) and C(j) are restricted to be a power of the radix between
     !! SMLNUM = smallest safe number and BIGNUM = largest safe number.  Use
     !! of these scaling factors is not guaranteed to reduce the condition
     !! number of A but works well in practice.
     !! This routine differs from SGEEQU by restricting the scaling factors
     !! to a power of the radix.  Barring over- and underflow, scaling by
     !! these factors introduces no additional rounding errors.  However, the
     !! scaled entries' magnitudes are no longer approximately 1 but lie
     !! between sqrt(radix) and 1/sqrt(radix).
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, ldab, m, n
           real(sp), intent(out) :: amax, colcnd, rowcnd
           ! Array Arguments 
           real(sp), intent(in) :: ab(ldab,*)
           real(sp), intent(out) :: c(*), r(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, kd
           real(sp) :: bignum, rcmax, rcmin, smlnum, radix, logrdx
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kl<0_ilp ) then
              info = -3_ilp
           else if( ku<0_ilp ) then
              info = -4_ilp
           else if( ldab<kl+ku+1 ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGBEQUB', -info )
              return
           end if
           ! quick return if possible.
           if( m==0_ilp .or. n==0_ilp ) then
              rowcnd = one
              colcnd = one
              amax = zero
              return
           end if
           ! get machine constants.  assume smlnum is a power of the radix.
           smlnum = stdlib_slamch( 'S' )
           bignum = one / smlnum
           radix = stdlib_slamch( 'B' )
           logrdx = log(radix)
           ! compute row scale factors.
           do i = 1, m
              r( i ) = zero
           end do
           ! find the maximum element in each row.
           kd = ku + 1_ilp
           do j = 1, n
              do i = max( j-ku, 1 ), min( j+kl, m )
                 r( i ) = max( r( i ), abs( ab( kd+i-j, j ) ) )
              end do
           end do
           do i = 1, m
              if( r( i )>zero ) then
                 r( i ) = radix**int( log( r( i ) ) / logrdx,KIND=ilp)
              end if
           end do
           ! find the maximum and minimum scale factors.
           rcmin = bignum
           rcmax = zero
           do i = 1, m
              rcmax = max( rcmax, r( i ) )
              rcmin = min( rcmin, r( i ) )
           end do
           amax = rcmax
           if( rcmin==zero ) then
              ! find the first zero scale factor and return an error code.
              do i = 1, m
                 if( r( i )==zero ) then
                    info = i
                    return
                 end if
              end do
           else
              ! invert the scale factors.
              do i = 1, m
                 r( i ) = one / min( max( r( i ), smlnum ), bignum )
              end do
              ! compute rowcnd = min(r(i)) / max(r(i)).
              rowcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
           end if
           ! compute column scale factors.
           do j = 1, n
              c( j ) = zero
           end do
           ! find the maximum element in each column,
           ! assuming the row scaling computed above.
           do j = 1, n
              do i = max( j-ku, 1 ), min( j+kl, m )
                 c( j ) = max( c( j ), abs( ab( kd+i-j, j ) )*r( i ) )
              end do
              if( c( j )>zero ) then
                 c( j ) = radix**int( log( c( j ) ) / logrdx,KIND=ilp)
              end if
           end do
           ! find the maximum and minimum scale factors.
           rcmin = bignum
           rcmax = zero
           do j = 1, n
              rcmin = min( rcmin, c( j ) )
              rcmax = max( rcmax, c( j ) )
           end do
           if( rcmin==zero ) then
              ! find the first zero scale factor and return an error code.
              do j = 1, n
                 if( c( j )==zero ) then
                    info = m + j
                    return
                 end if
              end do
           else
              ! invert the scale factors.
              do j = 1, n
                 c( j ) = one / min( max( c( j ), smlnum ), bignum )
              end do
              ! compute colcnd = min(c(j)) / max(c(j)).
              colcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
           end if
           return
     end subroutine stdlib_sgbequb

     pure module subroutine stdlib_dgbequb( m, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd,amax, info )
     !! DGBEQUB computes row and column scalings intended to equilibrate an
     !! M-by-N matrix A and reduce its condition number.  R returns the row
     !! scale factors and C the column scale factors, chosen to try to make
     !! the largest element in each row and column of the matrix B with
     !! elements B(i,j)=R(i)*A(i,j)*C(j) have an absolute value of at most
     !! the radix.
     !! R(i) and C(j) are restricted to be a power of the radix between
     !! SMLNUM = smallest safe number and BIGNUM = largest safe number.  Use
     !! of these scaling factors is not guaranteed to reduce the condition
     !! number of A but works well in practice.
     !! This routine differs from DGEEQU by restricting the scaling factors
     !! to a power of the radix.  Barring over- and underflow, scaling by
     !! these factors introduces no additional rounding errors.  However, the
     !! scaled entries' magnitudes are no longer approximately 1 but lie
     !! between sqrt(radix) and 1/sqrt(radix).
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, ldab, m, n
           real(dp), intent(out) :: amax, colcnd, rowcnd
           ! Array Arguments 
           real(dp), intent(in) :: ab(ldab,*)
           real(dp), intent(out) :: c(*), r(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, kd
           real(dp) :: bignum, rcmax, rcmin, smlnum, radix, logrdx
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kl<0_ilp ) then
              info = -3_ilp
           else if( ku<0_ilp ) then
              info = -4_ilp
           else if( ldab<kl+ku+1 ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGBEQUB', -info )
              return
           end if
           ! quick return if possible.
           if( m==0_ilp .or. n==0_ilp ) then
              rowcnd = one
              colcnd = one
              amax = zero
              return
           end if
           ! get machine constants.  assume smlnum is a power of the radix.
           smlnum = stdlib_dlamch( 'S' )
           bignum = one / smlnum
           radix = stdlib_dlamch( 'B' )
           logrdx = log(radix)
           ! compute row scale factors.
           do i = 1, m
              r( i ) = zero
           end do
           ! find the maximum element in each row.
           kd = ku + 1_ilp
           do j = 1, n
              do i = max( j-ku, 1 ), min( j+kl, m )
                 r( i ) = max( r( i ), abs( ab( kd+i-j, j ) ) )
              end do
           end do
           do i = 1, m
              if( r( i )>zero ) then
                 r( i ) = radix**int( log( r( i ) ) / logrdx,KIND=ilp)
              end if
           end do
           ! find the maximum and minimum scale factors.
           rcmin = bignum
           rcmax = zero
           do i = 1, m
              rcmax = max( rcmax, r( i ) )
              rcmin = min( rcmin, r( i ) )
           end do
           amax = rcmax
           if( rcmin==zero ) then
              ! find the first zero scale factor and return an error code.
              do i = 1, m
                 if( r( i )==zero ) then
                    info = i
                    return
                 end if
              end do
           else
              ! invert the scale factors.
              do i = 1, m
                 r( i ) = one / min( max( r( i ), smlnum ), bignum )
              end do
              ! compute rowcnd = min(r(i)) / max(r(i)).
              rowcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
           end if
           ! compute column scale factors.
           do j = 1, n
              c( j ) = zero
           end do
           ! find the maximum element in each column,
           ! assuming the row scaling computed above.
           do j = 1, n
              do i = max( j-ku, 1 ), min( j+kl, m )
                 c( j ) = max( c( j ), abs( ab( kd+i-j, j ) )*r( i ) )
              end do
              if( c( j )>zero ) then
                 c( j ) = radix**int( log( c( j ) ) / logrdx,KIND=ilp)
              end if
           end do
           ! find the maximum and minimum scale factors.
           rcmin = bignum
           rcmax = zero
           do j = 1, n
              rcmin = min( rcmin, c( j ) )
              rcmax = max( rcmax, c( j ) )
           end do
           if( rcmin==zero ) then
              ! find the first zero scale factor and return an error code.
              do j = 1, n
                 if( c( j )==zero ) then
                    info = m + j
                    return
                 end if
              end do
           else
              ! invert the scale factors.
              do j = 1, n
                 c( j ) = one / min( max( c( j ), smlnum ), bignum )
              end do
              ! compute colcnd = min(c(j)) / max(c(j)).
              colcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
           end if
           return
     end subroutine stdlib_dgbequb


     pure module subroutine stdlib_cgbequb( m, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd,amax, info )
     !! CGBEQUB computes row and column scalings intended to equilibrate an
     !! M-by-N matrix A and reduce its condition number.  R returns the row
     !! scale factors and C the column scale factors, chosen to try to make
     !! the largest element in each row and column of the matrix B with
     !! elements B(i,j)=R(i)*A(i,j)*C(j) have an absolute value of at most
     !! the radix.
     !! R(i) and C(j) are restricted to be a power of the radix between
     !! SMLNUM = smallest safe number and BIGNUM = largest safe number.  Use
     !! of these scaling factors is not guaranteed to reduce the condition
     !! number of A but works well in practice.
     !! This routine differs from CGEEQU by restricting the scaling factors
     !! to a power of the radix.  Barring over- and underflow, scaling by
     !! these factors introduces no additional rounding errors.  However, the
     !! scaled entries' magnitudes are no longer approximately 1 but lie
     !! between sqrt(radix) and 1/sqrt(radix).
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, ldab, m, n
           real(sp), intent(out) :: amax, colcnd, rowcnd
           ! Array Arguments 
           real(sp), intent(out) :: c(*), r(*)
           complex(sp), intent(in) :: ab(ldab,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, kd
           real(sp) :: bignum, rcmax, rcmin, smlnum, radix, logrdx
           complex(sp) :: zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kl<0_ilp ) then
              info = -3_ilp
           else if( ku<0_ilp ) then
              info = -4_ilp
           else if( ldab<kl+ku+1 ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGBEQUB', -info )
              return
           end if
           ! quick return if possible.
           if( m==0_ilp .or. n==0_ilp ) then
              rowcnd = one
              colcnd = one
              amax = zero
              return
           end if
           ! get machine constants.  assume smlnum is a power of the radix.
           smlnum = stdlib_slamch( 'S' )
           bignum = one / smlnum
           radix = stdlib_slamch( 'B' )
           logrdx = log(radix)
           ! compute row scale factors.
           do i = 1, m
              r( i ) = zero
           end do
           ! find the maximum element in each row.
           kd = ku + 1_ilp
           do j = 1, n
              do i = max( j-ku, 1 ), min( j+kl, m )
                 r( i ) = max( r( i ), cabs1( ab( kd+i-j, j ) ) )
              end do
           end do
           do i = 1, m
              if( r( i )>zero ) then
                 r( i ) = radix**int( log( r( i ) ) / logrdx,KIND=ilp)
              end if
           end do
           ! find the maximum and minimum scale factors.
           rcmin = bignum
           rcmax = zero
           do i = 1, m
              rcmax = max( rcmax, r( i ) )
              rcmin = min( rcmin, r( i ) )
           end do
           amax = rcmax
           if( rcmin==zero ) then
              ! find the first zero scale factor and return an error code.
              do i = 1, m
                 if( r( i )==zero ) then
                    info = i
                    return
                 end if
              end do
           else
              ! invert the scale factors.
              do i = 1, m
                 r( i ) = one / min( max( r( i ), smlnum ), bignum )
              end do
              ! compute rowcnd = min(r(i)) / max(r(i)).
              rowcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
           end if
           ! compute column scale factors.
           do j = 1, n
              c( j ) = zero
           end do
           ! find the maximum element in each column,
           ! assuming the row scaling computed above.
           do j = 1, n
              do i = max( j-ku, 1 ), min( j+kl, m )
                 c( j ) = max( c( j ), cabs1( ab( kd+i-j, j ) )*r( i ) )
              end do
              if( c( j )>zero ) then
                 c( j ) = radix**int( log( c( j ) ) / logrdx,KIND=ilp)
              end if
           end do
           ! find the maximum and minimum scale factors.
           rcmin = bignum
           rcmax = zero
           do j = 1, n
              rcmin = min( rcmin, c( j ) )
              rcmax = max( rcmax, c( j ) )
           end do
           if( rcmin==zero ) then
              ! find the first zero scale factor and return an error code.
              do j = 1, n
                 if( c( j )==zero ) then
                    info = m + j
                    return
                 end if
              end do
           else
              ! invert the scale factors.
              do j = 1, n
                 c( j ) = one / min( max( c( j ), smlnum ), bignum )
              end do
              ! compute colcnd = min(c(j)) / max(c(j)).
              colcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
           end if
           return
     end subroutine stdlib_cgbequb

     pure module subroutine stdlib_zgbequb( m, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd,amax, info )
     !! ZGBEQUB computes row and column scalings intended to equilibrate an
     !! M-by-N matrix A and reduce its condition number.  R returns the row
     !! scale factors and C the column scale factors, chosen to try to make
     !! the largest element in each row and column of the matrix B with
     !! elements B(i,j)=R(i)*A(i,j)*C(j) have an absolute value of at most
     !! the radix.
     !! R(i) and C(j) are restricted to be a power of the radix between
     !! SMLNUM = smallest safe number and BIGNUM = largest safe number.  Use
     !! of these scaling factors is not guaranteed to reduce the condition
     !! number of A but works well in practice.
     !! This routine differs from ZGEEQU by restricting the scaling factors
     !! to a power of the radix.  Barring over- and underflow, scaling by
     !! these factors introduces no additional rounding errors.  However, the
     !! scaled entries' magnitudes are no longer approximately 1 but lie
     !! between sqrt(radix) and 1/sqrt(radix).
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, ldab, m, n
           real(dp), intent(out) :: amax, colcnd, rowcnd
           ! Array Arguments 
           real(dp), intent(out) :: c(*), r(*)
           complex(dp), intent(in) :: ab(ldab,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, kd
           real(dp) :: bignum, rcmax, rcmin, smlnum, radix, logrdx
           complex(dp) :: zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kl<0_ilp ) then
              info = -3_ilp
           else if( ku<0_ilp ) then
              info = -4_ilp
           else if( ldab<kl+ku+1 ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGBEQUB', -info )
              return
           end if
           ! quick return if possible.
           if( m==0_ilp .or. n==0_ilp ) then
              rowcnd = one
              colcnd = one
              amax = zero
              return
           end if
           ! get machine constants.  assume smlnum is a power of the radix.
           smlnum = stdlib_dlamch( 'S' )
           bignum = one / smlnum
           radix = stdlib_dlamch( 'B' )
           logrdx = log(radix)
           ! compute row scale factors.
           do i = 1, m
              r( i ) = zero
           end do
           ! find the maximum element in each row.
           kd = ku + 1_ilp
           do j = 1, n
              do i = max( j-ku, 1 ), min( j+kl, m )
                 r( i ) = max( r( i ), cabs1( ab( kd+i-j, j ) ) )
              end do
           end do
           do i = 1, m
              if( r( i )>zero ) then
                 r( i ) = radix**int( log( r( i ) ) / logrdx,KIND=ilp)
              end if
           end do
           ! find the maximum and minimum scale factors.
           rcmin = bignum
           rcmax = zero
           do i = 1, m
              rcmax = max( rcmax, r( i ) )
              rcmin = min( rcmin, r( i ) )
           end do
           amax = rcmax
           if( rcmin==zero ) then
              ! find the first zero scale factor and return an error code.
              do i = 1, m
                 if( r( i )==zero ) then
                    info = i
                    return
                 end if
              end do
           else
              ! invert the scale factors.
              do i = 1, m
                 r( i ) = one / min( max( r( i ), smlnum ), bignum )
              end do
              ! compute rowcnd = min(r(i)) / max(r(i)).
              rowcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
           end if
           ! compute column scale factors.
           do j = 1, n
              c( j ) = zero
           end do
           ! find the maximum element in each column,
           ! assuming the row scaling computed above.
           do j = 1, n
              do i = max( j-ku, 1 ), min( j+kl, m )
                 c( j ) = max( c( j ), cabs1( ab( kd+i-j, j ) )*r( i ) )
              end do
              if( c( j )>zero ) then
                 c( j ) = radix**int( log( c( j ) ) / logrdx,KIND=ilp)
              end if
           end do
           ! find the maximum and minimum scale factors.
           rcmin = bignum
           rcmax = zero
           do j = 1, n
              rcmin = min( rcmin, c( j ) )
              rcmax = max( rcmax, c( j ) )
           end do
           if( rcmin==zero ) then
              ! find the first zero scale factor and return an error code.
              do j = 1, n
                 if( c( j )==zero ) then
                    info = m + j
                    return
                 end if
              end do
           else
              ! invert the scale factors.
              do j = 1, n
                 c( j ) = one / min( max( c( j ), smlnum ), bignum )
              end do
              ! compute colcnd = min(c(j)) / max(c(j)).
              colcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
           end if
           return
     end subroutine stdlib_zgbequb




     pure module subroutine stdlib_slaqgb( m, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd,amax, equed )
     !! SLAQGB equilibrates a general M by N band matrix A with KL
     !! subdiagonals and KU superdiagonals using the row and scaling factors
     !! in the vectors R and C.
               
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(out) :: equed
           integer(ilp), intent(in) :: kl, ku, ldab, m, n
           real(sp), intent(in) :: amax, colcnd, rowcnd
           ! Array Arguments 
           real(sp), intent(inout) :: ab(ldab,*)
           real(sp), intent(in) :: c(*), r(*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: thresh = 0.1e+0_sp
           
           ! Local Scalars 
           integer(ilp) :: i, j
           real(sp) :: cj, large, small
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( m<=0_ilp .or. n<=0_ilp ) then
              equed = 'N'
              return
           end if
           ! initialize large and small.
           small = stdlib_slamch( 'SAFE MINIMUM' ) / stdlib_slamch( 'PRECISION' )
           large = one / small
           if( rowcnd>=thresh .and. amax>=small .and. amax<=large )then
              ! no row scaling
              if( colcnd>=thresh ) then
                 ! no column scaling
                 equed = 'N'
              else
                 ! column scaling
                 do j = 1, n
                    cj = c( j )
                    do i = max( 1, j-ku ), min( m, j+kl )
                       ab( ku+1+i-j, j ) = cj*ab( ku+1+i-j, j )
                    end do
                 end do
                 equed = 'C'
              end if
           else if( colcnd>=thresh ) then
              ! row scaling, no column scaling
              do j = 1, n
                 do i = max( 1, j-ku ), min( m, j+kl )
                    ab( ku+1+i-j, j ) = r( i )*ab( ku+1+i-j, j )
                 end do
              end do
              equed = 'R'
           else
              ! row and column scaling
              do j = 1, n
                 cj = c( j )
                 do i = max( 1, j-ku ), min( m, j+kl )
                    ab( ku+1+i-j, j ) = cj*r( i )*ab( ku+1+i-j, j )
                 end do
              end do
              equed = 'B'
           end if
           return
     end subroutine stdlib_slaqgb

     pure module subroutine stdlib_dlaqgb( m, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd,amax, equed )
     !! DLAQGB equilibrates a general M by N band matrix A with KL
     !! subdiagonals and KU superdiagonals using the row and scaling factors
     !! in the vectors R and C.
               
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(out) :: equed
           integer(ilp), intent(in) :: kl, ku, ldab, m, n
           real(dp), intent(in) :: amax, colcnd, rowcnd
           ! Array Arguments 
           real(dp), intent(inout) :: ab(ldab,*)
           real(dp), intent(in) :: c(*), r(*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: thresh = 0.1e+0_dp
           
           ! Local Scalars 
           integer(ilp) :: i, j
           real(dp) :: cj, large, small
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( m<=0_ilp .or. n<=0_ilp ) then
              equed = 'N'
              return
           end if
           ! initialize large and small.
           small = stdlib_dlamch( 'SAFE MINIMUM' ) / stdlib_dlamch( 'PRECISION' )
           large = one / small
           if( rowcnd>=thresh .and. amax>=small .and. amax<=large )then
              ! no row scaling
              if( colcnd>=thresh ) then
                 ! no column scaling
                 equed = 'N'
              else
                 ! column scaling
                 do j = 1, n
                    cj = c( j )
                    do i = max( 1, j-ku ), min( m, j+kl )
                       ab( ku+1+i-j, j ) = cj*ab( ku+1+i-j, j )
                    end do
                 end do
                 equed = 'C'
              end if
           else if( colcnd>=thresh ) then
              ! row scaling, no column scaling
              do j = 1, n
                 do i = max( 1, j-ku ), min( m, j+kl )
                    ab( ku+1+i-j, j ) = r( i )*ab( ku+1+i-j, j )
                 end do
              end do
              equed = 'R'
           else
              ! row and column scaling
              do j = 1, n
                 cj = c( j )
                 do i = max( 1, j-ku ), min( m, j+kl )
                    ab( ku+1+i-j, j ) = cj*r( i )*ab( ku+1+i-j, j )
                 end do
              end do
              equed = 'B'
           end if
           return
     end subroutine stdlib_dlaqgb


     pure module subroutine stdlib_claqgb( m, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd,amax, equed )
     !! CLAQGB equilibrates a general M by N band matrix A with KL
     !! subdiagonals and KU superdiagonals using the row and scaling factors
     !! in the vectors R and C.
               
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(out) :: equed
           integer(ilp), intent(in) :: kl, ku, ldab, m, n
           real(sp), intent(in) :: amax, colcnd, rowcnd
           ! Array Arguments 
           real(sp), intent(in) :: c(*), r(*)
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
           if( m<=0_ilp .or. n<=0_ilp ) then
              equed = 'N'
              return
           end if
           ! initialize large and small.
           small = stdlib_slamch( 'SAFE MINIMUM' ) / stdlib_slamch( 'PRECISION' )
           large = one / small
           if( rowcnd>=thresh .and. amax>=small .and. amax<=large )then
              ! no row scaling
              if( colcnd>=thresh ) then
                 ! no column scaling
                 equed = 'N'
              else
                 ! column scaling
                 do j = 1, n
                    cj = c( j )
                    do i = max( 1, j-ku ), min( m, j+kl )
                       ab( ku+1+i-j, j ) = cj*ab( ku+1+i-j, j )
                    end do
                 end do
                 equed = 'C'
              end if
           else if( colcnd>=thresh ) then
              ! row scaling, no column scaling
              do j = 1, n
                 do i = max( 1, j-ku ), min( m, j+kl )
                    ab( ku+1+i-j, j ) = r( i )*ab( ku+1+i-j, j )
                 end do
              end do
              equed = 'R'
           else
              ! row and column scaling
              do j = 1, n
                 cj = c( j )
                 do i = max( 1, j-ku ), min( m, j+kl )
                    ab( ku+1+i-j, j ) = cj*r( i )*ab( ku+1+i-j, j )
                 end do
              end do
              equed = 'B'
           end if
           return
     end subroutine stdlib_claqgb

     pure module subroutine stdlib_zlaqgb( m, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd,amax, equed )
     !! ZLAQGB equilibrates a general M by N band matrix A with KL
     !! subdiagonals and KU superdiagonals using the row and scaling factors
     !! in the vectors R and C.
               
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(out) :: equed
           integer(ilp), intent(in) :: kl, ku, ldab, m, n
           real(dp), intent(in) :: amax, colcnd, rowcnd
           ! Array Arguments 
           real(dp), intent(in) :: c(*), r(*)
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
           if( m<=0_ilp .or. n<=0_ilp ) then
              equed = 'N'
              return
           end if
           ! initialize large and small.
           small = stdlib_dlamch( 'SAFE MINIMUM' ) / stdlib_dlamch( 'PRECISION' )
           large = one / small
           if( rowcnd>=thresh .and. amax>=small .and. amax<=large )then
              ! no row scaling
              if( colcnd>=thresh ) then
                 ! no column scaling
                 equed = 'N'
              else
                 ! column scaling
                 do j = 1, n
                    cj = c( j )
                    do i = max( 1, j-ku ), min( m, j+kl )
                       ab( ku+1+i-j, j ) = cj*ab( ku+1+i-j, j )
                    end do
                 end do
                 equed = 'C'
              end if
           else if( colcnd>=thresh ) then
              ! row scaling, no column scaling
              do j = 1, n
                 do i = max( 1, j-ku ), min( m, j+kl )
                    ab( ku+1+i-j, j ) = r( i )*ab( ku+1+i-j, j )
                 end do
              end do
              equed = 'R'
           else
              ! row and column scaling
              do j = 1, n
                 cj = c( j )
                 do i = max( 1, j-ku ), min( m, j+kl )
                    ab( ku+1+i-j, j ) = cj*r( i )*ab( ku+1+i-j, j )
                 end do
              end do
              equed = 'B'
           end if
           return
     end subroutine stdlib_zlaqgb




     real(sp) module function stdlib_sla_gbrcond( trans, n, kl, ku, ab, ldab, afb, ldafb,ipiv, cmode, c, &
     !! SLA_GBRCOND Estimates the Skeel condition number of  op(A) * op2(C)
     !! where op2 is determined by CMODE as follows
     !! CMODE =  1    op2(C) = C
     !! CMODE =  0    op2(C) = I
     !! CMODE = -1    op2(C) = inv(C)
     !! The Skeel condition number  cond(A) = norminf( |inv(A)||A| )
     !! is computed by computing scaling factors R such that
     !! diag(R)*A*op2(C) is row equilibrated and computing the standard
     !! infinity-norm condition number.
               info, work, iwork )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(in) :: n, ldab, ldafb, kl, ku, cmode
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           integer(ilp), intent(in) :: ipiv(*)
           real(sp), intent(in) :: ab(ldab,*), afb(ldafb,*), c(*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: notrans
           integer(ilp) :: kase, i, j, kd, ke
           real(sp) :: ainvnm, tmp
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           stdlib_sla_gbrcond = zero
           info = 0_ilp
           notrans = stdlib_lsame( trans, 'N' )
           if ( .not. notrans .and. .not. stdlib_lsame(trans, 'T').and. .not. stdlib_lsame(trans, &
                     'C') ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kl<0_ilp .or. kl>n-1 ) then
              info = -3_ilp
           else if( ku<0_ilp .or. ku>n-1 ) then
              info = -4_ilp
           else if( ldab<kl+ku+1 ) then
              info = -6_ilp
           else if( ldafb<2_ilp*kl+ku+1 ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SLA_GBRCOND', -info )
              return
           end if
           if( n==0_ilp ) then
              stdlib_sla_gbrcond = one
              return
           end if
           ! compute the equilibration matrix r such that
           ! inv(r)*a*c has unit 1-norm.
           kd = ku + 1_ilp
           ke = kl + 1_ilp
           if ( notrans ) then
              do i = 1, n
                 tmp = zero
                    if ( cmode == 1_ilp ) then
                    do j = max( i-kl, 1 ), min( i+ku, n )
                       tmp = tmp + abs( ab( kd+i-j, j ) * c( j ) )
                    end do
                    else if ( cmode == 0_ilp ) then
                       do j = max( i-kl, 1 ), min( i+ku, n )
                          tmp = tmp + abs( ab( kd+i-j, j ) )
                       end do
                    else
                       do j = max( i-kl, 1 ), min( i+ku, n )
                          tmp = tmp + abs( ab( kd+i-j, j ) / c( j ) )
                       end do
                    end if
                 work( 2_ilp*n+i ) = tmp
              end do
           else
              do i = 1, n
                 tmp = zero
                 if ( cmode == 1_ilp ) then
                    do j = max( i-kl, 1 ), min( i+ku, n )
                       tmp = tmp + abs( ab( ke-i+j, i ) * c( j ) )
                    end do
                 else if ( cmode == 0_ilp ) then
                    do j = max( i-kl, 1 ), min( i+ku, n )
                       tmp = tmp + abs( ab( ke-i+j, i ) )
                    end do
                 else
                    do j = max( i-kl, 1 ), min( i+ku, n )
                       tmp = tmp + abs( ab( ke-i+j, i ) / c( j ) )
                    end do
                 end if
                 work( 2_ilp*n+i ) = tmp
              end do
           end if
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
                 if ( notrans ) then
                    call stdlib_sgbtrs( 'NO TRANSPOSE', n, kl, ku, 1_ilp, afb, ldafb,ipiv, work, n, &
                              info )
                 else
                    call stdlib_sgbtrs( 'TRANSPOSE', n, kl, ku, 1_ilp, afb, ldafb, ipiv,work, n, info &
                              )
                 end if
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
                 if ( notrans ) then
                    call stdlib_sgbtrs( 'TRANSPOSE', n, kl, ku, 1_ilp, afb, ldafb, ipiv,work, n, info &
                              )
                 else
                    call stdlib_sgbtrs( 'NO TRANSPOSE', n, kl, ku, 1_ilp, afb, ldafb,ipiv, work, n, &
                              info )
                 end if
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * work( 2_ilp*n+i )
                 end do
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm /= 0.0_sp )stdlib_sla_gbrcond = ( one / ainvnm )
           return
     end function stdlib_sla_gbrcond

     real(dp) module function stdlib_dla_gbrcond( trans, n, kl, ku, ab, ldab,afb, ldafb, ipiv, cmode, c,&
     !! DLA_GBRCOND Estimates the Skeel condition number of  op(A) * op2(C)
     !! where op2 is determined by CMODE as follows
     !! CMODE =  1    op2(C) = C
     !! CMODE =  0    op2(C) = I
     !! CMODE = -1    op2(C) = inv(C)
     !! The Skeel condition number  cond(A) = norminf( |inv(A)||A| )
     !! is computed by computing scaling factors R such that
     !! diag(R)*A*op2(C) is row equilibrated and computing the standard
     !! infinity-norm condition number.
               info, work, iwork )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(in) :: n, ldab, ldafb, kl, ku, cmode
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           integer(ilp), intent(in) :: ipiv(*)
           real(dp), intent(in) :: ab(ldab,*), afb(ldafb,*), c(*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: notrans
           integer(ilp) :: kase, i, j, kd, ke
           real(dp) :: ainvnm, tmp
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           stdlib_dla_gbrcond = zero
           info = 0_ilp
           notrans = stdlib_lsame( trans, 'N' )
           if ( .not. notrans .and. .not. stdlib_lsame(trans, 'T').and. .not. stdlib_lsame(trans, &
                     'C') ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kl<0_ilp .or. kl>n-1 ) then
              info = -3_ilp
           else if( ku<0_ilp .or. ku>n-1 ) then
              info = -4_ilp
           else if( ldab<kl+ku+1 ) then
              info = -6_ilp
           else if( ldafb<2_ilp*kl+ku+1 ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DLA_GBRCOND', -info )
              return
           end if
           if( n==0_ilp ) then
              stdlib_dla_gbrcond = one
              return
           end if
           ! compute the equilibration matrix r such that
           ! inv(r)*a*c has unit 1-norm.
           kd = ku + 1_ilp
           ke = kl + 1_ilp
           if ( notrans ) then
              do i = 1, n
                 tmp = zero
                    if ( cmode == 1_ilp ) then
                       do j = max( i-kl, 1 ), min( i+ku, n )
                          tmp = tmp + abs( ab( kd+i-j, j ) * c( j ) )
                       end do
                    else if ( cmode == 0_ilp ) then
                       do j = max( i-kl, 1 ), min( i+ku, n )
                          tmp = tmp + abs( ab( kd+i-j, j ) )
                       end do
                    else
                       do j = max( i-kl, 1 ), min( i+ku, n )
                          tmp = tmp + abs( ab( kd+i-j, j ) / c( j ) )
                       end do
                    end if
                 work( 2_ilp*n+i ) = tmp
              end do
           else
              do i = 1, n
                 tmp = zero
                 if ( cmode == 1_ilp ) then
                    do j = max( i-kl, 1 ), min( i+ku, n )
                       tmp = tmp + abs( ab( ke-i+j, i ) * c( j ) )
                    end do
                 else if ( cmode == 0_ilp ) then
                    do j = max( i-kl, 1 ), min( i+ku, n )
                       tmp = tmp + abs( ab( ke-i+j, i ) )
                    end do
                 else
                    do j = max( i-kl, 1 ), min( i+ku, n )
                       tmp = tmp + abs( ab( ke-i+j, i ) / c( j ) )
                    end do
                 end if
                 work( 2_ilp*n+i ) = tmp
              end do
           end if
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
                 if ( notrans ) then
                    call stdlib_dgbtrs( 'NO TRANSPOSE', n, kl, ku, 1_ilp, afb, ldafb,ipiv, work, n, &
                              info )
                 else
                    call stdlib_dgbtrs( 'TRANSPOSE', n, kl, ku, 1_ilp, afb, ldafb, ipiv,work, n, info &
                              )
                 end if
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
                 if ( notrans ) then
                    call stdlib_dgbtrs( 'TRANSPOSE', n, kl, ku, 1_ilp, afb, ldafb, ipiv,work, n, info &
                              )
                 else
                    call stdlib_dgbtrs( 'NO TRANSPOSE', n, kl, ku, 1_ilp, afb, ldafb,ipiv, work, n, &
                              info )
                 end if
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * work( 2_ilp*n+i )
                 end do
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm /= zero )stdlib_dla_gbrcond = ( one / ainvnm )
           return
     end function stdlib_dla_gbrcond




     pure real(sp) module function stdlib_sla_gbrpvgrw( n, kl, ku, ncols, ab, ldab, afb,ldafb )
     !! SLA_GBRPVGRW computes the reciprocal pivot growth factor
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
           integer(ilp), intent(in) :: n, kl, ku, ncols, ldab, ldafb
           ! Array Arguments 
           real(sp), intent(in) :: ab(ldab,*), afb(ldafb,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, j, kd
           real(sp) :: amax, umax, rpvgrw
           ! Intrinsic Functions 
           ! Executable Statements 
           rpvgrw = one
           kd = ku + 1_ilp
           do j = 1, ncols
              amax = zero
              umax = zero
              do i = max( j-ku, 1 ), min( j+kl, n )
                 amax = max( abs( ab( kd+i-j, j)), amax )
              end do
              do i = max( j-ku, 1 ), j
                 umax = max( abs( afb( kd+i-j, j ) ), umax )
              end do
              if ( umax /= 0.0_sp ) then
                 rpvgrw = min( amax / umax, rpvgrw )
              end if
           end do
           stdlib_sla_gbrpvgrw = rpvgrw
     end function stdlib_sla_gbrpvgrw

     pure real(dp) module function stdlib_dla_gbrpvgrw( n, kl, ku, ncols, ab,ldab, afb, ldafb )
     !! DLA_GBRPVGRW computes the reciprocal pivot growth factor
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
           integer(ilp), intent(in) :: n, kl, ku, ncols, ldab, ldafb
           ! Array Arguments 
           real(dp), intent(in) :: ab(ldab,*), afb(ldafb,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, j, kd
           real(dp) :: amax, umax, rpvgrw
           ! Intrinsic Functions 
           ! Executable Statements 
           rpvgrw = one
           kd = ku + 1_ilp
           do j = 1, ncols
              amax = zero
              umax = zero
              do i = max( j-ku, 1 ), min( j+kl, n )
                 amax = max( abs( ab( kd+i-j, j)), amax )
              end do
              do i = max( j-ku, 1 ), j
                 umax = max( abs( afb( kd+i-j, j ) ), umax )
              end do
              if ( umax /= zero ) then
                 rpvgrw = min( amax / umax, rpvgrw )
              end if
           end do
           stdlib_dla_gbrpvgrw = rpvgrw
     end function stdlib_dla_gbrpvgrw


     pure real(sp) module function stdlib_cla_gbrpvgrw( n, kl, ku, ncols, ab, ldab, afb,ldafb )
     !! CLA_GBRPVGRW computes the reciprocal pivot growth factor
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
           integer(ilp), intent(in) :: n, kl, ku, ncols, ldab, ldafb
           ! Array Arguments 
           complex(sp), intent(in) :: ab(ldab,*), afb(ldafb,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, j, kd
           real(sp) :: amax, umax, rpvgrw
           complex(sp) :: zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           rpvgrw = one
           kd = ku + 1_ilp
           do j = 1, ncols
              amax = zero
              umax = zero
              do i = max( j-ku, 1 ), min( j+kl, n )
                 amax = max( cabs1( ab( kd+i-j, j ) ), amax )
              end do
              do i = max( j-ku, 1 ), j
                 umax = max( cabs1( afb( kd+i-j, j ) ), umax )
              end do
              if ( umax /= 0.0_sp ) then
                 rpvgrw = min( amax / umax, rpvgrw )
              end if
           end do
           stdlib_cla_gbrpvgrw = rpvgrw
     end function stdlib_cla_gbrpvgrw

     pure real(dp) module function stdlib_zla_gbrpvgrw( n, kl, ku, ncols, ab,ldab, afb, ldafb )
     !! ZLA_GBRPVGRW computes the reciprocal pivot growth factor
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
           integer(ilp), intent(in) :: n, kl, ku, ncols, ldab, ldafb
           ! Array Arguments 
           complex(dp), intent(in) :: ab(ldab,*), afb(ldafb,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, j, kd
           real(dp) :: amax, umax, rpvgrw
           complex(dp) :: zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           rpvgrw = one
           kd = ku + 1_ilp
           do j = 1, ncols
              amax = zero
              umax = zero
              do i = max( j-ku, 1 ), min( j+kl, n )
                 amax = max( cabs1( ab( kd+i-j, j ) ), amax )
              end do
              do i = max( j-ku, 1 ), j
                 umax = max( cabs1( afb( kd+i-j, j ) ), umax )
              end do
              if ( umax /= zero ) then
                 rpvgrw = min( amax / umax, rpvgrw )
              end if
           end do
           stdlib_zla_gbrpvgrw = rpvgrw
     end function stdlib_zla_gbrpvgrw




     pure module subroutine stdlib_sgtcon( norm, n, dl, d, du, du2, ipiv, anorm, rcond,work, iwork, info &
     !! SGTCON estimates the reciprocal of the condition number of a real
     !! tridiagonal matrix A using the LU factorization as computed by
     !! SGTTRF.
     !! An estimate is obtained for norm(inv(A)), and the reciprocal of the
     !! condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
               )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(sp), intent(in) :: anorm
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(in) :: d(*), dl(*), du(*), du2(*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: onenrm
           integer(ilp) :: i, kase, kase1
           real(sp) :: ainvnm
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Executable Statements 
           ! test the input arguments.
           info = 0_ilp
           onenrm = norm=='1' .or. stdlib_lsame( norm, 'O' )
           if( .not.onenrm .and. .not.stdlib_lsame( norm, 'I' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( anorm<zero ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGTCON', -info )
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
           ! check that d(1:n) is non-zero.
           do i = 1, n
              if( d( i )==zero )return
           end do
           ainvnm = zero
           if( onenrm ) then
              kase1 = 1_ilp
           else
              kase1 = 2_ilp
           end if
           kase = 0_ilp
           20 continue
           call stdlib_slacn2( n, work( n+1 ), work, iwork, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              if( kase==kase1 ) then
                 ! multiply by inv(u)*inv(l).
                 call stdlib_sgttrs( 'NO TRANSPOSE', n, 1_ilp, dl, d, du, du2, ipiv,work, n, info )
                           
              else
                 ! multiply by inv(l**t)*inv(u**t).
                 call stdlib_sgttrs( 'TRANSPOSE', n, 1_ilp, dl, d, du, du2, ipiv, work,n, info )
                           
              end if
              go to 20
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm/=zero )rcond = ( one / ainvnm ) / anorm
           return
     end subroutine stdlib_sgtcon

     pure module subroutine stdlib_dgtcon( norm, n, dl, d, du, du2, ipiv, anorm, rcond,work, iwork, info &
     !! DGTCON estimates the reciprocal of the condition number of a real
     !! tridiagonal matrix A using the LU factorization as computed by
     !! DGTTRF.
     !! An estimate is obtained for norm(inv(A)), and the reciprocal of the
     !! condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
               )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(dp), intent(in) :: anorm
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(in) :: d(*), dl(*), du(*), du2(*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: onenrm
           integer(ilp) :: i, kase, kase1
           real(dp) :: ainvnm
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Executable Statements 
           ! test the input arguments.
           info = 0_ilp
           onenrm = norm=='1' .or. stdlib_lsame( norm, 'O' )
           if( .not.onenrm .and. .not.stdlib_lsame( norm, 'I' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( anorm<zero ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGTCON', -info )
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
           ! check that d(1:n) is non-zero.
           do i = 1, n
              if( d( i )==zero )return
           end do
           ainvnm = zero
           if( onenrm ) then
              kase1 = 1_ilp
           else
              kase1 = 2_ilp
           end if
           kase = 0_ilp
           20 continue
           call stdlib_dlacn2( n, work( n+1 ), work, iwork, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              if( kase==kase1 ) then
                 ! multiply by inv(u)*inv(l).
                 call stdlib_dgttrs( 'NO TRANSPOSE', n, 1_ilp, dl, d, du, du2, ipiv,work, n, info )
                           
              else
                 ! multiply by inv(l**t)*inv(u**t).
                 call stdlib_dgttrs( 'TRANSPOSE', n, 1_ilp, dl, d, du, du2, ipiv, work,n, info )
                           
              end if
              go to 20
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm/=zero )rcond = ( one / ainvnm ) / anorm
           return
     end subroutine stdlib_dgtcon


     pure module subroutine stdlib_cgtcon( norm, n, dl, d, du, du2, ipiv, anorm, rcond,work, info )
     !! CGTCON estimates the reciprocal of the condition number of a complex
     !! tridiagonal matrix A using the LU factorization as computed by
     !! CGTTRF.
     !! An estimate is obtained for norm(inv(A)), and the reciprocal of the
     !! condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(sp), intent(in) :: anorm
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(sp), intent(in) :: d(*), dl(*), du(*), du2(*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: onenrm
           integer(ilp) :: i, kase, kase1
           real(sp) :: ainvnm
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments.
           info = 0_ilp
           onenrm = norm=='1' .or. stdlib_lsame( norm, 'O' )
           if( .not.onenrm .and. .not.stdlib_lsame( norm, 'I' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( anorm<zero ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGTCON', -info )
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
           ! check that d(1:n) is non-zero.
           do i = 1, n
              if( d( i )==cmplx( zero,KIND=sp) )return
           end do
           ainvnm = zero
           if( onenrm ) then
              kase1 = 1_ilp
           else
              kase1 = 2_ilp
           end if
           kase = 0_ilp
           20 continue
           call stdlib_clacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              if( kase==kase1 ) then
                 ! multiply by inv(u)*inv(l).
                 call stdlib_cgttrs( 'NO TRANSPOSE', n, 1_ilp, dl, d, du, du2, ipiv,work, n, info )
                           
              else
                 ! multiply by inv(l**h)*inv(u**h).
                 call stdlib_cgttrs( 'CONJUGATE TRANSPOSE', n, 1_ilp, dl, d, du, du2,ipiv, work, n, &
                           info )
              end if
              go to 20
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm/=zero )rcond = ( one / ainvnm ) / anorm
           return
     end subroutine stdlib_cgtcon

     pure module subroutine stdlib_zgtcon( norm, n, dl, d, du, du2, ipiv, anorm, rcond,work, info )
     !! ZGTCON estimates the reciprocal of the condition number of a complex
     !! tridiagonal matrix A using the LU factorization as computed by
     !! ZGTTRF.
     !! An estimate is obtained for norm(inv(A)), and the reciprocal of the
     !! condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(dp), intent(in) :: anorm
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(dp), intent(in) :: d(*), dl(*), du(*), du2(*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: onenrm
           integer(ilp) :: i, kase, kase1
           real(dp) :: ainvnm
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments.
           info = 0_ilp
           onenrm = norm=='1' .or. stdlib_lsame( norm, 'O' )
           if( .not.onenrm .and. .not.stdlib_lsame( norm, 'I' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( anorm<zero ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGTCON', -info )
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
           ! check that d(1:n) is non-zero.
           do i = 1, n
              if( d( i )==cmplx( zero,KIND=dp) )return
           end do
           ainvnm = zero
           if( onenrm ) then
              kase1 = 1_ilp
           else
              kase1 = 2_ilp
           end if
           kase = 0_ilp
           20 continue
           call stdlib_zlacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              if( kase==kase1 ) then
                 ! multiply by inv(u)*inv(l).
                 call stdlib_zgttrs( 'NO TRANSPOSE', n, 1_ilp, dl, d, du, du2, ipiv,work, n, info )
                           
              else
                 ! multiply by inv(l**h)*inv(u**h).
                 call stdlib_zgttrs( 'CONJUGATE TRANSPOSE', n, 1_ilp, dl, d, du, du2,ipiv, work, n, &
                           info )
              end if
              go to 20
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm/=zero )rcond = ( one / ainvnm ) / anorm
           return
     end subroutine stdlib_zgtcon




     pure module subroutine stdlib_sgttrf( n, dl, d, du, du2, ipiv, info )
     !! SGTTRF computes an LU factorization of a real tridiagonal matrix A
     !! using elimination with partial pivoting and row interchanges.
     !! The factorization has the form
     !! A = L * U
     !! where L is a product of permutation and unit lower bidiagonal
     !! matrices and U is upper triangular with nonzeros in only the main
     !! diagonal and first two superdiagonals.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(sp), intent(inout) :: d(*), dl(*), du(*)
           real(sp), intent(out) :: du2(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i
           real(sp) :: fact, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
              call stdlib_xerbla( 'SGTTRF', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! initialize ipiv(i) = i and du2(i) = 0
           do i = 1, n
              ipiv( i ) = i
           end do
           do i = 1, n - 2
              du2( i ) = zero
           end do
           do i = 1, n - 2
              if( abs( d( i ) )>=abs( dl( i ) ) ) then
                 ! no row interchange required, eliminate dl(i)
                 if( d( i )/=zero ) then
                    fact = dl( i ) / d( i )
                    dl( i ) = fact
                    d( i+1 ) = d( i+1 ) - fact*du( i )
                 end if
              else
                 ! interchange rows i and i+1, eliminate dl(i)
                 fact = d( i ) / dl( i )
                 d( i ) = dl( i )
                 dl( i ) = fact
                 temp = du( i )
                 du( i ) = d( i+1 )
                 d( i+1 ) = temp - fact*d( i+1 )
                 du2( i ) = du( i+1 )
                 du( i+1 ) = -fact*du( i+1 )
                 ipiv( i ) = i + 1_ilp
              end if
           end do
           if( n>1_ilp ) then
              i = n - 1_ilp
              if( abs( d( i ) )>=abs( dl( i ) ) ) then
                 if( d( i )/=zero ) then
                    fact = dl( i ) / d( i )
                    dl( i ) = fact
                    d( i+1 ) = d( i+1 ) - fact*du( i )
                 end if
              else
                 fact = d( i ) / dl( i )
                 d( i ) = dl( i )
                 dl( i ) = fact
                 temp = du( i )
                 du( i ) = d( i+1 )
                 d( i+1 ) = temp - fact*d( i+1 )
                 ipiv( i ) = i + 1_ilp
              end if
           end if
           ! check for a zero on the diagonal of u.
           do i = 1, n
              if( d( i )==zero ) then
                 info = i
                 go to 50
              end if
           end do
           50 continue
           return
     end subroutine stdlib_sgttrf

     pure module subroutine stdlib_dgttrf( n, dl, d, du, du2, ipiv, info )
     !! DGTTRF computes an LU factorization of a real tridiagonal matrix A
     !! using elimination with partial pivoting and row interchanges.
     !! The factorization has the form
     !! A = L * U
     !! where L is a product of permutation and unit lower bidiagonal
     !! matrices and U is upper triangular with nonzeros in only the main
     !! diagonal and first two superdiagonals.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(dp), intent(inout) :: d(*), dl(*), du(*)
           real(dp), intent(out) :: du2(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i
           real(dp) :: fact, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
              call stdlib_xerbla( 'DGTTRF', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! initialize ipiv(i) = i and du2(i) = 0
           do i = 1, n
              ipiv( i ) = i
           end do
           do i = 1, n - 2
              du2( i ) = zero
           end do
           do i = 1, n - 2
              if( abs( d( i ) )>=abs( dl( i ) ) ) then
                 ! no row interchange required, eliminate dl(i)
                 if( d( i )/=zero ) then
                    fact = dl( i ) / d( i )
                    dl( i ) = fact
                    d( i+1 ) = d( i+1 ) - fact*du( i )
                 end if
              else
                 ! interchange rows i and i+1, eliminate dl(i)
                 fact = d( i ) / dl( i )
                 d( i ) = dl( i )
                 dl( i ) = fact
                 temp = du( i )
                 du( i ) = d( i+1 )
                 d( i+1 ) = temp - fact*d( i+1 )
                 du2( i ) = du( i+1 )
                 du( i+1 ) = -fact*du( i+1 )
                 ipiv( i ) = i + 1_ilp
              end if
           end do
           if( n>1_ilp ) then
              i = n - 1_ilp
              if( abs( d( i ) )>=abs( dl( i ) ) ) then
                 if( d( i )/=zero ) then
                    fact = dl( i ) / d( i )
                    dl( i ) = fact
                    d( i+1 ) = d( i+1 ) - fact*du( i )
                 end if
              else
                 fact = d( i ) / dl( i )
                 d( i ) = dl( i )
                 dl( i ) = fact
                 temp = du( i )
                 du( i ) = d( i+1 )
                 d( i+1 ) = temp - fact*d( i+1 )
                 ipiv( i ) = i + 1_ilp
              end if
           end if
           ! check for a zero on the diagonal of u.
           do i = 1, n
              if( d( i )==zero ) then
                 info = i
                 go to 50
              end if
           end do
           50 continue
           return
     end subroutine stdlib_dgttrf


     pure module subroutine stdlib_cgttrf( n, dl, d, du, du2, ipiv, info )
     !! CGTTRF computes an LU factorization of a complex tridiagonal matrix A
     !! using elimination with partial pivoting and row interchanges.
     !! The factorization has the form
     !! A = L * U
     !! where L is a product of permutation and unit lower bidiagonal
     !! matrices and U is upper triangular with nonzeros in only the main
     !! diagonal and first two superdiagonals.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(sp), intent(inout) :: d(*), dl(*), du(*)
           complex(sp), intent(out) :: du2(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i
           complex(sp) :: fact, temp, zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
              call stdlib_xerbla( 'CGTTRF', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! initialize ipiv(i) = i and du2(i) = 0
           do i = 1, n
              ipiv( i ) = i
           end do
           do i = 1, n - 2
              du2( i ) = zero
           end do
           do i = 1, n - 2
              if( cabs1( d( i ) )>=cabs1( dl( i ) ) ) then
                 ! no row interchange required, eliminate dl(i)
                 if( cabs1( d( i ) )/=zero ) then
                    fact = dl( i ) / d( i )
                    dl( i ) = fact
                    d( i+1 ) = d( i+1 ) - fact*du( i )
                 end if
              else
                 ! interchange rows i and i+1, eliminate dl(i)
                 fact = d( i ) / dl( i )
                 d( i ) = dl( i )
                 dl( i ) = fact
                 temp = du( i )
                 du( i ) = d( i+1 )
                 d( i+1 ) = temp - fact*d( i+1 )
                 du2( i ) = du( i+1 )
                 du( i+1 ) = -fact*du( i+1 )
                 ipiv( i ) = i + 1_ilp
              end if
           end do
           if( n>1_ilp ) then
              i = n - 1_ilp
              if( cabs1( d( i ) )>=cabs1( dl( i ) ) ) then
                 if( cabs1( d( i ) )/=zero ) then
                    fact = dl( i ) / d( i )
                    dl( i ) = fact
                    d( i+1 ) = d( i+1 ) - fact*du( i )
                 end if
              else
                 fact = d( i ) / dl( i )
                 d( i ) = dl( i )
                 dl( i ) = fact
                 temp = du( i )
                 du( i ) = d( i+1 )
                 d( i+1 ) = temp - fact*d( i+1 )
                 ipiv( i ) = i + 1_ilp
              end if
           end if
           ! check for a zero on the diagonal of u.
           do i = 1, n
              if( cabs1( d( i ) )==zero ) then
                 info = i
                 go to 50
              end if
           end do
           50 continue
           return
     end subroutine stdlib_cgttrf

     pure module subroutine stdlib_zgttrf( n, dl, d, du, du2, ipiv, info )
     !! ZGTTRF computes an LU factorization of a complex tridiagonal matrix A
     !! using elimination with partial pivoting and row interchanges.
     !! The factorization has the form
     !! A = L * U
     !! where L is a product of permutation and unit lower bidiagonal
     !! matrices and U is upper triangular with nonzeros in only the main
     !! diagonal and first two superdiagonals.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(dp), intent(inout) :: d(*), dl(*), du(*)
           complex(dp), intent(out) :: du2(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i
           complex(dp) :: fact, temp, zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
              call stdlib_xerbla( 'ZGTTRF', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! initialize ipiv(i) = i and du2(i) = 0
           do i = 1, n
              ipiv( i ) = i
           end do
           do i = 1, n - 2
              du2( i ) = zero
           end do
           do i = 1, n - 2
              if( cabs1( d( i ) )>=cabs1( dl( i ) ) ) then
                 ! no row interchange required, eliminate dl(i)
                 if( cabs1( d( i ) )/=zero ) then
                    fact = dl( i ) / d( i )
                    dl( i ) = fact
                    d( i+1 ) = d( i+1 ) - fact*du( i )
                 end if
              else
                 ! interchange rows i and i+1, eliminate dl(i)
                 fact = d( i ) / dl( i )
                 d( i ) = dl( i )
                 dl( i ) = fact
                 temp = du( i )
                 du( i ) = d( i+1 )
                 d( i+1 ) = temp - fact*d( i+1 )
                 du2( i ) = du( i+1 )
                 du( i+1 ) = -fact*du( i+1 )
                 ipiv( i ) = i + 1_ilp
              end if
           end do
           if( n>1_ilp ) then
              i = n - 1_ilp
              if( cabs1( d( i ) )>=cabs1( dl( i ) ) ) then
                 if( cabs1( d( i ) )/=zero ) then
                    fact = dl( i ) / d( i )
                    dl( i ) = fact
                    d( i+1 ) = d( i+1 ) - fact*du( i )
                 end if
              else
                 fact = d( i ) / dl( i )
                 d( i ) = dl( i )
                 dl( i ) = fact
                 temp = du( i )
                 du( i ) = d( i+1 )
                 d( i+1 ) = temp - fact*d( i+1 )
                 ipiv( i ) = i + 1_ilp
              end if
           end if
           ! check for a zero on the diagonal of u.
           do i = 1, n
              if( cabs1( d( i ) )==zero ) then
                 info = i
                 go to 50
              end if
           end do
           50 continue
           return
     end subroutine stdlib_zgttrf




     pure module subroutine stdlib_sgttrs( trans, n, nrhs, dl, d, du, du2, ipiv, b, ldb,info )
     !! SGTTRS solves one of the systems of equations
     !! A*X = B  or  A**T*X = B,
     !! with a tridiagonal matrix A using the LU factorization computed
     !! by SGTTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(sp), intent(inout) :: b(ldb,*)
           real(sp), intent(in) :: d(*), dl(*), du(*), du2(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: notran
           integer(ilp) :: itrans, j, jb, nb
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           notran = ( trans=='N' .or. trans=='N' )
           if( .not.notran .and. .not.( trans=='T' .or. trans=='T' ) .and. .not.( trans=='C' .or. &
                     trans=='C' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( ldb<max( n, 1_ilp ) ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGTTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           ! decode trans
           if( notran ) then
              itrans = 0_ilp
           else
              itrans = 1_ilp
           end if
           ! determine the number of right-hand sides to solve at a time.
           if( nrhs==1_ilp ) then
              nb = 1_ilp
           else
              nb = max( 1_ilp, stdlib_ilaenv( 1_ilp, 'SGTTRS', trans, n, nrhs, -1_ilp, -1_ilp ) )
           end if
           if( nb>=nrhs ) then
              call stdlib_sgtts2( itrans, n, nrhs, dl, d, du, du2, ipiv, b, ldb )
           else
              do j = 1, nrhs, nb
                 jb = min( nrhs-j+1, nb )
                 call stdlib_sgtts2( itrans, n, jb, dl, d, du, du2, ipiv, b( 1_ilp, j ),ldb )
              end do
           end if
     end subroutine stdlib_sgttrs

     pure module subroutine stdlib_dgttrs( trans, n, nrhs, dl, d, du, du2, ipiv, b, ldb,info )
     !! DGTTRS solves one of the systems of equations
     !! A*X = B  or  A**T*X = B,
     !! with a tridiagonal matrix A using the LU factorization computed
     !! by DGTTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(dp), intent(inout) :: b(ldb,*)
           real(dp), intent(in) :: d(*), dl(*), du(*), du2(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: notran
           integer(ilp) :: itrans, j, jb, nb
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           notran = ( trans=='N' .or. trans=='N' )
           if( .not.notran .and. .not.( trans=='T' .or. trans=='T' ) .and. .not.( trans=='C' .or. &
                     trans=='C' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( ldb<max( n, 1_ilp ) ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGTTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           ! decode trans
           if( notran ) then
              itrans = 0_ilp
           else
              itrans = 1_ilp
           end if
           ! determine the number of right-hand sides to solve at a time.
           if( nrhs==1_ilp ) then
              nb = 1_ilp
           else
              nb = max( 1_ilp, stdlib_ilaenv( 1_ilp, 'DGTTRS', trans, n, nrhs, -1_ilp, -1_ilp ) )
           end if
           if( nb>=nrhs ) then
              call stdlib_dgtts2( itrans, n, nrhs, dl, d, du, du2, ipiv, b, ldb )
           else
              do j = 1, nrhs, nb
                 jb = min( nrhs-j+1, nb )
                 call stdlib_dgtts2( itrans, n, jb, dl, d, du, du2, ipiv, b( 1_ilp, j ),ldb )
              end do
           end if
     end subroutine stdlib_dgttrs


     pure module subroutine stdlib_cgttrs( trans, n, nrhs, dl, d, du, du2, ipiv, b, ldb,info )
     !! CGTTRS solves one of the systems of equations
     !! A * X = B,  A**T * X = B,  or  A**H * X = B,
     !! with a tridiagonal matrix A using the LU factorization computed
     !! by CGTTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(sp), intent(inout) :: b(ldb,*)
           complex(sp), intent(in) :: d(*), dl(*), du(*), du2(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: notran
           integer(ilp) :: itrans, j, jb, nb
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           notran = ( trans=='N' .or. trans=='N' )
           if( .not.notran .and. .not.( trans=='T' .or. trans=='T' ) .and. .not.( trans=='C' .or. &
                     trans=='C' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( ldb<max( n, 1_ilp ) ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGTTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           ! decode trans
           if( notran ) then
              itrans = 0_ilp
           else if( trans=='T' .or. trans=='T' ) then
              itrans = 1_ilp
           else
              itrans = 2_ilp
           end if
           ! determine the number of right-hand sides to solve at a time.
           if( nrhs==1_ilp ) then
              nb = 1_ilp
           else
              nb = max( 1_ilp, stdlib_ilaenv( 1_ilp, 'CGTTRS', trans, n, nrhs, -1_ilp, -1_ilp ) )
           end if
           if( nb>=nrhs ) then
              call stdlib_cgtts2( itrans, n, nrhs, dl, d, du, du2, ipiv, b, ldb )
           else
              do j = 1, nrhs, nb
                 jb = min( nrhs-j+1, nb )
                 call stdlib_cgtts2( itrans, n, jb, dl, d, du, du2, ipiv, b( 1_ilp, j ),ldb )
              end do
           end if
     end subroutine stdlib_cgttrs

     pure module subroutine stdlib_zgttrs( trans, n, nrhs, dl, d, du, du2, ipiv, b, ldb,info )
     !! ZGTTRS solves one of the systems of equations
     !! A * X = B,  A**T * X = B,  or  A**H * X = B,
     !! with a tridiagonal matrix A using the LU factorization computed
     !! by ZGTTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(dp), intent(inout) :: b(ldb,*)
           complex(dp), intent(in) :: d(*), dl(*), du(*), du2(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: notran
           integer(ilp) :: itrans, j, jb, nb
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           notran = ( trans=='N' .or. trans=='N' )
           if( .not.notran .and. .not.( trans=='T' .or. trans=='T' ) .and. .not.( trans=='C' .or. &
                     trans=='C' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( ldb<max( n, 1_ilp ) ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGTTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           ! decode trans
           if( notran ) then
              itrans = 0_ilp
           else if( trans=='T' .or. trans=='T' ) then
              itrans = 1_ilp
           else
              itrans = 2_ilp
           end if
           ! determine the number of right-hand sides to solve at a time.
           if( nrhs==1_ilp ) then
              nb = 1_ilp
           else
              nb = max( 1_ilp, stdlib_ilaenv( 1_ilp, 'ZGTTRS', trans, n, nrhs, -1_ilp, -1_ilp ) )
           end if
           if( nb>=nrhs ) then
              call stdlib_zgtts2( itrans, n, nrhs, dl, d, du, du2, ipiv, b, ldb )
           else
              do j = 1, nrhs, nb
                 jb = min( nrhs-j+1, nb )
                 call stdlib_zgtts2( itrans, n, jb, dl, d, du, du2, ipiv, b( 1_ilp, j ),ldb )
              end do
           end if
     end subroutine stdlib_zgttrs




     pure module subroutine stdlib_sgtts2( itrans, n, nrhs, dl, d, du, du2, ipiv, b, ldb )
     !! SGTTS2 solves one of the systems of equations
     !! A*X = B  or  A**T*X = B,
     !! with a tridiagonal matrix A using the LU factorization computed
     !! by SGTTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: itrans, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(sp), intent(inout) :: b(ldb,*)
           real(sp), intent(in) :: d(*), dl(*), du(*), du2(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ip, j
           real(sp) :: temp
           ! Executable Statements 
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( itrans==0_ilp ) then
              ! solve a*x = b using the lu factorization of a,
              ! overwriting each right hand side vector with its solution.
              if( nrhs<=1_ilp ) then
                 j = 1_ilp
                 10 continue
                 ! solve l*x = b.
                 do i = 1, n - 1
                    ip = ipiv( i )
                    temp = b( i+1-ip+i, j ) - dl( i )*b( ip, j )
                    b( i, j ) = b( ip, j )
                    b( i+1, j ) = temp
                 end do
                 ! solve u*x = b.
                 b( n, j ) = b( n, j ) / d( n )
                 if( n>1_ilp )b( n-1, j ) = ( b( n-1, j )-du( n-1 )*b( n, j ) ) /d( n-1 )
                 do i = n - 2, 1, -1
                    b( i, j ) = ( b( i, j )-du( i )*b( i+1, j )-du2( i )*b( i+2, j ) ) / d( i )
                              
                 end do
                 if( j<nrhs ) then
                    j = j + 1_ilp
                    go to 10
                 end if
              else
                 do j = 1, nrhs
                    ! solve l*x = b.
                    do i = 1, n - 1
                       if( ipiv( i )==i ) then
                          b( i+1, j ) = b( i+1, j ) - dl( i )*b( i, j )
                       else
                          temp = b( i, j )
                          b( i, j ) = b( i+1, j )
                          b( i+1, j ) = temp - dl( i )*b( i, j )
                       end if
                    end do
                    ! solve u*x = b.
                    b( n, j ) = b( n, j ) / d( n )
                    if( n>1_ilp )b( n-1, j ) = ( b( n-1, j )-du( n-1 )*b( n, j ) ) /d( n-1 )
                    do i = n - 2, 1, -1
                       b( i, j ) = ( b( i, j )-du( i )*b( i+1, j )-du2( i )*b( i+2, j ) ) / d( i )
                                 
                    end do
                 end do
              end if
           else
              ! solve a**t * x = b.
              if( nrhs<=1_ilp ) then
                 ! solve u**t*x = b.
                 j = 1_ilp
                 70 continue
                 b( 1_ilp, j ) = b( 1_ilp, j ) / d( 1_ilp )
                 if( n>1_ilp )b( 2_ilp, j ) = ( b( 2_ilp, j )-du( 1_ilp )*b( 1_ilp, j ) ) / d( 2_ilp )
                 do i = 3, n
                    b( i, j ) = ( b( i, j )-du( i-1 )*b( i-1, j )-du2( i-2 )*b( i-2, j ) ) / d( i &
                              )
                 end do
                 ! solve l**t*x = b.
                 do i = n - 1, 1, -1
                    ip = ipiv( i )
                    temp = b( i, j ) - dl( i )*b( i+1, j )
                    b( i, j ) = b( ip, j )
                    b( ip, j ) = temp
                 end do
                 if( j<nrhs ) then
                    j = j + 1_ilp
                    go to 70
                 end if
              else
                 do j = 1, nrhs
                    ! solve u**t*x = b.
                    b( 1_ilp, j ) = b( 1_ilp, j ) / d( 1_ilp )
                    if( n>1_ilp )b( 2_ilp, j ) = ( b( 2_ilp, j )-du( 1_ilp )*b( 1_ilp, j ) ) / d( 2_ilp )
                    do i = 3, n
                       b( i, j ) = ( b( i, j )-du( i-1 )*b( i-1, j )-du2( i-2 )*b( i-2, j ) ) / d(&
                                  i )
                    end do
                    do i = n - 1, 1, -1
                       if( ipiv( i )==i ) then
                          b( i, j ) = b( i, j ) - dl( i )*b( i+1, j )
                       else
                          temp = b( i+1, j )
                          b( i+1, j ) = b( i, j ) - dl( i )*temp
                          b( i, j ) = temp
                       end if
                    end do
                 end do
              end if
           end if
     end subroutine stdlib_sgtts2

     pure module subroutine stdlib_dgtts2( itrans, n, nrhs, dl, d, du, du2, ipiv, b, ldb )
     !! DGTTS2 solves one of the systems of equations
     !! A*X = B  or  A**T*X = B,
     !! with a tridiagonal matrix A using the LU factorization computed
     !! by DGTTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: itrans, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(dp), intent(inout) :: b(ldb,*)
           real(dp), intent(in) :: d(*), dl(*), du(*), du2(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ip, j
           real(dp) :: temp
           ! Executable Statements 
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( itrans==0_ilp ) then
              ! solve a*x = b using the lu factorization of a,
              ! overwriting each right hand side vector with its solution.
              if( nrhs<=1_ilp ) then
                 j = 1_ilp
                 10 continue
                 ! solve l*x = b.
                 do i = 1, n - 1
                    ip = ipiv( i )
                    temp = b( i+1-ip+i, j ) - dl( i )*b( ip, j )
                    b( i, j ) = b( ip, j )
                    b( i+1, j ) = temp
                 end do
                 ! solve u*x = b.
                 b( n, j ) = b( n, j ) / d( n )
                 if( n>1_ilp )b( n-1, j ) = ( b( n-1, j )-du( n-1 )*b( n, j ) ) /d( n-1 )
                 do i = n - 2, 1, -1
                    b( i, j ) = ( b( i, j )-du( i )*b( i+1, j )-du2( i )*b( i+2, j ) ) / d( i )
                              
                 end do
                 if( j<nrhs ) then
                    j = j + 1_ilp
                    go to 10
                 end if
              else
                 do j = 1, nrhs
                    ! solve l*x = b.
                    do i = 1, n - 1
                       if( ipiv( i )==i ) then
                          b( i+1, j ) = b( i+1, j ) - dl( i )*b( i, j )
                       else
                          temp = b( i, j )
                          b( i, j ) = b( i+1, j )
                          b( i+1, j ) = temp - dl( i )*b( i, j )
                       end if
                    end do
                    ! solve u*x = b.
                    b( n, j ) = b( n, j ) / d( n )
                    if( n>1_ilp )b( n-1, j ) = ( b( n-1, j )-du( n-1 )*b( n, j ) ) /d( n-1 )
                    do i = n - 2, 1, -1
                       b( i, j ) = ( b( i, j )-du( i )*b( i+1, j )-du2( i )*b( i+2, j ) ) / d( i )
                                 
                    end do
                 end do
              end if
           else
              ! solve a**t * x = b.
              if( nrhs<=1_ilp ) then
                 ! solve u**t*x = b.
                 j = 1_ilp
                 70 continue
                 b( 1_ilp, j ) = b( 1_ilp, j ) / d( 1_ilp )
                 if( n>1_ilp )b( 2_ilp, j ) = ( b( 2_ilp, j )-du( 1_ilp )*b( 1_ilp, j ) ) / d( 2_ilp )
                 do i = 3, n
                    b( i, j ) = ( b( i, j )-du( i-1 )*b( i-1, j )-du2( i-2 )*b( i-2, j ) ) / d( i &
                              )
                 end do
                 ! solve l**t*x = b.
                 do i = n - 1, 1, -1
                    ip = ipiv( i )
                    temp = b( i, j ) - dl( i )*b( i+1, j )
                    b( i, j ) = b( ip, j )
                    b( ip, j ) = temp
                 end do
                 if( j<nrhs ) then
                    j = j + 1_ilp
                    go to 70
                 end if
              else
                 do j = 1, nrhs
                    ! solve u**t*x = b.
                    b( 1_ilp, j ) = b( 1_ilp, j ) / d( 1_ilp )
                    if( n>1_ilp )b( 2_ilp, j ) = ( b( 2_ilp, j )-du( 1_ilp )*b( 1_ilp, j ) ) / d( 2_ilp )
                    do i = 3, n
                       b( i, j ) = ( b( i, j )-du( i-1 )*b( i-1, j )-du2( i-2 )*b( i-2, j ) ) / d(&
                                  i )
                    end do
                    do i = n - 1, 1, -1
                       if( ipiv( i )==i ) then
                          b( i, j ) = b( i, j ) - dl( i )*b( i+1, j )
                       else
                          temp = b( i+1, j )
                          b( i+1, j ) = b( i, j ) - dl( i )*temp
                          b( i, j ) = temp
                       end if
                    end do
                 end do
              end if
           end if
     end subroutine stdlib_dgtts2


     pure module subroutine stdlib_cgtts2( itrans, n, nrhs, dl, d, du, du2, ipiv, b, ldb )
     !! CGTTS2 solves one of the systems of equations
     !! A * X = B,  A**T * X = B,  or  A**H * X = B,
     !! with a tridiagonal matrix A using the LU factorization computed
     !! by CGTTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: itrans, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(sp), intent(inout) :: b(ldb,*)
           complex(sp), intent(in) :: d(*), dl(*), du(*), du2(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, j
           complex(sp) :: temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( itrans==0_ilp ) then
              ! solve a*x = b using the lu factorization of a,
              ! overwriting each right hand side vector with its solution.
              if( nrhs<=1_ilp ) then
                 j = 1_ilp
                 10 continue
                 ! solve l*x = b.
                 do i = 1, n - 1
                    if( ipiv( i )==i ) then
                       b( i+1, j ) = b( i+1, j ) - dl( i )*b( i, j )
                    else
                       temp = b( i, j )
                       b( i, j ) = b( i+1, j )
                       b( i+1, j ) = temp - dl( i )*b( i, j )
                    end if
                 end do
                 ! solve u*x = b.
                 b( n, j ) = b( n, j ) / d( n )
                 if( n>1_ilp )b( n-1, j ) = ( b( n-1, j )-du( n-1 )*b( n, j ) ) /d( n-1 )
                 do i = n - 2, 1, -1
                    b( i, j ) = ( b( i, j )-du( i )*b( i+1, j )-du2( i )*b( i+2, j ) ) / d( i )
                              
                 end do
                 if( j<nrhs ) then
                    j = j + 1_ilp
                    go to 10
                 end if
              else
                 do j = 1, nrhs
                 ! solve l*x = b.
                    do i = 1, n - 1
                       if( ipiv( i )==i ) then
                          b( i+1, j ) = b( i+1, j ) - dl( i )*b( i, j )
                       else
                          temp = b( i, j )
                          b( i, j ) = b( i+1, j )
                          b( i+1, j ) = temp - dl( i )*b( i, j )
                       end if
                    end do
                 ! solve u*x = b.
                    b( n, j ) = b( n, j ) / d( n )
                    if( n>1_ilp )b( n-1, j ) = ( b( n-1, j )-du( n-1 )*b( n, j ) ) /d( n-1 )
                    do i = n - 2, 1, -1
                       b( i, j ) = ( b( i, j )-du( i )*b( i+1, j )-du2( i )*b( i+2, j ) ) / d( i )
                                 
                    end do
                 end do
              end if
           else if( itrans==1_ilp ) then
              ! solve a**t * x = b.
              if( nrhs<=1_ilp ) then
                 j = 1_ilp
                 70 continue
                 ! solve u**t * x = b.
                 b( 1_ilp, j ) = b( 1_ilp, j ) / d( 1_ilp )
                 if( n>1_ilp )b( 2_ilp, j ) = ( b( 2_ilp, j )-du( 1_ilp )*b( 1_ilp, j ) ) / d( 2_ilp )
                 do i = 3, n
                    b( i, j ) = ( b( i, j )-du( i-1 )*b( i-1, j )-du2( i-2 )*b( i-2, j ) ) / d( i &
                              )
                 end do
                 ! solve l**t * x = b.
                 do i = n - 1, 1, -1
                    if( ipiv( i )==i ) then
                       b( i, j ) = b( i, j ) - dl( i )*b( i+1, j )
                    else
                       temp = b( i+1, j )
                       b( i+1, j ) = b( i, j ) - dl( i )*temp
                       b( i, j ) = temp
                    end if
                 end do
                 if( j<nrhs ) then
                    j = j + 1_ilp
                    go to 70
                 end if
              else
                 do j = 1, nrhs
                 ! solve u**t * x = b.
                    b( 1_ilp, j ) = b( 1_ilp, j ) / d( 1_ilp )
                    if( n>1_ilp )b( 2_ilp, j ) = ( b( 2_ilp, j )-du( 1_ilp )*b( 1_ilp, j ) ) / d( 2_ilp )
                    do i = 3, n
                       b( i, j ) = ( b( i, j )-du( i-1 )*b( i-1, j )-du2( i-2 )*b( i-2, j ) ) / d(&
                                  i )
                    end do
                 ! solve l**t * x = b.
                    do i = n - 1, 1, -1
                       if( ipiv( i )==i ) then
                          b( i, j ) = b( i, j ) - dl( i )*b( i+1, j )
                       else
                          temp = b( i+1, j )
                          b( i+1, j ) = b( i, j ) - dl( i )*temp
                          b( i, j ) = temp
                       end if
                    end do
                 end do
              end if
           else
              ! solve a**h * x = b.
              if( nrhs<=1_ilp ) then
                 j = 1_ilp
                 130 continue
                 ! solve u**h * x = b.
                 b( 1_ilp, j ) = b( 1_ilp, j ) / conjg( d( 1_ilp ) )
                 if( n>1_ilp )b( 2_ilp, j ) = ( b( 2_ilp, j )-conjg( du( 1_ilp ) )*b( 1_ilp, j ) ) /conjg( d( 2_ilp ) )
                           
                 do i = 3, n
                    b( i, j ) = ( b( i, j )-conjg( du( i-1 ) )*b( i-1, j )-conjg( du2( i-2 ) )*b( &
                              i-2, j ) ) /conjg( d( i ) )
                 end do
                 ! solve l**h * x = b.
                 do i = n - 1, 1, -1
                    if( ipiv( i )==i ) then
                       b( i, j ) = b( i, j ) - conjg( dl( i ) )*b( i+1, j )
                    else
                       temp = b( i+1, j )
                       b( i+1, j ) = b( i, j ) - conjg( dl( i ) )*temp
                       b( i, j ) = temp
                    end if
                 end do
                 if( j<nrhs ) then
                    j = j + 1_ilp
                    go to 130
                 end if
              else
                 do j = 1, nrhs
                 ! solve u**h * x = b.
                    b( 1_ilp, j ) = b( 1_ilp, j ) / conjg( d( 1_ilp ) )
                    if( n>1_ilp )b( 2_ilp, j ) = ( b( 2_ilp, j )-conjg( du( 1_ilp ) )*b( 1_ilp, j ) ) /conjg( d( 2_ilp ) )
                              
                    do i = 3, n
                       b( i, j ) = ( b( i, j )-conjg( du( i-1 ) )*b( i-1, j )-conjg( du2( i-2 ) )&
                                 *b( i-2, j ) ) / conjg( d( i ) )
                    end do
                 ! solve l**h * x = b.
                    do i = n - 1, 1, -1
                       if( ipiv( i )==i ) then
                          b( i, j ) = b( i, j ) - conjg( dl( i ) )*b( i+1, j )
                       else
                          temp = b( i+1, j )
                          b( i+1, j ) = b( i, j ) - conjg( dl( i ) )*temp
                          b( i, j ) = temp
                       end if
                    end do
                 end do
              end if
           end if
     end subroutine stdlib_cgtts2

     pure module subroutine stdlib_zgtts2( itrans, n, nrhs, dl, d, du, du2, ipiv, b, ldb )
     !! ZGTTS2 solves one of the systems of equations
     !! A * X = B,  A**T * X = B,  or  A**H * X = B,
     !! with a tridiagonal matrix A using the LU factorization computed
     !! by ZGTTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: itrans, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(dp), intent(inout) :: b(ldb,*)
           complex(dp), intent(in) :: d(*), dl(*), du(*), du2(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, j
           complex(dp) :: temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( itrans==0_ilp ) then
              ! solve a*x = b using the lu factorization of a,
              ! overwriting each right hand side vector with its solution.
              if( nrhs<=1_ilp ) then
                 j = 1_ilp
                 10 continue
                 ! solve l*x = b.
                 do i = 1, n - 1
                    if( ipiv( i )==i ) then
                       b( i+1, j ) = b( i+1, j ) - dl( i )*b( i, j )
                    else
                       temp = b( i, j )
                       b( i, j ) = b( i+1, j )
                       b( i+1, j ) = temp - dl( i )*b( i, j )
                    end if
                 end do
                 ! solve u*x = b.
                 b( n, j ) = b( n, j ) / d( n )
                 if( n>1_ilp )b( n-1, j ) = ( b( n-1, j )-du( n-1 )*b( n, j ) ) /d( n-1 )
                 do i = n - 2, 1, -1
                    b( i, j ) = ( b( i, j )-du( i )*b( i+1, j )-du2( i )*b( i+2, j ) ) / d( i )
                              
                 end do
                 if( j<nrhs ) then
                    j = j + 1_ilp
                    go to 10
                 end if
              else
                 do j = 1, nrhs
                 ! solve l*x = b.
                    do i = 1, n - 1
                       if( ipiv( i )==i ) then
                          b( i+1, j ) = b( i+1, j ) - dl( i )*b( i, j )
                       else
                          temp = b( i, j )
                          b( i, j ) = b( i+1, j )
                          b( i+1, j ) = temp - dl( i )*b( i, j )
                       end if
                    end do
                 ! solve u*x = b.
                    b( n, j ) = b( n, j ) / d( n )
                    if( n>1_ilp )b( n-1, j ) = ( b( n-1, j )-du( n-1 )*b( n, j ) ) /d( n-1 )
                    do i = n - 2, 1, -1
                       b( i, j ) = ( b( i, j )-du( i )*b( i+1, j )-du2( i )*b( i+2, j ) ) / d( i )
                                 
                    end do
                 end do
              end if
           else if( itrans==1_ilp ) then
              ! solve a**t * x = b.
              if( nrhs<=1_ilp ) then
                 j = 1_ilp
                 70 continue
                 ! solve u**t * x = b.
                 b( 1_ilp, j ) = b( 1_ilp, j ) / d( 1_ilp )
                 if( n>1_ilp )b( 2_ilp, j ) = ( b( 2_ilp, j )-du( 1_ilp )*b( 1_ilp, j ) ) / d( 2_ilp )
                 do i = 3, n
                    b( i, j ) = ( b( i, j )-du( i-1 )*b( i-1, j )-du2( i-2 )*b( i-2, j ) ) / d( i &
                              )
                 end do
                 ! solve l**t * x = b.
                 do i = n - 1, 1, -1
                    if( ipiv( i )==i ) then
                       b( i, j ) = b( i, j ) - dl( i )*b( i+1, j )
                    else
                       temp = b( i+1, j )
                       b( i+1, j ) = b( i, j ) - dl( i )*temp
                       b( i, j ) = temp
                    end if
                 end do
                 if( j<nrhs ) then
                    j = j + 1_ilp
                    go to 70
                 end if
              else
                 do j = 1, nrhs
                 ! solve u**t * x = b.
                    b( 1_ilp, j ) = b( 1_ilp, j ) / d( 1_ilp )
                    if( n>1_ilp )b( 2_ilp, j ) = ( b( 2_ilp, j )-du( 1_ilp )*b( 1_ilp, j ) ) / d( 2_ilp )
                    do i = 3, n
                       b( i, j ) = ( b( i, j )-du( i-1 )*b( i-1, j )-du2( i-2 )*b( i-2, j ) ) / d(&
                                  i )
                    end do
                 ! solve l**t * x = b.
                    do i = n - 1, 1, -1
                       if( ipiv( i )==i ) then
                          b( i, j ) = b( i, j ) - dl( i )*b( i+1, j )
                       else
                          temp = b( i+1, j )
                          b( i+1, j ) = b( i, j ) - dl( i )*temp
                          b( i, j ) = temp
                       end if
                    end do
                 end do
              end if
           else
              ! solve a**h * x = b.
              if( nrhs<=1_ilp ) then
                 j = 1_ilp
                 130 continue
                 ! solve u**h * x = b.
                 b( 1_ilp, j ) = b( 1_ilp, j ) / conjg( d( 1_ilp ) )
                 if( n>1_ilp )b( 2_ilp, j ) = ( b( 2_ilp, j )-conjg( du( 1_ilp ) )*b( 1_ilp, j ) ) /conjg( d( 2_ilp ) )
                           
                 do i = 3, n
                    b( i, j ) = ( b( i, j )-conjg( du( i-1 ) )*b( i-1, j )-conjg( du2( i-2 ) )*b( &
                              i-2, j ) ) /conjg( d( i ) )
                 end do
                 ! solve l**h * x = b.
                 do i = n - 1, 1, -1
                    if( ipiv( i )==i ) then
                       b( i, j ) = b( i, j ) - conjg( dl( i ) )*b( i+1, j )
                    else
                       temp = b( i+1, j )
                       b( i+1, j ) = b( i, j ) - conjg( dl( i ) )*temp
                       b( i, j ) = temp
                    end if
                 end do
                 if( j<nrhs ) then
                    j = j + 1_ilp
                    go to 130
                 end if
              else
                 do j = 1, nrhs
                 ! solve u**h * x = b.
                    b( 1_ilp, j ) = b( 1_ilp, j ) / conjg( d( 1_ilp ) )
                    if( n>1_ilp )b( 2_ilp, j ) = ( b( 2_ilp, j )-conjg( du( 1_ilp ) )*b( 1_ilp, j ) )/ conjg( d( 2_ilp ) )
                              
                    do i = 3, n
                       b( i, j ) = ( b( i, j )-conjg( du( i-1 ) )*b( i-1, j )-conjg( du2( i-2 ) )&
                                 *b( i-2, j ) ) / conjg( d( i ) )
                    end do
                 ! solve l**h * x = b.
                    do i = n - 1, 1, -1
                       if( ipiv( i )==i ) then
                          b( i, j ) = b( i, j ) - conjg( dl( i ) )*b( i+1, j )
                       else
                          temp = b( i+1, j )
                          b( i+1, j ) = b( i, j ) - conjg( dl( i ) )*temp
                          b( i, j ) = temp
                       end if
                    end do
                 end do
              end if
           end if
     end subroutine stdlib_zgtts2




     pure module subroutine stdlib_sgtrfs( trans, n, nrhs, dl, d, du, dlf, df, duf, du2,ipiv, b, ldb, x, &
     !! SGTRFS improves the computed solution to a system of linear
     !! equations when the coefficient matrix is tridiagonal, and provides
     !! error bounds and backward error estimates for the solution.
               ldx, ferr, berr, work, iwork,info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(in) :: b(ldb,*), d(*), df(*), dl(*), dlf(*), du(*), du2(*), duf(*)
                     
           real(sp), intent(out) :: berr(*), ferr(*), work(*)
           real(sp), intent(inout) :: x(ldx,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           
           
           ! Local Scalars 
           logical(lk) :: notran
           character :: transn, transt
           integer(ilp) :: count, i, j, kase, nz
           real(sp) :: eps, lstres, s, safe1, safe2, safmin
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           notran = stdlib_lsame( trans, 'N' )
           if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( trans, &
                     'C' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -13_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -15_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGTRFS', -info )
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
           if( notran ) then
              transn = 'N'
              transt = 'T'
           else
              transn = 'T'
              transt = 'N'
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = 4_ilp
           eps = stdlib_slamch( 'EPSILON' )
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           safe1 = nz*safmin
           safe2 = safe1 / eps
           ! do for each right hand side
           loop_110: do j = 1, nrhs
              count = 1_ilp
              lstres = three
              20 continue
              ! loop until stopping criterion is satisfied.
              ! compute residual r = b - op(a) * x,
              ! where op(a) = a, a**t, or a**h, depending on trans.
              call stdlib_scopy( n, b( 1_ilp, j ), 1_ilp, work( n+1 ), 1_ilp )
              call stdlib_slagtm( trans, n, 1_ilp, -one, dl, d, du, x( 1_ilp, j ), ldx, one,work( n+1 ), &
                        n )
              ! compute abs(op(a))*abs(x) + abs(b) for use in the backward
              ! error bound.
              if( notran ) then
                 if( n==1_ilp ) then
                    work( 1_ilp ) = abs( b( 1_ilp, j ) ) + abs( d( 1_ilp )*x( 1_ilp, j ) )
                 else
                    work( 1_ilp ) = abs( b( 1_ilp, j ) ) + abs( d( 1_ilp )*x( 1_ilp, j ) ) +abs( du( 1_ilp )*x( 2_ilp, j )&
                               )
                    do i = 2, n - 1
                       work( i ) = abs( b( i, j ) ) +abs( dl( i-1 )*x( i-1, j ) ) +abs( d( i )*x( &
                                 i, j ) ) +abs( du( i )*x( i+1, j ) )
                    end do
                    work( n ) = abs( b( n, j ) ) +abs( dl( n-1 )*x( n-1, j ) ) +abs( d( n )*x( n, &
                              j ) )
                 end if
              else
                 if( n==1_ilp ) then
                    work( 1_ilp ) = abs( b( 1_ilp, j ) ) + abs( d( 1_ilp )*x( 1_ilp, j ) )
                 else
                    work( 1_ilp ) = abs( b( 1_ilp, j ) ) + abs( d( 1_ilp )*x( 1_ilp, j ) ) +abs( dl( 1_ilp )*x( 2_ilp, j )&
                               )
                    do i = 2, n - 1
                       work( i ) = abs( b( i, j ) ) +abs( du( i-1 )*x( i-1, j ) ) +abs( d( i )*x( &
                                 i, j ) ) +abs( dl( i )*x( i+1, j ) )
                    end do
                    work( n ) = abs( b( n, j ) ) +abs( du( n-1 )*x( n-1, j ) ) +abs( d( n )*x( n, &
                              j ) )
                 end if
              end if
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(op(a))*abs(x) + abs(b) )(i) )
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
                 call stdlib_sgttrs( trans, n, 1_ilp, dlf, df, duf, du2, ipiv,work( n+1 ), n, info )
                           
                 call stdlib_saxpy( n, one, work( n+1 ), 1_ilp, x( 1_ilp, j ), 1_ilp )
                 lstres = berr( j )
                 count = count + 1_ilp
                 go to 20
              end if
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(op(a)))*
                 ! ( abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(op(a)) is the inverse of op(a)
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(op(a))*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(op(a))*abs(x) + abs(b) is less than safe2.
              ! use stdlib_slacn2 to estimate the infinity-norm of the matrix
                 ! inv(op(a)) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) )))
              do i = 1, n
                 if( work( i )>safe2 ) then
                    work( i ) = abs( work( n+i ) ) + nz*eps*work( i )
                 else
                    work( i ) = abs( work( n+i ) ) + nz*eps*work( i ) + safe1
                 end if
              end do
              kase = 0_ilp
              70 continue
              call stdlib_slacn2( n, work( 2_ilp*n+1 ), work( n+1 ), iwork, ferr( j ),kase, isave )
                        
              if( kase/=0_ilp ) then
                 if( kase==1_ilp ) then
                    ! multiply by diag(w)*inv(op(a)**t).
                    call stdlib_sgttrs( transt, n, 1_ilp, dlf, df, duf, du2, ipiv,work( n+1 ), n, &
                              info )
                    do i = 1, n
                       work( n+i ) = work( i )*work( n+i )
                    end do
                 else
                    ! multiply by inv(op(a))*diag(w).
                    do i = 1, n
                       work( n+i ) = work( i )*work( n+i )
                    end do
                    call stdlib_sgttrs( transn, n, 1_ilp, dlf, df, duf, du2, ipiv,work( n+1 ), n, &
                              info )
                 end if
                 go to 70
              end if
              ! normalize error.
              lstres = zero
              do i = 1, n
                 lstres = max( lstres, abs( x( i, j ) ) )
              end do
              if( lstres/=zero )ferr( j ) = ferr( j ) / lstres
           end do loop_110
           return
     end subroutine stdlib_sgtrfs

     pure module subroutine stdlib_dgtrfs( trans, n, nrhs, dl, d, du, dlf, df, duf, du2,ipiv, b, ldb, x, &
     !! DGTRFS improves the computed solution to a system of linear
     !! equations when the coefficient matrix is tridiagonal, and provides
     !! error bounds and backward error estimates for the solution.
               ldx, ferr, berr, work, iwork,info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(in) :: b(ldb,*), d(*), df(*), dl(*), dlf(*), du(*), du2(*), duf(*)
                     
           real(dp), intent(out) :: berr(*), ferr(*), work(*)
           real(dp), intent(inout) :: x(ldx,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           
           
           ! Local Scalars 
           logical(lk) :: notran
           character :: transn, transt
           integer(ilp) :: count, i, j, kase, nz
           real(dp) :: eps, lstres, s, safe1, safe2, safmin
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           notran = stdlib_lsame( trans, 'N' )
           if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( trans, &
                     'C' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -13_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -15_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGTRFS', -info )
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
           if( notran ) then
              transn = 'N'
              transt = 'T'
           else
              transn = 'T'
              transt = 'N'
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = 4_ilp
           eps = stdlib_dlamch( 'EPSILON' )
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           safe1 = nz*safmin
           safe2 = safe1 / eps
           ! do for each right hand side
           loop_110: do j = 1, nrhs
              count = 1_ilp
              lstres = three
              20 continue
              ! loop until stopping criterion is satisfied.
              ! compute residual r = b - op(a) * x,
              ! where op(a) = a, a**t, or a**h, depending on trans.
              call stdlib_dcopy( n, b( 1_ilp, j ), 1_ilp, work( n+1 ), 1_ilp )
              call stdlib_dlagtm( trans, n, 1_ilp, -one, dl, d, du, x( 1_ilp, j ), ldx, one,work( n+1 ), &
                        n )
              ! compute abs(op(a))*abs(x) + abs(b) for use in the backward
              ! error bound.
              if( notran ) then
                 if( n==1_ilp ) then
                    work( 1_ilp ) = abs( b( 1_ilp, j ) ) + abs( d( 1_ilp )*x( 1_ilp, j ) )
                 else
                    work( 1_ilp ) = abs( b( 1_ilp, j ) ) + abs( d( 1_ilp )*x( 1_ilp, j ) ) +abs( du( 1_ilp )*x( 2_ilp, j )&
                               )
                    do i = 2, n - 1
                       work( i ) = abs( b( i, j ) ) +abs( dl( i-1 )*x( i-1, j ) ) +abs( d( i )*x( &
                                 i, j ) ) +abs( du( i )*x( i+1, j ) )
                    end do
                    work( n ) = abs( b( n, j ) ) +abs( dl( n-1 )*x( n-1, j ) ) +abs( d( n )*x( n, &
                              j ) )
                 end if
              else
                 if( n==1_ilp ) then
                    work( 1_ilp ) = abs( b( 1_ilp, j ) ) + abs( d( 1_ilp )*x( 1_ilp, j ) )
                 else
                    work( 1_ilp ) = abs( b( 1_ilp, j ) ) + abs( d( 1_ilp )*x( 1_ilp, j ) ) +abs( dl( 1_ilp )*x( 2_ilp, j )&
                               )
                    do i = 2, n - 1
                       work( i ) = abs( b( i, j ) ) +abs( du( i-1 )*x( i-1, j ) ) +abs( d( i )*x( &
                                 i, j ) ) +abs( dl( i )*x( i+1, j ) )
                    end do
                    work( n ) = abs( b( n, j ) ) +abs( du( n-1 )*x( n-1, j ) ) +abs( d( n )*x( n, &
                              j ) )
                 end if
              end if
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(op(a))*abs(x) + abs(b) )(i) )
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
                 call stdlib_dgttrs( trans, n, 1_ilp, dlf, df, duf, du2, ipiv,work( n+1 ), n, info )
                           
                 call stdlib_daxpy( n, one, work( n+1 ), 1_ilp, x( 1_ilp, j ), 1_ilp )
                 lstres = berr( j )
                 count = count + 1_ilp
                 go to 20
              end if
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(op(a)))*
                 ! ( abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(op(a)) is the inverse of op(a)
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(op(a))*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(op(a))*abs(x) + abs(b) is less than safe2.
              ! use stdlib_dlacn2 to estimate the infinity-norm of the matrix
                 ! inv(op(a)) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) )))
              do i = 1, n
                 if( work( i )>safe2 ) then
                    work( i ) = abs( work( n+i ) ) + nz*eps*work( i )
                 else
                    work( i ) = abs( work( n+i ) ) + nz*eps*work( i ) + safe1
                 end if
              end do
              kase = 0_ilp
              70 continue
              call stdlib_dlacn2( n, work( 2_ilp*n+1 ), work( n+1 ), iwork, ferr( j ),kase, isave )
                        
              if( kase/=0_ilp ) then
                 if( kase==1_ilp ) then
                    ! multiply by diag(w)*inv(op(a)**t).
                    call stdlib_dgttrs( transt, n, 1_ilp, dlf, df, duf, du2, ipiv,work( n+1 ), n, &
                              info )
                    do i = 1, n
                       work( n+i ) = work( i )*work( n+i )
                    end do
                 else
                    ! multiply by inv(op(a))*diag(w).
                    do i = 1, n
                       work( n+i ) = work( i )*work( n+i )
                    end do
                    call stdlib_dgttrs( transn, n, 1_ilp, dlf, df, duf, du2, ipiv,work( n+1 ), n, &
                              info )
                 end if
                 go to 70
              end if
              ! normalize error.
              lstres = zero
              do i = 1, n
                 lstres = max( lstres, abs( x( i, j ) ) )
              end do
              if( lstres/=zero )ferr( j ) = ferr( j ) / lstres
           end do loop_110
           return
     end subroutine stdlib_dgtrfs


     pure module subroutine stdlib_cgtrfs( trans, n, nrhs, dl, d, du, dlf, df, duf, du2,ipiv, b, ldb, x, &
     !! CGTRFS improves the computed solution to a system of linear
     !! equations when the coefficient matrix is tridiagonal, and provides
     !! error bounds and backward error estimates for the solution.
               ldx, ferr, berr, work, rwork,info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(sp), intent(out) :: berr(*), ferr(*), rwork(*)
           complex(sp), intent(in) :: b(ldb,*), d(*), df(*), dl(*), dlf(*), du(*), du2(*), duf(*)
                     
           complex(sp), intent(out) :: work(*)
           complex(sp), intent(inout) :: x(ldx,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           
           
           ! Local Scalars 
           logical(lk) :: notran
           character :: transn, transt
           integer(ilp) :: count, i, j, kase, nz
           real(sp) :: eps, lstres, s, safe1, safe2, safmin
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
           notran = stdlib_lsame( trans, 'N' )
           if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( trans, &
                     'C' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -13_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -15_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGTRFS', -info )
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
           if( notran ) then
              transn = 'N'
              transt = 'C'
           else
              transn = 'C'
              transt = 'N'
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = 4_ilp
           eps = stdlib_slamch( 'EPSILON' )
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           safe1 = nz*safmin
           safe2 = safe1 / eps
           ! do for each right hand side
           loop_110: do j = 1, nrhs
              count = 1_ilp
              lstres = three
              20 continue
              ! loop until stopping criterion is satisfied.
              ! compute residual r = b - op(a) * x,
              ! where op(a) = a, a**t, or a**h, depending on trans.
              call stdlib_ccopy( n, b( 1_ilp, j ), 1_ilp, work, 1_ilp )
              call stdlib_clagtm( trans, n, 1_ilp, -one, dl, d, du, x( 1_ilp, j ), ldx, one,work, n )
                        
              ! compute abs(op(a))*abs(x) + abs(b) for use in the backward
              ! error bound.
              if( notran ) then
                 if( n==1_ilp ) then
                    rwork( 1_ilp ) = cabs1( b( 1_ilp, j ) ) +cabs1( d( 1_ilp ) )*cabs1( x( 1_ilp, j ) )
                 else
                    rwork( 1_ilp ) = cabs1( b( 1_ilp, j ) ) +cabs1( d( 1_ilp ) )*cabs1( x( 1_ilp, j ) ) +cabs1( &
                              du( 1_ilp ) )*cabs1( x( 2_ilp, j ) )
                    do i = 2, n - 1
                       rwork( i ) = cabs1( b( i, j ) ) +cabs1( dl( i-1 ) )*cabs1( x( i-1, j ) ) +&
                       cabs1( d( i ) )*cabs1( x( i, j ) ) +cabs1( du( i ) )*cabs1( x( i+1, j ) )
                                 
                    end do
                    rwork( n ) = cabs1( b( n, j ) ) +cabs1( dl( n-1 ) )*cabs1( x( n-1, j ) ) +&
                              cabs1( d( n ) )*cabs1( x( n, j ) )
                 end if
              else
                 if( n==1_ilp ) then
                    rwork( 1_ilp ) = cabs1( b( 1_ilp, j ) ) +cabs1( d( 1_ilp ) )*cabs1( x( 1_ilp, j ) )
                 else
                    rwork( 1_ilp ) = cabs1( b( 1_ilp, j ) ) +cabs1( d( 1_ilp ) )*cabs1( x( 1_ilp, j ) ) +cabs1( &
                              dl( 1_ilp ) )*cabs1( x( 2_ilp, j ) )
                    do i = 2, n - 1
                       rwork( i ) = cabs1( b( i, j ) ) +cabs1( du( i-1 ) )*cabs1( x( i-1, j ) ) +&
                       cabs1( d( i ) )*cabs1( x( i, j ) ) +cabs1( dl( i ) )*cabs1( x( i+1, j ) )
                                 
                    end do
                    rwork( n ) = cabs1( b( n, j ) ) +cabs1( du( n-1 ) )*cabs1( x( n-1, j ) ) +&
                              cabs1( d( n ) )*cabs1( x( n, j ) )
                 end if
              end if
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(op(a))*abs(x) + abs(b) )(i) )
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
                 call stdlib_cgttrs( trans, n, 1_ilp, dlf, df, duf, du2, ipiv, work, n,info )
                 call stdlib_caxpy( n, cmplx( one,KIND=sp), work, 1_ilp, x( 1_ilp, j ), 1_ilp )
                 lstres = berr( j )
                 count = count + 1_ilp
                 go to 20
              end if
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(op(a)))*
                 ! ( abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(op(a)) is the inverse of op(a)
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(op(a))*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(op(a))*abs(x) + abs(b) is less than safe2.
              ! use stdlib_clacn2 to estimate the infinity-norm of the matrix
                 ! inv(op(a)) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) )))
              do i = 1, n
                 if( rwork( i )>safe2 ) then
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i )
                 else
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i ) +safe1
                 end if
              end do
              kase = 0_ilp
              70 continue
              call stdlib_clacn2( n, work( n+1 ), work, ferr( j ), kase, isave )
              if( kase/=0_ilp ) then
                 if( kase==1_ilp ) then
                    ! multiply by diag(w)*inv(op(a)**h).
                    call stdlib_cgttrs( transt, n, 1_ilp, dlf, df, duf, du2, ipiv, work,n, info )
                              
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                 else
                    ! multiply by inv(op(a))*diag(w).
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                    call stdlib_cgttrs( transn, n, 1_ilp, dlf, df, duf, du2, ipiv, work,n, info )
                              
                 end if
                 go to 70
              end if
              ! normalize error.
              lstres = zero
              do i = 1, n
                 lstres = max( lstres, cabs1( x( i, j ) ) )
              end do
              if( lstres/=zero )ferr( j ) = ferr( j ) / lstres
           end do loop_110
           return
     end subroutine stdlib_cgtrfs

     pure module subroutine stdlib_zgtrfs( trans, n, nrhs, dl, d, du, dlf, df, duf, du2,ipiv, b, ldb, x, &
     !! ZGTRFS improves the computed solution to a system of linear
     !! equations when the coefficient matrix is tridiagonal, and provides
     !! error bounds and backward error estimates for the solution.
               ldx, ferr, berr, work, rwork,info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(dp), intent(out) :: berr(*), ferr(*), rwork(*)
           complex(dp), intent(in) :: b(ldb,*), d(*), df(*), dl(*), dlf(*), du(*), du2(*), duf(*)
                     
           complex(dp), intent(out) :: work(*)
           complex(dp), intent(inout) :: x(ldx,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           
           
           ! Local Scalars 
           logical(lk) :: notran
           character :: transn, transt
           integer(ilp) :: count, i, j, kase, nz
           real(dp) :: eps, lstres, s, safe1, safe2, safmin
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
           notran = stdlib_lsame( trans, 'N' )
           if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( trans, &
                     'C' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -13_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -15_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGTRFS', -info )
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
           if( notran ) then
              transn = 'N'
              transt = 'C'
           else
              transn = 'C'
              transt = 'N'
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = 4_ilp
           eps = stdlib_dlamch( 'EPSILON' )
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           safe1 = nz*safmin
           safe2 = safe1 / eps
           ! do for each right hand side
           loop_110: do j = 1, nrhs
              count = 1_ilp
              lstres = three
              20 continue
              ! loop until stopping criterion is satisfied.
              ! compute residual r = b - op(a) * x,
              ! where op(a) = a, a**t, or a**h, depending on trans.
              call stdlib_zcopy( n, b( 1_ilp, j ), 1_ilp, work, 1_ilp )
              call stdlib_zlagtm( trans, n, 1_ilp, -one, dl, d, du, x( 1_ilp, j ), ldx, one,work, n )
                        
              ! compute abs(op(a))*abs(x) + abs(b) for use in the backward
              ! error bound.
              if( notran ) then
                 if( n==1_ilp ) then
                    rwork( 1_ilp ) = cabs1( b( 1_ilp, j ) ) +cabs1( d( 1_ilp ) )*cabs1( x( 1_ilp, j ) )
                 else
                    rwork( 1_ilp ) = cabs1( b( 1_ilp, j ) ) +cabs1( d( 1_ilp ) )*cabs1( x( 1_ilp, j ) ) +cabs1( &
                              du( 1_ilp ) )*cabs1( x( 2_ilp, j ) )
                    do i = 2, n - 1
                       rwork( i ) = cabs1( b( i, j ) ) +cabs1( dl( i-1 ) )*cabs1( x( i-1, j ) ) +&
                       cabs1( d( i ) )*cabs1( x( i, j ) ) +cabs1( du( i ) )*cabs1( x( i+1, j ) )
                                 
                    end do
                    rwork( n ) = cabs1( b( n, j ) ) +cabs1( dl( n-1 ) )*cabs1( x( n-1, j ) ) +&
                              cabs1( d( n ) )*cabs1( x( n, j ) )
                 end if
              else
                 if( n==1_ilp ) then
                    rwork( 1_ilp ) = cabs1( b( 1_ilp, j ) ) +cabs1( d( 1_ilp ) )*cabs1( x( 1_ilp, j ) )
                 else
                    rwork( 1_ilp ) = cabs1( b( 1_ilp, j ) ) +cabs1( d( 1_ilp ) )*cabs1( x( 1_ilp, j ) ) +cabs1( &
                              dl( 1_ilp ) )*cabs1( x( 2_ilp, j ) )
                    do i = 2, n - 1
                       rwork( i ) = cabs1( b( i, j ) ) +cabs1( du( i-1 ) )*cabs1( x( i-1, j ) ) +&
                       cabs1( d( i ) )*cabs1( x( i, j ) ) +cabs1( dl( i ) )*cabs1( x( i+1, j ) )
                                 
                    end do
                    rwork( n ) = cabs1( b( n, j ) ) +cabs1( du( n-1 ) )*cabs1( x( n-1, j ) ) +&
                              cabs1( d( n ) )*cabs1( x( n, j ) )
                 end if
              end if
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(op(a))*abs(x) + abs(b) )(i) )
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
                 call stdlib_zgttrs( trans, n, 1_ilp, dlf, df, duf, du2, ipiv, work, n,info )
                 call stdlib_zaxpy( n, cmplx( one,KIND=dp), work, 1_ilp, x( 1_ilp, j ), 1_ilp )
                 lstres = berr( j )
                 count = count + 1_ilp
                 go to 20
              end if
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(op(a)))*
                 ! ( abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(op(a)) is the inverse of op(a)
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(op(a))*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(op(a))*abs(x) + abs(b) is less than safe2.
              ! use stdlib_zlacn2 to estimate the infinity-norm of the matrix
                 ! inv(op(a)) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) )))
              do i = 1, n
                 if( rwork( i )>safe2 ) then
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i )
                 else
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i ) +safe1
                 end if
              end do
              kase = 0_ilp
              70 continue
              call stdlib_zlacn2( n, work( n+1 ), work, ferr( j ), kase, isave )
              if( kase/=0_ilp ) then
                 if( kase==1_ilp ) then
                    ! multiply by diag(w)*inv(op(a)**h).
                    call stdlib_zgttrs( transt, n, 1_ilp, dlf, df, duf, du2, ipiv, work,n, info )
                              
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                 else
                    ! multiply by inv(op(a))*diag(w).
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                    call stdlib_zgttrs( transn, n, 1_ilp, dlf, df, duf, du2, ipiv, work,n, info )
                              
                 end if
                 go to 70
              end if
              ! normalize error.
              lstres = zero
              do i = 1, n
                 lstres = max( lstres, cabs1( x( i, j ) ) )
              end do
              if( lstres/=zero )ferr( j ) = ferr( j ) / lstres
           end do loop_110
           return
     end subroutine stdlib_zgtrfs



end submodule stdlib_lapack_solve_lu_comp
