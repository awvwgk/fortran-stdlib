submodule(stdlib_lapack_eig_svd_lsq) stdlib_lapack_eigv_gen3
  implicit none


  contains

     module subroutine stdlib_slaqtr( ltran, lreal, n, t, ldt, b, w, scale, x, work,info )
     !! SLAQTR solves the real quasi-triangular system
     !! op(T)*p = scale*c,               if LREAL = .TRUE.
     !! or the complex quasi-triangular systems
     !! op(T + iB)*(p+iq) = scale*(c+id),  if LREAL = .FALSE.
     !! in real arithmetic, where T is upper quasi-triangular.
     !! If LREAL = .FALSE., then the first diagonal block of T must be
     !! 1 by 1, B is the specially structured matrix
     !! B = [ b(1) b(2) ... b(n) ]
     !! [       w            ]
     !! [           w        ]
     !! [              .     ]
     !! [                 w  ]
     !! op(A) = A or A**T, A**T denotes the transpose of
     !! matrix A.
     !! On input, X = [ c ].  On output, X = [ p ].
     !! [ d ]                  [ q ]
     !! This subroutine is designed for the condition number estimation
     !! in routine STRSNA.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: lreal, ltran
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldt, n
           real(sp), intent(out) :: scale
           real(sp), intent(in) :: w
           ! Array Arguments 
           real(sp), intent(in) :: b(*), t(ldt,*)
           real(sp), intent(out) :: work(*)
           real(sp), intent(inout) :: x(*)
       ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: notran
           integer(ilp) :: i, ierr, j, j1, j2, jnext, k, n1, n2
           real(sp) :: bignum, eps, rec, scaloc, si, smin, sminw, smlnum, sr, tjj, tmp, xj, xmax, &
                     xnorm, z
           ! Local Arrays 
           real(sp) :: d(2_ilp,2_ilp), v(2_ilp,2_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! do not test the input parameters for errors
           notran = .not.ltran
           info = 0_ilp
           ! quick return if possible
           if( n==0 )return
           ! set constants to control overflow
           eps = stdlib_slamch( 'P' )
           smlnum = stdlib_slamch( 'S' ) / eps
           bignum = one / smlnum
           xnorm = stdlib_slange( 'M', n, n, t, ldt, d )
           if( .not.lreal )xnorm = max( xnorm, abs( w ), stdlib_slange( 'M', n, 1_ilp, b, n, d ) )
                     
           smin = max( smlnum, eps*xnorm )
           ! compute 1-norm of each column of strictly upper triangular
           ! part of t to control overflow in triangular solver.
           work( 1_ilp ) = zero
           do j = 2, n
              work( j ) = stdlib_sasum( j-1, t( 1_ilp, j ), 1_ilp )
           end do
           if( .not.lreal ) then
              do i = 2, n
                 work( i ) = work( i ) + abs( b( i ) )
              end do
           end if
           n2 = 2_ilp*n
           n1 = n
           if( .not.lreal )n1 = n2
           k = stdlib_isamax( n1, x, 1_ilp )
           xmax = abs( x( k ) )
           scale = one
           if( xmax>bignum ) then
              scale = bignum / xmax
              call stdlib_sscal( n1, scale, x, 1_ilp )
              xmax = bignum
           end if
           if( lreal ) then
              if( notran ) then
                 ! solve t*p = scale*c
                 jnext = n
                 loop_30: do j = n, 1, -1
                    if( j>jnext )cycle loop_30
                    j1 = j
                    j2 = j
                    jnext = j - 1_ilp
                    if( j>1_ilp ) then
                       if( t( j, j-1 )/=zero ) then
                          j1 = j - 1_ilp
                          jnext = j - 2_ilp
                       end if
                    end if
                    if( j1==j2 ) then
                       ! meet 1 by 1 diagonal block
                       ! scale to avoid overflow when computing
                           ! x(j) = b(j)/t(j,j)
                       xj = abs( x( j1 ) )
                       tjj = abs( t( j1, j1 ) )
                       tmp = t( j1, j1 )
                       if( tjj<smin ) then
                          tmp = smin
                          tjj = smin
                          info = 1_ilp
                       end if
                       if( xj==zero )cycle loop_30
                       if( tjj<one ) then
                          if( xj>bignum*tjj ) then
                             rec = one / xj
                             call stdlib_sscal( n, rec, x, 1_ilp )
                             scale = scale*rec
                             xmax = xmax*rec
                          end if
                       end if
                       x( j1 ) = x( j1 ) / tmp
                       xj = abs( x( j1 ) )
                       ! scale x if necessary to avoid overflow when adding a
                       ! multiple of column j1 of t.
                       if( xj>one ) then
                          rec = one / xj
                          if( work( j1 )>( bignum-xmax )*rec ) then
                             call stdlib_sscal( n, rec, x, 1_ilp )
                             scale = scale*rec
                          end if
                       end if
                       if( j1>1_ilp ) then
                          call stdlib_saxpy( j1-1, -x( j1 ), t( 1_ilp, j1 ), 1_ilp, x, 1_ilp )
                          k = stdlib_isamax( j1-1, x, 1_ilp )
                          xmax = abs( x( k ) )
                       end if
                    else
                       ! meet 2 by 2 diagonal block
                       ! call 2 by 2 linear system solve, to take
                       ! care of possible overflow by scaling factor.
                       d( 1_ilp, 1_ilp ) = x( j1 )
                       d( 2_ilp, 1_ilp ) = x( j2 )
                       call stdlib_slaln2( .false., 2_ilp, 1_ilp, smin, one, t( j1, j1 ),ldt, one, one, d,&
                                  2_ilp, zero, zero, v, 2_ilp,scaloc, xnorm, ierr )
                       if( ierr/=0_ilp )info = 2_ilp
                       if( scaloc/=one ) then
                          call stdlib_sscal( n, scaloc, x, 1_ilp )
                          scale = scale*scaloc
                       end if
                       x( j1 ) = v( 1_ilp, 1_ilp )
                       x( j2 ) = v( 2_ilp, 1_ilp )
                       ! scale v(1,1) (= x(j1)) and/or v(2,1) (=x(j2))
                       ! to avoid overflow in updating right-hand side.
                       xj = max( abs( v( 1_ilp, 1_ilp ) ), abs( v( 2_ilp, 1_ilp ) ) )
                       if( xj>one ) then
                          rec = one / xj
                          if( max( work( j1 ), work( j2 ) )>( bignum-xmax )*rec ) then
                             call stdlib_sscal( n, rec, x, 1_ilp )
                             scale = scale*rec
                          end if
                       end if
                       ! update right-hand side
                       if( j1>1_ilp ) then
                          call stdlib_saxpy( j1-1, -x( j1 ), t( 1_ilp, j1 ), 1_ilp, x, 1_ilp )
                          call stdlib_saxpy( j1-1, -x( j2 ), t( 1_ilp, j2 ), 1_ilp, x, 1_ilp )
                          k = stdlib_isamax( j1-1, x, 1_ilp )
                          xmax = abs( x( k ) )
                       end if
                    end if
                 end do loop_30
              else
                 ! solve t**t*p = scale*c
                 jnext = 1_ilp
                 loop_40: do j = 1, n
                    if( j<jnext )cycle loop_40
                    j1 = j
                    j2 = j
                    jnext = j + 1_ilp
                    if( j<n ) then
                       if( t( j+1, j )/=zero ) then
                          j2 = j + 1_ilp
                          jnext = j + 2_ilp
                       end if
                    end if
                    if( j1==j2 ) then
                       ! 1 by 1 diagonal block
                       ! scale if necessary to avoid overflow in forming the
                       ! right-hand side element by inner product.
                       xj = abs( x( j1 ) )
                       if( xmax>one ) then
                          rec = one / xmax
                          if( work( j1 )>( bignum-xj )*rec ) then
                             call stdlib_sscal( n, rec, x, 1_ilp )
                             scale = scale*rec
                             xmax = xmax*rec
                          end if
                       end if
                       x( j1 ) = x( j1 ) - stdlib_sdot( j1-1, t( 1_ilp, j1 ), 1_ilp, x, 1_ilp )
                       xj = abs( x( j1 ) )
                       tjj = abs( t( j1, j1 ) )
                       tmp = t( j1, j1 )
                       if( tjj<smin ) then
                          tmp = smin
                          tjj = smin
                          info = 1_ilp
                       end if
                       if( tjj<one ) then
                          if( xj>bignum*tjj ) then
                             rec = one / xj
                             call stdlib_sscal( n, rec, x, 1_ilp )
                             scale = scale*rec
                             xmax = xmax*rec
                          end if
                       end if
                       x( j1 ) = x( j1 ) / tmp
                       xmax = max( xmax, abs( x( j1 ) ) )
                    else
                       ! 2 by 2 diagonal block
                       ! scale if necessary to avoid overflow in forming the
                       ! right-hand side elements by inner product.
                       xj = max( abs( x( j1 ) ), abs( x( j2 ) ) )
                       if( xmax>one ) then
                          rec = one / xmax
                          if( max( work( j2 ), work( j1 ) )>( bignum-xj )*rec ) then
                             call stdlib_sscal( n, rec, x, 1_ilp )
                             scale = scale*rec
                             xmax = xmax*rec
                          end if
                       end if
                       d( 1_ilp, 1_ilp ) = x( j1 ) - stdlib_sdot( j1-1, t( 1_ilp, j1 ), 1_ilp, x,1_ilp )
                       d( 2_ilp, 1_ilp ) = x( j2 ) - stdlib_sdot( j1-1, t( 1_ilp, j2 ), 1_ilp, x,1_ilp )
                       call stdlib_slaln2( .true., 2_ilp, 1_ilp, smin, one, t( j1, j1 ),ldt, one, one, d, &
                                 2_ilp, zero, zero, v, 2_ilp,scaloc, xnorm, ierr )
                       if( ierr/=0_ilp )info = 2_ilp
                       if( scaloc/=one ) then
                          call stdlib_sscal( n, scaloc, x, 1_ilp )
                          scale = scale*scaloc
                       end if
                       x( j1 ) = v( 1_ilp, 1_ilp )
                       x( j2 ) = v( 2_ilp, 1_ilp )
                       xmax = max( abs( x( j1 ) ), abs( x( j2 ) ), xmax )
                    end if
                 end do loop_40
              end if
           else
              sminw = max( eps*abs( w ), smin )
              if( notran ) then
                 ! solve (t + ib)*(p+iq) = c+id
                 jnext = n
                 loop_70: do j = n, 1, -1
                    if( j>jnext )cycle loop_70
                    j1 = j
                    j2 = j
                    jnext = j - 1_ilp
                    if( j>1_ilp ) then
                       if( t( j, j-1 )/=zero ) then
                          j1 = j - 1_ilp
                          jnext = j - 2_ilp
                       end if
                    end if
                    if( j1==j2 ) then
                       ! 1 by 1 diagonal block
                       ! scale if necessary to avoid overflow in division
                       z = w
                       if( j1==1_ilp )z = b( 1_ilp )
                       xj = abs( x( j1 ) ) + abs( x( n+j1 ) )
                       tjj = abs( t( j1, j1 ) ) + abs( z )
                       tmp = t( j1, j1 )
                       if( tjj<sminw ) then
                          tmp = sminw
                          tjj = sminw
                          info = 1_ilp
                       end if
                       if( xj==zero )cycle loop_70
                       if( tjj<one ) then
                          if( xj>bignum*tjj ) then
                             rec = one / xj
                             call stdlib_sscal( n2, rec, x, 1_ilp )
                             scale = scale*rec
                             xmax = xmax*rec
                          end if
                       end if
                       call stdlib_sladiv( x( j1 ), x( n+j1 ), tmp, z, sr, si )
                       x( j1 ) = sr
                       x( n+j1 ) = si
                       xj = abs( x( j1 ) ) + abs( x( n+j1 ) )
                       ! scale x if necessary to avoid overflow when adding a
                       ! multiple of column j1 of t.
                       if( xj>one ) then
                          rec = one / xj
                          if( work( j1 )>( bignum-xmax )*rec ) then
                             call stdlib_sscal( n2, rec, x, 1_ilp )
                             scale = scale*rec
                          end if
                       end if
                       if( j1>1_ilp ) then
                          call stdlib_saxpy( j1-1, -x( j1 ), t( 1_ilp, j1 ), 1_ilp, x, 1_ilp )
                          call stdlib_saxpy( j1-1, -x( n+j1 ), t( 1_ilp, j1 ), 1_ilp,x( n+1 ), 1_ilp )
                          x( 1_ilp ) = x( 1_ilp ) + b( j1 )*x( n+j1 )
                          x( n+1 ) = x( n+1 ) - b( j1 )*x( j1 )
                          xmax = zero
                          do k = 1, j1 - 1
                             xmax = max( xmax, abs( x( k ) )+abs( x( k+n ) ) )
                          end do
                       end if
                    else
                       ! meet 2 by 2 diagonal block
                       d( 1_ilp, 1_ilp ) = x( j1 )
                       d( 2_ilp, 1_ilp ) = x( j2 )
                       d( 1_ilp, 2_ilp ) = x( n+j1 )
                       d( 2_ilp, 2_ilp ) = x( n+j2 )
                       call stdlib_slaln2( .false., 2_ilp, 2_ilp, sminw, one, t( j1, j1 ),ldt, one, one, &
                                 d, 2_ilp, zero, -w, v, 2_ilp,scaloc, xnorm, ierr )
                       if( ierr/=0_ilp )info = 2_ilp
                       if( scaloc/=one ) then
                          call stdlib_sscal( 2_ilp*n, scaloc, x, 1_ilp )
                          scale = scaloc*scale
                       end if
                       x( j1 ) = v( 1_ilp, 1_ilp )
                       x( j2 ) = v( 2_ilp, 1_ilp )
                       x( n+j1 ) = v( 1_ilp, 2_ilp )
                       x( n+j2 ) = v( 2_ilp, 2_ilp )
                       ! scale x(j1), .... to avoid overflow in
                       ! updating right hand side.
                       xj = max( abs( v( 1_ilp, 1_ilp ) )+abs( v( 1_ilp, 2_ilp ) ),abs( v( 2_ilp, 1_ilp ) )+abs( v( 2_ilp, 2_ilp )&
                                  ) )
                       if( xj>one ) then
                          rec = one / xj
                          if( max( work( j1 ), work( j2 ) )>( bignum-xmax )*rec ) then
                             call stdlib_sscal( n2, rec, x, 1_ilp )
                             scale = scale*rec
                          end if
                       end if
                       ! update the right-hand side.
                       if( j1>1_ilp ) then
                          call stdlib_saxpy( j1-1, -x( j1 ), t( 1_ilp, j1 ), 1_ilp, x, 1_ilp )
                          call stdlib_saxpy( j1-1, -x( j2 ), t( 1_ilp, j2 ), 1_ilp, x, 1_ilp )
                          call stdlib_saxpy( j1-1, -x( n+j1 ), t( 1_ilp, j1 ), 1_ilp,x( n+1 ), 1_ilp )
                          call stdlib_saxpy( j1-1, -x( n+j2 ), t( 1_ilp, j2 ), 1_ilp,x( n+1 ), 1_ilp )
                          x( 1_ilp ) = x( 1_ilp ) + b( j1 )*x( n+j1 ) +b( j2 )*x( n+j2 )
                          x( n+1 ) = x( n+1 ) - b( j1 )*x( j1 ) -b( j2 )*x( j2 )
                          xmax = zero
                          do k = 1, j1 - 1
                             xmax = max( abs( x( k ) )+abs( x( k+n ) ),xmax )
                          end do
                       end if
                    end if
                 end do loop_70
              else
                 ! solve (t + ib)**t*(p+iq) = c+id
                 jnext = 1_ilp
                 loop_80: do j = 1, n
                    if( j<jnext )cycle loop_80
                    j1 = j
                    j2 = j
                    jnext = j + 1_ilp
                    if( j<n ) then
                       if( t( j+1, j )/=zero ) then
                          j2 = j + 1_ilp
                          jnext = j + 2_ilp
                       end if
                    end if
                    if( j1==j2 ) then
                       ! 1 by 1 diagonal block
                       ! scale if necessary to avoid overflow in forming the
                       ! right-hand side element by inner product.
                       xj = abs( x( j1 ) ) + abs( x( j1+n ) )
                       if( xmax>one ) then
                          rec = one / xmax
                          if( work( j1 )>( bignum-xj )*rec ) then
                             call stdlib_sscal( n2, rec, x, 1_ilp )
                             scale = scale*rec
                             xmax = xmax*rec
                          end if
                       end if
                       x( j1 ) = x( j1 ) - stdlib_sdot( j1-1, t( 1_ilp, j1 ), 1_ilp, x, 1_ilp )
                       x( n+j1 ) = x( n+j1 ) - stdlib_sdot( j1-1, t( 1_ilp, j1 ), 1_ilp,x( n+1 ), 1_ilp )
                                 
                       if( j1>1_ilp ) then
                          x( j1 ) = x( j1 ) - b( j1 )*x( n+1 )
                          x( n+j1 ) = x( n+j1 ) + b( j1 )*x( 1_ilp )
                       end if
                       xj = abs( x( j1 ) ) + abs( x( j1+n ) )
                       z = w
                       if( j1==1_ilp )z = b( 1_ilp )
                       ! scale if necessary to avoid overflow in
                       ! complex division
                       tjj = abs( t( j1, j1 ) ) + abs( z )
                       tmp = t( j1, j1 )
                       if( tjj<sminw ) then
                          tmp = sminw
                          tjj = sminw
                          info = 1_ilp
                       end if
                       if( tjj<one ) then
                          if( xj>bignum*tjj ) then
                             rec = one / xj
                             call stdlib_sscal( n2, rec, x, 1_ilp )
                             scale = scale*rec
                             xmax = xmax*rec
                          end if
                       end if
                       call stdlib_sladiv( x( j1 ), x( n+j1 ), tmp, -z, sr, si )
                       x( j1 ) = sr
                       x( j1+n ) = si
                       xmax = max( abs( x( j1 ) )+abs( x( j1+n ) ), xmax )
                    else
                       ! 2 by 2 diagonal block
                       ! scale if necessary to avoid overflow in forming the
                       ! right-hand side element by inner product.
                       xj = max( abs( x( j1 ) )+abs( x( n+j1 ) ),abs( x( j2 ) )+abs( x( n+j2 ) ) )
                                 
                       if( xmax>one ) then
                          rec = one / xmax
                          if( max( work( j1 ), work( j2 ) )>( bignum-xj ) / xmax ) then
                             call stdlib_sscal( n2, rec, x, 1_ilp )
                             scale = scale*rec
                             xmax = xmax*rec
                          end if
                       end if
                       d( 1_ilp, 1_ilp ) = x( j1 ) - stdlib_sdot( j1-1, t( 1_ilp, j1 ), 1_ilp, x,1_ilp )
                       d( 2_ilp, 1_ilp ) = x( j2 ) - stdlib_sdot( j1-1, t( 1_ilp, j2 ), 1_ilp, x,1_ilp )
                       d( 1_ilp, 2_ilp ) = x( n+j1 ) - stdlib_sdot( j1-1, t( 1_ilp, j1 ), 1_ilp,x( n+1 ), 1_ilp )
                                 
                       d( 2_ilp, 2_ilp ) = x( n+j2 ) - stdlib_sdot( j1-1, t( 1_ilp, j2 ), 1_ilp,x( n+1 ), 1_ilp )
                                 
                       d( 1_ilp, 1_ilp ) = d( 1_ilp, 1_ilp ) - b( j1 )*x( n+1 )
                       d( 2_ilp, 1_ilp ) = d( 2_ilp, 1_ilp ) - b( j2 )*x( n+1 )
                       d( 1_ilp, 2_ilp ) = d( 1_ilp, 2_ilp ) + b( j1 )*x( 1_ilp )
                       d( 2_ilp, 2_ilp ) = d( 2_ilp, 2_ilp ) + b( j2 )*x( 1_ilp )
                       call stdlib_slaln2( .true., 2_ilp, 2_ilp, sminw, one, t( j1, j1 ),ldt, one, one, d,&
                                  2_ilp, zero, w, v, 2_ilp,scaloc, xnorm, ierr )
                       if( ierr/=0_ilp )info = 2_ilp
                       if( scaloc/=one ) then
                          call stdlib_sscal( n2, scaloc, x, 1_ilp )
                          scale = scaloc*scale
                       end if
                       x( j1 ) = v( 1_ilp, 1_ilp )
                       x( j2 ) = v( 2_ilp, 1_ilp )
                       x( n+j1 ) = v( 1_ilp, 2_ilp )
                       x( n+j2 ) = v( 2_ilp, 2_ilp )
                       xmax = max( abs( x( j1 ) )+abs( x( n+j1 ) ),abs( x( j2 ) )+abs( x( n+j2 ) )&
                                 , xmax )
                    end if
                 end do loop_80
              end if
           end if
           return
     end subroutine stdlib_slaqtr

     module subroutine stdlib_dlaqtr( ltran, lreal, n, t, ldt, b, w, scale, x, work,info )
     !! DLAQTR solves the real quasi-triangular system
     !! op(T)*p = scale*c,               if LREAL = .TRUE.
     !! or the complex quasi-triangular systems
     !! op(T + iB)*(p+iq) = scale*(c+id),  if LREAL = .FALSE.
     !! in real arithmetic, where T is upper quasi-triangular.
     !! If LREAL = .FALSE., then the first diagonal block of T must be
     !! 1 by 1, B is the specially structured matrix
     !! B = [ b(1) b(2) ... b(n) ]
     !! [       w            ]
     !! [           w        ]
     !! [              .     ]
     !! [                 w  ]
     !! op(A) = A or A**T, A**T denotes the transpose of
     !! matrix A.
     !! On input, X = [ c ].  On output, X = [ p ].
     !! [ d ]                  [ q ]
     !! This subroutine is designed for the condition number estimation
     !! in routine DTRSNA.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: lreal, ltran
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldt, n
           real(dp), intent(out) :: scale
           real(dp), intent(in) :: w
           ! Array Arguments 
           real(dp), intent(in) :: b(*), t(ldt,*)
           real(dp), intent(out) :: work(*)
           real(dp), intent(inout) :: x(*)
       ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: notran
           integer(ilp) :: i, ierr, j, j1, j2, jnext, k, n1, n2
           real(dp) :: bignum, eps, rec, scaloc, si, smin, sminw, smlnum, sr, tjj, tmp, xj, xmax, &
                     xnorm, z
           ! Local Arrays 
           real(dp) :: d(2_ilp,2_ilp), v(2_ilp,2_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! do not test the input parameters for errors
           notran = .not.ltran
           info = 0_ilp
           ! quick return if possible
           if( n==0 )return
           ! set constants to control overflow
           eps = stdlib_dlamch( 'P' )
           smlnum = stdlib_dlamch( 'S' ) / eps
           bignum = one / smlnum
           xnorm = stdlib_dlange( 'M', n, n, t, ldt, d )
           if( .not.lreal )xnorm = max( xnorm, abs( w ), stdlib_dlange( 'M', n, 1_ilp, b, n, d ) )
                     
           smin = max( smlnum, eps*xnorm )
           ! compute 1-norm of each column of strictly upper triangular
           ! part of t to control overflow in triangular solver.
           work( 1_ilp ) = zero
           do j = 2, n
              work( j ) = stdlib_dasum( j-1, t( 1_ilp, j ), 1_ilp )
           end do
           if( .not.lreal ) then
              do i = 2, n
                 work( i ) = work( i ) + abs( b( i ) )
              end do
           end if
           n2 = 2_ilp*n
           n1 = n
           if( .not.lreal )n1 = n2
           k = stdlib_idamax( n1, x, 1_ilp )
           xmax = abs( x( k ) )
           scale = one
           if( xmax>bignum ) then
              scale = bignum / xmax
              call stdlib_dscal( n1, scale, x, 1_ilp )
              xmax = bignum
           end if
           if( lreal ) then
              if( notran ) then
                 ! solve t*p = scale*c
                 jnext = n
                 loop_30: do j = n, 1, -1
                    if( j>jnext )cycle loop_30
                    j1 = j
                    j2 = j
                    jnext = j - 1_ilp
                    if( j>1_ilp ) then
                       if( t( j, j-1 )/=zero ) then
                          j1 = j - 1_ilp
                          jnext = j - 2_ilp
                       end if
                    end if
                    if( j1==j2 ) then
                       ! meet 1 by 1 diagonal block
                       ! scale to avoid overflow when computing
                           ! x(j) = b(j)/t(j,j)
                       xj = abs( x( j1 ) )
                       tjj = abs( t( j1, j1 ) )
                       tmp = t( j1, j1 )
                       if( tjj<smin ) then
                          tmp = smin
                          tjj = smin
                          info = 1_ilp
                       end if
                       if( xj==zero )cycle loop_30
                       if( tjj<one ) then
                          if( xj>bignum*tjj ) then
                             rec = one / xj
                             call stdlib_dscal( n, rec, x, 1_ilp )
                             scale = scale*rec
                             xmax = xmax*rec
                          end if
                       end if
                       x( j1 ) = x( j1 ) / tmp
                       xj = abs( x( j1 ) )
                       ! scale x if necessary to avoid overflow when adding a
                       ! multiple of column j1 of t.
                       if( xj>one ) then
                          rec = one / xj
                          if( work( j1 )>( bignum-xmax )*rec ) then
                             call stdlib_dscal( n, rec, x, 1_ilp )
                             scale = scale*rec
                          end if
                       end if
                       if( j1>1_ilp ) then
                          call stdlib_daxpy( j1-1, -x( j1 ), t( 1_ilp, j1 ), 1_ilp, x, 1_ilp )
                          k = stdlib_idamax( j1-1, x, 1_ilp )
                          xmax = abs( x( k ) )
                       end if
                    else
                       ! meet 2 by 2 diagonal block
                       ! call 2 by 2 linear system solve, to take
                       ! care of possible overflow by scaling factor.
                       d( 1_ilp, 1_ilp ) = x( j1 )
                       d( 2_ilp, 1_ilp ) = x( j2 )
                       call stdlib_dlaln2( .false., 2_ilp, 1_ilp, smin, one, t( j1, j1 ),ldt, one, one, d,&
                                  2_ilp, zero, zero, v, 2_ilp,scaloc, xnorm, ierr )
                       if( ierr/=0_ilp )info = 2_ilp
                       if( scaloc/=one ) then
                          call stdlib_dscal( n, scaloc, x, 1_ilp )
                          scale = scale*scaloc
                       end if
                       x( j1 ) = v( 1_ilp, 1_ilp )
                       x( j2 ) = v( 2_ilp, 1_ilp )
                       ! scale v(1,1) (= x(j1)) and/or v(2,1) (=x(j2))
                       ! to avoid overflow in updating right-hand side.
                       xj = max( abs( v( 1_ilp, 1_ilp ) ), abs( v( 2_ilp, 1_ilp ) ) )
                       if( xj>one ) then
                          rec = one / xj
                          if( max( work( j1 ), work( j2 ) )>( bignum-xmax )*rec ) then
                             call stdlib_dscal( n, rec, x, 1_ilp )
                             scale = scale*rec
                          end if
                       end if
                       ! update right-hand side
                       if( j1>1_ilp ) then
                          call stdlib_daxpy( j1-1, -x( j1 ), t( 1_ilp, j1 ), 1_ilp, x, 1_ilp )
                          call stdlib_daxpy( j1-1, -x( j2 ), t( 1_ilp, j2 ), 1_ilp, x, 1_ilp )
                          k = stdlib_idamax( j1-1, x, 1_ilp )
                          xmax = abs( x( k ) )
                       end if
                    end if
                 end do loop_30
              else
                 ! solve t**t*p = scale*c
                 jnext = 1_ilp
                 loop_40: do j = 1, n
                    if( j<jnext )cycle loop_40
                    j1 = j
                    j2 = j
                    jnext = j + 1_ilp
                    if( j<n ) then
                       if( t( j+1, j )/=zero ) then
                          j2 = j + 1_ilp
                          jnext = j + 2_ilp
                       end if
                    end if
                    if( j1==j2 ) then
                       ! 1 by 1 diagonal block
                       ! scale if necessary to avoid overflow in forming the
                       ! right-hand side element by inner product.
                       xj = abs( x( j1 ) )
                       if( xmax>one ) then
                          rec = one / xmax
                          if( work( j1 )>( bignum-xj )*rec ) then
                             call stdlib_dscal( n, rec, x, 1_ilp )
                             scale = scale*rec
                             xmax = xmax*rec
                          end if
                       end if
                       x( j1 ) = x( j1 ) - stdlib_ddot( j1-1, t( 1_ilp, j1 ), 1_ilp, x, 1_ilp )
                       xj = abs( x( j1 ) )
                       tjj = abs( t( j1, j1 ) )
                       tmp = t( j1, j1 )
                       if( tjj<smin ) then
                          tmp = smin
                          tjj = smin
                          info = 1_ilp
                       end if
                       if( tjj<one ) then
                          if( xj>bignum*tjj ) then
                             rec = one / xj
                             call stdlib_dscal( n, rec, x, 1_ilp )
                             scale = scale*rec
                             xmax = xmax*rec
                          end if
                       end if
                       x( j1 ) = x( j1 ) / tmp
                       xmax = max( xmax, abs( x( j1 ) ) )
                    else
                       ! 2 by 2 diagonal block
                       ! scale if necessary to avoid overflow in forming the
                       ! right-hand side elements by inner product.
                       xj = max( abs( x( j1 ) ), abs( x( j2 ) ) )
                       if( xmax>one ) then
                          rec = one / xmax
                          if( max( work( j2 ), work( j1 ) )>( bignum-xj )*rec ) then
                             call stdlib_dscal( n, rec, x, 1_ilp )
                             scale = scale*rec
                             xmax = xmax*rec
                          end if
                       end if
                       d( 1_ilp, 1_ilp ) = x( j1 ) - stdlib_ddot( j1-1, t( 1_ilp, j1 ), 1_ilp, x,1_ilp )
                       d( 2_ilp, 1_ilp ) = x( j2 ) - stdlib_ddot( j1-1, t( 1_ilp, j2 ), 1_ilp, x,1_ilp )
                       call stdlib_dlaln2( .true., 2_ilp, 1_ilp, smin, one, t( j1, j1 ),ldt, one, one, d, &
                                 2_ilp, zero, zero, v, 2_ilp,scaloc, xnorm, ierr )
                       if( ierr/=0_ilp )info = 2_ilp
                       if( scaloc/=one ) then
                          call stdlib_dscal( n, scaloc, x, 1_ilp )
                          scale = scale*scaloc
                       end if
                       x( j1 ) = v( 1_ilp, 1_ilp )
                       x( j2 ) = v( 2_ilp, 1_ilp )
                       xmax = max( abs( x( j1 ) ), abs( x( j2 ) ), xmax )
                    end if
                 end do loop_40
              end if
           else
              sminw = max( eps*abs( w ), smin )
              if( notran ) then
                 ! solve (t + ib)*(p+iq) = c+id
                 jnext = n
                 loop_70: do j = n, 1, -1
                    if( j>jnext )cycle loop_70
                    j1 = j
                    j2 = j
                    jnext = j - 1_ilp
                    if( j>1_ilp ) then
                       if( t( j, j-1 )/=zero ) then
                          j1 = j - 1_ilp
                          jnext = j - 2_ilp
                       end if
                    end if
                    if( j1==j2 ) then
                       ! 1 by 1 diagonal block
                       ! scale if necessary to avoid overflow in division
                       z = w
                       if( j1==1_ilp )z = b( 1_ilp )
                       xj = abs( x( j1 ) ) + abs( x( n+j1 ) )
                       tjj = abs( t( j1, j1 ) ) + abs( z )
                       tmp = t( j1, j1 )
                       if( tjj<sminw ) then
                          tmp = sminw
                          tjj = sminw
                          info = 1_ilp
                       end if
                       if( xj==zero )cycle loop_70
                       if( tjj<one ) then
                          if( xj>bignum*tjj ) then
                             rec = one / xj
                             call stdlib_dscal( n2, rec, x, 1_ilp )
                             scale = scale*rec
                             xmax = xmax*rec
                          end if
                       end if
                       call stdlib_dladiv( x( j1 ), x( n+j1 ), tmp, z, sr, si )
                       x( j1 ) = sr
                       x( n+j1 ) = si
                       xj = abs( x( j1 ) ) + abs( x( n+j1 ) )
                       ! scale x if necessary to avoid overflow when adding a
                       ! multiple of column j1 of t.
                       if( xj>one ) then
                          rec = one / xj
                          if( work( j1 )>( bignum-xmax )*rec ) then
                             call stdlib_dscal( n2, rec, x, 1_ilp )
                             scale = scale*rec
                          end if
                       end if
                       if( j1>1_ilp ) then
                          call stdlib_daxpy( j1-1, -x( j1 ), t( 1_ilp, j1 ), 1_ilp, x, 1_ilp )
                          call stdlib_daxpy( j1-1, -x( n+j1 ), t( 1_ilp, j1 ), 1_ilp,x( n+1 ), 1_ilp )
                          x( 1_ilp ) = x( 1_ilp ) + b( j1 )*x( n+j1 )
                          x( n+1 ) = x( n+1 ) - b( j1 )*x( j1 )
                          xmax = zero
                          do k = 1, j1 - 1
                             xmax = max( xmax, abs( x( k ) )+abs( x( k+n ) ) )
                          end do
                       end if
                    else
                       ! meet 2 by 2 diagonal block
                       d( 1_ilp, 1_ilp ) = x( j1 )
                       d( 2_ilp, 1_ilp ) = x( j2 )
                       d( 1_ilp, 2_ilp ) = x( n+j1 )
                       d( 2_ilp, 2_ilp ) = x( n+j2 )
                       call stdlib_dlaln2( .false., 2_ilp, 2_ilp, sminw, one, t( j1, j1 ),ldt, one, one, &
                                 d, 2_ilp, zero, -w, v, 2_ilp,scaloc, xnorm, ierr )
                       if( ierr/=0_ilp )info = 2_ilp
                       if( scaloc/=one ) then
                          call stdlib_dscal( 2_ilp*n, scaloc, x, 1_ilp )
                          scale = scaloc*scale
                       end if
                       x( j1 ) = v( 1_ilp, 1_ilp )
                       x( j2 ) = v( 2_ilp, 1_ilp )
                       x( n+j1 ) = v( 1_ilp, 2_ilp )
                       x( n+j2 ) = v( 2_ilp, 2_ilp )
                       ! scale x(j1), .... to avoid overflow in
                       ! updating right hand side.
                       xj = max( abs( v( 1_ilp, 1_ilp ) )+abs( v( 1_ilp, 2_ilp ) ),abs( v( 2_ilp, 1_ilp ) )+abs( v( 2_ilp, 2_ilp )&
                                  ) )
                       if( xj>one ) then
                          rec = one / xj
                          if( max( work( j1 ), work( j2 ) )>( bignum-xmax )*rec ) then
                             call stdlib_dscal( n2, rec, x, 1_ilp )
                             scale = scale*rec
                          end if
                       end if
                       ! update the right-hand side.
                       if( j1>1_ilp ) then
                          call stdlib_daxpy( j1-1, -x( j1 ), t( 1_ilp, j1 ), 1_ilp, x, 1_ilp )
                          call stdlib_daxpy( j1-1, -x( j2 ), t( 1_ilp, j2 ), 1_ilp, x, 1_ilp )
                          call stdlib_daxpy( j1-1, -x( n+j1 ), t( 1_ilp, j1 ), 1_ilp,x( n+1 ), 1_ilp )
                          call stdlib_daxpy( j1-1, -x( n+j2 ), t( 1_ilp, j2 ), 1_ilp,x( n+1 ), 1_ilp )
                          x( 1_ilp ) = x( 1_ilp ) + b( j1 )*x( n+j1 ) +b( j2 )*x( n+j2 )
                          x( n+1 ) = x( n+1 ) - b( j1 )*x( j1 ) -b( j2 )*x( j2 )
                          xmax = zero
                          do k = 1, j1 - 1
                             xmax = max( abs( x( k ) )+abs( x( k+n ) ),xmax )
                          end do
                       end if
                    end if
                 end do loop_70
              else
                 ! solve (t + ib)**t*(p+iq) = c+id
                 jnext = 1_ilp
                 loop_80: do j = 1, n
                    if( j<jnext )cycle loop_80
                    j1 = j
                    j2 = j
                    jnext = j + 1_ilp
                    if( j<n ) then
                       if( t( j+1, j )/=zero ) then
                          j2 = j + 1_ilp
                          jnext = j + 2_ilp
                       end if
                    end if
                    if( j1==j2 ) then
                       ! 1 by 1 diagonal block
                       ! scale if necessary to avoid overflow in forming the
                       ! right-hand side element by inner product.
                       xj = abs( x( j1 ) ) + abs( x( j1+n ) )
                       if( xmax>one ) then
                          rec = one / xmax
                          if( work( j1 )>( bignum-xj )*rec ) then
                             call stdlib_dscal( n2, rec, x, 1_ilp )
                             scale = scale*rec
                             xmax = xmax*rec
                          end if
                       end if
                       x( j1 ) = x( j1 ) - stdlib_ddot( j1-1, t( 1_ilp, j1 ), 1_ilp, x, 1_ilp )
                       x( n+j1 ) = x( n+j1 ) - stdlib_ddot( j1-1, t( 1_ilp, j1 ), 1_ilp,x( n+1 ), 1_ilp )
                                 
                       if( j1>1_ilp ) then
                          x( j1 ) = x( j1 ) - b( j1 )*x( n+1 )
                          x( n+j1 ) = x( n+j1 ) + b( j1 )*x( 1_ilp )
                       end if
                       xj = abs( x( j1 ) ) + abs( x( j1+n ) )
                       z = w
                       if( j1==1_ilp )z = b( 1_ilp )
                       ! scale if necessary to avoid overflow in
                       ! complex division
                       tjj = abs( t( j1, j1 ) ) + abs( z )
                       tmp = t( j1, j1 )
                       if( tjj<sminw ) then
                          tmp = sminw
                          tjj = sminw
                          info = 1_ilp
                       end if
                       if( tjj<one ) then
                          if( xj>bignum*tjj ) then
                             rec = one / xj
                             call stdlib_dscal( n2, rec, x, 1_ilp )
                             scale = scale*rec
                             xmax = xmax*rec
                          end if
                       end if
                       call stdlib_dladiv( x( j1 ), x( n+j1 ), tmp, -z, sr, si )
                       x( j1 ) = sr
                       x( j1+n ) = si
                       xmax = max( abs( x( j1 ) )+abs( x( j1+n ) ), xmax )
                    else
                       ! 2 by 2 diagonal block
                       ! scale if necessary to avoid overflow in forming the
                       ! right-hand side element by inner product.
                       xj = max( abs( x( j1 ) )+abs( x( n+j1 ) ),abs( x( j2 ) )+abs( x( n+j2 ) ) )
                                 
                       if( xmax>one ) then
                          rec = one / xmax
                          if( max( work( j1 ), work( j2 ) )>( bignum-xj ) / xmax ) then
                             call stdlib_dscal( n2, rec, x, 1_ilp )
                             scale = scale*rec
                             xmax = xmax*rec
                          end if
                       end if
                       d( 1_ilp, 1_ilp ) = x( j1 ) - stdlib_ddot( j1-1, t( 1_ilp, j1 ), 1_ilp, x,1_ilp )
                       d( 2_ilp, 1_ilp ) = x( j2 ) - stdlib_ddot( j1-1, t( 1_ilp, j2 ), 1_ilp, x,1_ilp )
                       d( 1_ilp, 2_ilp ) = x( n+j1 ) - stdlib_ddot( j1-1, t( 1_ilp, j1 ), 1_ilp,x( n+1 ), 1_ilp )
                                 
                       d( 2_ilp, 2_ilp ) = x( n+j2 ) - stdlib_ddot( j1-1, t( 1_ilp, j2 ), 1_ilp,x( n+1 ), 1_ilp )
                                 
                       d( 1_ilp, 1_ilp ) = d( 1_ilp, 1_ilp ) - b( j1 )*x( n+1 )
                       d( 2_ilp, 1_ilp ) = d( 2_ilp, 1_ilp ) - b( j2 )*x( n+1 )
                       d( 1_ilp, 2_ilp ) = d( 1_ilp, 2_ilp ) + b( j1 )*x( 1_ilp )
                       d( 2_ilp, 2_ilp ) = d( 2_ilp, 2_ilp ) + b( j2 )*x( 1_ilp )
                       call stdlib_dlaln2( .true., 2_ilp, 2_ilp, sminw, one, t( j1, j1 ),ldt, one, one, d,&
                                  2_ilp, zero, w, v, 2_ilp,scaloc, xnorm, ierr )
                       if( ierr/=0_ilp )info = 2_ilp
                       if( scaloc/=one ) then
                          call stdlib_dscal( n2, scaloc, x, 1_ilp )
                          scale = scaloc*scale
                       end if
                       x( j1 ) = v( 1_ilp, 1_ilp )
                       x( j2 ) = v( 2_ilp, 1_ilp )
                       x( n+j1 ) = v( 1_ilp, 2_ilp )
                       x( n+j2 ) = v( 2_ilp, 2_ilp )
                       xmax = max( abs( x( j1 ) )+abs( x( n+j1 ) ),abs( x( j2 ) )+abs( x( n+j2 ) )&
                                 , xmax )
                    end if
                 end do loop_80
              end if
           end if
           return
     end subroutine stdlib_dlaqtr




     pure module subroutine stdlib_slahqr( wantt, wantz, n, ilo, ihi, h, ldh, wr, wi,iloz, ihiz, z, ldz, &
     !! SLAHQR is an auxiliary routine called by SHSEQR to update the
     !! eigenvalues and Schur decomposition already computed by SHSEQR, by
     !! dealing with the Hessenberg submatrix in rows and columns ILO to
     !! IHI.
               info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ihi, ihiz, ilo, iloz, ldh, ldz, n
           integer(ilp), intent(out) :: info
           logical(lk), intent(in) :: wantt, wantz
           ! Array Arguments 
           real(sp), intent(inout) :: h(ldh,*), z(ldz,*)
           real(sp), intent(out) :: wi(*), wr(*)
        ! =========================================================
           ! Parameters 
           real(sp), parameter :: dat1 = 3.0_sp/4.0_sp
           real(sp), parameter :: dat2 = -0.4375_sp
           integer(ilp), parameter :: kexsh = 10_ilp
           
           
           
           ! Local Scalars 
           real(sp) :: aa, ab, ba, bb, cs, det, h11, h12, h21, h21s, h22, rt1i, rt1r, rt2i, rt2r, &
                     rtdisc, s, safmax, safmin, smlnum, sn, sum, t1, t2, t3, tr, tst, ulp, v2, v3
           integer(ilp) :: i, i1, i2, its, itmax, j, k, l, m, nh, nr, nz, kdefl
           ! Local Arrays 
           real(sp) :: v(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           ! quick return if possible
           if( n==0 )return
           if( ilo==ihi ) then
              wr( ilo ) = h( ilo, ilo )
              wi( ilo ) = zero
              return
           end if
           ! ==== clear out the trash ====
           do j = ilo, ihi - 3
              h( j+2, j ) = zero
              h( j+3, j ) = zero
           end do
           if( ilo<=ihi-2 )h( ihi, ihi-2 ) = zero
           nh = ihi - ilo + 1_ilp
           nz = ihiz - iloz + 1_ilp
           ! set machine-dependent constants for the stopping criterion.
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           safmax = one / safmin
           call stdlib_slabad( safmin, safmax )
           ulp = stdlib_slamch( 'PRECISION' )
           smlnum = safmin*( real( nh,KIND=sp) / ulp )
           ! i1 and i2 are the indices of the first row and last column of h
           ! to which transformations must be applied. if eigenvalues only are
           ! being computed, i1 and i2 are set inside the main loop.
           if( wantt ) then
              i1 = 1_ilp
              i2 = n
           end if
           ! itmax is the total number of qr iterations allowed.
           itmax = 30_ilp * max( 10_ilp, nh )
           ! kdefl counts the number of iterations since a deflation
           kdefl = 0_ilp
           ! the main loop begins here. i is the loop index and decreases from
           ! ihi to ilo in steps of 1 or 2. each iteration of the loop works
           ! with the active submatrix in rows and columns l to i.
           ! eigenvalues i+1 to ihi have already converged. either l = ilo or
           ! h(l,l-1) is negligible so that the matrix splits.
           i = ihi
           20 continue
           l = ilo
           if( i<ilo )go to 160
           ! perform qr iterations on rows and columns ilo to i until a
           ! submatrix of order 1 or 2 splits off at the bottom because a
           ! subdiagonal element has become negligible.
           loop_140: do its = 0, itmax
              ! look for a single small subdiagonal element.
              do k = i, l + 1, -1
                 if( abs( h( k, k-1 ) )<=smlnum )go to 40
                 tst = abs( h( k-1, k-1 ) ) + abs( h( k, k ) )
                 if( tst==zero ) then
                    if( k-2>=ilo )tst = tst + abs( h( k-1, k-2 ) )
                    if( k+1<=ihi )tst = tst + abs( h( k+1, k ) )
                 end if
                 ! ==== the following is a conservative small subdiagonal
                 ! .    deflation  criterion due to ahues
                 ! .    1997). it has better mathematical foundation and
                 ! .    improves accuracy in some cases.  ====
                 if( abs( h( k, k-1 ) )<=ulp*tst ) then
                    ab = max( abs( h( k, k-1 ) ), abs( h( k-1, k ) ) )
                    ba = min( abs( h( k, k-1 ) ), abs( h( k-1, k ) ) )
                    aa = max( abs( h( k, k ) ),abs( h( k-1, k-1 )-h( k, k ) ) )
                    bb = min( abs( h( k, k ) ),abs( h( k-1, k-1 )-h( k, k ) ) )
                    s = aa + ab
                    if( ba*( ab / s )<=max( smlnum,ulp*( bb*( aa / s ) ) ) )go to 40
                 end if
              end do
              40 continue
              l = k
              if( l>ilo ) then
                 ! h(l,l-1) is negligible
                 h( l, l-1 ) = zero
              end if
              ! exit from loop if a submatrix of order 1 or 2 has split off.
              if( l>=i-1 )go to 150
              kdefl = kdefl + 1_ilp
              ! now the active submatrix is in rows and columns l to i. if
              ! eigenvalues only are being computed, only the active submatrix
              ! need be transformed.
              if( .not.wantt ) then
                 i1 = l
                 i2 = i
              end if
              if( mod(kdefl,2_ilp*kexsh)==0_ilp ) then
                 ! exceptional shift.
                 s = abs( h( i, i-1 ) ) + abs( h( i-1, i-2 ) )
                 h11 = dat1*s + h( i, i )
                 h12 = dat2*s
                 h21 = s
                 h22 = h11
              else if( mod(kdefl,kexsh)==0_ilp ) then
                 ! exceptional shift.
                 s = abs( h( l+1, l ) ) + abs( h( l+2, l+1 ) )
                 h11 = dat1*s + h( l, l )
                 h12 = dat2*s
                 h21 = s
                 h22 = h11
              else
                 ! prepare to use francis' double shift
                 ! (i.e. 2nd degree generalized rayleigh quotient)
                 h11 = h( i-1, i-1 )
                 h21 = h( i, i-1 )
                 h12 = h( i-1, i )
                 h22 = h( i, i )
              end if
              s = abs( h11 ) + abs( h12 ) + abs( h21 ) + abs( h22 )
              if( s==zero ) then
                 rt1r = zero
                 rt1i = zero
                 rt2r = zero
                 rt2i = zero
              else
                 h11 = h11 / s
                 h21 = h21 / s
                 h12 = h12 / s
                 h22 = h22 / s
                 tr = ( h11+h22 ) / two
                 det = ( h11-tr )*( h22-tr ) - h12*h21
                 rtdisc = sqrt( abs( det ) )
                 if( det>=zero ) then
                    ! ==== complex conjugate shifts ====
                    rt1r = tr*s
                    rt2r = rt1r
                    rt1i = rtdisc*s
                    rt2i = -rt1i
                 else
                    ! ==== realshifts (use only one of them,KIND=sp)  ====
                    rt1r = tr + rtdisc
                    rt2r = tr - rtdisc
                    if( abs( rt1r-h22 )<=abs( rt2r-h22 ) ) then
                       rt1r = rt1r*s
                       rt2r = rt1r
                    else
                       rt2r = rt2r*s
                       rt1r = rt2r
                    end if
                    rt1i = zero
                    rt2i = zero
                 end if
              end if
              ! look for two consecutive small subdiagonal elements.
              do m = i - 2, l, -1
                 ! determine the effect of starting the double-shift qr
                 ! iteration at row m, and see if this would make h(m,m-1)
                 ! negligible.  (the following uses scaling to avoid
                 ! overflows and most underflows.)
                 h21s = h( m+1, m )
                 s = abs( h( m, m )-rt2r ) + abs( rt2i ) + abs( h21s )
                 h21s = h( m+1, m ) / s
                 v( 1_ilp ) = h21s*h( m, m+1 ) + ( h( m, m )-rt1r )*( ( h( m, m )-rt2r ) / s ) - &
                           rt1i*( rt2i / s )
                 v( 2_ilp ) = h21s*( h( m, m )+h( m+1, m+1 )-rt1r-rt2r )
                 v( 3_ilp ) = h21s*h( m+2, m+1 )
                 s = abs( v( 1_ilp ) ) + abs( v( 2_ilp ) ) + abs( v( 3_ilp ) )
                 v( 1_ilp ) = v( 1_ilp ) / s
                 v( 2_ilp ) = v( 2_ilp ) / s
                 v( 3_ilp ) = v( 3_ilp ) / s
                 if( m==l )go to 60
                 if( abs( h( m, m-1 ) )*( abs( v( 2_ilp ) )+abs( v( 3_ilp ) ) )<=ulp*abs( v( 1_ilp ) )*( abs( &
                           h( m-1, m-1 ) )+abs( h( m,m ) )+abs( h( m+1, m+1 ) ) ) )go to 60
              end do
              60 continue
              ! double-shift qr step
              loop_130: do k = m, i - 1
                 ! the first iteration of this loop determines a reflection g
                 ! from the vector v and applies it from left and right to h,
                 ! thus creating a nonzero bulge below the subdiagonal.
                 ! each subsequent iteration determines a reflection g to
                 ! restore the hessenberg form in the (k-1)th column, and thus
                 ! chases the bulge one step toward the bottom of the active
                 ! submatrix. nr is the order of g.
                 nr = min( 3_ilp, i-k+1 )
                 if( k>m )call stdlib_scopy( nr, h( k, k-1 ), 1_ilp, v, 1_ilp )
                 call stdlib_slarfg( nr, v( 1_ilp ), v( 2_ilp ), 1_ilp, t1 )
                 if( k>m ) then
                    h( k, k-1 ) = v( 1_ilp )
                    h( k+1, k-1 ) = zero
                    if( k<i-1 )h( k+2, k-1 ) = zero
                 else if( m>l ) then
                     ! ==== use the following instead of
                     ! .    h( k, k-1 ) = -h( k, k-1 ) to
                     ! .    avoid a bug when v(2) and v(3)
                     ! .    underflow. ====
                    h( k, k-1 ) = h( k, k-1 )*( one-t1 )
                 end if
                 v2 = v( 2_ilp )
                 t2 = t1*v2
                 if( nr==3_ilp ) then
                    v3 = v( 3_ilp )
                    t3 = t1*v3
                    ! apply g from the left to transform the rows of the matrix
                    ! in columns k to i2.
                    do j = k, i2
                       sum = h( k, j ) + v2*h( k+1, j ) + v3*h( k+2, j )
                       h( k, j ) = h( k, j ) - sum*t1
                       h( k+1, j ) = h( k+1, j ) - sum*t2
                       h( k+2, j ) = h( k+2, j ) - sum*t3
                    end do
                    ! apply g from the right to transform the columns of the
                    ! matrix in rows i1 to min(k+3,i).
                    do j = i1, min( k+3, i )
                       sum = h( j, k ) + v2*h( j, k+1 ) + v3*h( j, k+2 )
                       h( j, k ) = h( j, k ) - sum*t1
                       h( j, k+1 ) = h( j, k+1 ) - sum*t2
                       h( j, k+2 ) = h( j, k+2 ) - sum*t3
                    end do
                    if( wantz ) then
                       ! accumulate transformations in the matrix z
                       do j = iloz, ihiz
                          sum = z( j, k ) + v2*z( j, k+1 ) + v3*z( j, k+2 )
                          z( j, k ) = z( j, k ) - sum*t1
                          z( j, k+1 ) = z( j, k+1 ) - sum*t2
                          z( j, k+2 ) = z( j, k+2 ) - sum*t3
                       end do
                    end if
                 else if( nr==2_ilp ) then
                    ! apply g from the left to transform the rows of the matrix
                    ! in columns k to i2.
                    do j = k, i2
                       sum = h( k, j ) + v2*h( k+1, j )
                       h( k, j ) = h( k, j ) - sum*t1
                       h( k+1, j ) = h( k+1, j ) - sum*t2
                    end do
                    ! apply g from the right to transform the columns of the
                    ! matrix in rows i1 to min(k+3,i).
                    do j = i1, i
                       sum = h( j, k ) + v2*h( j, k+1 )
                       h( j, k ) = h( j, k ) - sum*t1
                       h( j, k+1 ) = h( j, k+1 ) - sum*t2
                    end do
                    if( wantz ) then
                       ! accumulate transformations in the matrix z
                       do j = iloz, ihiz
                          sum = z( j, k ) + v2*z( j, k+1 )
                          z( j, k ) = z( j, k ) - sum*t1
                          z( j, k+1 ) = z( j, k+1 ) - sum*t2
                       end do
                    end if
                 end if
              end do loop_130
           end do loop_140
           ! failure to converge in remaining number of iterations
           info = i
           return
           150 continue
           if( l==i ) then
              ! h(i,i-1) is negligible: one eigenvalue has converged.
              wr( i ) = h( i, i )
              wi( i ) = zero
           else if( l==i-1 ) then
              ! h(i-1,i-2) is negligible: a pair of eigenvalues have converged.
              ! transform the 2-by-2 submatrix to standard schur form,
              ! and compute and store the eigenvalues.
              call stdlib_slanv2( h( i-1, i-1 ), h( i-1, i ), h( i, i-1 ),h( i, i ), wr( i-1 ), &
                        wi( i-1 ), wr( i ), wi( i ),cs, sn )
              if( wantt ) then
                 ! apply the transformation to the rest of h.
                 if( i2>i )call stdlib_srot( i2-i, h( i-1, i+1 ), ldh, h( i, i+1 ), ldh,cs, sn )
                           
                 call stdlib_srot( i-i1-1, h( i1, i-1 ), 1_ilp, h( i1, i ), 1_ilp, cs, sn )
              end if
              if( wantz ) then
                 ! apply the transformation to z.
                 call stdlib_srot( nz, z( iloz, i-1 ), 1_ilp, z( iloz, i ), 1_ilp, cs, sn )
              end if
           end if
           ! reset deflation counter
           kdefl = 0_ilp
           ! return to start of the main loop with new value of i.
           i = l - 1_ilp
           go to 20
           160 continue
           return
     end subroutine stdlib_slahqr

     pure module subroutine stdlib_dlahqr( wantt, wantz, n, ilo, ihi, h, ldh, wr, wi,iloz, ihiz, z, ldz, &
     !! DLAHQR is an auxiliary routine called by DHSEQR to update the
     !! eigenvalues and Schur decomposition already computed by DHSEQR, by
     !! dealing with the Hessenberg submatrix in rows and columns ILO to
     !! IHI.
               info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ihi, ihiz, ilo, iloz, ldh, ldz, n
           integer(ilp), intent(out) :: info
           logical(lk), intent(in) :: wantt, wantz
           ! Array Arguments 
           real(dp), intent(inout) :: h(ldh,*), z(ldz,*)
           real(dp), intent(out) :: wi(*), wr(*)
        ! =========================================================
           ! Parameters 
           real(dp), parameter :: dat1 = 3.0_dp/4.0_dp
           real(dp), parameter :: dat2 = -0.4375_dp
           integer(ilp), parameter :: kexsh = 10_ilp
           
           
           
           ! Local Scalars 
           real(dp) :: aa, ab, ba, bb, cs, det, h11, h12, h21, h21s, h22, rt1i, rt1r, rt2i, rt2r, &
                     rtdisc, s, safmax, safmin, smlnum, sn, sum, t1, t2, t3, tr, tst, ulp, v2, v3
           integer(ilp) :: i, i1, i2, its, itmax, j, k, l, m, nh, nr, nz, kdefl
           ! Local Arrays 
           real(dp) :: v(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           ! quick return if possible
           if( n==0 )return
           if( ilo==ihi ) then
              wr( ilo ) = h( ilo, ilo )
              wi( ilo ) = zero
              return
           end if
           ! ==== clear out the trash ====
           do j = ilo, ihi - 3
              h( j+2, j ) = zero
              h( j+3, j ) = zero
           end do
           if( ilo<=ihi-2 )h( ihi, ihi-2 ) = zero
           nh = ihi - ilo + 1_ilp
           nz = ihiz - iloz + 1_ilp
           ! set machine-dependent constants for the stopping criterion.
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           safmax = one / safmin
           call stdlib_dlabad( safmin, safmax )
           ulp = stdlib_dlamch( 'PRECISION' )
           smlnum = safmin*( real( nh,KIND=dp) / ulp )
           ! i1 and i2 are the indices of the first row and last column of h
           ! to which transformations must be applied. if eigenvalues only are
           ! being computed, i1 and i2 are set inside the main loop.
           if( wantt ) then
              i1 = 1_ilp
              i2 = n
           end if
           ! itmax is the total number of qr iterations allowed.
           itmax = 30_ilp * max( 10_ilp, nh )
           ! kdefl counts the number of iterations since a deflation
           kdefl = 0_ilp
           ! the main loop begins here. i is the loop index and decreases from
           ! ihi to ilo in steps of 1 or 2. each iteration of the loop works
           ! with the active submatrix in rows and columns l to i.
           ! eigenvalues i+1 to ihi have already converged. either l = ilo or
           ! h(l,l-1) is negligible so that the matrix splits.
           i = ihi
           20 continue
           l = ilo
           if( i<ilo )go to 160
           ! perform qr iterations on rows and columns ilo to i until a
           ! submatrix of order 1 or 2 splits off at the bottom because a
           ! subdiagonal element has become negligible.
           loop_140: do its = 0, itmax
              ! look for a single small subdiagonal element.
              do k = i, l + 1, -1
                 if( abs( h( k, k-1 ) )<=smlnum )go to 40
                 tst = abs( h( k-1, k-1 ) ) + abs( h( k, k ) )
                 if( tst==zero ) then
                    if( k-2>=ilo )tst = tst + abs( h( k-1, k-2 ) )
                    if( k+1<=ihi )tst = tst + abs( h( k+1, k ) )
                 end if
                 ! ==== the following is a conservative small subdiagonal
                 ! .    deflation  criterion due to ahues
                 ! .    1997). it has better mathematical foundation and
                 ! .    improves accuracy in some cases.  ====
                 if( abs( h( k, k-1 ) )<=ulp*tst ) then
                    ab = max( abs( h( k, k-1 ) ), abs( h( k-1, k ) ) )
                    ba = min( abs( h( k, k-1 ) ), abs( h( k-1, k ) ) )
                    aa = max( abs( h( k, k ) ),abs( h( k-1, k-1 )-h( k, k ) ) )
                    bb = min( abs( h( k, k ) ),abs( h( k-1, k-1 )-h( k, k ) ) )
                    s = aa + ab
                    if( ba*( ab / s )<=max( smlnum,ulp*( bb*( aa / s ) ) ) )go to 40
                 end if
              end do
              40 continue
              l = k
              if( l>ilo ) then
                 ! h(l,l-1) is negligible
                 h( l, l-1 ) = zero
              end if
              ! exit from loop if a submatrix of order 1 or 2 has split off.
              if( l>=i-1 )go to 150
              kdefl = kdefl + 1_ilp
              ! now the active submatrix is in rows and columns l to i. if
              ! eigenvalues only are being computed, only the active submatrix
              ! need be transformed.
              if( .not.wantt ) then
                 i1 = l
                 i2 = i
              end if
              if( mod(kdefl,2_ilp*kexsh)==0_ilp ) then
                 ! exceptional shift.
                 s = abs( h( i, i-1 ) ) + abs( h( i-1, i-2 ) )
                 h11 = dat1*s + h( i, i )
                 h12 = dat2*s
                 h21 = s
                 h22 = h11
              else if( mod(kdefl,kexsh)==0_ilp ) then
                 ! exceptional shift.
                 s = abs( h( l+1, l ) ) + abs( h( l+2, l+1 ) )
                 h11 = dat1*s + h( l, l )
                 h12 = dat2*s
                 h21 = s
                 h22 = h11
              else
                 ! prepare to use francis' double shift
                 ! (i.e. 2nd degree generalized rayleigh quotient)
                 h11 = h( i-1, i-1 )
                 h21 = h( i, i-1 )
                 h12 = h( i-1, i )
                 h22 = h( i, i )
              end if
              s = abs( h11 ) + abs( h12 ) + abs( h21 ) + abs( h22 )
              if( s==zero ) then
                 rt1r = zero
                 rt1i = zero
                 rt2r = zero
                 rt2i = zero
              else
                 h11 = h11 / s
                 h21 = h21 / s
                 h12 = h12 / s
                 h22 = h22 / s
                 tr = ( h11+h22 ) / two
                 det = ( h11-tr )*( h22-tr ) - h12*h21
                 rtdisc = sqrt( abs( det ) )
                 if( det>=zero ) then
                    ! ==== complex conjugate shifts ====
                    rt1r = tr*s
                    rt2r = rt1r
                    rt1i = rtdisc*s
                    rt2i = -rt1i
                 else
                    ! ==== realshifts (use only one of them,KIND=dp)  ====
                    rt1r = tr + rtdisc
                    rt2r = tr - rtdisc
                    if( abs( rt1r-h22 )<=abs( rt2r-h22 ) ) then
                       rt1r = rt1r*s
                       rt2r = rt1r
                    else
                       rt2r = rt2r*s
                       rt1r = rt2r
                    end if
                    rt1i = zero
                    rt2i = zero
                 end if
              end if
              ! look for two consecutive small subdiagonal elements.
              do m = i - 2, l, -1
                 ! determine the effect of starting the double-shift qr
                 ! iteration at row m, and see if this would make h(m,m-1)
                 ! negligible.  (the following uses scaling to avoid
                 ! overflows and most underflows.)
                 h21s = h( m+1, m )
                 s = abs( h( m, m )-rt2r ) + abs( rt2i ) + abs( h21s )
                 h21s = h( m+1, m ) / s
                 v( 1_ilp ) = h21s*h( m, m+1 ) + ( h( m, m )-rt1r )*( ( h( m, m )-rt2r ) / s ) - &
                           rt1i*( rt2i / s )
                 v( 2_ilp ) = h21s*( h( m, m )+h( m+1, m+1 )-rt1r-rt2r )
                 v( 3_ilp ) = h21s*h( m+2, m+1 )
                 s = abs( v( 1_ilp ) ) + abs( v( 2_ilp ) ) + abs( v( 3_ilp ) )
                 v( 1_ilp ) = v( 1_ilp ) / s
                 v( 2_ilp ) = v( 2_ilp ) / s
                 v( 3_ilp ) = v( 3_ilp ) / s
                 if( m==l )go to 60
                 if( abs( h( m, m-1 ) )*( abs( v( 2_ilp ) )+abs( v( 3_ilp ) ) )<=ulp*abs( v( 1_ilp ) )*( abs( &
                           h( m-1, m-1 ) )+abs( h( m,m ) )+abs( h( m+1, m+1 ) ) ) )go to 60
              end do
              60 continue
              ! double-shift qr step
              loop_130: do k = m, i - 1
                 ! the first iteration of this loop determines a reflection g
                 ! from the vector v and applies it from left and right to h,
                 ! thus creating a nonzero bulge below the subdiagonal.
                 ! each subsequent iteration determines a reflection g to
                 ! restore the hessenberg form in the (k-1)th column, and thus
                 ! chases the bulge one step toward the bottom of the active
                 ! submatrix. nr is the order of g.
                 nr = min( 3_ilp, i-k+1 )
                 if( k>m )call stdlib_dcopy( nr, h( k, k-1 ), 1_ilp, v, 1_ilp )
                 call stdlib_dlarfg( nr, v( 1_ilp ), v( 2_ilp ), 1_ilp, t1 )
                 if( k>m ) then
                    h( k, k-1 ) = v( 1_ilp )
                    h( k+1, k-1 ) = zero
                    if( k<i-1 )h( k+2, k-1 ) = zero
                 else if( m>l ) then
                     ! ==== use the following instead of
                     ! .    h( k, k-1 ) = -h( k, k-1 ) to
                     ! .    avoid a bug when v(2) and v(3)
                     ! .    underflow. ====
                    h( k, k-1 ) = h( k, k-1 )*( one-t1 )
                 end if
                 v2 = v( 2_ilp )
                 t2 = t1*v2
                 if( nr==3_ilp ) then
                    v3 = v( 3_ilp )
                    t3 = t1*v3
                    ! apply g from the left to transform the rows of the matrix
                    ! in columns k to i2.
                    do j = k, i2
                       sum = h( k, j ) + v2*h( k+1, j ) + v3*h( k+2, j )
                       h( k, j ) = h( k, j ) - sum*t1
                       h( k+1, j ) = h( k+1, j ) - sum*t2
                       h( k+2, j ) = h( k+2, j ) - sum*t3
                    end do
                    ! apply g from the right to transform the columns of the
                    ! matrix in rows i1 to min(k+3,i).
                    do j = i1, min( k+3, i )
                       sum = h( j, k ) + v2*h( j, k+1 ) + v3*h( j, k+2 )
                       h( j, k ) = h( j, k ) - sum*t1
                       h( j, k+1 ) = h( j, k+1 ) - sum*t2
                       h( j, k+2 ) = h( j, k+2 ) - sum*t3
                    end do
                    if( wantz ) then
                       ! accumulate transformations in the matrix z
                       do j = iloz, ihiz
                          sum = z( j, k ) + v2*z( j, k+1 ) + v3*z( j, k+2 )
                          z( j, k ) = z( j, k ) - sum*t1
                          z( j, k+1 ) = z( j, k+1 ) - sum*t2
                          z( j, k+2 ) = z( j, k+2 ) - sum*t3
                       end do
                    end if
                 else if( nr==2_ilp ) then
                    ! apply g from the left to transform the rows of the matrix
                    ! in columns k to i2.
                    do j = k, i2
                       sum = h( k, j ) + v2*h( k+1, j )
                       h( k, j ) = h( k, j ) - sum*t1
                       h( k+1, j ) = h( k+1, j ) - sum*t2
                    end do
                    ! apply g from the right to transform the columns of the
                    ! matrix in rows i1 to min(k+3,i).
                    do j = i1, i
                       sum = h( j, k ) + v2*h( j, k+1 )
                       h( j, k ) = h( j, k ) - sum*t1
                       h( j, k+1 ) = h( j, k+1 ) - sum*t2
                    end do
                    if( wantz ) then
                       ! accumulate transformations in the matrix z
                       do j = iloz, ihiz
                          sum = z( j, k ) + v2*z( j, k+1 )
                          z( j, k ) = z( j, k ) - sum*t1
                          z( j, k+1 ) = z( j, k+1 ) - sum*t2
                       end do
                    end if
                 end if
              end do loop_130
           end do loop_140
           ! failure to converge in remaining number of iterations
           info = i
           return
           150 continue
           if( l==i ) then
              ! h(i,i-1) is negligible: one eigenvalue has converged.
              wr( i ) = h( i, i )
              wi( i ) = zero
           else if( l==i-1 ) then
              ! h(i-1,i-2) is negligible: a pair of eigenvalues have converged.
              ! transform the 2-by-2 submatrix to standard schur form,
              ! and compute and store the eigenvalues.
              call stdlib_dlanv2( h( i-1, i-1 ), h( i-1, i ), h( i, i-1 ),h( i, i ), wr( i-1 ), &
                        wi( i-1 ), wr( i ), wi( i ),cs, sn )
              if( wantt ) then
                 ! apply the transformation to the rest of h.
                 if( i2>i )call stdlib_drot( i2-i, h( i-1, i+1 ), ldh, h( i, i+1 ), ldh,cs, sn )
                           
                 call stdlib_drot( i-i1-1, h( i1, i-1 ), 1_ilp, h( i1, i ), 1_ilp, cs, sn )
              end if
              if( wantz ) then
                 ! apply the transformation to z.
                 call stdlib_drot( nz, z( iloz, i-1 ), 1_ilp, z( iloz, i ), 1_ilp, cs, sn )
              end if
           end if
           ! reset deflation counter
           kdefl = 0_ilp
           ! return to start of the main loop with new value of i.
           i = l - 1_ilp
           go to 20
           160 continue
           return
     end subroutine stdlib_dlahqr


     pure module subroutine stdlib_clahqr( wantt, wantz, n, ilo, ihi, h, ldh, w, iloz,ihiz, z, ldz, info &
     !! CLAHQR is an auxiliary routine called by CHSEQR to update the
     !! eigenvalues and Schur decomposition already computed by CHSEQR, by
     !! dealing with the Hessenberg submatrix in rows and columns ILO to
     !! IHI.
               )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ihi, ihiz, ilo, iloz, ldh, ldz, n
           integer(ilp), intent(out) :: info
           logical(lk), intent(in) :: wantt, wantz
           ! Array Arguments 
           complex(sp), intent(inout) :: h(ldh,*), z(ldz,*)
           complex(sp), intent(out) :: w(*)
        ! =========================================================
           ! Parameters 
           real(sp), parameter :: dat1 = 3.0_sp/4.0_sp
           integer(ilp), parameter :: kexsh = 10_ilp
           
           
           
           
           ! Local Scalars 
           complex(sp) :: cdum, h11, h11s, h22, sc, sum, t, t1, temp, u, v2, x, y
           real(sp) :: aa, ab, ba, bb, h10, h21, rtemp, s, safmax, safmin, smlnum, sx, t2, tst, &
                     ulp
           integer(ilp) :: i, i1, i2, its, itmax, j, jhi, jlo, k, l, m, nh, nz, kdefl
           ! Local Arrays 
           complex(sp) :: v(2_ilp)
           ! Statement Functions 
           real(sp) :: cabs1
           ! Intrinsic Functions 
           ! Statement Function Definitions 
           cabs1( cdum ) = abs( real( cdum,KIND=sp) ) + abs( aimag( cdum ) )
           ! Executable Statements 
           info = 0_ilp
           ! quick return if possible
           if( n==0 )return
           if( ilo==ihi ) then
              w( ilo ) = h( ilo, ilo )
              return
           end if
           ! ==== clear out the trash ====
           do j = ilo, ihi - 3
              h( j+2, j ) = czero
              h( j+3, j ) = czero
           end do
           if( ilo<=ihi-2 )h( ihi, ihi-2 ) = czero
           ! ==== ensure that subdiagonal entries are real ====
           if( wantt ) then
              jlo = 1_ilp
              jhi = n
           else
              jlo = ilo
              jhi = ihi
           end if
           do i = ilo + 1, ihi
              if( aimag( h( i, i-1 ) )/=zero ) then
                 ! ==== the following redundant normalization
                 ! .    avoids problems with both gradual and
                 ! .    sudden underflow in abs(h(i,i-1)) ====
                 sc = h( i, i-1 ) / cabs1( h( i, i-1 ) )
                 sc = conjg( sc ) / abs( sc )
                 h( i, i-1 ) = abs( h( i, i-1 ) )
                 call stdlib_cscal( jhi-i+1, sc, h( i, i ), ldh )
                 call stdlib_cscal( min( jhi, i+1 )-jlo+1, conjg( sc ), h( jlo, i ),1_ilp )
                 if( wantz )call stdlib_cscal( ihiz-iloz+1, conjg( sc ), z( iloz, i ), 1_ilp )
              end if
           end do
           nh = ihi - ilo + 1_ilp
           nz = ihiz - iloz + 1_ilp
           ! set machine-dependent constants for the stopping criterion.
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           safmax = one / safmin
           call stdlib_slabad( safmin, safmax )
           ulp = stdlib_slamch( 'PRECISION' )
           smlnum = safmin*( real( nh,KIND=sp) / ulp )
           ! i1 and i2 are the indices of the first row and last column of h
           ! to which transformations must be applied. if eigenvalues only are
           ! being computed, i1 and i2 are set inside the main loop.
           if( wantt ) then
              i1 = 1_ilp
              i2 = n
           end if
           ! itmax is the total number of qr iterations allowed.
           itmax = 30_ilp * max( 10_ilp, nh )
           ! kdefl counts the number of iterations since a deflation
           kdefl = 0_ilp
           ! the main loop begins here. i is the loop index and decreases from
           ! ihi to ilo in steps of 1. each iteration of the loop works
           ! with the active submatrix in rows and columns l to i.
           ! eigenvalues i+1 to ihi have already converged. either l = ilo, or
           ! h(l,l-1) is negligible so that the matrix splits.
           i = ihi
           30 continue
           if( i<ilo )go to 150
           ! perform qr iterations on rows and columns ilo to i until a
           ! submatrix of order 1 splits off at the bottom because a
           ! subdiagonal element has become negligible.
           l = ilo
           loop_130: do its = 0, itmax
              ! look for a single small subdiagonal element.
              do k = i, l + 1, -1
                 if( cabs1( h( k, k-1 ) )<=smlnum )go to 50
                 tst = cabs1( h( k-1, k-1 ) ) + cabs1( h( k, k ) )
                 if( tst==czero ) then
                    if( k-2>=ilo )tst = tst + abs( real( h( k-1, k-2 ),KIND=sp) )
                    if( k+1<=ihi )tst = tst + abs( real( h( k+1, k ),KIND=sp) )
                 end if
                 ! ==== the following is a conservative small subdiagonal
                 ! .    deflation criterion due to ahues
                 ! .    1997). it has better mathematical foundation and
                 ! .    improves accuracy in some examples.  ====
                 if( abs( real( h( k, k-1 ),KIND=sp) )<=ulp*tst ) then
                    ab = max( cabs1( h( k, k-1 ) ), cabs1( h( k-1, k ) ) )
                    ba = min( cabs1( h( k, k-1 ) ), cabs1( h( k-1, k ) ) )
                    aa = max( cabs1( h( k, k ) ),cabs1( h( k-1, k-1 )-h( k, k ) ) )
                    bb = min( cabs1( h( k, k ) ),cabs1( h( k-1, k-1 )-h( k, k ) ) )
                    s = aa + ab
                    if( ba*( ab / s )<=max( smlnum,ulp*( bb*( aa / s ) ) ) )go to 50
                 end if
              end do
              50 continue
              l = k
              if( l>ilo ) then
                 ! h(l,l-1) is negligible
                 h( l, l-1 ) = czero
              end if
              ! exit from loop if a submatrix of order 1 has split off.
              if( l>=i )go to 140
              kdefl = kdefl + 1_ilp
              ! now the active submatrix is in rows and columns l to i. if
              ! eigenvalues only are being computed, only the active submatrix
              ! need be transformed.
              if( .not.wantt ) then
                 i1 = l
                 i2 = i
              end if
              if( mod(kdefl,2_ilp*kexsh)==0_ilp ) then
                 ! exceptional shift.
                 s = dat1*abs( real( h( i, i-1 ),KIND=sp) )
                 t = s + h( i, i )
              else if( mod(kdefl,kexsh)==0_ilp ) then
                 ! exceptional shift.
                 s = dat1*abs( real( h( l+1, l ),KIND=sp) )
                 t = s + h( l, l )
              else
                 ! wilkinson's shift.
                 t = h( i, i )
                 u = sqrt( h( i-1, i ) )*sqrt( h( i, i-1 ) )
                 s = cabs1( u )
                 if( s/=zero ) then
                    x = half*( h( i-1, i-1 )-t )
                    sx = cabs1( x )
                    s = max( s, cabs1( x ) )
                    y = s*sqrt( ( x / s )**2_ilp+( u / s )**2_ilp )
                    if( sx>zero ) then
                       if( real( x / sx,KIND=sp)*real( y,KIND=sp)+aimag( x / sx )*aimag( y )&
                                 <zero )y = -y
                    end if
                    t = t - u*stdlib_cladiv( u, ( x+y ) )
                 end if
              end if
              ! look for two consecutive small subdiagonal elements.
              do m = i - 1, l + 1, -1
                 ! determine the effect of starting the single-shift qr
                 ! iteration at row m, and see if this would make h(m,m-1)
                 ! negligible.
                 h11 = h( m, m )
                 h22 = h( m+1, m+1 )
                 h11s = h11 - t
                 h21 = real( h( m+1, m ),KIND=sp)
                 s = cabs1( h11s ) + abs( h21 )
                 h11s = h11s / s
                 h21 = h21 / s
                 v( 1_ilp ) = h11s
                 v( 2_ilp ) = h21
                 h10 = real( h( m, m-1 ),KIND=sp)
                 if( abs( h10 )*abs( h21 )<=ulp*( cabs1( h11s )*( cabs1( h11 )+cabs1( h22 ) ) ) )&
                           go to 70
              end do
              h11 = h( l, l )
              h22 = h( l+1, l+1 )
              h11s = h11 - t
              h21 = real( h( l+1, l ),KIND=sp)
              s = cabs1( h11s ) + abs( h21 )
              h11s = h11s / s
              h21 = h21 / s
              v( 1_ilp ) = h11s
              v( 2_ilp ) = h21
              70 continue
              ! single-shift qr step
              loop_120: do k = m, i - 1
                 ! the first iteration of this loop determines a reflection g
                 ! from the vector v and applies it from left and right to h,
                 ! thus creating a nonzero bulge below the subdiagonal.
                 ! each subsequent iteration determines a reflection g to
                 ! restore the hessenberg form in the (k-1)th column, and thus
                 ! chases the bulge cone step toward the bottom of the active
                 ! submatrix.
                 ! v(2) is always real before the call to stdlib_clarfg, and hence
                 ! after the call t2 ( = t1*v(2) ) is also real.
                 if( k>m )call stdlib_ccopy( 2_ilp, h( k, k-1 ), 1_ilp, v, 1_ilp )
                 call stdlib_clarfg( 2_ilp, v( 1_ilp ), v( 2_ilp ), 1_ilp, t1 )
                 if( k>m ) then
                    h( k, k-1 ) = v( 1_ilp )
                    h( k+1, k-1 ) = czero
                 end if
                 v2 = v( 2_ilp )
                 t2 = real( t1*v2,KIND=sp)
                 ! apply g from the left to transform the rows of the matrix
                 ! in columns k to i2.
                 do j = k, i2
                    sum = conjg( t1 )*h( k, j ) + t2*h( k+1, j )
                    h( k, j ) = h( k, j ) - sum
                    h( k+1, j ) = h( k+1, j ) - sum*v2
                 end do
                 ! apply g from the right to transform the columns of the
                 ! matrix in rows i1 to min(k+2,i).
                 do j = i1, min( k+2, i )
                    sum = t1*h( j, k ) + t2*h( j, k+1 )
                    h( j, k ) = h( j, k ) - sum
                    h( j, k+1 ) = h( j, k+1 ) - sum*conjg( v2 )
                 end do
                 if( wantz ) then
                    ! accumulate transformations in the matrix z
                    do j = iloz, ihiz
                       sum = t1*z( j, k ) + t2*z( j, k+1 )
                       z( j, k ) = z( j, k ) - sum
                       z( j, k+1 ) = z( j, k+1 ) - sum*conjg( v2 )
                    end do
                 end if
                 if( k==m .and. m>l ) then
                    ! if the qr step was started at row m > l because two
                    ! consecutive small subdiagonals were found, then extra
                    ! scaling must be performed to ensure that h(m,m-1) remains
                    ! real.
                    temp = cone - t1
                    temp = temp / abs( temp )
                    h( m+1, m ) = h( m+1, m )*conjg( temp )
                    if( m+2<=i )h( m+2, m+1 ) = h( m+2, m+1 )*temp
                    do j = m, i
                       if( j/=m+1 ) then
                          if( i2>j )call stdlib_cscal( i2-j, temp, h( j, j+1 ), ldh )
                          call stdlib_cscal( j-i1, conjg( temp ), h( i1, j ), 1_ilp )
                          if( wantz ) then
                             call stdlib_cscal( nz, conjg( temp ), z( iloz, j ), 1_ilp )
                          end if
                       end if
                    end do
                 end if
              end do loop_120
              ! ensure that h(i,i-1) is real.
              temp = h( i, i-1 )
              if( aimag( temp )/=zero ) then
                 rtemp = abs( temp )
                 h( i, i-1 ) = rtemp
                 temp = temp / rtemp
                 if( i2>i )call stdlib_cscal( i2-i, conjg( temp ), h( i, i+1 ), ldh )
                 call stdlib_cscal( i-i1, temp, h( i1, i ), 1_ilp )
                 if( wantz ) then
                    call stdlib_cscal( nz, temp, z( iloz, i ), 1_ilp )
                 end if
              end if
           end do loop_130
           ! failure to converge in remaining number of iterations
           info = i
           return
           140 continue
           ! h(i,i-1) is negligible: cone eigenvalue has converged.
           w( i ) = h( i, i )
           ! reset deflation counter
           kdefl = 0_ilp
           ! return to start of the main loop with new value of i.
           i = l - 1_ilp
           go to 30
           150 continue
           return
     end subroutine stdlib_clahqr

     pure module subroutine stdlib_zlahqr( wantt, wantz, n, ilo, ihi, h, ldh, w, iloz,ihiz, z, ldz, info &
     !! ZLAHQR is an auxiliary routine called by CHSEQR to update the
     !! eigenvalues and Schur decomposition already computed by CHSEQR, by
     !! dealing with the Hessenberg submatrix in rows and columns ILO to
     !! IHI.
               )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ihi, ihiz, ilo, iloz, ldh, ldz, n
           integer(ilp), intent(out) :: info
           logical(lk), intent(in) :: wantt, wantz
           ! Array Arguments 
           complex(dp), intent(inout) :: h(ldh,*), z(ldz,*)
           complex(dp), intent(out) :: w(*)
        ! =========================================================
           ! Parameters 
           real(dp), parameter :: dat1 = 3.0_dp/4.0_dp
           integer(ilp), parameter :: kexsh = 10_ilp
           
           
           
           
           ! Local Scalars 
           complex(dp) :: cdum, h11, h11s, h22, sc, sum, t, t1, temp, u, v2, x, y
           real(dp) :: aa, ab, ba, bb, h10, h21, rtemp, s, safmax, safmin, smlnum, sx, t2, tst, &
                     ulp
           integer(ilp) :: i, i1, i2, its, itmax, j, jhi, jlo, k, l, m, nh, nz, kdefl
           ! Local Arrays 
           complex(dp) :: v(2_ilp)
           ! Statement Functions 
           real(dp) :: cabs1
           ! Intrinsic Functions 
           ! Statement Function Definitions 
           cabs1( cdum ) = abs( real( cdum,KIND=dp) ) + abs( aimag( cdum ) )
           ! Executable Statements 
           info = 0_ilp
           ! quick return if possible
           if( n==0 )return
           if( ilo==ihi ) then
              w( ilo ) = h( ilo, ilo )
              return
           end if
           ! ==== clear out the trash ====
           do j = ilo, ihi - 3
              h( j+2, j ) = czero
              h( j+3, j ) = czero
           end do
           if( ilo<=ihi-2 )h( ihi, ihi-2 ) = czero
           ! ==== ensure that subdiagonal entries are real ====
           if( wantt ) then
              jlo = 1_ilp
              jhi = n
           else
              jlo = ilo
              jhi = ihi
           end if
           do i = ilo + 1, ihi
              if( aimag( h( i, i-1 ) )/=zero ) then
                 ! ==== the following redundant normalization
                 ! .    avoids problems with both gradual and
                 ! .    sudden underflow in abs(h(i,i-1)) ====
                 sc = h( i, i-1 ) / cabs1( h( i, i-1 ) )
                 sc = conjg( sc ) / abs( sc )
                 h( i, i-1 ) = abs( h( i, i-1 ) )
                 call stdlib_zscal( jhi-i+1, sc, h( i, i ), ldh )
                 call stdlib_zscal( min( jhi, i+1 )-jlo+1, conjg( sc ),h( jlo, i ), 1_ilp )
                 if( wantz )call stdlib_zscal( ihiz-iloz+1, conjg( sc ), z( iloz, i ), 1_ilp )
              end if
           end do
           nh = ihi - ilo + 1_ilp
           nz = ihiz - iloz + 1_ilp
           ! set machine-dependent constants for the stopping criterion.
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           safmax = one / safmin
           call stdlib_dlabad( safmin, safmax )
           ulp = stdlib_dlamch( 'PRECISION' )
           smlnum = safmin*( real( nh,KIND=dp) / ulp )
           ! i1 and i2 are the indices of the first row and last column of h
           ! to which transformations must be applied. if eigenvalues only are
           ! being computed, i1 and i2 are set inside the main loop.
           if( wantt ) then
              i1 = 1_ilp
              i2 = n
           end if
           ! itmax is the total number of qr iterations allowed.
           itmax = 30_ilp * max( 10_ilp, nh )
           ! kdefl counts the number of iterations since a deflation
           kdefl = 0_ilp
           ! the main loop begins here. i is the loop index and decreases from
           ! ihi to ilo in steps of 1. each iteration of the loop works
           ! with the active submatrix in rows and columns l to i.
           ! eigenvalues i+1 to ihi have already converged. either l = ilo, or
           ! h(l,l-1) is negligible so that the matrix splits.
           i = ihi
           30 continue
           if( i<ilo )go to 150
           ! perform qr iterations on rows and columns ilo to i until a
           ! submatrix of order 1 splits off at the bottom because a
           ! subdiagonal element has become negligible.
           l = ilo
           loop_130: do its = 0, itmax
              ! look for a single small subdiagonal element.
              do k = i, l + 1, -1
                 if( cabs1( h( k, k-1 ) )<=smlnum )go to 50
                 tst = cabs1( h( k-1, k-1 ) ) + cabs1( h( k, k ) )
                 if( tst==czero ) then
                    if( k-2>=ilo )tst = tst + abs( real( h( k-1, k-2 ),KIND=dp) )
                    if( k+1<=ihi )tst = tst + abs( real( h( k+1, k ),KIND=dp) )
                 end if
                 ! ==== the following is a conservative small subdiagonal
                 ! .    deflation criterion due to ahues
                 ! .    1997). it has better mathematical foundation and
                 ! .    improves accuracy in some examples.  ====
                 if( abs( real( h( k, k-1 ),KIND=dp) )<=ulp*tst ) then
                    ab = max( cabs1( h( k, k-1 ) ), cabs1( h( k-1, k ) ) )
                    ba = min( cabs1( h( k, k-1 ) ), cabs1( h( k-1, k ) ) )
                    aa = max( cabs1( h( k, k ) ),cabs1( h( k-1, k-1 )-h( k, k ) ) )
                    bb = min( cabs1( h( k, k ) ),cabs1( h( k-1, k-1 )-h( k, k ) ) )
                    s = aa + ab
                    if( ba*( ab / s )<=max( smlnum,ulp*( bb*( aa / s ) ) ) )go to 50
                 end if
              end do
              50 continue
              l = k
              if( l>ilo ) then
                 ! h(l,l-1) is negligible
                 h( l, l-1 ) = czero
              end if
              ! exit from loop if a submatrix of order 1 has split off.
              if( l>=i )go to 140
              kdefl = kdefl + 1_ilp
              ! now the active submatrix is in rows and columns l to i. if
              ! eigenvalues only are being computed, only the active submatrix
              ! need be transformed.
              if( .not.wantt ) then
                 i1 = l
                 i2 = i
              end if
              if( mod(kdefl,2_ilp*kexsh)==0_ilp ) then
                 ! exceptional shift.
                 s = dat1*abs( real( h( i, i-1 ),KIND=dp) )
                 t = s + h( i, i )
              else if( mod(kdefl,kexsh)==0_ilp ) then
                 ! exceptional shift.
                 s = dat1*abs( real( h( l+1, l ),KIND=dp) )
                 t = s + h( l, l )
              else
                 ! wilkinson's shift.
                 t = h( i, i )
                 u = sqrt( h( i-1, i ) )*sqrt( h( i, i-1 ) )
                 s = cabs1( u )
                 if( s/=zero ) then
                    x = half*( h( i-1, i-1 )-t )
                    sx = cabs1( x )
                    s = max( s, cabs1( x ) )
                    y = s*sqrt( ( x / s )**2_ilp+( u / s )**2_ilp )
                    if( sx>zero ) then
                       if( real( x / sx,KIND=dp)*real( y,KIND=dp)+aimag( x / sx )*aimag( y )&
                                 <zero )y = -y
                    end if
                    t = t - u*stdlib_zladiv( u, ( x+y ) )
                 end if
              end if
              ! look for two consecutive small subdiagonal elements.
              do m = i - 1, l + 1, -1
                 ! determine the effect of starting the single-shift qr
                 ! iteration at row m, and see if this would make h(m,m-1)
                 ! negligible.
                 h11 = h( m, m )
                 h22 = h( m+1, m+1 )
                 h11s = h11 - t
                 h21 = real( h( m+1, m ),KIND=dp)
                 s = cabs1( h11s ) + abs( h21 )
                 h11s = h11s / s
                 h21 = h21 / s
                 v( 1_ilp ) = h11s
                 v( 2_ilp ) = h21
                 h10 = real( h( m, m-1 ),KIND=dp)
                 if( abs( h10 )*abs( h21 )<=ulp*( cabs1( h11s )*( cabs1( h11 )+cabs1( h22 ) ) ) )&
                           go to 70
              end do
              h11 = h( l, l )
              h22 = h( l+1, l+1 )
              h11s = h11 - t
              h21 = real( h( l+1, l ),KIND=dp)
              s = cabs1( h11s ) + abs( h21 )
              h11s = h11s / s
              h21 = h21 / s
              v( 1_ilp ) = h11s
              v( 2_ilp ) = h21
              70 continue
              ! single-shift qr step
              loop_120: do k = m, i - 1
                 ! the first iteration of this loop determines a reflection g
                 ! from the vector v and applies it from left and right to h,
                 ! thus creating a nonzero bulge below the subdiagonal.
                 ! each subsequent iteration determines a reflection g to
                 ! restore the hessenberg form in the (k-1)th column, and thus
                 ! chases the bulge cone step toward the bottom of the active
                 ! submatrix.
                 ! v(2) is always real before the call to stdlib_zlarfg, and hence
                 ! after the call t2 ( = t1*v(2) ) is also real.
                 if( k>m )call stdlib_zcopy( 2_ilp, h( k, k-1 ), 1_ilp, v, 1_ilp )
                 call stdlib_zlarfg( 2_ilp, v( 1_ilp ), v( 2_ilp ), 1_ilp, t1 )
                 if( k>m ) then
                    h( k, k-1 ) = v( 1_ilp )
                    h( k+1, k-1 ) = czero
                 end if
                 v2 = v( 2_ilp )
                 t2 = real( t1*v2,KIND=dp)
                 ! apply g from the left to transform the rows of the matrix
                 ! in columns k to i2.
                 do j = k, i2
                    sum = conjg( t1 )*h( k, j ) + t2*h( k+1, j )
                    h( k, j ) = h( k, j ) - sum
                    h( k+1, j ) = h( k+1, j ) - sum*v2
                 end do
                 ! apply g from the right to transform the columns of the
                 ! matrix in rows i1 to min(k+2,i).
                 do j = i1, min( k+2, i )
                    sum = t1*h( j, k ) + t2*h( j, k+1 )
                    h( j, k ) = h( j, k ) - sum
                    h( j, k+1 ) = h( j, k+1 ) - sum*conjg( v2 )
                 end do
                 if( wantz ) then
                    ! accumulate transformations in the matrix z
                    do j = iloz, ihiz
                       sum = t1*z( j, k ) + t2*z( j, k+1 )
                       z( j, k ) = z( j, k ) - sum
                       z( j, k+1 ) = z( j, k+1 ) - sum*conjg( v2 )
                    end do
                 end if
                 if( k==m .and. m>l ) then
                    ! if the qr step was started at row m > l because two
                    ! consecutive small subdiagonals were found, then extra
                    ! scaling must be performed to ensure that h(m,m-1) remains
                    ! real.
                    temp = cone - t1
                    temp = temp / abs( temp )
                    h( m+1, m ) = h( m+1, m )*conjg( temp )
                    if( m+2<=i )h( m+2, m+1 ) = h( m+2, m+1 )*temp
                    do j = m, i
                       if( j/=m+1 ) then
                          if( i2>j )call stdlib_zscal( i2-j, temp, h( j, j+1 ), ldh )
                          call stdlib_zscal( j-i1, conjg( temp ), h( i1, j ), 1_ilp )
                          if( wantz ) then
                             call stdlib_zscal( nz, conjg( temp ), z( iloz, j ),1_ilp )
                          end if
                       end if
                    end do
                 end if
              end do loop_120
              ! ensure that h(i,i-1) is real.
              temp = h( i, i-1 )
              if( aimag( temp )/=zero ) then
                 rtemp = abs( temp )
                 h( i, i-1 ) = rtemp
                 temp = temp / rtemp
                 if( i2>i )call stdlib_zscal( i2-i, conjg( temp ), h( i, i+1 ), ldh )
                 call stdlib_zscal( i-i1, temp, h( i1, i ), 1_ilp )
                 if( wantz ) then
                    call stdlib_zscal( nz, temp, z( iloz, i ), 1_ilp )
                 end if
              end if
           end do loop_130
           ! failure to converge in remaining number of iterations
           info = i
           return
           140 continue
           ! h(i,i-1) is negligible: cone eigenvalue has converged.
           w( i ) = h( i, i )
           ! reset deflation counter
           kdefl = 0_ilp
           ! return to start of the main loop with new value of i.
           i = l - 1_ilp
           go to 30
           150 continue
           return
     end subroutine stdlib_zlahqr




     module subroutine stdlib_slaqr0( wantt, wantz, n, ilo, ihi, h, ldh, wr, wi,iloz, ihiz, z, ldz, work,&
     !! SLAQR0 computes the eigenvalues of a Hessenberg matrix H
     !! and, optionally, the matrices T and Z from the Schur decomposition
     !! H = Z T Z**T, where T is an upper quasi-triangular matrix (the
     !! Schur form), and Z is the orthogonal matrix of Schur vectors.
     !! Optionally Z may be postmultiplied into an input orthogonal
     !! matrix Q so that this routine can give the Schur factorization
     !! of a matrix A which has been reduced to the Hessenberg form H
     !! by the orthogonal matrix Q:  A = Q*H*Q**T = (QZ)*T*(QZ)**T.
                lwork, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ihi, ihiz, ilo, iloz, ldh, ldz, lwork, n
           integer(ilp), intent(out) :: info
           logical(lk), intent(in) :: wantt, wantz
           ! Array Arguments 
           real(sp), intent(inout) :: h(ldh,*), z(ldz,*)
           real(sp), intent(out) :: wi(*), work(*), wr(*)
        ! ================================================================
           ! Parameters 
           integer(ilp), parameter :: ntiny = 15_ilp
           integer(ilp), parameter :: kexnw = 5_ilp
           integer(ilp), parameter :: kexsh = 6_ilp
           real(sp), parameter :: wilk1 = 0.75_sp
           real(sp), parameter :: wilk2 = -0.4375_sp
           ! ==== matrices of order ntiny or smaller must be processed by
           ! .    stdlib_slahqr because of insufficient subdiagonal scratch space.
           ! .    (this is a hard limit.) ====
           
           ! ==== exceptional deflation windows:  try to cure rare
           ! .    slow convergence by varying the size of the
           ! .    deflation window after kexnw iterations. ====
           
           ! ==== exceptional shifts: try to cure rare slow convergence
           ! .    with ad-hoc exceptional shifts every kexsh iterations.
           ! .    ====
           
           ! ==== the constants wilk1 and wilk2 are used to form the
           ! .    exceptional shifts. ====
           
           
           ! Local Scalars 
           real(sp) :: aa, bb, cc, cs, dd, sn, ss, swap
           integer(ilp) :: i, inf, it, itmax, k, kacc22, kbot, kdu, ks, kt, ktop, ku, kv, kwh, &
           kwtop, kwv, ld, ls, lwkopt, ndec, ndfl, nh, nho, nibble, nmin, ns, nsmax, nsr, nve, nw,&
                      nwmax, nwr, nwupbd
           logical(lk) :: sorted
           character(len=2_ilp) :: jbcmpz
           ! Local Arrays 
           real(sp) :: zdum(1_ilp,1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           ! ==== quick return for n = 0: nothing to do. ====
           if( n==0_ilp ) then
              work( 1_ilp ) = one
              return
           end if
           if( n<=ntiny ) then
              ! ==== tiny matrices must use stdlib_slahqr. ====
              lwkopt = 1_ilp
              if( lwork/=-1_ilp )call stdlib_slahqr( wantt, wantz, n, ilo, ihi, h, ldh, wr, wi,iloz, &
                        ihiz, z, ldz, info )
           else
              ! ==== use small bulge multi-shift qr with aggressive early
              ! .    deflation on larger-than-tiny matrices. ====
              ! ==== hope for the best. ====
              info = 0_ilp
              ! ==== set up job flags for stdlib_ilaenv. ====
              if( wantt ) then
                 jbcmpz( 1_ilp: 1_ilp ) = 'S'
              else
                 jbcmpz( 1_ilp: 1_ilp ) = 'E'
              end if
              if( wantz ) then
                 jbcmpz( 2_ilp: 2_ilp ) = 'V'
              else
                 jbcmpz( 2_ilp: 2_ilp ) = 'N'
              end if
              ! ==== nwr = recommended deflation window size.  at this
              ! .    point,  n > ntiny = 15, so there is enough
              ! .    subdiagonal workspace for nwr>=2 as required.
              ! .    (in fact, there is enough subdiagonal space for
              ! .    nwr>=4.) ====
              nwr = stdlib_ilaenv( 13_ilp, 'SLAQR0', jbcmpz, n, ilo, ihi, lwork )
              nwr = max( 2_ilp, nwr )
              nwr = min( ihi-ilo+1, ( n-1 ) / 3_ilp, nwr )
              ! ==== nsr = recommended number of simultaneous shifts.
              ! .    at this point n > ntiny = 15, so there is at
              ! .    enough subdiagonal workspace for nsr to be even
              ! .    and greater than or equal to two as required. ====
              nsr = stdlib_ilaenv( 15_ilp, 'SLAQR0', jbcmpz, n, ilo, ihi, lwork )
              nsr = min( nsr, ( n-3 ) / 6_ilp, ihi-ilo )
              nsr = max( 2_ilp, nsr-mod( nsr, 2_ilp ) )
              ! ==== estimate optimal workspace ====
              ! ==== workspace query call to stdlib_slaqr3 ====
              call stdlib_slaqr3( wantt, wantz, n, ilo, ihi, nwr+1, h, ldh, iloz,ihiz, z, ldz, ls,&
                         ld, wr, wi, h, ldh, n, h, ldh,n, h, ldh, work, -1_ilp )
              ! ==== optimal workspace = max(stdlib_slaqr5, stdlib_slaqr3) ====
              lwkopt = max( 3_ilp*nsr / 2_ilp, int( work( 1_ilp ),KIND=ilp) )
              ! ==== quick return in case of workspace query. ====
              if( lwork==-1_ilp ) then
                 work( 1_ilp ) = real( lwkopt,KIND=sp)
                 return
              end if
              ! ==== stdlib_slahqr/stdlib_slaqr0 crossover point ====
              nmin = stdlib_ilaenv( 12_ilp, 'SLAQR0', jbcmpz, n, ilo, ihi, lwork )
              nmin = max( ntiny, nmin )
              ! ==== nibble crossover point ====
              nibble = stdlib_ilaenv( 14_ilp, 'SLAQR0', jbcmpz, n, ilo, ihi, lwork )
              nibble = max( 0_ilp, nibble )
              ! ==== accumulate reflections during ttswp?  use block
              ! .    2-by-2 structure during matrix-matrix multiply? ====
              kacc22 = stdlib_ilaenv( 16_ilp, 'SLAQR0', jbcmpz, n, ilo, ihi, lwork )
              kacc22 = max( 0_ilp, kacc22 )
              kacc22 = min( 2_ilp, kacc22 )
              ! ==== nwmax = the largest possible deflation window for
              ! .    which there is sufficient workspace. ====
              nwmax = min( ( n-1 ) / 3_ilp, lwork / 2_ilp )
              nw = nwmax
              ! ==== nsmax = the largest number of simultaneous shifts
              ! .    for which there is sufficient workspace. ====
              nsmax = min( ( n-3 ) / 6_ilp, 2_ilp*lwork / 3_ilp )
              nsmax = nsmax - mod( nsmax, 2_ilp )
              ! ==== ndfl: an iteration count restarted at deflation. ====
              ndfl = 1_ilp
              ! ==== itmax = iteration limit ====
              itmax = max( 30_ilp, 2_ilp*kexsh )*max( 10_ilp, ( ihi-ilo+1 ) )
              ! ==== last row and column in the active block ====
              kbot = ihi
              ! ==== main loop ====
              loop_80: do it = 1, itmax
                 ! ==== done when kbot falls below ilo ====
                 if( kbot<ilo )go to 90
                 ! ==== locate active block ====
                 do k = kbot, ilo + 1, -1
                    if( h( k, k-1 )==zero )go to 20
                 end do
                 k = ilo
                 20 continue
                 ktop = k
                 ! ==== select deflation window size:
                 ! .    typical case:
                 ! .      if possible and advisable, nibble the entire
                 ! .      active block.  if not, use size min(nwr,nwmax)
                 ! .      or min(nwr+1,nwmax) depending upon which has
                 ! .      the smaller corresponding subdiagonal entry
                 ! .      (a heuristic).
                 ! .    exceptional case:
                 ! .      if there have been no deflations in kexnw or
                 ! .      more iterations, then vary the deflation window
                 ! .      size.   at first, because, larger windows are,
                 ! .      in general, more powerful than smaller ones,
                 ! .      rapidly increase the window to the maximum possible.
                 ! .      then, gradually reduce the window size. ====
                 nh = kbot - ktop + 1_ilp
                 nwupbd = min( nh, nwmax )
                 if( ndfl<kexnw ) then
                    nw = min( nwupbd, nwr )
                 else
                    nw = min( nwupbd, 2_ilp*nw )
                 end if
                 if( nw<nwmax ) then
                    if( nw>=nh-1 ) then
                       nw = nh
                    else
                       kwtop = kbot - nw + 1_ilp
                       if( abs( h( kwtop, kwtop-1 ) )>abs( h( kwtop-1, kwtop-2 ) ) )nw = nw + &
                                 1_ilp
                    end if
                 end if
                 if( ndfl<kexnw ) then
                    ndec = -1_ilp
                 else if( ndec>=0_ilp .or. nw>=nwupbd ) then
                    ndec = ndec + 1_ilp
                    if( nw-ndec<2_ilp )ndec = 0_ilp
                    nw = nw - ndec
                 end if
                 ! ==== aggressive early deflation:
                 ! .    split workspace under the subdiagonal into
                 ! .      - an nw-by-nw work array v in the lower
                 ! .        left-hand-corner,
                 ! .      - an nw-by-at-least-nw-but-more-is-better
                 ! .        (nw-by-nho) horizontal work array along
                 ! .        the bottom edge,
                 ! .      - an at-least-nw-but-more-is-better (nhv-by-nw)
                 ! .        vertical work array along the left-hand-edge.
                 ! .        ====
                 kv = n - nw + 1_ilp
                 kt = nw + 1_ilp
                 nho = ( n-nw-1 ) - kt + 1_ilp
                 kwv = nw + 2_ilp
                 nve = ( n-nw ) - kwv + 1_ilp
                 ! ==== aggressive early deflation ====
                 call stdlib_slaqr3( wantt, wantz, n, ktop, kbot, nw, h, ldh, iloz,ihiz, z, ldz, &
                 ls, ld, wr, wi, h( kv, 1_ilp ), ldh,nho, h( kv, kt ), ldh, nve, h( kwv, 1_ilp ), ldh,&
                           work, lwork )
                 ! ==== adjust kbot accounting for new deflations. ====
                 kbot = kbot - ld
                 ! ==== ks points to the shifts. ====
                 ks = kbot - ls + 1_ilp
                 ! ==== skip an expensive qr sweep if there is a (partly
                 ! .    heuristic) reason to expect that many eigenvalues
                 ! .    will deflate without it.  here, the qr sweep is
                 ! .    skipped if many eigenvalues have just been deflated
                 ! .    or if the remaining active block is small.
                 if( ( ld==0_ilp ) .or. ( ( 100_ilp*ld<=nw*nibble ) .and. ( kbot-ktop+1>min( nmin, nwmax )&
                            ) ) ) then
                    ! ==== ns = nominal number of simultaneous shifts.
                    ! .    this may be lowered (slightly) if stdlib_slaqr3
                    ! .    did not provide that many shifts. ====
                    ns = min( nsmax, nsr, max( 2_ilp, kbot-ktop ) )
                    ns = ns - mod( ns, 2_ilp )
                    ! ==== if there have been no deflations
                    ! .    in a multiple of kexsh iterations,
                    ! .    then try exceptional shifts.
                    ! .    otherwise use shifts provided by
                    ! .    stdlib_slaqr3 above or from the eigenvalues
                    ! .    of a trailing principal submatrix. ====
                    if( mod( ndfl, kexsh )==0_ilp ) then
                       ks = kbot - ns + 1_ilp
                       do i = kbot, max( ks+1, ktop+2 ), -2
                          ss = abs( h( i, i-1 ) ) + abs( h( i-1, i-2 ) )
                          aa = wilk1*ss + h( i, i )
                          bb = ss
                          cc = wilk2*ss
                          dd = aa
                          call stdlib_slanv2( aa, bb, cc, dd, wr( i-1 ), wi( i-1 ),wr( i ), wi( i &
                                    ), cs, sn )
                       end do
                       if( ks==ktop ) then
                          wr( ks+1 ) = h( ks+1, ks+1 )
                          wi( ks+1 ) = zero
                          wr( ks ) = wr( ks+1 )
                          wi( ks ) = wi( ks+1 )
                       end if
                    else
                       ! ==== got ns/2 or fewer shifts? use stdlib_slaqr4 or
                       ! .    stdlib_slahqr on a trailing principal submatrix to
                       ! .    get more. (since ns<=nsmax<=(n-3)/6,
                       ! .    there is enough space below the subdiagonal
                       ! .    to fit an ns-by-ns scratch array.) ====
                       if( kbot-ks+1<=ns / 2_ilp ) then
                          ks = kbot - ns + 1_ilp
                          kt = n - ns + 1_ilp
                          call stdlib_slacpy( 'A', ns, ns, h( ks, ks ), ldh,h( kt, 1_ilp ), ldh )
                                    
                          if( ns>nmin ) then
                             call stdlib_slaqr4( .false., .false., ns, 1_ilp, ns,h( kt, 1_ilp ), ldh, wr( &
                                       ks ),wi( ks ), 1_ilp, 1_ilp, zdum, 1_ilp, work,lwork, inf )
                          else
                             call stdlib_slahqr( .false., .false., ns, 1_ilp, ns,h( kt, 1_ilp ), ldh, wr( &
                                       ks ),wi( ks ), 1_ilp, 1_ilp, zdum, 1_ilp, inf )
                          end if
                          ks = ks + inf
                          ! ==== in case of a rare qr failure use
                          ! .    eigenvalues of the trailing 2-by-2
                          ! .    principal submatrix.  ====
                          if( ks>=kbot ) then
                             aa = h( kbot-1, kbot-1 )
                             cc = h( kbot, kbot-1 )
                             bb = h( kbot-1, kbot )
                             dd = h( kbot, kbot )
                             call stdlib_slanv2( aa, bb, cc, dd, wr( kbot-1 ),wi( kbot-1 ), wr( &
                                       kbot ),wi( kbot ), cs, sn )
                             ks = kbot - 1_ilp
                          end if
                       end if
                       if( kbot-ks+1>ns ) then
                          ! ==== sort the shifts (helps a little)
                          ! .    bubble sort keeps complex conjugate
                          ! .    pairs together. ====
                          sorted = .false.
                          do k = kbot, ks + 1, -1
                             if( sorted )go to 60
                             sorted = .true.
                             do i = ks, k - 1
                                if( abs( wr( i ) )+abs( wi( i ) )<abs( wr( i+1 ) )+abs( wi( i+1 ) &
                                          ) ) then
                                   sorted = .false.
                                   swap = wr( i )
                                   wr( i ) = wr( i+1 )
                                   wr( i+1 ) = swap
                                   swap = wi( i )
                                   wi( i ) = wi( i+1 )
                                   wi( i+1 ) = swap
                                end if
                             end do
                          end do
                          60 continue
                       end if
                       ! ==== shuffle shifts into pairs of real shifts
                       ! .    and pairs of complex conjugate shifts
                       ! .    assuming complex conjugate shifts are
                       ! .    already adjacent to one another. (yes,
                       ! .    they are.)  ====
                       do i = kbot, ks + 2, -2
                          if( wi( i )/=-wi( i-1 ) ) then
                             swap = wr( i )
                             wr( i ) = wr( i-1 )
                             wr( i-1 ) = wr( i-2 )
                             wr( i-2 ) = swap
                             swap = wi( i )
                             wi( i ) = wi( i-1 )
                             wi( i-1 ) = wi( i-2 )
                             wi( i-2 ) = swap
                          end if
                       end do
                    end if
                    ! ==== if there are only two shifts and both are
                    ! .    real, then use only one.  ====
                    if( kbot-ks+1==2_ilp ) then
                       if( wi( kbot )==zero ) then
                          if( abs( wr( kbot )-h( kbot, kbot ) )<abs( wr( kbot-1 )-h( kbot, kbot ) &
                                    ) ) then
                             wr( kbot-1 ) = wr( kbot )
                          else
                             wr( kbot ) = wr( kbot-1 )
                          end if
                       end if
                    end if
                    ! ==== use up to ns of the the smallest magnitude
                    ! .    shifts.  if there aren't ns shifts available,
                    ! .    then use them all, possibly dropping one to
                    ! .    make the number of shifts even. ====
                    ns = min( ns, kbot-ks+1 )
                    ns = ns - mod( ns, 2_ilp )
                    ks = kbot - ns + 1_ilp
                    ! ==== small-bulge multi-shift qr sweep:
                    ! .    split workspace under the subdiagonal into
                    ! .    - a kdu-by-kdu work array u in the lower
                    ! .      left-hand-corner,
                    ! .    - a kdu-by-at-least-kdu-but-more-is-better
                    ! .      (kdu-by-nho) horizontal work array wh along
                    ! .      the bottom edge,
                    ! .    - and an at-least-kdu-but-more-is-better-by-kdu
                    ! .      (nve-by-kdu) vertical work wv arrow along
                    ! .      the left-hand-edge. ====
                    kdu = 2_ilp*ns
                    ku = n - kdu + 1_ilp
                    kwh = kdu + 1_ilp
                    nho = ( n-kdu+1-4 ) - ( kdu+1 ) + 1_ilp
                    kwv = kdu + 4_ilp
                    nve = n - kdu - kwv + 1_ilp
                    ! ==== small-bulge multi-shift qr sweep ====
                    call stdlib_slaqr5( wantt, wantz, kacc22, n, ktop, kbot, ns,wr( ks ), wi( ks )&
                    , h, ldh, iloz, ihiz, z,ldz, work, 3_ilp, h( ku, 1_ilp ), ldh, nve,h( kwv, 1_ilp ), ldh, &
                              nho, h( ku, kwh ), ldh )
                 end if
                 ! ==== note progress (or the lack of it). ====
                 if( ld>0_ilp ) then
                    ndfl = 1_ilp
                 else
                    ndfl = ndfl + 1_ilp
                 end if
                 ! ==== end of main loop ====
              end do loop_80
              ! ==== iteration limit exceeded.  set info to show where
              ! .    the problem occurred and exit. ====
              info = kbot
              90 continue
           end if
           ! ==== return the optimal value of lwork. ====
           work( 1_ilp ) = real( lwkopt,KIND=sp)
     end subroutine stdlib_slaqr0

     module subroutine stdlib_dlaqr0( wantt, wantz, n, ilo, ihi, h, ldh, wr, wi,iloz, ihiz, z, ldz, work,&
     !! DLAQR0 computes the eigenvalues of a Hessenberg matrix H
     !! and, optionally, the matrices T and Z from the Schur decomposition
     !! H = Z T Z**T, where T is an upper quasi-triangular matrix (the
     !! Schur form), and Z is the orthogonal matrix of Schur vectors.
     !! Optionally Z may be postmultiplied into an input orthogonal
     !! matrix Q so that this routine can give the Schur factorization
     !! of a matrix A which has been reduced to the Hessenberg form H
     !! by the orthogonal matrix Q:  A = Q*H*Q**T = (QZ)*T*(QZ)**T.
                lwork, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ihi, ihiz, ilo, iloz, ldh, ldz, lwork, n
           integer(ilp), intent(out) :: info
           logical(lk), intent(in) :: wantt, wantz
           ! Array Arguments 
           real(dp), intent(inout) :: h(ldh,*), z(ldz,*)
           real(dp), intent(out) :: wi(*), work(*), wr(*)
        ! ================================================================
           ! Parameters 
           integer(ilp), parameter :: ntiny = 15_ilp
           integer(ilp), parameter :: kexnw = 5_ilp
           integer(ilp), parameter :: kexsh = 6_ilp
           real(dp), parameter :: wilk1 = 0.75_dp
           real(dp), parameter :: wilk2 = -0.4375_dp
           ! ==== matrices of order ntiny or smaller must be processed by
           ! .    stdlib_dlahqr because of insufficient subdiagonal scratch space.
           ! .    (this is a hard limit.) ====
           
           ! ==== exceptional deflation windows:  try to cure rare
           ! .    slow convergence by varying the size of the
           ! .    deflation window after kexnw iterations. ====
           
           ! ==== exceptional shifts: try to cure rare slow convergence
           ! .    with ad-hoc exceptional shifts every kexsh iterations.
           ! .    ====
           
           ! ==== the constants wilk1 and wilk2 are used to form the
           ! .    exceptional shifts. ====
           
           
           ! Local Scalars 
           real(dp) :: aa, bb, cc, cs, dd, sn, ss, swap
           integer(ilp) :: i, inf, it, itmax, k, kacc22, kbot, kdu, ks, kt, ktop, ku, kv, kwh, &
           kwtop, kwv, ld, ls, lwkopt, ndec, ndfl, nh, nho, nibble, nmin, ns, nsmax, nsr, nve, nw,&
                      nwmax, nwr, nwupbd
           logical(lk) :: sorted
           character(len=2_ilp) :: jbcmpz
           ! Local Arrays 
           real(dp) :: zdum(1_ilp,1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           ! ==== quick return for n = 0: nothing to do. ====
           if( n==0_ilp ) then
              work( 1_ilp ) = one
              return
           end if
           if( n<=ntiny ) then
              ! ==== tiny matrices must use stdlib_dlahqr. ====
              lwkopt = 1_ilp
              if( lwork/=-1_ilp )call stdlib_dlahqr( wantt, wantz, n, ilo, ihi, h, ldh, wr, wi,iloz, &
                        ihiz, z, ldz, info )
           else
              ! ==== use small bulge multi-shift qr with aggressive early
              ! .    deflation on larger-than-tiny matrices. ====
              ! ==== hope for the best. ====
              info = 0_ilp
              ! ==== set up job flags for stdlib_ilaenv. ====
              if( wantt ) then
                 jbcmpz( 1_ilp: 1_ilp ) = 'S'
              else
                 jbcmpz( 1_ilp: 1_ilp ) = 'E'
              end if
              if( wantz ) then
                 jbcmpz( 2_ilp: 2_ilp ) = 'V'
              else
                 jbcmpz( 2_ilp: 2_ilp ) = 'N'
              end if
              ! ==== nwr = recommended deflation window size.  at this
              ! .    point,  n > ntiny = 15, so there is enough
              ! .    subdiagonal workspace for nwr>=2 as required.
              ! .    (in fact, there is enough subdiagonal space for
              ! .    nwr>=4.) ====
              nwr = stdlib_ilaenv( 13_ilp, 'DLAQR0', jbcmpz, n, ilo, ihi, lwork )
              nwr = max( 2_ilp, nwr )
              nwr = min( ihi-ilo+1, ( n-1 ) / 3_ilp, nwr )
              ! ==== nsr = recommended number of simultaneous shifts.
              ! .    at this point n > ntiny = 15, so there is at
              ! .    enough subdiagonal workspace for nsr to be even
              ! .    and greater than or equal to two as required. ====
              nsr = stdlib_ilaenv( 15_ilp, 'DLAQR0', jbcmpz, n, ilo, ihi, lwork )
              nsr = min( nsr, ( n-3 ) / 6_ilp, ihi-ilo )
              nsr = max( 2_ilp, nsr-mod( nsr, 2_ilp ) )
              ! ==== estimate optimal workspace ====
              ! ==== workspace query call to stdlib_dlaqr3 ====
              call stdlib_dlaqr3( wantt, wantz, n, ilo, ihi, nwr+1, h, ldh, iloz,ihiz, z, ldz, ls,&
                         ld, wr, wi, h, ldh, n, h, ldh,n, h, ldh, work, -1_ilp )
              ! ==== optimal workspace = max(stdlib_dlaqr5, stdlib_dlaqr3) ====
              lwkopt = max( 3_ilp*nsr / 2_ilp, int( work( 1_ilp ),KIND=ilp) )
              ! ==== quick return in case of workspace query. ====
              if( lwork==-1_ilp ) then
                 work( 1_ilp ) = real( lwkopt,KIND=dp)
                 return
              end if
              ! ==== stdlib_dlahqr/stdlib_dlaqr0 crossover point ====
              nmin = stdlib_ilaenv( 12_ilp, 'DLAQR0', jbcmpz, n, ilo, ihi, lwork )
              nmin = max( ntiny, nmin )
              ! ==== nibble crossover point ====
              nibble = stdlib_ilaenv( 14_ilp, 'DLAQR0', jbcmpz, n, ilo, ihi, lwork )
              nibble = max( 0_ilp, nibble )
              ! ==== accumulate reflections during ttswp?  use block
              ! .    2-by-2 structure during matrix-matrix multiply? ====
              kacc22 = stdlib_ilaenv( 16_ilp, 'DLAQR0', jbcmpz, n, ilo, ihi, lwork )
              kacc22 = max( 0_ilp, kacc22 )
              kacc22 = min( 2_ilp, kacc22 )
              ! ==== nwmax = the largest possible deflation window for
              ! .    which there is sufficient workspace. ====
              nwmax = min( ( n-1 ) / 3_ilp, lwork / 2_ilp )
              nw = nwmax
              ! ==== nsmax = the largest number of simultaneous shifts
              ! .    for which there is sufficient workspace. ====
              nsmax = min( ( n-3 ) / 6_ilp, 2_ilp*lwork / 3_ilp )
              nsmax = nsmax - mod( nsmax, 2_ilp )
              ! ==== ndfl: an iteration count restarted at deflation. ====
              ndfl = 1_ilp
              ! ==== itmax = iteration limit ====
              itmax = max( 30_ilp, 2_ilp*kexsh )*max( 10_ilp, ( ihi-ilo+1 ) )
              ! ==== last row and column in the active block ====
              kbot = ihi
              ! ==== main loop ====
              loop_80: do it = 1, itmax
                 ! ==== done when kbot falls below ilo ====
                 if( kbot<ilo )go to 90
                 ! ==== locate active block ====
                 do k = kbot, ilo + 1, -1
                    if( h( k, k-1 )==zero )go to 20
                 end do
                 k = ilo
                 20 continue
                 ktop = k
                 ! ==== select deflation window size:
                 ! .    typical case:
                 ! .      if possible and advisable, nibble the entire
                 ! .      active block.  if not, use size min(nwr,nwmax)
                 ! .      or min(nwr+1,nwmax) depending upon which has
                 ! .      the smaller corresponding subdiagonal entry
                 ! .      (a heuristic).
                 ! .    exceptional case:
                 ! .      if there have been no deflations in kexnw or
                 ! .      more iterations, then vary the deflation window
                 ! .      size.   at first, because, larger windows are,
                 ! .      in general, more powerful than smaller ones,
                 ! .      rapidly increase the window to the maximum possible.
                 ! .      then, gradually reduce the window size. ====
                 nh = kbot - ktop + 1_ilp
                 nwupbd = min( nh, nwmax )
                 if( ndfl<kexnw ) then
                    nw = min( nwupbd, nwr )
                 else
                    nw = min( nwupbd, 2_ilp*nw )
                 end if
                 if( nw<nwmax ) then
                    if( nw>=nh-1 ) then
                       nw = nh
                    else
                       kwtop = kbot - nw + 1_ilp
                       if( abs( h( kwtop, kwtop-1 ) )>abs( h( kwtop-1, kwtop-2 ) ) )nw = nw + &
                                 1_ilp
                    end if
                 end if
                 if( ndfl<kexnw ) then
                    ndec = -1_ilp
                 else if( ndec>=0_ilp .or. nw>=nwupbd ) then
                    ndec = ndec + 1_ilp
                    if( nw-ndec<2_ilp )ndec = 0_ilp
                    nw = nw - ndec
                 end if
                 ! ==== aggressive early deflation:
                 ! .    split workspace under the subdiagonal into
                 ! .      - an nw-by-nw work array v in the lower
                 ! .        left-hand-corner,
                 ! .      - an nw-by-at-least-nw-but-more-is-better
                 ! .        (nw-by-nho) horizontal work array along
                 ! .        the bottom edge,
                 ! .      - an at-least-nw-but-more-is-better (nhv-by-nw)
                 ! .        vertical work array along the left-hand-edge.
                 ! .        ====
                 kv = n - nw + 1_ilp
                 kt = nw + 1_ilp
                 nho = ( n-nw-1 ) - kt + 1_ilp
                 kwv = nw + 2_ilp
                 nve = ( n-nw ) - kwv + 1_ilp
                 ! ==== aggressive early deflation ====
                 call stdlib_dlaqr3( wantt, wantz, n, ktop, kbot, nw, h, ldh, iloz,ihiz, z, ldz, &
                 ls, ld, wr, wi, h( kv, 1_ilp ), ldh,nho, h( kv, kt ), ldh, nve, h( kwv, 1_ilp ), ldh,&
                           work, lwork )
                 ! ==== adjust kbot accounting for new deflations. ====
                 kbot = kbot - ld
                 ! ==== ks points to the shifts. ====
                 ks = kbot - ls + 1_ilp
                 ! ==== skip an expensive qr sweep if there is a (partly
                 ! .    heuristic) reason to expect that many eigenvalues
                 ! .    will deflate without it.  here, the qr sweep is
                 ! .    skipped if many eigenvalues have just been deflated
                 ! .    or if the remaining active block is small.
                 if( ( ld==0_ilp ) .or. ( ( 100_ilp*ld<=nw*nibble ) .and. ( kbot-ktop+1>min( nmin, nwmax )&
                            ) ) ) then
                    ! ==== ns = nominal number of simultaneous shifts.
                    ! .    this may be lowered (slightly) if stdlib_dlaqr3
                    ! .    did not provide that many shifts. ====
                    ns = min( nsmax, nsr, max( 2_ilp, kbot-ktop ) )
                    ns = ns - mod( ns, 2_ilp )
                    ! ==== if there have been no deflations
                    ! .    in a multiple of kexsh iterations,
                    ! .    then try exceptional shifts.
                    ! .    otherwise use shifts provided by
                    ! .    stdlib_dlaqr3 above or from the eigenvalues
                    ! .    of a trailing principal submatrix. ====
                    if( mod( ndfl, kexsh )==0_ilp ) then
                       ks = kbot - ns + 1_ilp
                       do i = kbot, max( ks+1, ktop+2 ), -2
                          ss = abs( h( i, i-1 ) ) + abs( h( i-1, i-2 ) )
                          aa = wilk1*ss + h( i, i )
                          bb = ss
                          cc = wilk2*ss
                          dd = aa
                          call stdlib_dlanv2( aa, bb, cc, dd, wr( i-1 ), wi( i-1 ),wr( i ), wi( i &
                                    ), cs, sn )
                       end do
                       if( ks==ktop ) then
                          wr( ks+1 ) = h( ks+1, ks+1 )
                          wi( ks+1 ) = zero
                          wr( ks ) = wr( ks+1 )
                          wi( ks ) = wi( ks+1 )
                       end if
                    else
                       ! ==== got ns/2 or fewer shifts? use stdlib_dlaqr4 or
                       ! .    stdlib_dlahqr on a trailing principal submatrix to
                       ! .    get more. (since ns<=nsmax<=(n-3)/6,
                       ! .    there is enough space below the subdiagonal
                       ! .    to fit an ns-by-ns scratch array.) ====
                       if( kbot-ks+1<=ns / 2_ilp ) then
                          ks = kbot - ns + 1_ilp
                          kt = n - ns + 1_ilp
                          call stdlib_dlacpy( 'A', ns, ns, h( ks, ks ), ldh,h( kt, 1_ilp ), ldh )
                                    
                          if( ns>nmin ) then
                             call stdlib_dlaqr4( .false., .false., ns, 1_ilp, ns,h( kt, 1_ilp ), ldh, wr( &
                                       ks ),wi( ks ), 1_ilp, 1_ilp, zdum, 1_ilp, work,lwork, inf )
                          else
                             call stdlib_dlahqr( .false., .false., ns, 1_ilp, ns,h( kt, 1_ilp ), ldh, wr( &
                                       ks ),wi( ks ), 1_ilp, 1_ilp, zdum, 1_ilp, inf )
                          end if
                          ks = ks + inf
                          ! ==== in case of a rare qr failure use
                          ! .    eigenvalues of the trailing 2-by-2
                          ! .    principal submatrix.  ====
                          if( ks>=kbot ) then
                             aa = h( kbot-1, kbot-1 )
                             cc = h( kbot, kbot-1 )
                             bb = h( kbot-1, kbot )
                             dd = h( kbot, kbot )
                             call stdlib_dlanv2( aa, bb, cc, dd, wr( kbot-1 ),wi( kbot-1 ), wr( &
                                       kbot ),wi( kbot ), cs, sn )
                             ks = kbot - 1_ilp
                          end if
                       end if
                       if( kbot-ks+1>ns ) then
                          ! ==== sort the shifts (helps a little)
                          ! .    bubble sort keeps complex conjugate
                          ! .    pairs together. ====
                          sorted = .false.
                          do k = kbot, ks + 1, -1
                             if( sorted )go to 60
                             sorted = .true.
                             do i = ks, k - 1
                                if( abs( wr( i ) )+abs( wi( i ) )<abs( wr( i+1 ) )+abs( wi( i+1 ) &
                                          ) ) then
                                   sorted = .false.
                                   swap = wr( i )
                                   wr( i ) = wr( i+1 )
                                   wr( i+1 ) = swap
                                   swap = wi( i )
                                   wi( i ) = wi( i+1 )
                                   wi( i+1 ) = swap
                                end if
                             end do
                          end do
                          60 continue
                       end if
                       ! ==== shuffle shifts into pairs of real shifts
                       ! .    and pairs of complex conjugate shifts
                       ! .    assuming complex conjugate shifts are
                       ! .    already adjacent to one another. (yes,
                       ! .    they are.)  ====
                       do i = kbot, ks + 2, -2
                          if( wi( i )/=-wi( i-1 ) ) then
                             swap = wr( i )
                             wr( i ) = wr( i-1 )
                             wr( i-1 ) = wr( i-2 )
                             wr( i-2 ) = swap
                             swap = wi( i )
                             wi( i ) = wi( i-1 )
                             wi( i-1 ) = wi( i-2 )
                             wi( i-2 ) = swap
                          end if
                       end do
                    end if
                    ! ==== if there are only two shifts and both are
                    ! .    real, then use only one.  ====
                    if( kbot-ks+1==2_ilp ) then
                       if( wi( kbot )==zero ) then
                          if( abs( wr( kbot )-h( kbot, kbot ) )<abs( wr( kbot-1 )-h( kbot, kbot ) &
                                    ) ) then
                             wr( kbot-1 ) = wr( kbot )
                          else
                             wr( kbot ) = wr( kbot-1 )
                          end if
                       end if
                    end if
                    ! ==== use up to ns of the the smallest magnitude
                    ! .    shifts.  if there aren't ns shifts available,
                    ! .    then use them all, possibly dropping one to
                    ! .    make the number of shifts even. ====
                    ns = min( ns, kbot-ks+1 )
                    ns = ns - mod( ns, 2_ilp )
                    ks = kbot - ns + 1_ilp
                    ! ==== small-bulge multi-shift qr sweep:
                    ! .    split workspace under the subdiagonal into
                    ! .    - a kdu-by-kdu work array u in the lower
                    ! .      left-hand-corner,
                    ! .    - a kdu-by-at-least-kdu-but-more-is-better
                    ! .      (kdu-by-nho) horizontal work array wh along
                    ! .      the bottom edge,
                    ! .    - and an at-least-kdu-but-more-is-better-by-kdu
                    ! .      (nve-by-kdu) vertical work wv arrow along
                    ! .      the left-hand-edge. ====
                    kdu = 2_ilp*ns
                    ku = n - kdu + 1_ilp
                    kwh = kdu + 1_ilp
                    nho = ( n-kdu+1-4 ) - ( kdu+1 ) + 1_ilp
                    kwv = kdu + 4_ilp
                    nve = n - kdu - kwv + 1_ilp
                    ! ==== small-bulge multi-shift qr sweep ====
                    call stdlib_dlaqr5( wantt, wantz, kacc22, n, ktop, kbot, ns,wr( ks ), wi( ks )&
                    , h, ldh, iloz, ihiz, z,ldz, work, 3_ilp, h( ku, 1_ilp ), ldh, nve,h( kwv, 1_ilp ), ldh, &
                              nho, h( ku, kwh ), ldh )
                 end if
                 ! ==== note progress (or the lack of it). ====
                 if( ld>0_ilp ) then
                    ndfl = 1_ilp
                 else
                    ndfl = ndfl + 1_ilp
                 end if
                 ! ==== end of main loop ====
              end do loop_80
              ! ==== iteration limit exceeded.  set info to show where
              ! .    the problem occurred and exit. ====
              info = kbot
              90 continue
           end if
           ! ==== return the optimal value of lwork. ====
           work( 1_ilp ) = real( lwkopt,KIND=dp)
     end subroutine stdlib_dlaqr0


     pure module subroutine stdlib_claqr0( wantt, wantz, n, ilo, ihi, h, ldh, w, iloz,ihiz, z, ldz, work,&
     !! CLAQR0 computes the eigenvalues of a Hessenberg matrix H
     !! and, optionally, the matrices T and Z from the Schur decomposition
     !! H = Z T Z**H, where T is an upper triangular matrix (the
     !! Schur form), and Z is the unitary matrix of Schur vectors.
     !! Optionally Z may be postmultiplied into an input unitary
     !! matrix Q so that this routine can give the Schur factorization
     !! of a matrix A which has been reduced to the Hessenberg form H
     !! by the unitary matrix Q:  A = Q*H*Q**H = (QZ)*H*(QZ)**H.
                lwork, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ihi, ihiz, ilo, iloz, ldh, ldz, lwork, n
           integer(ilp), intent(out) :: info
           logical(lk), intent(in) :: wantt, wantz
           ! Array Arguments 
           complex(sp), intent(inout) :: h(ldh,*), z(ldz,*)
           complex(sp), intent(out) :: w(*), work(*)
        ! ================================================================
           ! Parameters 
           integer(ilp), parameter :: ntiny = 15_ilp
           integer(ilp), parameter :: kexnw = 5_ilp
           integer(ilp), parameter :: kexsh = 6_ilp
           real(sp), parameter :: wilk1 = 0.75_sp
           ! ==== matrices of order ntiny or smaller must be processed by
           ! .    stdlib_clahqr because of insufficient subdiagonal scratch space.
           ! .    (this is a hard limit.) ====
           
           ! ==== exceptional deflation windows:  try to cure rare
           ! .    slow convergence by varying the size of the
           ! .    deflation window after kexnw iterations. ====
           
           ! ==== exceptional shifts: try to cure rare slow convergence
           ! .    with ad-hoc exceptional shifts every kexsh iterations.
           ! .    ====
           
           ! ==== the constant wilk1 is used to form the exceptional
           ! .    shifts. ====
           
           
           
           ! Local Scalars 
           complex(sp) :: aa, bb, cc, cdum, dd, det, rtdisc, swap, tr2
           real(sp) :: s
           integer(ilp) :: i, inf, it, itmax, k, kacc22, kbot, kdu, ks, kt, ktop, ku, kv, kwh, &
           kwtop, kwv, ld, ls, lwkopt, ndec, ndfl, nh, nho, nibble, nmin, ns, nsmax, nsr, nve, nw,&
                      nwmax, nwr, nwupbd
           logical(lk) :: sorted
           character(len=2) :: jbcmpz
           ! Local Arrays 
           complex(sp) :: zdum(1_ilp,1_ilp)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( cdum ) = abs( real( cdum,KIND=sp) ) + abs( aimag( cdum ) )
           ! Executable Statements 
           info = 0_ilp
           ! ==== quick return for n = 0: nothing to do. ====
           if( n==0_ilp ) then
              work( 1_ilp ) = cone
              return
           end if
           if( n<=ntiny ) then
              ! ==== tiny matrices must use stdlib_clahqr. ====
              lwkopt = 1_ilp
              if( lwork/=-1_ilp )call stdlib_clahqr( wantt, wantz, n, ilo, ihi, h, ldh, w, iloz,ihiz, &
                        z, ldz, info )
           else
              ! ==== use small bulge multi-shift qr with aggressive early
              ! .    deflation on larger-than-tiny matrices. ====
              ! ==== hope for the best. ====
              info = 0_ilp
              ! ==== set up job flags for stdlib_ilaenv. ====
              if( wantt ) then
                 jbcmpz( 1_ilp: 1_ilp ) = 'S'
              else
                 jbcmpz( 1_ilp: 1_ilp ) = 'E'
              end if
              if( wantz ) then
                 jbcmpz( 2_ilp: 2_ilp ) = 'V'
              else
                 jbcmpz( 2_ilp: 2_ilp ) = 'N'
              end if
              ! ==== nwr = recommended deflation window size.  at this
              ! .    point,  n > ntiny = 15, so there is enough
              ! .    subdiagonal workspace for nwr>=2 as required.
              ! .    (in fact, there is enough subdiagonal space for
              ! .    nwr>=4.) ====
              nwr = stdlib_ilaenv( 13_ilp, 'CLAQR0', jbcmpz, n, ilo, ihi, lwork )
              nwr = max( 2_ilp, nwr )
              nwr = min( ihi-ilo+1, ( n-1 ) / 3_ilp, nwr )
              ! ==== nsr = recommended number of simultaneous shifts.
              ! .    at this point n > ntiny = 15, so there is at
              ! .    enough subdiagonal workspace for nsr to be even
              ! .    and greater than or equal to two as required. ====
              nsr = stdlib_ilaenv( 15_ilp, 'CLAQR0', jbcmpz, n, ilo, ihi, lwork )
              nsr = min( nsr, ( n-3 ) / 6_ilp, ihi-ilo )
              nsr = max( 2_ilp, nsr-mod( nsr, 2_ilp ) )
              ! ==== estimate optimal workspace ====
              ! ==== workspace query call to stdlib_claqr3 ====
              call stdlib_claqr3( wantt, wantz, n, ilo, ihi, nwr+1, h, ldh, iloz,ihiz, z, ldz, ls,&
                         ld, w, h, ldh, n, h, ldh, n, h,ldh, work, -1_ilp )
              ! ==== optimal workspace = max(stdlib_claqr5, stdlib_claqr3) ====
              lwkopt = max( 3_ilp*nsr / 2_ilp, int( work( 1_ilp ),KIND=ilp) )
              ! ==== quick return in case of workspace query. ====
              if( lwork==-1_ilp ) then
                 work( 1_ilp ) = cmplx( lwkopt, 0_ilp,KIND=sp)
                 return
              end if
              ! ==== stdlib_clahqr/stdlib_claqr0 crossover point ====
              nmin = stdlib_ilaenv( 12_ilp, 'CLAQR0', jbcmpz, n, ilo, ihi, lwork )
              nmin = max( ntiny, nmin )
              ! ==== nibble crossover point ====
              nibble = stdlib_ilaenv( 14_ilp, 'CLAQR0', jbcmpz, n, ilo, ihi, lwork )
              nibble = max( 0_ilp, nibble )
              ! ==== accumulate reflections during ttswp?  use block
              ! .    2-by-2 structure during matrix-matrix multiply? ====
              kacc22 = stdlib_ilaenv( 16_ilp, 'CLAQR0', jbcmpz, n, ilo, ihi, lwork )
              kacc22 = max( 0_ilp, kacc22 )
              kacc22 = min( 2_ilp, kacc22 )
              ! ==== nwmax = the largest possible deflation window for
              ! .    which there is sufficient workspace. ====
              nwmax = min( ( n-1 ) / 3_ilp, lwork / 2_ilp )
              nw = nwmax
              ! ==== nsmax = the largest number of simultaneous shifts
              ! .    for which there is sufficient workspace. ====
              nsmax = min( ( n-3 ) / 6_ilp, 2_ilp*lwork / 3_ilp )
              nsmax = nsmax - mod( nsmax, 2_ilp )
              ! ==== ndfl: an iteration count restarted at deflation. ====
              ndfl = 1_ilp
              ! ==== itmax = iteration limit ====
              itmax = max( 30_ilp, 2_ilp*kexsh )*max( 10_ilp, ( ihi-ilo+1 ) )
              ! ==== last row and column in the active block ====
              kbot = ihi
              ! ==== main loop ====
              loop_70: do it = 1, itmax
                 ! ==== done when kbot falls below ilo ====
                 if( kbot<ilo )go to 80
                 ! ==== locate active block ====
                 do k = kbot, ilo + 1, -1
                    if( h( k, k-1 )==czero )go to 20
                 end do
                 k = ilo
                 20 continue
                 ktop = k
                 ! ==== select deflation window size:
                 ! .    typical case:
                 ! .      if possible and advisable, nibble the entire
                 ! .      active block.  if not, use size min(nwr,nwmax)
                 ! .      or min(nwr+1,nwmax) depending upon which has
                 ! .      the smaller corresponding subdiagonal entry
                 ! .      (a heuristic).
                 ! .    exceptional case:
                 ! .      if there have been no deflations in kexnw or
                 ! .      more iterations, then vary the deflation window
                 ! .      size.   at first, because, larger windows are,
                 ! .      in general, more powerful than smaller ones,
                 ! .      rapidly increase the window to the maximum possible.
                 ! .      then, gradually reduce the window size. ====
                 nh = kbot - ktop + 1_ilp
                 nwupbd = min( nh, nwmax )
                 if( ndfl<kexnw ) then
                    nw = min( nwupbd, nwr )
                 else
                    nw = min( nwupbd, 2_ilp*nw )
                 end if
                 if( nw<nwmax ) then
                    if( nw>=nh-1 ) then
                       nw = nh
                    else
                       kwtop = kbot - nw + 1_ilp
                       if( cabs1( h( kwtop, kwtop-1 ) )>cabs1( h( kwtop-1, kwtop-2 ) ) )nw = nw + &
                                 1_ilp
                    end if
                 end if
                 if( ndfl<kexnw ) then
                    ndec = -1_ilp
                 else if( ndec>=0_ilp .or. nw>=nwupbd ) then
                    ndec = ndec + 1_ilp
                    if( nw-ndec<2_ilp )ndec = 0_ilp
                    nw = nw - ndec
                 end if
                 ! ==== aggressive early deflation:
                 ! .    split workspace under the subdiagonal into
                 ! .      - an nw-by-nw work array v in the lower
                 ! .        left-hand-corner,
                 ! .      - an nw-by-at-least-nw-but-more-is-better
                 ! .        (nw-by-nho) horizontal work array along
                 ! .        the bottom edge,
                 ! .      - an at-least-nw-but-more-is-better (nhv-by-nw)
                 ! .        vertical work array along the left-hand-edge.
                 ! .        ====
                 kv = n - nw + 1_ilp
                 kt = nw + 1_ilp
                 nho = ( n-nw-1 ) - kt + 1_ilp
                 kwv = nw + 2_ilp
                 nve = ( n-nw ) - kwv + 1_ilp
                 ! ==== aggressive early deflation ====
                 call stdlib_claqr3( wantt, wantz, n, ktop, kbot, nw, h, ldh, iloz,ihiz, z, ldz, &
                 ls, ld, w, h( kv, 1_ilp ), ldh, nho,h( kv, kt ), ldh, nve, h( kwv, 1_ilp ), ldh, work,&
                           lwork )
                 ! ==== adjust kbot accounting for new deflations. ====
                 kbot = kbot - ld
                 ! ==== ks points to the shifts. ====
                 ks = kbot - ls + 1_ilp
                 ! ==== skip an expensive qr sweep if there is a (partly
                 ! .    heuristic) reason to expect that many eigenvalues
                 ! .    will deflate without it.  here, the qr sweep is
                 ! .    skipped if many eigenvalues have just been deflated
                 ! .    or if the remaining active block is small.
                 if( ( ld==0_ilp ) .or. ( ( 100_ilp*ld<=nw*nibble ) .and. ( kbot-ktop+1>min( nmin, nwmax )&
                            ) ) ) then
                    ! ==== ns = nominal number of simultaneous shifts.
                    ! .    this may be lowered (slightly) if stdlib_claqr3
                    ! .    did not provide that many shifts. ====
                    ns = min( nsmax, nsr, max( 2_ilp, kbot-ktop ) )
                    ns = ns - mod( ns, 2_ilp )
                    ! ==== if there have been no deflations
                    ! .    in a multiple of kexsh iterations,
                    ! .    then try exceptional shifts.
                    ! .    otherwise use shifts provided by
                    ! .    stdlib_claqr3 above or from the eigenvalues
                    ! .    of a trailing principal submatrix. ====
                    if( mod( ndfl, kexsh )==0_ilp ) then
                       ks = kbot - ns + 1_ilp
                       do i = kbot, ks + 1, -2
                          w( i ) = h( i, i ) + wilk1*cabs1( h( i, i-1 ) )
                          w( i-1 ) = w( i )
                       end do
                    else
                       ! ==== got ns/2 or fewer shifts? use stdlib_claqr4 or
                       ! .    stdlib_clahqr on a trailing principal submatrix to
                       ! .    get more. (since ns<=nsmax<=(n-3)/6,
                       ! .    there is enough space below the subdiagonal
                       ! .    to fit an ns-by-ns scratch array.) ====
                       if( kbot-ks+1<=ns / 2_ilp ) then
                          ks = kbot - ns + 1_ilp
                          kt = n - ns + 1_ilp
                          call stdlib_clacpy( 'A', ns, ns, h( ks, ks ), ldh,h( kt, 1_ilp ), ldh )
                                    
                          if( ns>nmin ) then
                             call stdlib_claqr4( .false., .false., ns, 1_ilp, ns,h( kt, 1_ilp ), ldh, w( &
                                       ks ), 1_ilp, 1_ilp,zdum, 1_ilp, work, lwork, inf )
                          else
                             call stdlib_clahqr( .false., .false., ns, 1_ilp, ns,h( kt, 1_ilp ), ldh, w( &
                                       ks ), 1_ilp, 1_ilp,zdum, 1_ilp, inf )
                          end if
                          ks = ks + inf
                          ! ==== in case of a rare qr failure use
                          ! .    eigenvalues of the trailing 2-by-2
                          ! .    principal submatrix.  scale to avoid
                          ! .    overflows, underflows and subnormals.
                          ! .    (the scale factor s can not be czero,
                          ! .    because h(kbot,kbot-1) is nonzero.) ====
                          if( ks>=kbot ) then
                             s = cabs1( h( kbot-1, kbot-1 ) ) +cabs1( h( kbot, kbot-1 ) ) +cabs1( &
                                       h( kbot-1, kbot ) ) +cabs1( h( kbot, kbot ) )
                             aa = h( kbot-1, kbot-1 ) / s
                             cc = h( kbot, kbot-1 ) / s
                             bb = h( kbot-1, kbot ) / s
                             dd = h( kbot, kbot ) / s
                             tr2 = ( aa+dd ) / two
                             det = ( aa-tr2 )*( dd-tr2 ) - bb*cc
                             rtdisc = sqrt( -det )
                             w( kbot-1 ) = ( tr2+rtdisc )*s
                             w( kbot ) = ( tr2-rtdisc )*s
                             ks = kbot - 1_ilp
                          end if
                       end if
                       if( kbot-ks+1>ns ) then
                          ! ==== sort the shifts (helps a little) ====
                          sorted = .false.
                          do k = kbot, ks + 1, -1
                             if( sorted )go to 60
                             sorted = .true.
                             do i = ks, k - 1
                                if( cabs1( w( i ) )<cabs1( w( i+1 ) ) )then
                                   sorted = .false.
                                   swap = w( i )
                                   w( i ) = w( i+1 )
                                   w( i+1 ) = swap
                                end if
                             end do
                          end do
                          60 continue
                       end if
                    end if
                    ! ==== if there are only two shifts, then use
                    ! .    only cone.  ====
                    if( kbot-ks+1==2_ilp ) then
                       if( cabs1( w( kbot )-h( kbot, kbot ) )<cabs1( w( kbot-1 )-h( kbot, kbot ) )&
                                  ) then
                          w( kbot-1 ) = w( kbot )
                       else
                          w( kbot ) = w( kbot-1 )
                       end if
                    end if
                    ! ==== use up to ns of the the smallest magnitude
                    ! .    shifts.  if there aren't ns shifts available,
                    ! .    then use them all, possibly dropping cone to
                    ! .    make the number of shifts even. ====
                    ns = min( ns, kbot-ks+1 )
                    ns = ns - mod( ns, 2_ilp )
                    ks = kbot - ns + 1_ilp
                    ! ==== small-bulge multi-shift qr sweep:
                    ! .    split workspace under the subdiagonal into
                    ! .    - a kdu-by-kdu work array u in the lower
                    ! .      left-hand-corner,
                    ! .    - a kdu-by-at-least-kdu-but-more-is-better
                    ! .      (kdu-by-nho) horizontal work array wh along
                    ! .      the bottom edge,
                    ! .    - and an at-least-kdu-but-more-is-better-by-kdu
                    ! .      (nve-by-kdu) vertical work wv arrow along
                    ! .      the left-hand-edge. ====
                    kdu = 2_ilp*ns
                    ku = n - kdu + 1_ilp
                    kwh = kdu + 1_ilp
                    nho = ( n-kdu+1-4 ) - ( kdu+1 ) + 1_ilp
                    kwv = kdu + 4_ilp
                    nve = n - kdu - kwv + 1_ilp
                    ! ==== small-bulge multi-shift qr sweep ====
                    call stdlib_claqr5( wantt, wantz, kacc22, n, ktop, kbot, ns,w( ks ), h, ldh, &
                    iloz, ihiz, z, ldz, work,3_ilp, h( ku, 1_ilp ), ldh, nve, h( kwv, 1_ilp ), ldh,nho, h( ku,&
                               kwh ), ldh )
                 end if
                 ! ==== note progress (or the lack of it). ====
                 if( ld>0_ilp ) then
                    ndfl = 1_ilp
                 else
                    ndfl = ndfl + 1_ilp
                 end if
                 ! ==== end of main loop ====
              end do loop_70
              ! ==== iteration limit exceeded.  set info to show where
              ! .    the problem occurred and exit. ====
              info = kbot
              80 continue
           end if
           ! ==== return the optimal value of lwork. ====
           work( 1_ilp ) = cmplx( lwkopt, 0_ilp,KIND=sp)
     end subroutine stdlib_claqr0

     pure module subroutine stdlib_zlaqr0( wantt, wantz, n, ilo, ihi, h, ldh, w, iloz,ihiz, z, ldz, work,&
     !! ZLAQR0 computes the eigenvalues of a Hessenberg matrix H
     !! and, optionally, the matrices T and Z from the Schur decomposition
     !! H = Z T Z**H, where T is an upper triangular matrix (the
     !! Schur form), and Z is the unitary matrix of Schur vectors.
     !! Optionally Z may be postmultiplied into an input unitary
     !! matrix Q so that this routine can give the Schur factorization
     !! of a matrix A which has been reduced to the Hessenberg form H
     !! by the unitary matrix Q:  A = Q*H*Q**H = (QZ)*H*(QZ)**H.
                lwork, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ihi, ihiz, ilo, iloz, ldh, ldz, lwork, n
           integer(ilp), intent(out) :: info
           logical(lk), intent(in) :: wantt, wantz
           ! Array Arguments 
           complex(dp), intent(inout) :: h(ldh,*), z(ldz,*)
           complex(dp), intent(out) :: w(*), work(*)
        ! ================================================================
           ! Parameters 
           integer(ilp), parameter :: ntiny = 15_ilp
           integer(ilp), parameter :: kexnw = 5_ilp
           integer(ilp), parameter :: kexsh = 6_ilp
           real(dp), parameter :: wilk1 = 0.75_dp
           ! ==== matrices of order ntiny or smaller must be processed by
           ! .    stdlib_zlahqr because of insufficient subdiagonal scratch space.
           ! .    (this is a hard limit.) ====
           
           ! ==== exceptional deflation windows:  try to cure rare
           ! .    slow convergence by varying the size of the
           ! .    deflation window after kexnw iterations. ====
           
           ! ==== exceptional shifts: try to cure rare slow convergence
           ! .    with ad-hoc exceptional shifts every kexsh iterations.
           ! .    ====
           
           ! ==== the constant wilk1 is used to form the exceptional
           ! .    shifts. ====
           
           
           
           ! Local Scalars 
           complex(dp) :: aa, bb, cc, cdum, dd, det, rtdisc, swap, tr2
           real(dp) :: s
           integer(ilp) :: i, inf, it, itmax, k, kacc22, kbot, kdu, ks, kt, ktop, ku, kv, kwh, &
           kwtop, kwv, ld, ls, lwkopt, ndec, ndfl, nh, nho, nibble, nmin, ns, nsmax, nsr, nve, nw,&
                      nwmax, nwr, nwupbd
           logical(lk) :: sorted
           character(len=2) :: jbcmpz
           ! Local Arrays 
           complex(dp) :: zdum(1_ilp,1_ilp)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( cdum ) = abs( real( cdum,KIND=dp) ) + abs( aimag( cdum ) )
           ! Executable Statements 
           info = 0_ilp
           ! ==== quick return for n = 0: nothing to do. ====
           if( n==0_ilp ) then
              work( 1_ilp ) = cone
              return
           end if
           if( n<=ntiny ) then
              ! ==== tiny matrices must use stdlib_zlahqr. ====
              lwkopt = 1_ilp
              if( lwork/=-1_ilp )call stdlib_zlahqr( wantt, wantz, n, ilo, ihi, h, ldh, w, iloz,ihiz, &
                        z, ldz, info )
           else
              ! ==== use small bulge multi-shift qr with aggressive early
              ! .    deflation on larger-than-tiny matrices. ====
              ! ==== hope for the best. ====
              info = 0_ilp
              ! ==== set up job flags for stdlib_ilaenv. ====
              if( wantt ) then
                 jbcmpz( 1_ilp: 1_ilp ) = 'S'
              else
                 jbcmpz( 1_ilp: 1_ilp ) = 'E'
              end if
              if( wantz ) then
                 jbcmpz( 2_ilp: 2_ilp ) = 'V'
              else
                 jbcmpz( 2_ilp: 2_ilp ) = 'N'
              end if
              ! ==== nwr = recommended deflation window size.  at this
              ! .    point,  n > ntiny = 15, so there is enough
              ! .    subdiagonal workspace for nwr>=2 as required.
              ! .    (in fact, there is enough subdiagonal space for
              ! .    nwr>=4.) ====
              nwr = stdlib_ilaenv( 13_ilp, 'ZLAQR0', jbcmpz, n, ilo, ihi, lwork )
              nwr = max( 2_ilp, nwr )
              nwr = min( ihi-ilo+1, ( n-1 ) / 3_ilp, nwr )
              ! ==== nsr = recommended number of simultaneous shifts.
              ! .    at this point n > ntiny = 15, so there is at
              ! .    enough subdiagonal workspace for nsr to be even
              ! .    and greater than or equal to two as required. ====
              nsr = stdlib_ilaenv( 15_ilp, 'ZLAQR0', jbcmpz, n, ilo, ihi, lwork )
              nsr = min( nsr, ( n-3 ) / 6_ilp, ihi-ilo )
              nsr = max( 2_ilp, nsr-mod( nsr, 2_ilp ) )
              ! ==== estimate optimal workspace ====
              ! ==== workspace query call to stdlib_zlaqr3 ====
              call stdlib_zlaqr3( wantt, wantz, n, ilo, ihi, nwr+1, h, ldh, iloz,ihiz, z, ldz, ls,&
                         ld, w, h, ldh, n, h, ldh, n, h,ldh, work, -1_ilp )
              ! ==== optimal workspace = max(stdlib_zlaqr5, stdlib_zlaqr3) ====
              lwkopt = max( 3_ilp*nsr / 2_ilp, int( work( 1_ilp ),KIND=ilp) )
              ! ==== quick return in case of workspace query. ====
              if( lwork==-1_ilp ) then
                 work( 1_ilp ) = cmplx( lwkopt, 0_ilp,KIND=dp)
                 return
              end if
              ! ==== stdlib_zlahqr/stdlib_zlaqr0 crossover point ====
              nmin = stdlib_ilaenv( 12_ilp, 'ZLAQR0', jbcmpz, n, ilo, ihi, lwork )
              nmin = max( ntiny, nmin )
              ! ==== nibble crossover point ====
              nibble = stdlib_ilaenv( 14_ilp, 'ZLAQR0', jbcmpz, n, ilo, ihi, lwork )
              nibble = max( 0_ilp, nibble )
              ! ==== accumulate reflections during ttswp?  use block
              ! .    2-by-2 structure during matrix-matrix multiply? ====
              kacc22 = stdlib_ilaenv( 16_ilp, 'ZLAQR0', jbcmpz, n, ilo, ihi, lwork )
              kacc22 = max( 0_ilp, kacc22 )
              kacc22 = min( 2_ilp, kacc22 )
              ! ==== nwmax = the largest possible deflation window for
              ! .    which there is sufficient workspace. ====
              nwmax = min( ( n-1 ) / 3_ilp, lwork / 2_ilp )
              nw = nwmax
              ! ==== nsmax = the largest number of simultaneous shifts
              ! .    for which there is sufficient workspace. ====
              nsmax = min( ( n-3 ) / 6_ilp, 2_ilp*lwork / 3_ilp )
              nsmax = nsmax - mod( nsmax, 2_ilp )
              ! ==== ndfl: an iteration count restarted at deflation. ====
              ndfl = 1_ilp
              ! ==== itmax = iteration limit ====
              itmax = max( 30_ilp, 2_ilp*kexsh )*max( 10_ilp, ( ihi-ilo+1 ) )
              ! ==== last row and column in the active block ====
              kbot = ihi
              ! ==== main loop ====
              loop_70: do it = 1, itmax
                 ! ==== done when kbot falls below ilo ====
                 if( kbot<ilo )go to 80
                 ! ==== locate active block ====
                 do k = kbot, ilo + 1, -1
                    if( h( k, k-1 )==czero )go to 20
                 end do
                 k = ilo
                 20 continue
                 ktop = k
                 ! ==== select deflation window size:
                 ! .    typical case:
                 ! .      if possible and advisable, nibble the entire
                 ! .      active block.  if not, use size min(nwr,nwmax)
                 ! .      or min(nwr+1,nwmax) depending upon which has
                 ! .      the smaller corresponding subdiagonal entry
                 ! .      (a heuristic).
                 ! .    exceptional case:
                 ! .      if there have been no deflations in kexnw or
                 ! .      more iterations, then vary the deflation window
                 ! .      size.   at first, because, larger windows are,
                 ! .      in general, more powerful than smaller ones,
                 ! .      rapidly increase the window to the maximum possible.
                 ! .      then, gradually reduce the window size. ====
                 nh = kbot - ktop + 1_ilp
                 nwupbd = min( nh, nwmax )
                 if( ndfl<kexnw ) then
                    nw = min( nwupbd, nwr )
                 else
                    nw = min( nwupbd, 2_ilp*nw )
                 end if
                 if( nw<nwmax ) then
                    if( nw>=nh-1 ) then
                       nw = nh
                    else
                       kwtop = kbot - nw + 1_ilp
                       if( cabs1( h( kwtop, kwtop-1 ) )>cabs1( h( kwtop-1, kwtop-2 ) ) )nw = nw + &
                                 1_ilp
                    end if
                 end if
                 if( ndfl<kexnw ) then
                    ndec = -1_ilp
                 else if( ndec>=0_ilp .or. nw>=nwupbd ) then
                    ndec = ndec + 1_ilp
                    if( nw-ndec<2_ilp )ndec = 0_ilp
                    nw = nw - ndec
                 end if
                 ! ==== aggressive early deflation:
                 ! .    split workspace under the subdiagonal into
                 ! .      - an nw-by-nw work array v in the lower
                 ! .        left-hand-corner,
                 ! .      - an nw-by-at-least-nw-but-more-is-better
                 ! .        (nw-by-nho) horizontal work array along
                 ! .        the bottom edge,
                 ! .      - an at-least-nw-but-more-is-better (nhv-by-nw)
                 ! .        vertical work array along the left-hand-edge.
                 ! .        ====
                 kv = n - nw + 1_ilp
                 kt = nw + 1_ilp
                 nho = ( n-nw-1 ) - kt + 1_ilp
                 kwv = nw + 2_ilp
                 nve = ( n-nw ) - kwv + 1_ilp
                 ! ==== aggressive early deflation ====
                 call stdlib_zlaqr3( wantt, wantz, n, ktop, kbot, nw, h, ldh, iloz,ihiz, z, ldz, &
                 ls, ld, w, h( kv, 1_ilp ), ldh, nho,h( kv, kt ), ldh, nve, h( kwv, 1_ilp ), ldh, work,&
                           lwork )
                 ! ==== adjust kbot accounting for new deflations. ====
                 kbot = kbot - ld
                 ! ==== ks points to the shifts. ====
                 ks = kbot - ls + 1_ilp
                 ! ==== skip an expensive qr sweep if there is a (partly
                 ! .    heuristic) reason to expect that many eigenvalues
                 ! .    will deflate without it.  here, the qr sweep is
                 ! .    skipped if many eigenvalues have just been deflated
                 ! .    or if the remaining active block is small.
                 if( ( ld==0_ilp ) .or. ( ( 100_ilp*ld<=nw*nibble ) .and. ( kbot-ktop+1>min( nmin, nwmax )&
                            ) ) ) then
                    ! ==== ns = nominal number of simultaneous shifts.
                    ! .    this may be lowered (slightly) if stdlib_zlaqr3
                    ! .    did not provide that many shifts. ====
                    ns = min( nsmax, nsr, max( 2_ilp, kbot-ktop ) )
                    ns = ns - mod( ns, 2_ilp )
                    ! ==== if there have been no deflations
                    ! .    in a multiple of kexsh iterations,
                    ! .    then try exceptional shifts.
                    ! .    otherwise use shifts provided by
                    ! .    stdlib_zlaqr3 above or from the eigenvalues
                    ! .    of a trailing principal submatrix. ====
                    if( mod( ndfl, kexsh )==0_ilp ) then
                       ks = kbot - ns + 1_ilp
                       do i = kbot, ks + 1, -2
                          w( i ) = h( i, i ) + wilk1*cabs1( h( i, i-1 ) )
                          w( i-1 ) = w( i )
                       end do
                    else
                       ! ==== got ns/2 or fewer shifts? use stdlib_zlaqr4 or
                       ! .    stdlib_zlahqr on a trailing principal submatrix to
                       ! .    get more. (since ns<=nsmax<=(n-3)/6,
                       ! .    there is enough space below the subdiagonal
                       ! .    to fit an ns-by-ns scratch array.) ====
                       if( kbot-ks+1<=ns / 2_ilp ) then
                          ks = kbot - ns + 1_ilp
                          kt = n - ns + 1_ilp
                          call stdlib_zlacpy( 'A', ns, ns, h( ks, ks ), ldh,h( kt, 1_ilp ), ldh )
                                    
                          if( ns>nmin ) then
                             call stdlib_zlaqr4( .false., .false., ns, 1_ilp, ns,h( kt, 1_ilp ), ldh, w( &
                                       ks ), 1_ilp, 1_ilp,zdum, 1_ilp, work, lwork, inf )
                          else
                             call stdlib_zlahqr( .false., .false., ns, 1_ilp, ns,h( kt, 1_ilp ), ldh, w( &
                                       ks ), 1_ilp, 1_ilp,zdum, 1_ilp, inf )
                          end if
                          ks = ks + inf
                          ! ==== in case of a rare qr failure use
                          ! .    eigenvalues of the trailing 2-by-2
                          ! .    principal submatrix.  scale to avoid
                          ! .    overflows, underflows and subnormals.
                          ! .    (the scale factor s can not be czero,
                          ! .    because h(kbot,kbot-1) is nonzero.) ====
                          if( ks>=kbot ) then
                             s = cabs1( h( kbot-1, kbot-1 ) ) +cabs1( h( kbot, kbot-1 ) ) +cabs1( &
                                       h( kbot-1, kbot ) ) +cabs1( h( kbot, kbot ) )
                             aa = h( kbot-1, kbot-1 ) / s
                             cc = h( kbot, kbot-1 ) / s
                             bb = h( kbot-1, kbot ) / s
                             dd = h( kbot, kbot ) / s
                             tr2 = ( aa+dd ) / two
                             det = ( aa-tr2 )*( dd-tr2 ) - bb*cc
                             rtdisc = sqrt( -det )
                             w( kbot-1 ) = ( tr2+rtdisc )*s
                             w( kbot ) = ( tr2-rtdisc )*s
                             ks = kbot - 1_ilp
                          end if
                       end if
                       if( kbot-ks+1>ns ) then
                          ! ==== sort the shifts (helps a little) ====
                          sorted = .false.
                          do k = kbot, ks + 1, -1
                             if( sorted )go to 60
                             sorted = .true.
                             do i = ks, k - 1
                                if( cabs1( w( i ) )<cabs1( w( i+1 ) ) )then
                                   sorted = .false.
                                   swap = w( i )
                                   w( i ) = w( i+1 )
                                   w( i+1 ) = swap
                                end if
                             end do
                          end do
                          60 continue
                       end if
                    end if
                    ! ==== if there are only two shifts, then use
                    ! .    only cone.  ====
                    if( kbot-ks+1==2_ilp ) then
                       if( cabs1( w( kbot )-h( kbot, kbot ) )<cabs1( w( kbot-1 )-h( kbot, kbot ) )&
                                  ) then
                          w( kbot-1 ) = w( kbot )
                       else
                          w( kbot ) = w( kbot-1 )
                       end if
                    end if
                    ! ==== use up to ns of the the smallest magnitude
                    ! .    shifts.  if there aren't ns shifts available,
                    ! .    then use them all, possibly dropping cone to
                    ! .    make the number of shifts even. ====
                    ns = min( ns, kbot-ks+1 )
                    ns = ns - mod( ns, 2_ilp )
                    ks = kbot - ns + 1_ilp
                    ! ==== small-bulge multi-shift qr sweep:
                    ! .    split workspace under the subdiagonal into
                    ! .    - a kdu-by-kdu work array u in the lower
                    ! .      left-hand-corner,
                    ! .    - a kdu-by-at-least-kdu-but-more-is-better
                    ! .      (kdu-by-nho) horizontal work array wh along
                    ! .      the bottom edge,
                    ! .    - and an at-least-kdu-but-more-is-better-by-kdu
                    ! .      (nve-by-kdu) vertical work wv arrow along
                    ! .      the left-hand-edge. ====
                    kdu = 2_ilp*ns
                    ku = n - kdu + 1_ilp
                    kwh = kdu + 1_ilp
                    nho = ( n-kdu+1-4 ) - ( kdu+1 ) + 1_ilp
                    kwv = kdu + 4_ilp
                    nve = n - kdu - kwv + 1_ilp
                    ! ==== small-bulge multi-shift qr sweep ====
                    call stdlib_zlaqr5( wantt, wantz, kacc22, n, ktop, kbot, ns,w( ks ), h, ldh, &
                    iloz, ihiz, z, ldz, work,3_ilp, h( ku, 1_ilp ), ldh, nve, h( kwv, 1_ilp ), ldh,nho, h( ku,&
                               kwh ), ldh )
                 end if
                 ! ==== note progress (or the lack of it). ====
                 if( ld>0_ilp ) then
                    ndfl = 1_ilp
                 else
                    ndfl = ndfl + 1_ilp
                 end if
                 ! ==== end of main loop ====
              end do loop_70
              ! ==== iteration limit exceeded.  set info to show where
              ! .    the problem occurred and exit. ====
              info = kbot
              80 continue
           end if
           ! ==== return the optimal value of lwork. ====
           work( 1_ilp ) = cmplx( lwkopt, 0_ilp,KIND=dp)
     end subroutine stdlib_zlaqr0




     pure module subroutine stdlib_slaqr1( n, h, ldh, sr1, si1, sr2, si2, v )
     !! Given a 2-by-2 or 3-by-3 matrix H, SLAQR1: sets v to a
     !! scalar multiple of the first column of the product
     !! (*)  K = (H - (sr1 + i*si1)*I)*(H - (sr2 + i*si2)*I)
     !! scaling to avoid overflows and most underflows. It
     !! is assumed that either
     !! 1) sr1 = sr2 and si1 = -si2
     !! or
     !! 2) si1 = si2 = 0.
     !! This is useful for starting double implicit shift bulges
     !! in the QR algorithm.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(sp), intent(in) :: si1, si2, sr1, sr2
           integer(ilp), intent(in) :: ldh, n
           ! Array Arguments 
           real(sp), intent(in) :: h(ldh,*)
           real(sp), intent(out) :: v(*)
        ! ================================================================
           
           ! Local Scalars 
           real(sp) :: h21s, h31s, s
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( n/=2_ilp .and. n/=3_ilp ) then
              return
           end if
           if( n==2_ilp ) then
              s = abs( h( 1_ilp, 1_ilp )-sr2 ) + abs( si2 ) + abs( h( 2_ilp, 1_ilp ) )
              if( s==zero ) then
                 v( 1_ilp ) = zero
                 v( 2_ilp ) = zero
              else
                 h21s = h( 2_ilp, 1_ilp ) / s
                 v( 1_ilp ) = h21s*h( 1_ilp, 2_ilp ) + ( h( 1_ilp, 1_ilp )-sr1 )*( ( h( 1_ilp, 1_ilp )-sr2 ) / s ) - si1*( &
                           si2 / s )
                 v( 2_ilp ) = h21s*( h( 1_ilp, 1_ilp )+h( 2_ilp, 2_ilp )-sr1-sr2 )
              end if
           else
              s = abs( h( 1_ilp, 1_ilp )-sr2 ) + abs( si2 ) + abs( h( 2_ilp, 1_ilp ) ) +abs( h( 3_ilp, 1_ilp ) )
              if( s==zero ) then
                 v( 1_ilp ) = zero
                 v( 2_ilp ) = zero
                 v( 3_ilp ) = zero
              else
                 h21s = h( 2_ilp, 1_ilp ) / s
                 h31s = h( 3_ilp, 1_ilp ) / s
                 v( 1_ilp ) = ( h( 1_ilp, 1_ilp )-sr1 )*( ( h( 1_ilp, 1_ilp )-sr2 ) / s ) -si1*( si2 / s ) + h( 1_ilp, 2_ilp )&
                           *h21s + h( 1_ilp, 3_ilp )*h31s
                 v( 2_ilp ) = h21s*( h( 1_ilp, 1_ilp )+h( 2_ilp, 2_ilp )-sr1-sr2 ) +h( 2_ilp, 3_ilp )*h31s
                 v( 3_ilp ) = h31s*( h( 1_ilp, 1_ilp )+h( 3_ilp, 3_ilp )-sr1-sr2 ) +h21s*h( 3_ilp, 2_ilp )
              end if
           end if
     end subroutine stdlib_slaqr1

     pure module subroutine stdlib_dlaqr1( n, h, ldh, sr1, si1, sr2, si2, v )
     !! Given a 2-by-2 or 3-by-3 matrix H, DLAQR1: sets v to a
     !! scalar multiple of the first column of the product
     !! (*)  K = (H - (sr1 + i*si1)*I)*(H - (sr2 + i*si2)*I)
     !! scaling to avoid overflows and most underflows. It
     !! is assumed that either
     !! 1) sr1 = sr2 and si1 = -si2
     !! or
     !! 2) si1 = si2 = 0.
     !! This is useful for starting double implicit shift bulges
     !! in the QR algorithm.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(dp), intent(in) :: si1, si2, sr1, sr2
           integer(ilp), intent(in) :: ldh, n
           ! Array Arguments 
           real(dp), intent(in) :: h(ldh,*)
           real(dp), intent(out) :: v(*)
        ! ================================================================
           
           ! Local Scalars 
           real(dp) :: h21s, h31s, s
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( n/=2_ilp .and. n/=3_ilp ) then
              return
           end if
           if( n==2_ilp ) then
              s = abs( h( 1_ilp, 1_ilp )-sr2 ) + abs( si2 ) + abs( h( 2_ilp, 1_ilp ) )
              if( s==zero ) then
                 v( 1_ilp ) = zero
                 v( 2_ilp ) = zero
              else
                 h21s = h( 2_ilp, 1_ilp ) / s
                 v( 1_ilp ) = h21s*h( 1_ilp, 2_ilp ) + ( h( 1_ilp, 1_ilp )-sr1 )*( ( h( 1_ilp, 1_ilp )-sr2 ) / s ) - si1*( &
                           si2 / s )
                 v( 2_ilp ) = h21s*( h( 1_ilp, 1_ilp )+h( 2_ilp, 2_ilp )-sr1-sr2 )
              end if
           else
              s = abs( h( 1_ilp, 1_ilp )-sr2 ) + abs( si2 ) + abs( h( 2_ilp, 1_ilp ) ) +abs( h( 3_ilp, 1_ilp ) )
              if( s==zero ) then
                 v( 1_ilp ) = zero
                 v( 2_ilp ) = zero
                 v( 3_ilp ) = zero
              else
                 h21s = h( 2_ilp, 1_ilp ) / s
                 h31s = h( 3_ilp, 1_ilp ) / s
                 v( 1_ilp ) = ( h( 1_ilp, 1_ilp )-sr1 )*( ( h( 1_ilp, 1_ilp )-sr2 ) / s ) -si1*( si2 / s ) + h( 1_ilp, 2_ilp )&
                           *h21s + h( 1_ilp, 3_ilp )*h31s
                 v( 2_ilp ) = h21s*( h( 1_ilp, 1_ilp )+h( 2_ilp, 2_ilp )-sr1-sr2 ) +h( 2_ilp, 3_ilp )*h31s
                 v( 3_ilp ) = h31s*( h( 1_ilp, 1_ilp )+h( 3_ilp, 3_ilp )-sr1-sr2 ) +h21s*h( 3_ilp, 2_ilp )
              end if
           end if
     end subroutine stdlib_dlaqr1


     pure module subroutine stdlib_claqr1( n, h, ldh, s1, s2, v )
     !! Given a 2-by-2 or 3-by-3 matrix H, CLAQR1: sets v to a
     !! scalar multiple of the first column of the product
     !! (*)  K = (H - s1*I)*(H - s2*I)
     !! scaling to avoid overflows and most underflows.
     !! This is useful for starting double implicit shift bulges
     !! in the QR algorithm.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           complex(sp), intent(in) :: s1, s2
           integer(ilp), intent(in) :: ldh, n
           ! Array Arguments 
           complex(sp), intent(in) :: h(ldh,*)
           complex(sp), intent(out) :: v(*)
        ! ================================================================
           ! Parameters 
           
           ! Local Scalars 
           complex(sp) :: cdum, h21s, h31s
           real(sp) :: s
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( cdum ) = abs( real( cdum,KIND=sp) ) + abs( aimag( cdum ) )
           ! Executable Statements 
           ! quick return if possible
           if( n/=2_ilp .and. n/=3_ilp ) then
              return
           end if
           if( n==2_ilp ) then
              s = cabs1( h( 1_ilp, 1_ilp )-s2 ) + cabs1( h( 2_ilp, 1_ilp ) )
              if( s==zero ) then
                 v( 1_ilp ) = czero
                 v( 2_ilp ) = czero
              else
                 h21s = h( 2_ilp, 1_ilp ) / s
                 v( 1_ilp ) = h21s*h( 1_ilp, 2_ilp ) + ( h( 1_ilp, 1_ilp )-s1 )*( ( h( 1_ilp, 1_ilp )-s2 ) / s )
                 v( 2_ilp ) = h21s*( h( 1_ilp, 1_ilp )+h( 2_ilp, 2_ilp )-s1-s2 )
              end if
           else
              s = cabs1( h( 1_ilp, 1_ilp )-s2 ) + cabs1( h( 2_ilp, 1_ilp ) ) +cabs1( h( 3_ilp, 1_ilp ) )
              if( s==czero ) then
                 v( 1_ilp ) = czero
                 v( 2_ilp ) = czero
                 v( 3_ilp ) = czero
              else
                 h21s = h( 2_ilp, 1_ilp ) / s
                 h31s = h( 3_ilp, 1_ilp ) / s
                 v( 1_ilp ) = ( h( 1_ilp, 1_ilp )-s1 )*( ( h( 1_ilp, 1_ilp )-s2 ) / s ) +h( 1_ilp, 2_ilp )*h21s + h( 1_ilp, 3_ilp )&
                           *h31s
                 v( 2_ilp ) = h21s*( h( 1_ilp, 1_ilp )+h( 2_ilp, 2_ilp )-s1-s2 ) + h( 2_ilp, 3_ilp )*h31s
                 v( 3_ilp ) = h31s*( h( 1_ilp, 1_ilp )+h( 3_ilp, 3_ilp )-s1-s2 ) + h21s*h( 3_ilp, 2_ilp )
              end if
           end if
     end subroutine stdlib_claqr1

     pure module subroutine stdlib_zlaqr1( n, h, ldh, s1, s2, v )
     !! Given a 2-by-2 or 3-by-3 matrix H, ZLAQR1: sets v to a
     !! scalar multiple of the first column of the product
     !! (*)  K = (H - s1*I)*(H - s2*I)
     !! scaling to avoid overflows and most underflows.
     !! This is useful for starting double implicit shift bulges
     !! in the QR algorithm.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           complex(dp), intent(in) :: s1, s2
           integer(ilp), intent(in) :: ldh, n
           ! Array Arguments 
           complex(dp), intent(in) :: h(ldh,*)
           complex(dp), intent(out) :: v(*)
        ! ================================================================
           ! Parameters 
           
           ! Local Scalars 
           complex(dp) :: cdum, h21s, h31s
           real(dp) :: s
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( cdum ) = abs( real( cdum,KIND=dp) ) + abs( aimag( cdum ) )
           ! Executable Statements 
           ! quick return if possible
           if( n/=2_ilp .and. n/=3_ilp ) then
              return
           end if
           if( n==2_ilp ) then
              s = cabs1( h( 1_ilp, 1_ilp )-s2 ) + cabs1( h( 2_ilp, 1_ilp ) )
              if( s==zero ) then
                 v( 1_ilp ) = czero
                 v( 2_ilp ) = czero
              else
                 h21s = h( 2_ilp, 1_ilp ) / s
                 v( 1_ilp ) = h21s*h( 1_ilp, 2_ilp ) + ( h( 1_ilp, 1_ilp )-s1 )*( ( h( 1_ilp, 1_ilp )-s2 ) / s )
                 v( 2_ilp ) = h21s*( h( 1_ilp, 1_ilp )+h( 2_ilp, 2_ilp )-s1-s2 )
              end if
           else
              s = cabs1( h( 1_ilp, 1_ilp )-s2 ) + cabs1( h( 2_ilp, 1_ilp ) ) +cabs1( h( 3_ilp, 1_ilp ) )
              if( s==czero ) then
                 v( 1_ilp ) = czero
                 v( 2_ilp ) = czero
                 v( 3_ilp ) = czero
              else
                 h21s = h( 2_ilp, 1_ilp ) / s
                 h31s = h( 3_ilp, 1_ilp ) / s
                 v( 1_ilp ) = ( h( 1_ilp, 1_ilp )-s1 )*( ( h( 1_ilp, 1_ilp )-s2 ) / s ) +h( 1_ilp, 2_ilp )*h21s + h( 1_ilp, 3_ilp )&
                           *h31s
                 v( 2_ilp ) = h21s*( h( 1_ilp, 1_ilp )+h( 2_ilp, 2_ilp )-s1-s2 ) + h( 2_ilp, 3_ilp )*h31s
                 v( 3_ilp ) = h31s*( h( 1_ilp, 1_ilp )+h( 3_ilp, 3_ilp )-s1-s2 ) + h21s*h( 3_ilp, 2_ilp )
              end if
           end if
     end subroutine stdlib_zlaqr1




     module subroutine stdlib_slaqr2( wantt, wantz, n, ktop, kbot, nw, h, ldh, iloz,ihiz, z, ldz, ns, nd,&
     !! SLAQR2 is identical to SLAQR3 except that it avoids
     !! recursion by calling SLAHQR instead of SLAQR4.
     !! Aggressive early deflation:
     !! This subroutine accepts as input an upper Hessenberg matrix
     !! H and performs an orthogonal similarity transformation
     !! designed to detect and deflate fully converged eigenvalues from
     !! a trailing principal submatrix.  On output H has been over-
     !! written by a new Hessenberg matrix that is a perturbation of
     !! an orthogonal similarity transformation of H.  It is to be
     !! hoped that the final version of H has many zero subdiagonal
     !! entries.
                sr, si, v, ldv, nh, t,ldt, nv, wv, ldwv, work, lwork )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ihiz, iloz, kbot, ktop, ldh, ldt, ldv, ldwv, ldz, lwork, n,&
                      nh, nv, nw
           integer(ilp), intent(out) :: nd, ns
           logical(lk), intent(in) :: wantt, wantz
           ! Array Arguments 
           real(sp), intent(inout) :: h(ldh,*), z(ldz,*)
           real(sp), intent(out) :: si(*), sr(*), t(ldt,*), v(ldv,*), work(*), wv(ldwv,*)
        ! ================================================================
           
           ! Local Scalars 
           real(sp) :: aa, bb, beta, cc, cs, dd, evi, evk, foo, s, safmax, safmin, smlnum, sn, &
                     tau, ulp
           integer(ilp) :: i, ifst, ilst, info, infqr, j, jw, k, kcol, kend, kln, krow, kwtop, &
                     ltop, lwk1, lwk2, lwkopt
           logical(lk) :: bulge, sorted
           ! Intrinsic Functions 
           ! Executable Statements 
           ! ==== estimate optimal workspace. ====
           jw = min( nw, kbot-ktop+1 )
           if( jw<=2_ilp ) then
              lwkopt = 1_ilp
           else
              ! ==== workspace query call to stdlib_sgehrd ====
              call stdlib_sgehrd( jw, 1_ilp, jw-1, t, ldt, work, work, -1_ilp, info )
              lwk1 = int( work( 1_ilp ),KIND=ilp)
              ! ==== workspace query call to stdlib_sormhr ====
              call stdlib_sormhr( 'R', 'N', jw, jw, 1_ilp, jw-1, t, ldt, work, v, ldv,work, -1_ilp, info )
                        
              lwk2 = int( work( 1_ilp ),KIND=ilp)
              ! ==== optimal workspace ====
              lwkopt = jw + max( lwk1, lwk2 )
           end if
           ! ==== quick return in case of workspace query. ====
           if( lwork==-1_ilp ) then
              work( 1_ilp ) = real( lwkopt,KIND=sp)
              return
           end if
           ! ==== nothing to do ...
           ! ... for an empty active block ... ====
           ns = 0_ilp
           nd = 0_ilp
           work( 1_ilp ) = one
           if( ktop>kbot )return
           ! ... nor for an empty deflation window. ====
           if( nw<1 )return
           ! ==== machine constants ====
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           safmax = one / safmin
           call stdlib_slabad( safmin, safmax )
           ulp = stdlib_slamch( 'PRECISION' )
           smlnum = safmin*( real( n,KIND=sp) / ulp )
           ! ==== setup deflation window ====
           jw = min( nw, kbot-ktop+1 )
           kwtop = kbot - jw + 1_ilp
           if( kwtop==ktop ) then
              s = zero
           else
              s = h( kwtop, kwtop-1 )
           end if
           if( kbot==kwtop ) then
              ! ==== 1-by-1 deflation window: not much to do ====
              sr( kwtop ) = h( kwtop, kwtop )
              si( kwtop ) = zero
              ns = 1_ilp
              nd = 0_ilp
              if( abs( s )<=max( smlnum, ulp*abs( h( kwtop, kwtop ) ) ) )then
                 ns = 0_ilp
                 nd = 1_ilp
                 if( kwtop>ktop )h( kwtop, kwtop-1 ) = zero
              end if
              work( 1_ilp ) = one
              return
           end if
           ! ==== convert to spike-triangular form.  (in case of a
           ! .    rare qr failure, this routine continues to do
           ! .    aggressive early deflation using that part of
           ! .    the deflation window that converged using infqr
           ! .    here and there to keep track.) ====
           call stdlib_slacpy( 'U', jw, jw, h( kwtop, kwtop ), ldh, t, ldt )
           call stdlib_scopy( jw-1, h( kwtop+1, kwtop ), ldh+1, t( 2_ilp, 1_ilp ), ldt+1 )
           call stdlib_slaset( 'A', jw, jw, zero, one, v, ldv )
           call stdlib_slahqr( .true., .true., jw, 1_ilp, jw, t, ldt, sr( kwtop ),si( kwtop ), 1_ilp, jw, &
                     v, ldv, infqr )
           ! ==== stdlib_strexc needs a clean margin near the diagonal ====
           do j = 1, jw - 3
              t( j+2, j ) = zero
              t( j+3, j ) = zero
           end do
           if( jw>2_ilp )t( jw, jw-2 ) = zero
           ! ==== deflation detection loop ====
           ns = jw
           ilst = infqr + 1_ilp
           20 continue
           if( ilst<=ns ) then
              if( ns==1_ilp ) then
                 bulge = .false.
              else
                 bulge = t( ns, ns-1 )/=zero
              end if
              ! ==== small spike tip test for deflation ====
              if( .not.bulge ) then
                 ! ==== real eigenvalue ====
                 foo = abs( t( ns, ns ) )
                 if( foo==zero )foo = abs( s )
                 if( abs( s*v( 1_ilp, ns ) )<=max( smlnum, ulp*foo ) ) then
                    ! ==== deflatable ====
                    ns = ns - 1_ilp
                 else
                    ! ==== undeflatable.   move it up out of the way.
                    ! .    (stdlib_strexc can not fail in this case.) ====
                    ifst = ns
                    call stdlib_strexc( 'V', jw, t, ldt, v, ldv, ifst, ilst, work,info )
                    ilst = ilst + 1_ilp
                 end if
              else
                 ! ==== complex conjugate pair ====
                 foo = abs( t( ns, ns ) ) + sqrt( abs( t( ns, ns-1 ) ) )*sqrt( abs( t( ns-1, ns ) &
                           ) )
                 if( foo==zero )foo = abs( s )
                 if( max( abs( s*v( 1_ilp, ns ) ), abs( s*v( 1_ilp, ns-1 ) ) )<=max( smlnum, ulp*foo ) ) &
                           then
                    ! ==== deflatable ====
                    ns = ns - 2_ilp
                 else
                    ! ==== undeflatable. move them up out of the way.
                    ! .    fortunately, stdlib_strexc does the right thing with
                    ! .    ilst in case of a rare exchange failure. ====
                    ifst = ns
                    call stdlib_strexc( 'V', jw, t, ldt, v, ldv, ifst, ilst, work,info )
                    ilst = ilst + 2_ilp
                 end if
              end if
              ! ==== end deflation detection loop ====
              go to 20
           end if
              ! ==== return to hessenberg form ====
           if( ns==0_ilp )s = zero
           if( ns<jw ) then
              ! ==== sorting diagonal blocks of t improves accuracy for
              ! .    graded matrices.  bubble sort deals well with
              ! .    exchange failures. ====
              sorted = .false.
              i = ns + 1_ilp
              30 continue
              if( sorted )go to 50
              sorted = .true.
              kend = i - 1_ilp
              i = infqr + 1_ilp
              if( i==ns ) then
                 k = i + 1_ilp
              else if( t( i+1, i )==zero ) then
                 k = i + 1_ilp
              else
                 k = i + 2_ilp
              end if
              40 continue
              if( k<=kend ) then
                 if( k==i+1 ) then
                    evi = abs( t( i, i ) )
                 else
                    evi = abs( t( i, i ) ) + sqrt( abs( t( i+1, i ) ) )*sqrt( abs( t( i, i+1 ) ) )
                              
                 end if
                 if( k==kend ) then
                    evk = abs( t( k, k ) )
                 else if( t( k+1, k )==zero ) then
                    evk = abs( t( k, k ) )
                 else
                    evk = abs( t( k, k ) ) + sqrt( abs( t( k+1, k ) ) )*sqrt( abs( t( k, k+1 ) ) )
                              
                 end if
                 if( evi>=evk ) then
                    i = k
                 else
                    sorted = .false.
                    ifst = i
                    ilst = k
                    call stdlib_strexc( 'V', jw, t, ldt, v, ldv, ifst, ilst, work,info )
                    if( info==0_ilp ) then
                       i = ilst
                    else
                       i = k
                    end if
                 end if
                 if( i==kend ) then
                    k = i + 1_ilp
                 else if( t( i+1, i )==zero ) then
                    k = i + 1_ilp
                 else
                    k = i + 2_ilp
                 end if
                 go to 40
              end if
              go to 30
              50 continue
           end if
           ! ==== restore shift/eigenvalue array from t ====
           i = jw
           60 continue
           if( i>=infqr+1 ) then
              if( i==infqr+1 ) then
                 sr( kwtop+i-1 ) = t( i, i )
                 si( kwtop+i-1 ) = zero
                 i = i - 1_ilp
              else if( t( i, i-1 )==zero ) then
                 sr( kwtop+i-1 ) = t( i, i )
                 si( kwtop+i-1 ) = zero
                 i = i - 1_ilp
              else
                 aa = t( i-1, i-1 )
                 cc = t( i, i-1 )
                 bb = t( i-1, i )
                 dd = t( i, i )
                 call stdlib_slanv2( aa, bb, cc, dd, sr( kwtop+i-2 ),si( kwtop+i-2 ), sr( kwtop+i-&
                           1_ilp ),si( kwtop+i-1 ), cs, sn )
                 i = i - 2_ilp
              end if
              go to 60
           end if
           if( ns<jw .or. s==zero ) then
              if( ns>1_ilp .and. s/=zero ) then
                 ! ==== reflect spike back into lower triangle ====
                 call stdlib_scopy( ns, v, ldv, work, 1_ilp )
                 beta = work( 1_ilp )
                 call stdlib_slarfg( ns, beta, work( 2_ilp ), 1_ilp, tau )
                 work( 1_ilp ) = one
                 call stdlib_slaset( 'L', jw-2, jw-2, zero, zero, t( 3_ilp, 1_ilp ), ldt )
                 call stdlib_slarf( 'L', ns, jw, work, 1_ilp, tau, t, ldt,work( jw+1 ) )
                 call stdlib_slarf( 'R', ns, ns, work, 1_ilp, tau, t, ldt,work( jw+1 ) )
                 call stdlib_slarf( 'R', jw, ns, work, 1_ilp, tau, v, ldv,work( jw+1 ) )
                 call stdlib_sgehrd( jw, 1_ilp, ns, t, ldt, work, work( jw+1 ),lwork-jw, info )
                           
              end if
              ! ==== copy updated reduced window into place ====
              if( kwtop>1_ilp )h( kwtop, kwtop-1 ) = s*v( 1_ilp, 1_ilp )
              call stdlib_slacpy( 'U', jw, jw, t, ldt, h( kwtop, kwtop ), ldh )
              call stdlib_scopy( jw-1, t( 2_ilp, 1_ilp ), ldt+1, h( kwtop+1, kwtop ),ldh+1 )
              ! ==== accumulate orthogonal matrix in order update
              ! .    h and z, if requested.  ====
              if( ns>1_ilp .and. s/=zero )call stdlib_sormhr( 'R', 'N', jw, ns, 1_ilp, ns, t, ldt, work, &
                        v, ldv,work( jw+1 ), lwork-jw, info )
              ! ==== update vertical slab in h ====
              if( wantt ) then
                 ltop = 1_ilp
              else
                 ltop = ktop
              end if
              do krow = ltop, kwtop - 1, nv
                 kln = min( nv, kwtop-krow )
                 call stdlib_sgemm( 'N', 'N', kln, jw, jw, one, h( krow, kwtop ),ldh, v, ldv, &
                           zero, wv, ldwv )
                 call stdlib_slacpy( 'A', kln, jw, wv, ldwv, h( krow, kwtop ), ldh )
              end do
              ! ==== update horizontal slab in h ====
              if( wantt ) then
                 do kcol = kbot + 1, n, nh
                    kln = min( nh, n-kcol+1 )
                    call stdlib_sgemm( 'C', 'N', jw, kln, jw, one, v, ldv,h( kwtop, kcol ), ldh, &
                              zero, t, ldt )
                    call stdlib_slacpy( 'A', jw, kln, t, ldt, h( kwtop, kcol ),ldh )
                 end do
              end if
              ! ==== update vertical slab in z ====
              if( wantz ) then
                 do krow = iloz, ihiz, nv
                    kln = min( nv, ihiz-krow+1 )
                    call stdlib_sgemm( 'N', 'N', kln, jw, jw, one, z( krow, kwtop ),ldz, v, ldv, &
                              zero, wv, ldwv )
                    call stdlib_slacpy( 'A', kln, jw, wv, ldwv, z( krow, kwtop ),ldz )
                 end do
              end if
           end if
           ! ==== return the number of deflations ... ====
           nd = jw - ns
           ! ==== ... and the number of shifts. (subtracting
           ! .    infqr from the spike length takes care
           ! .    of the case of a rare qr failure while
           ! .    calculating eigenvalues of the deflation
           ! .    window.)  ====
           ns = ns - infqr
            ! ==== return optimal workspace. ====
           work( 1_ilp ) = real( lwkopt,KIND=sp)
     end subroutine stdlib_slaqr2

     module subroutine stdlib_dlaqr2( wantt, wantz, n, ktop, kbot, nw, h, ldh, iloz,ihiz, z, ldz, ns, nd,&
     !! DLAQR2 is identical to DLAQR3 except that it avoids
     !! recursion by calling DLAHQR instead of DLAQR4.
     !! Aggressive early deflation:
     !! This subroutine accepts as input an upper Hessenberg matrix
     !! H and performs an orthogonal similarity transformation
     !! designed to detect and deflate fully converged eigenvalues from
     !! a trailing principal submatrix.  On output H has been over-
     !! written by a new Hessenberg matrix that is a perturbation of
     !! an orthogonal similarity transformation of H.  It is to be
     !! hoped that the final version of H has many zero subdiagonal
     !! entries.
                sr, si, v, ldv, nh, t,ldt, nv, wv, ldwv, work, lwork )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ihiz, iloz, kbot, ktop, ldh, ldt, ldv, ldwv, ldz, lwork, n,&
                      nh, nv, nw
           integer(ilp), intent(out) :: nd, ns
           logical(lk), intent(in) :: wantt, wantz
           ! Array Arguments 
           real(dp), intent(inout) :: h(ldh,*), z(ldz,*)
           real(dp), intent(out) :: si(*), sr(*), t(ldt,*), v(ldv,*), work(*), wv(ldwv,*)
        ! ================================================================
           
           ! Local Scalars 
           real(dp) :: aa, bb, beta, cc, cs, dd, evi, evk, foo, s, safmax, safmin, smlnum, sn, &
                     tau, ulp
           integer(ilp) :: i, ifst, ilst, info, infqr, j, jw, k, kcol, kend, kln, krow, kwtop, &
                     ltop, lwk1, lwk2, lwkopt
           logical(lk) :: bulge, sorted
           ! Intrinsic Functions 
           ! Executable Statements 
           ! ==== estimate optimal workspace. ====
           jw = min( nw, kbot-ktop+1 )
           if( jw<=2_ilp ) then
              lwkopt = 1_ilp
           else
              ! ==== workspace query call to stdlib_dgehrd ====
              call stdlib_dgehrd( jw, 1_ilp, jw-1, t, ldt, work, work, -1_ilp, info )
              lwk1 = int( work( 1_ilp ),KIND=ilp)
              ! ==== workspace query call to stdlib_dormhr ====
              call stdlib_dormhr( 'R', 'N', jw, jw, 1_ilp, jw-1, t, ldt, work, v, ldv,work, -1_ilp, info )
                        
              lwk2 = int( work( 1_ilp ),KIND=ilp)
              ! ==== optimal workspace ====
              lwkopt = jw + max( lwk1, lwk2 )
           end if
           ! ==== quick return in case of workspace query. ====
           if( lwork==-1_ilp ) then
              work( 1_ilp ) = real( lwkopt,KIND=dp)
              return
           end if
           ! ==== nothing to do ...
           ! ... for an empty active block ... ====
           ns = 0_ilp
           nd = 0_ilp
           work( 1_ilp ) = one
           if( ktop>kbot )return
           ! ... nor for an empty deflation window. ====
           if( nw<1 )return
           ! ==== machine constants ====
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           safmax = one / safmin
           call stdlib_dlabad( safmin, safmax )
           ulp = stdlib_dlamch( 'PRECISION' )
           smlnum = safmin*( real( n,KIND=dp) / ulp )
           ! ==== setup deflation window ====
           jw = min( nw, kbot-ktop+1 )
           kwtop = kbot - jw + 1_ilp
           if( kwtop==ktop ) then
              s = zero
           else
              s = h( kwtop, kwtop-1 )
           end if
           if( kbot==kwtop ) then
              ! ==== 1-by-1 deflation window: not much to do ====
              sr( kwtop ) = h( kwtop, kwtop )
              si( kwtop ) = zero
              ns = 1_ilp
              nd = 0_ilp
              if( abs( s )<=max( smlnum, ulp*abs( h( kwtop, kwtop ) ) ) )then
                 ns = 0_ilp
                 nd = 1_ilp
                 if( kwtop>ktop )h( kwtop, kwtop-1 ) = zero
              end if
              work( 1_ilp ) = one
              return
           end if
           ! ==== convert to spike-triangular form.  (in case of a
           ! .    rare qr failure, this routine continues to do
           ! .    aggressive early deflation using that part of
           ! .    the deflation window that converged using infqr
           ! .    here and there to keep track.) ====
           call stdlib_dlacpy( 'U', jw, jw, h( kwtop, kwtop ), ldh, t, ldt )
           call stdlib_dcopy( jw-1, h( kwtop+1, kwtop ), ldh+1, t( 2_ilp, 1_ilp ), ldt+1 )
           call stdlib_dlaset( 'A', jw, jw, zero, one, v, ldv )
           call stdlib_dlahqr( .true., .true., jw, 1_ilp, jw, t, ldt, sr( kwtop ),si( kwtop ), 1_ilp, jw, &
                     v, ldv, infqr )
           ! ==== stdlib_dtrexc needs a clean margin near the diagonal ====
           do j = 1, jw - 3
              t( j+2, j ) = zero
              t( j+3, j ) = zero
           end do
           if( jw>2_ilp )t( jw, jw-2 ) = zero
           ! ==== deflation detection loop ====
           ns = jw
           ilst = infqr + 1_ilp
           20 continue
           if( ilst<=ns ) then
              if( ns==1_ilp ) then
                 bulge = .false.
              else
                 bulge = t( ns, ns-1 )/=zero
              end if
              ! ==== small spike tip test for deflation ====
              if( .not.bulge ) then
                 ! ==== real eigenvalue ====
                 foo = abs( t( ns, ns ) )
                 if( foo==zero )foo = abs( s )
                 if( abs( s*v( 1_ilp, ns ) )<=max( smlnum, ulp*foo ) ) then
                    ! ==== deflatable ====
                    ns = ns - 1_ilp
                 else
                    ! ==== undeflatable.   move it up out of the way.
                    ! .    (stdlib_dtrexc can not fail in this case.) ====
                    ifst = ns
                    call stdlib_dtrexc( 'V', jw, t, ldt, v, ldv, ifst, ilst, work,info )
                    ilst = ilst + 1_ilp
                 end if
              else
                 ! ==== complex conjugate pair ====
                 foo = abs( t( ns, ns ) ) + sqrt( abs( t( ns, ns-1 ) ) )*sqrt( abs( t( ns-1, ns ) &
                           ) )
                 if( foo==zero )foo = abs( s )
                 if( max( abs( s*v( 1_ilp, ns ) ), abs( s*v( 1_ilp, ns-1 ) ) )<=max( smlnum, ulp*foo ) ) &
                           then
                    ! ==== deflatable ====
                    ns = ns - 2_ilp
                 else
                    ! ==== undeflatable. move them up out of the way.
                    ! .    fortunately, stdlib_dtrexc does the right thing with
                    ! .    ilst in case of a rare exchange failure. ====
                    ifst = ns
                    call stdlib_dtrexc( 'V', jw, t, ldt, v, ldv, ifst, ilst, work,info )
                    ilst = ilst + 2_ilp
                 end if
              end if
              ! ==== end deflation detection loop ====
              go to 20
           end if
              ! ==== return to hessenberg form ====
           if( ns==0_ilp )s = zero
           if( ns<jw ) then
              ! ==== sorting diagonal blocks of t improves accuracy for
              ! .    graded matrices.  bubble sort deals well with
              ! .    exchange failures. ====
              sorted = .false.
              i = ns + 1_ilp
              30 continue
              if( sorted )go to 50
              sorted = .true.
              kend = i - 1_ilp
              i = infqr + 1_ilp
              if( i==ns ) then
                 k = i + 1_ilp
              else if( t( i+1, i )==zero ) then
                 k = i + 1_ilp
              else
                 k = i + 2_ilp
              end if
              40 continue
              if( k<=kend ) then
                 if( k==i+1 ) then
                    evi = abs( t( i, i ) )
                 else
                    evi = abs( t( i, i ) ) + sqrt( abs( t( i+1, i ) ) )*sqrt( abs( t( i, i+1 ) ) )
                              
                 end if
                 if( k==kend ) then
                    evk = abs( t( k, k ) )
                 else if( t( k+1, k )==zero ) then
                    evk = abs( t( k, k ) )
                 else
                    evk = abs( t( k, k ) ) + sqrt( abs( t( k+1, k ) ) )*sqrt( abs( t( k, k+1 ) ) )
                              
                 end if
                 if( evi>=evk ) then
                    i = k
                 else
                    sorted = .false.
                    ifst = i
                    ilst = k
                    call stdlib_dtrexc( 'V', jw, t, ldt, v, ldv, ifst, ilst, work,info )
                    if( info==0_ilp ) then
                       i = ilst
                    else
                       i = k
                    end if
                 end if
                 if( i==kend ) then
                    k = i + 1_ilp
                 else if( t( i+1, i )==zero ) then
                    k = i + 1_ilp
                 else
                    k = i + 2_ilp
                 end if
                 go to 40
              end if
              go to 30
              50 continue
           end if
           ! ==== restore shift/eigenvalue array from t ====
           i = jw
           60 continue
           if( i>=infqr+1 ) then
              if( i==infqr+1 ) then
                 sr( kwtop+i-1 ) = t( i, i )
                 si( kwtop+i-1 ) = zero
                 i = i - 1_ilp
              else if( t( i, i-1 )==zero ) then
                 sr( kwtop+i-1 ) = t( i, i )
                 si( kwtop+i-1 ) = zero
                 i = i - 1_ilp
              else
                 aa = t( i-1, i-1 )
                 cc = t( i, i-1 )
                 bb = t( i-1, i )
                 dd = t( i, i )
                 call stdlib_dlanv2( aa, bb, cc, dd, sr( kwtop+i-2 ),si( kwtop+i-2 ), sr( kwtop+i-&
                           1_ilp ),si( kwtop+i-1 ), cs, sn )
                 i = i - 2_ilp
              end if
              go to 60
           end if
           if( ns<jw .or. s==zero ) then
              if( ns>1_ilp .and. s/=zero ) then
                 ! ==== reflect spike back into lower triangle ====
                 call stdlib_dcopy( ns, v, ldv, work, 1_ilp )
                 beta = work( 1_ilp )
                 call stdlib_dlarfg( ns, beta, work( 2_ilp ), 1_ilp, tau )
                 work( 1_ilp ) = one
                 call stdlib_dlaset( 'L', jw-2, jw-2, zero, zero, t( 3_ilp, 1_ilp ), ldt )
                 call stdlib_dlarf( 'L', ns, jw, work, 1_ilp, tau, t, ldt,work( jw+1 ) )
                 call stdlib_dlarf( 'R', ns, ns, work, 1_ilp, tau, t, ldt,work( jw+1 ) )
                 call stdlib_dlarf( 'R', jw, ns, work, 1_ilp, tau, v, ldv,work( jw+1 ) )
                 call stdlib_dgehrd( jw, 1_ilp, ns, t, ldt, work, work( jw+1 ),lwork-jw, info )
                           
              end if
              ! ==== copy updated reduced window into place ====
              if( kwtop>1_ilp )h( kwtop, kwtop-1 ) = s*v( 1_ilp, 1_ilp )
              call stdlib_dlacpy( 'U', jw, jw, t, ldt, h( kwtop, kwtop ), ldh )
              call stdlib_dcopy( jw-1, t( 2_ilp, 1_ilp ), ldt+1, h( kwtop+1, kwtop ),ldh+1 )
              ! ==== accumulate orthogonal matrix in order update
              ! .    h and z, if requested.  ====
              if( ns>1_ilp .and. s/=zero )call stdlib_dormhr( 'R', 'N', jw, ns, 1_ilp, ns, t, ldt, work, &
                        v, ldv,work( jw+1 ), lwork-jw, info )
              ! ==== update vertical slab in h ====
              if( wantt ) then
                 ltop = 1_ilp
              else
                 ltop = ktop
              end if
              do krow = ltop, kwtop - 1, nv
                 kln = min( nv, kwtop-krow )
                 call stdlib_dgemm( 'N', 'N', kln, jw, jw, one, h( krow, kwtop ),ldh, v, ldv, &
                           zero, wv, ldwv )
                 call stdlib_dlacpy( 'A', kln, jw, wv, ldwv, h( krow, kwtop ), ldh )
              end do
              ! ==== update horizontal slab in h ====
              if( wantt ) then
                 do kcol = kbot + 1, n, nh
                    kln = min( nh, n-kcol+1 )
                    call stdlib_dgemm( 'C', 'N', jw, kln, jw, one, v, ldv,h( kwtop, kcol ), ldh, &
                              zero, t, ldt )
                    call stdlib_dlacpy( 'A', jw, kln, t, ldt, h( kwtop, kcol ),ldh )
                 end do
              end if
              ! ==== update vertical slab in z ====
              if( wantz ) then
                 do krow = iloz, ihiz, nv
                    kln = min( nv, ihiz-krow+1 )
                    call stdlib_dgemm( 'N', 'N', kln, jw, jw, one, z( krow, kwtop ),ldz, v, ldv, &
                              zero, wv, ldwv )
                    call stdlib_dlacpy( 'A', kln, jw, wv, ldwv, z( krow, kwtop ),ldz )
                 end do
              end if
           end if
           ! ==== return the number of deflations ... ====
           nd = jw - ns
           ! ==== ... and the number of shifts. (subtracting
           ! .    infqr from the spike length takes care
           ! .    of the case of a rare qr failure while
           ! .    calculating eigenvalues of the deflation
           ! .    window.)  ====
           ns = ns - infqr
            ! ==== return optimal workspace. ====
           work( 1_ilp ) = real( lwkopt,KIND=dp)
     end subroutine stdlib_dlaqr2


     pure module subroutine stdlib_claqr2( wantt, wantz, n, ktop, kbot, nw, h, ldh, iloz,ihiz, z, ldz, &
     !! CLAQR2 is identical to CLAQR3 except that it avoids
     !! recursion by calling CLAHQR instead of CLAQR4.
     !! Aggressive early deflation:
     !! This subroutine accepts as input an upper Hessenberg matrix
     !! H and performs an unitary similarity transformation
     !! designed to detect and deflate fully converged eigenvalues from
     !! a trailing principal submatrix.  On output H has been over-
     !! written by a new Hessenberg matrix that is a perturbation of
     !! an unitary similarity transformation of H.  It is to be
     !! hoped that the final version of H has many zero subdiagonal
     !! entries.
               ns, nd, sh, v, ldv, nh, t, ldt,nv, wv, ldwv, work, lwork )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ihiz, iloz, kbot, ktop, ldh, ldt, ldv, ldwv, ldz, lwork, n,&
                      nh, nv, nw
           integer(ilp), intent(out) :: nd, ns
           logical(lk), intent(in) :: wantt, wantz
           ! Array Arguments 
           complex(sp), intent(inout) :: h(ldh,*), z(ldz,*)
           complex(sp), intent(out) :: sh(*), t(ldt,*), v(ldv,*), work(*), wv(ldwv,*)
        ! ================================================================
           ! Parameters 
           
           ! Local Scalars 
           complex(sp) :: beta, cdum, s, tau
           real(sp) :: foo, safmax, safmin, smlnum, ulp
           integer(ilp) :: i, ifst, ilst, info, infqr, j, jw, kcol, kln, knt, krow, kwtop, ltop, &
                     lwk1, lwk2, lwkopt
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( cdum ) = abs( real( cdum,KIND=sp) ) + abs( aimag( cdum ) )
           ! Executable Statements 
           ! ==== estimate optimal workspace. ====
           jw = min( nw, kbot-ktop+1 )
           if( jw<=2_ilp ) then
              lwkopt = 1_ilp
           else
              ! ==== workspace query call to stdlib_cgehrd ====
              call stdlib_cgehrd( jw, 1_ilp, jw-1, t, ldt, work, work, -1_ilp, info )
              lwk1 = int( work( 1_ilp ),KIND=ilp)
              ! ==== workspace query call to stdlib_cunmhr ====
              call stdlib_cunmhr( 'R', 'N', jw, jw, 1_ilp, jw-1, t, ldt, work, v, ldv,work, -1_ilp, info )
                        
              lwk2 = int( work( 1_ilp ),KIND=ilp)
              ! ==== optimal workspace ====
              lwkopt = jw + max( lwk1, lwk2 )
           end if
           ! ==== quick return in case of workspace query. ====
           if( lwork==-1_ilp ) then
              work( 1_ilp ) = cmplx( lwkopt, 0_ilp,KIND=sp)
              return
           end if
           ! ==== nothing to do ...
           ! ... for an empty active block ... ====
           ns = 0_ilp
           nd = 0_ilp
           work( 1_ilp ) = cone
           if( ktop>kbot )return
           ! ... nor for an empty deflation window. ====
           if( nw<1 )return
           ! ==== machine constants ====
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           safmax = one / safmin
           call stdlib_slabad( safmin, safmax )
           ulp = stdlib_slamch( 'PRECISION' )
           smlnum = safmin*( real( n,KIND=sp) / ulp )
           ! ==== setup deflation window ====
           jw = min( nw, kbot-ktop+1 )
           kwtop = kbot - jw + 1_ilp
           if( kwtop==ktop ) then
              s = czero
           else
              s = h( kwtop, kwtop-1 )
           end if
           if( kbot==kwtop ) then
              ! ==== 1-by-1 deflation window: not much to do ====
              sh( kwtop ) = h( kwtop, kwtop )
              ns = 1_ilp
              nd = 0_ilp
              if( cabs1( s )<=max( smlnum, ulp*cabs1( h( kwtop,kwtop ) ) ) ) then
                 ns = 0_ilp
                 nd = 1_ilp
                 if( kwtop>ktop )h( kwtop, kwtop-1 ) = czero
              end if
              work( 1_ilp ) = cone
              return
           end if
           ! ==== convert to spike-triangular form.  (in case of a
           ! .    rare qr failure, this routine continues to do
           ! .    aggressive early deflation using that part of
           ! .    the deflation window that converged using infqr
           ! .    here and there to keep track.) ====
           call stdlib_clacpy( 'U', jw, jw, h( kwtop, kwtop ), ldh, t, ldt )
           call stdlib_ccopy( jw-1, h( kwtop+1, kwtop ), ldh+1, t( 2_ilp, 1_ilp ), ldt+1 )
           call stdlib_claset( 'A', jw, jw, czero, cone, v, ldv )
           call stdlib_clahqr( .true., .true., jw, 1_ilp, jw, t, ldt, sh( kwtop ), 1_ilp,jw, v, ldv, &
                     infqr )
           ! ==== deflation detection loop ====
           ns = jw
           ilst = infqr + 1_ilp
           do knt = infqr + 1, jw
              ! ==== small spike tip deflation test ====
              foo = cabs1( t( ns, ns ) )
              if( foo==zero )foo = cabs1( s )
              if( cabs1( s )*cabs1( v( 1_ilp, ns ) )<=max( smlnum, ulp*foo ) )then
                 ! ==== cone more converged eigenvalue ====
                 ns = ns - 1_ilp
              else
                 ! ==== cone undeflatable eigenvalue.  move it up out of the
                 ! .    way.   (stdlib_ctrexc can not fail in this case.) ====
                 ifst = ns
                 call stdlib_ctrexc( 'V', jw, t, ldt, v, ldv, ifst, ilst, info )
                 ilst = ilst + 1_ilp
              end if
           end do
              ! ==== return to hessenberg form ====
           if( ns==0_ilp )s = czero
           if( ns<jw ) then
              ! ==== sorting the diagonal of t improves accuracy for
              ! .    graded matrices.  ====
              do i = infqr + 1, ns
                 ifst = i
                 do j = i + 1, ns
                    if( cabs1( t( j, j ) )>cabs1( t( ifst, ifst ) ) )ifst = j
                 end do
                 ilst = i
                 if( ifst/=ilst )call stdlib_ctrexc( 'V', jw, t, ldt, v, ldv, ifst, ilst, info )
                           
              end do
           end if
           ! ==== restore shift/eigenvalue array from t ====
           do i = infqr + 1, jw
              sh( kwtop+i-1 ) = t( i, i )
           end do
           if( ns<jw .or. s==czero ) then
              if( ns>1_ilp .and. s/=czero ) then
                 ! ==== reflect spike back into lower triangle ====
                 call stdlib_ccopy( ns, v, ldv, work, 1_ilp )
                 do i = 1, ns
                    work( i ) = conjg( work( i ) )
                 end do
                 beta = work( 1_ilp )
                 call stdlib_clarfg( ns, beta, work( 2_ilp ), 1_ilp, tau )
                 work( 1_ilp ) = cone
                 call stdlib_claset( 'L', jw-2, jw-2, czero, czero, t( 3_ilp, 1_ilp ), ldt )
                 call stdlib_clarf( 'L', ns, jw, work, 1_ilp, conjg( tau ), t, ldt,work( jw+1 ) )
                           
                 call stdlib_clarf( 'R', ns, ns, work, 1_ilp, tau, t, ldt,work( jw+1 ) )
                 call stdlib_clarf( 'R', jw, ns, work, 1_ilp, tau, v, ldv,work( jw+1 ) )
                 call stdlib_cgehrd( jw, 1_ilp, ns, t, ldt, work, work( jw+1 ),lwork-jw, info )
                           
              end if
              ! ==== copy updated reduced window into place ====
              if( kwtop>1_ilp )h( kwtop, kwtop-1 ) = s*conjg( v( 1_ilp, 1_ilp ) )
              call stdlib_clacpy( 'U', jw, jw, t, ldt, h( kwtop, kwtop ), ldh )
              call stdlib_ccopy( jw-1, t( 2_ilp, 1_ilp ), ldt+1, h( kwtop+1, kwtop ),ldh+1 )
              ! ==== accumulate orthogonal matrix in order update
              ! .    h and z, if requested.  ====
              if( ns>1_ilp .and. s/=czero )call stdlib_cunmhr( 'R', 'N', jw, ns, 1_ilp, ns, t, ldt, work, &
                        v, ldv,work( jw+1 ), lwork-jw, info )
              ! ==== update vertical slab in h ====
              if( wantt ) then
                 ltop = 1_ilp
              else
                 ltop = ktop
              end if
              do krow = ltop, kwtop - 1, nv
                 kln = min( nv, kwtop-krow )
                 call stdlib_cgemm( 'N', 'N', kln, jw, jw, cone, h( krow, kwtop ),ldh, v, ldv, &
                           czero, wv, ldwv )
                 call stdlib_clacpy( 'A', kln, jw, wv, ldwv, h( krow, kwtop ), ldh )
              end do
              ! ==== update horizontal slab in h ====
              if( wantt ) then
                 do kcol = kbot + 1, n, nh
                    kln = min( nh, n-kcol+1 )
                    call stdlib_cgemm( 'C', 'N', jw, kln, jw, cone, v, ldv,h( kwtop, kcol ), ldh, &
                              czero, t, ldt )
                    call stdlib_clacpy( 'A', jw, kln, t, ldt, h( kwtop, kcol ),ldh )
                 end do
              end if
              ! ==== update vertical slab in z ====
              if( wantz ) then
                 do krow = iloz, ihiz, nv
                    kln = min( nv, ihiz-krow+1 )
                    call stdlib_cgemm( 'N', 'N', kln, jw, jw, cone, z( krow, kwtop ),ldz, v, ldv, &
                              czero, wv, ldwv )
                    call stdlib_clacpy( 'A', kln, jw, wv, ldwv, z( krow, kwtop ),ldz )
                 end do
              end if
           end if
           ! ==== return the number of deflations ... ====
           nd = jw - ns
           ! ==== ... and the number of shifts. (subtracting
           ! .    infqr from the spike length takes care
           ! .    of the case of a rare qr failure while
           ! .    calculating eigenvalues of the deflation
           ! .    window.)  ====
           ns = ns - infqr
            ! ==== return optimal workspace. ====
           work( 1_ilp ) = cmplx( lwkopt, 0_ilp,KIND=sp)
     end subroutine stdlib_claqr2

     pure module subroutine stdlib_zlaqr2( wantt, wantz, n, ktop, kbot, nw, h, ldh, iloz,ihiz, z, ldz, &
     !! ZLAQR2 is identical to ZLAQR3 except that it avoids
     !! recursion by calling ZLAHQR instead of ZLAQR4.
     !! Aggressive early deflation:
     !! ZLAQR2 accepts as input an upper Hessenberg matrix
     !! H and performs an unitary similarity transformation
     !! designed to detect and deflate fully converged eigenvalues from
     !! a trailing principal submatrix.  On output H has been over-
     !! written by a new Hessenberg matrix that is a perturbation of
     !! an unitary similarity transformation of H.  It is to be
     !! hoped that the final version of H has many zero subdiagonal
     !! entries.
               ns, nd, sh, v, ldv, nh, t, ldt,nv, wv, ldwv, work, lwork )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ihiz, iloz, kbot, ktop, ldh, ldt, ldv, ldwv, ldz, lwork, n,&
                      nh, nv, nw
           integer(ilp), intent(out) :: nd, ns
           logical(lk), intent(in) :: wantt, wantz
           ! Array Arguments 
           complex(dp), intent(inout) :: h(ldh,*), z(ldz,*)
           complex(dp), intent(out) :: sh(*), t(ldt,*), v(ldv,*), work(*), wv(ldwv,*)
        ! ================================================================
           ! Parameters 
           
           ! Local Scalars 
           complex(dp) :: beta, cdum, s, tau
           real(dp) :: foo, safmax, safmin, smlnum, ulp
           integer(ilp) :: i, ifst, ilst, info, infqr, j, jw, kcol, kln, knt, krow, kwtop, ltop, &
                     lwk1, lwk2, lwkopt
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( cdum ) = abs( real( cdum,KIND=dp) ) + abs( aimag( cdum ) )
           ! Executable Statements 
           ! ==== estimate optimal workspace. ====
           jw = min( nw, kbot-ktop+1 )
           if( jw<=2_ilp ) then
              lwkopt = 1_ilp
           else
              ! ==== workspace query call to stdlib_zgehrd ====
              call stdlib_zgehrd( jw, 1_ilp, jw-1, t, ldt, work, work, -1_ilp, info )
              lwk1 = int( work( 1_ilp ),KIND=ilp)
              ! ==== workspace query call to stdlib_zunmhr ====
              call stdlib_zunmhr( 'R', 'N', jw, jw, 1_ilp, jw-1, t, ldt, work, v, ldv,work, -1_ilp, info )
                        
              lwk2 = int( work( 1_ilp ),KIND=ilp)
              ! ==== optimal workspace ====
              lwkopt = jw + max( lwk1, lwk2 )
           end if
           ! ==== quick return in case of workspace query. ====
           if( lwork==-1_ilp ) then
              work( 1_ilp ) = cmplx( lwkopt, 0_ilp,KIND=dp)
              return
           end if
           ! ==== nothing to do ...
           ! ... for an empty active block ... ====
           ns = 0_ilp
           nd = 0_ilp
           work( 1_ilp ) = cone
           if( ktop>kbot )return
           ! ... nor for an empty deflation window. ====
           if( nw<1 )return
           ! ==== machine constants ====
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           safmax = one / safmin
           call stdlib_dlabad( safmin, safmax )
           ulp = stdlib_dlamch( 'PRECISION' )
           smlnum = safmin*( real( n,KIND=dp) / ulp )
           ! ==== setup deflation window ====
           jw = min( nw, kbot-ktop+1 )
           kwtop = kbot - jw + 1_ilp
           if( kwtop==ktop ) then
              s = czero
           else
              s = h( kwtop, kwtop-1 )
           end if
           if( kbot==kwtop ) then
              ! ==== 1-by-1 deflation window: not much to do ====
              sh( kwtop ) = h( kwtop, kwtop )
              ns = 1_ilp
              nd = 0_ilp
              if( cabs1( s )<=max( smlnum, ulp*cabs1( h( kwtop,kwtop ) ) ) ) then
                 ns = 0_ilp
                 nd = 1_ilp
                 if( kwtop>ktop )h( kwtop, kwtop-1 ) = czero
              end if
              work( 1_ilp ) = cone
              return
           end if
           ! ==== convert to spike-triangular form.  (in case of a
           ! .    rare qr failure, this routine continues to do
           ! .    aggressive early deflation using that part of
           ! .    the deflation window that converged using infqr
           ! .    here and there to keep track.) ====
           call stdlib_zlacpy( 'U', jw, jw, h( kwtop, kwtop ), ldh, t, ldt )
           call stdlib_zcopy( jw-1, h( kwtop+1, kwtop ), ldh+1, t( 2_ilp, 1_ilp ), ldt+1 )
           call stdlib_zlaset( 'A', jw, jw, czero, cone, v, ldv )
           call stdlib_zlahqr( .true., .true., jw, 1_ilp, jw, t, ldt, sh( kwtop ), 1_ilp,jw, v, ldv, &
                     infqr )
           ! ==== deflation detection loop ====
           ns = jw
           ilst = infqr + 1_ilp
           do knt = infqr + 1, jw
              ! ==== small spike tip deflation test ====
              foo = cabs1( t( ns, ns ) )
              if( foo==zero )foo = cabs1( s )
              if( cabs1( s )*cabs1( v( 1_ilp, ns ) )<=max( smlnum, ulp*foo ) )then
                 ! ==== cone more converged eigenvalue ====
                 ns = ns - 1_ilp
              else
                 ! ==== cone undeflatable eigenvalue.  move it up out of the
                 ! .    way.   (stdlib_ztrexc can not fail in this case.) ====
                 ifst = ns
                 call stdlib_ztrexc( 'V', jw, t, ldt, v, ldv, ifst, ilst, info )
                 ilst = ilst + 1_ilp
              end if
           end do
              ! ==== return to hessenberg form ====
           if( ns==0_ilp )s = czero
           if( ns<jw ) then
              ! ==== sorting the diagonal of t improves accuracy for
              ! .    graded matrices.  ====
              do i = infqr + 1, ns
                 ifst = i
                 do j = i + 1, ns
                    if( cabs1( t( j, j ) )>cabs1( t( ifst, ifst ) ) )ifst = j
                 end do
                 ilst = i
                 if( ifst/=ilst )call stdlib_ztrexc( 'V', jw, t, ldt, v, ldv, ifst, ilst, info )
                           
              end do
           end if
           ! ==== restore shift/eigenvalue array from t ====
           do i = infqr + 1, jw
              sh( kwtop+i-1 ) = t( i, i )
           end do
           if( ns<jw .or. s==czero ) then
              if( ns>1_ilp .and. s/=czero ) then
                 ! ==== reflect spike back into lower triangle ====
                 call stdlib_zcopy( ns, v, ldv, work, 1_ilp )
                 do i = 1, ns
                    work( i ) = conjg( work( i ) )
                 end do
                 beta = work( 1_ilp )
                 call stdlib_zlarfg( ns, beta, work( 2_ilp ), 1_ilp, tau )
                 work( 1_ilp ) = cone
                 call stdlib_zlaset( 'L', jw-2, jw-2, czero, czero, t( 3_ilp, 1_ilp ), ldt )
                 call stdlib_zlarf( 'L', ns, jw, work, 1_ilp, conjg( tau ), t, ldt,work( jw+1 ) )
                           
                 call stdlib_zlarf( 'R', ns, ns, work, 1_ilp, tau, t, ldt,work( jw+1 ) )
                 call stdlib_zlarf( 'R', jw, ns, work, 1_ilp, tau, v, ldv,work( jw+1 ) )
                 call stdlib_zgehrd( jw, 1_ilp, ns, t, ldt, work, work( jw+1 ),lwork-jw, info )
                           
              end if
              ! ==== copy updated reduced window into place ====
              if( kwtop>1_ilp )h( kwtop, kwtop-1 ) = s*conjg( v( 1_ilp, 1_ilp ) )
              call stdlib_zlacpy( 'U', jw, jw, t, ldt, h( kwtop, kwtop ), ldh )
              call stdlib_zcopy( jw-1, t( 2_ilp, 1_ilp ), ldt+1, h( kwtop+1, kwtop ),ldh+1 )
              ! ==== accumulate orthogonal matrix in order update
              ! .    h and z, if requested.  ====
              if( ns>1_ilp .and. s/=czero )call stdlib_zunmhr( 'R', 'N', jw, ns, 1_ilp, ns, t, ldt, work, &
                        v, ldv,work( jw+1 ), lwork-jw, info )
              ! ==== update vertical slab in h ====
              if( wantt ) then
                 ltop = 1_ilp
              else
                 ltop = ktop
              end if
              do krow = ltop, kwtop - 1, nv
                 kln = min( nv, kwtop-krow )
                 call stdlib_zgemm( 'N', 'N', kln, jw, jw, cone, h( krow, kwtop ),ldh, v, ldv, &
                           czero, wv, ldwv )
                 call stdlib_zlacpy( 'A', kln, jw, wv, ldwv, h( krow, kwtop ), ldh )
              end do
              ! ==== update horizontal slab in h ====
              if( wantt ) then
                 do kcol = kbot + 1, n, nh
                    kln = min( nh, n-kcol+1 )
                    call stdlib_zgemm( 'C', 'N', jw, kln, jw, cone, v, ldv,h( kwtop, kcol ), ldh, &
                              czero, t, ldt )
                    call stdlib_zlacpy( 'A', jw, kln, t, ldt, h( kwtop, kcol ),ldh )
                 end do
              end if
              ! ==== update vertical slab in z ====
              if( wantz ) then
                 do krow = iloz, ihiz, nv
                    kln = min( nv, ihiz-krow+1 )
                    call stdlib_zgemm( 'N', 'N', kln, jw, jw, cone, z( krow, kwtop ),ldz, v, ldv, &
                              czero, wv, ldwv )
                    call stdlib_zlacpy( 'A', kln, jw, wv, ldwv, z( krow, kwtop ),ldz )
                 end do
              end if
           end if
           ! ==== return the number of deflations ... ====
           nd = jw - ns
           ! ==== ... and the number of shifts. (subtracting
           ! .    infqr from the spike length takes care
           ! .    of the case of a rare qr failure while
           ! .    calculating eigenvalues of the deflation
           ! .    window.)  ====
           ns = ns - infqr
            ! ==== return optimal workspace. ====
           work( 1_ilp ) = cmplx( lwkopt, 0_ilp,KIND=dp)
     end subroutine stdlib_zlaqr2




     module subroutine stdlib_slaqr3( wantt, wantz, n, ktop, kbot, nw, h, ldh, iloz,ihiz, z, ldz, ns, nd,&
     !! Aggressive early deflation:
     !! SLAQR3 accepts as input an upper Hessenberg matrix
     !! H and performs an orthogonal similarity transformation
     !! designed to detect and deflate fully converged eigenvalues from
     !! a trailing principal submatrix.  On output H has been over-
     !! written by a new Hessenberg matrix that is a perturbation of
     !! an orthogonal similarity transformation of H.  It is to be
     !! hoped that the final version of H has many zero subdiagonal
     !! entries.
                sr, si, v, ldv, nh, t,ldt, nv, wv, ldwv, work, lwork )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ihiz, iloz, kbot, ktop, ldh, ldt, ldv, ldwv, ldz, lwork, n,&
                      nh, nv, nw
           integer(ilp), intent(out) :: nd, ns
           logical(lk), intent(in) :: wantt, wantz
           ! Array Arguments 
           real(sp), intent(inout) :: h(ldh,*), z(ldz,*)
           real(sp), intent(out) :: si(*), sr(*), t(ldt,*), v(ldv,*), work(*), wv(ldwv,*)
        ! ================================================================
           
           ! Local Scalars 
           real(sp) :: aa, bb, beta, cc, cs, dd, evi, evk, foo, s, safmax, safmin, smlnum, sn, &
                     tau, ulp
           integer(ilp) :: i, ifst, ilst, info, infqr, j, jw, k, kcol, kend, kln, krow, kwtop, &
                     ltop, lwk1, lwk2, lwk3, lwkopt, nmin
           logical(lk) :: bulge, sorted
           ! Intrinsic Functions 
           ! Executable Statements 
           ! ==== estimate optimal workspace. ====
           jw = min( nw, kbot-ktop+1 )
           if( jw<=2_ilp ) then
              lwkopt = 1_ilp
           else
              ! ==== workspace query call to stdlib_sgehrd ====
              call stdlib_sgehrd( jw, 1_ilp, jw-1, t, ldt, work, work, -1_ilp, info )
              lwk1 = int( work( 1_ilp ),KIND=ilp)
              ! ==== workspace query call to stdlib_sormhr ====
              call stdlib_sormhr( 'R', 'N', jw, jw, 1_ilp, jw-1, t, ldt, work, v, ldv,work, -1_ilp, info )
                        
              lwk2 = int( work( 1_ilp ),KIND=ilp)
              ! ==== workspace query call to stdlib_slaqr4 ====
              call stdlib_slaqr4( .true., .true., jw, 1_ilp, jw, t, ldt, sr, si, 1_ilp, jw,v, ldv, work, -&
                        1_ilp, infqr )
              lwk3 = int( work( 1_ilp ),KIND=ilp)
              ! ==== optimal workspace ====
              lwkopt = max( jw+max( lwk1, lwk2 ), lwk3 )
           end if
           ! ==== quick return in case of workspace query. ====
           if( lwork==-1_ilp ) then
              work( 1_ilp ) = real( lwkopt,KIND=sp)
              return
           end if
           ! ==== nothing to do ...
           ! ... for an empty active block ... ====
           ns = 0_ilp
           nd = 0_ilp
           work( 1_ilp ) = one
           if( ktop>kbot )return
           ! ... nor for an empty deflation window. ====
           if( nw<1 )return
           ! ==== machine constants ====
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           safmax = one / safmin
           call stdlib_slabad( safmin, safmax )
           ulp = stdlib_slamch( 'PRECISION' )
           smlnum = safmin*( real( n,KIND=sp) / ulp )
           ! ==== setup deflation window ====
           jw = min( nw, kbot-ktop+1 )
           kwtop = kbot - jw + 1_ilp
           if( kwtop==ktop ) then
              s = zero
           else
              s = h( kwtop, kwtop-1 )
           end if
           if( kbot==kwtop ) then
              ! ==== 1-by-1 deflation window: not much to do ====
              sr( kwtop ) = h( kwtop, kwtop )
              si( kwtop ) = zero
              ns = 1_ilp
              nd = 0_ilp
              if( abs( s )<=max( smlnum, ulp*abs( h( kwtop, kwtop ) ) ) )then
                 ns = 0_ilp
                 nd = 1_ilp
                 if( kwtop>ktop )h( kwtop, kwtop-1 ) = zero
              end if
              work( 1_ilp ) = one
              return
           end if
           ! ==== convert to spike-triangular form.  (in case of a
           ! .    rare qr failure, this routine continues to do
           ! .    aggressive early deflation using that part of
           ! .    the deflation window that converged using infqr
           ! .    here and there to keep track.) ====
           call stdlib_slacpy( 'U', jw, jw, h( kwtop, kwtop ), ldh, t, ldt )
           call stdlib_scopy( jw-1, h( kwtop+1, kwtop ), ldh+1, t( 2_ilp, 1_ilp ), ldt+1 )
           call stdlib_slaset( 'A', jw, jw, zero, one, v, ldv )
           nmin = stdlib_ilaenv( 12_ilp, 'SLAQR3', 'SV', jw, 1_ilp, jw, lwork )
           if( jw>nmin ) then
              call stdlib_slaqr4( .true., .true., jw, 1_ilp, jw, t, ldt, sr( kwtop ),si( kwtop ), 1_ilp, &
                        jw, v, ldv, work, lwork, infqr )
           else
              call stdlib_slahqr( .true., .true., jw, 1_ilp, jw, t, ldt, sr( kwtop ),si( kwtop ), 1_ilp, &
                        jw, v, ldv, infqr )
           end if
           ! ==== stdlib_strexc needs a clean margin near the diagonal ====
           do j = 1, jw - 3
              t( j+2, j ) = zero
              t( j+3, j ) = zero
           end do
           if( jw>2_ilp )t( jw, jw-2 ) = zero
           ! ==== deflation detection loop ====
           ns = jw
           ilst = infqr + 1_ilp
           20 continue
           if( ilst<=ns ) then
              if( ns==1_ilp ) then
                 bulge = .false.
              else
                 bulge = t( ns, ns-1 )/=zero
              end if
              ! ==== small spike tip test for deflation ====
              if( .not. bulge ) then
                 ! ==== real eigenvalue ====
                 foo = abs( t( ns, ns ) )
                 if( foo==zero )foo = abs( s )
                 if( abs( s*v( 1_ilp, ns ) )<=max( smlnum, ulp*foo ) ) then
                    ! ==== deflatable ====
                    ns = ns - 1_ilp
                 else
                    ! ==== undeflatable.   move it up out of the way.
                    ! .    (stdlib_strexc can not fail in this case.) ====
                    ifst = ns
                    call stdlib_strexc( 'V', jw, t, ldt, v, ldv, ifst, ilst, work,info )
                    ilst = ilst + 1_ilp
                 end if
              else
                 ! ==== complex conjugate pair ====
                 foo = abs( t( ns, ns ) ) + sqrt( abs( t( ns, ns-1 ) ) )*sqrt( abs( t( ns-1, ns ) &
                           ) )
                 if( foo==zero )foo = abs( s )
                 if( max( abs( s*v( 1_ilp, ns ) ), abs( s*v( 1_ilp, ns-1 ) ) )<=max( smlnum, ulp*foo ) ) &
                           then
                    ! ==== deflatable ====
                    ns = ns - 2_ilp
                 else
                    ! ==== undeflatable. move them up out of the way.
                    ! .    fortunately, stdlib_strexc does the right thing with
                    ! .    ilst in case of a rare exchange failure. ====
                    ifst = ns
                    call stdlib_strexc( 'V', jw, t, ldt, v, ldv, ifst, ilst, work,info )
                    ilst = ilst + 2_ilp
                 end if
              end if
              ! ==== end deflation detection loop ====
              go to 20
           end if
              ! ==== return to hessenberg form ====
           if( ns==0_ilp )s = zero
           if( ns<jw ) then
              ! ==== sorting diagonal blocks of t improves accuracy for
              ! .    graded matrices.  bubble sort deals well with
              ! .    exchange failures. ====
              sorted = .false.
              i = ns + 1_ilp
              30 continue
              if( sorted )go to 50
              sorted = .true.
              kend = i - 1_ilp
              i = infqr + 1_ilp
              if( i==ns ) then
                 k = i + 1_ilp
              else if( t( i+1, i )==zero ) then
                 k = i + 1_ilp
              else
                 k = i + 2_ilp
              end if
              40 continue
              if( k<=kend ) then
                 if( k==i+1 ) then
                    evi = abs( t( i, i ) )
                 else
                    evi = abs( t( i, i ) ) + sqrt( abs( t( i+1, i ) ) )*sqrt( abs( t( i, i+1 ) ) )
                              
                 end if
                 if( k==kend ) then
                    evk = abs( t( k, k ) )
                 else if( t( k+1, k )==zero ) then
                    evk = abs( t( k, k ) )
                 else
                    evk = abs( t( k, k ) ) + sqrt( abs( t( k+1, k ) ) )*sqrt( abs( t( k, k+1 ) ) )
                              
                 end if
                 if( evi>=evk ) then
                    i = k
                 else
                    sorted = .false.
                    ifst = i
                    ilst = k
                    call stdlib_strexc( 'V', jw, t, ldt, v, ldv, ifst, ilst, work,info )
                    if( info==0_ilp ) then
                       i = ilst
                    else
                       i = k
                    end if
                 end if
                 if( i==kend ) then
                    k = i + 1_ilp
                 else if( t( i+1, i )==zero ) then
                    k = i + 1_ilp
                 else
                    k = i + 2_ilp
                 end if
                 go to 40
              end if
              go to 30
              50 continue
           end if
           ! ==== restore shift/eigenvalue array from t ====
           i = jw
           60 continue
           if( i>=infqr+1 ) then
              if( i==infqr+1 ) then
                 sr( kwtop+i-1 ) = t( i, i )
                 si( kwtop+i-1 ) = zero
                 i = i - 1_ilp
              else if( t( i, i-1 )==zero ) then
                 sr( kwtop+i-1 ) = t( i, i )
                 si( kwtop+i-1 ) = zero
                 i = i - 1_ilp
              else
                 aa = t( i-1, i-1 )
                 cc = t( i, i-1 )
                 bb = t( i-1, i )
                 dd = t( i, i )
                 call stdlib_slanv2( aa, bb, cc, dd, sr( kwtop+i-2 ),si( kwtop+i-2 ), sr( kwtop+i-&
                           1_ilp ),si( kwtop+i-1 ), cs, sn )
                 i = i - 2_ilp
              end if
              go to 60
           end if
           if( ns<jw .or. s==zero ) then
              if( ns>1_ilp .and. s/=zero ) then
                 ! ==== reflect spike back into lower triangle ====
                 call stdlib_scopy( ns, v, ldv, work, 1_ilp )
                 beta = work( 1_ilp )
                 call stdlib_slarfg( ns, beta, work( 2_ilp ), 1_ilp, tau )
                 work( 1_ilp ) = one
                 call stdlib_slaset( 'L', jw-2, jw-2, zero, zero, t( 3_ilp, 1_ilp ), ldt )
                 call stdlib_slarf( 'L', ns, jw, work, 1_ilp, tau, t, ldt,work( jw+1 ) )
                 call stdlib_slarf( 'R', ns, ns, work, 1_ilp, tau, t, ldt,work( jw+1 ) )
                 call stdlib_slarf( 'R', jw, ns, work, 1_ilp, tau, v, ldv,work( jw+1 ) )
                 call stdlib_sgehrd( jw, 1_ilp, ns, t, ldt, work, work( jw+1 ),lwork-jw, info )
                           
              end if
              ! ==== copy updated reduced window into place ====
              if( kwtop>1_ilp )h( kwtop, kwtop-1 ) = s*v( 1_ilp, 1_ilp )
              call stdlib_slacpy( 'U', jw, jw, t, ldt, h( kwtop, kwtop ), ldh )
              call stdlib_scopy( jw-1, t( 2_ilp, 1_ilp ), ldt+1, h( kwtop+1, kwtop ),ldh+1 )
              ! ==== accumulate orthogonal matrix in order update
              ! .    h and z, if requested.  ====
              if( ns>1_ilp .and. s/=zero )call stdlib_sormhr( 'R', 'N', jw, ns, 1_ilp, ns, t, ldt, work, &
                        v, ldv,work( jw+1 ), lwork-jw, info )
              ! ==== update vertical slab in h ====
              if( wantt ) then
                 ltop = 1_ilp
              else
                 ltop = ktop
              end if
              do krow = ltop, kwtop - 1, nv
                 kln = min( nv, kwtop-krow )
                 call stdlib_sgemm( 'N', 'N', kln, jw, jw, one, h( krow, kwtop ),ldh, v, ldv, &
                           zero, wv, ldwv )
                 call stdlib_slacpy( 'A', kln, jw, wv, ldwv, h( krow, kwtop ), ldh )
              end do
              ! ==== update horizontal slab in h ====
              if( wantt ) then
                 do kcol = kbot + 1, n, nh
                    kln = min( nh, n-kcol+1 )
                    call stdlib_sgemm( 'C', 'N', jw, kln, jw, one, v, ldv,h( kwtop, kcol ), ldh, &
                              zero, t, ldt )
                    call stdlib_slacpy( 'A', jw, kln, t, ldt, h( kwtop, kcol ),ldh )
                 end do
              end if
              ! ==== update vertical slab in z ====
              if( wantz ) then
                 do krow = iloz, ihiz, nv
                    kln = min( nv, ihiz-krow+1 )
                    call stdlib_sgemm( 'N', 'N', kln, jw, jw, one, z( krow, kwtop ),ldz, v, ldv, &
                              zero, wv, ldwv )
                    call stdlib_slacpy( 'A', kln, jw, wv, ldwv, z( krow, kwtop ),ldz )
                 end do
              end if
           end if
           ! ==== return the number of deflations ... ====
           nd = jw - ns
           ! ==== ... and the number of shifts. (subtracting
           ! .    infqr from the spike length takes care
           ! .    of the case of a rare qr failure while
           ! .    calculating eigenvalues of the deflation
           ! .    window.)  ====
           ns = ns - infqr
            ! ==== return optimal workspace. ====
           work( 1_ilp ) = real( lwkopt,KIND=sp)
     end subroutine stdlib_slaqr3

     module subroutine stdlib_dlaqr3( wantt, wantz, n, ktop, kbot, nw, h, ldh, iloz,ihiz, z, ldz, ns, nd,&
     !! Aggressive early deflation:
     !! DLAQR3 accepts as input an upper Hessenberg matrix
     !! H and performs an orthogonal similarity transformation
     !! designed to detect and deflate fully converged eigenvalues from
     !! a trailing principal submatrix.  On output H has been over-
     !! written by a new Hessenberg matrix that is a perturbation of
     !! an orthogonal similarity transformation of H.  It is to be
     !! hoped that the final version of H has many zero subdiagonal
     !! entries.
                sr, si, v, ldv, nh, t,ldt, nv, wv, ldwv, work, lwork )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ihiz, iloz, kbot, ktop, ldh, ldt, ldv, ldwv, ldz, lwork, n,&
                      nh, nv, nw
           integer(ilp), intent(out) :: nd, ns
           logical(lk), intent(in) :: wantt, wantz
           ! Array Arguments 
           real(dp), intent(inout) :: h(ldh,*), z(ldz,*)
           real(dp), intent(out) :: si(*), sr(*), t(ldt,*), v(ldv,*), work(*), wv(ldwv,*)
        ! ================================================================
           
           ! Local Scalars 
           real(dp) :: aa, bb, beta, cc, cs, dd, evi, evk, foo, s, safmax, safmin, smlnum, sn, &
                     tau, ulp
           integer(ilp) :: i, ifst, ilst, info, infqr, j, jw, k, kcol, kend, kln, krow, kwtop, &
                     ltop, lwk1, lwk2, lwk3, lwkopt, nmin
           logical(lk) :: bulge, sorted
           ! Intrinsic Functions 
           ! Executable Statements 
           ! ==== estimate optimal workspace. ====
           jw = min( nw, kbot-ktop+1 )
           if( jw<=2_ilp ) then
              lwkopt = 1_ilp
           else
              ! ==== workspace query call to stdlib_dgehrd ====
              call stdlib_dgehrd( jw, 1_ilp, jw-1, t, ldt, work, work, -1_ilp, info )
              lwk1 = int( work( 1_ilp ),KIND=ilp)
              ! ==== workspace query call to stdlib_dormhr ====
              call stdlib_dormhr( 'R', 'N', jw, jw, 1_ilp, jw-1, t, ldt, work, v, ldv,work, -1_ilp, info )
                        
              lwk2 = int( work( 1_ilp ),KIND=ilp)
              ! ==== workspace query call to stdlib_dlaqr4 ====
              call stdlib_dlaqr4( .true., .true., jw, 1_ilp, jw, t, ldt, sr, si, 1_ilp, jw,v, ldv, work, -&
                        1_ilp, infqr )
              lwk3 = int( work( 1_ilp ),KIND=ilp)
              ! ==== optimal workspace ====
              lwkopt = max( jw+max( lwk1, lwk2 ), lwk3 )
           end if
           ! ==== quick return in case of workspace query. ====
           if( lwork==-1_ilp ) then
              work( 1_ilp ) = real( lwkopt,KIND=dp)
              return
           end if
           ! ==== nothing to do ...
           ! ... for an empty active block ... ====
           ns = 0_ilp
           nd = 0_ilp
           work( 1_ilp ) = one
           if( ktop>kbot )return
           ! ... nor for an empty deflation window. ====
           if( nw<1 )return
           ! ==== machine constants ====
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           safmax = one / safmin
           call stdlib_dlabad( safmin, safmax )
           ulp = stdlib_dlamch( 'PRECISION' )
           smlnum = safmin*( real( n,KIND=dp) / ulp )
           ! ==== setup deflation window ====
           jw = min( nw, kbot-ktop+1 )
           kwtop = kbot - jw + 1_ilp
           if( kwtop==ktop ) then
              s = zero
           else
              s = h( kwtop, kwtop-1 )
           end if
           if( kbot==kwtop ) then
              ! ==== 1-by-1 deflation window: not much to do ====
              sr( kwtop ) = h( kwtop, kwtop )
              si( kwtop ) = zero
              ns = 1_ilp
              nd = 0_ilp
              if( abs( s )<=max( smlnum, ulp*abs( h( kwtop, kwtop ) ) ) )then
                 ns = 0_ilp
                 nd = 1_ilp
                 if( kwtop>ktop )h( kwtop, kwtop-1 ) = zero
              end if
              work( 1_ilp ) = one
              return
           end if
           ! ==== convert to spike-triangular form.  (in case of a
           ! .    rare qr failure, this routine continues to do
           ! .    aggressive early deflation using that part of
           ! .    the deflation window that converged using infqr
           ! .    here and there to keep track.) ====
           call stdlib_dlacpy( 'U', jw, jw, h( kwtop, kwtop ), ldh, t, ldt )
           call stdlib_dcopy( jw-1, h( kwtop+1, kwtop ), ldh+1, t( 2_ilp, 1_ilp ), ldt+1 )
           call stdlib_dlaset( 'A', jw, jw, zero, one, v, ldv )
           nmin = stdlib_ilaenv( 12_ilp, 'DLAQR3', 'SV', jw, 1_ilp, jw, lwork )
           if( jw>nmin ) then
              call stdlib_dlaqr4( .true., .true., jw, 1_ilp, jw, t, ldt, sr( kwtop ),si( kwtop ), 1_ilp, &
                        jw, v, ldv, work, lwork, infqr )
           else
              call stdlib_dlahqr( .true., .true., jw, 1_ilp, jw, t, ldt, sr( kwtop ),si( kwtop ), 1_ilp, &
                        jw, v, ldv, infqr )
           end if
           ! ==== stdlib_dtrexc needs a clean margin near the diagonal ====
           do j = 1, jw - 3
              t( j+2, j ) = zero
              t( j+3, j ) = zero
           end do
           if( jw>2_ilp )t( jw, jw-2 ) = zero
           ! ==== deflation detection loop ====
           ns = jw
           ilst = infqr + 1_ilp
           20 continue
           if( ilst<=ns ) then
              if( ns==1_ilp ) then
                 bulge = .false.
              else
                 bulge = t( ns, ns-1 )/=zero
              end if
              ! ==== small spike tip test for deflation ====
              if( .not. bulge ) then
                 ! ==== real eigenvalue ====
                 foo = abs( t( ns, ns ) )
                 if( foo==zero )foo = abs( s )
                 if( abs( s*v( 1_ilp, ns ) )<=max( smlnum, ulp*foo ) ) then
                    ! ==== deflatable ====
                    ns = ns - 1_ilp
                 else
                    ! ==== undeflatable.   move it up out of the way.
                    ! .    (stdlib_dtrexc can not fail in this case.) ====
                    ifst = ns
                    call stdlib_dtrexc( 'V', jw, t, ldt, v, ldv, ifst, ilst, work,info )
                    ilst = ilst + 1_ilp
                 end if
              else
                 ! ==== complex conjugate pair ====
                 foo = abs( t( ns, ns ) ) + sqrt( abs( t( ns, ns-1 ) ) )*sqrt( abs( t( ns-1, ns ) &
                           ) )
                 if( foo==zero )foo = abs( s )
                 if( max( abs( s*v( 1_ilp, ns ) ), abs( s*v( 1_ilp, ns-1 ) ) )<=max( smlnum, ulp*foo ) ) &
                           then
                    ! ==== deflatable ====
                    ns = ns - 2_ilp
                 else
                    ! ==== undeflatable. move them up out of the way.
                    ! .    fortunately, stdlib_dtrexc does the right thing with
                    ! .    ilst in case of a rare exchange failure. ====
                    ifst = ns
                    call stdlib_dtrexc( 'V', jw, t, ldt, v, ldv, ifst, ilst, work,info )
                    ilst = ilst + 2_ilp
                 end if
              end if
              ! ==== end deflation detection loop ====
              go to 20
           end if
              ! ==== return to hessenberg form ====
           if( ns==0_ilp )s = zero
           if( ns<jw ) then
              ! ==== sorting diagonal blocks of t improves accuracy for
              ! .    graded matrices.  bubble sort deals well with
              ! .    exchange failures. ====
              sorted = .false.
              i = ns + 1_ilp
              30 continue
              if( sorted )go to 50
              sorted = .true.
              kend = i - 1_ilp
              i = infqr + 1_ilp
              if( i==ns ) then
                 k = i + 1_ilp
              else if( t( i+1, i )==zero ) then
                 k = i + 1_ilp
              else
                 k = i + 2_ilp
              end if
              40 continue
              if( k<=kend ) then
                 if( k==i+1 ) then
                    evi = abs( t( i, i ) )
                 else
                    evi = abs( t( i, i ) ) + sqrt( abs( t( i+1, i ) ) )*sqrt( abs( t( i, i+1 ) ) )
                              
                 end if
                 if( k==kend ) then
                    evk = abs( t( k, k ) )
                 else if( t( k+1, k )==zero ) then
                    evk = abs( t( k, k ) )
                 else
                    evk = abs( t( k, k ) ) + sqrt( abs( t( k+1, k ) ) )*sqrt( abs( t( k, k+1 ) ) )
                              
                 end if
                 if( evi>=evk ) then
                    i = k
                 else
                    sorted = .false.
                    ifst = i
                    ilst = k
                    call stdlib_dtrexc( 'V', jw, t, ldt, v, ldv, ifst, ilst, work,info )
                    if( info==0_ilp ) then
                       i = ilst
                    else
                       i = k
                    end if
                 end if
                 if( i==kend ) then
                    k = i + 1_ilp
                 else if( t( i+1, i )==zero ) then
                    k = i + 1_ilp
                 else
                    k = i + 2_ilp
                 end if
                 go to 40
              end if
              go to 30
              50 continue
           end if
           ! ==== restore shift/eigenvalue array from t ====
           i = jw
           60 continue
           if( i>=infqr+1 ) then
              if( i==infqr+1 ) then
                 sr( kwtop+i-1 ) = t( i, i )
                 si( kwtop+i-1 ) = zero
                 i = i - 1_ilp
              else if( t( i, i-1 )==zero ) then
                 sr( kwtop+i-1 ) = t( i, i )
                 si( kwtop+i-1 ) = zero
                 i = i - 1_ilp
              else
                 aa = t( i-1, i-1 )
                 cc = t( i, i-1 )
                 bb = t( i-1, i )
                 dd = t( i, i )
                 call stdlib_dlanv2( aa, bb, cc, dd, sr( kwtop+i-2 ),si( kwtop+i-2 ), sr( kwtop+i-&
                           1_ilp ),si( kwtop+i-1 ), cs, sn )
                 i = i - 2_ilp
              end if
              go to 60
           end if
           if( ns<jw .or. s==zero ) then
              if( ns>1_ilp .and. s/=zero ) then
                 ! ==== reflect spike back into lower triangle ====
                 call stdlib_dcopy( ns, v, ldv, work, 1_ilp )
                 beta = work( 1_ilp )
                 call stdlib_dlarfg( ns, beta, work( 2_ilp ), 1_ilp, tau )
                 work( 1_ilp ) = one
                 call stdlib_dlaset( 'L', jw-2, jw-2, zero, zero, t( 3_ilp, 1_ilp ), ldt )
                 call stdlib_dlarf( 'L', ns, jw, work, 1_ilp, tau, t, ldt,work( jw+1 ) )
                 call stdlib_dlarf( 'R', ns, ns, work, 1_ilp, tau, t, ldt,work( jw+1 ) )
                 call stdlib_dlarf( 'R', jw, ns, work, 1_ilp, tau, v, ldv,work( jw+1 ) )
                 call stdlib_dgehrd( jw, 1_ilp, ns, t, ldt, work, work( jw+1 ),lwork-jw, info )
                           
              end if
              ! ==== copy updated reduced window into place ====
              if( kwtop>1_ilp )h( kwtop, kwtop-1 ) = s*v( 1_ilp, 1_ilp )
              call stdlib_dlacpy( 'U', jw, jw, t, ldt, h( kwtop, kwtop ), ldh )
              call stdlib_dcopy( jw-1, t( 2_ilp, 1_ilp ), ldt+1, h( kwtop+1, kwtop ),ldh+1 )
              ! ==== accumulate orthogonal matrix in order update
              ! .    h and z, if requested.  ====
              if( ns>1_ilp .and. s/=zero )call stdlib_dormhr( 'R', 'N', jw, ns, 1_ilp, ns, t, ldt, work, &
                        v, ldv,work( jw+1 ), lwork-jw, info )
              ! ==== update vertical slab in h ====
              if( wantt ) then
                 ltop = 1_ilp
              else
                 ltop = ktop
              end if
              do krow = ltop, kwtop - 1, nv
                 kln = min( nv, kwtop-krow )
                 call stdlib_dgemm( 'N', 'N', kln, jw, jw, one, h( krow, kwtop ),ldh, v, ldv, &
                           zero, wv, ldwv )
                 call stdlib_dlacpy( 'A', kln, jw, wv, ldwv, h( krow, kwtop ), ldh )
              end do
              ! ==== update horizontal slab in h ====
              if( wantt ) then
                 do kcol = kbot + 1, n, nh
                    kln = min( nh, n-kcol+1 )
                    call stdlib_dgemm( 'C', 'N', jw, kln, jw, one, v, ldv,h( kwtop, kcol ), ldh, &
                              zero, t, ldt )
                    call stdlib_dlacpy( 'A', jw, kln, t, ldt, h( kwtop, kcol ),ldh )
                 end do
              end if
              ! ==== update vertical slab in z ====
              if( wantz ) then
                 do krow = iloz, ihiz, nv
                    kln = min( nv, ihiz-krow+1 )
                    call stdlib_dgemm( 'N', 'N', kln, jw, jw, one, z( krow, kwtop ),ldz, v, ldv, &
                              zero, wv, ldwv )
                    call stdlib_dlacpy( 'A', kln, jw, wv, ldwv, z( krow, kwtop ),ldz )
                 end do
              end if
           end if
           ! ==== return the number of deflations ... ====
           nd = jw - ns
           ! ==== ... and the number of shifts. (subtracting
           ! .    infqr from the spike length takes care
           ! .    of the case of a rare qr failure while
           ! .    calculating eigenvalues of the deflation
           ! .    window.)  ====
           ns = ns - infqr
            ! ==== return optimal workspace. ====
           work( 1_ilp ) = real( lwkopt,KIND=dp)
     end subroutine stdlib_dlaqr3


     pure module subroutine stdlib_claqr3( wantt, wantz, n, ktop, kbot, nw, h, ldh, iloz,ihiz, z, ldz, &
     !! Aggressive early deflation:
     !! CLAQR3 accepts as input an upper Hessenberg matrix
     !! H and performs an unitary similarity transformation
     !! designed to detect and deflate fully converged eigenvalues from
     !! a trailing principal submatrix.  On output H has been over-
     !! written by a new Hessenberg matrix that is a perturbation of
     !! an unitary similarity transformation of H.  It is to be
     !! hoped that the final version of H has many zero subdiagonal
     !! entries.
               ns, nd, sh, v, ldv, nh, t, ldt,nv, wv, ldwv, work, lwork )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ihiz, iloz, kbot, ktop, ldh, ldt, ldv, ldwv, ldz, lwork, n,&
                      nh, nv, nw
           integer(ilp), intent(out) :: nd, ns
           logical(lk), intent(in) :: wantt, wantz
           ! Array Arguments 
           complex(sp), intent(inout) :: h(ldh,*), z(ldz,*)
           complex(sp), intent(out) :: sh(*), t(ldt,*), v(ldv,*), work(*), wv(ldwv,*)
        ! ================================================================
           ! Parameters 
           
           ! Local Scalars 
           complex(sp) :: beta, cdum, s, tau
           real(sp) :: foo, safmax, safmin, smlnum, ulp
           integer(ilp) :: i, ifst, ilst, info, infqr, j, jw, kcol, kln, knt, krow, kwtop, ltop, &
                     lwk1, lwk2, lwk3, lwkopt, nmin
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( cdum ) = abs( real( cdum,KIND=sp) ) + abs( aimag( cdum ) )
           ! Executable Statements 
           ! ==== estimate optimal workspace. ====
           jw = min( nw, kbot-ktop+1 )
           if( jw<=2_ilp ) then
              lwkopt = 1_ilp
           else
              ! ==== workspace query call to stdlib_cgehrd ====
              call stdlib_cgehrd( jw, 1_ilp, jw-1, t, ldt, work, work, -1_ilp, info )
              lwk1 = int( work( 1_ilp ),KIND=ilp)
              ! ==== workspace query call to stdlib_cunmhr ====
              call stdlib_cunmhr( 'R', 'N', jw, jw, 1_ilp, jw-1, t, ldt, work, v, ldv,work, -1_ilp, info )
                        
              lwk2 = int( work( 1_ilp ),KIND=ilp)
              ! ==== workspace query call to stdlib_claqr4 ====
              call stdlib_claqr4( .true., .true., jw, 1_ilp, jw, t, ldt, sh, 1_ilp, jw, v,ldv, work, -1_ilp, &
                        infqr )
              lwk3 = int( work( 1_ilp ),KIND=ilp)
              ! ==== optimal workspace ====
              lwkopt = max( jw+max( lwk1, lwk2 ), lwk3 )
           end if
           ! ==== quick return in case of workspace query. ====
           if( lwork==-1_ilp ) then
              work( 1_ilp ) = cmplx( lwkopt, 0_ilp,KIND=sp)
              return
           end if
           ! ==== nothing to do ...
           ! ... for an empty active block ... ====
           ns = 0_ilp
           nd = 0_ilp
           work( 1_ilp ) = cone
           if( ktop>kbot )return
           ! ... nor for an empty deflation window. ====
           if( nw<1 )return
           ! ==== machine constants ====
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           safmax = one / safmin
           call stdlib_slabad( safmin, safmax )
           ulp = stdlib_slamch( 'PRECISION' )
           smlnum = safmin*( real( n,KIND=sp) / ulp )
           ! ==== setup deflation window ====
           jw = min( nw, kbot-ktop+1 )
           kwtop = kbot - jw + 1_ilp
           if( kwtop==ktop ) then
              s = czero
           else
              s = h( kwtop, kwtop-1 )
           end if
           if( kbot==kwtop ) then
              ! ==== 1-by-1 deflation window: not much to do ====
              sh( kwtop ) = h( kwtop, kwtop )
              ns = 1_ilp
              nd = 0_ilp
              if( cabs1( s )<=max( smlnum, ulp*cabs1( h( kwtop,kwtop ) ) ) ) then
                 ns = 0_ilp
                 nd = 1_ilp
                 if( kwtop>ktop )h( kwtop, kwtop-1 ) = czero
              end if
              work( 1_ilp ) = cone
              return
           end if
           ! ==== convert to spike-triangular form.  (in case of a
           ! .    rare qr failure, this routine continues to do
           ! .    aggressive early deflation using that part of
           ! .    the deflation window that converged using infqr
           ! .    here and there to keep track.) ====
           call stdlib_clacpy( 'U', jw, jw, h( kwtop, kwtop ), ldh, t, ldt )
           call stdlib_ccopy( jw-1, h( kwtop+1, kwtop ), ldh+1, t( 2_ilp, 1_ilp ), ldt+1 )
           call stdlib_claset( 'A', jw, jw, czero, cone, v, ldv )
           nmin = stdlib_ilaenv( 12_ilp, 'CLAQR3', 'SV', jw, 1_ilp, jw, lwork )
           if( jw>nmin ) then
              call stdlib_claqr4( .true., .true., jw, 1_ilp, jw, t, ldt, sh( kwtop ), 1_ilp,jw, v, ldv, &
                        work, lwork, infqr )
           else
              call stdlib_clahqr( .true., .true., jw, 1_ilp, jw, t, ldt, sh( kwtop ), 1_ilp,jw, v, ldv, &
                        infqr )
           end if
           ! ==== deflation detection loop ====
           ns = jw
           ilst = infqr + 1_ilp
           do knt = infqr + 1, jw
              ! ==== small spike tip deflation test ====
              foo = cabs1( t( ns, ns ) )
              if( foo==zero )foo = cabs1( s )
              if( cabs1( s )*cabs1( v( 1_ilp, ns ) )<=max( smlnum, ulp*foo ) )then
                 ! ==== cone more converged eigenvalue ====
                 ns = ns - 1_ilp
              else
                 ! ==== cone undeflatable eigenvalue.  move it up out of the
                 ! .    way.   (stdlib_ctrexc can not fail in this case.) ====
                 ifst = ns
                 call stdlib_ctrexc( 'V', jw, t, ldt, v, ldv, ifst, ilst, info )
                 ilst = ilst + 1_ilp
              end if
           end do
              ! ==== return to hessenberg form ====
           if( ns==0_ilp )s = czero
           if( ns<jw ) then
              ! ==== sorting the diagonal of t improves accuracy for
              ! .    graded matrices.  ====
              do i = infqr + 1, ns
                 ifst = i
                 do j = i + 1, ns
                    if( cabs1( t( j, j ) )>cabs1( t( ifst, ifst ) ) )ifst = j
                 end do
                 ilst = i
                 if( ifst/=ilst )call stdlib_ctrexc( 'V', jw, t, ldt, v, ldv, ifst, ilst, info )
                           
              end do
           end if
           ! ==== restore shift/eigenvalue array from t ====
           do i = infqr + 1, jw
              sh( kwtop+i-1 ) = t( i, i )
           end do
           if( ns<jw .or. s==czero ) then
              if( ns>1_ilp .and. s/=czero ) then
                 ! ==== reflect spike back into lower triangle ====
                 call stdlib_ccopy( ns, v, ldv, work, 1_ilp )
                 do i = 1, ns
                    work( i ) = conjg( work( i ) )
                 end do
                 beta = work( 1_ilp )
                 call stdlib_clarfg( ns, beta, work( 2_ilp ), 1_ilp, tau )
                 work( 1_ilp ) = cone
                 call stdlib_claset( 'L', jw-2, jw-2, czero, czero, t( 3_ilp, 1_ilp ), ldt )
                 call stdlib_clarf( 'L', ns, jw, work, 1_ilp, conjg( tau ), t, ldt,work( jw+1 ) )
                           
                 call stdlib_clarf( 'R', ns, ns, work, 1_ilp, tau, t, ldt,work( jw+1 ) )
                 call stdlib_clarf( 'R', jw, ns, work, 1_ilp, tau, v, ldv,work( jw+1 ) )
                 call stdlib_cgehrd( jw, 1_ilp, ns, t, ldt, work, work( jw+1 ),lwork-jw, info )
                           
              end if
              ! ==== copy updated reduced window into place ====
              if( kwtop>1_ilp )h( kwtop, kwtop-1 ) = s*conjg( v( 1_ilp, 1_ilp ) )
              call stdlib_clacpy( 'U', jw, jw, t, ldt, h( kwtop, kwtop ), ldh )
              call stdlib_ccopy( jw-1, t( 2_ilp, 1_ilp ), ldt+1, h( kwtop+1, kwtop ),ldh+1 )
              ! ==== accumulate orthogonal matrix in order update
              ! .    h and z, if requested.  ====
              if( ns>1_ilp .and. s/=czero )call stdlib_cunmhr( 'R', 'N', jw, ns, 1_ilp, ns, t, ldt, work, &
                        v, ldv,work( jw+1 ), lwork-jw, info )
              ! ==== update vertical slab in h ====
              if( wantt ) then
                 ltop = 1_ilp
              else
                 ltop = ktop
              end if
              do krow = ltop, kwtop - 1, nv
                 kln = min( nv, kwtop-krow )
                 call stdlib_cgemm( 'N', 'N', kln, jw, jw, cone, h( krow, kwtop ),ldh, v, ldv, &
                           czero, wv, ldwv )
                 call stdlib_clacpy( 'A', kln, jw, wv, ldwv, h( krow, kwtop ), ldh )
              end do
              ! ==== update horizontal slab in h ====
              if( wantt ) then
                 do kcol = kbot + 1, n, nh
                    kln = min( nh, n-kcol+1 )
                    call stdlib_cgemm( 'C', 'N', jw, kln, jw, cone, v, ldv,h( kwtop, kcol ), ldh, &
                              czero, t, ldt )
                    call stdlib_clacpy( 'A', jw, kln, t, ldt, h( kwtop, kcol ),ldh )
                 end do
              end if
              ! ==== update vertical slab in z ====
              if( wantz ) then
                 do krow = iloz, ihiz, nv
                    kln = min( nv, ihiz-krow+1 )
                    call stdlib_cgemm( 'N', 'N', kln, jw, jw, cone, z( krow, kwtop ),ldz, v, ldv, &
                              czero, wv, ldwv )
                    call stdlib_clacpy( 'A', kln, jw, wv, ldwv, z( krow, kwtop ),ldz )
                 end do
              end if
           end if
           ! ==== return the number of deflations ... ====
           nd = jw - ns
           ! ==== ... and the number of shifts. (subtracting
           ! .    infqr from the spike length takes care
           ! .    of the case of a rare qr failure while
           ! .    calculating eigenvalues of the deflation
           ! .    window.)  ====
           ns = ns - infqr
            ! ==== return optimal workspace. ====
           work( 1_ilp ) = cmplx( lwkopt, 0_ilp,KIND=sp)
     end subroutine stdlib_claqr3

     pure module subroutine stdlib_zlaqr3( wantt, wantz, n, ktop, kbot, nw, h, ldh, iloz,ihiz, z, ldz, &
     !! Aggressive early deflation:
     !! ZLAQR3 accepts as input an upper Hessenberg matrix
     !! H and performs an unitary similarity transformation
     !! designed to detect and deflate fully converged eigenvalues from
     !! a trailing principal submatrix.  On output H has been over-
     !! written by a new Hessenberg matrix that is a perturbation of
     !! an unitary similarity transformation of H.  It is to be
     !! hoped that the final version of H has many zero subdiagonal
     !! entries.
               ns, nd, sh, v, ldv, nh, t, ldt,nv, wv, ldwv, work, lwork )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ihiz, iloz, kbot, ktop, ldh, ldt, ldv, ldwv, ldz, lwork, n,&
                      nh, nv, nw
           integer(ilp), intent(out) :: nd, ns
           logical(lk), intent(in) :: wantt, wantz
           ! Array Arguments 
           complex(dp), intent(inout) :: h(ldh,*), z(ldz,*)
           complex(dp), intent(out) :: sh(*), t(ldt,*), v(ldv,*), work(*), wv(ldwv,*)
        ! ================================================================
           ! Parameters 
           
           ! Local Scalars 
           complex(dp) :: beta, cdum, s, tau
           real(dp) :: foo, safmax, safmin, smlnum, ulp
           integer(ilp) :: i, ifst, ilst, info, infqr, j, jw, kcol, kln, knt, krow, kwtop, ltop, &
                     lwk1, lwk2, lwk3, lwkopt, nmin
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( cdum ) = abs( real( cdum,KIND=dp) ) + abs( aimag( cdum ) )
           ! Executable Statements 
           ! ==== estimate optimal workspace. ====
           jw = min( nw, kbot-ktop+1 )
           if( jw<=2_ilp ) then
              lwkopt = 1_ilp
           else
              ! ==== workspace query call to stdlib_zgehrd ====
              call stdlib_zgehrd( jw, 1_ilp, jw-1, t, ldt, work, work, -1_ilp, info )
              lwk1 = int( work( 1_ilp ),KIND=ilp)
              ! ==== workspace query call to stdlib_zunmhr ====
              call stdlib_zunmhr( 'R', 'N', jw, jw, 1_ilp, jw-1, t, ldt, work, v, ldv,work, -1_ilp, info )
                        
              lwk2 = int( work( 1_ilp ),KIND=ilp)
              ! ==== workspace query call to stdlib_zlaqr4 ====
              call stdlib_zlaqr4( .true., .true., jw, 1_ilp, jw, t, ldt, sh, 1_ilp, jw, v,ldv, work, -1_ilp, &
                        infqr )
              lwk3 = int( work( 1_ilp ),KIND=ilp)
              ! ==== optimal workspace ====
              lwkopt = max( jw+max( lwk1, lwk2 ), lwk3 )
           end if
           ! ==== quick return in case of workspace query. ====
           if( lwork==-1_ilp ) then
              work( 1_ilp ) = cmplx( lwkopt, 0_ilp,KIND=dp)
              return
           end if
           ! ==== nothing to do ...
           ! ... for an empty active block ... ====
           ns = 0_ilp
           nd = 0_ilp
           work( 1_ilp ) = cone
           if( ktop>kbot )return
           ! ... nor for an empty deflation window. ====
           if( nw<1 )return
           ! ==== machine constants ====
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           safmax = one / safmin
           call stdlib_dlabad( safmin, safmax )
           ulp = stdlib_dlamch( 'PRECISION' )
           smlnum = safmin*( real( n,KIND=dp) / ulp )
           ! ==== setup deflation window ====
           jw = min( nw, kbot-ktop+1 )
           kwtop = kbot - jw + 1_ilp
           if( kwtop==ktop ) then
              s = czero
           else
              s = h( kwtop, kwtop-1 )
           end if
           if( kbot==kwtop ) then
              ! ==== 1-by-1 deflation window: not much to do ====
              sh( kwtop ) = h( kwtop, kwtop )
              ns = 1_ilp
              nd = 0_ilp
              if( cabs1( s )<=max( smlnum, ulp*cabs1( h( kwtop,kwtop ) ) ) ) then
                 ns = 0_ilp
                 nd = 1_ilp
                 if( kwtop>ktop )h( kwtop, kwtop-1 ) = czero
              end if
              work( 1_ilp ) = cone
              return
           end if
           ! ==== convert to spike-triangular form.  (in case of a
           ! .    rare qr failure, this routine continues to do
           ! .    aggressive early deflation using that part of
           ! .    the deflation window that converged using infqr
           ! .    here and there to keep track.) ====
           call stdlib_zlacpy( 'U', jw, jw, h( kwtop, kwtop ), ldh, t, ldt )
           call stdlib_zcopy( jw-1, h( kwtop+1, kwtop ), ldh+1, t( 2_ilp, 1_ilp ), ldt+1 )
           call stdlib_zlaset( 'A', jw, jw, czero, cone, v, ldv )
           nmin = stdlib_ilaenv( 12_ilp, 'ZLAQR3', 'SV', jw, 1_ilp, jw, lwork )
           if( jw>nmin ) then
              call stdlib_zlaqr4( .true., .true., jw, 1_ilp, jw, t, ldt, sh( kwtop ), 1_ilp,jw, v, ldv, &
                        work, lwork, infqr )
           else
              call stdlib_zlahqr( .true., .true., jw, 1_ilp, jw, t, ldt, sh( kwtop ), 1_ilp,jw, v, ldv, &
                        infqr )
           end if
           ! ==== deflation detection loop ====
           ns = jw
           ilst = infqr + 1_ilp
           do knt = infqr + 1, jw
              ! ==== small spike tip deflation test ====
              foo = cabs1( t( ns, ns ) )
              if( foo==zero )foo = cabs1( s )
              if( cabs1( s )*cabs1( v( 1_ilp, ns ) )<=max( smlnum, ulp*foo ) )then
                 ! ==== cone more converged eigenvalue ====
                 ns = ns - 1_ilp
              else
                 ! ==== cone undeflatable eigenvalue.  move it up out of the
                 ! .    way.   (stdlib_ztrexc can not fail in this case.) ====
                 ifst = ns
                 call stdlib_ztrexc( 'V', jw, t, ldt, v, ldv, ifst, ilst, info )
                 ilst = ilst + 1_ilp
              end if
           end do
              ! ==== return to hessenberg form ====
           if( ns==0_ilp )s = czero
           if( ns<jw ) then
              ! ==== sorting the diagonal of t improves accuracy for
              ! .    graded matrices.  ====
              do i = infqr + 1, ns
                 ifst = i
                 do j = i + 1, ns
                    if( cabs1( t( j, j ) )>cabs1( t( ifst, ifst ) ) )ifst = j
                 end do
                 ilst = i
                 if( ifst/=ilst )call stdlib_ztrexc( 'V', jw, t, ldt, v, ldv, ifst, ilst, info )
                           
              end do
           end if
           ! ==== restore shift/eigenvalue array from t ====
           do i = infqr + 1, jw
              sh( kwtop+i-1 ) = t( i, i )
           end do
           if( ns<jw .or. s==czero ) then
              if( ns>1_ilp .and. s/=czero ) then
                 ! ==== reflect spike back into lower triangle ====
                 call stdlib_zcopy( ns, v, ldv, work, 1_ilp )
                 do i = 1, ns
                    work( i ) = conjg( work( i ) )
                 end do
                 beta = work( 1_ilp )
                 call stdlib_zlarfg( ns, beta, work( 2_ilp ), 1_ilp, tau )
                 work( 1_ilp ) = cone
                 call stdlib_zlaset( 'L', jw-2, jw-2, czero, czero, t( 3_ilp, 1_ilp ), ldt )
                 call stdlib_zlarf( 'L', ns, jw, work, 1_ilp, conjg( tau ), t, ldt,work( jw+1 ) )
                           
                 call stdlib_zlarf( 'R', ns, ns, work, 1_ilp, tau, t, ldt,work( jw+1 ) )
                 call stdlib_zlarf( 'R', jw, ns, work, 1_ilp, tau, v, ldv,work( jw+1 ) )
                 call stdlib_zgehrd( jw, 1_ilp, ns, t, ldt, work, work( jw+1 ),lwork-jw, info )
                           
              end if
              ! ==== copy updated reduced window into place ====
              if( kwtop>1_ilp )h( kwtop, kwtop-1 ) = s*conjg( v( 1_ilp, 1_ilp ) )
              call stdlib_zlacpy( 'U', jw, jw, t, ldt, h( kwtop, kwtop ), ldh )
              call stdlib_zcopy( jw-1, t( 2_ilp, 1_ilp ), ldt+1, h( kwtop+1, kwtop ),ldh+1 )
              ! ==== accumulate orthogonal matrix in order update
              ! .    h and z, if requested.  ====
              if( ns>1_ilp .and. s/=czero )call stdlib_zunmhr( 'R', 'N', jw, ns, 1_ilp, ns, t, ldt, work, &
                        v, ldv,work( jw+1 ), lwork-jw, info )
              ! ==== update vertical slab in h ====
              if( wantt ) then
                 ltop = 1_ilp
              else
                 ltop = ktop
              end if
              do krow = ltop, kwtop - 1, nv
                 kln = min( nv, kwtop-krow )
                 call stdlib_zgemm( 'N', 'N', kln, jw, jw, cone, h( krow, kwtop ),ldh, v, ldv, &
                           czero, wv, ldwv )
                 call stdlib_zlacpy( 'A', kln, jw, wv, ldwv, h( krow, kwtop ), ldh )
              end do
              ! ==== update horizontal slab in h ====
              if( wantt ) then
                 do kcol = kbot + 1, n, nh
                    kln = min( nh, n-kcol+1 )
                    call stdlib_zgemm( 'C', 'N', jw, kln, jw, cone, v, ldv,h( kwtop, kcol ), ldh, &
                              czero, t, ldt )
                    call stdlib_zlacpy( 'A', jw, kln, t, ldt, h( kwtop, kcol ),ldh )
                 end do
              end if
              ! ==== update vertical slab in z ====
              if( wantz ) then
                 do krow = iloz, ihiz, nv
                    kln = min( nv, ihiz-krow+1 )
                    call stdlib_zgemm( 'N', 'N', kln, jw, jw, cone, z( krow, kwtop ),ldz, v, ldv, &
                              czero, wv, ldwv )
                    call stdlib_zlacpy( 'A', kln, jw, wv, ldwv, z( krow, kwtop ),ldz )
                 end do
              end if
           end if
           ! ==== return the number of deflations ... ====
           nd = jw - ns
           ! ==== ... and the number of shifts. (subtracting
           ! .    infqr from the spike length takes care
           ! .    of the case of a rare qr failure while
           ! .    calculating eigenvalues of the deflation
           ! .    window.)  ====
           ns = ns - infqr
            ! ==== return optimal workspace. ====
           work( 1_ilp ) = cmplx( lwkopt, 0_ilp,KIND=dp)
     end subroutine stdlib_zlaqr3




     module subroutine stdlib_slaqr4( wantt, wantz, n, ilo, ihi, h, ldh, wr, wi,iloz, ihiz, z, ldz, work,&
     !! SLAQR4 implements one level of recursion for SLAQR0.
     !! It is a complete implementation of the small bulge multi-shift
     !! QR algorithm.  It may be called by SLAQR0 and, for large enough
     !! deflation window size, it may be called by SLAQR3.  This
     !! subroutine is identical to SLAQR0 except that it calls SLAQR2
     !! instead of SLAQR3.
     !! SLAQR4 computes the eigenvalues of a Hessenberg matrix H
     !! and, optionally, the matrices T and Z from the Schur decomposition
     !! H = Z T Z**T, where T is an upper quasi-triangular matrix (the
     !! Schur form), and Z is the orthogonal matrix of Schur vectors.
     !! Optionally Z may be postmultiplied into an input orthogonal
     !! matrix Q so that this routine can give the Schur factorization
     !! of a matrix A which has been reduced to the Hessenberg form H
     !! by the orthogonal matrix Q:  A = Q*H*Q**T = (QZ)*T*(QZ)**T.
                lwork, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ihi, ihiz, ilo, iloz, ldh, ldz, lwork, n
           integer(ilp), intent(out) :: info
           logical(lk), intent(in) :: wantt, wantz
           ! Array Arguments 
           real(sp), intent(inout) :: h(ldh,*), z(ldz,*)
           real(sp), intent(out) :: wi(*), work(*), wr(*)
        ! ================================================================
           ! Parameters 
           integer(ilp), parameter :: ntiny = 15_ilp
           integer(ilp), parameter :: kexnw = 5_ilp
           integer(ilp), parameter :: kexsh = 6_ilp
           real(sp), parameter :: wilk1 = 0.75_sp
           real(sp), parameter :: wilk2 = -0.4375_sp
           ! ==== matrices of order ntiny or smaller must be processed by
           ! .    stdlib_slahqr because of insufficient subdiagonal scratch space.
           ! .    (this is a hard limit.) ====
           
           ! ==== exceptional deflation windows:  try to cure rare
           ! .    slow convergence by varying the size of the
           ! .    deflation window after kexnw iterations. ====
           
           ! ==== exceptional shifts: try to cure rare slow convergence
           ! .    with ad-hoc exceptional shifts every kexsh iterations.
           ! .    ====
           
           ! ==== the constants wilk1 and wilk2 are used to form the
           ! .    exceptional shifts. ====
           
           
           ! Local Scalars 
           real(sp) :: aa, bb, cc, cs, dd, sn, ss, swap
           integer(ilp) :: i, inf, it, itmax, k, kacc22, kbot, kdu, ks, kt, ktop, ku, kv, kwh, &
           kwtop, kwv, ld, ls, lwkopt, ndec, ndfl, nh, nho, nibble, nmin, ns, nsmax, nsr, nve, nw,&
                      nwmax, nwr, nwupbd
           logical(lk) :: sorted
           character(len=2_ilp) :: jbcmpz
           ! Local Arrays 
           real(sp) :: zdum(1_ilp,1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           ! ==== quick return for n = 0: nothing to do. ====
           if( n==0_ilp ) then
              work( 1_ilp ) = one
              return
           end if
           if( n<=ntiny ) then
              ! ==== tiny matrices must use stdlib_slahqr. ====
              lwkopt = 1_ilp
              if( lwork/=-1_ilp )call stdlib_slahqr( wantt, wantz, n, ilo, ihi, h, ldh, wr, wi,iloz, &
                        ihiz, z, ldz, info )
           else
              ! ==== use small bulge multi-shift qr with aggressive early
              ! .    deflation on larger-than-tiny matrices. ====
              ! ==== hope for the best. ====
              info = 0_ilp
              ! ==== set up job flags for stdlib_ilaenv. ====
              if( wantt ) then
                 jbcmpz( 1_ilp: 1_ilp ) = 'S'
              else
                 jbcmpz( 1_ilp: 1_ilp ) = 'E'
              end if
              if( wantz ) then
                 jbcmpz( 2_ilp: 2_ilp ) = 'V'
              else
                 jbcmpz( 2_ilp: 2_ilp ) = 'N'
              end if
              ! ==== nwr = recommended deflation window size.  at this
              ! .    point,  n > ntiny = 15, so there is enough
              ! .    subdiagonal workspace for nwr>=2 as required.
              ! .    (in fact, there is enough subdiagonal space for
              ! .    nwr>=4.) ====
              nwr = stdlib_ilaenv( 13_ilp, 'SLAQR4', jbcmpz, n, ilo, ihi, lwork )
              nwr = max( 2_ilp, nwr )
              nwr = min( ihi-ilo+1, ( n-1 ) / 3_ilp, nwr )
              ! ==== nsr = recommended number of simultaneous shifts.
              ! .    at this point n > ntiny = 15, so there is at
              ! .    enough subdiagonal workspace for nsr to be even
              ! .    and greater than or equal to two as required. ====
              nsr = stdlib_ilaenv( 15_ilp, 'SLAQR4', jbcmpz, n, ilo, ihi, lwork )
              nsr = min( nsr, ( n-3 ) / 6_ilp, ihi-ilo )
              nsr = max( 2_ilp, nsr-mod( nsr, 2_ilp ) )
              ! ==== estimate optimal workspace ====
              ! ==== workspace query call to stdlib_slaqr2 ====
              call stdlib_slaqr2( wantt, wantz, n, ilo, ihi, nwr+1, h, ldh, iloz,ihiz, z, ldz, ls,&
                         ld, wr, wi, h, ldh, n, h, ldh,n, h, ldh, work, -1_ilp )
              ! ==== optimal workspace = max(stdlib_slaqr5, stdlib_slaqr2) ====
              lwkopt = max( 3_ilp*nsr / 2_ilp, int( work( 1_ilp ),KIND=ilp) )
              ! ==== quick return in case of workspace query. ====
              if( lwork==-1_ilp ) then
                 work( 1_ilp ) = real( lwkopt,KIND=sp)
                 return
              end if
              ! ==== stdlib_slahqr/stdlib_slaqr0 crossover point ====
              nmin = stdlib_ilaenv( 12_ilp, 'SLAQR4', jbcmpz, n, ilo, ihi, lwork )
              nmin = max( ntiny, nmin )
              ! ==== nibble crossover point ====
              nibble = stdlib_ilaenv( 14_ilp, 'SLAQR4', jbcmpz, n, ilo, ihi, lwork )
              nibble = max( 0_ilp, nibble )
              ! ==== accumulate reflections during ttswp?  use block
              ! .    2-by-2 structure during matrix-matrix multiply? ====
              kacc22 = stdlib_ilaenv( 16_ilp, 'SLAQR4', jbcmpz, n, ilo, ihi, lwork )
              kacc22 = max( 0_ilp, kacc22 )
              kacc22 = min( 2_ilp, kacc22 )
              ! ==== nwmax = the largest possible deflation window for
              ! .    which there is sufficient workspace. ====
              nwmax = min( ( n-1 ) / 3_ilp, lwork / 2_ilp )
              nw = nwmax
              ! ==== nsmax = the largest number of simultaneous shifts
              ! .    for which there is sufficient workspace. ====
              nsmax = min( ( n-3 ) / 6_ilp, 2_ilp*lwork / 3_ilp )
              nsmax = nsmax - mod( nsmax, 2_ilp )
              ! ==== ndfl: an iteration count restarted at deflation. ====
              ndfl = 1_ilp
              ! ==== itmax = iteration limit ====
              itmax = max( 30_ilp, 2_ilp*kexsh )*max( 10_ilp, ( ihi-ilo+1 ) )
              ! ==== last row and column in the active block ====
              kbot = ihi
              ! ==== main loop ====
              loop_80: do it = 1, itmax
                 ! ==== done when kbot falls below ilo ====
                 if( kbot<ilo )go to 90
                 ! ==== locate active block ====
                 do k = kbot, ilo + 1, -1
                    if( h( k, k-1 )==zero )go to 20
                 end do
                 k = ilo
                 20 continue
                 ktop = k
                 ! ==== select deflation window size:
                 ! .    typical case:
                 ! .      if possible and advisable, nibble the entire
                 ! .      active block.  if not, use size min(nwr,nwmax)
                 ! .      or min(nwr+1,nwmax) depending upon which has
                 ! .      the smaller corresponding subdiagonal entry
                 ! .      (a heuristic).
                 ! .    exceptional case:
                 ! .      if there have been no deflations in kexnw or
                 ! .      more iterations, then vary the deflation window
                 ! .      size.   at first, because, larger windows are,
                 ! .      in general, more powerful than smaller ones,
                 ! .      rapidly increase the window to the maximum possible.
                 ! .      then, gradually reduce the window size. ====
                 nh = kbot - ktop + 1_ilp
                 nwupbd = min( nh, nwmax )
                 if( ndfl<kexnw ) then
                    nw = min( nwupbd, nwr )
                 else
                    nw = min( nwupbd, 2_ilp*nw )
                 end if
                 if( nw<nwmax ) then
                    if( nw>=nh-1 ) then
                       nw = nh
                    else
                       kwtop = kbot - nw + 1_ilp
                       if( abs( h( kwtop, kwtop-1 ) )>abs( h( kwtop-1, kwtop-2 ) ) )nw = nw + &
                                 1_ilp
                    end if
                 end if
                 if( ndfl<kexnw ) then
                    ndec = -1_ilp
                 else if( ndec>=0_ilp .or. nw>=nwupbd ) then
                    ndec = ndec + 1_ilp
                    if( nw-ndec<2_ilp )ndec = 0_ilp
                    nw = nw - ndec
                 end if
                 ! ==== aggressive early deflation:
                 ! .    split workspace under the subdiagonal into
                 ! .      - an nw-by-nw work array v in the lower
                 ! .        left-hand-corner,
                 ! .      - an nw-by-at-least-nw-but-more-is-better
                 ! .        (nw-by-nho) horizontal work array along
                 ! .        the bottom edge,
                 ! .      - an at-least-nw-but-more-is-better (nhv-by-nw)
                 ! .        vertical work array along the left-hand-edge.
                 ! .        ====
                 kv = n - nw + 1_ilp
                 kt = nw + 1_ilp
                 nho = ( n-nw-1 ) - kt + 1_ilp
                 kwv = nw + 2_ilp
                 nve = ( n-nw ) - kwv + 1_ilp
                 ! ==== aggressive early deflation ====
                 call stdlib_slaqr2( wantt, wantz, n, ktop, kbot, nw, h, ldh, iloz,ihiz, z, ldz, &
                 ls, ld, wr, wi, h( kv, 1_ilp ), ldh,nho, h( kv, kt ), ldh, nve, h( kwv, 1_ilp ), ldh,&
                           work, lwork )
                 ! ==== adjust kbot accounting for new deflations. ====
                 kbot = kbot - ld
                 ! ==== ks points to the shifts. ====
                 ks = kbot - ls + 1_ilp
                 ! ==== skip an expensive qr sweep if there is a (partly
                 ! .    heuristic) reason to expect that many eigenvalues
                 ! .    will deflate without it.  here, the qr sweep is
                 ! .    skipped if many eigenvalues have just been deflated
                 ! .    or if the remaining active block is small.
                 if( ( ld==0_ilp ) .or. ( ( 100_ilp*ld<=nw*nibble ) .and. ( kbot-ktop+1>min( nmin, nwmax )&
                            ) ) ) then
                    ! ==== ns = nominal number of simultaneous shifts.
                    ! .    this may be lowered (slightly) if stdlib_slaqr2
                    ! .    did not provide that many shifts. ====
                    ns = min( nsmax, nsr, max( 2_ilp, kbot-ktop ) )
                    ns = ns - mod( ns, 2_ilp )
                    ! ==== if there have been no deflations
                    ! .    in a multiple of kexsh iterations,
                    ! .    then try exceptional shifts.
                    ! .    otherwise use shifts provided by
                    ! .    stdlib_slaqr2 above or from the eigenvalues
                    ! .    of a trailing principal submatrix. ====
                    if( mod( ndfl, kexsh )==0_ilp ) then
                       ks = kbot - ns + 1_ilp
                       do i = kbot, max( ks+1, ktop+2 ), -2
                          ss = abs( h( i, i-1 ) ) + abs( h( i-1, i-2 ) )
                          aa = wilk1*ss + h( i, i )
                          bb = ss
                          cc = wilk2*ss
                          dd = aa
                          call stdlib_slanv2( aa, bb, cc, dd, wr( i-1 ), wi( i-1 ),wr( i ), wi( i &
                                    ), cs, sn )
                       end do
                       if( ks==ktop ) then
                          wr( ks+1 ) = h( ks+1, ks+1 )
                          wi( ks+1 ) = zero
                          wr( ks ) = wr( ks+1 )
                          wi( ks ) = wi( ks+1 )
                       end if
                    else
                       ! ==== got ns/2 or fewer shifts? use stdlib_slahqr
                       ! .    on a trailing principal submatrix to
                       ! .    get more. (since ns<=nsmax<=(n-3)/6,
                       ! .    there is enough space below the subdiagonal
                       ! .    to fit an ns-by-ns scratch array.) ====
                       if( kbot-ks+1<=ns / 2_ilp ) then
                          ks = kbot - ns + 1_ilp
                          kt = n - ns + 1_ilp
                          call stdlib_slacpy( 'A', ns, ns, h( ks, ks ), ldh,h( kt, 1_ilp ), ldh )
                                    
                          call stdlib_slahqr( .false., .false., ns, 1_ilp, ns,h( kt, 1_ilp ), ldh, wr( ks &
                                    ), wi( ks ),1_ilp, 1_ilp, zdum, 1_ilp, inf )
                          ks = ks + inf
                          ! ==== in case of a rare qr failure use
                          ! .    eigenvalues of the trailing 2-by-2
                          ! .    principal submatrix.  ====
                          if( ks>=kbot ) then
                             aa = h( kbot-1, kbot-1 )
                             cc = h( kbot, kbot-1 )
                             bb = h( kbot-1, kbot )
                             dd = h( kbot, kbot )
                             call stdlib_slanv2( aa, bb, cc, dd, wr( kbot-1 ),wi( kbot-1 ), wr( &
                                       kbot ),wi( kbot ), cs, sn )
                             ks = kbot - 1_ilp
                          end if
                       end if
                       if( kbot-ks+1>ns ) then
                          ! ==== sort the shifts (helps a little)
                          ! .    bubble sort keeps complex conjugate
                          ! .    pairs together. ====
                          sorted = .false.
                          do k = kbot, ks + 1, -1
                             if( sorted )go to 60
                             sorted = .true.
                             do i = ks, k - 1
                                if( abs( wr( i ) )+abs( wi( i ) )<abs( wr( i+1 ) )+abs( wi( i+1 ) &
                                          ) ) then
                                   sorted = .false.
                                   swap = wr( i )
                                   wr( i ) = wr( i+1 )
                                   wr( i+1 ) = swap
                                   swap = wi( i )
                                   wi( i ) = wi( i+1 )
                                   wi( i+1 ) = swap
                                end if
                             end do
                          end do
                          60 continue
                       end if
                       ! ==== shuffle shifts into pairs of real shifts
                       ! .    and pairs of complex conjugate shifts
                       ! .    assuming complex conjugate shifts are
                       ! .    already adjacent to one another. (yes,
                       ! .    they are.)  ====
                       do i = kbot, ks + 2, -2
                          if( wi( i )/=-wi( i-1 ) ) then
                             swap = wr( i )
                             wr( i ) = wr( i-1 )
                             wr( i-1 ) = wr( i-2 )
                             wr( i-2 ) = swap
                             swap = wi( i )
                             wi( i ) = wi( i-1 )
                             wi( i-1 ) = wi( i-2 )
                             wi( i-2 ) = swap
                          end if
                       end do
                    end if
                    ! ==== if there are only two shifts and both are
                    ! .    real, then use only one.  ====
                    if( kbot-ks+1==2_ilp ) then
                       if( wi( kbot )==zero ) then
                          if( abs( wr( kbot )-h( kbot, kbot ) )<abs( wr( kbot-1 )-h( kbot, kbot ) &
                                    ) ) then
                             wr( kbot-1 ) = wr( kbot )
                          else
                             wr( kbot ) = wr( kbot-1 )
                          end if
                       end if
                    end if
                    ! ==== use up to ns of the the smallest magnitude
                    ! .    shifts.  if there aren't ns shifts available,
                    ! .    then use them all, possibly dropping one to
                    ! .    make the number of shifts even. ====
                    ns = min( ns, kbot-ks+1 )
                    ns = ns - mod( ns, 2_ilp )
                    ks = kbot - ns + 1_ilp
                    ! ==== small-bulge multi-shift qr sweep:
                    ! .    split workspace under the subdiagonal into
                    ! .    - a kdu-by-kdu work array u in the lower
                    ! .      left-hand-corner,
                    ! .    - a kdu-by-at-least-kdu-but-more-is-better
                    ! .      (kdu-by-nho) horizontal work array wh along
                    ! .      the bottom edge,
                    ! .    - and an at-least-kdu-but-more-is-better-by-kdu
                    ! .      (nve-by-kdu) vertical work wv arrow along
                    ! .      the left-hand-edge. ====
                    kdu = 2_ilp*ns
                    ku = n - kdu + 1_ilp
                    kwh = kdu + 1_ilp
                    nho = ( n-kdu+1-4 ) - ( kdu+1 ) + 1_ilp
                    kwv = kdu + 4_ilp
                    nve = n - kdu - kwv + 1_ilp
                    ! ==== small-bulge multi-shift qr sweep ====
                    call stdlib_slaqr5( wantt, wantz, kacc22, n, ktop, kbot, ns,wr( ks ), wi( ks )&
                    , h, ldh, iloz, ihiz, z,ldz, work, 3_ilp, h( ku, 1_ilp ), ldh, nve,h( kwv, 1_ilp ), ldh, &
                              nho, h( ku, kwh ), ldh )
                 end if
                 ! ==== note progress (or the lack of it). ====
                 if( ld>0_ilp ) then
                    ndfl = 1_ilp
                 else
                    ndfl = ndfl + 1_ilp
                 end if
                 ! ==== end of main loop ====
              end do loop_80
              ! ==== iteration limit exceeded.  set info to show where
              ! .    the problem occurred and exit. ====
              info = kbot
              90 continue
           end if
           ! ==== return the optimal value of lwork. ====
           work( 1_ilp ) = real( lwkopt,KIND=sp)
     end subroutine stdlib_slaqr4

     module subroutine stdlib_dlaqr4( wantt, wantz, n, ilo, ihi, h, ldh, wr, wi,iloz, ihiz, z, ldz, work,&
     !! DLAQR4 implements one level of recursion for DLAQR0.
     !! It is a complete implementation of the small bulge multi-shift
     !! QR algorithm.  It may be called by DLAQR0 and, for large enough
     !! deflation window size, it may be called by DLAQR3.  This
     !! subroutine is identical to DLAQR0 except that it calls DLAQR2
     !! instead of DLAQR3.
     !! DLAQR4 computes the eigenvalues of a Hessenberg matrix H
     !! and, optionally, the matrices T and Z from the Schur decomposition
     !! H = Z T Z**T, where T is an upper quasi-triangular matrix (the
     !! Schur form), and Z is the orthogonal matrix of Schur vectors.
     !! Optionally Z may be postmultiplied into an input orthogonal
     !! matrix Q so that this routine can give the Schur factorization
     !! of a matrix A which has been reduced to the Hessenberg form H
     !! by the orthogonal matrix Q:  A = Q*H*Q**T = (QZ)*T*(QZ)**T.
                lwork, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ihi, ihiz, ilo, iloz, ldh, ldz, lwork, n
           integer(ilp), intent(out) :: info
           logical(lk), intent(in) :: wantt, wantz
           ! Array Arguments 
           real(dp), intent(inout) :: h(ldh,*), z(ldz,*)
           real(dp), intent(out) :: wi(*), work(*), wr(*)
        ! ================================================================
           ! Parameters 
           integer(ilp), parameter :: ntiny = 15_ilp
           integer(ilp), parameter :: kexnw = 5_ilp
           integer(ilp), parameter :: kexsh = 6_ilp
           real(dp), parameter :: wilk1 = 0.75_dp
           real(dp), parameter :: wilk2 = -0.4375_dp
           ! ==== matrices of order ntiny or smaller must be processed by
           ! .    stdlib_dlahqr because of insufficient subdiagonal scratch space.
           ! .    (this is a hard limit.) ====
           
           ! ==== exceptional deflation windows:  try to cure rare
           ! .    slow convergence by varying the size of the
           ! .    deflation window after kexnw iterations. ====
           
           ! ==== exceptional shifts: try to cure rare slow convergence
           ! .    with ad-hoc exceptional shifts every kexsh iterations.
           ! .    ====
           
           ! ==== the constants wilk1 and wilk2 are used to form the
           ! .    exceptional shifts. ====
           
           
           ! Local Scalars 
           real(dp) :: aa, bb, cc, cs, dd, sn, ss, swap
           integer(ilp) :: i, inf, it, itmax, k, kacc22, kbot, kdu, ks, kt, ktop, ku, kv, kwh, &
           kwtop, kwv, ld, ls, lwkopt, ndec, ndfl, nh, nho, nibble, nmin, ns, nsmax, nsr, nve, nw,&
                      nwmax, nwr, nwupbd
           logical(lk) :: sorted
           character(len=2_ilp) :: jbcmpz
           ! Local Arrays 
           real(dp) :: zdum(1_ilp,1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           ! ==== quick return for n = 0: nothing to do. ====
           if( n==0_ilp ) then
              work( 1_ilp ) = one
              return
           end if
           if( n<=ntiny ) then
              ! ==== tiny matrices must use stdlib_dlahqr. ====
              lwkopt = 1_ilp
              if( lwork/=-1_ilp )call stdlib_dlahqr( wantt, wantz, n, ilo, ihi, h, ldh, wr, wi,iloz, &
                        ihiz, z, ldz, info )
           else
              ! ==== use small bulge multi-shift qr with aggressive early
              ! .    deflation on larger-than-tiny matrices. ====
              ! ==== hope for the best. ====
              info = 0_ilp
              ! ==== set up job flags for stdlib_ilaenv. ====
              if( wantt ) then
                 jbcmpz( 1_ilp: 1_ilp ) = 'S'
              else
                 jbcmpz( 1_ilp: 1_ilp ) = 'E'
              end if
              if( wantz ) then
                 jbcmpz( 2_ilp: 2_ilp ) = 'V'
              else
                 jbcmpz( 2_ilp: 2_ilp ) = 'N'
              end if
              ! ==== nwr = recommended deflation window size.  at this
              ! .    point,  n > ntiny = 15, so there is enough
              ! .    subdiagonal workspace for nwr>=2 as required.
              ! .    (in fact, there is enough subdiagonal space for
              ! .    nwr>=4.) ====
              nwr = stdlib_ilaenv( 13_ilp, 'DLAQR4', jbcmpz, n, ilo, ihi, lwork )
              nwr = max( 2_ilp, nwr )
              nwr = min( ihi-ilo+1, ( n-1 ) / 3_ilp, nwr )
              ! ==== nsr = recommended number of simultaneous shifts.
              ! .    at this point n > ntiny = 15, so there is at
              ! .    enough subdiagonal workspace for nsr to be even
              ! .    and greater than or equal to two as required. ====
              nsr = stdlib_ilaenv( 15_ilp, 'DLAQR4', jbcmpz, n, ilo, ihi, lwork )
              nsr = min( nsr, ( n-3 ) / 6_ilp, ihi-ilo )
              nsr = max( 2_ilp, nsr-mod( nsr, 2_ilp ) )
              ! ==== estimate optimal workspace ====
              ! ==== workspace query call to stdlib_dlaqr2 ====
              call stdlib_dlaqr2( wantt, wantz, n, ilo, ihi, nwr+1, h, ldh, iloz,ihiz, z, ldz, ls,&
                         ld, wr, wi, h, ldh, n, h, ldh,n, h, ldh, work, -1_ilp )
              ! ==== optimal workspace = max(stdlib_dlaqr5, stdlib_dlaqr2) ====
              lwkopt = max( 3_ilp*nsr / 2_ilp, int( work( 1_ilp ),KIND=ilp) )
              ! ==== quick return in case of workspace query. ====
              if( lwork==-1_ilp ) then
                 work( 1_ilp ) = real( lwkopt,KIND=dp)
                 return
              end if
              ! ==== stdlib_dlahqr/stdlib_dlaqr0 crossover point ====
              nmin = stdlib_ilaenv( 12_ilp, 'DLAQR4', jbcmpz, n, ilo, ihi, lwork )
              nmin = max( ntiny, nmin )
              ! ==== nibble crossover point ====
              nibble = stdlib_ilaenv( 14_ilp, 'DLAQR4', jbcmpz, n, ilo, ihi, lwork )
              nibble = max( 0_ilp, nibble )
              ! ==== accumulate reflections during ttswp?  use block
              ! .    2-by-2 structure during matrix-matrix multiply? ====
              kacc22 = stdlib_ilaenv( 16_ilp, 'DLAQR4', jbcmpz, n, ilo, ihi, lwork )
              kacc22 = max( 0_ilp, kacc22 )
              kacc22 = min( 2_ilp, kacc22 )
              ! ==== nwmax = the largest possible deflation window for
              ! .    which there is sufficient workspace. ====
              nwmax = min( ( n-1 ) / 3_ilp, lwork / 2_ilp )
              nw = nwmax
              ! ==== nsmax = the largest number of simultaneous shifts
              ! .    for which there is sufficient workspace. ====
              nsmax = min( ( n-3 ) / 6_ilp, 2_ilp*lwork / 3_ilp )
              nsmax = nsmax - mod( nsmax, 2_ilp )
              ! ==== ndfl: an iteration count restarted at deflation. ====
              ndfl = 1_ilp
              ! ==== itmax = iteration limit ====
              itmax = max( 30_ilp, 2_ilp*kexsh )*max( 10_ilp, ( ihi-ilo+1 ) )
              ! ==== last row and column in the active block ====
              kbot = ihi
              ! ==== main loop ====
              loop_80: do it = 1, itmax
                 ! ==== done when kbot falls below ilo ====
                 if( kbot<ilo )go to 90
                 ! ==== locate active block ====
                 do k = kbot, ilo + 1, -1
                    if( h( k, k-1 )==zero )go to 20
                 end do
                 k = ilo
                 20 continue
                 ktop = k
                 ! ==== select deflation window size:
                 ! .    typical case:
                 ! .      if possible and advisable, nibble the entire
                 ! .      active block.  if not, use size min(nwr,nwmax)
                 ! .      or min(nwr+1,nwmax) depending upon which has
                 ! .      the smaller corresponding subdiagonal entry
                 ! .      (a heuristic).
                 ! .    exceptional case:
                 ! .      if there have been no deflations in kexnw or
                 ! .      more iterations, then vary the deflation window
                 ! .      size.   at first, because, larger windows are,
                 ! .      in general, more powerful than smaller ones,
                 ! .      rapidly increase the window to the maximum possible.
                 ! .      then, gradually reduce the window size. ====
                 nh = kbot - ktop + 1_ilp
                 nwupbd = min( nh, nwmax )
                 if( ndfl<kexnw ) then
                    nw = min( nwupbd, nwr )
                 else
                    nw = min( nwupbd, 2_ilp*nw )
                 end if
                 if( nw<nwmax ) then
                    if( nw>=nh-1 ) then
                       nw = nh
                    else
                       kwtop = kbot - nw + 1_ilp
                       if( abs( h( kwtop, kwtop-1 ) )>abs( h( kwtop-1, kwtop-2 ) ) )nw = nw + &
                                 1_ilp
                    end if
                 end if
                 if( ndfl<kexnw ) then
                    ndec = -1_ilp
                 else if( ndec>=0_ilp .or. nw>=nwupbd ) then
                    ndec = ndec + 1_ilp
                    if( nw-ndec<2_ilp )ndec = 0_ilp
                    nw = nw - ndec
                 end if
                 ! ==== aggressive early deflation:
                 ! .    split workspace under the subdiagonal into
                 ! .      - an nw-by-nw work array v in the lower
                 ! .        left-hand-corner,
                 ! .      - an nw-by-at-least-nw-but-more-is-better
                 ! .        (nw-by-nho) horizontal work array along
                 ! .        the bottom edge,
                 ! .      - an at-least-nw-but-more-is-better (nhv-by-nw)
                 ! .        vertical work array along the left-hand-edge.
                 ! .        ====
                 kv = n - nw + 1_ilp
                 kt = nw + 1_ilp
                 nho = ( n-nw-1 ) - kt + 1_ilp
                 kwv = nw + 2_ilp
                 nve = ( n-nw ) - kwv + 1_ilp
                 ! ==== aggressive early deflation ====
                 call stdlib_dlaqr2( wantt, wantz, n, ktop, kbot, nw, h, ldh, iloz,ihiz, z, ldz, &
                 ls, ld, wr, wi, h( kv, 1_ilp ), ldh,nho, h( kv, kt ), ldh, nve, h( kwv, 1_ilp ), ldh,&
                           work, lwork )
                 ! ==== adjust kbot accounting for new deflations. ====
                 kbot = kbot - ld
                 ! ==== ks points to the shifts. ====
                 ks = kbot - ls + 1_ilp
                 ! ==== skip an expensive qr sweep if there is a (partly
                 ! .    heuristic) reason to expect that many eigenvalues
                 ! .    will deflate without it.  here, the qr sweep is
                 ! .    skipped if many eigenvalues have just been deflated
                 ! .    or if the remaining active block is small.
                 if( ( ld==0_ilp ) .or. ( ( 100_ilp*ld<=nw*nibble ) .and. ( kbot-ktop+1>min( nmin, nwmax )&
                            ) ) ) then
                    ! ==== ns = nominal number of simultaneous shifts.
                    ! .    this may be lowered (slightly) if stdlib_dlaqr2
                    ! .    did not provide that many shifts. ====
                    ns = min( nsmax, nsr, max( 2_ilp, kbot-ktop ) )
                    ns = ns - mod( ns, 2_ilp )
                    ! ==== if there have been no deflations
                    ! .    in a multiple of kexsh iterations,
                    ! .    then try exceptional shifts.
                    ! .    otherwise use shifts provided by
                    ! .    stdlib_dlaqr2 above or from the eigenvalues
                    ! .    of a trailing principal submatrix. ====
                    if( mod( ndfl, kexsh )==0_ilp ) then
                       ks = kbot - ns + 1_ilp
                       do i = kbot, max( ks+1, ktop+2 ), -2
                          ss = abs( h( i, i-1 ) ) + abs( h( i-1, i-2 ) )
                          aa = wilk1*ss + h( i, i )
                          bb = ss
                          cc = wilk2*ss
                          dd = aa
                          call stdlib_dlanv2( aa, bb, cc, dd, wr( i-1 ), wi( i-1 ),wr( i ), wi( i &
                                    ), cs, sn )
                       end do
                       if( ks==ktop ) then
                          wr( ks+1 ) = h( ks+1, ks+1 )
                          wi( ks+1 ) = zero
                          wr( ks ) = wr( ks+1 )
                          wi( ks ) = wi( ks+1 )
                       end if
                    else
                       ! ==== got ns/2 or fewer shifts? use stdlib_dlahqr
                       ! .    on a trailing principal submatrix to
                       ! .    get more. (since ns<=nsmax<=(n-3)/6,
                       ! .    there is enough space below the subdiagonal
                       ! .    to fit an ns-by-ns scratch array.) ====
                       if( kbot-ks+1<=ns / 2_ilp ) then
                          ks = kbot - ns + 1_ilp
                          kt = n - ns + 1_ilp
                          call stdlib_dlacpy( 'A', ns, ns, h( ks, ks ), ldh,h( kt, 1_ilp ), ldh )
                                    
                          call stdlib_dlahqr( .false., .false., ns, 1_ilp, ns,h( kt, 1_ilp ), ldh, wr( ks &
                                    ), wi( ks ),1_ilp, 1_ilp, zdum, 1_ilp, inf )
                          ks = ks + inf
                          ! ==== in case of a rare qr failure use
                          ! .    eigenvalues of the trailing 2-by-2
                          ! .    principal submatrix.  ====
                          if( ks>=kbot ) then
                             aa = h( kbot-1, kbot-1 )
                             cc = h( kbot, kbot-1 )
                             bb = h( kbot-1, kbot )
                             dd = h( kbot, kbot )
                             call stdlib_dlanv2( aa, bb, cc, dd, wr( kbot-1 ),wi( kbot-1 ), wr( &
                                       kbot ),wi( kbot ), cs, sn )
                             ks = kbot - 1_ilp
                          end if
                       end if
                       if( kbot-ks+1>ns ) then
                          ! ==== sort the shifts (helps a little)
                          ! .    bubble sort keeps complex conjugate
                          ! .    pairs together. ====
                          sorted = .false.
                          do k = kbot, ks + 1, -1
                             if( sorted )go to 60
                             sorted = .true.
                             do i = ks, k - 1
                                if( abs( wr( i ) )+abs( wi( i ) )<abs( wr( i+1 ) )+abs( wi( i+1 ) &
                                          ) ) then
                                   sorted = .false.
                                   swap = wr( i )
                                   wr( i ) = wr( i+1 )
                                   wr( i+1 ) = swap
                                   swap = wi( i )
                                   wi( i ) = wi( i+1 )
                                   wi( i+1 ) = swap
                                end if
                             end do
                          end do
                          60 continue
                       end if
                       ! ==== shuffle shifts into pairs of real shifts
                       ! .    and pairs of complex conjugate shifts
                       ! .    assuming complex conjugate shifts are
                       ! .    already adjacent to one another. (yes,
                       ! .    they are.)  ====
                       do i = kbot, ks + 2, -2
                          if( wi( i )/=-wi( i-1 ) ) then
                             swap = wr( i )
                             wr( i ) = wr( i-1 )
                             wr( i-1 ) = wr( i-2 )
                             wr( i-2 ) = swap
                             swap = wi( i )
                             wi( i ) = wi( i-1 )
                             wi( i-1 ) = wi( i-2 )
                             wi( i-2 ) = swap
                          end if
                       end do
                    end if
                    ! ==== if there are only two shifts and both are
                    ! .    real, then use only one.  ====
                    if( kbot-ks+1==2_ilp ) then
                       if( wi( kbot )==zero ) then
                          if( abs( wr( kbot )-h( kbot, kbot ) )<abs( wr( kbot-1 )-h( kbot, kbot ) &
                                    ) ) then
                             wr( kbot-1 ) = wr( kbot )
                          else
                             wr( kbot ) = wr( kbot-1 )
                          end if
                       end if
                    end if
                    ! ==== use up to ns of the the smallest magnitude
                    ! .    shifts.  if there aren't ns shifts available,
                    ! .    then use them all, possibly dropping one to
                    ! .    make the number of shifts even. ====
                    ns = min( ns, kbot-ks+1 )
                    ns = ns - mod( ns, 2_ilp )
                    ks = kbot - ns + 1_ilp
                    ! ==== small-bulge multi-shift qr sweep:
                    ! .    split workspace under the subdiagonal into
                    ! .    - a kdu-by-kdu work array u in the lower
                    ! .      left-hand-corner,
                    ! .    - a kdu-by-at-least-kdu-but-more-is-better
                    ! .      (kdu-by-nho) horizontal work array wh along
                    ! .      the bottom edge,
                    ! .    - and an at-least-kdu-but-more-is-better-by-kdu
                    ! .      (nve-by-kdu) vertical work wv arrow along
                    ! .      the left-hand-edge. ====
                    kdu = 2_ilp*ns
                    ku = n - kdu + 1_ilp
                    kwh = kdu + 1_ilp
                    nho = ( n-kdu+1-4 ) - ( kdu+1 ) + 1_ilp
                    kwv = kdu + 4_ilp
                    nve = n - kdu - kwv + 1_ilp
                    ! ==== small-bulge multi-shift qr sweep ====
                    call stdlib_dlaqr5( wantt, wantz, kacc22, n, ktop, kbot, ns,wr( ks ), wi( ks )&
                    , h, ldh, iloz, ihiz, z,ldz, work, 3_ilp, h( ku, 1_ilp ), ldh, nve,h( kwv, 1_ilp ), ldh, &
                              nho, h( ku, kwh ), ldh )
                 end if
                 ! ==== note progress (or the lack of it). ====
                 if( ld>0_ilp ) then
                    ndfl = 1_ilp
                 else
                    ndfl = ndfl + 1_ilp
                 end if
                 ! ==== end of main loop ====
              end do loop_80
              ! ==== iteration limit exceeded.  set info to show where
              ! .    the problem occurred and exit. ====
              info = kbot
              90 continue
           end if
           ! ==== return the optimal value of lwork. ====
           work( 1_ilp ) = real( lwkopt,KIND=dp)
     end subroutine stdlib_dlaqr4


     pure module subroutine stdlib_claqr4( wantt, wantz, n, ilo, ihi, h, ldh, w, iloz,ihiz, z, ldz, work,&
     !! CLAQR4 implements one level of recursion for CLAQR0.
     !! It is a complete implementation of the small bulge multi-shift
     !! QR algorithm.  It may be called by CLAQR0 and, for large enough
     !! deflation window size, it may be called by CLAQR3.  This
     !! subroutine is identical to CLAQR0 except that it calls CLAQR2
     !! instead of CLAQR3.
     !! CLAQR4 computes the eigenvalues of a Hessenberg matrix H
     !! and, optionally, the matrices T and Z from the Schur decomposition
     !! H = Z T Z**H, where T is an upper triangular matrix (the
     !! Schur form), and Z is the unitary matrix of Schur vectors.
     !! Optionally Z may be postmultiplied into an input unitary
     !! matrix Q so that this routine can give the Schur factorization
     !! of a matrix A which has been reduced to the Hessenberg form H
     !! by the unitary matrix Q:  A = Q*H*Q**H = (QZ)*H*(QZ)**H.
                lwork, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ihi, ihiz, ilo, iloz, ldh, ldz, lwork, n
           integer(ilp), intent(out) :: info
           logical(lk), intent(in) :: wantt, wantz
           ! Array Arguments 
           complex(sp), intent(inout) :: h(ldh,*), z(ldz,*)
           complex(sp), intent(out) :: w(*), work(*)
        ! ================================================================
           ! Parameters 
           integer(ilp), parameter :: ntiny = 15_ilp
           integer(ilp), parameter :: kexnw = 5_ilp
           integer(ilp), parameter :: kexsh = 6_ilp
           real(sp), parameter :: wilk1 = 0.75_sp
           ! ==== matrices of order ntiny or smaller must be processed by
           ! .    stdlib_clahqr because of insufficient subdiagonal scratch space.
           ! .    (this is a hard limit.) ====
           
           ! ==== exceptional deflation windows:  try to cure rare
           ! .    slow convergence by varying the size of the
           ! .    deflation window after kexnw iterations. ====
           
           ! ==== exceptional shifts: try to cure rare slow convergence
           ! .    with ad-hoc exceptional shifts every kexsh iterations.
           ! .    ====
           
           ! ==== the constant wilk1 is used to form the exceptional
           ! .    shifts. ====
           
           
           
           ! Local Scalars 
           complex(sp) :: aa, bb, cc, cdum, dd, det, rtdisc, swap, tr2
           real(sp) :: s
           integer(ilp) :: i, inf, it, itmax, k, kacc22, kbot, kdu, ks, kt, ktop, ku, kv, kwh, &
           kwtop, kwv, ld, ls, lwkopt, ndec, ndfl, nh, nho, nibble, nmin, ns, nsmax, nsr, nve, nw,&
                      nwmax, nwr, nwupbd
           logical(lk) :: sorted
           character(len=2) :: jbcmpz
           ! Local Arrays 
           complex(sp) :: zdum(1_ilp,1_ilp)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( cdum ) = abs( real( cdum,KIND=sp) ) + abs( aimag( cdum ) )
           ! Executable Statements 
           info = 0_ilp
           ! ==== quick return for n = 0: nothing to do. ====
           if( n==0_ilp ) then
              work( 1_ilp ) = cone
              return
           end if
           if( n<=ntiny ) then
              ! ==== tiny matrices must use stdlib_clahqr. ====
              lwkopt = 1_ilp
              if( lwork/=-1_ilp )call stdlib_clahqr( wantt, wantz, n, ilo, ihi, h, ldh, w, iloz,ihiz, &
                        z, ldz, info )
           else
              ! ==== use small bulge multi-shift qr with aggressive early
              ! .    deflation on larger-than-tiny matrices. ====
              ! ==== hope for the best. ====
              info = 0_ilp
              ! ==== set up job flags for stdlib_ilaenv. ====
              if( wantt ) then
                 jbcmpz( 1_ilp: 1_ilp ) = 'S'
              else
                 jbcmpz( 1_ilp: 1_ilp ) = 'E'
              end if
              if( wantz ) then
                 jbcmpz( 2_ilp: 2_ilp ) = 'V'
              else
                 jbcmpz( 2_ilp: 2_ilp ) = 'N'
              end if
              ! ==== nwr = recommended deflation window size.  at this
              ! .    point,  n > ntiny = 15, so there is enough
              ! .    subdiagonal workspace for nwr>=2 as required.
              ! .    (in fact, there is enough subdiagonal space for
              ! .    nwr>=4.) ====
              nwr = stdlib_ilaenv( 13_ilp, 'CLAQR4', jbcmpz, n, ilo, ihi, lwork )
              nwr = max( 2_ilp, nwr )
              nwr = min( ihi-ilo+1, ( n-1 ) / 3_ilp, nwr )
              ! ==== nsr = recommended number of simultaneous shifts.
              ! .    at this point n > ntiny = 15, so there is at
              ! .    enough subdiagonal workspace for nsr to be even
              ! .    and greater than or equal to two as required. ====
              nsr = stdlib_ilaenv( 15_ilp, 'CLAQR4', jbcmpz, n, ilo, ihi, lwork )
              nsr = min( nsr, ( n-3 ) / 6_ilp, ihi-ilo )
              nsr = max( 2_ilp, nsr-mod( nsr, 2_ilp ) )
              ! ==== estimate optimal workspace ====
              ! ==== workspace query call to stdlib_claqr2 ====
              call stdlib_claqr2( wantt, wantz, n, ilo, ihi, nwr+1, h, ldh, iloz,ihiz, z, ldz, ls,&
                         ld, w, h, ldh, n, h, ldh, n, h,ldh, work, -1_ilp )
              ! ==== optimal workspace = max(stdlib_claqr5, stdlib_claqr2) ====
              lwkopt = max( 3_ilp*nsr / 2_ilp, int( work( 1_ilp ),KIND=ilp) )
              ! ==== quick return in case of workspace query. ====
              if( lwork==-1_ilp ) then
                 work( 1_ilp ) = cmplx( lwkopt, 0_ilp,KIND=sp)
                 return
              end if
              ! ==== stdlib_clahqr/stdlib_claqr0 crossover point ====
              nmin = stdlib_ilaenv( 12_ilp, 'CLAQR4', jbcmpz, n, ilo, ihi, lwork )
              nmin = max( ntiny, nmin )
              ! ==== nibble crossover point ====
              nibble = stdlib_ilaenv( 14_ilp, 'CLAQR4', jbcmpz, n, ilo, ihi, lwork )
              nibble = max( 0_ilp, nibble )
              ! ==== accumulate reflections during ttswp?  use block
              ! .    2-by-2 structure during matrix-matrix multiply? ====
              kacc22 = stdlib_ilaenv( 16_ilp, 'CLAQR4', jbcmpz, n, ilo, ihi, lwork )
              kacc22 = max( 0_ilp, kacc22 )
              kacc22 = min( 2_ilp, kacc22 )
              ! ==== nwmax = the largest possible deflation window for
              ! .    which there is sufficient workspace. ====
              nwmax = min( ( n-1 ) / 3_ilp, lwork / 2_ilp )
              nw = nwmax
              ! ==== nsmax = the largest number of simultaneous shifts
              ! .    for which there is sufficient workspace. ====
              nsmax = min( ( n-3 ) / 6_ilp, 2_ilp*lwork / 3_ilp )
              nsmax = nsmax - mod( nsmax, 2_ilp )
              ! ==== ndfl: an iteration count restarted at deflation. ====
              ndfl = 1_ilp
              ! ==== itmax = iteration limit ====
              itmax = max( 30_ilp, 2_ilp*kexsh )*max( 10_ilp, ( ihi-ilo+1 ) )
              ! ==== last row and column in the active block ====
              kbot = ihi
              ! ==== main loop ====
              loop_70: do it = 1, itmax
                 ! ==== done when kbot falls below ilo ====
                 if( kbot<ilo )go to 80
                 ! ==== locate active block ====
                 do k = kbot, ilo + 1, -1
                    if( h( k, k-1 )==czero )go to 20
                 end do
                 k = ilo
                 20 continue
                 ktop = k
                 ! ==== select deflation window size:
                 ! .    typical case:
                 ! .      if possible and advisable, nibble the entire
                 ! .      active block.  if not, use size min(nwr,nwmax)
                 ! .      or min(nwr+1,nwmax) depending upon which has
                 ! .      the smaller corresponding subdiagonal entry
                 ! .      (a heuristic).
                 ! .    exceptional case:
                 ! .      if there have been no deflations in kexnw or
                 ! .      more iterations, then vary the deflation window
                 ! .      size.   at first, because, larger windows are,
                 ! .      in general, more powerful than smaller ones,
                 ! .      rapidly increase the window to the maximum possible.
                 ! .      then, gradually reduce the window size. ====
                 nh = kbot - ktop + 1_ilp
                 nwupbd = min( nh, nwmax )
                 if( ndfl<kexnw ) then
                    nw = min( nwupbd, nwr )
                 else
                    nw = min( nwupbd, 2_ilp*nw )
                 end if
                 if( nw<nwmax ) then
                    if( nw>=nh-1 ) then
                       nw = nh
                    else
                       kwtop = kbot - nw + 1_ilp
                       if( cabs1( h( kwtop, kwtop-1 ) )>cabs1( h( kwtop-1, kwtop-2 ) ) )nw = nw + &
                                 1_ilp
                    end if
                 end if
                 if( ndfl<kexnw ) then
                    ndec = -1_ilp
                 else if( ndec>=0_ilp .or. nw>=nwupbd ) then
                    ndec = ndec + 1_ilp
                    if( nw-ndec<2_ilp )ndec = 0_ilp
                    nw = nw - ndec
                 end if
                 ! ==== aggressive early deflation:
                 ! .    split workspace under the subdiagonal into
                 ! .      - an nw-by-nw work array v in the lower
                 ! .        left-hand-corner,
                 ! .      - an nw-by-at-least-nw-but-more-is-better
                 ! .        (nw-by-nho) horizontal work array along
                 ! .        the bottom edge,
                 ! .      - an at-least-nw-but-more-is-better (nhv-by-nw)
                 ! .        vertical work array along the left-hand-edge.
                 ! .        ====
                 kv = n - nw + 1_ilp
                 kt = nw + 1_ilp
                 nho = ( n-nw-1 ) - kt + 1_ilp
                 kwv = nw + 2_ilp
                 nve = ( n-nw ) - kwv + 1_ilp
                 ! ==== aggressive early deflation ====
                 call stdlib_claqr2( wantt, wantz, n, ktop, kbot, nw, h, ldh, iloz,ihiz, z, ldz, &
                 ls, ld, w, h( kv, 1_ilp ), ldh, nho,h( kv, kt ), ldh, nve, h( kwv, 1_ilp ), ldh, work,&
                           lwork )
                 ! ==== adjust kbot accounting for new deflations. ====
                 kbot = kbot - ld
                 ! ==== ks points to the shifts. ====
                 ks = kbot - ls + 1_ilp
                 ! ==== skip an expensive qr sweep if there is a (partly
                 ! .    heuristic) reason to expect that many eigenvalues
                 ! .    will deflate without it.  here, the qr sweep is
                 ! .    skipped if many eigenvalues have just been deflated
                 ! .    or if the remaining active block is small.
                 if( ( ld==0_ilp ) .or. ( ( 100_ilp*ld<=nw*nibble ) .and. ( kbot-ktop+1>min( nmin, nwmax )&
                            ) ) ) then
                    ! ==== ns = nominal number of simultaneous shifts.
                    ! .    this may be lowered (slightly) if stdlib_claqr2
                    ! .    did not provide that many shifts. ====
                    ns = min( nsmax, nsr, max( 2_ilp, kbot-ktop ) )
                    ns = ns - mod( ns, 2_ilp )
                    ! ==== if there have been no deflations
                    ! .    in a multiple of kexsh iterations,
                    ! .    then try exceptional shifts.
                    ! .    otherwise use shifts provided by
                    ! .    stdlib_claqr2 above or from the eigenvalues
                    ! .    of a trailing principal submatrix. ====
                    if( mod( ndfl, kexsh )==0_ilp ) then
                       ks = kbot - ns + 1_ilp
                       do i = kbot, ks + 1, -2
                          w( i ) = h( i, i ) + wilk1*cabs1( h( i, i-1 ) )
                          w( i-1 ) = w( i )
                       end do
                    else
                       ! ==== got ns/2 or fewer shifts? use stdlib_clahqr
                       ! .    on a trailing principal submatrix to
                       ! .    get more. (since ns<=nsmax<=(n-3)/6,
                       ! .    there is enough space below the subdiagonal
                       ! .    to fit an ns-by-ns scratch array.) ====
                       if( kbot-ks+1<=ns / 2_ilp ) then
                          ks = kbot - ns + 1_ilp
                          kt = n - ns + 1_ilp
                          call stdlib_clacpy( 'A', ns, ns, h( ks, ks ), ldh,h( kt, 1_ilp ), ldh )
                                    
                          call stdlib_clahqr( .false., .false., ns, 1_ilp, ns,h( kt, 1_ilp ), ldh, w( ks )&
                                    , 1_ilp, 1_ilp, zdum,1_ilp, inf )
                          ks = ks + inf
                          ! ==== in case of a rare qr failure use
                          ! .    eigenvalues of the trailing 2-by-2
                          ! .    principal submatrix.  scale to avoid
                          ! .    overflows, underflows and subnormals.
                          ! .    (the scale factor s can not be czero,
                          ! .    because h(kbot,kbot-1) is nonzero.) ====
                          if( ks>=kbot ) then
                             s = cabs1( h( kbot-1, kbot-1 ) ) +cabs1( h( kbot, kbot-1 ) ) +cabs1( &
                                       h( kbot-1, kbot ) ) +cabs1( h( kbot, kbot ) )
                             aa = h( kbot-1, kbot-1 ) / s
                             cc = h( kbot, kbot-1 ) / s
                             bb = h( kbot-1, kbot ) / s
                             dd = h( kbot, kbot ) / s
                             tr2 = ( aa+dd ) / two
                             det = ( aa-tr2 )*( dd-tr2 ) - bb*cc
                             rtdisc = sqrt( -det )
                             w( kbot-1 ) = ( tr2+rtdisc )*s
                             w( kbot ) = ( tr2-rtdisc )*s
                             ks = kbot - 1_ilp
                          end if
                       end if
                       if( kbot-ks+1>ns ) then
                          ! ==== sort the shifts (helps a little) ====
                          sorted = .false.
                          do k = kbot, ks + 1, -1
                             if( sorted )go to 60
                             sorted = .true.
                             do i = ks, k - 1
                                if( cabs1( w( i ) )<cabs1( w( i+1 ) ) )then
                                   sorted = .false.
                                   swap = w( i )
                                   w( i ) = w( i+1 )
                                   w( i+1 ) = swap
                                end if
                             end do
                          end do
                          60 continue
                       end if
                    end if
                    ! ==== if there are only two shifts, then use
                    ! .    only cone.  ====
                    if( kbot-ks+1==2_ilp ) then
                       if( cabs1( w( kbot )-h( kbot, kbot ) )<cabs1( w( kbot-1 )-h( kbot, kbot ) )&
                                  ) then
                          w( kbot-1 ) = w( kbot )
                       else
                          w( kbot ) = w( kbot-1 )
                       end if
                    end if
                    ! ==== use up to ns of the the smallest magnitude
                    ! .    shifts.  if there aren't ns shifts available,
                    ! .    then use them all, possibly dropping cone to
                    ! .    make the number of shifts even. ====
                    ns = min( ns, kbot-ks+1 )
                    ns = ns - mod( ns, 2_ilp )
                    ks = kbot - ns + 1_ilp
                    ! ==== small-bulge multi-shift qr sweep:
                    ! .    split workspace under the subdiagonal into
                    ! .    - a kdu-by-kdu work array u in the lower
                    ! .      left-hand-corner,
                    ! .    - a kdu-by-at-least-kdu-but-more-is-better
                    ! .      (kdu-by-nho) horizontal work array wh along
                    ! .      the bottom edge,
                    ! .    - and an at-least-kdu-but-more-is-better-by-kdu
                    ! .      (nve-by-kdu) vertical work wv arrow along
                    ! .      the left-hand-edge. ====
                    kdu = 2_ilp*ns
                    ku = n - kdu + 1_ilp
                    kwh = kdu + 1_ilp
                    nho = ( n-kdu+1-4 ) - ( kdu+1 ) + 1_ilp
                    kwv = kdu + 4_ilp
                    nve = n - kdu - kwv + 1_ilp
                    ! ==== small-bulge multi-shift qr sweep ====
                    call stdlib_claqr5( wantt, wantz, kacc22, n, ktop, kbot, ns,w( ks ), h, ldh, &
                    iloz, ihiz, z, ldz, work,3_ilp, h( ku, 1_ilp ), ldh, nve, h( kwv, 1_ilp ), ldh,nho, h( ku,&
                               kwh ), ldh )
                 end if
                 ! ==== note progress (or the lack of it). ====
                 if( ld>0_ilp ) then
                    ndfl = 1_ilp
                 else
                    ndfl = ndfl + 1_ilp
                 end if
                 ! ==== end of main loop ====
              end do loop_70
              ! ==== iteration limit exceeded.  set info to show where
              ! .    the problem occurred and exit. ====
              info = kbot
              80 continue
           end if
           ! ==== return the optimal value of lwork. ====
           work( 1_ilp ) = cmplx( lwkopt, 0_ilp,KIND=sp)
     end subroutine stdlib_claqr4

     pure module subroutine stdlib_zlaqr4( wantt, wantz, n, ilo, ihi, h, ldh, w, iloz,ihiz, z, ldz, work,&
     !! ZLAQR4 implements one level of recursion for ZLAQR0.
     !! It is a complete implementation of the small bulge multi-shift
     !! QR algorithm.  It may be called by ZLAQR0 and, for large enough
     !! deflation window size, it may be called by ZLAQR3.  This
     !! subroutine is identical to ZLAQR0 except that it calls ZLAQR2
     !! instead of ZLAQR3.
     !! ZLAQR4 computes the eigenvalues of a Hessenberg matrix H
     !! and, optionally, the matrices T and Z from the Schur decomposition
     !! H = Z T Z**H, where T is an upper triangular matrix (the
     !! Schur form), and Z is the unitary matrix of Schur vectors.
     !! Optionally Z may be postmultiplied into an input unitary
     !! matrix Q so that this routine can give the Schur factorization
     !! of a matrix A which has been reduced to the Hessenberg form H
     !! by the unitary matrix Q:  A = Q*H*Q**H = (QZ)*H*(QZ)**H.
                lwork, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ihi, ihiz, ilo, iloz, ldh, ldz, lwork, n
           integer(ilp), intent(out) :: info
           logical(lk), intent(in) :: wantt, wantz
           ! Array Arguments 
           complex(dp), intent(inout) :: h(ldh,*), z(ldz,*)
           complex(dp), intent(out) :: w(*), work(*)
        ! ================================================================
           ! Parameters 
           integer(ilp), parameter :: ntiny = 15_ilp
           integer(ilp), parameter :: kexnw = 5_ilp
           integer(ilp), parameter :: kexsh = 6_ilp
           real(dp), parameter :: wilk1 = 0.75_dp
           ! ==== matrices of order ntiny or smaller must be processed by
           ! .    stdlib_zlahqr because of insufficient subdiagonal scratch space.
           ! .    (this is a hard limit.) ====
           
           ! ==== exceptional deflation windows:  try to cure rare
           ! .    slow convergence by varying the size of the
           ! .    deflation window after kexnw iterations. ====
           
           ! ==== exceptional shifts: try to cure rare slow convergence
           ! .    with ad-hoc exceptional shifts every kexsh iterations.
           ! .    ====
           
           ! ==== the constant wilk1 is used to form the exceptional
           ! .    shifts. ====
           
           
           
           ! Local Scalars 
           complex(dp) :: aa, bb, cc, cdum, dd, det, rtdisc, swap, tr2
           real(dp) :: s
           integer(ilp) :: i, inf, it, itmax, k, kacc22, kbot, kdu, ks, kt, ktop, ku, kv, kwh, &
           kwtop, kwv, ld, ls, lwkopt, ndec, ndfl, nh, nho, nibble, nmin, ns, nsmax, nsr, nve, nw,&
                      nwmax, nwr, nwupbd
           logical(lk) :: sorted
           character(len=2) :: jbcmpz
           ! Local Arrays 
           complex(dp) :: zdum(1_ilp,1_ilp)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( cdum ) = abs( real( cdum,KIND=dp) ) + abs( aimag( cdum ) )
           ! Executable Statements 
           info = 0_ilp
           ! ==== quick return for n = 0: nothing to do. ====
           if( n==0_ilp ) then
              work( 1_ilp ) = cone
              return
           end if
           if( n<=ntiny ) then
              ! ==== tiny matrices must use stdlib_zlahqr. ====
              lwkopt = 1_ilp
              if( lwork/=-1_ilp )call stdlib_zlahqr( wantt, wantz, n, ilo, ihi, h, ldh, w, iloz,ihiz, &
                        z, ldz, info )
           else
              ! ==== use small bulge multi-shift qr with aggressive early
              ! .    deflation on larger-than-tiny matrices. ====
              ! ==== hope for the best. ====
              info = 0_ilp
              ! ==== set up job flags for stdlib_ilaenv. ====
              if( wantt ) then
                 jbcmpz( 1_ilp: 1_ilp ) = 'S'
              else
                 jbcmpz( 1_ilp: 1_ilp ) = 'E'
              end if
              if( wantz ) then
                 jbcmpz( 2_ilp: 2_ilp ) = 'V'
              else
                 jbcmpz( 2_ilp: 2_ilp ) = 'N'
              end if
              ! ==== nwr = recommended deflation window size.  at this
              ! .    point,  n > ntiny = 15, so there is enough
              ! .    subdiagonal workspace for nwr>=2 as required.
              ! .    (in fact, there is enough subdiagonal space for
              ! .    nwr>=4.) ====
              nwr = stdlib_ilaenv( 13_ilp, 'ZLAQR4', jbcmpz, n, ilo, ihi, lwork )
              nwr = max( 2_ilp, nwr )
              nwr = min( ihi-ilo+1, ( n-1 ) / 3_ilp, nwr )
              ! ==== nsr = recommended number of simultaneous shifts.
              ! .    at this point n > ntiny = 15, so there is at
              ! .    enough subdiagonal workspace for nsr to be even
              ! .    and greater than or equal to two as required. ====
              nsr = stdlib_ilaenv( 15_ilp, 'ZLAQR4', jbcmpz, n, ilo, ihi, lwork )
              nsr = min( nsr, ( n-3 ) / 6_ilp, ihi-ilo )
              nsr = max( 2_ilp, nsr-mod( nsr, 2_ilp ) )
              ! ==== estimate optimal workspace ====
              ! ==== workspace query call to stdlib_zlaqr2 ====
              call stdlib_zlaqr2( wantt, wantz, n, ilo, ihi, nwr+1, h, ldh, iloz,ihiz, z, ldz, ls,&
                         ld, w, h, ldh, n, h, ldh, n, h,ldh, work, -1_ilp )
              ! ==== optimal workspace = max(stdlib_zlaqr5, stdlib_zlaqr2) ====
              lwkopt = max( 3_ilp*nsr / 2_ilp, int( work( 1_ilp ),KIND=ilp) )
              ! ==== quick return in case of workspace query. ====
              if( lwork==-1_ilp ) then
                 work( 1_ilp ) = cmplx( lwkopt, 0_ilp,KIND=dp)
                 return
              end if
              ! ==== stdlib_zlahqr/stdlib_zlaqr0 crossover point ====
              nmin = stdlib_ilaenv( 12_ilp, 'ZLAQR4', jbcmpz, n, ilo, ihi, lwork )
              nmin = max( ntiny, nmin )
              ! ==== nibble crossover point ====
              nibble = stdlib_ilaenv( 14_ilp, 'ZLAQR4', jbcmpz, n, ilo, ihi, lwork )
              nibble = max( 0_ilp, nibble )
              ! ==== accumulate reflections during ttswp?  use block
              ! .    2-by-2 structure during matrix-matrix multiply? ====
              kacc22 = stdlib_ilaenv( 16_ilp, 'ZLAQR4', jbcmpz, n, ilo, ihi, lwork )
              kacc22 = max( 0_ilp, kacc22 )
              kacc22 = min( 2_ilp, kacc22 )
              ! ==== nwmax = the largest possible deflation window for
              ! .    which there is sufficient workspace. ====
              nwmax = min( ( n-1 ) / 3_ilp, lwork / 2_ilp )
              nw = nwmax
              ! ==== nsmax = the largest number of simultaneous shifts
              ! .    for which there is sufficient workspace. ====
              nsmax = min( ( n-3 ) / 6_ilp, 2_ilp*lwork / 3_ilp )
              nsmax = nsmax - mod( nsmax, 2_ilp )
              ! ==== ndfl: an iteration count restarted at deflation. ====
              ndfl = 1_ilp
              ! ==== itmax = iteration limit ====
              itmax = max( 30_ilp, 2_ilp*kexsh )*max( 10_ilp, ( ihi-ilo+1 ) )
              ! ==== last row and column in the active block ====
              kbot = ihi
              ! ==== main loop ====
              loop_70: do it = 1, itmax
                 ! ==== done when kbot falls below ilo ====
                 if( kbot<ilo )go to 80
                 ! ==== locate active block ====
                 do k = kbot, ilo + 1, -1
                    if( h( k, k-1 )==czero )go to 20
                 end do
                 k = ilo
                 20 continue
                 ktop = k
                 ! ==== select deflation window size:
                 ! .    typical case:
                 ! .      if possible and advisable, nibble the entire
                 ! .      active block.  if not, use size min(nwr,nwmax)
                 ! .      or min(nwr+1,nwmax) depending upon which has
                 ! .      the smaller corresponding subdiagonal entry
                 ! .      (a heuristic).
                 ! .    exceptional case:
                 ! .      if there have been no deflations in kexnw or
                 ! .      more iterations, then vary the deflation window
                 ! .      size.   at first, because, larger windows are,
                 ! .      in general, more powerful than smaller ones,
                 ! .      rapidly increase the window to the maximum possible.
                 ! .      then, gradually reduce the window size. ====
                 nh = kbot - ktop + 1_ilp
                 nwupbd = min( nh, nwmax )
                 if( ndfl<kexnw ) then
                    nw = min( nwupbd, nwr )
                 else
                    nw = min( nwupbd, 2_ilp*nw )
                 end if
                 if( nw<nwmax ) then
                    if( nw>=nh-1 ) then
                       nw = nh
                    else
                       kwtop = kbot - nw + 1_ilp
                       if( cabs1( h( kwtop, kwtop-1 ) )>cabs1( h( kwtop-1, kwtop-2 ) ) )nw = nw + &
                                 1_ilp
                    end if
                 end if
                 if( ndfl<kexnw ) then
                    ndec = -1_ilp
                 else if( ndec>=0_ilp .or. nw>=nwupbd ) then
                    ndec = ndec + 1_ilp
                    if( nw-ndec<2_ilp )ndec = 0_ilp
                    nw = nw - ndec
                 end if
                 ! ==== aggressive early deflation:
                 ! .    split workspace under the subdiagonal into
                 ! .      - an nw-by-nw work array v in the lower
                 ! .        left-hand-corner,
                 ! .      - an nw-by-at-least-nw-but-more-is-better
                 ! .        (nw-by-nho) horizontal work array along
                 ! .        the bottom edge,
                 ! .      - an at-least-nw-but-more-is-better (nhv-by-nw)
                 ! .        vertical work array along the left-hand-edge.
                 ! .        ====
                 kv = n - nw + 1_ilp
                 kt = nw + 1_ilp
                 nho = ( n-nw-1 ) - kt + 1_ilp
                 kwv = nw + 2_ilp
                 nve = ( n-nw ) - kwv + 1_ilp
                 ! ==== aggressive early deflation ====
                 call stdlib_zlaqr2( wantt, wantz, n, ktop, kbot, nw, h, ldh, iloz,ihiz, z, ldz, &
                 ls, ld, w, h( kv, 1_ilp ), ldh, nho,h( kv, kt ), ldh, nve, h( kwv, 1_ilp ), ldh, work,&
                           lwork )
                 ! ==== adjust kbot accounting for new deflations. ====
                 kbot = kbot - ld
                 ! ==== ks points to the shifts. ====
                 ks = kbot - ls + 1_ilp
                 ! ==== skip an expensive qr sweep if there is a (partly
                 ! .    heuristic) reason to expect that many eigenvalues
                 ! .    will deflate without it.  here, the qr sweep is
                 ! .    skipped if many eigenvalues have just been deflated
                 ! .    or if the remaining active block is small.
                 if( ( ld==0_ilp ) .or. ( ( 100_ilp*ld<=nw*nibble ) .and. ( kbot-ktop+1>min( nmin, nwmax )&
                            ) ) ) then
                    ! ==== ns = nominal number of simultaneous shifts.
                    ! .    this may be lowered (slightly) if stdlib_zlaqr2
                    ! .    did not provide that many shifts. ====
                    ns = min( nsmax, nsr, max( 2_ilp, kbot-ktop ) )
                    ns = ns - mod( ns, 2_ilp )
                    ! ==== if there have been no deflations
                    ! .    in a multiple of kexsh iterations,
                    ! .    then try exceptional shifts.
                    ! .    otherwise use shifts provided by
                    ! .    stdlib_zlaqr2 above or from the eigenvalues
                    ! .    of a trailing principal submatrix. ====
                    if( mod( ndfl, kexsh )==0_ilp ) then
                       ks = kbot - ns + 1_ilp
                       do i = kbot, ks + 1, -2
                          w( i ) = h( i, i ) + wilk1*cabs1( h( i, i-1 ) )
                          w( i-1 ) = w( i )
                       end do
                    else
                       ! ==== got ns/2 or fewer shifts? use stdlib_zlahqr
                       ! .    on a trailing principal submatrix to
                       ! .    get more. (since ns<=nsmax<=(n-3)/6,
                       ! .    there is enough space below the subdiagonal
                       ! .    to fit an ns-by-ns scratch array.) ====
                       if( kbot-ks+1<=ns / 2_ilp ) then
                          ks = kbot - ns + 1_ilp
                          kt = n - ns + 1_ilp
                          call stdlib_zlacpy( 'A', ns, ns, h( ks, ks ), ldh,h( kt, 1_ilp ), ldh )
                                    
                          call stdlib_zlahqr( .false., .false., ns, 1_ilp, ns,h( kt, 1_ilp ), ldh, w( ks )&
                                    , 1_ilp, 1_ilp, zdum,1_ilp, inf )
                          ks = ks + inf
                          ! ==== in case of a rare qr failure use
                          ! .    eigenvalues of the trailing 2-by-2
                          ! .    principal submatrix.  scale to avoid
                          ! .    overflows, underflows and subnormals.
                          ! .    (the scale factor s can not be czero,
                          ! .    because h(kbot,kbot-1) is nonzero.) ====
                          if( ks>=kbot ) then
                             s = cabs1( h( kbot-1, kbot-1 ) ) +cabs1( h( kbot, kbot-1 ) ) +cabs1( &
                                       h( kbot-1, kbot ) ) +cabs1( h( kbot, kbot ) )
                             aa = h( kbot-1, kbot-1 ) / s
                             cc = h( kbot, kbot-1 ) / s
                             bb = h( kbot-1, kbot ) / s
                             dd = h( kbot, kbot ) / s
                             tr2 = ( aa+dd ) / two
                             det = ( aa-tr2 )*( dd-tr2 ) - bb*cc
                             rtdisc = sqrt( -det )
                             w( kbot-1 ) = ( tr2+rtdisc )*s
                             w( kbot ) = ( tr2-rtdisc )*s
                             ks = kbot - 1_ilp
                          end if
                       end if
                       if( kbot-ks+1>ns ) then
                          ! ==== sort the shifts (helps a little) ====
                          sorted = .false.
                          do k = kbot, ks + 1, -1
                             if( sorted )go to 60
                             sorted = .true.
                             do i = ks, k - 1
                                if( cabs1( w( i ) )<cabs1( w( i+1 ) ) )then
                                   sorted = .false.
                                   swap = w( i )
                                   w( i ) = w( i+1 )
                                   w( i+1 ) = swap
                                end if
                             end do
                          end do
                          60 continue
                       end if
                    end if
                    ! ==== if there are only two shifts, then use
                    ! .    only cone.  ====
                    if( kbot-ks+1==2_ilp ) then
                       if( cabs1( w( kbot )-h( kbot, kbot ) )<cabs1( w( kbot-1 )-h( kbot, kbot ) )&
                                  ) then
                          w( kbot-1 ) = w( kbot )
                       else
                          w( kbot ) = w( kbot-1 )
                       end if
                    end if
                    ! ==== use up to ns of the the smallest magnitude
                    ! .    shifts.  if there aren't ns shifts available,
                    ! .    then use them all, possibly dropping cone to
                    ! .    make the number of shifts even. ====
                    ns = min( ns, kbot-ks+1 )
                    ns = ns - mod( ns, 2_ilp )
                    ks = kbot - ns + 1_ilp
                    ! ==== small-bulge multi-shift qr sweep:
                    ! .    split workspace under the subdiagonal into
                    ! .    - a kdu-by-kdu work array u in the lower
                    ! .      left-hand-corner,
                    ! .    - a kdu-by-at-least-kdu-but-more-is-better
                    ! .      (kdu-by-nho) horizontal work array wh along
                    ! .      the bottom edge,
                    ! .    - and an at-least-kdu-but-more-is-better-by-kdu
                    ! .      (nve-by-kdu) vertical work wv arrow along
                    ! .      the left-hand-edge. ====
                    kdu = 2_ilp*ns
                    ku = n - kdu + 1_ilp
                    kwh = kdu + 1_ilp
                    nho = ( n-kdu+1-4 ) - ( kdu+1 ) + 1_ilp
                    kwv = kdu + 4_ilp
                    nve = n - kdu - kwv + 1_ilp
                    ! ==== small-bulge multi-shift qr sweep ====
                    call stdlib_zlaqr5( wantt, wantz, kacc22, n, ktop, kbot, ns,w( ks ), h, ldh, &
                    iloz, ihiz, z, ldz, work,3_ilp, h( ku, 1_ilp ), ldh, nve, h( kwv, 1_ilp ), ldh,nho, h( ku,&
                               kwh ), ldh )
                 end if
                 ! ==== note progress (or the lack of it). ====
                 if( ld>0_ilp ) then
                    ndfl = 1_ilp
                 else
                    ndfl = ndfl + 1_ilp
                 end if
                 ! ==== end of main loop ====
              end do loop_70
              ! ==== iteration limit exceeded.  set info to show where
              ! .    the problem occurred and exit. ====
              info = kbot
              80 continue
           end if
           ! ==== return the optimal value of lwork. ====
           work( 1_ilp ) = cmplx( lwkopt, 0_ilp,KIND=dp)
     end subroutine stdlib_zlaqr4




     pure module subroutine stdlib_slaqr5( wantt, wantz, kacc22, n, ktop, kbot, nshfts,sr, si, h, ldh, &
     !! SLAQR5 , called by SLAQR0, performs a
     !! single small-bulge multi-shift QR sweep.
               iloz, ihiz, z, ldz, v, ldv, u,ldu, nv, wv, ldwv, nh, wh, ldwh )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ihiz, iloz, kacc22, kbot, ktop, ldh, ldu, ldv, ldwh, ldwv, &
                     ldz, n, nh, nshfts, nv
           logical(lk), intent(in) :: wantt, wantz
           ! Array Arguments 
           real(sp), intent(inout) :: h(ldh,*), si(*), sr(*), z(ldz,*)
           real(sp), intent(out) :: u(ldu,*), v(ldv,*), wh(ldwh,*), wv(ldwv,*)
        ! ================================================================
           
           ! Local Scalars 
           real(sp) :: alpha, beta, h11, h12, h21, h22, refsum, safmax, safmin, scl, smlnum, swap,&
                      tst1, tst2, ulp
           integer(ilp) :: i, i2, i4, incol, j, jbot, jcol, jlen, jrow, jtop, k, k1, kdu, kms, &
                     krcol, m, m22, mbot, mtop, nbmps, ndcol, ns, nu
           logical(lk) :: accum, bmp22
           ! Intrinsic Functions 
           ! Local Arrays 
           real(sp) :: vt(3_ilp)
           ! Executable Statements 
           ! ==== if there are no shifts, then there is nothing to do. ====
           if( nshfts<2 )return
           ! ==== if the active block is empty or 1-by-1, then there
           ! .    is nothing to do. ====
           if( ktop>=kbot )return
           ! ==== shuffle shifts into pairs of real shifts and pairs
           ! .    of complex conjugate shifts assuming complex
           ! .    conjugate shifts are already adjacent to one
           ! .    another. ====
           do i = 1, nshfts - 2, 2
              if( si( i )/=-si( i+1 ) ) then
                 swap = sr( i )
                 sr( i ) = sr( i+1 )
                 sr( i+1 ) = sr( i+2 )
                 sr( i+2 ) = swap
                 swap = si( i )
                 si( i ) = si( i+1 )
                 si( i+1 ) = si( i+2 )
                 si( i+2 ) = swap
              end if
           end do
           ! ==== nshfts is supposed to be even, but if it is odd,
           ! .    then simply reduce it by one.  the shuffle above
           ! .    ensures that the dropped shift is real and that
           ! .    the remaining shifts are paired. ====
           ns = nshfts - mod( nshfts, 2_ilp )
           ! ==== machine constants for deflation ====
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           safmax = one / safmin
           call stdlib_slabad( safmin, safmax )
           ulp = stdlib_slamch( 'PRECISION' )
           smlnum = safmin*( real( n,KIND=sp) / ulp )
           ! ==== use accumulated reflections to update far-from-diagonal
           ! .    entries ? ====
           accum = ( kacc22==1_ilp ) .or. ( kacc22==2_ilp )
           ! ==== clear trash ====
           if( ktop+2<=kbot )h( ktop+2, ktop ) = zero
           ! ==== nbmps = number of 2-shift bulges in the chain ====
           nbmps = ns / 2_ilp
           ! ==== kdu = width of slab ====
           kdu = 4_ilp*nbmps
           ! ==== create and chase chains of nbmps bulges ====
           loop_180: do incol = ktop - 2*nbmps + 1, kbot - 2, 2*nbmps
              ! jtop = index from which updates from the right start.
              if( accum ) then
                 jtop = max( ktop, incol )
              else if( wantt ) then
                 jtop = 1_ilp
              else
                 jtop = ktop
              end if
              ndcol = incol + kdu
              if( accum )call stdlib_slaset( 'ALL', kdu, kdu, zero, one, u, ldu )
              ! ==== near-the-diagonal bulge chase.  the following loop
              ! .    performs the near-the-diagonal part of a small bulge
              ! .    multi-shift qr sweep.  each 4*nbmps column diagonal
              ! .    chunk extends from column incol to column ndcol
              ! .    (including both column incol and column ndcol). the
              ! .    following loop chases a 2*nbmps+1 column long chain of
              ! .    nbmps bulges 2*nbmps-1 columns to the right.  (incol
              ! .    may be less than ktop and and ndcol may be greater than
              ! .    kbot indicating phantom columns from which to chase
              ! .    bulges before they are actually introduced or to which
              ! .    to chase bulges beyond column kbot.)  ====
              loop_145: do krcol = incol, min( incol+2*nbmps-1, kbot-2 )
                 ! ==== bulges number mtop to mbot are active double implicit
                 ! .    shift bulges.  there may or may not also be small
                 ! .    2-by-2 bulge, if there is room.  the inactive bulges
                 ! .    (if any) must wait until the active bulges have moved
                 ! .    down the diagonal to make room.  the phantom matrix
                 ! .    paradigm described above helps keep track.  ====
                 mtop = max( 1_ilp, ( ktop-krcol ) / 2_ilp+1 )
                 mbot = min( nbmps, ( kbot-krcol-1 ) / 2_ilp )
                 m22 = mbot + 1_ilp
                 bmp22 = ( mbot<nbmps ) .and. ( krcol+2*( m22-1 ) )==( kbot-2 )
                 ! ==== generate reflections to chase the chain right
                 ! .    one column.  (the minimum value of k is ktop-1.) ====
                 if ( bmp22 ) then
                    ! ==== special case: 2-by-2 reflection at bottom treated
                    ! .    separately ====
                    k = krcol + 2_ilp*( m22-1 )
                    if( k==ktop-1 ) then
                       call stdlib_slaqr1( 2_ilp, h( k+1, k+1 ), ldh, sr( 2_ilp*m22-1 ),si( 2_ilp*m22-1 ), sr(&
                                  2_ilp*m22 ), si( 2_ilp*m22 ),v( 1_ilp, m22 ) )
                       beta = v( 1_ilp, m22 )
                       call stdlib_slarfg( 2_ilp, beta, v( 2_ilp, m22 ), 1_ilp, v( 1_ilp, m22 ) )
                    else
                       beta = h( k+1, k )
                       v( 2_ilp, m22 ) = h( k+2, k )
                       call stdlib_slarfg( 2_ilp, beta, v( 2_ilp, m22 ), 1_ilp, v( 1_ilp, m22 ) )
                       h( k+1, k ) = beta
                       h( k+2, k ) = zero
                    end if
                    ! ==== perform update from right within
                    ! .    computational window. ====
                    do j = jtop, min( kbot, k+3 )
                       refsum = v( 1_ilp, m22 )*( h( j, k+1 )+v( 2_ilp, m22 )*h( j, k+2 ) )
                       h( j, k+1 ) = h( j, k+1 ) - refsum
                       h( j, k+2 ) = h( j, k+2 ) - refsum*v( 2_ilp, m22 )
                    end do
                    ! ==== perform update from left within
                    ! .    computational window. ====
                    if( accum ) then
                       jbot = min( ndcol, kbot )
                    else if( wantt ) then
                       jbot = n
                    else
                       jbot = kbot
                    end if
                    do j = k+1, jbot
                       refsum = v( 1_ilp, m22 )*( h( k+1, j )+v( 2_ilp, m22 )*h( k+2, j ) )
                       h( k+1, j ) = h( k+1, j ) - refsum
                       h( k+2, j ) = h( k+2, j ) - refsum*v( 2_ilp, m22 )
                    end do
                    ! ==== the following convergence test requires that
                    ! .    the tradition small-compared-to-nearby-diagonals
                    ! .    criterion and the ahues
                    ! .    criteria both be satisfied.  the latter improves
                    ! .    accuracy in some examples. falling back on an
                    ! .    alternate convergence criterion when tst1 or tst2
                    ! .    is zero (as done here) is traditional but probably
                    ! .    unnecessary. ====
                    if( k>=ktop ) then
                       if( h( k+1, k )/=zero ) then
                          tst1 = abs( h( k, k ) ) + abs( h( k+1, k+1 ) )
                          if( tst1==zero ) then
                             if( k>=ktop+1 )tst1 = tst1 + abs( h( k, k-1 ) )
                             if( k>=ktop+2 )tst1 = tst1 + abs( h( k, k-2 ) )
                             if( k>=ktop+3 )tst1 = tst1 + abs( h( k, k-3 ) )
                             if( k<=kbot-2 )tst1 = tst1 + abs( h( k+2, k+1 ) )
                             if( k<=kbot-3 )tst1 = tst1 + abs( h( k+3, k+1 ) )
                             if( k<=kbot-4 )tst1 = tst1 + abs( h( k+4, k+1 ) )
                          end if
                          if( abs( h( k+1, k ) )<=max( smlnum, ulp*tst1 ) )then
                             h12 = max( abs( h( k+1, k ) ),abs( h( k, k+1 ) ) )
                             h21 = min( abs( h( k+1, k ) ),abs( h( k, k+1 ) ) )
                             h11 = max( abs( h( k+1, k+1 ) ),abs( h( k, k )-h( k+1, k+1 ) ) )
                                       
                             h22 = min( abs( h( k+1, k+1 ) ),abs( h( k, k )-h( k+1, k+1 ) ) )
                                       
                             scl = h11 + h12
                             tst2 = h22*( h11 / scl )
                             if( tst2==zero .or. h21*( h12 / scl )<=max( smlnum, ulp*tst2 ) ) &
                                       then
                                h( k+1, k ) = zero
                             end if
                          end if
                       end if
                    end if
                    ! ==== accumulate orthogonal transformations. ====
                    if( accum ) then
                       kms = k - incol
                       do j = max( 1, ktop-incol ), kdu
                          refsum = v( 1_ilp, m22 )*( u( j, kms+1 )+v( 2_ilp, m22 )*u( j, kms+2 ) )
                          u( j, kms+1 ) = u( j, kms+1 ) - refsum
                          u( j, kms+2 ) = u( j, kms+2 ) - refsum*v( 2_ilp, m22 )
                       end do
                    else if( wantz ) then
                       do j = iloz, ihiz
                          refsum = v( 1_ilp, m22 )*( z( j, k+1 )+v( 2_ilp, m22 )*z( j, k+2 ) )
                          z( j, k+1 ) = z( j, k+1 ) - refsum
                          z( j, k+2 ) = z( j, k+2 ) - refsum*v( 2_ilp, m22 )
                       end do
                    end if
                 end if
                 ! ==== normal case: chain of 3-by-3 reflections ====
                 loop_80: do m = mbot, mtop, -1
                    k = krcol + 2_ilp*( m-1 )
                    if( k==ktop-1 ) then
                       call stdlib_slaqr1( 3_ilp, h( ktop, ktop ), ldh, sr( 2_ilp*m-1 ),si( 2_ilp*m-1 ), sr( &
                                 2_ilp*m ), si( 2_ilp*m ),v( 1_ilp, m ) )
                       alpha = v( 1_ilp, m )
                       call stdlib_slarfg( 3_ilp, alpha, v( 2_ilp, m ), 1_ilp, v( 1_ilp, m ) )
                    else
                       ! ==== perform delayed transformation of row below
                       ! .    mth bulge. exploit fact that first two elements
                       ! .    of row are actually zero. ====
                       refsum = v( 1_ilp, m )*v( 3_ilp, m )*h( k+3, k+2 )
                       h( k+3, k   ) = -refsum
                       h( k+3, k+1 ) = -refsum*v( 2_ilp, m )
                       h( k+3, k+2 ) = h( k+3, k+2 ) - refsum*v( 3_ilp, m )
                       ! ==== calculate reflection to move
                       ! .    mth bulge one step. ====
                       beta      = h( k+1, k )
                       v( 2_ilp, m ) = h( k+2, k )
                       v( 3_ilp, m ) = h( k+3, k )
                       call stdlib_slarfg( 3_ilp, beta, v( 2_ilp, m ), 1_ilp, v( 1_ilp, m ) )
                       ! ==== a bulge may collapse because of vigilant
                       ! .    deflation or destructive underflow.  in the
                       ! .    underflow case, try the two-small-subdiagonals
                       ! .    trick to try to reinflate the bulge.  ====
                       if( h( k+3, k )/=zero .or. h( k+3, k+1 )/=zero .or. h( k+3, k+2 )==zero ) &
                                 then
                          ! ==== typical case: not collapsed (yet). ====
                          h( k+1, k ) = beta
                          h( k+2, k ) = zero
                          h( k+3, k ) = zero
                       else
                          ! ==== atypical case: collapsed.  attempt to
                          ! .    reintroduce ignoring h(k+1,k) and h(k+2,k).
                          ! .    if the fill resulting from the new
                          ! .    reflector is too large, then abandon it.
                          ! .    otherwise, use the new one. ====
                          call stdlib_slaqr1( 3_ilp, h( k+1, k+1 ), ldh, sr( 2_ilp*m-1 ),si( 2_ilp*m-1 ), sr( &
                                    2_ilp*m ), si( 2_ilp*m ),vt )
                          alpha = vt( 1_ilp )
                          call stdlib_slarfg( 3_ilp, alpha, vt( 2_ilp ), 1_ilp, vt( 1_ilp ) )
                          refsum = vt( 1_ilp )*( h( k+1, k )+vt( 2_ilp )*h( k+2, k ) )
                          if( abs( h( k+2, k )-refsum*vt( 2_ilp ) )+abs( refsum*vt( 3_ilp ) )>ulp*( abs( &
                                    h( k, k ) )+abs( h( k+1,k+1 ) )+abs( h( k+2, k+2 ) ) ) ) then
                             ! ==== starting a new bulge here would
                             ! .    create non-negligible fill.  use
                             ! .    the old one with trepidation. ====
                             h( k+1, k ) = beta
                             h( k+2, k ) = zero
                             h( k+3, k ) = zero
                          else
                             ! ==== starting a new bulge here would
                             ! .    create only negligible fill.
                             ! .    replace the old reflector with
                             ! .    the new one. ====
                             h( k+1, k ) = h( k+1, k ) - refsum
                             h( k+2, k ) = zero
                             h( k+3, k ) = zero
                             v( 1_ilp, m ) = vt( 1_ilp )
                             v( 2_ilp, m ) = vt( 2_ilp )
                             v( 3_ilp, m ) = vt( 3_ilp )
                          end if
                       end if
                    end if
                    ! ====  apply reflection from the right and
                    ! .     the first column of update from the left.
                    ! .     these updates are required for the vigilant
                    ! .     deflation check. we still delay most of the
                    ! .     updates from the left for efficiency. ====
                    do j = jtop, min( kbot, k+3 )
                       refsum = v( 1_ilp, m )*( h( j, k+1 )+v( 2_ilp, m )*h( j, k+2 )+v( 3_ilp, m )*h( j, k+3 &
                                 ) )
                       h( j, k+1 ) = h( j, k+1 ) - refsum
                       h( j, k+2 ) = h( j, k+2 ) - refsum*v( 2_ilp, m )
                       h( j, k+3 ) = h( j, k+3 ) - refsum*v( 3_ilp, m )
                    end do
                    ! ==== perform update from left for subsequent
                    ! .    column. ====
                    refsum = v( 1_ilp, m )*( h( k+1, k+1 )+v( 2_ilp, m )*h( k+2, k+1 )+v( 3_ilp, m )*h( k+3, &
                              k+1 ) )
                    h( k+1, k+1 ) = h( k+1, k+1 ) - refsum
                    h( k+2, k+1 ) = h( k+2, k+1 ) - refsum*v( 2_ilp, m )
                    h( k+3, k+1 ) = h( k+3, k+1 ) - refsum*v( 3_ilp, m )
                    ! ==== the following convergence test requires that
                    ! .    the tradition small-compared-to-nearby-diagonals
                    ! .    criterion and the ahues
                    ! .    criteria both be satisfied.  the latter improves
                    ! .    accuracy in some examples. falling back on an
                    ! .    alternate convergence criterion when tst1 or tst2
                    ! .    is zero (as done here) is traditional but probably
                    ! .    unnecessary. ====
                    if( k<ktop)cycle
                    if( h( k+1, k )/=zero ) then
                       tst1 = abs( h( k, k ) ) + abs( h( k+1, k+1 ) )
                       if( tst1==zero ) then
                          if( k>=ktop+1 )tst1 = tst1 + abs( h( k, k-1 ) )
                          if( k>=ktop+2 )tst1 = tst1 + abs( h( k, k-2 ) )
                          if( k>=ktop+3 )tst1 = tst1 + abs( h( k, k-3 ) )
                          if( k<=kbot-2 )tst1 = tst1 + abs( h( k+2, k+1 ) )
                          if( k<=kbot-3 )tst1 = tst1 + abs( h( k+3, k+1 ) )
                          if( k<=kbot-4 )tst1 = tst1 + abs( h( k+4, k+1 ) )
                       end if
                       if( abs( h( k+1, k ) )<=max( smlnum, ulp*tst1 ) )then
                          h12 = max( abs( h( k+1, k ) ), abs( h( k, k+1 ) ) )
                          h21 = min( abs( h( k+1, k ) ), abs( h( k, k+1 ) ) )
                          h11 = max( abs( h( k+1, k+1 ) ),abs( h( k, k )-h( k+1, k+1 ) ) )
                          h22 = min( abs( h( k+1, k+1 ) ),abs( h( k, k )-h( k+1, k+1 ) ) )
                          scl = h11 + h12
                          tst2 = h22*( h11 / scl )
                          if( tst2==zero .or. h21*( h12 / scl )<=max( smlnum, ulp*tst2 ) ) &
                                    then
                             h( k+1, k ) = zero
                          end if
                       end if
                    end if
                 end do loop_80
                 ! ==== multiply h by reflections from the left ====
                 if( accum ) then
                    jbot = min( ndcol, kbot )
                 else if( wantt ) then
                    jbot = n
                 else
                    jbot = kbot
                 end if
                 do m = mbot, mtop, -1
                    k = krcol + 2_ilp*( m-1 )
                    do j = max( ktop, krcol + 2*m ), jbot
                       refsum = v( 1_ilp, m )*( h( k+1, j )+v( 2_ilp, m )*h( k+2, j )+v( 3_ilp, m )*h( k+3, j &
                                 ) )
                       h( k+1, j ) = h( k+1, j ) - refsum
                       h( k+2, j ) = h( k+2, j ) - refsum*v( 2_ilp, m )
                       h( k+3, j ) = h( k+3, j ) - refsum*v( 3_ilp, m )
                    end do
                 end do
                 ! ==== accumulate orthogonal transformations. ====
                 if( accum ) then
                    ! ==== accumulate u. (if needed, update z later
                    ! .    with an efficient matrix-matrix
                    ! .    multiply.) ====
                    do m = mbot, mtop, -1
                       k = krcol + 2_ilp*( m-1 )
                       kms = k - incol
                       i2 = max( 1_ilp, ktop-incol )
                       i2 = max( i2, kms-(krcol-incol)+1_ilp )
                       i4 = min( kdu, krcol + 2_ilp*( mbot-1 ) - incol + 5_ilp )
                       do j = i2, i4
                          refsum = v( 1_ilp, m )*( u( j, kms+1 )+v( 2_ilp, m )*u( j, kms+2 )+v( 3_ilp, m )*u( &
                                    j, kms+3 ) )
                          u( j, kms+1 ) = u( j, kms+1 ) - refsum
                          u( j, kms+2 ) = u( j, kms+2 ) - refsum*v( 2_ilp, m )
                          u( j, kms+3 ) = u( j, kms+3 ) - refsum*v( 3_ilp, m )
                       end do
                    end do
                 else if( wantz ) then
                    ! ==== u is not accumulated, so update z
                    ! .    now by multiplying by reflections
                    ! .    from the right. ====
                    do m = mbot, mtop, -1
                       k = krcol + 2_ilp*( m-1 )
                       do j = iloz, ihiz
                          refsum = v( 1_ilp, m )*( z( j, k+1 )+v( 2_ilp, m )*z( j, k+2 )+v( 3_ilp, m )*z( j, &
                                    k+3 ) )
                          z( j, k+1 ) = z( j, k+1 ) - refsum
                          z( j, k+2 ) = z( j, k+2 ) - refsum*v( 2_ilp, m )
                          z( j, k+3 ) = z( j, k+3 ) - refsum*v( 3_ilp, m )
                       end do
                    end do
                 end if
                 ! ==== end of near-the-diagonal bulge chase. ====
              end do loop_145
              ! ==== use u (if accumulated) to update far-from-diagonal
              ! .    entries in h.  if required, use u to update z as
              ! .    well. ====
              if( accum ) then
                 if( wantt ) then
                    jtop = 1_ilp
                    jbot = n
                 else
                    jtop = ktop
                    jbot = kbot
                 end if
                 k1 = max( 1_ilp, ktop-incol )
                 nu = ( kdu-max( 0_ilp, ndcol-kbot ) ) - k1 + 1_ilp
                 ! ==== horizontal multiply ====
                 do jcol = min( ndcol, kbot ) + 1, jbot, nh
                    jlen = min( nh, jbot-jcol+1 )
                    call stdlib_sgemm( 'C', 'N', nu, jlen, nu, one, u( k1, k1 ),ldu, h( incol+k1, &
                              jcol ), ldh, zero, wh,ldwh )
                    call stdlib_slacpy( 'ALL', nu, jlen, wh, ldwh,h( incol+k1, jcol ), ldh )
                              
                 end do
                 ! ==== vertical multiply ====
                 do jrow = jtop, max( ktop, incol ) - 1, nv
                    jlen = min( nv, max( ktop, incol )-jrow )
                    call stdlib_sgemm( 'N', 'N', jlen, nu, nu, one,h( jrow, incol+k1 ), ldh, u( &
                              k1, k1 ),ldu, zero, wv, ldwv )
                    call stdlib_slacpy( 'ALL', jlen, nu, wv, ldwv,h( jrow, incol+k1 ), ldh )
                              
                 end do
                 ! ==== z multiply (also vertical) ====
                 if( wantz ) then
                    do jrow = iloz, ihiz, nv
                       jlen = min( nv, ihiz-jrow+1 )
                       call stdlib_sgemm( 'N', 'N', jlen, nu, nu, one,z( jrow, incol+k1 ), ldz, u(&
                                  k1, k1 ),ldu, zero, wv, ldwv )
                       call stdlib_slacpy( 'ALL', jlen, nu, wv, ldwv,z( jrow, incol+k1 ), ldz )
                                 
                    end do
                 end if
              end if
           end do loop_180
     end subroutine stdlib_slaqr5

     pure module subroutine stdlib_dlaqr5( wantt, wantz, kacc22, n, ktop, kbot, nshfts,sr, si, h, ldh, &
     !! DLAQR5 , called by DLAQR0, performs a
     !! single small-bulge multi-shift QR sweep.
               iloz, ihiz, z, ldz, v, ldv, u,ldu, nv, wv, ldwv, nh, wh, ldwh )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ihiz, iloz, kacc22, kbot, ktop, ldh, ldu, ldv, ldwh, ldwv, &
                     ldz, n, nh, nshfts, nv
           logical(lk), intent(in) :: wantt, wantz
           ! Array Arguments 
           real(dp), intent(inout) :: h(ldh,*), si(*), sr(*), z(ldz,*)
           real(dp), intent(out) :: u(ldu,*), v(ldv,*), wh(ldwh,*), wv(ldwv,*)
        ! ================================================================
           
           ! Local Scalars 
           real(dp) :: alpha, beta, h11, h12, h21, h22, refsum, safmax, safmin, scl, smlnum, swap,&
                      tst1, tst2, ulp
           integer(ilp) :: i, i2, i4, incol, j, jbot, jcol, jlen, jrow, jtop, k, k1, kdu, kms, &
                     krcol, m, m22, mbot, mtop, nbmps, ndcol, ns, nu
           logical(lk) :: accum, bmp22
           ! Intrinsic Functions 
           ! Local Arrays 
           real(dp) :: vt(3_ilp)
           ! Executable Statements 
           ! ==== if there are no shifts, then there is nothing to do. ====
           if( nshfts<2 )return
           ! ==== if the active block is empty or 1-by-1, then there
           ! .    is nothing to do. ====
           if( ktop>=kbot )return
           ! ==== shuffle shifts into pairs of real shifts and pairs
           ! .    of complex conjugate shifts assuming complex
           ! .    conjugate shifts are already adjacent to one
           ! .    another. ====
           do i = 1, nshfts - 2, 2
              if( si( i )/=-si( i+1 ) ) then
                 swap = sr( i )
                 sr( i ) = sr( i+1 )
                 sr( i+1 ) = sr( i+2 )
                 sr( i+2 ) = swap
                 swap = si( i )
                 si( i ) = si( i+1 )
                 si( i+1 ) = si( i+2 )
                 si( i+2 ) = swap
              end if
           end do
           ! ==== nshfts is supposed to be even, but if it is odd,
           ! .    then simply reduce it by one.  the shuffle above
           ! .    ensures that the dropped shift is real and that
           ! .    the remaining shifts are paired. ====
           ns = nshfts - mod( nshfts, 2_ilp )
           ! ==== machine constants for deflation ====
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           safmax = one / safmin
           call stdlib_dlabad( safmin, safmax )
           ulp = stdlib_dlamch( 'PRECISION' )
           smlnum = safmin*( real( n,KIND=dp) / ulp )
           ! ==== use accumulated reflections to update far-from-diagonal
           ! .    entries ? ====
           accum = ( kacc22==1_ilp ) .or. ( kacc22==2_ilp )
           ! ==== clear trash ====
           if( ktop+2<=kbot )h( ktop+2, ktop ) = zero
           ! ==== nbmps = number of 2-shift bulges in the chain ====
           nbmps = ns / 2_ilp
           ! ==== kdu = width of slab ====
           kdu = 4_ilp*nbmps
           ! ==== create and chase chains of nbmps bulges ====
           loop_180: do incol = ktop - 2*nbmps + 1, kbot - 2, 2*nbmps
              ! jtop = index from which updates from the right start.
              if( accum ) then
                 jtop = max( ktop, incol )
              else if( wantt ) then
                 jtop = 1_ilp
              else
                 jtop = ktop
              end if
              ndcol = incol + kdu
              if( accum )call stdlib_dlaset( 'ALL', kdu, kdu, zero, one, u, ldu )
              ! ==== near-the-diagonal bulge chase.  the following loop
              ! .    performs the near-the-diagonal part of a small bulge
              ! .    multi-shift qr sweep.  each 4*nbmps column diagonal
              ! .    chunk extends from column incol to column ndcol
              ! .    (including both column incol and column ndcol). the
              ! .    following loop chases a 2*nbmps+1 column long chain of
              ! .    nbmps bulges 2*nbmps columns to the right.  (incol
              ! .    may be less than ktop and and ndcol may be greater than
              ! .    kbot indicating phantom columns from which to chase
              ! .    bulges before they are actually introduced or to which
              ! .    to chase bulges beyond column kbot.)  ====
              loop_145: do krcol = incol, min( incol+2*nbmps-1, kbot-2 )
                 ! ==== bulges number mtop to mbot are active double implicit
                 ! .    shift bulges.  there may or may not also be small
                 ! .    2-by-2 bulge, if there is room.  the inactive bulges
                 ! .    (if any) must wait until the active bulges have moved
                 ! .    down the diagonal to make room.  the phantom matrix
                 ! .    paradigm described above helps keep track.  ====
                 mtop = max( 1_ilp, ( ktop-krcol ) / 2_ilp+1 )
                 mbot = min( nbmps, ( kbot-krcol-1 ) / 2_ilp )
                 m22 = mbot + 1_ilp
                 bmp22 = ( mbot<nbmps ) .and. ( krcol+2*( m22-1 ) )==( kbot-2 )
                 ! ==== generate reflections to chase the chain right
                 ! .    one column.  (the minimum value of k is ktop-1.) ====
                 if ( bmp22 ) then
                    ! ==== special case: 2-by-2 reflection at bottom treated
                    ! .    separately ====
                    k = krcol + 2_ilp*( m22-1 )
                    if( k==ktop-1 ) then
                       call stdlib_dlaqr1( 2_ilp, h( k+1, k+1 ), ldh, sr( 2_ilp*m22-1 ),si( 2_ilp*m22-1 ), sr(&
                                  2_ilp*m22 ), si( 2_ilp*m22 ),v( 1_ilp, m22 ) )
                       beta = v( 1_ilp, m22 )
                       call stdlib_dlarfg( 2_ilp, beta, v( 2_ilp, m22 ), 1_ilp, v( 1_ilp, m22 ) )
                    else
                       beta = h( k+1, k )
                       v( 2_ilp, m22 ) = h( k+2, k )
                       call stdlib_dlarfg( 2_ilp, beta, v( 2_ilp, m22 ), 1_ilp, v( 1_ilp, m22 ) )
                       h( k+1, k ) = beta
                       h( k+2, k ) = zero
                    end if
                    ! ==== perform update from right within
                    ! .    computational window. ====
                    do j = jtop, min( kbot, k+3 )
                       refsum = v( 1_ilp, m22 )*( h( j, k+1 )+v( 2_ilp, m22 )*h( j, k+2 ) )
                       h( j, k+1 ) = h( j, k+1 ) - refsum
                       h( j, k+2 ) = h( j, k+2 ) - refsum*v( 2_ilp, m22 )
                    end do
                    ! ==== perform update from left within
                    ! .    computational window. ====
                    if( accum ) then
                       jbot = min( ndcol, kbot )
                    else if( wantt ) then
                       jbot = n
                    else
                       jbot = kbot
                    end if
                    do j = k+1, jbot
                       refsum = v( 1_ilp, m22 )*( h( k+1, j )+v( 2_ilp, m22 )*h( k+2, j ) )
                       h( k+1, j ) = h( k+1, j ) - refsum
                       h( k+2, j ) = h( k+2, j ) - refsum*v( 2_ilp, m22 )
                    end do
                    ! ==== the following convergence test requires that
                    ! .    the tradition small-compared-to-nearby-diagonals
                    ! .    criterion and the ahues
                    ! .    criteria both be satisfied.  the latter improves
                    ! .    accuracy in some examples. falling back on an
                    ! .    alternate convergence criterion when tst1 or tst2
                    ! .    is zero (as done here) is traditional but probably
                    ! .    unnecessary. ====
                    if( k>=ktop ) then
                       if( h( k+1, k )/=zero ) then
                          tst1 = abs( h( k, k ) ) + abs( h( k+1, k+1 ) )
                          if( tst1==zero ) then
                             if( k>=ktop+1 )tst1 = tst1 + abs( h( k, k-1 ) )
                             if( k>=ktop+2 )tst1 = tst1 + abs( h( k, k-2 ) )
                             if( k>=ktop+3 )tst1 = tst1 + abs( h( k, k-3 ) )
                             if( k<=kbot-2 )tst1 = tst1 + abs( h( k+2, k+1 ) )
                             if( k<=kbot-3 )tst1 = tst1 + abs( h( k+3, k+1 ) )
                             if( k<=kbot-4 )tst1 = tst1 + abs( h( k+4, k+1 ) )
                          end if
                          if( abs( h( k+1, k ) )<=max( smlnum, ulp*tst1 ) ) then
                             h12 = max( abs( h( k+1, k ) ),abs( h( k, k+1 ) ) )
                             h21 = min( abs( h( k+1, k ) ),abs( h( k, k+1 ) ) )
                             h11 = max( abs( h( k+1, k+1 ) ),abs( h( k, k )-h( k+1, k+1 ) ) )
                                       
                             h22 = min( abs( h( k+1, k+1 ) ),abs( h( k, k )-h( k+1, k+1 ) ) )
                                       
                             scl = h11 + h12
                             tst2 = h22*( h11 / scl )
                             if( tst2==zero .or. h21*( h12 / scl )<=max( smlnum, ulp*tst2 ) ) &
                                       then
                                h( k+1, k ) = zero
                             end if
                          end if
                       end if
                    end if
                    ! ==== accumulate orthogonal transformations. ====
                    if( accum ) then
                       kms = k - incol
                       do j = max( 1, ktop-incol ), kdu
                          refsum = v( 1_ilp, m22 )*( u( j, kms+1 )+v( 2_ilp, m22 )*u( j, kms+2 ) )
                          u( j, kms+1 ) = u( j, kms+1 ) - refsum
                          u( j, kms+2 ) = u( j, kms+2 ) - refsum*v( 2_ilp, m22 )
                       end do
                    else if( wantz ) then
                       do j = iloz, ihiz
                          refsum = v( 1_ilp, m22 )*( z( j, k+1 )+v( 2_ilp, m22 )*z( j, k+2 ) )
                          z( j, k+1 ) = z( j, k+1 ) - refsum
                          z( j, k+2 ) = z( j, k+2 ) - refsum*v( 2_ilp, m22 )
                       end do
                    end if
                 end if
                 ! ==== normal case: chain of 3-by-3 reflections ====
                 loop_80: do m = mbot, mtop, -1
                    k = krcol + 2_ilp*( m-1 )
                    if( k==ktop-1 ) then
                       call stdlib_dlaqr1( 3_ilp, h( ktop, ktop ), ldh, sr( 2_ilp*m-1 ),si( 2_ilp*m-1 ), sr( &
                                 2_ilp*m ), si( 2_ilp*m ),v( 1_ilp, m ) )
                       alpha = v( 1_ilp, m )
                       call stdlib_dlarfg( 3_ilp, alpha, v( 2_ilp, m ), 1_ilp, v( 1_ilp, m ) )
                    else
                       ! ==== perform delayed transformation of row below
                       ! .    mth bulge. exploit fact that first two elements
                       ! .    of row are actually zero. ====
                       refsum = v( 1_ilp, m )*v( 3_ilp, m )*h( k+3, k+2 )
                       h( k+3, k   ) = -refsum
                       h( k+3, k+1 ) = -refsum*v( 2_ilp, m )
                       h( k+3, k+2 ) = h( k+3, k+2 ) - refsum*v( 3_ilp, m )
                       ! ==== calculate reflection to move
                       ! .    mth bulge one step. ====
                       beta      = h( k+1, k )
                       v( 2_ilp, m ) = h( k+2, k )
                       v( 3_ilp, m ) = h( k+3, k )
                       call stdlib_dlarfg( 3_ilp, beta, v( 2_ilp, m ), 1_ilp, v( 1_ilp, m ) )
                       ! ==== a bulge may collapse because of vigilant
                       ! .    deflation or destructive underflow.  in the
                       ! .    underflow case, try the two-small-subdiagonals
                       ! .    trick to try to reinflate the bulge.  ====
                       if( h( k+3, k )/=zero .or. h( k+3, k+1 )/=zero .or. h( k+3, k+2 )==zero ) &
                                 then
                          ! ==== typical case: not collapsed (yet). ====
                          h( k+1, k ) = beta
                          h( k+2, k ) = zero
                          h( k+3, k ) = zero
                       else
                          ! ==== atypical case: collapsed.  attempt to
                          ! .    reintroduce ignoring h(k+1,k) and h(k+2,k).
                          ! .    if the fill resulting from the new
                          ! .    reflector is too large, then abandon it.
                          ! .    otherwise, use the new one. ====
                          call stdlib_dlaqr1( 3_ilp, h( k+1, k+1 ), ldh, sr( 2_ilp*m-1 ),si( 2_ilp*m-1 ), sr( &
                                    2_ilp*m ), si( 2_ilp*m ),vt )
                          alpha = vt( 1_ilp )
                          call stdlib_dlarfg( 3_ilp, alpha, vt( 2_ilp ), 1_ilp, vt( 1_ilp ) )
                          refsum = vt( 1_ilp )*( h( k+1, k )+vt( 2_ilp )*h( k+2, k ) )
                          if( abs( h( k+2, k )-refsum*vt( 2_ilp ) )+abs( refsum*vt( 3_ilp ) )>ulp*( abs( &
                                    h( k, k ) )+abs( h( k+1,k+1 ) )+abs( h( k+2, k+2 ) ) ) ) then
                             ! ==== starting a new bulge here would
                             ! .    create non-negligible fill.  use
                             ! .    the old one with trepidation. ====
                             h( k+1, k ) = beta
                             h( k+2, k ) = zero
                             h( k+3, k ) = zero
                          else
                             ! ==== starting a new bulge here would
                             ! .    create only negligible fill.
                             ! .    replace the old reflector with
                             ! .    the new one. ====
                             h( k+1, k ) = h( k+1, k ) - refsum
                             h( k+2, k ) = zero
                             h( k+3, k ) = zero
                             v( 1_ilp, m ) = vt( 1_ilp )
                             v( 2_ilp, m ) = vt( 2_ilp )
                             v( 3_ilp, m ) = vt( 3_ilp )
                          end if
                       end if
                    end if
                    ! ====  apply reflection from the right and
                    ! .     the first column of update from the left.
                    ! .     these updates are required for the vigilant
                    ! .     deflation check. we still delay most of the
                    ! .     updates from the left for efficiency. ====
                    do j = jtop, min( kbot, k+3 )
                       refsum = v( 1_ilp, m )*( h( j, k+1 )+v( 2_ilp, m )*h( j, k+2 )+v( 3_ilp, m )*h( j, k+3 &
                                 ) )
                       h( j, k+1 ) = h( j, k+1 ) - refsum
                       h( j, k+2 ) = h( j, k+2 ) - refsum*v( 2_ilp, m )
                       h( j, k+3 ) = h( j, k+3 ) - refsum*v( 3_ilp, m )
                    end do
                    ! ==== perform update from left for subsequent
                    ! .    column. ====
                    refsum = v( 1_ilp, m )*( h( k+1, k+1 )+v( 2_ilp, m )*h( k+2, k+1 )+v( 3_ilp, m )*h( k+3, &
                              k+1 ) )
                    h( k+1, k+1 ) = h( k+1, k+1 ) - refsum
                    h( k+2, k+1 ) = h( k+2, k+1 ) - refsum*v( 2_ilp, m )
                    h( k+3, k+1 ) = h( k+3, k+1 ) - refsum*v( 3_ilp, m )
                    ! ==== the following convergence test requires that
                    ! .    the tradition small-compared-to-nearby-diagonals
                    ! .    criterion and the ahues
                    ! .    criteria both be satisfied.  the latter improves
                    ! .    accuracy in some examples. falling back on an
                    ! .    alternate convergence criterion when tst1 or tst2
                    ! .    is zero (as done here) is traditional but probably
                    ! .    unnecessary. ====
                    if( k<ktop)cycle
                    if( h( k+1, k )/=zero ) then
                       tst1 = abs( h( k, k ) ) + abs( h( k+1, k+1 ) )
                       if( tst1==zero ) then
                          if( k>=ktop+1 )tst1 = tst1 + abs( h( k, k-1 ) )
                          if( k>=ktop+2 )tst1 = tst1 + abs( h( k, k-2 ) )
                          if( k>=ktop+3 )tst1 = tst1 + abs( h( k, k-3 ) )
                          if( k<=kbot-2 )tst1 = tst1 + abs( h( k+2, k+1 ) )
                          if( k<=kbot-3 )tst1 = tst1 + abs( h( k+3, k+1 ) )
                          if( k<=kbot-4 )tst1 = tst1 + abs( h( k+4, k+1 ) )
                       end if
                       if( abs( h( k+1, k ) )<=max( smlnum, ulp*tst1 ) )then
                          h12 = max( abs( h( k+1, k ) ), abs( h( k, k+1 ) ) )
                          h21 = min( abs( h( k+1, k ) ), abs( h( k, k+1 ) ) )
                          h11 = max( abs( h( k+1, k+1 ) ),abs( h( k, k )-h( k+1, k+1 ) ) )
                          h22 = min( abs( h( k+1, k+1 ) ),abs( h( k, k )-h( k+1, k+1 ) ) )
                          scl = h11 + h12
                          tst2 = h22*( h11 / scl )
                          if( tst2==zero .or. h21*( h12 / scl )<=max( smlnum, ulp*tst2 ) ) &
                                    then
                             h( k+1, k ) = zero
                          end if
                       end if
                    end if
                 end do loop_80
                 ! ==== multiply h by reflections from the left ====
                 if( accum ) then
                    jbot = min( ndcol, kbot )
                 else if( wantt ) then
                    jbot = n
                 else
                    jbot = kbot
                 end if
                 do m = mbot, mtop, -1
                    k = krcol + 2_ilp*( m-1 )
                    do j = max( ktop, krcol + 2*m ), jbot
                       refsum = v( 1_ilp, m )*( h( k+1, j )+v( 2_ilp, m )*h( k+2, j )+v( 3_ilp, m )*h( k+3, j &
                                 ) )
                       h( k+1, j ) = h( k+1, j ) - refsum
                       h( k+2, j ) = h( k+2, j ) - refsum*v( 2_ilp, m )
                       h( k+3, j ) = h( k+3, j ) - refsum*v( 3_ilp, m )
                    end do
                 end do
                 ! ==== accumulate orthogonal transformations. ====
                 if( accum ) then
                    ! ==== accumulate u. (if needed, update z later
                    ! .    with an efficient matrix-matrix
                    ! .    multiply.) ====
                    do m = mbot, mtop, -1
                       k = krcol + 2_ilp*( m-1 )
                       kms = k - incol
                       i2 = max( 1_ilp, ktop-incol )
                       i2 = max( i2, kms-(krcol-incol)+1_ilp )
                       i4 = min( kdu, krcol + 2_ilp*( mbot-1 ) - incol + 5_ilp )
                       do j = i2, i4
                          refsum = v( 1_ilp, m )*( u( j, kms+1 )+v( 2_ilp, m )*u( j, kms+2 )+v( 3_ilp, m )*u( &
                                    j, kms+3 ) )
                          u( j, kms+1 ) = u( j, kms+1 ) - refsum
                          u( j, kms+2 ) = u( j, kms+2 ) - refsum*v( 2_ilp, m )
                          u( j, kms+3 ) = u( j, kms+3 ) - refsum*v( 3_ilp, m )
                       end do
                    end do
                 else if( wantz ) then
                    ! ==== u is not accumulated, so update z
                    ! .    now by multiplying by reflections
                    ! .    from the right. ====
                    do m = mbot, mtop, -1
                       k = krcol + 2_ilp*( m-1 )
                       do j = iloz, ihiz
                          refsum = v( 1_ilp, m )*( z( j, k+1 )+v( 2_ilp, m )*z( j, k+2 )+v( 3_ilp, m )*z( j, &
                                    k+3 ) )
                          z( j, k+1 ) = z( j, k+1 ) - refsum
                          z( j, k+2 ) = z( j, k+2 ) - refsum*v( 2_ilp, m )
                          z( j, k+3 ) = z( j, k+3 ) - refsum*v( 3_ilp, m )
                       end do
                    end do
                 end if
                 ! ==== end of near-the-diagonal bulge chase. ====
              end do loop_145
              ! ==== use u (if accumulated) to update far-from-diagonal
              ! .    entries in h.  if required, use u to update z as
              ! .    well. ====
              if( accum ) then
                 if( wantt ) then
                    jtop = 1_ilp
                    jbot = n
                 else
                    jtop = ktop
                    jbot = kbot
                 end if
                 k1 = max( 1_ilp, ktop-incol )
                 nu = ( kdu-max( 0_ilp, ndcol-kbot ) ) - k1 + 1_ilp
                 ! ==== horizontal multiply ====
                 do jcol = min( ndcol, kbot ) + 1, jbot, nh
                    jlen = min( nh, jbot-jcol+1 )
                    call stdlib_dgemm( 'C', 'N', nu, jlen, nu, one, u( k1, k1 ),ldu, h( incol+k1, &
                              jcol ), ldh, zero, wh,ldwh )
                    call stdlib_dlacpy( 'ALL', nu, jlen, wh, ldwh,h( incol+k1, jcol ), ldh )
                              
                 end do
                 ! ==== vertical multiply ====
                 do jrow = jtop, max( ktop, incol ) - 1, nv
                    jlen = min( nv, max( ktop, incol )-jrow )
                    call stdlib_dgemm( 'N', 'N', jlen, nu, nu, one,h( jrow, incol+k1 ), ldh, u( &
                              k1, k1 ),ldu, zero, wv, ldwv )
                    call stdlib_dlacpy( 'ALL', jlen, nu, wv, ldwv,h( jrow, incol+k1 ), ldh )
                              
                 end do
                 ! ==== z multiply (also vertical) ====
                 if( wantz ) then
                    do jrow = iloz, ihiz, nv
                       jlen = min( nv, ihiz-jrow+1 )
                       call stdlib_dgemm( 'N', 'N', jlen, nu, nu, one,z( jrow, incol+k1 ), ldz, u(&
                                  k1, k1 ),ldu, zero, wv, ldwv )
                       call stdlib_dlacpy( 'ALL', jlen, nu, wv, ldwv,z( jrow, incol+k1 ), ldz )
                                 
                    end do
                 end if
              end if
           end do loop_180
     end subroutine stdlib_dlaqr5


     pure module subroutine stdlib_claqr5( wantt, wantz, kacc22, n, ktop, kbot, nshfts, s,h, ldh, iloz, &
     !! CLAQR5 called by CLAQR0 performs a
     !! single small-bulge multi-shift QR sweep.
               ihiz, z, ldz, v, ldv, u, ldu, nv,wv, ldwv, nh, wh, ldwh )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ihiz, iloz, kacc22, kbot, ktop, ldh, ldu, ldv, ldwh, ldwv, &
                     ldz, n, nh, nshfts, nv
           logical(lk), intent(in) :: wantt, wantz
           ! Array Arguments 
           complex(sp), intent(inout) :: h(ldh,*), s(*), z(ldz,*)
           complex(sp), intent(out) :: u(ldu,*), v(ldv,*), wh(ldwh,*), wv(ldwv,*)
        ! ================================================================
           ! Parameters 
           
           ! Local Scalars 
           complex(sp) :: alpha, beta, cdum, refsum
           real(sp) :: h11, h12, h21, h22, safmax, safmin, scl, smlnum, tst1, tst2, ulp
           integer(ilp) :: i2, i4, incol, j, jbot, jcol, jlen, jrow, jtop, k, k1, kdu, kms, krcol,&
                      m, m22, mbot, mtop, nbmps, ndcol, ns, nu
           logical(lk) :: accum, bmp22
           ! Intrinsic Functions 
           ! Local Arrays 
           complex(sp) :: vt(3_ilp)
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( cdum ) = abs( real( cdum,KIND=sp) ) + abs( aimag( cdum ) )
           ! Executable Statements 
           ! ==== if there are no shifts, then there is nothing to do. ====
           if( nshfts<2 )return
           ! ==== if the active block is empty or 1-by-1, then there
           ! .    is nothing to do. ====
           if( ktop>=kbot )return
           ! ==== nshfts is supposed to be even, but if it is odd,
           ! .    then simply reduce it by cone.  ====
           ns = nshfts - mod( nshfts, 2_ilp )
           ! ==== machine constants for deflation ====
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           safmax = one / safmin
           call stdlib_slabad( safmin, safmax )
           ulp = stdlib_slamch( 'PRECISION' )
           smlnum = safmin*( real( n,KIND=sp) / ulp )
           ! ==== use accumulated reflections to update far-from-diagonal
           ! .    entries ? ====
           accum = ( kacc22==1_ilp ) .or. ( kacc22==2_ilp )
           ! ==== clear trash ====
           if( ktop+2<=kbot )h( ktop+2, ktop ) = czero
           ! ==== nbmps = number of 2-shift bulges in the chain ====
           nbmps = ns / 2_ilp
           ! ==== kdu = width of slab ====
           kdu = 4_ilp*nbmps
           ! ==== create and chase chains of nbmps bulges ====
           loop_180: do incol = ktop - 2*nbmps + 1, kbot - 2, 2*nbmps
              ! jtop = index from which updates from the right start.
              if( accum ) then
                 jtop = max( ktop, incol )
              else if( wantt ) then
                 jtop = 1_ilp
              else
                 jtop = ktop
              end if
              ndcol = incol + kdu
              if( accum )call stdlib_claset( 'ALL', kdu, kdu, czero, cone, u, ldu )
              ! ==== near-the-diagonal bulge chase.  the following loop
              ! .    performs the near-the-diagonal part of a small bulge
              ! .    multi-shift qr sweep.  each 4*nbmps column diagonal
              ! .    chunk extends from column incol to column ndcol
              ! .    (including both column incol and column ndcol). the
              ! .    following loop chases a 2*nbmps+1 column long chain of
              ! .    nbmps bulges 2*nbmps columns to the right.  (incol
              ! .    may be less than ktop and and ndcol may be greater than
              ! .    kbot indicating phantom columns from which to chase
              ! .    bulges before they are actually introduced or to which
              ! .    to chase bulges beyond column kbot.)  ====
              loop_145: do krcol = incol, min( incol+2*nbmps-1, kbot-2 )
                 ! ==== bulges number mtop to mbot are active double implicit
                 ! .    shift bulges.  there may or may not also be small
                 ! .    2-by-2 bulge, if there is room.  the inactive bulges
                 ! .    (if any) must wait until the active bulges have moved
                 ! .    down the diagonal to make room.  the phantom matrix
                 ! .    paradigm described above helps keep track.  ====
                 mtop = max( 1_ilp, ( ktop-krcol ) / 2_ilp+1 )
                 mbot = min( nbmps, ( kbot-krcol-1 ) / 2_ilp )
                 m22 = mbot + 1_ilp
                 bmp22 = ( mbot<nbmps ) .and. ( krcol+2*( m22-1 ) )==( kbot-2 )
                 ! ==== generate reflections to chase the chain right
                 ! .    cone column.  (the minimum value of k is ktop-1.) ====
                 if ( bmp22 ) then
                    ! ==== special case: 2-by-2 reflection at bottom treated
                    ! .    separately ====
                    k = krcol + 2_ilp*( m22-1 )
                    if( k==ktop-1 ) then
                       call stdlib_claqr1( 2_ilp, h( k+1, k+1 ), ldh, s( 2_ilp*m22-1 ),s( 2_ilp*m22 ), v( 1_ilp, &
                                 m22 ) )
                       beta = v( 1_ilp, m22 )
                       call stdlib_clarfg( 2_ilp, beta, v( 2_ilp, m22 ), 1_ilp, v( 1_ilp, m22 ) )
                    else
                       beta = h( k+1, k )
                       v( 2_ilp, m22 ) = h( k+2, k )
                       call stdlib_clarfg( 2_ilp, beta, v( 2_ilp, m22 ), 1_ilp, v( 1_ilp, m22 ) )
                       h( k+1, k ) = beta
                       h( k+2, k ) = czero
                    end if
                    ! ==== perform update from right within
                    ! .    computational window. ====
                    do j = jtop, min( kbot, k+3 )
                       refsum = v( 1_ilp, m22 )*( h( j, k+1 )+v( 2_ilp, m22 )*h( j, k+2 ) )
                       h( j, k+1 ) = h( j, k+1 ) - refsum
                       h( j, k+2 ) = h( j, k+2 ) -refsum*conjg( v( 2_ilp, m22 ) )
                    end do
                    ! ==== perform update from left within
                    ! .    computational window. ====
                    if( accum ) then
                       jbot = min( ndcol, kbot )
                    else if( wantt ) then
                       jbot = n
                    else
                       jbot = kbot
                    end if
                    do j = k+1, jbot
                       refsum = conjg( v( 1_ilp, m22 ) )*( h( k+1, j )+conjg( v( 2_ilp, m22 ) )*h( k+2, j &
                                 ) )
                       h( k+1, j ) = h( k+1, j ) - refsum
                       h( k+2, j ) = h( k+2, j ) - refsum*v( 2_ilp, m22 )
                    end do
                    ! ==== the following convergence test requires that
                    ! .    the tradition small-compared-to-nearby-diagonals
                    ! .    criterion and the ahues
                    ! .    criteria both be satisfied.  the latter improves
                    ! .    accuracy in some examples. falling back on an
                    ! .    alternate convergence criterion when tst1 or tst2
                    ! .    is czero (as done here) is traditional but probably
                    ! .    unnecessary. ====
                    if( k>=ktop) then
                       if( h( k+1, k )/=czero ) then
                          tst1 = cabs1( h( k, k ) ) + cabs1( h( k+1, k+1 ) )
                          if( tst1==zero ) then
                             if( k>=ktop+1 )tst1 = tst1 + cabs1( h( k, k-1 ) )
                             if( k>=ktop+2 )tst1 = tst1 + cabs1( h( k, k-2 ) )
                             if( k>=ktop+3 )tst1 = tst1 + cabs1( h( k, k-3 ) )
                             if( k<=kbot-2 )tst1 = tst1 + cabs1( h( k+2, k+1 ) )
                             if( k<=kbot-3 )tst1 = tst1 + cabs1( h( k+3, k+1 ) )
                             if( k<=kbot-4 )tst1 = tst1 + cabs1( h( k+4, k+1 ) )
                          end if
                          if( cabs1( h( k+1, k ) )<=max( smlnum, ulp*tst1 ) ) then
                             h12 = max( cabs1( h( k+1, k ) ),cabs1( h( k, k+1 ) ) )
                             h21 = min( cabs1( h( k+1, k ) ),cabs1( h( k, k+1 ) ) )
                             h11 = max( cabs1( h( k+1, k+1 ) ),cabs1( h( k, k )-h( k+1, k+1 ) ) )
                                       
                             h22 = min( cabs1( h( k+1, k+1 ) ),cabs1( h( k, k )-h( k+1, k+1 ) ) )
                                       
                             scl = h11 + h12
                             tst2 = h22*( h11 / scl )
                             if( tst2==zero .or. h21*( h12 / scl )<=max( smlnum, ulp*tst2 ) )h( &
                                       k+1, k ) = czero
                          end if
                       end if
                    end if
                    ! ==== accumulate orthogonal transformations. ====
                    if( accum ) then
                       kms = k - incol
                       do j = max( 1, ktop-incol ), kdu
                          refsum = v( 1_ilp, m22 )*( u( j, kms+1 )+v( 2_ilp, m22 )*u( j, kms+2 ) )
                          u( j, kms+1 ) = u( j, kms+1 ) - refsum
                          u( j, kms+2 ) = u( j, kms+2 ) -refsum*conjg( v( 2_ilp, m22 ) )
                       end do
                    else if( wantz ) then
                       do j = iloz, ihiz
                          refsum = v( 1_ilp, m22 )*( z( j, k+1 )+v( 2_ilp, m22 )*z( j, k+2 ) )
                          z( j, k+1 ) = z( j, k+1 ) - refsum
                          z( j, k+2 ) = z( j, k+2 ) -refsum*conjg( v( 2_ilp, m22 ) )
                       end do
                    end if
                 end if
                 ! ==== normal case: chain of 3-by-3 reflections ====
                 loop_80: do m = mbot, mtop, -1
                    k = krcol + 2_ilp*( m-1 )
                    if( k==ktop-1 ) then
                       call stdlib_claqr1( 3_ilp, h( ktop, ktop ), ldh, s( 2_ilp*m-1 ),s( 2_ilp*m ), v( 1_ilp, m )&
                                  )
                       alpha = v( 1_ilp, m )
                       call stdlib_clarfg( 3_ilp, alpha, v( 2_ilp, m ), 1_ilp, v( 1_ilp, m ) )
                    else
                       ! ==== perform delayed transformation of row below
                       ! .    mth bulge. exploit fact that first two elements
                       ! .    of row are actually czero. ====
                       refsum = v( 1_ilp, m )*v( 3_ilp, m )*h( k+3, k+2 )
                       h( k+3, k   ) = -refsum
                       h( k+3, k+1 ) = -refsum*conjg( v( 2_ilp, m ) )
                       h( k+3, k+2 ) = h( k+3, k+2 ) -refsum*conjg( v( 3_ilp, m ) )
                       ! ==== calculate reflection to move
                       ! .    mth bulge cone step. ====
                       beta      = h( k+1, k )
                       v( 2_ilp, m ) = h( k+2, k )
                       v( 3_ilp, m ) = h( k+3, k )
                       call stdlib_clarfg( 3_ilp, beta, v( 2_ilp, m ), 1_ilp, v( 1_ilp, m ) )
                       ! ==== a bulge may collapse because of vigilant
                       ! .    deflation or destructive underflow.  in the
                       ! .    underflow case, try the two-small-subdiagonals
                       ! .    trick to try to reinflate the bulge.  ====
                       if( h( k+3, k )/=czero .or. h( k+3, k+1 )/=czero .or. h( k+3, k+2 )==czero &
                                 ) then
                          ! ==== typical case: not collapsed (yet). ====
                          h( k+1, k ) = beta
                          h( k+2, k ) = czero
                          h( k+3, k ) = czero
                       else
                          ! ==== atypical case: collapsed.  attempt to
                          ! .    reintroduce ignoring h(k+1,k) and h(k+2,k).
                          ! .    if the fill resulting from the new
                          ! .    reflector is too large, then abandon it.
                          ! .    otherwise, use the new cone. ====
                          call stdlib_claqr1( 3_ilp, h( k+1, k+1 ), ldh, s( 2_ilp*m-1 ),s( 2_ilp*m ), vt )
                                    
                          alpha = vt( 1_ilp )
                          call stdlib_clarfg( 3_ilp, alpha, vt( 2_ilp ), 1_ilp, vt( 1_ilp ) )
                          refsum = conjg( vt( 1_ilp ) )*( h( k+1, k )+conjg( vt( 2_ilp ) )*h( k+2, k ) )
                                    
                          if( cabs1( h( k+2, k )-refsum*vt( 2_ilp ) )+cabs1( refsum*vt( 3_ilp ) )>ulp*( &
                          cabs1( h( k, k ) )+cabs1( h( k+1,k+1 ) )+cabs1( h( k+2, k+2 ) ) ) ) &
                                    then
                             ! ==== starting a new bulge here would
                             ! .    create non-negligible fill.  use
                             ! .    the old cone with trepidation. ====
                             h( k+1, k ) = beta
                             h( k+2, k ) = czero
                             h( k+3, k ) = czero
                          else
                             ! ==== starting a new bulge here would
                             ! .    create only negligible fill.
                             ! .    replace the old reflector with
                             ! .    the new cone. ====
                             h( k+1, k ) = h( k+1, k ) - refsum
                             h( k+2, k ) = czero
                             h( k+3, k ) = czero
                             v( 1_ilp, m ) = vt( 1_ilp )
                             v( 2_ilp, m ) = vt( 2_ilp )
                             v( 3_ilp, m ) = vt( 3_ilp )
                          end if
                       end if
                    end if
                    ! ====  apply reflection from the right and
                    ! .     the first column of update from the left.
                    ! .     these updates are required for the vigilant
                    ! .     deflation check. we still delay most of the
                    ! .     updates from the left for efficiency. ====
                    do j = jtop, min( kbot, k+3 )
                       refsum = v( 1_ilp, m )*( h( j, k+1 )+v( 2_ilp, m )*h( j, k+2 )+v( 3_ilp, m )*h( j, k+3 &
                                 ) )
                       h( j, k+1 ) = h( j, k+1 ) - refsum
                       h( j, k+2 ) = h( j, k+2 ) -refsum*conjg( v( 2_ilp, m ) )
                       h( j, k+3 ) = h( j, k+3 ) -refsum*conjg( v( 3_ilp, m ) )
                    end do
                    ! ==== perform update from left for subsequent
                    ! .    column. ====
                    refsum =  conjg( v( 1_ilp, m ) )*( h( k+1, k+1 )+conjg( v( 2_ilp, m ) )*h( k+2, k+1 )+&
                              conjg( v( 3_ilp, m ) )*h( k+3, k+1 ) )
                    h( k+1, k+1 ) = h( k+1, k+1 ) - refsum
                    h( k+2, k+1 ) = h( k+2, k+1 ) - refsum*v( 2_ilp, m )
                    h( k+3, k+1 ) = h( k+3, k+1 ) - refsum*v( 3_ilp, m )
                    ! ==== the following convergence test requires that
                    ! .    the tradition small-compared-to-nearby-diagonals
                    ! .    criterion and the ahues
                    ! .    criteria both be satisfied.  the latter improves
                    ! .    accuracy in some examples. falling back on an
                    ! .    alternate convergence criterion when tst1 or tst2
                    ! .    is czero (as done here) is traditional but probably
                    ! .    unnecessary. ====
                    if( k<ktop)cycle
                    if( h( k+1, k )/=czero ) then
                       tst1 = cabs1( h( k, k ) ) + cabs1( h( k+1, k+1 ) )
                       if( tst1==zero ) then
                          if( k>=ktop+1 )tst1 = tst1 + cabs1( h( k, k-1 ) )
                          if( k>=ktop+2 )tst1 = tst1 + cabs1( h( k, k-2 ) )
                          if( k>=ktop+3 )tst1 = tst1 + cabs1( h( k, k-3 ) )
                          if( k<=kbot-2 )tst1 = tst1 + cabs1( h( k+2, k+1 ) )
                          if( k<=kbot-3 )tst1 = tst1 + cabs1( h( k+3, k+1 ) )
                          if( k<=kbot-4 )tst1 = tst1 + cabs1( h( k+4, k+1 ) )
                       end if
                       if( cabs1( h( k+1, k ) )<=max( smlnum, ulp*tst1 ) )then
                          h12 = max( cabs1( h( k+1, k ) ),cabs1( h( k, k+1 ) ) )
                          h21 = min( cabs1( h( k+1, k ) ),cabs1( h( k, k+1 ) ) )
                          h11 = max( cabs1( h( k+1, k+1 ) ),cabs1( h( k, k )-h( k+1, k+1 ) ) )
                                    
                          h22 = min( cabs1( h( k+1, k+1 ) ),cabs1( h( k, k )-h( k+1, k+1 ) ) )
                                    
                          scl = h11 + h12
                          tst2 = h22*( h11 / scl )
                          if( tst2==zero .or. h21*( h12 / scl )<=max( smlnum, ulp*tst2 ) )h( k+1,&
                                     k ) = czero
                       end if
                    end if
                 end do loop_80
                 ! ==== multiply h by reflections from the left ====
                 if( accum ) then
                    jbot = min( ndcol, kbot )
                 else if( wantt ) then
                    jbot = n
                 else
                    jbot = kbot
                 end if
                 do m = mbot, mtop, -1
                    k = krcol + 2_ilp*( m-1 )
                    do j = max( ktop, krcol + 2*m ), jbot
                       refsum = conjg( v( 1_ilp, m ) )*( h( k+1, j )+conjg( v( 2_ilp, m ) )*h( k+2, j )+&
                                 conjg( v( 3_ilp, m ) )*h( k+3, j ) )
                       h( k+1, j ) = h( k+1, j ) - refsum
                       h( k+2, j ) = h( k+2, j ) - refsum*v( 2_ilp, m )
                       h( k+3, j ) = h( k+3, j ) - refsum*v( 3_ilp, m )
                    end do
                 end do
                 ! ==== accumulate orthogonal transformations. ====
                 if( accum ) then
                    ! ==== accumulate u. (if needed, update z later
                    ! .    with an efficient matrix-matrix
                    ! .    multiply.) ====
                    do m = mbot, mtop, -1
                       k = krcol + 2_ilp*( m-1 )
                       kms = k - incol
                       i2 = max( 1_ilp, ktop-incol )
                       i2 = max( i2, kms-(krcol-incol)+1_ilp )
                       i4 = min( kdu, krcol + 2_ilp*( mbot-1 ) - incol + 5_ilp )
                       do j = i2, i4
                          refsum = v( 1_ilp, m )*( u( j, kms+1 )+v( 2_ilp, m )*u( j, kms+2 )+v( 3_ilp, m )*u( &
                                    j, kms+3 ) )
                          u( j, kms+1 ) = u( j, kms+1 ) - refsum
                          u( j, kms+2 ) = u( j, kms+2 ) -refsum*conjg( v( 2_ilp, m ) )
                          u( j, kms+3 ) = u( j, kms+3 ) -refsum*conjg( v( 3_ilp, m ) )
                       end do
                    end do
                 else if( wantz ) then
                    ! ==== u is not accumulated, so update z
                    ! .    now by multiplying by reflections
                    ! .    from the right. ====
                    do m = mbot, mtop, -1
                       k = krcol + 2_ilp*( m-1 )
                       do j = iloz, ihiz
                          refsum = v( 1_ilp, m )*( z( j, k+1 )+v( 2_ilp, m )*z( j, k+2 )+v( 3_ilp, m )*z( j, &
                                    k+3 ) )
                          z( j, k+1 ) = z( j, k+1 ) - refsum
                          z( j, k+2 ) = z( j, k+2 ) -refsum*conjg( v( 2_ilp, m ) )
                          z( j, k+3 ) = z( j, k+3 ) -refsum*conjg( v( 3_ilp, m ) )
                       end do
                    end do
                 end if
                 ! ==== end of near-the-diagonal bulge chase. ====
              end do loop_145
              ! ==== use u (if accumulated) to update far-from-diagonal
              ! .    entries in h.  if required, use u to update z as
              ! .    well. ====
              if( accum ) then
                 if( wantt ) then
                    jtop = 1_ilp
                    jbot = n
                 else
                    jtop = ktop
                    jbot = kbot
                 end if
                 k1 = max( 1_ilp, ktop-incol )
                 nu = ( kdu-max( 0_ilp, ndcol-kbot ) ) - k1 + 1_ilp
                 ! ==== horizontal multiply ====
                 do jcol = min( ndcol, kbot ) + 1, jbot, nh
                    jlen = min( nh, jbot-jcol+1 )
                    call stdlib_cgemm( 'C', 'N', nu, jlen, nu, cone, u( k1, k1 ),ldu, h( incol+k1,&
                               jcol ), ldh, czero, wh,ldwh )
                    call stdlib_clacpy( 'ALL', nu, jlen, wh, ldwh,h( incol+k1, jcol ), ldh )
                              
                 end do
                 ! ==== vertical multiply ====
                 do jrow = jtop, max( ktop, incol ) - 1, nv
                    jlen = min( nv, max( ktop, incol )-jrow )
                    call stdlib_cgemm( 'N', 'N', jlen, nu, nu, cone,h( jrow, incol+k1 ), ldh, u( &
                              k1, k1 ),ldu, czero, wv, ldwv )
                    call stdlib_clacpy( 'ALL', jlen, nu, wv, ldwv,h( jrow, incol+k1 ), ldh )
                              
                 end do
                 ! ==== z multiply (also vertical) ====
                 if( wantz ) then
                    do jrow = iloz, ihiz, nv
                       jlen = min( nv, ihiz-jrow+1 )
                       call stdlib_cgemm( 'N', 'N', jlen, nu, nu, cone,z( jrow, incol+k1 ), ldz, &
                                 u( k1, k1 ),ldu, czero, wv, ldwv )
                       call stdlib_clacpy( 'ALL', jlen, nu, wv, ldwv,z( jrow, incol+k1 ), ldz )
                                 
                    end do
                 end if
              end if
           end do loop_180
     end subroutine stdlib_claqr5

     pure module subroutine stdlib_zlaqr5( wantt, wantz, kacc22, n, ktop, kbot, nshfts, s,h, ldh, iloz, &
     !! ZLAQR5 , called by ZLAQR0, performs a
     !! single small-bulge multi-shift QR sweep.
               ihiz, z, ldz, v, ldv, u, ldu, nv,wv, ldwv, nh, wh, ldwh )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ihiz, iloz, kacc22, kbot, ktop, ldh, ldu, ldv, ldwh, ldwv, &
                     ldz, n, nh, nshfts, nv
           logical(lk), intent(in) :: wantt, wantz
           ! Array Arguments 
           complex(dp), intent(inout) :: h(ldh,*), s(*), z(ldz,*)
           complex(dp), intent(out) :: u(ldu,*), v(ldv,*), wh(ldwh,*), wv(ldwv,*)
        ! ================================================================
           ! Parameters 
           
           ! Local Scalars 
           complex(dp) :: alpha, beta, cdum, refsum
           real(dp) :: h11, h12, h21, h22, safmax, safmin, scl, smlnum, tst1, tst2, ulp
           integer(ilp) :: i2, i4, incol, j, jbot, jcol, jlen, jrow, jtop, k, k1, kdu, kms, krcol,&
                      m, m22, mbot, mtop, nbmps, ndcol, ns, nu
           logical(lk) :: accum, bmp22
           ! Intrinsic Functions 
           ! Local Arrays 
           complex(dp) :: vt(3_ilp)
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( cdum ) = abs( real( cdum,KIND=dp) ) + abs( aimag( cdum ) )
           ! Executable Statements 
           ! ==== if there are no shifts, then there is nothing to do. ====
           if( nshfts<2 )return
           ! ==== if the active block is empty or 1-by-1, then there
           ! .    is nothing to do. ====
           if( ktop>=kbot )return
           ! ==== nshfts is supposed to be even, but if it is odd,
           ! .    then simply reduce it by cone.  ====
           ns = nshfts - mod( nshfts, 2_ilp )
           ! ==== machine constants for deflation ====
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           safmax = one / safmin
           call stdlib_dlabad( safmin, safmax )
           ulp = stdlib_dlamch( 'PRECISION' )
           smlnum = safmin*( real( n,KIND=dp) / ulp )
           ! ==== use accumulated reflections to update far-from-diagonal
           ! .    entries ? ====
           accum = ( kacc22==1_ilp ) .or. ( kacc22==2_ilp )
           ! ==== clear trash ====
           if( ktop+2<=kbot )h( ktop+2, ktop ) = czero
           ! ==== nbmps = number of 2-shift bulges in the chain ====
           nbmps = ns / 2_ilp
           ! ==== kdu = width of slab ====
           kdu = 4_ilp*nbmps
           ! ==== create and chase chains of nbmps bulges ====
           loop_180: do incol = ktop - 2*nbmps + 1, kbot - 2, 2*nbmps
              ! jtop = index from which updates from the right start.
              if( accum ) then
                 jtop = max( ktop, incol )
              else if( wantt ) then
                 jtop = 1_ilp
              else
                 jtop = ktop
              end if
              ndcol = incol + kdu
              if( accum )call stdlib_zlaset( 'ALL', kdu, kdu, czero, cone, u, ldu )
              ! ==== near-the-diagonal bulge chase.  the following loop
              ! .    performs the near-the-diagonal part of a small bulge
              ! .    multi-shift qr sweep.  each 4*nbmps column diagonal
              ! .    chunk extends from column incol to column ndcol
              ! .    (including both column incol and column ndcol). the
              ! .    following loop chases a 2*nbmps+1 column long chain of
              ! .    nbmps bulges 2*nbmps columns to the right.  (incol
              ! .    may be less than ktop and and ndcol may be greater than
              ! .    kbot indicating phantom columns from which to chase
              ! .    bulges before they are actually introduced or to which
              ! .    to chase bulges beyond column kbot.)  ====
              loop_145: do krcol = incol, min( incol+2*nbmps-1, kbot-2 )
                 ! ==== bulges number mtop to mbot are active double implicit
                 ! .    shift bulges.  there may or may not also be small
                 ! .    2-by-2 bulge, if there is room.  the inactive bulges
                 ! .    (if any) must wait until the active bulges have moved
                 ! .    down the diagonal to make room.  the phantom matrix
                 ! .    paradigm described above helps keep track.  ====
                 mtop = max( 1_ilp, ( ktop-krcol ) / 2_ilp+1 )
                 mbot = min( nbmps, ( kbot-krcol-1 ) / 2_ilp )
                 m22 = mbot + 1_ilp
                 bmp22 = ( mbot<nbmps ) .and. ( krcol+2*( m22-1 ) )==( kbot-2 )
                 ! ==== generate reflections to chase the chain right
                 ! .    cone column.  (the minimum value of k is ktop-1.) ====
                 if ( bmp22 ) then
                    ! ==== special case: 2-by-2 reflection at bottom treated
                    ! .    separately ====
                    k = krcol + 2_ilp*( m22-1 )
                    if( k==ktop-1 ) then
                       call stdlib_zlaqr1( 2_ilp, h( k+1, k+1 ), ldh, s( 2_ilp*m22-1 ),s( 2_ilp*m22 ), v( 1_ilp, &
                                 m22 ) )
                       beta = v( 1_ilp, m22 )
                       call stdlib_zlarfg( 2_ilp, beta, v( 2_ilp, m22 ), 1_ilp, v( 1_ilp, m22 ) )
                    else
                       beta = h( k+1, k )
                       v( 2_ilp, m22 ) = h( k+2, k )
                       call stdlib_zlarfg( 2_ilp, beta, v( 2_ilp, m22 ), 1_ilp, v( 1_ilp, m22 ) )
                       h( k+1, k ) = beta
                       h( k+2, k ) = czero
                    end if
                    ! ==== perform update from right within
                    ! .    computational window. ====
                    do j = jtop, min( kbot, k+3 )
                       refsum = v( 1_ilp, m22 )*( h( j, k+1 )+v( 2_ilp, m22 )*h( j, k+2 ) )
                       h( j, k+1 ) = h( j, k+1 ) - refsum
                       h( j, k+2 ) = h( j, k+2 ) -refsum*conjg( v( 2_ilp, m22 ) )
                    end do
                    ! ==== perform update from left within
                    ! .    computational window. ====
                    if( accum ) then
                       jbot = min( ndcol, kbot )
                    else if( wantt ) then
                       jbot = n
                    else
                       jbot = kbot
                    end if
                    do j = k+1, jbot
                       refsum = conjg( v( 1_ilp, m22 ) )*( h( k+1, j )+conjg( v( 2_ilp, m22 ) )*h( k+2, j &
                                 ) )
                       h( k+1, j ) = h( k+1, j ) - refsum
                       h( k+2, j ) = h( k+2, j ) - refsum*v( 2_ilp, m22 )
                    end do
                    ! ==== the following convergence test requires that
                    ! .    the tradition small-compared-to-nearby-diagonals
                    ! .    criterion and the ahues
                    ! .    criteria both be satisfied.  the latter improves
                    ! .    accuracy in some examples. falling back on an
                    ! .    alternate convergence criterion when tst1 or tst2
                    ! .    is czero (as done here) is traditional but probably
                    ! .    unnecessary. ====
                    if( k>=ktop ) then
                       if( h( k+1, k )/=czero ) then
                          tst1 = cabs1( h( k, k ) ) + cabs1( h( k+1, k+1 ) )
                          if( tst1==zero ) then
                             if( k>=ktop+1 )tst1 = tst1 + cabs1( h( k, k-1 ) )
                             if( k>=ktop+2 )tst1 = tst1 + cabs1( h( k, k-2 ) )
                             if( k>=ktop+3 )tst1 = tst1 + cabs1( h( k, k-3 ) )
                             if( k<=kbot-2 )tst1 = tst1 + cabs1( h( k+2, k+1 ) )
                             if( k<=kbot-3 )tst1 = tst1 + cabs1( h( k+3, k+1 ) )
                             if( k<=kbot-4 )tst1 = tst1 + cabs1( h( k+4, k+1 ) )
                          end if
                          if( cabs1( h( k+1, k ) )<=max( smlnum, ulp*tst1 ) ) then
                             h12 = max( cabs1( h( k+1, k ) ),cabs1( h( k, k+1 ) ) )
                             h21 = min( cabs1( h( k+1, k ) ),cabs1( h( k, k+1 ) ) )
                             h11 = max( cabs1( h( k+1, k+1 ) ),cabs1( h( k, k )-h( k+1, k+1 ) ) )
                                       
                             h22 = min( cabs1( h( k+1, k+1 ) ),cabs1( h( k, k )-h( k+1, k+1 ) ) )
                                       
                             scl = h11 + h12
                             tst2 = h22*( h11 / scl )
                             if( tst2==zero .or. h21*( h12 / scl )<=max( smlnum, ulp*tst2 ) )h( &
                                       k+1, k ) = czero
                          end if
                       end if
                    end if
                    ! ==== accumulate orthogonal transformations. ====
                    if( accum ) then
                       kms = k - incol
                       do j = max( 1, ktop-incol ), kdu
                          refsum = v( 1_ilp, m22 )*( u( j, kms+1 )+v( 2_ilp, m22 )*u( j, kms+2 ) )
                          u( j, kms+1 ) = u( j, kms+1 ) - refsum
                          u( j, kms+2 ) = u( j, kms+2 ) -refsum*conjg( v( 2_ilp, m22 ) )
                       end do
                    else if( wantz ) then
                       do j = iloz, ihiz
                          refsum = v( 1_ilp, m22 )*( z( j, k+1 )+v( 2_ilp, m22 )*z( j, k+2 ) )
                          z( j, k+1 ) = z( j, k+1 ) - refsum
                          z( j, k+2 ) = z( j, k+2 ) -refsum*conjg( v( 2_ilp, m22 ) )
                       end do
                    end if
                 end if
                 ! ==== normal case: chain of 3-by-3 reflections ====
                 loop_80: do m = mbot, mtop, -1
                    k = krcol + 2_ilp*( m-1 )
                    if( k==ktop-1 ) then
                       call stdlib_zlaqr1( 3_ilp, h( ktop, ktop ), ldh, s( 2_ilp*m-1 ),s( 2_ilp*m ), v( 1_ilp, m )&
                                  )
                       alpha = v( 1_ilp, m )
                       call stdlib_zlarfg( 3_ilp, alpha, v( 2_ilp, m ), 1_ilp, v( 1_ilp, m ) )
                    else
                       ! ==== perform delayed transformation of row below
                       ! .    mth bulge. exploit fact that first two elements
                       ! .    of row are actually czero. ====
                       refsum = v( 1_ilp, m )*v( 3_ilp, m )*h( k+3, k+2 )
                       h( k+3, k   ) = -refsum
                       h( k+3, k+1 ) = -refsum*conjg( v( 2_ilp, m ) )
                       h( k+3, k+2 ) = h( k+3, k+2 ) -refsum*conjg( v( 3_ilp, m ) )
                       ! ==== calculate reflection to move
                       ! .    mth bulge cone step. ====
                       beta      = h( k+1, k )
                       v( 2_ilp, m ) = h( k+2, k )
                       v( 3_ilp, m ) = h( k+3, k )
                       call stdlib_zlarfg( 3_ilp, beta, v( 2_ilp, m ), 1_ilp, v( 1_ilp, m ) )
                       ! ==== a bulge may collapse because of vigilant
                       ! .    deflation or destructive underflow.  in the
                       ! .    underflow case, try the two-small-subdiagonals
                       ! .    trick to try to reinflate the bulge.  ====
                       if( h( k+3, k )/=czero .or. h( k+3, k+1 )/=czero .or. h( k+3, k+2 )==czero &
                                 ) then
                          ! ==== typical case: not collapsed (yet). ====
                          h( k+1, k ) = beta
                          h( k+2, k ) = czero
                          h( k+3, k ) = czero
                       else
                          ! ==== atypical case: collapsed.  attempt to
                          ! .    reintroduce ignoring h(k+1,k) and h(k+2,k).
                          ! .    if the fill resulting from the new
                          ! .    reflector is too large, then abandon it.
                          ! .    otherwise, use the new cone. ====
                          call stdlib_zlaqr1( 3_ilp, h( k+1, k+1 ), ldh, s( 2_ilp*m-1 ),s( 2_ilp*m ), vt )
                                    
                          alpha = vt( 1_ilp )
                          call stdlib_zlarfg( 3_ilp, alpha, vt( 2_ilp ), 1_ilp, vt( 1_ilp ) )
                          refsum = conjg( vt( 1_ilp ) )*( h( k+1, k )+conjg( vt( 2_ilp ) )*h( k+2, k ) )
                                    
                          if( cabs1( h( k+2, k )-refsum*vt( 2_ilp ) )+cabs1( refsum*vt( 3_ilp ) )>ulp*( &
                          cabs1( h( k, k ) )+cabs1( h( k+1,k+1 ) )+cabs1( h( k+2, k+2 ) ) ) ) &
                                    then
                             ! ==== starting a new bulge here would
                             ! .    create non-negligible fill.  use
                             ! .    the old cone with trepidation. ====
                             h( k+1, k ) = beta
                             h( k+2, k ) = czero
                             h( k+3, k ) = czero
                          else
                             ! ==== starting a new bulge here would
                             ! .    create only negligible fill.
                             ! .    replace the old reflector with
                             ! .    the new cone. ====
                             h( k+1, k ) = h( k+1, k ) - refsum
                             h( k+2, k ) = czero
                             h( k+3, k ) = czero
                             v( 1_ilp, m ) = vt( 1_ilp )
                             v( 2_ilp, m ) = vt( 2_ilp )
                             v( 3_ilp, m ) = vt( 3_ilp )
                          end if
                       end if
                    end if
                    ! ====  apply reflection from the right and
                    ! .     the first column of update from the left.
                    ! .     these updates are required for the vigilant
                    ! .     deflation check. we still delay most of the
                    ! .     updates from the left for efficiency. ====
                    do j = jtop, min( kbot, k+3 )
                       refsum = v( 1_ilp, m )*( h( j, k+1 )+v( 2_ilp, m )*h( j, k+2 )+v( 3_ilp, m )*h( j, k+3 &
                                 ) )
                       h( j, k+1 ) = h( j, k+1 ) - refsum
                       h( j, k+2 ) = h( j, k+2 ) -refsum*conjg( v( 2_ilp, m ) )
                       h( j, k+3 ) = h( j, k+3 ) -refsum*conjg( v( 3_ilp, m ) )
                    end do
                    ! ==== perform update from left for subsequent
                    ! .    column. ====
                    refsum =  conjg( v( 1_ilp, m ) )*( h( k+1, k+1 )+conjg( v( 2_ilp, m ) )*h( k+2, k+1 )+&
                              conjg( v( 3_ilp, m ) )*h( k+3, k+1 ) )
                    h( k+1, k+1 ) = h( k+1, k+1 ) - refsum
                    h( k+2, k+1 ) = h( k+2, k+1 ) - refsum*v( 2_ilp, m )
                    h( k+3, k+1 ) = h( k+3, k+1 ) - refsum*v( 3_ilp, m )
                    ! ==== the following convergence test requires that
                    ! .    the tradition small-compared-to-nearby-diagonals
                    ! .    criterion and the ahues
                    ! .    criteria both be satisfied.  the latter improves
                    ! .    accuracy in some examples. falling back on an
                    ! .    alternate convergence criterion when tst1 or tst2
                    ! .    is czero (as done here) is traditional but probably
                    ! .    unnecessary. ====
                    if( k<ktop)cycle
                    if( h( k+1, k )/=czero ) then
                       tst1 = cabs1( h( k, k ) ) + cabs1( h( k+1, k+1 ) )
                       if( tst1==zero ) then
                          if( k>=ktop+1 )tst1 = tst1 + cabs1( h( k, k-1 ) )
                          if( k>=ktop+2 )tst1 = tst1 + cabs1( h( k, k-2 ) )
                          if( k>=ktop+3 )tst1 = tst1 + cabs1( h( k, k-3 ) )
                          if( k<=kbot-2 )tst1 = tst1 + cabs1( h( k+2, k+1 ) )
                          if( k<=kbot-3 )tst1 = tst1 + cabs1( h( k+3, k+1 ) )
                          if( k<=kbot-4 )tst1 = tst1 + cabs1( h( k+4, k+1 ) )
                       end if
                       if( cabs1( h( k+1, k ) )<=max( smlnum, ulp*tst1 ) )then
                          h12 = max( cabs1( h( k+1, k ) ),cabs1( h( k, k+1 ) ) )
                          h21 = min( cabs1( h( k+1, k ) ),cabs1( h( k, k+1 ) ) )
                          h11 = max( cabs1( h( k+1, k+1 ) ),cabs1( h( k, k )-h( k+1, k+1 ) ) )
                                    
                          h22 = min( cabs1( h( k+1, k+1 ) ),cabs1( h( k, k )-h( k+1, k+1 ) ) )
                                    
                          scl = h11 + h12
                          tst2 = h22*( h11 / scl )
                          if( tst2==zero .or. h21*( h12 / scl )<=max( smlnum, ulp*tst2 ) )h( k+1,&
                                     k ) = czero
                       end if
                    end if
                 end do loop_80
                 ! ==== multiply h by reflections from the left ====
                 if( accum ) then
                    jbot = min( ndcol, kbot )
                 else if( wantt ) then
                    jbot = n
                 else
                    jbot = kbot
                 end if
                 do m = mbot, mtop, -1
                    k = krcol + 2_ilp*( m-1 )
                    do j = max( ktop, krcol + 2*m ), jbot
                       refsum = conjg( v( 1_ilp, m ) )*( h( k+1, j )+conjg( v( 2_ilp, m ) )*h( k+2, j )+&
                                 conjg( v( 3_ilp, m ) )*h( k+3, j ) )
                       h( k+1, j ) = h( k+1, j ) - refsum
                       h( k+2, j ) = h( k+2, j ) - refsum*v( 2_ilp, m )
                       h( k+3, j ) = h( k+3, j ) - refsum*v( 3_ilp, m )
                    end do
                 end do
                 ! ==== accumulate orthogonal transformations. ====
                 if( accum ) then
                    ! ==== accumulate u. (if needed, update z later
                    ! .    with an efficient matrix-matrix
                    ! .    multiply.) ====
                    do m = mbot, mtop, -1
                       k = krcol + 2_ilp*( m-1 )
                       kms = k - incol
                       i2 = max( 1_ilp, ktop-incol )
                       i2 = max( i2, kms-(krcol-incol)+1_ilp )
                       i4 = min( kdu, krcol + 2_ilp*( mbot-1 ) - incol + 5_ilp )
                       do j = i2, i4
                          refsum = v( 1_ilp, m )*( u( j, kms+1 )+v( 2_ilp, m )*u( j, kms+2 )+v( 3_ilp, m )*u( &
                                    j, kms+3 ) )
                          u( j, kms+1 ) = u( j, kms+1 ) - refsum
                          u( j, kms+2 ) = u( j, kms+2 ) -refsum*conjg( v( 2_ilp, m ) )
                          u( j, kms+3 ) = u( j, kms+3 ) -refsum*conjg( v( 3_ilp, m ) )
                       end do
                    end do
                 else if( wantz ) then
                    ! ==== u is not accumulated, so update z
                    ! .    now by multiplying by reflections
                    ! .    from the right. ====
                    do m = mbot, mtop, -1
                       k = krcol + 2_ilp*( m-1 )
                       do j = iloz, ihiz
                          refsum = v( 1_ilp, m )*( z( j, k+1 )+v( 2_ilp, m )*z( j, k+2 )+v( 3_ilp, m )*z( j, &
                                    k+3 ) )
                          z( j, k+1 ) = z( j, k+1 ) - refsum
                          z( j, k+2 ) = z( j, k+2 ) -refsum*conjg( v( 2_ilp, m ) )
                          z( j, k+3 ) = z( j, k+3 ) -refsum*conjg( v( 3_ilp, m ) )
                       end do
                    end do
                 end if
                 ! ==== end of near-the-diagonal bulge chase. ====
              end do loop_145
              ! ==== use u (if accumulated) to update far-from-diagonal
              ! .    entries in h.  if required, use u to update z as
              ! .    well. ====
              if( accum ) then
                 if( wantt ) then
                    jtop = 1_ilp
                    jbot = n
                 else
                    jtop = ktop
                    jbot = kbot
                 end if
                 k1 = max( 1_ilp, ktop-incol )
                 nu = ( kdu-max( 0_ilp, ndcol-kbot ) ) - k1 + 1_ilp
                 ! ==== horizontal multiply ====
                 do jcol = min( ndcol, kbot ) + 1, jbot, nh
                    jlen = min( nh, jbot-jcol+1 )
                    call stdlib_zgemm( 'C', 'N', nu, jlen, nu, cone, u( k1, k1 ),ldu, h( incol+k1,&
                               jcol ), ldh, czero, wh,ldwh )
                    call stdlib_zlacpy( 'ALL', nu, jlen, wh, ldwh,h( incol+k1, jcol ), ldh )
                              
                 end do
                 ! ==== vertical multiply ====
                 do jrow = jtop, max( ktop, incol ) - 1, nv
                    jlen = min( nv, max( ktop, incol )-jrow )
                    call stdlib_zgemm( 'N', 'N', jlen, nu, nu, cone,h( jrow, incol+k1 ), ldh, u( &
                              k1, k1 ),ldu, czero, wv, ldwv )
                    call stdlib_zlacpy( 'ALL', jlen, nu, wv, ldwv,h( jrow, incol+k1 ), ldh )
                              
                 end do
                 ! ==== z multiply (also vertical) ====
                 if( wantz ) then
                    do jrow = iloz, ihiz, nv
                       jlen = min( nv, ihiz-jrow+1 )
                       call stdlib_zgemm( 'N', 'N', jlen, nu, nu, cone,z( jrow, incol+k1 ), ldz, &
                                 u( k1, k1 ),ldu, czero, wv, ldwv )
                       call stdlib_zlacpy( 'ALL', jlen, nu, wv, ldwv,z( jrow, incol+k1 ), ldz )
                                 
                    end do
                 end if
              end if
           end do loop_180
     end subroutine stdlib_zlaqr5




     recursive module subroutine stdlib_slaqz0( wants, wantq, wantz, n, ilo, ihi, a,lda, b, ldb, alphar, &
     !! SLAQZ0 computes the eigenvalues of a real matrix pair (H,T),
     !! where H is an upper Hessenberg matrix and T is upper triangular,
     !! using the double-shift QZ method.
     !! Matrix pairs of this type are produced by the reduction to
     !! generalized upper Hessenberg form of a real matrix pair (A,B):
     !! A = Q1*H*Z1**T,  B = Q1*T*Z1**T,
     !! as computed by SGGHRD.
     !! If JOB='S', then the Hessenberg-triangular pair (H,T) is
     !! also reduced to generalized Schur form,
     !! H = Q*S*Z**T,  T = Q*P*Z**T,
     !! where Q and Z are orthogonal matrices, P is an upper triangular
     !! matrix, and S is a quasi-triangular matrix with 1-by-1 and 2-by-2
     !! diagonal blocks.
     !! The 1-by-1 blocks correspond to real eigenvalues of the matrix pair
     !! (H,T) and the 2-by-2 blocks correspond to complex conjugate pairs of
     !! eigenvalues.
     !! Additionally, the 2-by-2 upper triangular diagonal blocks of P
     !! corresponding to 2-by-2 blocks of S are reduced to positive diagonal
     !! form, i.e., if S(j+1,j) is non-zero, then P(j+1,j) = P(j,j+1) = 0,
     !! P(j,j) > 0, and P(j+1,j+1) > 0.
     !! Optionally, the orthogonal matrix Q from the generalized Schur
     !! factorization may be postmultiplied into an input matrix Q1, and the
     !! orthogonal matrix Z may be postmultiplied into an input matrix Z1.
     !! If Q1 and Z1 are the orthogonal matrices from SGGHRD that reduced
     !! the matrix pair (A,B) to generalized upper Hessenberg form, then the
     !! output matrices Q1*Q and Z1*Z are the orthogonal factors from the
     !! generalized Schur factorization of (A,B):
     !! A = (Q1*Q)*S*(Z1*Z)**T,  B = (Q1*Q)*P*(Z1*Z)**T.
     !! To avoid overflow, eigenvalues of the matrix pair (H,T) (equivalently,
     !! of (A,B)) are computed as a pair of values (alpha,beta), where alpha is
     !! complex and beta real.
     !! If beta is nonzero, lambda = alpha / beta is an eigenvalue of the
     !! generalized nonsymmetric eigenvalue problem (GNEP)
     !! A*x = lambda*B*x
     !! and if alpha is nonzero, mu = beta / alpha is an eigenvalue of the
     !! alternate form of the GNEP
     !! mu*A*y = B*y.
     !! Real eigenvalues can be read directly from the generalized Schur
     !! form:
     !! alpha = S(i,i), beta = P(i,i).
     !! Ref: C.B. Moler
     !! Eigenvalue Problems", SIAM J. Numer. Anal., 10(1973),
     !! pp. 241--256.
     !! Ref: B. Kagstrom, D. Kressner, "Multishift Variants of the QZ
     !! Algorithm with Aggressive Early Deflation", SIAM J. Numer.
     !! Anal., 29(2006), pp. 199--227.
     !! Ref: T. Steel, D. Camps, K. Meerbergen, R. Vandebril "A multishift,
     !! multipole rational QZ method with agressive early deflation"
               alphai, beta,q, ldq, z, ldz, work, lwork, rec,info )
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! arguments
           character, intent( in ) :: wants, wantq, wantz
           integer(ilp), intent( in ) :: n, ilo, ihi, lda, ldb, ldq, ldz, lwork,rec
           integer(ilp), intent( out ) :: info
           real(sp), intent( inout ) :: a( lda, * ), b( ldb, * ), q( ldq, * ),z( ldz, * ), alphar(&
                      * ), alphai( * ), beta( * ), work( * )
           ! ================================================================
           ! local scalars
           real(sp) :: smlnum, ulp, eshift, safmin, safmax, c1, s1, temp, swap
           integer(ilp) :: istart, istop, iiter, maxit, istart2, k, ld, nshifts, nblock, nw, nmin,&
            nibble, n_undeflated, n_deflated, ns, sweep_info, shiftpos, lworkreq, k2, istartm, &
            istopm, iwants, iwantq, iwantz, norm_info, aed_info, nwr, nbr, nsr, itemp1, itemp2, &
                      rcost, i
           logical(lk) :: ilschur, ilq, ilz
           character(len=3_ilp) :: jbcmpz
           if( stdlib_lsame( wants, 'E' ) ) then
              ilschur = .false.
              iwants = 1_ilp
           else if( stdlib_lsame( wants, 'S' ) ) then
              ilschur = .true.
              iwants = 2_ilp
           else
              iwants = 0_ilp
           end if
           if( stdlib_lsame( wantq, 'N' ) ) then
              ilq = .false.
              iwantq = 1_ilp
           else if( stdlib_lsame( wantq, 'V' ) ) then
              ilq = .true.
              iwantq = 2_ilp
           else if( stdlib_lsame( wantq, 'I' ) ) then
              ilq = .true.
              iwantq = 3_ilp
           else
              iwantq = 0_ilp
           end if
           if( stdlib_lsame( wantz, 'N' ) ) then
              ilz = .false.
              iwantz = 1_ilp
           else if( stdlib_lsame( wantz, 'V' ) ) then
              ilz = .true.
              iwantz = 2_ilp
           else if( stdlib_lsame( wantz, 'I' ) ) then
              ilz = .true.
              iwantz = 3_ilp
           else
              iwantz = 0_ilp
           end if
           ! check argument values
           info = 0_ilp
           if( iwants==0_ilp ) then
              info = -1_ilp
           else if( iwantq==0_ilp ) then
              info = -2_ilp
           else if( iwantz==0_ilp ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( ilo<1_ilp ) then
              info = -5_ilp
           else if( ihi>n .or. ihi<ilo-1 ) then
              info = -6_ilp
           else if( lda<n ) then
              info = -8_ilp
           else if( ldb<n ) then
              info = -10_ilp
           else if( ldq<1_ilp .or. ( ilq .and. ldq<n ) ) then
              info = -15_ilp
           else if( ldz<1_ilp .or. ( ilz .and. ldz<n ) ) then
              info = -17_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SLAQZ0', -info )
              return
           end if
           ! quick return if possible
           if( n<=0_ilp ) then
              work( 1_ilp ) = real( 1_ilp,KIND=sp)
              return
           end if
           ! get the parameters
           jbcmpz( 1_ilp:1_ilp ) = wants
           jbcmpz( 2_ilp:2_ilp ) = wantq
           jbcmpz( 3_ilp:3_ilp ) = wantz
           nmin = stdlib_ilaenv( 12_ilp, 'SLAQZ0', jbcmpz, n, ilo, ihi, lwork )
           nwr = stdlib_ilaenv( 13_ilp, 'SLAQZ0', jbcmpz, n, ilo, ihi, lwork )
           nwr = max( 2_ilp, nwr )
           nwr = min( ihi-ilo+1, ( n-1 ) / 3_ilp, nwr )
           nibble = stdlib_ilaenv( 14_ilp, 'SLAQZ0', jbcmpz, n, ilo, ihi, lwork )
           nsr = stdlib_ilaenv( 15_ilp, 'SLAQZ0', jbcmpz, n, ilo, ihi, lwork )
           nsr = min( nsr, ( n+6 ) / 9_ilp, ihi-ilo )
           nsr = max( 2_ilp, nsr-mod( nsr, 2_ilp ) )
           rcost = stdlib_ilaenv( 17_ilp, 'SLAQZ0', jbcmpz, n, ilo, ihi, lwork )
           itemp1 = int( nsr/sqrt( 1_ilp+2*nsr/( real( rcost,KIND=sp)/100_ilp*n ) ),KIND=ilp)
           itemp1 = ( ( itemp1-1 )/4_ilp )*4_ilp+4
           nbr = nsr+itemp1
           if( n < nmin .or. rec >= 2_ilp ) then
              call stdlib_shgeqz( wants, wantq, wantz, n, ilo, ihi, a, lda, b, ldb,alphar, alphai,&
                         beta, q, ldq, z, ldz, work,lwork, info )
              return
           end if
           ! find out required workspace
           ! workspace query to stdlib_slaqz3
           nw = max( nwr, nmin )
           call stdlib_slaqz3( ilschur, ilq, ilz, n, ilo, ihi, nw, a, lda, b, ldb,q, ldq, z, ldz, &
           n_undeflated, n_deflated, alphar,alphai, beta, work, nw, work, nw, work, -1_ilp, rec,&
                     aed_info )
           itemp1 = int( work( 1_ilp ),KIND=ilp)
           ! workspace query to stdlib_slaqz4
           call stdlib_slaqz4( ilschur, ilq, ilz, n, ilo, ihi, nsr, nbr, alphar,alphai, beta, a, &
                     lda, b, ldb, q, ldq, z, ldz, work,nbr, work, nbr, work, -1_ilp, sweep_info )
           itemp2 = int( work( 1_ilp ),KIND=ilp)
           lworkreq = max( itemp1+2*nw**2_ilp, itemp2+2*nbr**2_ilp )
           if ( lwork ==-1_ilp ) then
              work( 1_ilp ) = real( lworkreq,KIND=sp)
              return
           else if ( lwork < lworkreq ) then
              info = -19_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SLAQZ0', info )
              return
           end if
           ! initialize q and z
           if( iwantq==3_ilp ) call stdlib_slaset( 'FULL', n, n, zero, one, q, ldq )
           if( iwantz==3_ilp ) call stdlib_slaset( 'FULL', n, n, zero, one, z, ldz )
           ! get machine constants
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           safmax = one/safmin
           call stdlib_slabad( safmin, safmax )
           ulp = stdlib_slamch( 'PRECISION' )
           smlnum = safmin*( real( n,KIND=sp)/ulp )
           istart = ilo
           istop = ihi
           maxit = 3_ilp*( ihi-ilo+1 )
           ld = 0_ilp
           do iiter = 1, maxit
              if( iiter >= maxit ) then
                 info = istop+1
                 goto 80
              end if
              if ( istart+1 >= istop ) then
                 istop = istart
                 exit
              end if
              ! check deflations at the end
              if ( abs( a( istop-1, istop-2 ) ) <= max( smlnum,ulp*( abs( a( istop-1, istop-1 ) )+&
                        abs( a( istop-2,istop-2 ) ) ) ) ) then
                 a( istop-1, istop-2 ) = zero
                 istop = istop-2
                 ld = 0_ilp
                 eshift = zero
              else if ( abs( a( istop, istop-1 ) ) <= max( smlnum,ulp*( abs( a( istop, istop ) )+&
                        abs( a( istop-1,istop-1 ) ) ) ) ) then
                 a( istop, istop-1 ) = zero
                 istop = istop-1
                 ld = 0_ilp
                 eshift = zero
              end if
              ! check deflations at the start
              if ( abs( a( istart+2, istart+1 ) ) <= max( smlnum,ulp*( abs( a( istart+1, istart+1 &
                        ) )+abs( a( istart+2,istart+2 ) ) ) ) ) then
                 a( istart+2, istart+1 ) = zero
                 istart = istart+2
                 ld = 0_ilp
                 eshift = zero
              else if ( abs( a( istart+1, istart ) ) <= max( smlnum,ulp*( abs( a( istart, istart )&
                         )+abs( a( istart+1,istart+1 ) ) ) ) ) then
                 a( istart+1, istart ) = zero
                 istart = istart+1
                 ld = 0_ilp
                 eshift = zero
              end if
              if ( istart+1 >= istop ) then
                 exit
              end if
              ! check interior deflations
              istart2 = istart
              do k = istop, istart+1, -1
                 if ( abs( a( k, k-1 ) ) <= max( smlnum, ulp*( abs( a( k,k ) )+abs( a( k-1, k-1 ) &
                           ) ) ) ) then
                    a( k, k-1 ) = zero
                    istart2 = k
                    exit
                 end if
              end do
              ! get range to apply rotations to
              if ( ilschur ) then
                 istartm = 1_ilp
                 istopm = n
              else
                 istartm = istart2
                 istopm = istop
              end if
              ! check infinite eigenvalues, this is done without blocking so might
              ! slow down the method when many infinite eigenvalues are present
              k = istop
              do while ( k>=istart2 )
                 temp = zero
                 if( k < istop ) then
                    temp = temp+abs( b( k, k+1 ) )
                 end if
                 if( k > istart2 ) then
                    temp = temp+abs( b( k-1, k ) )
                 end if
                 if( abs( b( k, k ) ) < max( smlnum, ulp*temp ) ) then
                    ! a diagonal element of b is negligable, move it
                    ! to the top and deflate it
                    do k2 = k, istart2+1, -1
                       call stdlib_slartg( b( k2-1, k2 ), b( k2-1, k2-1 ), c1, s1,temp )
                       b( k2-1, k2 ) = temp
                       b( k2-1, k2-1 ) = zero
                       call stdlib_srot( k2-2-istartm+1, b( istartm, k2 ), 1_ilp,b( istartm, k2-1 ), &
                                 1_ilp, c1, s1 )
                       call stdlib_srot( min( k2+1, istop )-istartm+1, a( istartm,k2 ), 1_ilp, a( &
                                 istartm, k2-1 ), 1_ilp, c1, s1 )
                       if ( ilz ) then
                          call stdlib_srot( n, z( 1_ilp, k2 ), 1_ilp, z( 1_ilp, k2-1 ), 1_ilp, c1,s1 )
                       end if
                       if( k2<istop ) then
                          call stdlib_slartg( a( k2, k2-1 ), a( k2+1, k2-1 ), c1,s1, temp )
                                    
                          a( k2, k2-1 ) = temp
                          a( k2+1, k2-1 ) = zero
                          call stdlib_srot( istopm-k2+1, a( k2, k2 ), lda, a( k2+1,k2 ), lda, c1, &
                                    s1 )
                          call stdlib_srot( istopm-k2+1, b( k2, k2 ), ldb, b( k2+1,k2 ), ldb, c1, &
                                    s1 )
                          if( ilq ) then
                             call stdlib_srot( n, q( 1_ilp, k2 ), 1_ilp, q( 1_ilp, k2+1 ), 1_ilp,c1, s1 )
                          end if
                       end if
                    end do
                    if( istart2<istop )then
                       call stdlib_slartg( a( istart2, istart2 ), a( istart2+1,istart2 ), c1, s1, &
                                 temp )
                       a( istart2, istart2 ) = temp
                       a( istart2+1, istart2 ) = zero
                       call stdlib_srot( istopm-( istart2+1 )+1_ilp, a( istart2,istart2+1 ), lda, a( &
                                 istart2+1,istart2+1 ), lda, c1, s1 )
                       call stdlib_srot( istopm-( istart2+1 )+1_ilp, b( istart2,istart2+1 ), ldb, b( &
                                 istart2+1,istart2+1 ), ldb, c1, s1 )
                       if( ilq ) then
                          call stdlib_srot( n, q( 1_ilp, istart2 ), 1_ilp, q( 1_ilp,istart2+1 ), 1_ilp, c1, s1 )
                                    
                       end if
                    end if
                    istart2 = istart2+1
                 end if
                 k = k-1
              end do
              ! istart2 now points to the top of the bottom right
              ! unreduced hessenberg block
              if ( istart2 >= istop ) then
                 istop = istart2-1
                 ld = 0_ilp
                 eshift = zero
                 cycle
              end if
              nw = nwr
              nshifts = nsr
              nblock = nbr
              if ( istop-istart2+1 < nmin ) then
                 ! setting nw to the size of the subblock will make aed deflate
                 ! all the eigenvalues. this is slightly more efficient than just
                 ! using qz_small because the off diagonal part gets updated via blas.
                 if ( istop-istart+1 < nmin ) then
                    nw = istop-istart+1
                    istart2 = istart
                 else
                    nw = istop-istart2+1
                 end if
              end if
              ! time for aed
              call stdlib_slaqz3( ilschur, ilq, ilz, n, istart2, istop, nw, a, lda,b, ldb, q, ldq,&
               z, ldz, n_undeflated, n_deflated,alphar, alphai, beta, work, nw, work( nw**2_ilp+1 ),&
                         nw, work( 2_ilp*nw**2_ilp+1 ), lwork-2*nw**2_ilp, rec,aed_info )
              if ( n_deflated > 0_ilp ) then
                 istop = istop-n_deflated
                 ld = 0_ilp
                 eshift = zero
              end if
              if ( 100_ilp*n_deflated > nibble*( n_deflated+n_undeflated ) .or.istop-istart2+1 < nmin &
                        ) then
                 ! aed has uncovered many eigenvalues. skip a qz sweep and run
                 ! aed again.
                 cycle
              end if
              ld = ld+1
              ns = min( nshifts, istop-istart2 )
              ns = min( ns, n_undeflated )
              shiftpos = istop-n_deflated-n_undeflated+1
              ! shuffle shifts to put double shifts in front
              ! this ensures that we don't split up a double shift
              do i = shiftpos, shiftpos+n_undeflated-1, 2
                 if( alphai( i )/=-alphai( i+1 ) ) then
                    swap = alphar( i )
                    alphar( i ) = alphar( i+1 )
                    alphar( i+1 ) = alphar( i+2 )
                    alphar( i+2 ) = swap
                    swap = alphai( i )
                    alphai( i ) = alphai( i+1 )
                    alphai( i+1 ) = alphai( i+2 )
                    alphai( i+2 ) = swap
                    swap = beta( i )
                    beta( i ) = beta( i+1 )
                    beta( i+1 ) = beta( i+2 )
                    beta( i+2 ) = swap
                 end if
              end do
              if ( mod( ld, 6_ilp ) == 0_ilp ) then
                 ! exceptional shift.  chosen for no particularly good reason.
                 if( ( real( maxit,KIND=sp)*safmin )*abs( a( istop,istop-1 ) )<abs( a( istop-1, &
                           istop-1 ) ) ) then
                    eshift = a( istop, istop-1 )/b( istop-1, istop-1 )
                 else
                    eshift = eshift+one/( safmin*real( maxit,KIND=sp) )
                 end if
                 alphar( shiftpos ) = one
                 alphar( shiftpos+1 ) = zero
                 alphai( shiftpos ) = zero
                 alphai( shiftpos+1 ) = zero
                 beta( shiftpos ) = eshift
                 beta( shiftpos+1 ) = eshift
                 ns = 2_ilp
              end if
              ! time for a qz sweep
              call stdlib_slaqz4( ilschur, ilq, ilz, n, istart2, istop, ns, nblock,alphar( &
              shiftpos ), alphai( shiftpos ),beta( shiftpos ), a, lda, b, ldb, q, ldq, z, ldz,&
              work, nblock, work( nblock**2_ilp+1 ), nblock,work( 2_ilp*nblock**2_ilp+1 ), lwork-2*nblock**2_ilp,&
                        sweep_info )
           end do
           ! call stdlib_shgeqz to normalize the eigenvalue blocks and set the eigenvalues
           ! if all the eigenvalues have been found, stdlib_shgeqz will not do any iterations
           ! and only normalize the blocks. in case of a rare convergence failure,
           ! the single shift might perform better.
        80 continue 
           call stdlib_shgeqz( wants, wantq, wantz, n, ilo, ihi, a, lda, b, ldb,alphar, alphai, &
                  beta, q, ldq, z, ldz, work, lwork,norm_info )
           info = norm_info
     end subroutine stdlib_slaqz0

     recursive module subroutine stdlib_dlaqz0( wants, wantq, wantz, n, ilo, ihi, a,lda, b, ldb, alphar, &
     !! DLAQZ0 computes the eigenvalues of a real matrix pair (H,T),
     !! where H is an upper Hessenberg matrix and T is upper triangular,
     !! using the double-shift QZ method.
     !! Matrix pairs of this type are produced by the reduction to
     !! generalized upper Hessenberg form of a real matrix pair (A,B):
     !! A = Q1*H*Z1**T,  B = Q1*T*Z1**T,
     !! as computed by DGGHRD.
     !! If JOB='S', then the Hessenberg-triangular pair (H,T) is
     !! also reduced to generalized Schur form,
     !! H = Q*S*Z**T,  T = Q*P*Z**T,
     !! where Q and Z are orthogonal matrices, P is an upper triangular
     !! matrix, and S is a quasi-triangular matrix with 1-by-1 and 2-by-2
     !! diagonal blocks.
     !! The 1-by-1 blocks correspond to real eigenvalues of the matrix pair
     !! (H,T) and the 2-by-2 blocks correspond to complex conjugate pairs of
     !! eigenvalues.
     !! Additionally, the 2-by-2 upper triangular diagonal blocks of P
     !! corresponding to 2-by-2 blocks of S are reduced to positive diagonal
     !! form, i.e., if S(j+1,j) is non-zero, then P(j+1,j) = P(j,j+1) = 0,
     !! P(j,j) > 0, and P(j+1,j+1) > 0.
     !! Optionally, the orthogonal matrix Q from the generalized Schur
     !! factorization may be postmultiplied into an input matrix Q1, and the
     !! orthogonal matrix Z may be postmultiplied into an input matrix Z1.
     !! If Q1 and Z1 are the orthogonal matrices from DGGHRD that reduced
     !! the matrix pair (A,B) to generalized upper Hessenberg form, then the
     !! output matrices Q1*Q and Z1*Z are the orthogonal factors from the
     !! generalized Schur factorization of (A,B):
     !! A = (Q1*Q)*S*(Z1*Z)**T,  B = (Q1*Q)*P*(Z1*Z)**T.
     !! To avoid overflow, eigenvalues of the matrix pair (H,T) (equivalently,
     !! of (A,B)) are computed as a pair of values (alpha,beta), where alpha is
     !! complex and beta real.
     !! If beta is nonzero, lambda = alpha / beta is an eigenvalue of the
     !! generalized nonsymmetric eigenvalue problem (GNEP)
     !! A*x = lambda*B*x
     !! and if alpha is nonzero, mu = beta / alpha is an eigenvalue of the
     !! alternate form of the GNEP
     !! mu*A*y = B*y.
     !! Real eigenvalues can be read directly from the generalized Schur
     !! form:
     !! alpha = S(i,i), beta = P(i,i).
     !! Ref: C.B. Moler
     !! Eigenvalue Problems", SIAM J. Numer. Anal., 10(1973),
     !! pp. 241--256.
     !! Ref: B. Kagstrom, D. Kressner, "Multishift Variants of the QZ
     !! Algorithm with Aggressive Early Deflation", SIAM J. Numer.
     !! Anal., 29(2006), pp. 199--227.
     !! Ref: T. Steel, D. Camps, K. Meerbergen, R. Vandebril "A multishift,
     !! multipole rational QZ method with agressive early deflation"
               alphai, beta,q, ldq, z, ldz, work, lwork, rec,info )
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! arguments
           character, intent( in ) :: wants, wantq, wantz
           integer(ilp), intent( in ) :: n, ilo, ihi, lda, ldb, ldq, ldz, lwork,rec
           integer(ilp), intent( out ) :: info
           real(dp), intent( inout ) :: a( lda, * ), b( ldb, * ),q( ldq, * ), z( ldz, * ), alphar(&
                      * ),alphai( * ), beta( * ), work( * )
           ! ================================================================
           ! local scalars
           real(dp) :: smlnum, ulp, eshift, safmin, safmax, c1, s1, temp, swap
           integer(ilp) :: istart, istop, iiter, maxit, istart2, k, ld, nshifts, nblock, nw, nmin,&
            nibble, n_undeflated, n_deflated, ns, sweep_info, shiftpos, lworkreq, k2, istartm, &
            istopm, iwants, iwantq, iwantz, norm_info, aed_info, nwr, nbr, nsr, itemp1, itemp2, &
                      rcost, i
           logical(lk) :: ilschur, ilq, ilz
           character(len=3_ilp) :: jbcmpz
           if( stdlib_lsame( wants, 'E' ) ) then
              ilschur = .false.
              iwants = 1_ilp
           else if( stdlib_lsame( wants, 'S' ) ) then
              ilschur = .true.
              iwants = 2_ilp
           else
              iwants = 0_ilp
           end if
           if( stdlib_lsame( wantq, 'N' ) ) then
              ilq = .false.
              iwantq = 1_ilp
           else if( stdlib_lsame( wantq, 'V' ) ) then
              ilq = .true.
              iwantq = 2_ilp
           else if( stdlib_lsame( wantq, 'I' ) ) then
              ilq = .true.
              iwantq = 3_ilp
           else
              iwantq = 0_ilp
           end if
           if( stdlib_lsame( wantz, 'N' ) ) then
              ilz = .false.
              iwantz = 1_ilp
           else if( stdlib_lsame( wantz, 'V' ) ) then
              ilz = .true.
              iwantz = 2_ilp
           else if( stdlib_lsame( wantz, 'I' ) ) then
              ilz = .true.
              iwantz = 3_ilp
           else
              iwantz = 0_ilp
           end if
           ! check argument values
           info = 0_ilp
           if( iwants==0_ilp ) then
              info = -1_ilp
           else if( iwantq==0_ilp ) then
              info = -2_ilp
           else if( iwantz==0_ilp ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( ilo<1_ilp ) then
              info = -5_ilp
           else if( ihi>n .or. ihi<ilo-1 ) then
              info = -6_ilp
           else if( lda<n ) then
              info = -8_ilp
           else if( ldb<n ) then
              info = -10_ilp
           else if( ldq<1_ilp .or. ( ilq .and. ldq<n ) ) then
              info = -15_ilp
           else if( ldz<1_ilp .or. ( ilz .and. ldz<n ) ) then
              info = -17_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DLAQZ0', -info )
              return
           end if
           ! quick return if possible
           if( n<=0_ilp ) then
              work( 1_ilp ) = real( 1_ilp,KIND=dp)
              return
           end if
           ! get the parameters
           jbcmpz( 1_ilp:1_ilp ) = wants
           jbcmpz( 2_ilp:2_ilp ) = wantq
           jbcmpz( 3_ilp:3_ilp ) = wantz
           nmin = stdlib_ilaenv( 12_ilp, 'DLAQZ0', jbcmpz, n, ilo, ihi, lwork )
           nwr = stdlib_ilaenv( 13_ilp, 'DLAQZ0', jbcmpz, n, ilo, ihi, lwork )
           nwr = max( 2_ilp, nwr )
           nwr = min( ihi-ilo+1, ( n-1 ) / 3_ilp, nwr )
           nibble = stdlib_ilaenv( 14_ilp, 'DLAQZ0', jbcmpz, n, ilo, ihi, lwork )
           nsr = stdlib_ilaenv( 15_ilp, 'DLAQZ0', jbcmpz, n, ilo, ihi, lwork )
           nsr = min( nsr, ( n+6 ) / 9_ilp, ihi-ilo )
           nsr = max( 2_ilp, nsr-mod( nsr, 2_ilp ) )
           rcost = stdlib_ilaenv( 17_ilp, 'DLAQZ0', jbcmpz, n, ilo, ihi, lwork )
           itemp1 = int( nsr/sqrt( 1_ilp+2*nsr/( real( rcost,KIND=dp)/100_ilp*n ) ),KIND=ilp)
           itemp1 = ( ( itemp1-1 )/4_ilp )*4_ilp+4
           nbr = nsr+itemp1
           if( n < nmin .or. rec >= 2_ilp ) then
              call stdlib_dhgeqz( wants, wantq, wantz, n, ilo, ihi, a, lda, b, ldb,alphar, alphai,&
                         beta, q, ldq, z, ldz, work,lwork, info )
              return
           end if
           ! find out required workspace
           ! workspace query to stdlib_dlaqz3
           nw = max( nwr, nmin )
           call stdlib_dlaqz3( ilschur, ilq, ilz, n, ilo, ihi, nw, a, lda, b, ldb,q, ldq, z, ldz, &
           n_undeflated, n_deflated, alphar,alphai, beta, work, nw, work, nw, work, -1_ilp, rec,&
                     aed_info )
           itemp1 = int( work( 1_ilp ),KIND=ilp)
           ! workspace query to stdlib_dlaqz4
           call stdlib_dlaqz4( ilschur, ilq, ilz, n, ilo, ihi, nsr, nbr, alphar,alphai, beta, a, &
                     lda, b, ldb, q, ldq, z, ldz, work,nbr, work, nbr, work, -1_ilp, sweep_info )
           itemp2 = int( work( 1_ilp ),KIND=ilp)
           lworkreq = max( itemp1+2*nw**2_ilp, itemp2+2*nbr**2_ilp )
           if ( lwork ==-1_ilp ) then
              work( 1_ilp ) = real( lworkreq,KIND=dp)
              return
           else if ( lwork < lworkreq ) then
              info = -19_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DLAQZ0', info )
              return
           end if
           ! initialize q and z
           if( iwantq==3_ilp ) call stdlib_dlaset( 'FULL', n, n, zero, one, q, ldq )
           if( iwantz==3_ilp ) call stdlib_dlaset( 'FULL', n, n, zero, one, z, ldz )
           ! get machine constants
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           safmax = one/safmin
           call stdlib_dlabad( safmin, safmax )
           ulp = stdlib_dlamch( 'PRECISION' )
           smlnum = safmin*( real( n,KIND=dp)/ulp )
           istart = ilo
           istop = ihi
           maxit = 3_ilp*( ihi-ilo+1 )
           ld = 0_ilp
           do iiter = 1, maxit
              if( iiter >= maxit ) then
                 info = istop+1
                 goto 80
              end if
              if ( istart+1 >= istop ) then
                 istop = istart
                 exit
              end if
              ! check deflations at the end
              if ( abs( a( istop-1, istop-2 ) ) <= max( smlnum,ulp*( abs( a( istop-1, istop-1 ) )+&
                        abs( a( istop-2,istop-2 ) ) ) ) ) then
                 a( istop-1, istop-2 ) = zero
                 istop = istop-2
                 ld = 0_ilp
                 eshift = zero
              else if ( abs( a( istop, istop-1 ) ) <= max( smlnum,ulp*( abs( a( istop, istop ) )+&
                        abs( a( istop-1,istop-1 ) ) ) ) ) then
                 a( istop, istop-1 ) = zero
                 istop = istop-1
                 ld = 0_ilp
                 eshift = zero
              end if
              ! check deflations at the start
              if ( abs( a( istart+2, istart+1 ) ) <= max( smlnum,ulp*( abs( a( istart+1, istart+1 &
                        ) )+abs( a( istart+2,istart+2 ) ) ) ) ) then
                 a( istart+2, istart+1 ) = zero
                 istart = istart+2
                 ld = 0_ilp
                 eshift = zero
              else if ( abs( a( istart+1, istart ) ) <= max( smlnum,ulp*( abs( a( istart, istart )&
                         )+abs( a( istart+1,istart+1 ) ) ) ) ) then
                 a( istart+1, istart ) = zero
                 istart = istart+1
                 ld = 0_ilp
                 eshift = zero
              end if
              if ( istart+1 >= istop ) then
                 exit
              end if
              ! check interior deflations
              istart2 = istart
              do k = istop, istart+1, -1
                 if ( abs( a( k, k-1 ) ) <= max( smlnum, ulp*( abs( a( k,k ) )+abs( a( k-1, k-1 ) &
                           ) ) ) ) then
                    a( k, k-1 ) = zero
                    istart2 = k
                    exit
                 end if
              end do
              ! get range to apply rotations to
              if ( ilschur ) then
                 istartm = 1_ilp
                 istopm = n
              else
                 istartm = istart2
                 istopm = istop
              end if
              ! check infinite eigenvalues, this is done without blocking so might
              ! slow down the method when many infinite eigenvalues are present
              k = istop
              do while ( k>=istart2 )
                 temp = zero
                 if( k < istop ) then
                    temp = temp+abs( b( k, k+1 ) )
                 end if
                 if( k > istart2 ) then
                    temp = temp+abs( b( k-1, k ) )
                 end if
                 if( abs( b( k, k ) ) < max( smlnum, ulp*temp ) ) then
                    ! a diagonal element of b is negligable, move it
                    ! to the top and deflate it
                    do k2 = k, istart2+1, -1
                       call stdlib_dlartg( b( k2-1, k2 ), b( k2-1, k2-1 ), c1, s1,temp )
                       b( k2-1, k2 ) = temp
                       b( k2-1, k2-1 ) = zero
                       call stdlib_drot( k2-2-istartm+1, b( istartm, k2 ), 1_ilp,b( istartm, k2-1 ), &
                                 1_ilp, c1, s1 )
                       call stdlib_drot( min( k2+1, istop )-istartm+1, a( istartm,k2 ), 1_ilp, a( &
                                 istartm, k2-1 ), 1_ilp, c1, s1 )
                       if ( ilz ) then
                          call stdlib_drot( n, z( 1_ilp, k2 ), 1_ilp, z( 1_ilp, k2-1 ), 1_ilp, c1,s1 )
                       end if
                       if( k2<istop ) then
                          call stdlib_dlartg( a( k2, k2-1 ), a( k2+1, k2-1 ), c1,s1, temp )
                                    
                          a( k2, k2-1 ) = temp
                          a( k2+1, k2-1 ) = zero
                          call stdlib_drot( istopm-k2+1, a( k2, k2 ), lda, a( k2+1,k2 ), lda, c1, &
                                    s1 )
                          call stdlib_drot( istopm-k2+1, b( k2, k2 ), ldb, b( k2+1,k2 ), ldb, c1, &
                                    s1 )
                          if( ilq ) then
                             call stdlib_drot( n, q( 1_ilp, k2 ), 1_ilp, q( 1_ilp, k2+1 ), 1_ilp,c1, s1 )
                          end if
                       end if
                    end do
                    if( istart2<istop )then
                       call stdlib_dlartg( a( istart2, istart2 ), a( istart2+1,istart2 ), c1, s1, &
                                 temp )
                       a( istart2, istart2 ) = temp
                       a( istart2+1, istart2 ) = zero
                       call stdlib_drot( istopm-( istart2+1 )+1_ilp, a( istart2,istart2+1 ), lda, a( &
                                 istart2+1,istart2+1 ), lda, c1, s1 )
                       call stdlib_drot( istopm-( istart2+1 )+1_ilp, b( istart2,istart2+1 ), ldb, b( &
                                 istart2+1,istart2+1 ), ldb, c1, s1 )
                       if( ilq ) then
                          call stdlib_drot( n, q( 1_ilp, istart2 ), 1_ilp, q( 1_ilp,istart2+1 ), 1_ilp, c1, s1 )
                                    
                       end if
                    end if
                    istart2 = istart2+1
                 end if
                 k = k-1
              end do
              ! istart2 now points to the top of the bottom right
              ! unreduced hessenberg block
              if ( istart2 >= istop ) then
                 istop = istart2-1
                 ld = 0_ilp
                 eshift = zero
                 cycle
              end if
              nw = nwr
              nshifts = nsr
              nblock = nbr
              if ( istop-istart2+1 < nmin ) then
                 ! setting nw to the size of the subblock will make aed deflate
                 ! all the eigenvalues. this is slightly more efficient than just
                 ! using stdlib_dhgeqz because the off diagonal part gets updated via blas.
                 if ( istop-istart+1 < nmin ) then
                    nw = istop-istart+1
                    istart2 = istart
                 else
                    nw = istop-istart2+1
                 end if
              end if
              ! time for aed
              call stdlib_dlaqz3( ilschur, ilq, ilz, n, istart2, istop, nw, a, lda,b, ldb, q, ldq,&
               z, ldz, n_undeflated, n_deflated,alphar, alphai, beta, work, nw, work( nw**2_ilp+1 ),&
                         nw, work( 2_ilp*nw**2_ilp+1 ), lwork-2*nw**2_ilp, rec,aed_info )
              if ( n_deflated > 0_ilp ) then
                 istop = istop-n_deflated
                 ld = 0_ilp
                 eshift = zero
              end if
              if ( 100_ilp*n_deflated > nibble*( n_deflated+n_undeflated ) .or.istop-istart2+1 < nmin &
                        ) then
                 ! aed has uncovered many eigenvalues. skip a qz sweep and run
                 ! aed again.
                 cycle
              end if
              ld = ld+1
              ns = min( nshifts, istop-istart2 )
              ns = min( ns, n_undeflated )
              shiftpos = istop-n_deflated-n_undeflated+1
              ! shuffle shifts to put double shifts in front
              ! this ensures that we don't split up a double shift
              do i = shiftpos, shiftpos+n_undeflated-1, 2
                 if( alphai( i )/=-alphai( i+1 ) ) then
                    swap = alphar( i )
                    alphar( i ) = alphar( i+1 )
                    alphar( i+1 ) = alphar( i+2 )
                    alphar( i+2 ) = swap
                    swap = alphai( i )
                    alphai( i ) = alphai( i+1 )
                    alphai( i+1 ) = alphai( i+2 )
                    alphai( i+2 ) = swap
                    swap = beta( i )
                    beta( i ) = beta( i+1 )
                    beta( i+1 ) = beta( i+2 )
                    beta( i+2 ) = swap
                 end if
              end do
              if ( mod( ld, 6_ilp ) == 0_ilp ) then
                 ! exceptional shift.  chosen for no particularly good reason.
                 if( ( real( maxit,KIND=dp)*safmin )*abs( a( istop,istop-1 ) )<abs( a( istop-1, &
                           istop-1 ) ) ) then
                    eshift = a( istop, istop-1 )/b( istop-1, istop-1 )
                 else
                    eshift = eshift+one/( safmin*real( maxit,KIND=dp) )
                 end if
                 alphar( shiftpos ) = one
                 alphar( shiftpos+1 ) = zero
                 alphai( shiftpos ) = zero
                 alphai( shiftpos+1 ) = zero
                 beta( shiftpos ) = eshift
                 beta( shiftpos+1 ) = eshift
                 ns = 2_ilp
              end if
              ! time for a qz sweep
              call stdlib_dlaqz4( ilschur, ilq, ilz, n, istart2, istop, ns, nblock,alphar( &
              shiftpos ), alphai( shiftpos ),beta( shiftpos ), a, lda, b, ldb, q, ldq, z, ldz,&
              work, nblock, work( nblock**2_ilp+1 ), nblock,work( 2_ilp*nblock**2_ilp+1 ), lwork-2*nblock**2_ilp,&
                        sweep_info )
           end do
           ! call stdlib_dhgeqz to normalize the eigenvalue blocks and set the eigenvalues
           ! if all the eigenvalues have been found, stdlib_dhgeqz will not do any iterations
           ! and only normalize the blocks. in case of a rare convergence failure,
           ! the single shift might perform better.
        80 call stdlib_dhgeqz( wants, wantq, wantz, n, ilo, ihi, a, lda, b, ldb,alphar, alphai, &
                  beta, q, ldq, z, ldz, work, lwork,norm_info )
           info = norm_info
     end subroutine stdlib_dlaqz0


     recursive module subroutine stdlib_claqz0( wants, wantq, wantz, n, ilo, ihi, a,lda, b, ldb, alpha, &
     !! CLAQZ0 computes the eigenvalues of a matrix pair (H,T),
     !! where H is an upper Hessenberg matrix and T is upper triangular,
     !! using the double-shift QZ method.
     !! Matrix pairs of this type are produced by the reduction to
     !! generalized upper Hessenberg form of a matrix pair (A,B):
     !! A = Q1*H*Z1**H,  B = Q1*T*Z1**H,
     !! as computed by CGGHRD.
     !! If JOB='S', then the Hessenberg-triangular pair (H,T) is
     !! also reduced to generalized Schur form,
     !! H = Q*S*Z**H,  T = Q*P*Z**H,
     !! where Q and Z are unitary matrices, P and S are an upper triangular
     !! matrices.
     !! Optionally, the unitary matrix Q from the generalized Schur
     !! factorization may be postmultiplied into an input matrix Q1, and the
     !! unitary matrix Z may be postmultiplied into an input matrix Z1.
     !! If Q1 and Z1 are the unitary matrices from CGGHRD that reduced
     !! the matrix pair (A,B) to generalized upper Hessenberg form, then the
     !! output matrices Q1*Q and Z1*Z are the unitary factors from the
     !! generalized Schur factorization of (A,B):
     !! A = (Q1*Q)*S*(Z1*Z)**H,  B = (Q1*Q)*P*(Z1*Z)**H.
     !! To avoid overflow, eigenvalues of the matrix pair (H,T) (equivalently,
     !! of (A,B)) are computed as a pair of values (alpha,beta), where alpha is
     !! complex and beta real.
     !! If beta is nonzero, lambda = alpha / beta is an eigenvalue of the
     !! generalized nonsymmetric eigenvalue problem (GNEP)
     !! A*x = lambda*B*x
     !! and if alpha is nonzero, mu = beta / alpha is an eigenvalue of the
     !! alternate form of the GNEP
     !! mu*A*y = B*y.
     !! Eigenvalues can be read directly from the generalized Schur
     !! form:
     !! alpha = S(i,i), beta = P(i,i).
     !! Ref: C.B. Moler
     !! Eigenvalue Problems", SIAM J. Numer. Anal., 10(1973),
     !! pp. 241--256.
     !! Ref: B. Kagstrom, D. Kressner, "Multishift Variants of the QZ
     !! Algorithm with Aggressive Early Deflation", SIAM J. Numer.
     !! Anal., 29(2006), pp. 199--227.
     !! Ref: T. Steel, D. Camps, K. Meerbergen, R. Vandebril "A multishift,
     !! multipole rational QZ method with agressive early deflation"
               beta, q, ldq, z,ldz, work, lwork, rwork, rec,info )
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! arguments
           character, intent( in ) :: wants, wantq, wantz
           integer(ilp), intent( in ) :: n, ilo, ihi, lda, ldb, ldq, ldz, lwork,rec
           integer(ilp), intent( out ) :: info
           complex(sp), intent( inout ) :: a( lda, * ), b( ldb, * ), q( ldq, * ),z( ldz, * ), &
                     alpha( * ), beta( * ), work( * )
           real(sp), intent( out ) :: rwork( * )
           
           ! ================================================================
           ! local scalars
           real(sp) :: smlnum, ulp, safmin, safmax, c1, tempr
           complex(sp) :: eshift, s1, temp
           integer(ilp) :: istart, istop, iiter, maxit, istart2, k, ld, nshifts, nblock, nw, nmin,&
            nibble, n_undeflated, n_deflated, ns, sweep_info, shiftpos, lworkreq, k2, istartm, &
            istopm, iwants, iwantq, iwantz, norm_info, aed_info, nwr, nbr, nsr, itemp1, itemp2, &
                      rcost
           logical(lk) :: ilschur, ilq, ilz
           character(len=3) :: jbcmpz
           if( stdlib_lsame( wants, 'E' ) ) then
              ilschur = .false.
              iwants = 1_ilp
           else if( stdlib_lsame( wants, 'S' ) ) then
              ilschur = .true.
              iwants = 2_ilp
           else
              iwants = 0_ilp
           end if
           if( stdlib_lsame( wantq, 'N' ) ) then
              ilq = .false.
              iwantq = 1_ilp
           else if( stdlib_lsame( wantq, 'V' ) ) then
              ilq = .true.
              iwantq = 2_ilp
           else if( stdlib_lsame( wantq, 'I' ) ) then
              ilq = .true.
              iwantq = 3_ilp
           else
              iwantq = 0_ilp
           end if
           if( stdlib_lsame( wantz, 'N' ) ) then
              ilz = .false.
              iwantz = 1_ilp
           else if( stdlib_lsame( wantz, 'V' ) ) then
              ilz = .true.
              iwantz = 2_ilp
           else if( stdlib_lsame( wantz, 'I' ) ) then
              ilz = .true.
              iwantz = 3_ilp
           else
              iwantz = 0_ilp
           end if
           ! check argument values
           info = 0_ilp
           if( iwants==0_ilp ) then
              info = -1_ilp
           else if( iwantq==0_ilp ) then
              info = -2_ilp
           else if( iwantz==0_ilp ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( ilo<1_ilp ) then
              info = -5_ilp
           else if( ihi>n .or. ihi<ilo-1 ) then
              info = -6_ilp
           else if( lda<n ) then
              info = -8_ilp
           else if( ldb<n ) then
              info = -10_ilp
           else if( ldq<1_ilp .or. ( ilq .and. ldq<n ) ) then
              info = -15_ilp
           else if( ldz<1_ilp .or. ( ilz .and. ldz<n ) ) then
              info = -17_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CLAQZ0', -info )
              return
           end if
           ! quick return if possible
           if( n<=0_ilp ) then
              work( 1_ilp ) = real( 1_ilp,KIND=sp)
              return
           end if
           ! get the parameters
           jbcmpz( 1_ilp:1_ilp ) = wants
           jbcmpz( 2_ilp:2_ilp ) = wantq
           jbcmpz( 3_ilp:3_ilp ) = wantz
           nmin = stdlib_ilaenv( 12_ilp, 'CLAQZ0', jbcmpz, n, ilo, ihi, lwork )
           nwr = stdlib_ilaenv( 13_ilp, 'CLAQZ0', jbcmpz, n, ilo, ihi, lwork )
           nwr = max( 2_ilp, nwr )
           nwr = min( ihi-ilo+1, ( n-1 ) / 3_ilp, nwr )
           nibble = stdlib_ilaenv( 14_ilp, 'CLAQZ0', jbcmpz, n, ilo, ihi, lwork )
           nsr = stdlib_ilaenv( 15_ilp, 'CLAQZ0', jbcmpz, n, ilo, ihi, lwork )
           nsr = min( nsr, ( n+6 ) / 9_ilp, ihi-ilo )
           nsr = max( 2_ilp, nsr-mod( nsr, 2_ilp ) )
           rcost = stdlib_ilaenv( 17_ilp, 'CLAQZ0', jbcmpz, n, ilo, ihi, lwork )
           itemp1 = int( nsr/sqrt( 1_ilp+2*nsr/( real( rcost,KIND=sp)/100_ilp*n ) ),KIND=ilp)
           itemp1 = ( ( itemp1-1 )/4_ilp )*4_ilp+4
           nbr = nsr+itemp1
           if( n < nmin .or. rec >= 2_ilp ) then
              call stdlib_chgeqz( wants, wantq, wantz, n, ilo, ihi, a, lda, b, ldb,alpha, beta, q,&
                         ldq, z, ldz, work, lwork, rwork,info )
              return
           end if
           ! find out required workspace
           ! workspace query to stdlib_claqz2
           nw = max( nwr, nmin )
           call stdlib_claqz2( ilschur, ilq, ilz, n, ilo, ihi, nw, a, lda, b, ldb,q, ldq, z, ldz, &
           n_undeflated, n_deflated, alpha,beta, work, nw, work, nw, work, -1_ilp, rwork, rec,&
                     aed_info )
           itemp1 = int( work( 1_ilp ),KIND=ilp)
           ! workspace query to stdlib_claqz3
           call stdlib_claqz3( ilschur, ilq, ilz, n, ilo, ihi, nsr, nbr, alpha,beta, a, lda, b, &
                     ldb, q, ldq, z, ldz, work, nbr,work, nbr, work, -1_ilp, sweep_info )
           itemp2 = int( work( 1_ilp ),KIND=ilp)
           lworkreq = max( itemp1+2*nw**2_ilp, itemp2+2*nbr**2_ilp )
           if ( lwork ==-1_ilp ) then
              work( 1_ilp ) = real( lworkreq,KIND=sp)
              return
           else if ( lwork < lworkreq ) then
              info = -19_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CLAQZ0', info )
              return
           end if
           ! initialize q and z
           if( iwantq==3_ilp ) call stdlib_claset( 'FULL', n, n, czero, cone, q,ldq )
           if( iwantz==3_ilp ) call stdlib_claset( 'FULL', n, n, czero, cone, z,ldz )
           ! get machine constants
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           safmax = one/safmin
           call stdlib_slabad( safmin, safmax )
           ulp = stdlib_slamch( 'PRECISION' )
           smlnum = safmin*( real( n,KIND=sp)/ulp )
           istart = ilo
           istop = ihi
           maxit = 30_ilp*( ihi-ilo+1 )
           ld = 0_ilp
           do iiter = 1, maxit
              if( iiter >= maxit ) then
                 info = istop+1
                 goto 80
              end if
              if ( istart+1 >= istop ) then
                 istop = istart
                 exit
              end if
              ! check deflations at the end
              if ( abs( a( istop, istop-1 ) ) <= max( smlnum,ulp*( abs( a( istop, istop ) )+abs( &
                        a( istop-1,istop-1 ) ) ) ) ) then
                 a( istop, istop-1 ) = czero
                 istop = istop-1
                 ld = 0_ilp
                 eshift = czero
              end if
              ! check deflations at the start
              if ( abs( a( istart+1, istart ) ) <= max( smlnum,ulp*( abs( a( istart, istart ) )+&
                        abs( a( istart+1,istart+1 ) ) ) ) ) then
                 a( istart+1, istart ) = czero
                 istart = istart+1
                 ld = 0_ilp
                 eshift = czero
              end if
              if ( istart+1 >= istop ) then
                 exit
              end if
              ! check interior deflations
              istart2 = istart
              do k = istop, istart+1, -1
                 if ( abs( a( k, k-1 ) ) <= max( smlnum, ulp*( abs( a( k,k ) )+abs( a( k-1, k-1 ) &
                           ) ) ) ) then
                    a( k, k-1 ) = czero
                    istart2 = k
                    exit
                 end if
              end do
              ! get range to apply rotations to
              if ( ilschur ) then
                 istartm = 1_ilp
                 istopm = n
              else
                 istartm = istart2
                 istopm = istop
              end if
              ! check infinite eigenvalues, this is done without blocking so might
              ! slow down the method when many infinite eigenvalues are present
              k = istop
              do while ( k>=istart2 )
                 tempr = zero
                 if( k < istop ) then
                    tempr = tempr+abs( b( k, k+1 ) )
                 end if
                 if( k > istart2 ) then
                    tempr = tempr+abs( b( k-1, k ) )
                 end if
                 if( abs( b( k, k ) ) < max( smlnum, ulp*tempr ) ) then
                    ! a diagonal element of b is negligable, move it
                    ! to the top and deflate it
                    do k2 = k, istart2+1, -1
                       call stdlib_clartg( b( k2-1, k2 ), b( k2-1, k2-1 ), c1, s1,temp )
                       b( k2-1, k2 ) = temp
                       b( k2-1, k2-1 ) = czero
                       call stdlib_crot( k2-2-istartm+1, b( istartm, k2 ), 1_ilp,b( istartm, k2-1 ), &
                                 1_ilp, c1, s1 )
                       call stdlib_crot( min( k2+1, istop )-istartm+1, a( istartm,k2 ), 1_ilp, a( &
                                 istartm, k2-1 ), 1_ilp, c1, s1 )
                       if ( ilz ) then
                          call stdlib_crot( n, z( 1_ilp, k2 ), 1_ilp, z( 1_ilp, k2-1 ), 1_ilp, c1,s1 )
                       end if
                       if( k2<istop ) then
                          call stdlib_clartg( a( k2, k2-1 ), a( k2+1, k2-1 ), c1,s1, temp )
                                    
                          a( k2, k2-1 ) = temp
                          a( k2+1, k2-1 ) = czero
                          call stdlib_crot( istopm-k2+1, a( k2, k2 ), lda, a( k2+1,k2 ), lda, c1, &
                                    s1 )
                          call stdlib_crot( istopm-k2+1, b( k2, k2 ), ldb, b( k2+1,k2 ), ldb, c1, &
                                    s1 )
                          if( ilq ) then
                             call stdlib_crot( n, q( 1_ilp, k2 ), 1_ilp, q( 1_ilp, k2+1 ), 1_ilp,c1, conjg( s1 ) )
                                       
                          end if
                       end if
                    end do
                    if( istart2<istop )then
                       call stdlib_clartg( a( istart2, istart2 ), a( istart2+1,istart2 ), c1, s1, &
                                 temp )
                       a( istart2, istart2 ) = temp
                       a( istart2+1, istart2 ) = czero
                       call stdlib_crot( istopm-( istart2+1 )+1_ilp, a( istart2,istart2+1 ), lda, a( &
                                 istart2+1,istart2+1 ), lda, c1, s1 )
                       call stdlib_crot( istopm-( istart2+1 )+1_ilp, b( istart2,istart2+1 ), ldb, b( &
                                 istart2+1,istart2+1 ), ldb, c1, s1 )
                       if( ilq ) then
                          call stdlib_crot( n, q( 1_ilp, istart2 ), 1_ilp, q( 1_ilp,istart2+1 ), 1_ilp, c1, conjg(&
                                     s1 ) )
                       end if
                    end if
                    istart2 = istart2+1
                 end if
                 k = k-1
              end do
              ! istart2 now points to the top of the bottom right
              ! unreduced hessenberg block
              if ( istart2 >= istop ) then
                 istop = istart2-1
                 ld = 0_ilp
                 eshift = czero
                 cycle
              end if
              nw = nwr
              nshifts = nsr
              nblock = nbr
              if ( istop-istart2+1 < nmin ) then
                 ! setting nw to the size of the subblock will make aed deflate
                 ! all the eigenvalues. this is slightly more efficient than just
                 ! using stdlib_chgeqz because the off diagonal part gets updated via blas.
                 if ( istop-istart+1 < nmin ) then
                    nw = istop-istart+1
                    istart2 = istart
                 else
                    nw = istop-istart2+1
                 end if
              end if
              ! time for aed
              call stdlib_claqz2( ilschur, ilq, ilz, n, istart2, istop, nw, a, lda,b, ldb, q, ldq,&
               z, ldz, n_undeflated, n_deflated,alpha, beta, work, nw, work( nw**2_ilp+1 ), nw,work( &
                         2_ilp*nw**2_ilp+1 ), lwork-2*nw**2_ilp, rwork, rec,aed_info )
              if ( n_deflated > 0_ilp ) then
                 istop = istop-n_deflated
                 ld = 0_ilp
                 eshift = czero
              end if
              if ( 100_ilp*n_deflated > nibble*( n_deflated+n_undeflated ) .or.istop-istart2+1 < nmin &
                        ) then
                 ! aed has uncovered many eigenvalues. skip a qz sweep and run
                 ! aed again.
                 cycle
              end if
              ld = ld+1
              ns = min( nshifts, istop-istart2 )
              ns = min( ns, n_undeflated )
              shiftpos = istop-n_deflated-n_undeflated+1
              if ( mod( ld, 6_ilp ) == 0_ilp ) then
                 ! exceptional shift.  chosen for no particularly good reason.
                 if( ( real( maxit,KIND=sp)*safmin )*abs( a( istop,istop-1 ) )<abs( a( istop-1, &
                           istop-1 ) ) ) then
                    eshift = a( istop, istop-1 )/b( istop-1, istop-1 )
                 else
                    eshift = eshift+cone/( safmin*real( maxit,KIND=sp) )
                 end if
                 alpha( shiftpos ) = cone
                 beta( shiftpos ) = eshift
                 ns = 1_ilp
              end if
              ! time for a qz sweep
              call stdlib_claqz3( ilschur, ilq, ilz, n, istart2, istop, ns, nblock,alpha( &
              shiftpos ), beta( shiftpos ), a, lda, b,ldb, q, ldq, z, ldz, work, nblock, work( &
                        nblock**2_ilp+1 ), nblock, work( 2_ilp*nblock**2_ilp+1 ),lwork-2*nblock**2_ilp, sweep_info )
           end do
           ! call stdlib_chgeqz to normalize the eigenvalue blocks and set the eigenvalues
           ! if all the eigenvalues have been found, stdlib_chgeqz will not do any iterations
           ! and only normalize the blocks. in case of a rare convergence failure,
           ! the single shift might perform better.
        80 call stdlib_chgeqz( wants, wantq, wantz, n, ilo, ihi, a, lda, b, ldb,alpha, beta, q, &
                  ldq, z, ldz, work, lwork, rwork,norm_info )
           info = norm_info
     end subroutine stdlib_claqz0

     recursive module subroutine stdlib_zlaqz0( wants, wantq, wantz, n, ilo, ihi, a,lda, b, ldb, alpha, &
     !! ZLAQZ0 computes the eigenvalues of a real matrix pair (H,T),
     !! where H is an upper Hessenberg matrix and T is upper triangular,
     !! using the double-shift QZ method.
     !! Matrix pairs of this type are produced by the reduction to
     !! generalized upper Hessenberg form of a real matrix pair (A,B):
     !! A = Q1*H*Z1**H,  B = Q1*T*Z1**H,
     !! as computed by ZGGHRD.
     !! If JOB='S', then the Hessenberg-triangular pair (H,T) is
     !! also reduced to generalized Schur form,
     !! H = Q*S*Z**H,  T = Q*P*Z**H,
     !! where Q and Z are unitary matrices, P and S are an upper triangular
     !! matrices.
     !! Optionally, the unitary matrix Q from the generalized Schur
     !! factorization may be postmultiplied into an input matrix Q1, and the
     !! unitary matrix Z may be postmultiplied into an input matrix Z1.
     !! If Q1 and Z1 are the unitary matrices from ZGGHRD that reduced
     !! the matrix pair (A,B) to generalized upper Hessenberg form, then the
     !! output matrices Q1*Q and Z1*Z are the unitary factors from the
     !! generalized Schur factorization of (A,B):
     !! A = (Q1*Q)*S*(Z1*Z)**H,  B = (Q1*Q)*P*(Z1*Z)**H.
     !! To avoid overflow, eigenvalues of the matrix pair (H,T) (equivalently,
     !! of (A,B)) are computed as a pair of values (alpha,beta), where alpha is
     !! complex and beta real.
     !! If beta is nonzero, lambda = alpha / beta is an eigenvalue of the
     !! generalized nonsymmetric eigenvalue problem (GNEP)
     !! A*x = lambda*B*x
     !! and if alpha is nonzero, mu = beta / alpha is an eigenvalue of the
     !! alternate form of the GNEP
     !! mu*A*y = B*y.
     !! Eigenvalues can be read directly from the generalized Schur
     !! form:
     !! alpha = S(i,i), beta = P(i,i).
     !! Ref: C.B. Moler
     !! Eigenvalue Problems", SIAM J. Numer. Anal., 10(1973),
     !! pp. 241--256.
     !! Ref: B. Kagstrom, D. Kressner, "Multishift Variants of the QZ
     !! Algorithm with Aggressive Early Deflation", SIAM J. Numer.
     !! Anal., 29(2006), pp. 199--227.
     !! Ref: T. Steel, D. Camps, K. Meerbergen, R. Vandebril "A multishift,
     !! multipole rational QZ method with agressive early deflation"
               beta, q, ldq, z,ldz, work, lwork, rwork, rec,info )
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! arguments
           character, intent( in ) :: wants, wantq, wantz
           integer(ilp), intent( in ) :: n, ilo, ihi, lda, ldb, ldq, ldz, lwork,rec
           integer(ilp), intent( out ) :: info
           complex(dp), intent( inout ) :: a( lda, * ), b( ldb, * ), q( ldq,* ), z( ldz, * ), &
                     alpha( * ), beta( * ), work( * )
           real(dp), intent( out ) :: rwork( * )
           
           ! ================================================================
           ! local scalars
           real(dp) :: smlnum, ulp, safmin, safmax, c1, tempr
           complex(dp) :: eshift, s1, temp
           integer(ilp) :: istart, istop, iiter, maxit, istart2, k, ld, nshifts, nblock, nw, nmin,&
            nibble, n_undeflated, n_deflated, ns, sweep_info, shiftpos, lworkreq, k2, istartm, &
            istopm, iwants, iwantq, iwantz, norm_info, aed_info, nwr, nbr, nsr, itemp1, itemp2, &
                      rcost
           logical(lk) :: ilschur, ilq, ilz
           character(len=3) :: jbcmpz
           if( stdlib_lsame( wants, 'E' ) ) then
              ilschur = .false.
              iwants = 1_ilp
           else if( stdlib_lsame( wants, 'S' ) ) then
              ilschur = .true.
              iwants = 2_ilp
           else
              iwants = 0_ilp
           end if
           if( stdlib_lsame( wantq, 'N' ) ) then
              ilq = .false.
              iwantq = 1_ilp
           else if( stdlib_lsame( wantq, 'V' ) ) then
              ilq = .true.
              iwantq = 2_ilp
           else if( stdlib_lsame( wantq, 'I' ) ) then
              ilq = .true.
              iwantq = 3_ilp
           else
              iwantq = 0_ilp
           end if
           if( stdlib_lsame( wantz, 'N' ) ) then
              ilz = .false.
              iwantz = 1_ilp
           else if( stdlib_lsame( wantz, 'V' ) ) then
              ilz = .true.
              iwantz = 2_ilp
           else if( stdlib_lsame( wantz, 'I' ) ) then
              ilz = .true.
              iwantz = 3_ilp
           else
              iwantz = 0_ilp
           end if
           ! check argument values
           info = 0_ilp
           if( iwants==0_ilp ) then
              info = -1_ilp
           else if( iwantq==0_ilp ) then
              info = -2_ilp
           else if( iwantz==0_ilp ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( ilo<1_ilp ) then
              info = -5_ilp
           else if( ihi>n .or. ihi<ilo-1 ) then
              info = -6_ilp
           else if( lda<n ) then
              info = -8_ilp
           else if( ldb<n ) then
              info = -10_ilp
           else if( ldq<1_ilp .or. ( ilq .and. ldq<n ) ) then
              info = -15_ilp
           else if( ldz<1_ilp .or. ( ilz .and. ldz<n ) ) then
              info = -17_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZLAQZ0', -info )
              return
           end if
           ! quick return if possible
           if( n<=0_ilp ) then
              work( 1_ilp ) = real( 1_ilp,KIND=dp)
              return
           end if
           ! get the parameters
           jbcmpz( 1_ilp:1_ilp ) = wants
           jbcmpz( 2_ilp:2_ilp ) = wantq
           jbcmpz( 3_ilp:3_ilp ) = wantz
           nmin = stdlib_ilaenv( 12_ilp, 'ZLAQZ0', jbcmpz, n, ilo, ihi, lwork )
           nwr = stdlib_ilaenv( 13_ilp, 'ZLAQZ0', jbcmpz, n, ilo, ihi, lwork )
           nwr = max( 2_ilp, nwr )
           nwr = min( ihi-ilo+1, ( n-1 ) / 3_ilp, nwr )
           nibble = stdlib_ilaenv( 14_ilp, 'ZLAQZ0', jbcmpz, n, ilo, ihi, lwork )
           nsr = stdlib_ilaenv( 15_ilp, 'ZLAQZ0', jbcmpz, n, ilo, ihi, lwork )
           nsr = min( nsr, ( n+6 ) / 9_ilp, ihi-ilo )
           nsr = max( 2_ilp, nsr-mod( nsr, 2_ilp ) )
           rcost = stdlib_ilaenv( 17_ilp, 'ZLAQZ0', jbcmpz, n, ilo, ihi, lwork )
           itemp1 = int( nsr/sqrt( 1_ilp+2*nsr/( real( rcost,KIND=dp)/100_ilp*n ) ),KIND=ilp)
           itemp1 = ( ( itemp1-1 )/4_ilp )*4_ilp+4
           nbr = nsr+itemp1
           if( n < nmin .or. rec >= 2_ilp ) then
              call stdlib_zhgeqz( wants, wantq, wantz, n, ilo, ihi, a, lda, b, ldb,alpha, beta, q,&
                         ldq, z, ldz, work, lwork, rwork,info )
              return
           end if
           ! find out required workspace
           ! workspace query to stdlib_zlaqz2
           nw = max( nwr, nmin )
           call stdlib_zlaqz2( ilschur, ilq, ilz, n, ilo, ihi, nw, a, lda, b, ldb,q, ldq, z, ldz, &
           n_undeflated, n_deflated, alpha,beta, work, nw, work, nw, work, -1_ilp, rwork, rec,&
                     aed_info )
           itemp1 = int( work( 1_ilp ),KIND=ilp)
           ! workspace query to stdlib_zlaqz3
           call stdlib_zlaqz3( ilschur, ilq, ilz, n, ilo, ihi, nsr, nbr, alpha,beta, a, lda, b, &
                     ldb, q, ldq, z, ldz, work, nbr,work, nbr, work, -1_ilp, sweep_info )
           itemp2 = int( work( 1_ilp ),KIND=ilp)
           lworkreq = max( itemp1+2*nw**2_ilp, itemp2+2*nbr**2_ilp )
           if ( lwork ==-1_ilp ) then
              work( 1_ilp ) = real( lworkreq,KIND=dp)
              return
           else if ( lwork < lworkreq ) then
              info = -19_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZLAQZ0', info )
              return
           end if
           ! initialize q and z
           if( iwantq==3_ilp ) call stdlib_zlaset( 'FULL', n, n, czero, cone, q,ldq )
           if( iwantz==3_ilp ) call stdlib_zlaset( 'FULL', n, n, czero, cone, z,ldz )
           ! get machine constants
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           safmax = one/safmin
           call stdlib_dlabad( safmin, safmax )
           ulp = stdlib_dlamch( 'PRECISION' )
           smlnum = safmin*( real( n,KIND=dp)/ulp )
           istart = ilo
           istop = ihi
           maxit = 30_ilp*( ihi-ilo+1 )
           ld = 0_ilp
           do iiter = 1, maxit
              if( iiter >= maxit ) then
                 info = istop+1
                 goto 80
              end if
              if ( istart+1 >= istop ) then
                 istop = istart
                 exit
              end if
              ! check deflations at the end
              if ( abs( a( istop, istop-1 ) ) <= max( smlnum,ulp*( abs( a( istop, istop ) )+abs( &
                        a( istop-1,istop-1 ) ) ) ) ) then
                 a( istop, istop-1 ) = czero
                 istop = istop-1
                 ld = 0_ilp
                 eshift = czero
              end if
              ! check deflations at the start
              if ( abs( a( istart+1, istart ) ) <= max( smlnum,ulp*( abs( a( istart, istart ) )+&
                        abs( a( istart+1,istart+1 ) ) ) ) ) then
                 a( istart+1, istart ) = czero
                 istart = istart+1
                 ld = 0_ilp
                 eshift = czero
              end if
              if ( istart+1 >= istop ) then
                 exit
              end if
              ! check interior deflations
              istart2 = istart
              do k = istop, istart+1, -1
                 if ( abs( a( k, k-1 ) ) <= max( smlnum, ulp*( abs( a( k,k ) )+abs( a( k-1, k-1 ) &
                           ) ) ) ) then
                    a( k, k-1 ) = czero
                    istart2 = k
                    exit
                 end if
              end do
              ! get range to apply rotations to
              if ( ilschur ) then
                 istartm = 1_ilp
                 istopm = n
              else
                 istartm = istart2
                 istopm = istop
              end if
              ! check infinite eigenvalues, this is done without blocking so might
              ! slow down the method when many infinite eigenvalues are present
              k = istop
              do while ( k>=istart2 )
                 tempr = zero
                 if( k < istop ) then
                    tempr = tempr+abs( b( k, k+1 ) )
                 end if
                 if( k > istart2 ) then
                    tempr = tempr+abs( b( k-1, k ) )
                 end if
                 if( abs( b( k, k ) ) < max( smlnum, ulp*tempr ) ) then
                    ! a diagonal element of b is negligable, move it
                    ! to the top and deflate it
                    do k2 = k, istart2+1, -1
                       call stdlib_zlartg( b( k2-1, k2 ), b( k2-1, k2-1 ), c1, s1,temp )
                       b( k2-1, k2 ) = temp
                       b( k2-1, k2-1 ) = czero
                       call stdlib_zrot( k2-2-istartm+1, b( istartm, k2 ), 1_ilp,b( istartm, k2-1 ), &
                                 1_ilp, c1, s1 )
                       call stdlib_zrot( min( k2+1, istop )-istartm+1, a( istartm,k2 ), 1_ilp, a( &
                                 istartm, k2-1 ), 1_ilp, c1, s1 )
                       if ( ilz ) then
                          call stdlib_zrot( n, z( 1_ilp, k2 ), 1_ilp, z( 1_ilp, k2-1 ), 1_ilp, c1,s1 )
                       end if
                       if( k2<istop ) then
                          call stdlib_zlartg( a( k2, k2-1 ), a( k2+1, k2-1 ), c1,s1, temp )
                                    
                          a( k2, k2-1 ) = temp
                          a( k2+1, k2-1 ) = czero
                          call stdlib_zrot( istopm-k2+1, a( k2, k2 ), lda, a( k2+1,k2 ), lda, c1, &
                                    s1 )
                          call stdlib_zrot( istopm-k2+1, b( k2, k2 ), ldb, b( k2+1,k2 ), ldb, c1, &
                                    s1 )
                          if( ilq ) then
                             call stdlib_zrot( n, q( 1_ilp, k2 ), 1_ilp, q( 1_ilp, k2+1 ), 1_ilp,c1, conjg( s1 ) )
                                       
                          end if
                       end if
                    end do
                    if( istart2<istop )then
                       call stdlib_zlartg( a( istart2, istart2 ), a( istart2+1,istart2 ), c1, s1, &
                                 temp )
                       a( istart2, istart2 ) = temp
                       a( istart2+1, istart2 ) = czero
                       call stdlib_zrot( istopm-( istart2+1 )+1_ilp, a( istart2,istart2+1 ), lda, a( &
                                 istart2+1,istart2+1 ), lda, c1, s1 )
                       call stdlib_zrot( istopm-( istart2+1 )+1_ilp, b( istart2,istart2+1 ), ldb, b( &
                                 istart2+1,istart2+1 ), ldb, c1, s1 )
                       if( ilq ) then
                          call stdlib_zrot( n, q( 1_ilp, istart2 ), 1_ilp, q( 1_ilp,istart2+1 ), 1_ilp, c1, conjg(&
                                     s1 ) )
                       end if
                    end if
                    istart2 = istart2+1
                 end if
                 k = k-1
              end do
              ! istart2 now points to the top of the bottom right
              ! unreduced hessenberg block
              if ( istart2 >= istop ) then
                 istop = istart2-1
                 ld = 0_ilp
                 eshift = czero
                 cycle
              end if
              nw = nwr
              nshifts = nsr
              nblock = nbr
              if ( istop-istart2+1 < nmin ) then
                 ! setting nw to the size of the subblock will make aed deflate
                 ! all the eigenvalues. this is slightly more efficient than just
                 ! using qz_small because the off diagonal part gets updated via blas.
                 if ( istop-istart+1 < nmin ) then
                    nw = istop-istart+1
                    istart2 = istart
                 else
                    nw = istop-istart2+1
                 end if
              end if
              ! time for aed
              call stdlib_zlaqz2( ilschur, ilq, ilz, n, istart2, istop, nw, a, lda,b, ldb, q, ldq,&
               z, ldz, n_undeflated, n_deflated,alpha, beta, work, nw, work( nw**2_ilp+1 ), nw,work( &
                         2_ilp*nw**2_ilp+1 ), lwork-2*nw**2_ilp, rwork, rec,aed_info )
              if ( n_deflated > 0_ilp ) then
                 istop = istop-n_deflated
                 ld = 0_ilp
                 eshift = czero
              end if
              if ( 100_ilp*n_deflated > nibble*( n_deflated+n_undeflated ) .or.istop-istart2+1 < nmin &
                        ) then
                 ! aed has uncovered many eigenvalues. skip a qz sweep and run
                 ! aed again.
                 cycle
              end if
              ld = ld+1
              ns = min( nshifts, istop-istart2 )
              ns = min( ns, n_undeflated )
              shiftpos = istop-n_deflated-n_undeflated+1
              if ( mod( ld, 6_ilp ) == 0_ilp ) then
                 ! exceptional shift.  chosen for no particularly good reason.
                 if( ( real( maxit,KIND=dp)*safmin )*abs( a( istop,istop-1 ) )<abs( a( istop-1, &
                           istop-1 ) ) ) then
                    eshift = a( istop, istop-1 )/b( istop-1, istop-1 )
                 else
                    eshift = eshift+cone/( safmin*real( maxit,KIND=dp) )
                 end if
                 alpha( shiftpos ) = cone
                 beta( shiftpos ) = eshift
                 ns = 1_ilp
              end if
              ! time for a qz sweep
              call stdlib_zlaqz3( ilschur, ilq, ilz, n, istart2, istop, ns, nblock,alpha( &
              shiftpos ), beta( shiftpos ), a, lda, b,ldb, q, ldq, z, ldz, work, nblock, work( &
                        nblock**2_ilp+1 ), nblock, work( 2_ilp*nblock**2_ilp+1 ),lwork-2*nblock**2_ilp, sweep_info )
           end do
           ! call stdlib_zhgeqz to normalize the eigenvalue blocks and set the eigenvalues
           ! if all the eigenvalues have been found, stdlib_zhgeqz will not do any iterations
           ! and only normalize the blocks. in case of a rare convergence failure,
           ! the single shift might perform better.
        80 call stdlib_zhgeqz( wants, wantq, wantz, n, ilo, ihi, a, lda, b, ldb,alpha, beta, q, &
                  ldq, z, ldz, work, lwork, rwork,norm_info )
           info = norm_info
     end subroutine stdlib_zlaqz0




     pure module subroutine stdlib_slaqz1( a, lda, b, ldb, sr1, sr2, si, beta1, beta2,v )
     !! Given a 3-by-3 matrix pencil (A,B), SLAQZ1: sets v to a
     !! scalar multiple of the first column of the product
     !! (*)  K = (A - (beta2*sr2 - i*si)*B)*B^(-1)*(beta1*A - (sr2 + i*si2)*B)*B^(-1).
     !! It is assumed that either
     !! 1) sr1 = sr2
     !! or
     !! 2) si = 0.
     !! This is useful for starting double implicit shift bulges
     !! in the QZ algorithm.
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! arguments
           integer(ilp), intent( in ) :: lda, ldb
           real(sp), intent( in ) :: a( lda, * ), b( ldb, * ), sr1, sr2, si,beta1, beta2
           real(sp), intent( out ) :: v( * )
           ! ================================================================
           ! local scalars
           real(sp) :: w(2_ilp), safmin, safmax, scale1, scale2
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           safmax = one/safmin
           ! calculate first shifted vector
           w( 1_ilp ) = beta1*a( 1_ilp, 1_ilp )-sr1*b( 1_ilp, 1_ilp )
           w( 2_ilp ) = beta1*a( 2_ilp, 1_ilp )-sr1*b( 2_ilp, 1_ilp )
           scale1 = sqrt( abs( w( 1_ilp ) ) ) * sqrt( abs( w( 2_ilp ) ) )
           if( scale1 >= safmin .and. scale1 <= safmax ) then
              w( 1_ilp ) = w( 1_ilp )/scale1
              w( 2_ilp ) = w( 2_ilp )/scale1
           end if
           ! solve linear system
           w( 2_ilp ) = w( 2_ilp )/b( 2_ilp, 2_ilp )
           w( 1_ilp ) = ( w( 1_ilp )-b( 1_ilp, 2_ilp )*w( 2_ilp ) )/b( 1_ilp, 1_ilp )
           scale2 = sqrt( abs( w( 1_ilp ) ) ) * sqrt( abs( w( 2_ilp ) ) )
           if( scale2 >= safmin .and. scale2 <= safmax ) then
              w( 1_ilp ) = w( 1_ilp )/scale2
              w( 2_ilp ) = w( 2_ilp )/scale2
           end if
           ! apply second shift
           v( 1_ilp ) = beta2*( a( 1_ilp, 1_ilp )*w( 1_ilp )+a( 1_ilp, 2_ilp )*w( 2_ilp ) )-sr2*( b( 1_ilp,1_ilp )*w( 1_ilp )+b(&
               & 1_ilp, 2_ilp )*w(&
                      2_ilp ) )
           v( 2_ilp ) = beta2*( a( 2_ilp, 1_ilp )*w( 1_ilp )+a( 2_ilp, 2_ilp )*w( 2_ilp ) )-sr2*( b( 2_ilp,1_ilp )*w( 1_ilp )+b(&
               & 2_ilp, 2_ilp )*w(&
                      2_ilp ) )
           v( 3_ilp ) = beta2*( a( 3_ilp, 1_ilp )*w( 1_ilp )+a( 3_ilp, 2_ilp )*w( 2_ilp ) )-sr2*( b( 3_ilp,1_ilp )*w( 1_ilp )+b(&
               & 3_ilp, 2_ilp )*w(&
                      2_ilp ) )
           ! account for imaginary part
           v( 1_ilp ) = v( 1_ilp )+si*si*b( 1_ilp, 1_ilp )/scale1/scale2
           ! check for overflow
           if( abs( v( 1_ilp ) )>safmax .or. abs( v( 2_ilp ) ) > safmax .or.abs( v( 3_ilp ) )>safmax .or. &
           stdlib_sisnan( v( 1_ilp ) ) .or.stdlib_sisnan( v( 2_ilp ) ) .or. stdlib_sisnan( v( 3_ilp ) ) ) &
                     then
              v( 1_ilp ) = zero
              v( 2_ilp ) = zero
              v( 3_ilp ) = zero
           end if
     end subroutine stdlib_slaqz1

     pure module subroutine stdlib_dlaqz1( a, lda, b, ldb, sr1, sr2, si, beta1, beta2,v )
     !! Given a 3-by-3 matrix pencil (A,B), DLAQZ1: sets v to a
     !! scalar multiple of the first column of the product
     !! (*)  K = (A - (beta2*sr2 - i*si)*B)*B^(-1)*(beta1*A - (sr2 + i*si2)*B)*B^(-1).
     !! It is assumed that either
     !! 1) sr1 = sr2
     !! or
     !! 2) si = 0.
     !! This is useful for starting double implicit shift bulges
     !! in the QZ algorithm.
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! arguments
           integer(ilp), intent( in ) :: lda, ldb
           real(dp), intent( in ) :: a( lda, * ), b( ldb, * ), sr1,sr2, si, beta1, beta2
           real(dp), intent( out ) :: v( * )
           ! ================================================================
           ! local scalars
           real(dp) :: w(2_ilp), safmin, safmax, scale1, scale2
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           safmax = one/safmin
           ! calculate first shifted vector
           w( 1_ilp ) = beta1*a( 1_ilp, 1_ilp )-sr1*b( 1_ilp, 1_ilp )
           w( 2_ilp ) = beta1*a( 2_ilp, 1_ilp )-sr1*b( 2_ilp, 1_ilp )
           scale1 = sqrt( abs( w( 1_ilp ) ) ) * sqrt( abs( w( 2_ilp ) ) )
           if( scale1 >= safmin .and. scale1 <= safmax ) then
              w( 1_ilp ) = w( 1_ilp )/scale1
              w( 2_ilp ) = w( 2_ilp )/scale1
           end if
           ! solve linear system
           w( 2_ilp ) = w( 2_ilp )/b( 2_ilp, 2_ilp )
           w( 1_ilp ) = ( w( 1_ilp )-b( 1_ilp, 2_ilp )*w( 2_ilp ) )/b( 1_ilp, 1_ilp )
           scale2 = sqrt( abs( w( 1_ilp ) ) ) * sqrt( abs( w( 2_ilp ) ) )
           if( scale2 >= safmin .and. scale2 <= safmax ) then
              w( 1_ilp ) = w( 1_ilp )/scale2
              w( 2_ilp ) = w( 2_ilp )/scale2
           end if
           ! apply second shift
           v( 1_ilp ) = beta2*( a( 1_ilp, 1_ilp )*w( 1_ilp )+a( 1_ilp, 2_ilp )*w( 2_ilp ) )-sr2*( b( 1_ilp,1_ilp )*w( 1_ilp )+b(&
               & 1_ilp, 2_ilp )*w(&
                      2_ilp ) )
           v( 2_ilp ) = beta2*( a( 2_ilp, 1_ilp )*w( 1_ilp )+a( 2_ilp, 2_ilp )*w( 2_ilp ) )-sr2*( b( 2_ilp,1_ilp )*w( 1_ilp )+b(&
               & 2_ilp, 2_ilp )*w(&
                      2_ilp ) )
           v( 3_ilp ) = beta2*( a( 3_ilp, 1_ilp )*w( 1_ilp )+a( 3_ilp, 2_ilp )*w( 2_ilp ) )-sr2*( b( 3_ilp,1_ilp )*w( 1_ilp )+b(&
               & 3_ilp, 2_ilp )*w(&
                      2_ilp ) )
           ! account for imaginary part
           v( 1_ilp ) = v( 1_ilp )+si*si*b( 1_ilp, 1_ilp )/scale1/scale2
           ! check for overflow
           if( abs( v( 1_ilp ) )>safmax .or. abs( v( 2_ilp ) ) > safmax .or.abs( v( 3_ilp ) )>safmax .or. &
           stdlib_disnan( v( 1_ilp ) ) .or.stdlib_disnan( v( 2_ilp ) ) .or. stdlib_disnan( v( 3_ilp ) ) ) &
                     then
              v( 1_ilp ) = zero
              v( 2_ilp ) = zero
              v( 3_ilp ) = zero
           end if
     end subroutine stdlib_dlaqz1


     pure module subroutine stdlib_claqz1( ilq, ilz, k, istartm, istopm, ihi, a, lda, b,ldb, nq, qstart, &
     !! CLAQZ1 chases a 1x1 shift bulge in a matrix pencil down a single position
               q, ldq, nz, zstart, z, ldz )
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! arguments
           logical(lk), intent( in ) :: ilq, ilz
           integer(ilp), intent( in ) :: k, lda, ldb, ldq, ldz, istartm, istopm,nq, nz, qstart, &
                     zstart, ihi
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
           
           ! ================================================================
           ! local variables
           real(sp) :: c
           complex(sp) :: s, temp
           if( k+1 == ihi ) then
              ! shift is located on the edge of the matrix, remove it
              call stdlib_clartg( b( ihi, ihi ), b( ihi, ihi-1 ), c, s, temp )
              b( ihi, ihi ) = temp
              b( ihi, ihi-1 ) = czero
              call stdlib_crot( ihi-istartm, b( istartm, ihi ), 1_ilp, b( istartm,ihi-1 ), 1_ilp, c, s )
                        
              call stdlib_crot( ihi-istartm+1, a( istartm, ihi ), 1_ilp, a( istartm,ihi-1 ), 1_ilp, c, s )
                        
              if ( ilz ) then
                 call stdlib_crot( nz, z( 1_ilp, ihi-zstart+1 ), 1_ilp, z( 1_ilp, ihi-1-zstart+1 ), 1_ilp, c, s )
                           
              end if
           else
              ! normal operation, move bulge down
              ! apply transformation from the right
              call stdlib_clartg( b( k+1, k+1 ), b( k+1, k ), c, s, temp )
              b( k+1, k+1 ) = temp
              b( k+1, k ) = czero
              call stdlib_crot( k+2-istartm+1, a( istartm, k+1 ), 1_ilp, a( istartm,k ), 1_ilp, c, s )
                        
              call stdlib_crot( k-istartm+1, b( istartm, k+1 ), 1_ilp, b( istartm, k ),1_ilp, c, s )
                        
              if ( ilz ) then
                 call stdlib_crot( nz, z( 1_ilp, k+1-zstart+1 ), 1_ilp, z( 1_ilp, k-zstart+1 ),1_ilp, c, s )
                           
              end if
              ! apply transformation from the left
              call stdlib_clartg( a( k+1, k ), a( k+2, k ), c, s, temp )
              a( k+1, k ) = temp
              a( k+2, k ) = czero
              call stdlib_crot( istopm-k, a( k+1, k+1 ), lda, a( k+2, k+1 ), lda, c,s )
              call stdlib_crot( istopm-k, b( k+1, k+1 ), ldb, b( k+2, k+1 ), ldb, c,s )
              if ( ilq ) then
                 call stdlib_crot( nq, q( 1_ilp, k+1-qstart+1 ), 1_ilp, q( 1_ilp, k+2-qstart+1 ), 1_ilp, c, conjg(&
                            s ) )
              end if
           end if
     end subroutine stdlib_claqz1

     pure module subroutine stdlib_zlaqz1( ilq, ilz, k, istartm, istopm, ihi, a, lda, b,ldb, nq, qstart, &
     !! ZLAQZ1 chases a 1x1 shift bulge in a matrix pencil down a single position
               q, ldq, nz, zstart, z, ldz )
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! arguments
           logical(lk), intent( in ) :: ilq, ilz
           integer(ilp), intent( in ) :: k, lda, ldb, ldq, ldz, istartm, istopm,nq, nz, qstart, &
                     zstart, ihi
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
           
           ! ================================================================
           ! local variables
           real(dp) :: c
           complex(dp) :: s, temp
           if( k+1 == ihi ) then
              ! shift is located on the edge of the matrix, remove it
              call stdlib_zlartg( b( ihi, ihi ), b( ihi, ihi-1 ), c, s, temp )
              b( ihi, ihi ) = temp
              b( ihi, ihi-1 ) = czero
              call stdlib_zrot( ihi-istartm, b( istartm, ihi ), 1_ilp, b( istartm,ihi-1 ), 1_ilp, c, s )
                        
              call stdlib_zrot( ihi-istartm+1, a( istartm, ihi ), 1_ilp, a( istartm,ihi-1 ), 1_ilp, c, s )
                        
              if ( ilz ) then
                 call stdlib_zrot( nz, z( 1_ilp, ihi-zstart+1 ), 1_ilp, z( 1_ilp, ihi-1-zstart+1 ), 1_ilp, c, s )
                           
              end if
           else
              ! normal operation, move bulge down
              ! apply transformation from the right
              call stdlib_zlartg( b( k+1, k+1 ), b( k+1, k ), c, s, temp )
              b( k+1, k+1 ) = temp
              b( k+1, k ) = czero
              call stdlib_zrot( k+2-istartm+1, a( istartm, k+1 ), 1_ilp, a( istartm,k ), 1_ilp, c, s )
                        
              call stdlib_zrot( k-istartm+1, b( istartm, k+1 ), 1_ilp, b( istartm, k ),1_ilp, c, s )
                        
              if ( ilz ) then
                 call stdlib_zrot( nz, z( 1_ilp, k+1-zstart+1 ), 1_ilp, z( 1_ilp, k-zstart+1 ),1_ilp, c, s )
                           
              end if
              ! apply transformation from the left
              call stdlib_zlartg( a( k+1, k ), a( k+2, k ), c, s, temp )
              a( k+1, k ) = temp
              a( k+2, k ) = czero
              call stdlib_zrot( istopm-k, a( k+1, k+1 ), lda, a( k+2, k+1 ), lda, c,s )
              call stdlib_zrot( istopm-k, b( k+1, k+1 ), ldb, b( k+2, k+1 ), ldb, c,s )
              if ( ilq ) then
                 call stdlib_zrot( nq, q( 1_ilp, k+1-qstart+1 ), 1_ilp, q( 1_ilp, k+2-qstart+1 ), 1_ilp, c, conjg(&
                            s ) )
              end if
           end if
     end subroutine stdlib_zlaqz1




     pure module subroutine stdlib_slaqz2( ilq, ilz, k, istartm, istopm, ihi, a, lda, b,ldb, nq, qstart, &
     !! SLAQZ2 chases a 2x2 shift bulge in a matrix pencil down a single position
               q, ldq, nz, zstart, z, ldz )
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! arguments
           logical(lk), intent( in ) :: ilq, ilz
           integer(ilp), intent( in ) :: k, lda, ldb, ldq, ldz, istartm, istopm,nq, nz, qstart, &
                     zstart, ihi
           real(sp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
           ! ================================================================
           ! local variables
           real(sp) :: h(2_ilp,3_ilp), c1, s1, c2, s2, temp
           if( k+2 == ihi ) then
              ! shift is located on the edge of the matrix, remove it
              h = b( ihi-1:ihi, ihi-2:ihi )
              ! make h upper triangular
              call stdlib_slartg( h( 1_ilp, 1_ilp ), h( 2_ilp, 1_ilp ), c1, s1, temp )
              h( 2_ilp, 1_ilp ) = zero
              h( 1_ilp, 1_ilp ) = temp
              call stdlib_srot( 2_ilp, h( 1_ilp, 2_ilp ), 2_ilp, h( 2_ilp, 2_ilp ), 2_ilp, c1, s1 )
              call stdlib_slartg( h( 2_ilp, 3_ilp ), h( 2_ilp, 2_ilp ), c1, s1, temp )
              call stdlib_srot( 1_ilp, h( 1_ilp, 3_ilp ), 1_ilp, h( 1_ilp, 2_ilp ), 1_ilp, c1, s1 )
              call stdlib_slartg( h( 1_ilp, 2_ilp ), h( 1_ilp, 1_ilp ), c2, s2, temp )
              call stdlib_srot( ihi-istartm+1, b( istartm, ihi ), 1_ilp, b( istartm,ihi-1 ), 1_ilp, c1, &
                        s1 )
              call stdlib_srot( ihi-istartm+1, b( istartm, ihi-1 ), 1_ilp, b( istartm,ihi-2 ), 1_ilp, c2, &
                        s2 )
              b( ihi-1, ihi-2 ) = zero
              b( ihi, ihi-2 ) = zero
              call stdlib_srot( ihi-istartm+1, a( istartm, ihi ), 1_ilp, a( istartm,ihi-1 ), 1_ilp, c1, &
                        s1 )
              call stdlib_srot( ihi-istartm+1, a( istartm, ihi-1 ), 1_ilp, a( istartm,ihi-2 ), 1_ilp, c2, &
                        s2 )
              if ( ilz ) then
                 call stdlib_srot( nz, z( 1_ilp, ihi-zstart+1 ), 1_ilp, z( 1_ilp, ihi-1-zstart+1 ), 1_ilp, c1, s1 &
                           )
                 call stdlib_srot( nz, z( 1_ilp, ihi-1-zstart+1 ), 1_ilp, z( 1_ilp,ihi-2-zstart+1 ), 1_ilp, c2, &
                           s2 )
              end if
              call stdlib_slartg( a( ihi-1, ihi-2 ), a( ihi, ihi-2 ), c1, s1,temp )
              a( ihi-1, ihi-2 ) = temp
              a( ihi, ihi-2 ) = zero
              call stdlib_srot( istopm-ihi+2, a( ihi-1, ihi-1 ), lda, a( ihi,ihi-1 ), lda, c1, s1 &
                        )
              call stdlib_srot( istopm-ihi+2, b( ihi-1, ihi-1 ), ldb, b( ihi,ihi-1 ), ldb, c1, s1 &
                        )
              if ( ilq ) then
                 call stdlib_srot( nq, q( 1_ilp, ihi-1-qstart+1 ), 1_ilp, q( 1_ilp, ihi-qstart+1 ), 1_ilp, c1, s1 &
                           )
              end if
              call stdlib_slartg( b( ihi, ihi ), b( ihi, ihi-1 ), c1, s1, temp )
              b( ihi, ihi ) = temp
              b( ihi, ihi-1 ) = zero
              call stdlib_srot( ihi-istartm, b( istartm, ihi ), 1_ilp, b( istartm,ihi-1 ), 1_ilp, c1, s1 )
                        
              call stdlib_srot( ihi-istartm+1, a( istartm, ihi ), 1_ilp, a( istartm,ihi-1 ), 1_ilp, c1, &
                        s1 )
              if ( ilz ) then
                 call stdlib_srot( nz, z( 1_ilp, ihi-zstart+1 ), 1_ilp, z( 1_ilp, ihi-1-zstart+1 ), 1_ilp, c1, s1 &
                           )
              end if
           else
              ! normal operation, move bulge down
              h = b( k+1:k+2, k:k+2 )
              ! make h upper triangular
              call stdlib_slartg( h( 1_ilp, 1_ilp ), h( 2_ilp, 1_ilp ), c1, s1, temp )
              h( 2_ilp, 1_ilp ) = zero
              h( 1_ilp, 1_ilp ) = temp
              call stdlib_srot( 2_ilp, h( 1_ilp, 2_ilp ), 2_ilp, h( 2_ilp, 2_ilp ), 2_ilp, c1, s1 )
              ! calculate z1 and z2
              call stdlib_slartg( h( 2_ilp, 3_ilp ), h( 2_ilp, 2_ilp ), c1, s1, temp )
              call stdlib_srot( 1_ilp, h( 1_ilp, 3_ilp ), 1_ilp, h( 1_ilp, 2_ilp ), 1_ilp, c1, s1 )
              call stdlib_slartg( h( 1_ilp, 2_ilp ), h( 1_ilp, 1_ilp ), c2, s2, temp )
              ! apply transformations from the right
              call stdlib_srot( k+3-istartm+1, a( istartm, k+2 ), 1_ilp, a( istartm,k+1 ), 1_ilp, c1, s1 )
                        
              call stdlib_srot( k+3-istartm+1, a( istartm, k+1 ), 1_ilp, a( istartm,k ), 1_ilp, c2, s2 )
                        
              call stdlib_srot( k+2-istartm+1, b( istartm, k+2 ), 1_ilp, b( istartm,k+1 ), 1_ilp, c1, s1 )
                        
              call stdlib_srot( k+2-istartm+1, b( istartm, k+1 ), 1_ilp, b( istartm,k ), 1_ilp, c2, s2 )
                        
              if ( ilz ) then
                 call stdlib_srot( nz, z( 1_ilp, k+2-zstart+1 ), 1_ilp, z( 1_ilp, k+1-zstart+1 ), 1_ilp, c1, s1 )
                           
                 call stdlib_srot( nz, z( 1_ilp, k+1-zstart+1 ), 1_ilp, z( 1_ilp, k-zstart+1 ),1_ilp, c2, s2 )
                           
              end if
              b( k+1, k ) = zero
              b( k+2, k ) = zero
              ! calculate q1 and q2
              call stdlib_slartg( a( k+2, k ), a( k+3, k ), c1, s1, temp )
              a( k+2, k ) = temp
              a( k+3, k ) = zero
              call stdlib_slartg( a( k+1, k ), a( k+2, k ), c2, s2, temp )
              a( k+1, k ) = temp
              a( k+2, k ) = zero
           ! apply transformations from the left
              call stdlib_srot( istopm-k, a( k+2, k+1 ), lda, a( k+3, k+1 ), lda,c1, s1 )
              call stdlib_srot( istopm-k, a( k+1, k+1 ), lda, a( k+2, k+1 ), lda,c2, s2 )
              call stdlib_srot( istopm-k, b( k+2, k+1 ), ldb, b( k+3, k+1 ), ldb,c1, s1 )
              call stdlib_srot( istopm-k, b( k+1, k+1 ), ldb, b( k+2, k+1 ), ldb,c2, s2 )
              if ( ilq ) then
                 call stdlib_srot( nq, q( 1_ilp, k+2-qstart+1 ), 1_ilp, q( 1_ilp, k+3-qstart+1 ), 1_ilp, c1, s1 )
                           
                 call stdlib_srot( nq, q( 1_ilp, k+1-qstart+1 ), 1_ilp, q( 1_ilp, k+2-qstart+1 ), 1_ilp, c2, s2 )
                           
              end if
           end if
     end subroutine stdlib_slaqz2

     pure module subroutine stdlib_dlaqz2( ilq, ilz, k, istartm, istopm, ihi, a, lda, b,ldb, nq, qstart, &
     !! DLAQZ2 chases a 2x2 shift bulge in a matrix pencil down a single position
               q, ldq, nz, zstart, z, ldz )
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! arguments
           logical(lk), intent( in ) :: ilq, ilz
           integer(ilp), intent( in ) :: k, lda, ldb, ldq, ldz, istartm, istopm,nq, nz, qstart, &
                     zstart, ihi
           real(dp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
           ! ================================================================
           ! local variables
           real(dp) :: h(2_ilp,3_ilp), c1, s1, c2, s2, temp
           if( k+2 == ihi ) then
              ! shift is located on the edge of the matrix, remove it
              h = b( ihi-1:ihi, ihi-2:ihi )
              ! make h upper triangular
              call stdlib_dlartg( h( 1_ilp, 1_ilp ), h( 2_ilp, 1_ilp ), c1, s1, temp )
              h( 2_ilp, 1_ilp ) = zero
              h( 1_ilp, 1_ilp ) = temp
              call stdlib_drot( 2_ilp, h( 1_ilp, 2_ilp ), 2_ilp, h( 2_ilp, 2_ilp ), 2_ilp, c1, s1 )
              call stdlib_dlartg( h( 2_ilp, 3_ilp ), h( 2_ilp, 2_ilp ), c1, s1, temp )
              call stdlib_drot( 1_ilp, h( 1_ilp, 3_ilp ), 1_ilp, h( 1_ilp, 2_ilp ), 1_ilp, c1, s1 )
              call stdlib_dlartg( h( 1_ilp, 2_ilp ), h( 1_ilp, 1_ilp ), c2, s2, temp )
              call stdlib_drot( ihi-istartm+1, b( istartm, ihi ), 1_ilp, b( istartm,ihi-1 ), 1_ilp, c1, &
                        s1 )
              call stdlib_drot( ihi-istartm+1, b( istartm, ihi-1 ), 1_ilp, b( istartm,ihi-2 ), 1_ilp, c2, &
                        s2 )
              b( ihi-1, ihi-2 ) = zero
              b( ihi, ihi-2 ) = zero
              call stdlib_drot( ihi-istartm+1, a( istartm, ihi ), 1_ilp, a( istartm,ihi-1 ), 1_ilp, c1, &
                        s1 )
              call stdlib_drot( ihi-istartm+1, a( istartm, ihi-1 ), 1_ilp, a( istartm,ihi-2 ), 1_ilp, c2, &
                        s2 )
              if ( ilz ) then
                 call stdlib_drot( nz, z( 1_ilp, ihi-zstart+1 ), 1_ilp, z( 1_ilp, ihi-1-zstart+1 ), 1_ilp, c1, s1 &
                           )
                 call stdlib_drot( nz, z( 1_ilp, ihi-1-zstart+1 ), 1_ilp, z( 1_ilp,ihi-2-zstart+1 ), 1_ilp, c2, &
                           s2 )
              end if
              call stdlib_dlartg( a( ihi-1, ihi-2 ), a( ihi, ihi-2 ), c1, s1,temp )
              a( ihi-1, ihi-2 ) = temp
              a( ihi, ihi-2 ) = zero
              call stdlib_drot( istopm-ihi+2, a( ihi-1, ihi-1 ), lda, a( ihi,ihi-1 ), lda, c1, s1 &
                        )
              call stdlib_drot( istopm-ihi+2, b( ihi-1, ihi-1 ), ldb, b( ihi,ihi-1 ), ldb, c1, s1 &
                        )
              if ( ilq ) then
                 call stdlib_drot( nq, q( 1_ilp, ihi-1-qstart+1 ), 1_ilp, q( 1_ilp, ihi-qstart+1 ), 1_ilp, c1, s1 &
                           )
              end if
              call stdlib_dlartg( b( ihi, ihi ), b( ihi, ihi-1 ), c1, s1, temp )
              b( ihi, ihi ) = temp
              b( ihi, ihi-1 ) = zero
              call stdlib_drot( ihi-istartm, b( istartm, ihi ), 1_ilp, b( istartm,ihi-1 ), 1_ilp, c1, s1 )
                        
              call stdlib_drot( ihi-istartm+1, a( istartm, ihi ), 1_ilp, a( istartm,ihi-1 ), 1_ilp, c1, &
                        s1 )
              if ( ilz ) then
                 call stdlib_drot( nz, z( 1_ilp, ihi-zstart+1 ), 1_ilp, z( 1_ilp, ihi-1-zstart+1 ), 1_ilp, c1, s1 &
                           )
              end if
           else
              ! normal operation, move bulge down
              h = b( k+1:k+2, k:k+2 )
              ! make h upper triangular
              call stdlib_dlartg( h( 1_ilp, 1_ilp ), h( 2_ilp, 1_ilp ), c1, s1, temp )
              h( 2_ilp, 1_ilp ) = zero
              h( 1_ilp, 1_ilp ) = temp
              call stdlib_drot( 2_ilp, h( 1_ilp, 2_ilp ), 2_ilp, h( 2_ilp, 2_ilp ), 2_ilp, c1, s1 )
              ! calculate z1 and z2
              call stdlib_dlartg( h( 2_ilp, 3_ilp ), h( 2_ilp, 2_ilp ), c1, s1, temp )
              call stdlib_drot( 1_ilp, h( 1_ilp, 3_ilp ), 1_ilp, h( 1_ilp, 2_ilp ), 1_ilp, c1, s1 )
              call stdlib_dlartg( h( 1_ilp, 2_ilp ), h( 1_ilp, 1_ilp ), c2, s2, temp )
              ! apply transformations from the right
              call stdlib_drot( k+3-istartm+1, a( istartm, k+2 ), 1_ilp, a( istartm,k+1 ), 1_ilp, c1, s1 )
                        
              call stdlib_drot( k+3-istartm+1, a( istartm, k+1 ), 1_ilp, a( istartm,k ), 1_ilp, c2, s2 )
                        
              call stdlib_drot( k+2-istartm+1, b( istartm, k+2 ), 1_ilp, b( istartm,k+1 ), 1_ilp, c1, s1 )
                        
              call stdlib_drot( k+2-istartm+1, b( istartm, k+1 ), 1_ilp, b( istartm,k ), 1_ilp, c2, s2 )
                        
              if ( ilz ) then
                 call stdlib_drot( nz, z( 1_ilp, k+2-zstart+1 ), 1_ilp, z( 1_ilp, k+1-zstart+1 ), 1_ilp, c1, s1 )
                           
                 call stdlib_drot( nz, z( 1_ilp, k+1-zstart+1 ), 1_ilp, z( 1_ilp, k-zstart+1 ),1_ilp, c2, s2 )
                           
              end if
              b( k+1, k ) = zero
              b( k+2, k ) = zero
              ! calculate q1 and q2
              call stdlib_dlartg( a( k+2, k ), a( k+3, k ), c1, s1, temp )
              a( k+2, k ) = temp
              a( k+3, k ) = zero
              call stdlib_dlartg( a( k+1, k ), a( k+2, k ), c2, s2, temp )
              a( k+1, k ) = temp
              a( k+2, k ) = zero
              ! apply transformations from the left
              call stdlib_drot( istopm-k, a( k+2, k+1 ), lda, a( k+3, k+1 ), lda,c1, s1 )
              call stdlib_drot( istopm-k, a( k+1, k+1 ), lda, a( k+2, k+1 ), lda,c2, s2 )
              call stdlib_drot( istopm-k, b( k+2, k+1 ), ldb, b( k+3, k+1 ), ldb,c1, s1 )
              call stdlib_drot( istopm-k, b( k+1, k+1 ), ldb, b( k+2, k+1 ), ldb,c2, s2 )
              if ( ilq ) then
                 call stdlib_drot( nq, q( 1_ilp, k+2-qstart+1 ), 1_ilp, q( 1_ilp, k+3-qstart+1 ), 1_ilp, c1, s1 )
                           
                 call stdlib_drot( nq, q( 1_ilp, k+1-qstart+1 ), 1_ilp, q( 1_ilp, k+2-qstart+1 ), 1_ilp, c2, s2 )
                           
              end if
           end if
     end subroutine stdlib_dlaqz2


     recursive module subroutine stdlib_claqz2( ilschur, ilq, ilz, n, ilo, ihi, nw,a, lda, b, ldb, q, &
     !! CLAQZ2 performs AED
               ldq, z, ldz, ns,nd, alpha, beta, qc, ldqc, zc, ldzc,work, lwork, rwork, rec, info )
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! arguments
           logical(lk), intent( in ) :: ilschur, ilq, ilz
           integer(ilp), intent( in ) :: n, ilo, ihi, nw, lda, ldb, ldq, ldz,ldqc, ldzc, lwork, &
                     rec
           complex(sp), intent( inout ) :: a( lda, * ), b( ldb, * ), q( ldq, * ),z( ldz, * ), &
                     alpha( * ), beta( * )
           integer(ilp), intent( out ) :: ns, nd, info
           complex(sp), intent(inout) :: qc(ldqc,*), zc(ldzc,*)
           complex(sp), intent(out) :: work(*)
           real(sp), intent(out) :: rwork(*)
           
           ! ================================================================
           ! local scalars
           integer(ilp) :: jw, kwtop, kwbot, istopm, istartm, k, k2, ctgexc_info, ifst, ilst, &
                     lworkreq, qz_small_info
           real(sp) :: smlnum, ulp, safmin, safmax, c1, tempr
           complex(sp) :: s, s1, temp
           info = 0_ilp
           ! set up deflation window
           jw = min( nw, ihi-ilo+1 )
           kwtop = ihi-jw+1
           if ( kwtop == ilo ) then
              s = czero
           else
              s = a( kwtop, kwtop-1 )
           end if
           ! determine required workspace
           ifst = 1_ilp
           ilst = jw
           call stdlib_claqz0( 'S', 'V', 'V', jw, 1_ilp, jw, a( kwtop, kwtop ), lda,b( kwtop, kwtop ),&
                      ldb, alpha, beta, qc, ldqc, zc,ldzc, work, -1_ilp, rwork, rec+1, qz_small_info )
           lworkreq = int( work( 1_ilp ),KIND=ilp)+2_ilp*jw**2_ilp
           lworkreq = max( lworkreq, n*nw, 2_ilp*nw**2_ilp+n )
           if ( lwork ==-1_ilp ) then
              ! workspace query, quick return
              work( 1_ilp ) = lworkreq
              return
           else if ( lwork < lworkreq ) then
              info = -26_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CLAQZ2', -info )
              return
           end if
           ! get machine constants
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           safmax = one/safmin
           call stdlib_slabad( safmin, safmax )
           ulp = stdlib_slamch( 'PRECISION' )
           smlnum = safmin*( real( n,KIND=sp)/ulp )
           if ( ihi == kwtop ) then
              ! 1 by 1 deflation window, just try a regular deflation
              alpha( kwtop ) = a( kwtop, kwtop )
              beta( kwtop ) = b( kwtop, kwtop )
              ns = 1_ilp
              nd = 0_ilp
              if ( abs( s ) <= max( smlnum, ulp*abs( a( kwtop,kwtop ) ) ) ) then
                 ns = 0_ilp
                 nd = 1_ilp
                 if ( kwtop > ilo ) then
                    a( kwtop, kwtop-1 ) = czero
                 end if
              end if
           end if
           ! store window in case of convergence failure
           call stdlib_clacpy( 'ALL', jw, jw, a( kwtop, kwtop ), lda, work, jw )
           call stdlib_clacpy( 'ALL', jw, jw, b( kwtop, kwtop ), ldb, work( jw**2_ilp+1 ), jw )
                     
           ! transform window to real schur form
           call stdlib_claset( 'FULL', jw, jw, czero, cone, qc, ldqc )
           call stdlib_claset( 'FULL', jw, jw, czero, cone, zc, ldzc )
           call stdlib_claqz0( 'S', 'V', 'V', jw, 1_ilp, jw, a( kwtop, kwtop ), lda,b( kwtop, kwtop ),&
            ldb, alpha, beta, qc, ldqc, zc,ldzc, work( 2_ilp*jw**2_ilp+1 ), lwork-2*jw**2_ilp, rwork,rec+1, &
                      qz_small_info )
           if( qz_small_info /= 0_ilp ) then
              ! convergence failure, restore the window and exit
              nd = 0_ilp
              ns = jw-qz_small_info
              call stdlib_clacpy( 'ALL', jw, jw, work, jw, a( kwtop, kwtop ), lda )
              call stdlib_clacpy( 'ALL', jw, jw, work( jw**2_ilp+1 ), jw, b( kwtop,kwtop ), ldb )
                        
              return
           end if
           ! deflation detection loop
           if ( kwtop == ilo .or. s == czero ) then
              kwbot = kwtop-1
           else
              kwbot = ihi
              k = 1_ilp
              k2 = 1_ilp
              do while ( k <= jw )
                    ! try to deflate eigenvalue
                    tempr = abs( a( kwbot, kwbot ) )
                    if( tempr == zero ) then
                       tempr = abs( s )
                    end if
                    if ( ( abs( s*qc( 1_ilp, kwbot-kwtop+1 ) ) ) <= max( ulp*tempr, smlnum ) ) &
                              then
                       ! deflatable
                       kwbot = kwbot-1
                    else
                       ! not deflatable, move out of the way
                       ifst = kwbot-kwtop+1
                       ilst = k2
                       call stdlib_ctgexc( .true., .true., jw, a( kwtop, kwtop ),lda, b( kwtop, &
                                 kwtop ), ldb, qc, ldqc,zc, ldzc, ifst, ilst, ctgexc_info )
                       k2 = k2+1
                    end if
                    k = k+1
              end do
           end if
           ! store eigenvalues
           nd = ihi-kwbot
           ns = jw-nd
           k = kwtop
           do while ( k <= ihi )
              alpha( k ) = a( k, k )
              beta( k ) = b( k, k )
              k = k+1
           end do
           if ( kwtop /= ilo .and. s /= czero ) then
              ! reflect spike back, this will create optimally packed bulges
              a( kwtop:kwbot, kwtop-1 ) = a( kwtop, kwtop-1 ) *conjg( qc( 1_ilp,1_ilp:jw-nd ) )
              do k = kwbot-1, kwtop, -1
                 call stdlib_clartg( a( k, kwtop-1 ), a( k+1, kwtop-1 ), c1, s1,temp )
                 a( k, kwtop-1 ) = temp
                 a( k+1, kwtop-1 ) = czero
                 k2 = max( kwtop, k-1 )
                 call stdlib_crot( ihi-k2+1, a( k, k2 ), lda, a( k+1, k2 ), lda, c1,s1 )
                 call stdlib_crot( ihi-( k-1 )+1_ilp, b( k, k-1 ), ldb, b( k+1, k-1 ),ldb, c1, s1 )
                           
                 call stdlib_crot( jw, qc( 1_ilp, k-kwtop+1 ), 1_ilp, qc( 1_ilp, k+1-kwtop+1 ),1_ilp, c1, conjg( &
                           s1 ) )
              end do
              ! chase bulges down
              istartm = kwtop
              istopm = ihi
              k = kwbot-1
              do while ( k >= kwtop )
                 ! move bulge down and remove it
                 do k2 = k, kwbot-1
                    call stdlib_claqz1( .true., .true., k2, kwtop, kwtop+jw-1,kwbot, a, lda, b, &
                              ldb, jw, kwtop, qc, ldqc,jw, kwtop, zc, ldzc )
                 end do
                 k = k-1
              end do
           end if
           ! apply qc and zc to rest of the matrix
           if ( ilschur ) then
              istartm = 1_ilp
              istopm = n
           else
              istartm = ilo
              istopm = ihi
           end if
           if ( istopm-ihi > 0_ilp ) then
              call stdlib_cgemm( 'C', 'N', jw, istopm-ihi, jw, cone, qc, ldqc,a( kwtop, ihi+1 ), &
                        lda, czero, work, jw )
              call stdlib_clacpy( 'ALL', jw, istopm-ihi, work, jw, a( kwtop,ihi+1 ), lda )
              call stdlib_cgemm( 'C', 'N', jw, istopm-ihi, jw, cone, qc, ldqc,b( kwtop, ihi+1 ), &
                        ldb, czero, work, jw )
              call stdlib_clacpy( 'ALL', jw, istopm-ihi, work, jw, b( kwtop,ihi+1 ), ldb )
           end if
           if ( ilq ) then
              call stdlib_cgemm( 'N', 'N', n, jw, jw, cone, q( 1_ilp, kwtop ), ldq, qc,ldqc, czero, &
                        work, n )
              call stdlib_clacpy( 'ALL', n, jw, work, n, q( 1_ilp, kwtop ), ldq )
           end if
           if ( kwtop-1-istartm+1 > 0_ilp ) then
              call stdlib_cgemm( 'N', 'N', kwtop-istartm, jw, jw, cone, a( istartm,kwtop ), lda, &
                        zc, ldzc, czero, work,kwtop-istartm )
             call stdlib_clacpy( 'ALL', kwtop-istartm, jw, work, kwtop-istartm,a( istartm, kwtop )&
                       , lda )
              call stdlib_cgemm( 'N', 'N', kwtop-istartm, jw, jw, cone, b( istartm,kwtop ), ldb, &
                        zc, ldzc, czero, work,kwtop-istartm )
             call stdlib_clacpy( 'ALL', kwtop-istartm, jw, work, kwtop-istartm,b( istartm, kwtop )&
                       , ldb )
           end if
           if ( ilz ) then
              call stdlib_cgemm( 'N', 'N', n, jw, jw, cone, z( 1_ilp, kwtop ), ldz, zc,ldzc, czero, &
                        work, n )
              call stdlib_clacpy( 'ALL', n, jw, work, n, z( 1_ilp, kwtop ), ldz )
           end if
     end subroutine stdlib_claqz2

     recursive module subroutine stdlib_zlaqz2( ilschur, ilq, ilz, n, ilo, ihi, nw,a, lda, b, ldb, q, &
     !! ZLAQZ2 performs AED
               ldq, z, ldz, ns,nd, alpha, beta, qc, ldqc, zc, ldzc,work, lwork, rwork, rec, info )
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! arguments
           logical(lk), intent( in ) :: ilschur, ilq, ilz
           integer(ilp), intent( in ) :: n, ilo, ihi, nw, lda, ldb, ldq, ldz,ldqc, ldzc, lwork, &
                     rec
           complex(dp), intent( inout ) :: a( lda, * ), b( ldb, * ), q( ldq,* ), z( ldz, * ), &
                     alpha( * ), beta( * )
           integer(ilp), intent( out ) :: ns, nd, info
           complex(dp), intent(inout) :: qc(ldqc,*), zc(ldzc,*)
           complex(dp), intent(out) :: work(*)
           real(dp), intent(out) :: rwork(*)
           
           ! ================================================================
           ! local scalars
           integer(ilp) :: jw, kwtop, kwbot, istopm, istartm, k, k2, ztgexc_info, ifst, ilst, &
                     lworkreq, qz_small_info
           real(dp) ::smlnum, ulp, safmin, safmax, c1, tempr
           complex(dp) :: s, s1, temp
           info = 0_ilp
           ! set up deflation window
           jw = min( nw, ihi-ilo+1 )
           kwtop = ihi-jw+1
           if ( kwtop == ilo ) then
              s = czero
           else
              s = a( kwtop, kwtop-1 )
           end if
           ! determine required workspace
           ifst = 1_ilp
           ilst = jw
           call stdlib_zlaqz0( 'S', 'V', 'V', jw, 1_ilp, jw, a( kwtop, kwtop ), lda,b( kwtop, kwtop ),&
                      ldb, alpha, beta, qc, ldqc, zc,ldzc, work, -1_ilp, rwork, rec+1, qz_small_info )
           lworkreq = int( work( 1_ilp ),KIND=ilp)+2_ilp*jw**2_ilp
           lworkreq = max( lworkreq, n*nw, 2_ilp*nw**2_ilp+n )
           if ( lwork ==-1_ilp ) then
              ! workspace query, quick return
              work( 1_ilp ) = lworkreq
              return
           else if ( lwork < lworkreq ) then
              info = -26_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZLAQZ2', -info )
              return
           end if
           ! get machine constants
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           safmax = one/safmin
           call stdlib_dlabad( safmin, safmax )
           ulp = stdlib_dlamch( 'PRECISION' )
           smlnum = safmin*( real( n,KIND=dp)/ulp )
           if ( ihi == kwtop ) then
              ! 1 by 1 deflation window, just try a regular deflation
              alpha( kwtop ) = a( kwtop, kwtop )
              beta( kwtop ) = b( kwtop, kwtop )
              ns = 1_ilp
              nd = 0_ilp
              if ( abs( s ) <= max( smlnum, ulp*abs( a( kwtop,kwtop ) ) ) ) then
                 ns = 0_ilp
                 nd = 1_ilp
                 if ( kwtop > ilo ) then
                    a( kwtop, kwtop-1 ) = czero
                 end if
              end if
           end if
           ! store window in case of convergence failure
           call stdlib_zlacpy( 'ALL', jw, jw, a( kwtop, kwtop ), lda, work, jw )
           call stdlib_zlacpy( 'ALL', jw, jw, b( kwtop, kwtop ), ldb, work( jw**2_ilp+1 ), jw )
                     
           ! transform window to real schur form
           call stdlib_zlaset( 'FULL', jw, jw, czero, cone, qc, ldqc )
           call stdlib_zlaset( 'FULL', jw, jw, czero, cone, zc, ldzc )
           call stdlib_zlaqz0( 'S', 'V', 'V', jw, 1_ilp, jw, a( kwtop, kwtop ), lda,b( kwtop, kwtop ),&
            ldb, alpha, beta, qc, ldqc, zc,ldzc, work( 2_ilp*jw**2_ilp+1 ), lwork-2*jw**2_ilp, rwork,rec+1, &
                      qz_small_info )
           if( qz_small_info /= 0_ilp ) then
              ! convergence failure, restore the window and exit
              nd = 0_ilp
              ns = jw-qz_small_info
              call stdlib_zlacpy( 'ALL', jw, jw, work, jw, a( kwtop, kwtop ), lda )
              call stdlib_zlacpy( 'ALL', jw, jw, work( jw**2_ilp+1 ), jw, b( kwtop,kwtop ), ldb )
                        
              return
           end if
           ! deflation detection loop
           if ( kwtop == ilo .or. s == czero ) then
              kwbot = kwtop-1
           else
              kwbot = ihi
              k = 1_ilp
              k2 = 1_ilp
              do while ( k <= jw )
                    ! try to deflate eigenvalue
                    tempr = abs( a( kwbot, kwbot ) )
                    if( tempr == zero ) then
                       tempr = abs( s )
                    end if
                    if ( ( abs( s*qc( 1_ilp, kwbot-kwtop+1 ) ) ) <= max( ulp*tempr, smlnum ) ) &
                              then
                       ! deflatable
                       kwbot = kwbot-1
                    else
                       ! not deflatable, move out of the way
                       ifst = kwbot-kwtop+1
                       ilst = k2
                       call stdlib_ztgexc( .true., .true., jw, a( kwtop, kwtop ),lda, b( kwtop, &
                                 kwtop ), ldb, qc, ldqc,zc, ldzc, ifst, ilst, ztgexc_info )
                       k2 = k2+1
                    end if
                    k = k+1
              end do
           end if
           ! store eigenvalues
           nd = ihi-kwbot
           ns = jw-nd
           k = kwtop
           do while ( k <= ihi )
              alpha( k ) = a( k, k )
              beta( k ) = b( k, k )
              k = k+1
           end do
           if ( kwtop /= ilo .and. s /= czero ) then
              ! reflect spike back, this will create optimally packed bulges
              a( kwtop:kwbot, kwtop-1 ) = a( kwtop, kwtop-1 ) *conjg( qc( 1_ilp,1_ilp:jw-nd ) )
              do k = kwbot-1, kwtop, -1
                 call stdlib_zlartg( a( k, kwtop-1 ), a( k+1, kwtop-1 ), c1, s1,temp )
                 a( k, kwtop-1 ) = temp
                 a( k+1, kwtop-1 ) = czero
                 k2 = max( kwtop, k-1 )
                 call stdlib_zrot( ihi-k2+1, a( k, k2 ), lda, a( k+1, k2 ), lda, c1,s1 )
                 call stdlib_zrot( ihi-( k-1 )+1_ilp, b( k, k-1 ), ldb, b( k+1, k-1 ),ldb, c1, s1 )
                           
                 call stdlib_zrot( jw, qc( 1_ilp, k-kwtop+1 ), 1_ilp, qc( 1_ilp, k+1-kwtop+1 ),1_ilp, c1, conjg( &
                           s1 ) )
              end do
              ! chase bulges down
              istartm = kwtop
              istopm = ihi
              k = kwbot-1
              do while ( k >= kwtop )
                 ! move bulge down and remove it
                 do k2 = k, kwbot-1
                    call stdlib_zlaqz1( .true., .true., k2, kwtop, kwtop+jw-1,kwbot, a, lda, b, &
                              ldb, jw, kwtop, qc, ldqc,jw, kwtop, zc, ldzc )
                 end do
                 k = k-1
              end do
           end if
           ! apply qc and zc to rest of the matrix
           if ( ilschur ) then
              istartm = 1_ilp
              istopm = n
           else
              istartm = ilo
              istopm = ihi
           end if
           if ( istopm-ihi > 0_ilp ) then
              call stdlib_zgemm( 'C', 'N', jw, istopm-ihi, jw, cone, qc, ldqc,a( kwtop, ihi+1 ), &
                        lda, czero, work, jw )
              call stdlib_zlacpy( 'ALL', jw, istopm-ihi, work, jw, a( kwtop,ihi+1 ), lda )
              call stdlib_zgemm( 'C', 'N', jw, istopm-ihi, jw, cone, qc, ldqc,b( kwtop, ihi+1 ), &
                        ldb, czero, work, jw )
              call stdlib_zlacpy( 'ALL', jw, istopm-ihi, work, jw, b( kwtop,ihi+1 ), ldb )
           end if
           if ( ilq ) then
              call stdlib_zgemm( 'N', 'N', n, jw, jw, cone, q( 1_ilp, kwtop ), ldq, qc,ldqc, czero, &
                        work, n )
              call stdlib_zlacpy( 'ALL', n, jw, work, n, q( 1_ilp, kwtop ), ldq )
           end if
           if ( kwtop-1-istartm+1 > 0_ilp ) then
              call stdlib_zgemm( 'N', 'N', kwtop-istartm, jw, jw, cone, a( istartm,kwtop ), lda, &
                        zc, ldzc, czero, work,kwtop-istartm )
             call stdlib_zlacpy( 'ALL', kwtop-istartm, jw, work, kwtop-istartm,a( istartm, kwtop )&
                       , lda )
              call stdlib_zgemm( 'N', 'N', kwtop-istartm, jw, jw, cone, b( istartm,kwtop ), ldb, &
                        zc, ldzc, czero, work,kwtop-istartm )
             call stdlib_zlacpy( 'ALL', kwtop-istartm, jw, work, kwtop-istartm,b( istartm, kwtop )&
                       , ldb )
           end if
           if ( ilz ) then
              call stdlib_zgemm( 'N', 'N', n, jw, jw, cone, z( 1_ilp, kwtop ), ldz, zc,ldzc, czero, &
                        work, n )
              call stdlib_zlacpy( 'ALL', n, jw, work, n, z( 1_ilp, kwtop ), ldz )
           end if
     end subroutine stdlib_zlaqz2




     recursive module subroutine stdlib_slaqz3( ilschur, ilq, ilz, n, ilo, ihi, nw,a, lda, b, ldb, q, &
     !! SLAQZ3 performs AED
               ldq, z, ldz, ns,nd, alphar, alphai, beta, qc, ldqc,zc, ldzc, work, lwork, rec, info )
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! arguments
           logical(lk), intent( in ) :: ilschur, ilq, ilz
           integer(ilp), intent( in ) :: n, ilo, ihi, nw, lda, ldb, ldq, ldz,ldqc, ldzc, lwork, &
                     rec
           real(sp), intent( inout ) :: a( lda, * ), b( ldb, * ), q( ldq, * ),z( ldz, * ), alphar(&
                      * ), alphai( * ), beta( * )
           integer(ilp), intent( out ) :: ns, nd, info
           real(sp), intent(inout) :: qc(ldqc,*), zc(ldzc,*)
           real(sp), intent(out) :: work(*)
           
           ! ================================================================
           ! local scalars
           logical(lk) :: bulge
           integer(ilp) :: jw, kwtop, kwbot, istopm, istartm, k, k2, stgexc_info, ifst, ilst, &
                     lworkreq, qz_small_info
           real(sp) :: s, smlnum, ulp, safmin, safmax, c1, s1, temp
           info = 0_ilp
           ! set up deflation window
           jw = min( nw, ihi-ilo+1 )
           kwtop = ihi-jw+1
           if ( kwtop == ilo ) then
              s = zero
           else
              s = a( kwtop, kwtop-1 )
           end if
           ! determine required workspace
           ifst = 1_ilp
           ilst = jw
           call stdlib_stgexc( .true., .true., jw, a, lda, b, ldb, qc, ldqc, zc,ldzc, ifst, ilst, &
                     work, -1_ilp, stgexc_info )
           lworkreq = int( work( 1_ilp ),KIND=ilp)
           call stdlib_slaqz0( 'S', 'V', 'V', jw, 1_ilp, jw, a( kwtop, kwtop ), lda,b( kwtop, kwtop ),&
                      ldb, alphar, alphai, beta, qc,ldqc, zc, ldzc, work, -1_ilp, rec+1, qz_small_info )
           lworkreq = max( lworkreq, int( work( 1_ilp ),KIND=ilp)+2_ilp*jw**2_ilp )
           lworkreq = max( lworkreq, n*nw, 2_ilp*nw**2_ilp+n )
           if ( lwork ==-1_ilp ) then
              ! workspace query, quick return
              work( 1_ilp ) = lworkreq
              return
           else if ( lwork < lworkreq ) then
              info = -26_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SLAQZ3', -info )
              return
           end if
           ! get machine constants
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           safmax = one/safmin
           call stdlib_slabad( safmin, safmax )
           ulp = stdlib_slamch( 'PRECISION' )
           smlnum = safmin*( real( n,KIND=sp)/ulp )
           if ( ihi == kwtop ) then
              ! 1 by 1 deflation window, just try a regular deflation
              alphar( kwtop ) = a( kwtop, kwtop )
              alphai( kwtop ) = zero
              beta( kwtop ) = b( kwtop, kwtop )
              ns = 1_ilp
              nd = 0_ilp
              if ( abs( s ) <= max( smlnum, ulp*abs( a( kwtop,kwtop ) ) ) ) then
                 ns = 0_ilp
                 nd = 1_ilp
                 if ( kwtop > ilo ) then
                    a( kwtop, kwtop-1 ) = zero
                 end if
              end if
           end if
           ! store window in case of convergence failure
           call stdlib_slacpy( 'ALL', jw, jw, a( kwtop, kwtop ), lda, work, jw )
           call stdlib_slacpy( 'ALL', jw, jw, b( kwtop, kwtop ), ldb, work( jw**2_ilp+1 ), jw )
                     
           ! transform window to real schur form
           call stdlib_slaset( 'FULL', jw, jw, zero, one, qc, ldqc )
           call stdlib_slaset( 'FULL', jw, jw, zero, one, zc, ldzc )
           call stdlib_slaqz0( 'S', 'V', 'V', jw, 1_ilp, jw, a( kwtop, kwtop ), lda,b( kwtop, kwtop ),&
            ldb, alphar, alphai, beta, qc,ldqc, zc, ldzc, work( 2_ilp*jw**2_ilp+1 ), lwork-2*jw**2_ilp,rec+1, &
                      qz_small_info )
           if( qz_small_info /= 0_ilp ) then
              ! convergence failure, restore the window and exit
              nd = 0_ilp
              ns = jw-qz_small_info
              call stdlib_slacpy( 'ALL', jw, jw, work, jw, a( kwtop, kwtop ), lda )
              call stdlib_slacpy( 'ALL', jw, jw, work( jw**2_ilp+1 ), jw, b( kwtop,kwtop ), ldb )
                        
              return
           end if
           ! deflation detection loop
           if ( kwtop == ilo .or. s == zero ) then
              kwbot = kwtop-1
           else
              kwbot = ihi
              k = 1_ilp
              k2 = 1_ilp
              do while ( k <= jw )
                 bulge = .false.
                 if ( kwbot-kwtop+1 >= 2_ilp ) then
                    bulge = a( kwbot, kwbot-1 ) /= zero
                 end if
                 if ( bulge ) then
                    ! try to deflate complex conjugate eigenvalue pair
                    temp = abs( a( kwbot, kwbot ) )+sqrt( abs( a( kwbot,kwbot-1 ) ) )*sqrt( abs( &
                              a( kwbot-1, kwbot ) ) )
                    if( temp == zero )then
                       temp = abs( s )
                    end if
                    if ( max( abs( s*qc( 1_ilp, kwbot-kwtop ) ), abs( s*qc( 1_ilp,kwbot-kwtop+1 ) ) ) <= &
                              max( smlnum,ulp*temp ) ) then
                       ! deflatable
                       kwbot = kwbot-2
                    else
                       ! not deflatable, move out of the way
                       ifst = kwbot-kwtop+1
                       ilst = k2
                       call stdlib_stgexc( .true., .true., jw, a( kwtop, kwtop ),lda, b( kwtop, &
                       kwtop ), ldb, qc, ldqc,zc, ldzc, ifst, ilst, work, lwork,stgexc_info )
                                 
                       k2 = k2+2
                    end if
                    k = k+2
                 else
                    ! try to deflate real eigenvalue
                    temp = abs( a( kwbot, kwbot ) )
                    if( temp == zero ) then
                       temp = abs( s )
                    end if
                    if ( ( abs( s*qc( 1_ilp, kwbot-kwtop+1 ) ) ) <= max( ulp*temp, smlnum ) ) &
                              then
                       ! deflatable
                       kwbot = kwbot-1
                    else
                       ! not deflatable, move out of the way
                       ifst = kwbot-kwtop+1
                       ilst = k2
                       call stdlib_stgexc( .true., .true., jw, a( kwtop, kwtop ),lda, b( kwtop, &
                       kwtop ), ldb, qc, ldqc,zc, ldzc, ifst, ilst, work, lwork,stgexc_info )
                                 
                       k2 = k2+1
                    end if
                    k = k+1
                 end if
              end do
           end if
           ! store eigenvalues
           nd = ihi-kwbot
           ns = jw-nd
           k = kwtop
           do while ( k <= ihi )
              bulge = .false.
              if ( k < ihi ) then
                 if ( a( k+1, k ) /= zero ) then
                    bulge = .true.
                 end if
              end if
              if ( bulge ) then
                 ! 2x2 eigenvalue block
                 call stdlib_slag2( a( k, k ), lda, b( k, k ), ldb, safmin,beta( k ), beta( k+1 ),&
                            alphar( k ),alphar( k+1 ), alphai( k ) )
                 alphai( k+1 ) = -alphai( k )
                 k = k+2
              else
                 ! 1x1 eigenvalue block
                 alphar( k ) = a( k, k )
                 alphai( k ) = zero
                 beta( k ) = b( k, k )
                 k = k+1
              end if
           end do
           if ( kwtop /= ilo .and. s /= zero ) then
              ! reflect spike back, this will create optimally packed bulges
              a( kwtop:kwbot, kwtop-1 ) = a( kwtop, kwtop-1 )*qc( 1_ilp,1_ilp:jw-nd )
              do k = kwbot-1, kwtop, -1
                 call stdlib_slartg( a( k, kwtop-1 ), a( k+1, kwtop-1 ), c1, s1,temp )
                 a( k, kwtop-1 ) = temp
                 a( k+1, kwtop-1 ) = zero
                 k2 = max( kwtop, k-1 )
                 call stdlib_srot( ihi-k2+1, a( k, k2 ), lda, a( k+1, k2 ), lda, c1,s1 )
                 call stdlib_srot( ihi-( k-1 )+1_ilp, b( k, k-1 ), ldb, b( k+1, k-1 ),ldb, c1, s1 )
                           
                 call stdlib_srot( jw, qc( 1_ilp, k-kwtop+1 ), 1_ilp, qc( 1_ilp, k+1-kwtop+1 ),1_ilp, c1, s1 )
                           
              end do
              ! chase bulges down
              istartm = kwtop
              istopm = ihi
              k = kwbot-1
              do while ( k >= kwtop )
                 if ( ( k >= kwtop+1 ) .and. a( k+1, k-1 ) /= zero ) then
                    ! move double pole block down and remove it
                    do k2 = k-1, kwbot-2
                       call stdlib_slaqz2( .true., .true., k2, kwtop, kwtop+jw-1,kwbot, a, lda, b,&
                                  ldb, jw, kwtop, qc,ldqc, jw, kwtop, zc, ldzc )
                    end do
                    k = k-2
                 else
                    ! k points to single shift
                    do k2 = k, kwbot-2
                       ! move shift down
                       call stdlib_slartg( b( k2+1, k2+1 ), b( k2+1, k2 ), c1, s1,temp )
                       b( k2+1, k2+1 ) = temp
                       b( k2+1, k2 ) = zero
                       call stdlib_srot( k2+2-istartm+1, a( istartm, k2+1 ), 1_ilp,a( istartm, k2 ), &
                                 1_ilp, c1, s1 )
                       call stdlib_srot( k2-istartm+1, b( istartm, k2+1 ), 1_ilp,b( istartm, k2 ), 1_ilp, &
                                 c1, s1 )
                       call stdlib_srot( jw, zc( 1_ilp, k2+1-kwtop+1 ), 1_ilp, zc( 1_ilp,k2-kwtop+1 ), 1_ilp, c1, &
                                 s1 )
                       call stdlib_slartg( a( k2+1, k2 ), a( k2+2, k2 ), c1, s1,temp )
                       a( k2+1, k2 ) = temp
                       a( k2+2, k2 ) = zero
                       call stdlib_srot( istopm-k2, a( k2+1, k2+1 ), lda, a( k2+2,k2+1 ), lda, c1,&
                                  s1 )
                       call stdlib_srot( istopm-k2, b( k2+1, k2+1 ), ldb, b( k2+2,k2+1 ), ldb, c1,&
                                  s1 )
                       call stdlib_srot( jw, qc( 1_ilp, k2+1-kwtop+1 ), 1_ilp, qc( 1_ilp,k2+2-kwtop+1 ), 1_ilp, &
                                 c1, s1 )
                    end do
                    ! remove the shift
                    call stdlib_slartg( b( kwbot, kwbot ), b( kwbot, kwbot-1 ), c1,s1, temp )
                              
                    b( kwbot, kwbot ) = temp
                    b( kwbot, kwbot-1 ) = zero
                    call stdlib_srot( kwbot-istartm, b( istartm, kwbot ), 1_ilp,b( istartm, kwbot-1 ),&
                               1_ilp, c1, s1 )
                    call stdlib_srot( kwbot-istartm+1, a( istartm, kwbot ), 1_ilp,a( istartm, kwbot-1 &
                              ), 1_ilp, c1, s1 )
                    call stdlib_srot( jw, zc( 1_ilp, kwbot-kwtop+1 ), 1_ilp, zc( 1_ilp,kwbot-1-kwtop+1 ), 1_ilp, &
                              c1, s1 )
                    k = k-1
                 end if
              end do
           end if
           ! apply qc and zc to rest of the matrix
           if ( ilschur ) then
              istartm = 1_ilp
              istopm = n
           else
              istartm = ilo
              istopm = ihi
           end if
           if ( istopm-ihi > 0_ilp ) then
              call stdlib_sgemm( 'T', 'N', jw, istopm-ihi, jw, one, qc, ldqc,a( kwtop, ihi+1 ), &
                        lda, zero, work, jw )
              call stdlib_slacpy( 'ALL', jw, istopm-ihi, work, jw, a( kwtop,ihi+1 ), lda )
              call stdlib_sgemm( 'T', 'N', jw, istopm-ihi, jw, one, qc, ldqc,b( kwtop, ihi+1 ), &
                        ldb, zero, work, jw )
              call stdlib_slacpy( 'ALL', jw, istopm-ihi, work, jw, b( kwtop,ihi+1 ), ldb )
           end if
           if ( ilq ) then
              call stdlib_sgemm( 'N', 'N', n, jw, jw, one, q( 1_ilp, kwtop ), ldq, qc,ldqc, zero, &
                        work, n )
              call stdlib_slacpy( 'ALL', n, jw, work, n, q( 1_ilp, kwtop ), ldq )
           end if
           if ( kwtop-1-istartm+1 > 0_ilp ) then
              call stdlib_sgemm( 'N', 'N', kwtop-istartm, jw, jw, one, a( istartm,kwtop ), lda, &
                        zc, ldzc, zero, work,kwtop-istartm )
              call stdlib_slacpy( 'ALL', kwtop-istartm, jw, work, kwtop-istartm,a( istartm, kwtop &
                        ), lda )
              call stdlib_sgemm( 'N', 'N', kwtop-istartm, jw, jw, one, b( istartm,kwtop ), ldb, &
                        zc, ldzc, zero, work,kwtop-istartm )
              call stdlib_slacpy( 'ALL', kwtop-istartm, jw, work, kwtop-istartm,b( istartm, kwtop &
                        ), ldb )
           end if
           if ( ilz ) then
              call stdlib_sgemm( 'N', 'N', n, jw, jw, one, z( 1_ilp, kwtop ), ldz, zc,ldzc, zero, &
                        work, n )
              call stdlib_slacpy( 'ALL', n, jw, work, n, z( 1_ilp, kwtop ), ldz )
           end if
     end subroutine stdlib_slaqz3

     recursive module subroutine stdlib_dlaqz3( ilschur, ilq, ilz, n, ilo, ihi, nw,a, lda, b, ldb, q, &
     !! DLAQZ3 performs AED
               ldq, z, ldz, ns,nd, alphar, alphai, beta, qc, ldqc,zc, ldzc, work, lwork, rec, info )
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! arguments
           logical(lk), intent( in ) :: ilschur, ilq, ilz
           integer(ilp), intent( in ) :: n, ilo, ihi, nw, lda, ldb, ldq, ldz,ldqc, ldzc, lwork, &
                     rec
           real(dp), intent( inout ) :: a( lda, * ), b( ldb, * ),q( ldq, * ), z( ldz, * ), alphar(&
                      * ),alphai( * ), beta( * )
           integer(ilp), intent( out ) :: ns, nd, info
           real(dp), intent(inout) :: qc(ldqc,*), zc(ldzc,*)
           real(dp), intent(out) :: work(*)
           ! ================================================================
           ! local scalars
           logical(lk) :: bulge
           integer(ilp) :: jw, kwtop, kwbot, istopm, istartm, k, k2, dtgexc_info, ifst, ilst, &
                     lworkreq, qz_small_info
           real(dp) :: s, smlnum, ulp, safmin, safmax, c1, s1, temp
           info = 0_ilp
           ! set up deflation window
           jw = min( nw, ihi-ilo+1 )
           kwtop = ihi-jw+1
           if ( kwtop == ilo ) then
              s = zero
           else
              s = a( kwtop, kwtop-1 )
           end if
           ! determine required workspace
           ifst = 1_ilp
           ilst = jw
           call stdlib_dtgexc( .true., .true., jw, a, lda, b, ldb, qc, ldqc, zc,ldzc, ifst, ilst, &
                     work, -1_ilp, dtgexc_info )
           lworkreq = int( work( 1_ilp ),KIND=ilp)
           call stdlib_dlaqz0( 'S', 'V', 'V', jw, 1_ilp, jw, a( kwtop, kwtop ), lda,b( kwtop, kwtop ),&
                      ldb, alphar, alphai, beta, qc,ldqc, zc, ldzc, work, -1_ilp, rec+1, qz_small_info )
           lworkreq = max( lworkreq, int( work( 1_ilp ),KIND=ilp)+2_ilp*jw**2_ilp )
           lworkreq = max( lworkreq, n*nw, 2_ilp*nw**2_ilp+n )
           if ( lwork ==-1_ilp ) then
              ! workspace query, quick return
              work( 1_ilp ) = lworkreq
              return
           else if ( lwork < lworkreq ) then
              info = -26_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DLAQZ3', -info )
              return
           end if
           ! get machine constants
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           safmax = one/safmin
           call stdlib_dlabad( safmin, safmax )
           ulp = stdlib_dlamch( 'PRECISION' )
           smlnum = safmin*( real( n,KIND=dp)/ulp )
           if ( ihi == kwtop ) then
              ! 1 by 1 deflation window, just try a regular deflation
              alphar( kwtop ) = a( kwtop, kwtop )
              alphai( kwtop ) = zero
              beta( kwtop ) = b( kwtop, kwtop )
              ns = 1_ilp
              nd = 0_ilp
              if ( abs( s ) <= max( smlnum, ulp*abs( a( kwtop,kwtop ) ) ) ) then
                 ns = 0_ilp
                 nd = 1_ilp
                 if ( kwtop > ilo ) then
                    a( kwtop, kwtop-1 ) = zero
                 end if
              end if
           end if
           ! store window in case of convergence failure
           call stdlib_dlacpy( 'ALL', jw, jw, a( kwtop, kwtop ), lda, work, jw )
           call stdlib_dlacpy( 'ALL', jw, jw, b( kwtop, kwtop ), ldb, work( jw**2_ilp+1 ), jw )
                     
           ! transform window to real schur form
           call stdlib_dlaset( 'FULL', jw, jw, zero, one, qc, ldqc )
           call stdlib_dlaset( 'FULL', jw, jw, zero, one, zc, ldzc )
           call stdlib_dlaqz0( 'S', 'V', 'V', jw, 1_ilp, jw, a( kwtop, kwtop ), lda,b( kwtop, kwtop ),&
            ldb, alphar, alphai, beta, qc,ldqc, zc, ldzc, work( 2_ilp*jw**2_ilp+1 ), lwork-2*jw**2_ilp,rec+1, &
                      qz_small_info )
           if( qz_small_info /= 0_ilp ) then
              ! convergence failure, restore the window and exit
              nd = 0_ilp
              ns = jw-qz_small_info
              call stdlib_dlacpy( 'ALL', jw, jw, work, jw, a( kwtop, kwtop ), lda )
              call stdlib_dlacpy( 'ALL', jw, jw, work( jw**2_ilp+1 ), jw, b( kwtop,kwtop ), ldb )
                        
              return
           end if
           ! deflation detection loop
           if ( kwtop == ilo .or. s == zero ) then
              kwbot = kwtop-1
           else
              kwbot = ihi
              k = 1_ilp
              k2 = 1_ilp
              do while ( k <= jw )
                 bulge = .false.
                 if ( kwbot-kwtop+1 >= 2_ilp ) then
                    bulge = a( kwbot, kwbot-1 ) /= zero
                 end if
                 if ( bulge ) then
                    ! try to deflate complex conjugate eigenvalue pair
                    temp = abs( a( kwbot, kwbot ) )+sqrt( abs( a( kwbot,kwbot-1 ) ) )*sqrt( abs( &
                              a( kwbot-1, kwbot ) ) )
                    if( temp == zero )then
                       temp = abs( s )
                    end if
                    if ( max( abs( s*qc( 1_ilp, kwbot-kwtop ) ), abs( s*qc( 1_ilp,kwbot-kwtop+1 ) ) ) <= &
                              max( smlnum,ulp*temp ) ) then
                       ! deflatable
                       kwbot = kwbot-2
                    else
                       ! not deflatable, move out of the way
                       ifst = kwbot-kwtop+1
                       ilst = k2
                       call stdlib_dtgexc( .true., .true., jw, a( kwtop, kwtop ),lda, b( kwtop, &
                       kwtop ), ldb, qc, ldqc,zc, ldzc, ifst, ilst, work, lwork,dtgexc_info )
                                 
                       k2 = k2+2
                    end if
                    k = k+2
                 else
                    ! try to deflate real eigenvalue
                    temp = abs( a( kwbot, kwbot ) )
                    if( temp == zero ) then
                       temp = abs( s )
                    end if
                    if ( ( abs( s*qc( 1_ilp, kwbot-kwtop+1 ) ) ) <= max( ulp*temp, smlnum ) ) &
                              then
                       ! deflatable
                       kwbot = kwbot-1
                    else
                       ! not deflatable, move out of the way
                       ifst = kwbot-kwtop+1
                       ilst = k2
                       call stdlib_dtgexc( .true., .true., jw, a( kwtop, kwtop ),lda, b( kwtop, &
                       kwtop ), ldb, qc, ldqc,zc, ldzc, ifst, ilst, work, lwork,dtgexc_info )
                                 
                       k2 = k2+1
                    end if
                    k = k+1
                 end if
              end do
           end if
           ! store eigenvalues
           nd = ihi-kwbot
           ns = jw-nd
           k = kwtop
           do while ( k <= ihi )
              bulge = .false.
              if ( k < ihi ) then
                 if ( a( k+1, k ) /= zero ) then
                    bulge = .true.
                 end if
              end if
              if ( bulge ) then
                 ! 2x2 eigenvalue block
                 call stdlib_dlag2( a( k, k ), lda, b( k, k ), ldb, safmin,beta( k ), beta( k+1 ),&
                            alphar( k ),alphar( k+1 ), alphai( k ) )
                 alphai( k+1 ) = -alphai( k )
                 k = k+2
              else
                 ! 1x1 eigenvalue block
                 alphar( k ) = a( k, k )
                 alphai( k ) = zero
                 beta( k ) = b( k, k )
                 k = k+1
              end if
           end do
           if ( kwtop /= ilo .and. s /= zero ) then
              ! reflect spike back, this will create optimally packed bulges
              a( kwtop:kwbot, kwtop-1 ) = a( kwtop, kwtop-1 )*qc( 1_ilp,1_ilp:jw-nd )
              do k = kwbot-1, kwtop, -1
                 call stdlib_dlartg( a( k, kwtop-1 ), a( k+1, kwtop-1 ), c1, s1,temp )
                 a( k, kwtop-1 ) = temp
                 a( k+1, kwtop-1 ) = zero
                 k2 = max( kwtop, k-1 )
                 call stdlib_drot( ihi-k2+1, a( k, k2 ), lda, a( k+1, k2 ), lda, c1,s1 )
                 call stdlib_drot( ihi-( k-1 )+1_ilp, b( k, k-1 ), ldb, b( k+1, k-1 ),ldb, c1, s1 )
                           
                 call stdlib_drot( jw, qc( 1_ilp, k-kwtop+1 ), 1_ilp, qc( 1_ilp, k+1-kwtop+1 ),1_ilp, c1, s1 )
                           
              end do
              ! chase bulges down
              istartm = kwtop
              istopm = ihi
              k = kwbot-1
              do while ( k >= kwtop )
                 if ( ( k >= kwtop+1 ) .and. a( k+1, k-1 ) /= zero ) then
                    ! move double pole block down and remove it
                    do k2 = k-1, kwbot-2
                       call stdlib_dlaqz2( .true., .true., k2, kwtop, kwtop+jw-1,kwbot, a, lda, b,&
                                  ldb, jw, kwtop, qc,ldqc, jw, kwtop, zc, ldzc )
                    end do
                    k = k-2
                 else
                    ! k points to single shift
                    do k2 = k, kwbot-2
                       ! move shift down
                       call stdlib_dlartg( b( k2+1, k2+1 ), b( k2+1, k2 ), c1, s1,temp )
                       b( k2+1, k2+1 ) = temp
                       b( k2+1, k2 ) = zero
                       call stdlib_drot( k2+2-istartm+1, a( istartm, k2+1 ), 1_ilp,a( istartm, k2 ), &
                                 1_ilp, c1, s1 )
                       call stdlib_drot( k2-istartm+1, b( istartm, k2+1 ), 1_ilp,b( istartm, k2 ), 1_ilp, &
                                 c1, s1 )
                       call stdlib_drot( jw, zc( 1_ilp, k2+1-kwtop+1 ), 1_ilp, zc( 1_ilp,k2-kwtop+1 ), 1_ilp, c1, &
                                 s1 )
                       call stdlib_dlartg( a( k2+1, k2 ), a( k2+2, k2 ), c1, s1,temp )
                       a( k2+1, k2 ) = temp
                       a( k2+2, k2 ) = zero
                       call stdlib_drot( istopm-k2, a( k2+1, k2+1 ), lda, a( k2+2,k2+1 ), lda, c1,&
                                  s1 )
                       call stdlib_drot( istopm-k2, b( k2+1, k2+1 ), ldb, b( k2+2,k2+1 ), ldb, c1,&
                                  s1 )
                       call stdlib_drot( jw, qc( 1_ilp, k2+1-kwtop+1 ), 1_ilp, qc( 1_ilp,k2+2-kwtop+1 ), 1_ilp, &
                                 c1, s1 )
                    end do
                    ! remove the shift
                    call stdlib_dlartg( b( kwbot, kwbot ), b( kwbot, kwbot-1 ), c1,s1, temp )
                              
                    b( kwbot, kwbot ) = temp
                    b( kwbot, kwbot-1 ) = zero
                    call stdlib_drot( kwbot-istartm, b( istartm, kwbot ), 1_ilp,b( istartm, kwbot-1 ),&
                               1_ilp, c1, s1 )
                    call stdlib_drot( kwbot-istartm+1, a( istartm, kwbot ), 1_ilp,a( istartm, kwbot-1 &
                              ), 1_ilp, c1, s1 )
                    call stdlib_drot( jw, zc( 1_ilp, kwbot-kwtop+1 ), 1_ilp, zc( 1_ilp,kwbot-1-kwtop+1 ), 1_ilp, &
                              c1, s1 )
                    k = k-1
                 end if
              end do
           end if
           ! apply qc and zc to rest of the matrix
           if ( ilschur ) then
              istartm = 1_ilp
              istopm = n
           else
              istartm = ilo
              istopm = ihi
           end if
           if ( istopm-ihi > 0_ilp ) then
              call stdlib_dgemm( 'T', 'N', jw, istopm-ihi, jw, one, qc, ldqc,a( kwtop, ihi+1 ), &
                        lda, zero, work, jw )
              call stdlib_dlacpy( 'ALL', jw, istopm-ihi, work, jw, a( kwtop,ihi+1 ), lda )
              call stdlib_dgemm( 'T', 'N', jw, istopm-ihi, jw, one, qc, ldqc,b( kwtop, ihi+1 ), &
                        ldb, zero, work, jw )
              call stdlib_dlacpy( 'ALL', jw, istopm-ihi, work, jw, b( kwtop,ihi+1 ), ldb )
           end if
           if ( ilq ) then
              call stdlib_dgemm( 'N', 'N', n, jw, jw, one, q( 1_ilp, kwtop ), ldq, qc,ldqc, zero, &
                        work, n )
              call stdlib_dlacpy( 'ALL', n, jw, work, n, q( 1_ilp, kwtop ), ldq )
           end if
           if ( kwtop-1-istartm+1 > 0_ilp ) then
              call stdlib_dgemm( 'N', 'N', kwtop-istartm, jw, jw, one, a( istartm,kwtop ), lda, &
                        zc, ldzc, zero, work,kwtop-istartm )
              call stdlib_dlacpy( 'ALL', kwtop-istartm, jw, work, kwtop-istartm,a( istartm, kwtop &
                        ), lda )
              call stdlib_dgemm( 'N', 'N', kwtop-istartm, jw, jw, one, b( istartm,kwtop ), ldb, &
                        zc, ldzc, zero, work,kwtop-istartm )
              call stdlib_dlacpy( 'ALL', kwtop-istartm, jw, work, kwtop-istartm,b( istartm, kwtop &
                        ), ldb )
           end if
           if ( ilz ) then
              call stdlib_dgemm( 'N', 'N', n, jw, jw, one, z( 1_ilp, kwtop ), ldz, zc,ldzc, zero, &
                        work, n )
              call stdlib_dlacpy( 'ALL', n, jw, work, n, z( 1_ilp, kwtop ), ldz )
           end if
     end subroutine stdlib_dlaqz3


     pure module subroutine stdlib_claqz3( ilschur, ilq, ilz, n, ilo, ihi, nshifts,nblock_desired, alpha,&
     !! CLAQZ3 Executes a single multishift QZ sweep
                beta, a, lda, b, ldb,q, ldq, z, ldz, qc, ldqc, zc, ldzc, work,lwork, info )
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! function arguments
           logical(lk), intent( in ) :: ilschur, ilq, ilz
           integer(ilp), intent( in ) :: n, ilo, ihi, lda, ldb, ldq, ldz, lwork,nshifts, &
                     nblock_desired, ldqc, ldzc
           complex(sp), intent( inout ) :: a( lda, * ), b( ldb, * ), q( ldq, * ),z( ldz, * ), qc( &
                     ldqc, * ), zc( ldzc, * ), work( * ),alpha( * ), beta( * )
           integer(ilp), intent( out ) :: info
           
           ! ================================================================
           ! local scalars
           integer(ilp) :: i, j, ns, istartm, istopm, sheight, swidth, k, np, istartb, istopb, &
                     ishift, nblock, npos
           real(sp) :: safmin, safmax, c, scale
           complex(sp) :: temp, temp2, temp3, s
           info = 0_ilp
           if ( nblock_desired < nshifts+1 ) then
              info = -8_ilp
           end if
           if ( lwork ==-1_ilp ) then
              ! workspace query, quick return
              work( 1_ilp ) = n*nblock_desired
              return
           else if ( lwork < n*nblock_desired ) then
              info = -25_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CLAQZ3', -info )
              return
           end if
           ! executable statements
           ! get machine constants
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           safmax = one/safmin
           call stdlib_slabad( safmin, safmax )
           if ( ilo >= ihi ) then
              return
           end if
           if ( ilschur ) then
              istartm = 1_ilp
              istopm = n
           else
              istartm = ilo
              istopm = ihi
           end if
           ns = nshifts
           npos = max( nblock_desired-ns, 1_ilp )
           ! the following block introduces the shifts and chases
           ! them down one by one just enough to make space for
           ! the other shifts. the near-the-diagonal block is
           ! of size (ns+1) x ns.
           call stdlib_claset( 'FULL', ns+1, ns+1, czero, cone, qc, ldqc )
           call stdlib_claset( 'FULL', ns, ns, czero, cone, zc, ldzc )
           do i = 1, ns
              ! introduce the shift
              scale = sqrt( abs( alpha( i ) ) ) * sqrt( abs( beta( i ) ) )
              if( scale >= safmin .and. scale <= safmax ) then
                 alpha( i ) = alpha( i )/scale
                 beta( i ) = beta( i )/scale
              end if
              temp2 = beta( i )*a( ilo, ilo )-alpha( i )*b( ilo, ilo )
              temp3 = beta( i )*a( ilo+1, ilo )
              if ( abs( temp2 ) > safmax .or.abs( temp3 ) > safmax ) then
                 temp2 = cone
                 temp3 = czero
              end if
              call stdlib_clartg( temp2, temp3, c, s, temp )
              call stdlib_crot( ns, a( ilo, ilo ), lda, a( ilo+1, ilo ), lda, c,s )
              call stdlib_crot( ns, b( ilo, ilo ), ldb, b( ilo+1, ilo ), ldb, c,s )
              call stdlib_crot( ns+1, qc( 1_ilp, 1_ilp ), 1_ilp, qc( 1_ilp, 2_ilp ), 1_ilp, c, conjg( s ) )
              ! chase the shift down
              do j = 1, ns-i
                 call stdlib_claqz1( .true., .true., j, 1_ilp, ns, ihi-ilo+1, a( ilo,ilo ), lda, b( &
                           ilo, ilo ), ldb, ns+1, 1_ilp, qc,ldqc, ns, 1_ilp, zc, ldzc )
              end do
           end do
           ! update the rest of the pencil
           ! update a(ilo:ilo+ns,ilo+ns:istopm) and b(ilo:ilo+ns,ilo+ns:istopm)
           ! from the left with qc(1:ns+1,1:ns+1)'
           sheight = ns+1
           swidth = istopm-( ilo+ns )+1_ilp
           if ( swidth > 0_ilp ) then
              call stdlib_cgemm( 'C', 'N', sheight, swidth, sheight, cone, qc, ldqc,a( ilo, ilo+&
                        ns ), lda, czero, work, sheight )
              call stdlib_clacpy( 'ALL', sheight, swidth, work, sheight, a( ilo,ilo+ns ), lda )
                        
              call stdlib_cgemm( 'C', 'N', sheight, swidth, sheight, cone, qc, ldqc,b( ilo, ilo+&
                        ns ), ldb, czero, work, sheight )
              call stdlib_clacpy( 'ALL', sheight, swidth, work, sheight, b( ilo,ilo+ns ), ldb )
                        
           end if
           if ( ilq ) then
             call stdlib_cgemm( 'N', 'N', n, sheight, sheight, cone, q( 1_ilp, ilo ),ldq, qc, ldqc, &
                       czero, work, n )
              call stdlib_clacpy( 'ALL', n, sheight, work, n, q( 1_ilp, ilo ), ldq )
           end if
           ! update a(istartm:ilo-1,ilo:ilo+ns-1) and b(istartm:ilo-1,ilo:ilo+ns-1)
           ! from the right with zc(1:ns,1:ns)
           sheight = ilo-1-istartm+1
           swidth = ns
           if ( sheight > 0_ilp ) then
              call stdlib_cgemm( 'N', 'N', sheight, swidth, swidth, cone,a( istartm, ilo ), lda, &
                        zc, ldzc, czero, work,sheight )
              call stdlib_clacpy( 'ALL', sheight, swidth, work, sheight, a( istartm,ilo ), lda )
                        
              call stdlib_cgemm( 'N', 'N', sheight, swidth, swidth, cone,b( istartm, ilo ), ldb, &
                        zc, ldzc, czero, work,sheight )
              call stdlib_clacpy( 'ALL', sheight, swidth, work, sheight, b( istartm,ilo ), ldb )
                        
           end if
           if ( ilz ) then
              call stdlib_cgemm( 'N', 'N', n, swidth, swidth, cone, z( 1_ilp, ilo ),ldz, zc, ldzc, &
                        czero, work, n )
              call stdlib_clacpy( 'ALL', n, swidth, work, n, z( 1_ilp, ilo ), ldz )
           end if
           ! the following block chases the shifts down to the bottom
           ! right block. if possible, a shift is moved down npos
           ! positions at a time
           k = ilo
           do while ( k < ihi-ns )
              np = min( ihi-ns-k, npos )
              ! size of the near-the-diagonal block
              nblock = ns+np
              ! istartb points to the first row we will be updating
              istartb = k+1
              ! istopb points to the last column we will be updating
              istopb = k+nblock-1
              call stdlib_claset( 'FULL', ns+np, ns+np, czero, cone, qc, ldqc )
              call stdlib_claset( 'FULL', ns+np, ns+np, czero, cone, zc, ldzc )
              ! near the diagonal shift chase
              do i = ns-1, 0, -1
                 do j = 0, np-1
                    ! move down the block with index k+i+j, updating
                    ! the (ns+np x ns+np) block:
                    ! (k:k+ns+np,k:k+ns+np-1)
                    call stdlib_claqz1( .true., .true., k+i+j, istartb, istopb, ihi,a, lda, b, &
                              ldb, nblock, k+1, qc, ldqc,nblock, k, zc, ldzc )
                 end do
              end do
              ! update rest of the pencil
              ! update a(k+1:k+ns+np, k+ns+np:istopm) and
              ! b(k+1:k+ns+np, k+ns+np:istopm)
              ! from the left with qc(1:ns+np,1:ns+np)'
              sheight = ns+np
              swidth = istopm-( k+ns+np )+1_ilp
              if ( swidth > 0_ilp ) then
                 call stdlib_cgemm( 'C', 'N', sheight, swidth, sheight, cone, qc,ldqc, a( k+1, k+&
                           ns+np ), lda, czero, work,sheight )
                 call stdlib_clacpy( 'ALL', sheight, swidth, work, sheight, a( k+1,k+ns+np ), lda &
                           )
                 call stdlib_cgemm( 'C', 'N', sheight, swidth, sheight, cone, qc,ldqc, b( k+1, k+&
                           ns+np ), ldb, czero, work,sheight )
                 call stdlib_clacpy( 'ALL', sheight, swidth, work, sheight, b( k+1,k+ns+np ), ldb &
                           )
              end if
              if ( ilq ) then
                 call stdlib_cgemm( 'N', 'N', n, nblock, nblock, cone, q( 1_ilp, k+1 ),ldq, qc, ldqc, &
                           czero, work, n )
                 call stdlib_clacpy( 'ALL', n, nblock, work, n, q( 1_ilp, k+1 ), ldq )
              end if
              ! update a(istartm:k,k:k+ns+npos-1) and b(istartm:k,k:k+ns+npos-1)
              ! from the right with zc(1:ns+np,1:ns+np)
              sheight = k-istartm+1
              swidth = nblock
              if ( sheight > 0_ilp ) then
                 call stdlib_cgemm( 'N', 'N', sheight, swidth, swidth, cone,a( istartm, k ), lda, &
                           zc, ldzc, czero, work,sheight )
                 call stdlib_clacpy( 'ALL', sheight, swidth, work, sheight,a( istartm, k ), lda )
                           
                 call stdlib_cgemm( 'N', 'N', sheight, swidth, swidth, cone,b( istartm, k ), ldb, &
                           zc, ldzc, czero, work,sheight )
                 call stdlib_clacpy( 'ALL', sheight, swidth, work, sheight,b( istartm, k ), ldb )
                           
              end if
              if ( ilz ) then
                 call stdlib_cgemm( 'N', 'N', n, nblock, nblock, cone, z( 1_ilp, k ),ldz, zc, ldzc, &
                           czero, work, n )
                 call stdlib_clacpy( 'ALL', n, nblock, work, n, z( 1_ilp, k ), ldz )
              end if
              k = k+np
           end do
           ! the following block removes the shifts from the bottom right corner
           ! one by one. updates are initially applied to a(ihi-ns+1:ihi,ihi-ns:ihi).
           call stdlib_claset( 'FULL', ns, ns, czero, cone, qc, ldqc )
           call stdlib_claset( 'FULL', ns+1, ns+1, czero, cone, zc, ldzc )
           ! istartb points to the first row we will be updating
           istartb = ihi-ns+1
           ! istopb points to the last column we will be updating
           istopb = ihi
           do i = 1, ns
              ! chase the shift down to the bottom right corner
              do ishift = ihi-i, ihi-1
                 call stdlib_claqz1( .true., .true., ishift, istartb, istopb, ihi,a, lda, b, ldb, &
                           ns, ihi-ns+1, qc, ldqc, ns+1,ihi-ns, zc, ldzc )
              end do
           end do
           ! update rest of the pencil
           ! update a(ihi-ns+1:ihi, ihi+1:istopm)
           ! from the left with qc(1:ns,1:ns)'
           sheight = ns
           swidth = istopm-( ihi+1 )+1_ilp
           if ( swidth > 0_ilp ) then
              call stdlib_cgemm( 'C', 'N', sheight, swidth, sheight, cone, qc, ldqc,a( ihi-ns+1, &
                        ihi+1 ), lda, czero, work, sheight )
              call stdlib_clacpy( 'ALL', sheight, swidth, work, sheight,a( ihi-ns+1, ihi+1 ), lda &
                        )
              call stdlib_cgemm( 'C', 'N', sheight, swidth, sheight, cone, qc, ldqc,b( ihi-ns+1, &
                        ihi+1 ), ldb, czero, work, sheight )
              call stdlib_clacpy( 'ALL', sheight, swidth, work, sheight,b( ihi-ns+1, ihi+1 ), ldb &
                        )
           end if
           if ( ilq ) then
              call stdlib_cgemm( 'N', 'N', n, ns, ns, cone, q( 1_ilp, ihi-ns+1 ), ldq,qc, ldqc, czero,&
                         work, n )
              call stdlib_clacpy( 'ALL', n, ns, work, n, q( 1_ilp, ihi-ns+1 ), ldq )
           end if
           ! update a(istartm:ihi-ns,ihi-ns:ihi)
           ! from the right with zc(1:ns+1,1:ns+1)
           sheight = ihi-ns-istartm+1
           swidth = ns+1
           if ( sheight > 0_ilp ) then
              call stdlib_cgemm( 'N', 'N', sheight, swidth, swidth, cone,a( istartm, ihi-ns ), &
                        lda, zc, ldzc, czero, work,sheight )
              call stdlib_clacpy( 'ALL', sheight, swidth, work, sheight, a( istartm,ihi-ns ), lda &
                        )
              call stdlib_cgemm( 'N', 'N', sheight, swidth, swidth, cone,b( istartm, ihi-ns ), &
                        ldb, zc, ldzc, czero, work,sheight )
              call stdlib_clacpy( 'ALL', sheight, swidth, work, sheight, b( istartm,ihi-ns ), ldb &
                        )
           end if
           if ( ilz ) then
              call stdlib_cgemm( 'N', 'N', n, ns+1, ns+1, cone, z( 1_ilp, ihi-ns ), ldz,zc, ldzc, &
                        czero, work, n )
              call stdlib_clacpy( 'ALL', n, ns+1, work, n, z( 1_ilp, ihi-ns ), ldz )
           end if
     end subroutine stdlib_claqz3

     pure module subroutine stdlib_zlaqz3( ilschur, ilq, ilz, n, ilo, ihi, nshifts,nblock_desired, alpha,&
     !! ZLAQZ3 Executes a single multishift QZ sweep
                beta, a, lda, b, ldb,q, ldq, z, ldz, qc, ldqc, zc, ldzc, work,lwork, info )
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! function arguments
           logical(lk), intent( in ) :: ilschur, ilq, ilz
           integer(ilp), intent( in ) :: n, ilo, ihi, lda, ldb, ldq, ldz, lwork,nshifts, &
                     nblock_desired, ldqc, ldzc
           complex(dp), intent( inout ) :: a( lda, * ), b( ldb, * ), q( ldq,* ), z( ldz, * ), qc( &
                     ldqc, * ), zc( ldzc, * ), work( * ),alpha( * ), beta( * )
           integer(ilp), intent( out ) :: info
           
           ! ================================================================
           ! local scalars
           integer(ilp) :: i, j, ns, istartm, istopm, sheight, swidth, k, np, istartb, istopb, &
                     ishift, nblock, npos
           real(dp) :: safmin, safmax, c, scale
           complex(dp) :: temp, temp2, temp3, s
           info = 0_ilp
           if ( nblock_desired < nshifts+1 ) then
              info = -8_ilp
           end if
           if ( lwork ==-1_ilp ) then
              ! workspace query, quick return
              work( 1_ilp ) = n*nblock_desired
              return
           else if ( lwork < n*nblock_desired ) then
              info = -25_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZLAQZ3', -info )
              return
           end if
           ! executable statements
           ! get machine constants
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           safmax = one/safmin
           call stdlib_dlabad( safmin, safmax )
           if ( ilo >= ihi ) then
              return
           end if
           if ( ilschur ) then
              istartm = 1_ilp
              istopm = n
           else
              istartm = ilo
              istopm = ihi
           end if
           ns = nshifts
           npos = max( nblock_desired-ns, 1_ilp )
           ! the following block introduces the shifts and chases
           ! them down one by one just enough to make space for
           ! the other shifts. the near-the-diagonal block is
           ! of size (ns+1) x ns.
           call stdlib_zlaset( 'FULL', ns+1, ns+1, czero, cone, qc, ldqc )
           call stdlib_zlaset( 'FULL', ns, ns, czero, cone, zc, ldzc )
           do i = 1, ns
              ! introduce the shift
              scale = sqrt( abs( alpha( i ) ) ) * sqrt( abs( beta( i ) ) )
              if( scale >= safmin .and. scale <= safmax ) then
                 alpha( i ) = alpha( i )/scale
                 beta( i ) = beta( i )/scale
              end if
              temp2 = beta( i )*a( ilo, ilo )-alpha( i )*b( ilo, ilo )
              temp3 = beta( i )*a( ilo+1, ilo )
              if ( abs( temp2 ) > safmax .or.abs( temp3 ) > safmax ) then
                 temp2 = cone
                 temp3 = czero
              end if
              call stdlib_zlartg( temp2, temp3, c, s, temp )
              call stdlib_zrot( ns, a( ilo, ilo ), lda, a( ilo+1, ilo ), lda, c,s )
              call stdlib_zrot( ns, b( ilo, ilo ), ldb, b( ilo+1, ilo ), ldb, c,s )
              call stdlib_zrot( ns+1, qc( 1_ilp, 1_ilp ), 1_ilp, qc( 1_ilp, 2_ilp ), 1_ilp, c,conjg( s ) )
              ! chase the shift down
              do j = 1, ns-i
                 call stdlib_zlaqz1( .true., .true., j, 1_ilp, ns, ihi-ilo+1, a( ilo,ilo ), lda, b( &
                           ilo, ilo ), ldb, ns+1, 1_ilp, qc,ldqc, ns, 1_ilp, zc, ldzc )
              end do
           end do
           ! update the rest of the pencil
           ! update a(ilo:ilo+ns,ilo+ns:istopm) and b(ilo:ilo+ns,ilo+ns:istopm)
           ! from the left with qc(1:ns+1,1:ns+1)'
           sheight = ns+1
           swidth = istopm-( ilo+ns )+1_ilp
           if ( swidth > 0_ilp ) then
              call stdlib_zgemm( 'C', 'N', sheight, swidth, sheight, cone, qc, ldqc,a( ilo, ilo+&
                        ns ), lda, czero, work, sheight )
              call stdlib_zlacpy( 'ALL', sheight, swidth, work, sheight, a( ilo,ilo+ns ), lda )
                        
              call stdlib_zgemm( 'C', 'N', sheight, swidth, sheight, cone, qc, ldqc,b( ilo, ilo+&
                        ns ), ldb, czero, work, sheight )
              call stdlib_zlacpy( 'ALL', sheight, swidth, work, sheight, b( ilo,ilo+ns ), ldb )
                        
           end if
           if ( ilq ) then
              call stdlib_zgemm( 'N', 'N', n, sheight, sheight, cone, q( 1_ilp, ilo ),ldq, qc, ldqc, &
                        czero, work, n )
              call stdlib_zlacpy( 'ALL', n, sheight, work, n, q( 1_ilp, ilo ), ldq )
           end if
           ! update a(istartm:ilo-1,ilo:ilo+ns-1) and b(istartm:ilo-1,ilo:ilo+ns-1)
           ! from the right with zc(1:ns,1:ns)
           sheight = ilo-1-istartm+1
           swidth = ns
           if ( sheight > 0_ilp ) then
              call stdlib_zgemm( 'N', 'N', sheight, swidth, swidth, cone,a( istartm, ilo ), lda, &
                        zc, ldzc, czero, work,sheight )
              call stdlib_zlacpy( 'ALL', sheight, swidth, work, sheight, a( istartm,ilo ), lda )
                        
              call stdlib_zgemm( 'N', 'N', sheight, swidth, swidth, cone,b( istartm, ilo ), ldb, &
                        zc, ldzc, czero, work,sheight )
              call stdlib_zlacpy( 'ALL', sheight, swidth, work, sheight, b( istartm,ilo ), ldb )
                        
           end if
           if ( ilz ) then
              call stdlib_zgemm( 'N', 'N', n, swidth, swidth, cone, z( 1_ilp, ilo ),ldz, zc, ldzc, &
                        czero, work, n )
              call stdlib_zlacpy( 'ALL', n, swidth, work, n, z( 1_ilp, ilo ), ldz )
           end if
           ! the following block chases the shifts down to the bottom
           ! right block. if possible, a shift is moved down npos
           ! positions at a time
           k = ilo
           do while ( k < ihi-ns )
              np = min( ihi-ns-k, npos )
              ! size of the near-the-diagonal block
              nblock = ns+np
              ! istartb points to the first row we will be updating
              istartb = k+1
              ! istopb points to the last column we will be updating
              istopb = k+nblock-1
              call stdlib_zlaset( 'FULL', ns+np, ns+np, czero, cone, qc, ldqc )
              call stdlib_zlaset( 'FULL', ns+np, ns+np, czero, cone, zc, ldzc )
              ! near the diagonal shift chase
              do i = ns-1, 0, -1
                 do j = 0, np-1
                    ! move down the block with index k+i+j, updating
                    ! the (ns+np x ns+np) block:
                    ! (k:k+ns+np,k:k+ns+np-1)
                    call stdlib_zlaqz1( .true., .true., k+i+j, istartb, istopb, ihi,a, lda, b, &
                              ldb, nblock, k+1, qc, ldqc,nblock, k, zc, ldzc )
                 end do
              end do
              ! update rest of the pencil
              ! update a(k+1:k+ns+np, k+ns+np:istopm) and
              ! b(k+1:k+ns+np, k+ns+np:istopm)
              ! from the left with qc(1:ns+np,1:ns+np)'
              sheight = ns+np
              swidth = istopm-( k+ns+np )+1_ilp
              if ( swidth > 0_ilp ) then
                 call stdlib_zgemm( 'C', 'N', sheight, swidth, sheight, cone, qc,ldqc, a( k+1, k+&
                           ns+np ), lda, czero, work,sheight )
                 call stdlib_zlacpy( 'ALL', sheight, swidth, work, sheight, a( k+1,k+ns+np ), lda &
                           )
                 call stdlib_zgemm( 'C', 'N', sheight, swidth, sheight, cone, qc,ldqc, b( k+1, k+&
                           ns+np ), ldb, czero, work,sheight )
                 call stdlib_zlacpy( 'ALL', sheight, swidth, work, sheight, b( k+1,k+ns+np ), ldb &
                           )
              end if
              if ( ilq ) then
                 call stdlib_zgemm( 'N', 'N', n, nblock, nblock, cone, q( 1_ilp, k+1 ),ldq, qc, ldqc, &
                           czero, work, n )
                 call stdlib_zlacpy( 'ALL', n, nblock, work, n, q( 1_ilp, k+1 ), ldq )
              end if
              ! update a(istartm:k,k:k+ns+npos-1) and b(istartm:k,k:k+ns+npos-1)
              ! from the right with zc(1:ns+np,1:ns+np)
              sheight = k-istartm+1
              swidth = nblock
              if ( sheight > 0_ilp ) then
                 call stdlib_zgemm( 'N', 'N', sheight, swidth, swidth, cone,a( istartm, k ), lda, &
                           zc, ldzc, czero, work,sheight )
                 call stdlib_zlacpy( 'ALL', sheight, swidth, work, sheight,a( istartm, k ), lda )
                           
                 call stdlib_zgemm( 'N', 'N', sheight, swidth, swidth, cone,b( istartm, k ), ldb, &
                           zc, ldzc, czero, work,sheight )
                 call stdlib_zlacpy( 'ALL', sheight, swidth, work, sheight,b( istartm, k ), ldb )
                           
              end if
              if ( ilz ) then
                 call stdlib_zgemm( 'N', 'N', n, nblock, nblock, cone, z( 1_ilp, k ),ldz, zc, ldzc, &
                           czero, work, n )
                 call stdlib_zlacpy( 'ALL', n, nblock, work, n, z( 1_ilp, k ), ldz )
              end if
              k = k+np
           end do
           ! the following block removes the shifts from the bottom right corner
           ! one by one. updates are initially applied to a(ihi-ns+1:ihi,ihi-ns:ihi).
           call stdlib_zlaset( 'FULL', ns, ns, czero, cone, qc, ldqc )
           call stdlib_zlaset( 'FULL', ns+1, ns+1, czero, cone, zc, ldzc )
           ! istartb points to the first row we will be updating
           istartb = ihi-ns+1
           ! istopb points to the last column we will be updating
           istopb = ihi
           do i = 1, ns
              ! chase the shift down to the bottom right corner
              do ishift = ihi-i, ihi-1
                 call stdlib_zlaqz1( .true., .true., ishift, istartb, istopb, ihi,a, lda, b, ldb, &
                           ns, ihi-ns+1, qc, ldqc, ns+1,ihi-ns, zc, ldzc )
              end do
           end do
           ! update rest of the pencil
           ! update a(ihi-ns+1:ihi, ihi+1:istopm)
           ! from the left with qc(1:ns,1:ns)'
           sheight = ns
           swidth = istopm-( ihi+1 )+1_ilp
           if ( swidth > 0_ilp ) then
              call stdlib_zgemm( 'C', 'N', sheight, swidth, sheight, cone, qc, ldqc,a( ihi-ns+1, &
                        ihi+1 ), lda, czero, work, sheight )
              call stdlib_zlacpy( 'ALL', sheight, swidth, work, sheight,a( ihi-ns+1, ihi+1 ), lda &
                        )
              call stdlib_zgemm( 'C', 'N', sheight, swidth, sheight, cone, qc, ldqc,b( ihi-ns+1, &
                        ihi+1 ), ldb, czero, work, sheight )
              call stdlib_zlacpy( 'ALL', sheight, swidth, work, sheight,b( ihi-ns+1, ihi+1 ), ldb &
                        )
           end if
           if ( ilq ) then
              call stdlib_zgemm( 'N', 'N', n, ns, ns, cone, q( 1_ilp, ihi-ns+1 ), ldq,qc, ldqc, czero,&
                         work, n )
              call stdlib_zlacpy( 'ALL', n, ns, work, n, q( 1_ilp, ihi-ns+1 ), ldq )
           end if
           ! update a(istartm:ihi-ns,ihi-ns:ihi)
           ! from the right with zc(1:ns+1,1:ns+1)
           sheight = ihi-ns-istartm+1
           swidth = ns+1
           if ( sheight > 0_ilp ) then
              call stdlib_zgemm( 'N', 'N', sheight, swidth, swidth, cone,a( istartm, ihi-ns ), &
                        lda, zc, ldzc, czero, work,sheight )
              call stdlib_zlacpy( 'ALL', sheight, swidth, work, sheight, a( istartm,ihi-ns ), lda &
                        )
              call stdlib_zgemm( 'N', 'N', sheight, swidth, swidth, cone,b( istartm, ihi-ns ), &
                        ldb, zc, ldzc, czero, work,sheight )
              call stdlib_zlacpy( 'ALL', sheight, swidth, work, sheight, b( istartm,ihi-ns ), ldb &
                        )
           end if
           if ( ilz ) then
              call stdlib_zgemm( 'N', 'N', n, ns+1, ns+1, cone, z( 1_ilp, ihi-ns ), ldz,zc, ldzc, &
                        czero, work, n )
              call stdlib_zlacpy( 'ALL', n, ns+1, work, n, z( 1_ilp, ihi-ns ), ldz )
           end if
     end subroutine stdlib_zlaqz3




     pure module subroutine stdlib_slaqz4( ilschur, ilq, ilz, n, ilo, ihi, nshifts,nblock_desired, sr, &
     !! SLAQZ4 Executes a single multishift QZ sweep
               si, ss, a, lda, b, ldb, q,ldq, z, ldz, qc, ldqc, zc, ldzc, work, lwork,info )
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! function arguments
           logical(lk), intent( in ) :: ilschur, ilq, ilz
           integer(ilp), intent( in ) :: n, ilo, ihi, lda, ldb, ldq, ldz, lwork,nshifts, &
                     nblock_desired, ldqc, ldzc
           real(sp), intent( inout ) :: a( lda, * ), b( ldb, * ), q( ldq, * ),z( ldz, * ), qc( &
                     ldqc, * ), zc( ldzc, * ), work( * ), sr( * ),si( * ), ss( * )
           integer(ilp), intent( out ) :: info
           ! ================================================================
           ! local variables
           integer(ilp) :: i, j, ns, istartm, istopm, sheight, swidth, k, np, istartb, istopb, &
                     ishift, nblock, npos
           real(sp) :: temp, v(3_ilp), c1, s1, c2, s2, swap
           info = 0_ilp
           if ( nblock_desired < nshifts+1 ) then
              info = -8_ilp
           end if
           if ( lwork ==-1_ilp ) then
              ! workspace query, quick return
              work( 1_ilp ) = n*nblock_desired
              return
           else if ( lwork < n*nblock_desired ) then
              info = -25_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SLAQZ4', -info )
              return
           end if
           ! executable statements
           if ( nshifts < 2_ilp ) then
              return
           end if
           if ( ilo >= ihi ) then
              return
           end if
           if ( ilschur ) then
              istartm = 1_ilp
              istopm = n
           else
              istartm = ilo
              istopm = ihi
           end if
           ! shuffle shifts into pairs of real shifts and pairs
           ! of complex conjugate shifts assuming complex
           ! conjugate shifts are already adjacent to one
           ! another
           do i = 1, nshifts-2, 2
              if( si( i )/=-si( i+1 ) ) then
                 swap = sr( i )
                 sr( i ) = sr( i+1 )
                 sr( i+1 ) = sr( i+2 )
                 sr( i+2 ) = swap
                 swap = si( i )
                 si( i ) = si( i+1 )
                 si( i+1 ) = si( i+2 )
                 si( i+2 ) = swap
                 swap = ss( i )
                 ss( i ) = ss( i+1 )
                 ss( i+1 ) = ss( i+2 )
                 ss( i+2 ) = swap
              end if
           end do
           ! nshfts is supposed to be even, but if it is odd,
           ! then simply reduce it by one.  the shuffle above
           ! ensures that the dropped shift is real and that
           ! the remaining shifts are paired.
           ns = nshifts-mod( nshifts, 2_ilp )
           npos = max( nblock_desired-ns, 1_ilp )
           ! the following block introduces the shifts and chases
           ! them down one by one just enough to make space for
           ! the other shifts. the near-the-diagonal block is
           ! of size (ns+1) x ns.
           call stdlib_slaset( 'FULL', ns+1, ns+1, zero, one, qc, ldqc )
           call stdlib_slaset( 'FULL', ns, ns, zero, one, zc, ldzc )
           do i = 1, ns, 2
              ! introduce the shift
              call stdlib_slaqz1( a( ilo, ilo ), lda, b( ilo, ilo ), ldb, sr( i ),sr( i+1 ), si( &
                        i ), ss( i ), ss( i+1 ), v )
              temp = v( 2_ilp )
              call stdlib_slartg( temp, v( 3_ilp ), c1, s1, v( 2_ilp ) )
              call stdlib_slartg( v( 1_ilp ), v( 2_ilp ), c2, s2, temp )
              call stdlib_srot( ns, a( ilo+1, ilo ), lda, a( ilo+2, ilo ), lda, c1,s1 )
              call stdlib_srot( ns, a( ilo, ilo ), lda, a( ilo+1, ilo ), lda, c2,s2 )
              call stdlib_srot( ns, b( ilo+1, ilo ), ldb, b( ilo+2, ilo ), ldb, c1,s1 )
              call stdlib_srot( ns, b( ilo, ilo ), ldb, b( ilo+1, ilo ), ldb, c2,s2 )
              call stdlib_srot( ns+1, qc( 1_ilp, 2_ilp ), 1_ilp, qc( 1_ilp, 3_ilp ), 1_ilp, c1, s1 )
              call stdlib_srot( ns+1, qc( 1_ilp, 1_ilp ), 1_ilp, qc( 1_ilp, 2_ilp ), 1_ilp, c2, s2 )
              ! chase the shift down
              do j = 1, ns-1-i
                 call stdlib_slaqz2( .true., .true., j, 1_ilp, ns, ihi-ilo+1, a( ilo,ilo ), lda, b( &
                           ilo, ilo ), ldb, ns+1, 1_ilp, qc,ldqc, ns, 1_ilp, zc, ldzc )
              end do
           end do
           ! update the rest of the pencil
           ! update a(ilo:ilo+ns,ilo+ns:istopm) and b(ilo:ilo+ns,ilo+ns:istopm)
           ! from the left with qc(1:ns+1,1:ns+1)'
           sheight = ns+1
           swidth = istopm-( ilo+ns )+1_ilp
           if ( swidth > 0_ilp ) then
              call stdlib_sgemm( 'T', 'N', sheight, swidth, sheight, one, qc, ldqc,a( ilo, ilo+ns &
                        ), lda, zero, work, sheight )
              call stdlib_slacpy( 'ALL', sheight, swidth, work, sheight, a( ilo,ilo+ns ), lda )
                        
              call stdlib_sgemm( 'T', 'N', sheight, swidth, sheight, one, qc, ldqc,b( ilo, ilo+ns &
                        ), ldb, zero, work, sheight )
              call stdlib_slacpy( 'ALL', sheight, swidth, work, sheight, b( ilo,ilo+ns ), ldb )
                        
           end if
           if ( ilq ) then
              call stdlib_sgemm( 'N', 'N', n, sheight, sheight, one, q( 1_ilp, ilo ),ldq, qc, ldqc, &
                        zero, work, n )
              call stdlib_slacpy( 'ALL', n, sheight, work, n, q( 1_ilp, ilo ), ldq )
           end if
           ! update a(istartm:ilo-1,ilo:ilo+ns-1) and b(istartm:ilo-1,ilo:ilo+ns-1)
           ! from the right with zc(1:ns,1:ns)
           sheight = ilo-1-istartm+1
           swidth = ns
           if ( sheight > 0_ilp ) then
              call stdlib_sgemm( 'N', 'N', sheight, swidth, swidth, one, a( istartm,ilo ), lda, &
                        zc, ldzc, zero, work, sheight )
              call stdlib_slacpy( 'ALL', sheight, swidth, work, sheight, a( istartm,ilo ), lda )
                        
              call stdlib_sgemm( 'N', 'N', sheight, swidth, swidth, one, b( istartm,ilo ), ldb, &
                        zc, ldzc, zero, work, sheight )
              call stdlib_slacpy( 'ALL', sheight, swidth, work, sheight, b( istartm,ilo ), ldb )
                        
           end if
           if ( ilz ) then
              call stdlib_sgemm( 'N', 'N', n, swidth, swidth, one, z( 1_ilp, ilo ), ldz,zc, ldzc, &
                        zero, work, n )
              call stdlib_slacpy( 'ALL', n, swidth, work, n, z( 1_ilp, ilo ), ldz )
           end if
           ! the following block chases the shifts down to the bottom
           ! right block. if possible, a shift is moved down npos
           ! positions at a time
           k = ilo
           do while ( k < ihi-ns )
              np = min( ihi-ns-k, npos )
              ! size of the near-the-diagonal block
              nblock = ns+np
              ! istartb points to the first row we will be updating
              istartb = k+1
              ! istopb points to the last column we will be updating
              istopb = k+nblock-1
              call stdlib_slaset( 'FULL', ns+np, ns+np, zero, one, qc, ldqc )
              call stdlib_slaset( 'FULL', ns+np, ns+np, zero, one, zc, ldzc )
              ! near the diagonal shift chase
              do i = ns-1, 0, -2
                 do j = 0, np-1
                    ! move down the block with index k+i+j-1, updating
                    ! the (ns+np x ns+np) block:
                    ! (k:k+ns+np,k:k+ns+np-1)
                    call stdlib_slaqz2( .true., .true., k+i+j-1, istartb, istopb,ihi, a, lda, b, &
                              ldb, nblock, k+1, qc, ldqc,nblock, k, zc, ldzc )
                 end do
              end do
              ! update rest of the pencil
              ! update a(k+1:k+ns+np, k+ns+np:istopm) and
              ! b(k+1:k+ns+np, k+ns+np:istopm)
              ! from the left with qc(1:ns+np,1:ns+np)'
              sheight = ns+np
              swidth = istopm-( k+ns+np )+1_ilp
              if ( swidth > 0_ilp ) then
                 call stdlib_sgemm( 'T', 'N', sheight, swidth, sheight, one, qc,ldqc, a( k+1, k+&
                           ns+np ), lda, zero, work,sheight )
                 call stdlib_slacpy( 'ALL', sheight, swidth, work, sheight, a( k+1,k+ns+np ), lda &
                           )
                 call stdlib_sgemm( 'T', 'N', sheight, swidth, sheight, one, qc,ldqc, b( k+1, k+&
                           ns+np ), ldb, zero, work,sheight )
                 call stdlib_slacpy( 'ALL', sheight, swidth, work, sheight, b( k+1,k+ns+np ), ldb &
                           )
              end if
              if ( ilq ) then
                 call stdlib_sgemm( 'N', 'N', n, nblock, nblock, one, q( 1_ilp, k+1 ),ldq, qc, ldqc, &
                           zero, work, n )
                 call stdlib_slacpy( 'ALL', n, nblock, work, n, q( 1_ilp, k+1 ), ldq )
              end if
              ! update a(istartm:k,k:k+ns+npos-1) and b(istartm:k,k:k+ns+npos-1)
              ! from the right with zc(1:ns+np,1:ns+np)
              sheight = k-istartm+1
              swidth = nblock
              if ( sheight > 0_ilp ) then
                 call stdlib_sgemm( 'N', 'N', sheight, swidth, swidth, one,a( istartm, k ), lda, &
                           zc, ldzc, zero, work,sheight )
                 call stdlib_slacpy( 'ALL', sheight, swidth, work, sheight,a( istartm, k ), lda )
                           
                 call stdlib_sgemm( 'N', 'N', sheight, swidth, swidth, one,b( istartm, k ), ldb, &
                           zc, ldzc, zero, work,sheight )
                 call stdlib_slacpy( 'ALL', sheight, swidth, work, sheight,b( istartm, k ), ldb )
                           
              end if
              if ( ilz ) then
                 call stdlib_sgemm( 'N', 'N', n, nblock, nblock, one, z( 1_ilp, k ),ldz, zc, ldzc, &
                           zero, work, n )
                 call stdlib_slacpy( 'ALL', n, nblock, work, n, z( 1_ilp, k ), ldz )
              end if
              k = k+np
           end do
           ! the following block removes the shifts from the bottom right corner
           ! one by one. updates are initially applied to a(ihi-ns+1:ihi,ihi-ns:ihi).
           call stdlib_slaset( 'FULL', ns, ns, zero, one, qc, ldqc )
           call stdlib_slaset( 'FULL', ns+1, ns+1, zero, one, zc, ldzc )
           ! istartb points to the first row we will be updating
           istartb = ihi-ns+1
           ! istopb points to the last column we will be updating
           istopb = ihi
           do i = 1, ns, 2
              ! chase the shift down to the bottom right corner
              do ishift = ihi-i-1, ihi-2
                 call stdlib_slaqz2( .true., .true., ishift, istartb, istopb, ihi,a, lda, b, ldb, &
                           ns, ihi-ns+1, qc, ldqc, ns+1,ihi-ns, zc, ldzc )
              end do
           end do
           ! update rest of the pencil
           ! update a(ihi-ns+1:ihi, ihi+1:istopm)
           ! from the left with qc(1:ns,1:ns)'
           sheight = ns
           swidth = istopm-( ihi+1 )+1_ilp
           if ( swidth > 0_ilp ) then
              call stdlib_sgemm( 'T', 'N', sheight, swidth, sheight, one, qc, ldqc,a( ihi-ns+1, &
                        ihi+1 ), lda, zero, work, sheight )
              call stdlib_slacpy( 'ALL', sheight, swidth, work, sheight,a( ihi-ns+1, ihi+1 ), lda &
                        )
              call stdlib_sgemm( 'T', 'N', sheight, swidth, sheight, one, qc, ldqc,b( ihi-ns+1, &
                        ihi+1 ), ldb, zero, work, sheight )
              call stdlib_slacpy( 'ALL', sheight, swidth, work, sheight,b( ihi-ns+1, ihi+1 ), ldb &
                        )
           end if
           if ( ilq ) then
              call stdlib_sgemm( 'N', 'N', n, ns, ns, one, q( 1_ilp, ihi-ns+1 ), ldq,qc, ldqc, zero, &
                        work, n )
              call stdlib_slacpy( 'ALL', n, ns, work, n, q( 1_ilp, ihi-ns+1 ), ldq )
           end if
           ! update a(istartm:ihi-ns,ihi-ns:ihi)
           ! from the right with zc(1:ns+1,1:ns+1)
           sheight = ihi-ns-istartm+1
           swidth = ns+1
           if ( sheight > 0_ilp ) then
              call stdlib_sgemm( 'N', 'N', sheight, swidth, swidth, one, a( istartm,ihi-ns ), lda,&
                         zc, ldzc, zero, work, sheight )
              call stdlib_slacpy( 'ALL', sheight, swidth, work, sheight, a( istartm,ihi-ns ), lda &
                        )
              call stdlib_sgemm( 'N', 'N', sheight, swidth, swidth, one, b( istartm,ihi-ns ), ldb,&
                         zc, ldzc, zero, work, sheight )
              call stdlib_slacpy( 'ALL', sheight, swidth, work, sheight, b( istartm,ihi-ns ), ldb &
                        )
           end if
           if ( ilz ) then
           call stdlib_sgemm( 'N', 'N', n, ns+1, ns+1, one, z( 1_ilp, ihi-ns ), ldz, zc,ldzc, zero, &
                     work, n )
              call stdlib_slacpy( 'ALL', n, ns+1, work, n, z( 1_ilp, ihi-ns ), ldz )
           end if
     end subroutine stdlib_slaqz4

     pure module subroutine stdlib_dlaqz4( ilschur, ilq, ilz, n, ilo, ihi, nshifts,nblock_desired, sr, &
     !! DLAQZ4 Executes a single multishift QZ sweep
               si, ss, a, lda, b, ldb, q,ldq, z, ldz, qc, ldqc, zc, ldzc, work, lwork,info )
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! function arguments
           logical(lk), intent( in ) :: ilschur, ilq, ilz
           integer(ilp), intent( in ) :: n, ilo, ihi, lda, ldb, ldq, ldz, lwork,nshifts, &
                     nblock_desired, ldqc, ldzc
           real(dp), intent( inout ) :: a( lda, * ), b( ldb, * ),q( ldq, * ), z( ldz, * ), qc( &
                     ldqc, * ),zc( ldzc, * ), work( * ), sr( * ), si( * ),ss( * )
           integer(ilp), intent( out ) :: info
           ! ================================================================
           ! local scalars
           integer(ilp) :: i, j, ns, istartm, istopm, sheight, swidth, k, np, istartb, istopb, &
                     ishift, nblock, npos
           real(dp) :: temp, v(3_ilp), c1, s1, c2, s2, swap
           info = 0_ilp
           if ( nblock_desired < nshifts+1 ) then
              info = -8_ilp
           end if
           if ( lwork ==-1_ilp ) then
              ! workspace query, quick return
              work( 1_ilp ) = n*nblock_desired
              return
           else if ( lwork < n*nblock_desired ) then
              info = -25_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DLAQZ4', -info )
              return
           end if
           ! executable statements
           if ( nshifts < 2_ilp ) then
              return
           end if
           if ( ilo >= ihi ) then
              return
           end if
           if ( ilschur ) then
              istartm = 1_ilp
              istopm = n
           else
              istartm = ilo
              istopm = ihi
           end if
           ! shuffle shifts into pairs of real shifts and pairs
           ! of complex conjugate shifts assuming complex
           ! conjugate shifts are already adjacent to one
           ! another
           do i = 1, nshifts-2, 2
              if( si( i )/=-si( i+1 ) ) then
                 swap = sr( i )
                 sr( i ) = sr( i+1 )
                 sr( i+1 ) = sr( i+2 )
                 sr( i+2 ) = swap
                 swap = si( i )
                 si( i ) = si( i+1 )
                 si( i+1 ) = si( i+2 )
                 si( i+2 ) = swap
                 swap = ss( i )
                 ss( i ) = ss( i+1 )
                 ss( i+1 ) = ss( i+2 )
                 ss( i+2 ) = swap
              end if
           end do
           ! nshfts is supposed to be even, but if it is odd,
           ! then simply reduce it by one.  the shuffle above
           ! ensures that the dropped shift is real and that
           ! the remaining shifts are paired.
           ns = nshifts-mod( nshifts, 2_ilp )
           npos = max( nblock_desired-ns, 1_ilp )
           ! the following block introduces the shifts and chases
           ! them down one by one just enough to make space for
           ! the other shifts. the near-the-diagonal block is
           ! of size (ns+1) x ns.
           call stdlib_dlaset( 'FULL', ns+1, ns+1, zero, one, qc, ldqc )
           call stdlib_dlaset( 'FULL', ns, ns, zero, one, zc, ldzc )
           do i = 1, ns, 2
              ! introduce the shift
              call stdlib_dlaqz1( a( ilo, ilo ), lda, b( ilo, ilo ), ldb, sr( i ),sr( i+1 ), si( &
                        i ), ss( i ), ss( i+1 ), v )
              temp = v( 2_ilp )
              call stdlib_dlartg( temp, v( 3_ilp ), c1, s1, v( 2_ilp ) )
              call stdlib_dlartg( v( 1_ilp ), v( 2_ilp ), c2, s2, temp )
              call stdlib_drot( ns, a( ilo+1, ilo ), lda, a( ilo+2, ilo ), lda, c1,s1 )
              call stdlib_drot( ns, a( ilo, ilo ), lda, a( ilo+1, ilo ), lda, c2,s2 )
              call stdlib_drot( ns, b( ilo+1, ilo ), ldb, b( ilo+2, ilo ), ldb, c1,s1 )
              call stdlib_drot( ns, b( ilo, ilo ), ldb, b( ilo+1, ilo ), ldb, c2,s2 )
              call stdlib_drot( ns+1, qc( 1_ilp, 2_ilp ), 1_ilp, qc( 1_ilp, 3_ilp ), 1_ilp, c1, s1 )
              call stdlib_drot( ns+1, qc( 1_ilp, 1_ilp ), 1_ilp, qc( 1_ilp, 2_ilp ), 1_ilp, c2, s2 )
              ! chase the shift down
              do j = 1, ns-1-i
                 call stdlib_dlaqz2( .true., .true., j, 1_ilp, ns, ihi-ilo+1, a( ilo,ilo ), lda, b( &
                           ilo, ilo ), ldb, ns+1, 1_ilp, qc,ldqc, ns, 1_ilp, zc, ldzc )
              end do
           end do
           ! update the rest of the pencil
           ! update a(ilo:ilo+ns,ilo+ns:istopm) and b(ilo:ilo+ns,ilo+ns:istopm)
           ! from the left with qc(1:ns+1,1:ns+1)'
           sheight = ns+1
           swidth = istopm-( ilo+ns )+1_ilp
           if ( swidth > 0_ilp ) then
              call stdlib_dgemm( 'T', 'N', sheight, swidth, sheight, one, qc, ldqc,a( ilo, ilo+ns &
                        ), lda, zero, work, sheight )
              call stdlib_dlacpy( 'ALL', sheight, swidth, work, sheight, a( ilo,ilo+ns ), lda )
                        
              call stdlib_dgemm( 'T', 'N', sheight, swidth, sheight, one, qc, ldqc,b( ilo, ilo+ns &
                        ), ldb, zero, work, sheight )
              call stdlib_dlacpy( 'ALL', sheight, swidth, work, sheight, b( ilo,ilo+ns ), ldb )
                        
           end if
           if ( ilq ) then
              call stdlib_dgemm( 'N', 'N', n, sheight, sheight, one, q( 1_ilp, ilo ),ldq, qc, ldqc, &
                        zero, work, n )
              call stdlib_dlacpy( 'ALL', n, sheight, work, n, q( 1_ilp, ilo ), ldq )
           end if
           ! update a(istartm:ilo-1,ilo:ilo+ns-1) and b(istartm:ilo-1,ilo:ilo+ns-1)
           ! from the right with zc(1:ns,1:ns)
           sheight = ilo-1-istartm+1
           swidth = ns
           if ( sheight > 0_ilp ) then
              call stdlib_dgemm( 'N', 'N', sheight, swidth, swidth, one, a( istartm,ilo ), lda, &
                        zc, ldzc, zero, work, sheight )
              call stdlib_dlacpy( 'ALL', sheight, swidth, work, sheight, a( istartm,ilo ), lda )
                        
              call stdlib_dgemm( 'N', 'N', sheight, swidth, swidth, one, b( istartm,ilo ), ldb, &
                        zc, ldzc, zero, work, sheight )
              call stdlib_dlacpy( 'ALL', sheight, swidth, work, sheight, b( istartm,ilo ), ldb )
                        
           end if
           if ( ilz ) then
              call stdlib_dgemm( 'N', 'N', n, swidth, swidth, one, z( 1_ilp, ilo ), ldz,zc, ldzc, &
                        zero, work, n )
              call stdlib_dlacpy( 'ALL', n, swidth, work, n, z( 1_ilp, ilo ), ldz )
           end if
           ! the following block chases the shifts down to the bottom
           ! right block. if possible, a shift is moved down npos
           ! positions at a time
           k = ilo
           do while ( k < ihi-ns )
              np = min( ihi-ns-k, npos )
              ! size of the near-the-diagonal block
              nblock = ns+np
              ! istartb points to the first row we will be updating
              istartb = k+1
              ! istopb points to the last column we will be updating
              istopb = k+nblock-1
              call stdlib_dlaset( 'FULL', ns+np, ns+np, zero, one, qc, ldqc )
              call stdlib_dlaset( 'FULL', ns+np, ns+np, zero, one, zc, ldzc )
              ! near the diagonal shift chase
              do i = ns-1, 0, -2
                 do j = 0, np-1
                    ! move down the block with index k+i+j-1, updating
                    ! the (ns+np x ns+np) block:
                    ! (k:k+ns+np,k:k+ns+np-1)
                    call stdlib_dlaqz2( .true., .true., k+i+j-1, istartb, istopb,ihi, a, lda, b, &
                              ldb, nblock, k+1, qc, ldqc,nblock, k, zc, ldzc )
                 end do
              end do
              ! update rest of the pencil
              ! update a(k+1:k+ns+np, k+ns+np:istopm) and
              ! b(k+1:k+ns+np, k+ns+np:istopm)
              ! from the left with qc(1:ns+np,1:ns+np)'
              sheight = ns+np
              swidth = istopm-( k+ns+np )+1_ilp
              if ( swidth > 0_ilp ) then
                 call stdlib_dgemm( 'T', 'N', sheight, swidth, sheight, one, qc,ldqc, a( k+1, k+&
                           ns+np ), lda, zero, work,sheight )
                 call stdlib_dlacpy( 'ALL', sheight, swidth, work, sheight, a( k+1,k+ns+np ), lda &
                           )
                 call stdlib_dgemm( 'T', 'N', sheight, swidth, sheight, one, qc,ldqc, b( k+1, k+&
                           ns+np ), ldb, zero, work,sheight )
                 call stdlib_dlacpy( 'ALL', sheight, swidth, work, sheight, b( k+1,k+ns+np ), ldb &
                           )
              end if
              if ( ilq ) then
                 call stdlib_dgemm( 'N', 'N', n, nblock, nblock, one, q( 1_ilp, k+1 ),ldq, qc, ldqc, &
                           zero, work, n )
                 call stdlib_dlacpy( 'ALL', n, nblock, work, n, q( 1_ilp, k+1 ), ldq )
              end if
              ! update a(istartm:k,k:k+ns+npos-1) and b(istartm:k,k:k+ns+npos-1)
              ! from the right with zc(1:ns+np,1:ns+np)
              sheight = k-istartm+1
              swidth = nblock
              if ( sheight > 0_ilp ) then
                 call stdlib_dgemm( 'N', 'N', sheight, swidth, swidth, one,a( istartm, k ), lda, &
                           zc, ldzc, zero, work,sheight )
                 call stdlib_dlacpy( 'ALL', sheight, swidth, work, sheight,a( istartm, k ), lda )
                           
                 call stdlib_dgemm( 'N', 'N', sheight, swidth, swidth, one,b( istartm, k ), ldb, &
                           zc, ldzc, zero, work,sheight )
                 call stdlib_dlacpy( 'ALL', sheight, swidth, work, sheight,b( istartm, k ), ldb )
                           
              end if
              if ( ilz ) then
                 call stdlib_dgemm( 'N', 'N', n, nblock, nblock, one, z( 1_ilp, k ),ldz, zc, ldzc, &
                           zero, work, n )
                 call stdlib_dlacpy( 'ALL', n, nblock, work, n, z( 1_ilp, k ), ldz )
              end if
              k = k+np
           end do
           ! the following block removes the shifts from the bottom right corner
           ! one by one. updates are initially applied to a(ihi-ns+1:ihi,ihi-ns:ihi).
           call stdlib_dlaset( 'FULL', ns, ns, zero, one, qc, ldqc )
           call stdlib_dlaset( 'FULL', ns+1, ns+1, zero, one, zc, ldzc )
           ! istartb points to the first row we will be updating
           istartb = ihi-ns+1
           ! istopb points to the last column we will be updating
           istopb = ihi
           do i = 1, ns, 2
              ! chase the shift down to the bottom right corner
              do ishift = ihi-i-1, ihi-2
                 call stdlib_dlaqz2( .true., .true., ishift, istartb, istopb, ihi,a, lda, b, ldb, &
                           ns, ihi-ns+1, qc, ldqc, ns+1,ihi-ns, zc, ldzc )
              end do
           end do
           ! update rest of the pencil
           ! update a(ihi-ns+1:ihi, ihi+1:istopm)
           ! from the left with qc(1:ns,1:ns)'
           sheight = ns
           swidth = istopm-( ihi+1 )+1_ilp
           if ( swidth > 0_ilp ) then
              call stdlib_dgemm( 'T', 'N', sheight, swidth, sheight, one, qc, ldqc,a( ihi-ns+1, &
                        ihi+1 ), lda, zero, work, sheight )
              call stdlib_dlacpy( 'ALL', sheight, swidth, work, sheight,a( ihi-ns+1, ihi+1 ), lda &
                        )
              call stdlib_dgemm( 'T', 'N', sheight, swidth, sheight, one, qc, ldqc,b( ihi-ns+1, &
                        ihi+1 ), ldb, zero, work, sheight )
              call stdlib_dlacpy( 'ALL', sheight, swidth, work, sheight,b( ihi-ns+1, ihi+1 ), ldb &
                        )
           end if
           if ( ilq ) then
              call stdlib_dgemm( 'N', 'N', n, ns, ns, one, q( 1_ilp, ihi-ns+1 ), ldq,qc, ldqc, zero, &
                        work, n )
              call stdlib_dlacpy( 'ALL', n, ns, work, n, q( 1_ilp, ihi-ns+1 ), ldq )
           end if
           ! update a(istartm:ihi-ns,ihi-ns:ihi)
           ! from the right with zc(1:ns+1,1:ns+1)
           sheight = ihi-ns-istartm+1
           swidth = ns+1
           if ( sheight > 0_ilp ) then
              call stdlib_dgemm( 'N', 'N', sheight, swidth, swidth, one, a( istartm,ihi-ns ), lda,&
                         zc, ldzc, zero, work, sheight )
              call stdlib_dlacpy( 'ALL', sheight, swidth, work, sheight, a( istartm,ihi-ns ), lda &
                        )
              call stdlib_dgemm( 'N', 'N', sheight, swidth, swidth, one, b( istartm,ihi-ns ), ldb,&
                         zc, ldzc, zero, work, sheight )
              call stdlib_dlacpy( 'ALL', sheight, swidth, work, sheight, b( istartm,ihi-ns ), ldb &
                        )
           end if
           if ( ilz ) then
              call stdlib_dgemm( 'N', 'N', n, ns+1, ns+1, one, z( 1_ilp, ihi-ns ), ldz,zc, ldzc, zero,&
                         work, n )
              call stdlib_dlacpy( 'ALL', n, ns+1, work, n, z( 1_ilp, ihi-ns ), ldz )
           end if
     end subroutine stdlib_dlaqz4



end submodule stdlib_lapack_eigv_gen3
