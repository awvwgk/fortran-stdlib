
! Vector norms
submodule(stdlib_linalg) stdlib_linalg_norms
     use stdlib_linalg_constants
     use stdlib_linalg_blas
     use stdlib_linalg_lapack
     use stdlib_linalg_state, only: linalg_state_type, linalg_error_handling, LINALG_ERROR, &
         LINALG_INTERNAL_ERROR, LINALG_VALUE_ERROR     
     use iso_c_binding, only: c_intptr_t,c_char,c_loc
     implicit none(type,external)
     
     character(*), parameter :: this = 'norm'
     
     !> List of internal norm flags
     integer(ilp), parameter :: NORM_ONE       = 1_ilp 
     integer(ilp), parameter :: NORM_TWO       = 2_ilp
     integer(ilp), parameter :: NORM_POW_FIRST = 3_ilp       
     integer(ilp), parameter :: NORM_INF       = +huge(0_ilp) ! infinity norm 
     integer(ilp), parameter :: NORM_POW_LAST  = NORM_INF - 1_ilp
     integer(ilp), parameter :: NORM_MINUSINF  = -huge(0_ilp)
     
     interface parse_norm_type
        module procedure parse_norm_type_integer
        module procedure parse_norm_type_character
     end interface parse_norm_type
     
     
     interface stride_1d
        module procedure stride_1d_s
        module procedure stride_1d_d
        module procedure stride_1d_c
        module procedure stride_1d_z
     end interface stride_1d
     
     contains
     
     !> Parse norm type from an integer user input
     pure subroutine parse_norm_type_integer(order,norm_type,err)
        !> User input value
        integer(ilp), intent(in) :: order
        !> Return value: norm type
        integer(ilp), intent(out) :: norm_type
        !> State return flag
        type(linalg_state_type), intent(out) :: err
        
        select case (order)
           case (1_ilp)
               norm_type = NORM_ONE
           case (2_ilp)
               norm_type = NORM_TWO
           case (3_ilp:NORM_POW_LAST)
               norm_type = order
           case (NORM_INF:)
               norm_type = NORM_INF
           case (:NORM_MINUSINF)
               norm_type = NORM_MINUSINF
           
           case default
               norm_type = NORM_ONE
               err = linalg_state_type(this,LINALG_ERROR,'Input norm type ',order,' is not recognized.')
        end select    
        
     end subroutine parse_norm_type_integer

     pure subroutine parse_norm_type_character(order,norm_type,err)
        !> User input value
        character(len=*), intent(in) :: order
        !> Return value: norm type
        integer(ilp), intent(out) :: norm_type
        !> State return flag
        type(linalg_state_type), intent(out) :: err
        
        integer(ilp) :: int_order,read_err
        
        select case (order)
           case ('inf','Inf','INF')
              norm_type = NORM_INF
           case ('-inf','-Inf','-INF')
              norm_type = NORM_MINUSINF
           case ('Euclidean','euclidean','EUCLIDEAN')
              norm_type = NORM_TWO
           case default
            
              ! Check if this input can be read as an integer
              read(order,*,iostat=read_err) int_order
              if (read_err/=0) then 
                 ! Cannot read as an integer
                 norm_type = NORM_ONE
                 err = linalg_state_type(this,LINALG_ERROR,'Input norm type ',order,' is not recognized.')                 
              else
                 call parse_norm_type_integer(int_order,norm_type,err)
              endif  

        end select    
        
     end subroutine parse_norm_type_character

    
    ! Compute stride of a 1d array
    pure integer(ilp) function stride_1d_s(a) result(stride)
        !> Input 1-d array 
        real(sp), intent(in), target :: a(:)
        
        integer(c_intptr_t) :: a1,a2
        
        if (size(a,kind=ilp)<=1_ilp) then 
           stride = 1_ilp
        else
           a1 = transfer(c_loc(a(1)),a1)
           a2 = transfer(c_loc(a(2)),a2)
           stride = bit_size(0_c_char)*int(a2-a1, ilp)/storage_size(a, kind=ilp)
        endif
        
    end function stride_1d_s
    
    ! Private internal 1D implementation. This has to be used only internally, 
    ! when all inputs are already checked for correctness.
    pure subroutine internal_norm_1D_s(sze, a, nrm, norm_request)
        !> Input matrix length
        integer(ilp), intent(in) :: sze
        !> Input contiguous 1-d matrix a(*)
        real(sp), intent(in) :: a(sze)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Internal matrix request 
        integer(ilp), intent(in) :: norm_request
        
        integer(ilp) :: i
        real(sp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        select case(norm_request)
            case(NORM_ONE)
                nrm = asum(sze,a,incx=1_ilp)
            case(NORM_TWO)            
                nrm = nrm2(sze,a,incx=1_ilp)
            case(NORM_INF)
                i = stdlib_isamax(sze,a,incx=1_ilp)
                nrm = abs(a(i))
            case(NORM_MINUSINF)
                nrm = minval( abs(a) )
            case (NORM_POW_FIRST:NORM_POW_LAST)
                rorder = 1.0_sp / norm_request
                nrm = sum( abs(a) ** norm_request ) ** rorder
        end select
        
    end subroutine internal_norm_1D_s     
    

    !==============================================
    ! Norms : any rank to scalar
    !==============================================


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_1D_order_char_s(a, order) result(nrm)
        !> Input 1-d matrix a(:)
        real(sp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm
                                    
        call norm_1D_char_s(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_1D_order_char_s
    
    ! Function interface with output error
    module function stdlib_linalg_norm_1D_order_err_char_s(a, order, err) result(nrm)
        !> Input 1-d matrix a(:)
        real(sp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm                 
                
        call norm_1D_char_s(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_1D_order_err_char_s

    ! Internal implementation: 1-d    
    pure module subroutine norm_1D_char_s(a, nrm, order, err)
        !> Input 1-d matrix a(:)
        real(sp), intent(in), target :: a(:)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(sp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_s(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_1D_char_s


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_2D_order_char_s(a, order) result(nrm)
        !> Input 2-d matrix a(:,:)
        real(sp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm
                                    
        call norm_2D_char_s(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_2D_order_char_s
    
    ! Function interface with output error
    module function stdlib_linalg_norm_2D_order_err_char_s(a, order, err) result(nrm)
        !> Input 2-d matrix a(:,:)
        real(sp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm                 
                
        call norm_2D_char_s(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_2D_order_err_char_s

    ! Internal implementation: 2-d    
    pure module subroutine norm_2D_char_s(a, nrm, order, err)
        !> Input 2-d matrix a(:,:)
        real(sp), intent(in), target :: a(:,:)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(sp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_s(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_2D_char_s


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_3D_order_char_s(a, order) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        real(sp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm
                                    
        call norm_3D_char_s(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_3D_order_char_s
    
    ! Function interface with output error
    module function stdlib_linalg_norm_3D_order_err_char_s(a, order, err) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        real(sp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm                 
                
        call norm_3D_char_s(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_3D_order_err_char_s

    ! Internal implementation: 3-d    
    pure module subroutine norm_3D_char_s(a, nrm, order, err)
        !> Input 3-d matrix a(:,:,:)
        real(sp), intent(in), target :: a(:,:,:)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(sp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_s(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_3D_char_s


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_4D_order_char_s(a, order) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        real(sp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm
                                    
        call norm_4D_char_s(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_4D_order_char_s
    
    ! Function interface with output error
    module function stdlib_linalg_norm_4D_order_err_char_s(a, order, err) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        real(sp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm                 
                
        call norm_4D_char_s(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_4D_order_err_char_s

    ! Internal implementation: 4-d    
    pure module subroutine norm_4D_char_s(a, nrm, order, err)
        !> Input 4-d matrix a(:,:,:,:)
        real(sp), intent(in), target :: a(:,:,:,:)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(sp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_s(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_4D_char_s


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_5D_order_char_s(a, order) result(nrm)
        !> Input 5-d matrix a(:,:,:,:,:)
        real(sp), intent(in) :: a(:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm
                                    
        call norm_5D_char_s(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_5D_order_char_s
    
    ! Function interface with output error
    module function stdlib_linalg_norm_5D_order_err_char_s(a, order, err) result(nrm)
        !> Input 5-d matrix a(:,:,:,:,:)
        real(sp), intent(in) :: a(:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm                 
                
        call norm_5D_char_s(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_5D_order_err_char_s

    ! Internal implementation: 5-d    
    pure module subroutine norm_5D_char_s(a, nrm, order, err)
        !> Input 5-d matrix a(:,:,:,:,:)
        real(sp), intent(in), target :: a(:,:,:,:,:)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(sp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_s(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_5D_char_s


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_6D_order_char_s(a, order) result(nrm)
        !> Input 6-d matrix a(:,:,:,:,:,:)
        real(sp), intent(in) :: a(:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm
                                    
        call norm_6D_char_s(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_6D_order_char_s
    
    ! Function interface with output error
    module function stdlib_linalg_norm_6D_order_err_char_s(a, order, err) result(nrm)
        !> Input 6-d matrix a(:,:,:,:,:,:)
        real(sp), intent(in) :: a(:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm                 
                
        call norm_6D_char_s(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_6D_order_err_char_s

    ! Internal implementation: 6-d    
    pure module subroutine norm_6D_char_s(a, nrm, order, err)
        !> Input 6-d matrix a(:,:,:,:,:,:)
        real(sp), intent(in), target :: a(:,:,:,:,:,:)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(sp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_s(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_6D_char_s


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_7D_order_char_s(a, order) result(nrm)
        !> Input 7-d matrix a(:,:,:,:,:,:,:)
        real(sp), intent(in) :: a(:,:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm
                                    
        call norm_7D_char_s(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_7D_order_char_s
    
    ! Function interface with output error
    module function stdlib_linalg_norm_7D_order_err_char_s(a, order, err) result(nrm)
        !> Input 7-d matrix a(:,:,:,:,:,:,:)
        real(sp), intent(in) :: a(:,:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm                 
                
        call norm_7D_char_s(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_7D_order_err_char_s

    ! Internal implementation: 7-d    
    pure module subroutine norm_7D_char_s(a, nrm, order, err)
        !> Input 7-d matrix a(:,:,:,:,:,:,:)
        real(sp), intent(in), target :: a(:,:,:,:,:,:,:)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(sp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_s(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_7D_char_s


    !====================================================================
    ! Norms : any rank to rank-1, with DIM specifier and char input
    !====================================================================

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_2D_to_1D_char_s(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        
        call norm_2D_to_1D_char_s(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_2D_to_1D_char_s

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_2D_to_1D_err_char_s(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        
        call norm_2D_to_1D_char_s(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_2D_to_1D_err_char_s
    
    ! Internal implementation 
    pure module subroutine norm_2D_to_1D_char_s(a, nrm, order, dim, err)
        !> Input matrix a[..]
        real(sp), intent(in) :: a(:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2
        logical :: contiguous_data
        real(sp) :: rorder
        integer(ilp), dimension(2) :: spe,spack,perm,iperm
        integer(ilp), dimension(2), parameter :: dim_range = [(lda,lda=1_ilp,2_ilp)]
        real(sp), allocatable :: apack(:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>2) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_s(lda, apack(:, j2), &
                    nrm(j2), norm_request)
            enddo            
            
        else
            
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_s(lda, a(:, j2), &
                    nrm(j2), norm_request)
            enddo               
            
        endif        
        
    end subroutine norm_2D_to_1D_char_s

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_3D_to_2D_char_s(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        
        call norm_3D_to_2D_char_s(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_3D_to_2D_char_s

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_3D_to_2D_err_char_s(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        
        call norm_3D_to_2D_char_s(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_3D_to_2D_err_char_s
    
    ! Internal implementation 
    pure module subroutine norm_3D_to_2D_char_s(a, nrm, order, dim, err)
        !> Input matrix a[..]
        real(sp), intent(in) :: a(:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3
        logical :: contiguous_data
        real(sp) :: rorder
        integer(ilp), dimension(3) :: spe,spack,perm,iperm
        integer(ilp), dimension(3), parameter :: dim_range = [(lda,lda=1_ilp,3_ilp)]
        real(sp), allocatable :: apack(:,:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>3) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_s(lda, apack(:, j2, j3), &
                    nrm(j2, j3), norm_request)
            enddo; enddo            
            
        else
            
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_s(lda, a(:, j2, j3), &
                    nrm(j2, j3), norm_request)
            enddo; enddo               
            
        endif        
        
    end subroutine norm_3D_to_2D_char_s

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_4D_to_3D_char_s(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))     
        
        call norm_4D_to_3D_char_s(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_4D_to_3D_char_s

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_4D_to_3D_err_char_s(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))     
        
        call norm_4D_to_3D_char_s(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_4D_to_3D_err_char_s
    
    ! Internal implementation 
    pure module subroutine norm_4D_to_3D_char_s(a, nrm, order, dim, err)
        !> Input matrix a[..]
        real(sp), intent(in) :: a(:,:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
            & merge(size(a, 3), size(a, 4), mask=3<dim))     
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3, j4
        logical :: contiguous_data
        real(sp) :: rorder
        integer(ilp), dimension(4) :: spe,spack,perm,iperm
        integer(ilp), dimension(4), parameter :: dim_range = [(lda,lda=1_ilp,4_ilp)]
        real(sp), allocatable :: apack(:,:,:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>4) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j4 = lbound(apack, 4), ubound(apack, 4)
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_s(lda, apack(:, j2, j3, j4), &
                    nrm(j2, j3, j4), norm_request)
            enddo; enddo; enddo            
            
        else
            
            do j4 = lbound(a, 4), ubound(a, 4)
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_s(lda, a(:, j2, j3, j4), &
                    nrm(j2, j3, j4), norm_request)
            enddo; enddo; enddo               
            
        endif        
        
    end subroutine norm_4D_to_3D_char_s

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_5D_to_4D_char_s(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim))     
        
        call norm_5D_to_4D_char_s(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_5D_to_4D_char_s

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_5D_to_4D_err_char_s(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim))     
        
        call norm_5D_to_4D_char_s(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_5D_to_4D_err_char_s
    
    ! Internal implementation 
    pure module subroutine norm_5D_to_4D_char_s(a, nrm, order, dim, err)
        !> Input matrix a[..]
        real(sp), intent(in) :: a(:,:,:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
            & merge(size(a, 3), size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim))     
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3, j4, j5
        logical :: contiguous_data
        real(sp) :: rorder
        integer(ilp), dimension(5) :: spe,spack,perm,iperm
        integer(ilp), dimension(5), parameter :: dim_range = [(lda,lda=1_ilp,5_ilp)]
        real(sp), allocatable :: apack(:,:,:,:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>5) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j5 = lbound(apack, 5), ubound(apack, 5)
            do j4 = lbound(apack, 4), ubound(apack, 4)
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_s(lda, apack(:, j2, j3, j4, j5), &
                    nrm(j2, j3, j4, j5), norm_request)
            enddo; enddo; enddo; enddo            
            
        else
            
            do j5 = lbound(a, 5), ubound(a, 5)
            do j4 = lbound(a, 4), ubound(a, 4)
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_s(lda, a(:, j2, j3, j4, j5), &
                    nrm(j2, j3, j4, j5), norm_request)
            enddo; enddo; enddo; enddo               
            
        endif        
        
    end subroutine norm_5D_to_4D_char_s

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_6D_to_5D_char_s(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6), mask=5<dim))     
        
        call norm_6D_to_5D_char_s(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_6D_to_5D_char_s

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_6D_to_5D_err_char_s(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6), mask=5<dim))     
        
        call norm_6D_to_5D_char_s(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_6D_to_5D_err_char_s
    
    ! Internal implementation 
    pure module subroutine norm_6D_to_5D_char_s(a, nrm, order, dim, err)
        !> Input matrix a[..]
        real(sp), intent(in) :: a(:,:,:,:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
            & merge(size(a, 3), size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6),&
            & mask=5<dim))     
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3, j4, j5, j6
        logical :: contiguous_data
        real(sp) :: rorder
        integer(ilp), dimension(6) :: spe,spack,perm,iperm
        integer(ilp), dimension(6), parameter :: dim_range = [(lda,lda=1_ilp,6_ilp)]
        real(sp), allocatable :: apack(:,:,:,:,:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>6) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j6 = lbound(apack, 6), ubound(apack, 6)
            do j5 = lbound(apack, 5), ubound(apack, 5)
            do j4 = lbound(apack, 4), ubound(apack, 4)
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_s(lda, apack(:, j2, j3, j4, j5, j6), &
                    nrm(j2, j3, j4, j5, j6), norm_request)
            enddo; enddo; enddo; enddo; enddo            
            
        else
            
            do j6 = lbound(a, 6), ubound(a, 6)
            do j5 = lbound(a, 5), ubound(a, 5)
            do j4 = lbound(a, 4), ubound(a, 4)
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_s(lda, a(:, j2, j3, j4, j5, j6), &
                    nrm(j2, j3, j4, j5, j6), norm_request)
            enddo; enddo; enddo; enddo; enddo               
            
        endif        
        
    end subroutine norm_6D_to_5D_char_s

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_7D_to_6D_char_s(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6), mask=5<dim),&
            & merge(size(a, 6), size(a, 7), mask=6<dim))     
        
        call norm_7D_to_6D_char_s(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_7D_to_6D_char_s

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_7D_to_6D_err_char_s(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6), mask=5<dim),&
            & merge(size(a, 6), size(a, 7), mask=6<dim))     
        
        call norm_7D_to_6D_char_s(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_7D_to_6D_err_char_s
    
    ! Internal implementation 
    pure module subroutine norm_7D_to_6D_char_s(a, nrm, order, dim, err)
        !> Input matrix a[..]
        real(sp), intent(in) :: a(:,:,:,:,:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
            & merge(size(a, 3), size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6),&
            & mask=5<dim), merge(size(a, 6), size(a, 7), mask=6<dim))     
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3, j4, j5, j6, j7
        logical :: contiguous_data
        real(sp) :: rorder
        integer(ilp), dimension(7) :: spe,spack,perm,iperm
        integer(ilp), dimension(7), parameter :: dim_range = [(lda,lda=1_ilp,7_ilp)]
        real(sp), allocatable :: apack(:,:,:,:,:,:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>7) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j7 = lbound(apack, 7), ubound(apack, 7)
            do j6 = lbound(apack, 6), ubound(apack, 6)
            do j5 = lbound(apack, 5), ubound(apack, 5)
            do j4 = lbound(apack, 4), ubound(apack, 4)
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_s(lda, apack(:, j2, j3, j4, j5, j6, j7), &
                    nrm(j2, j3, j4, j5, j6, j7), norm_request)
            enddo; enddo; enddo; enddo; enddo; enddo            
            
        else
            
            do j7 = lbound(a, 7), ubound(a, 7)
            do j6 = lbound(a, 6), ubound(a, 6)
            do j5 = lbound(a, 5), ubound(a, 5)
            do j4 = lbound(a, 4), ubound(a, 4)
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_s(lda, a(:, j2, j3, j4, j5, j6, j7), &
                    nrm(j2, j3, j4, j5, j6, j7), norm_request)
            enddo; enddo; enddo; enddo; enddo; enddo               
            
        endif        
        
    end subroutine norm_7D_to_6D_char_s


    !==============================================
    ! Norms : any rank to scalar
    !==============================================


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_1D_order_int_s(a, order) result(nrm)
        !> Input 1-d matrix a(:)
        real(sp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm
                                    
        call norm_1D_int_s(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_1D_order_int_s
    
    ! Function interface with output error
    module function stdlib_linalg_norm_1D_order_err_int_s(a, order, err) result(nrm)
        !> Input 1-d matrix a(:)
        real(sp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm                 
                
        call norm_1D_int_s(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_1D_order_err_int_s

    ! Internal implementation: 1-d    
    pure module subroutine norm_1D_int_s(a, nrm, order, err)
        !> Input 1-d matrix a(:)
        real(sp), intent(in), target :: a(:)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(sp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_s(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_1D_int_s


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_2D_order_int_s(a, order) result(nrm)
        !> Input 2-d matrix a(:,:)
        real(sp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm
                                    
        call norm_2D_int_s(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_2D_order_int_s
    
    ! Function interface with output error
    module function stdlib_linalg_norm_2D_order_err_int_s(a, order, err) result(nrm)
        !> Input 2-d matrix a(:,:)
        real(sp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm                 
                
        call norm_2D_int_s(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_2D_order_err_int_s

    ! Internal implementation: 2-d    
    pure module subroutine norm_2D_int_s(a, nrm, order, err)
        !> Input 2-d matrix a(:,:)
        real(sp), intent(in), target :: a(:,:)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(sp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_s(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_2D_int_s


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_3D_order_int_s(a, order) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        real(sp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm
                                    
        call norm_3D_int_s(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_3D_order_int_s
    
    ! Function interface with output error
    module function stdlib_linalg_norm_3D_order_err_int_s(a, order, err) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        real(sp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm                 
                
        call norm_3D_int_s(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_3D_order_err_int_s

    ! Internal implementation: 3-d    
    pure module subroutine norm_3D_int_s(a, nrm, order, err)
        !> Input 3-d matrix a(:,:,:)
        real(sp), intent(in), target :: a(:,:,:)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(sp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_s(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_3D_int_s


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_4D_order_int_s(a, order) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        real(sp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm
                                    
        call norm_4D_int_s(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_4D_order_int_s
    
    ! Function interface with output error
    module function stdlib_linalg_norm_4D_order_err_int_s(a, order, err) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        real(sp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm                 
                
        call norm_4D_int_s(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_4D_order_err_int_s

    ! Internal implementation: 4-d    
    pure module subroutine norm_4D_int_s(a, nrm, order, err)
        !> Input 4-d matrix a(:,:,:,:)
        real(sp), intent(in), target :: a(:,:,:,:)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(sp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_s(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_4D_int_s


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_5D_order_int_s(a, order) result(nrm)
        !> Input 5-d matrix a(:,:,:,:,:)
        real(sp), intent(in) :: a(:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm
                                    
        call norm_5D_int_s(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_5D_order_int_s
    
    ! Function interface with output error
    module function stdlib_linalg_norm_5D_order_err_int_s(a, order, err) result(nrm)
        !> Input 5-d matrix a(:,:,:,:,:)
        real(sp), intent(in) :: a(:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm                 
                
        call norm_5D_int_s(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_5D_order_err_int_s

    ! Internal implementation: 5-d    
    pure module subroutine norm_5D_int_s(a, nrm, order, err)
        !> Input 5-d matrix a(:,:,:,:,:)
        real(sp), intent(in), target :: a(:,:,:,:,:)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(sp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_s(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_5D_int_s


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_6D_order_int_s(a, order) result(nrm)
        !> Input 6-d matrix a(:,:,:,:,:,:)
        real(sp), intent(in) :: a(:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm
                                    
        call norm_6D_int_s(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_6D_order_int_s
    
    ! Function interface with output error
    module function stdlib_linalg_norm_6D_order_err_int_s(a, order, err) result(nrm)
        !> Input 6-d matrix a(:,:,:,:,:,:)
        real(sp), intent(in) :: a(:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm                 
                
        call norm_6D_int_s(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_6D_order_err_int_s

    ! Internal implementation: 6-d    
    pure module subroutine norm_6D_int_s(a, nrm, order, err)
        !> Input 6-d matrix a(:,:,:,:,:,:)
        real(sp), intent(in), target :: a(:,:,:,:,:,:)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(sp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_s(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_6D_int_s


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_7D_order_int_s(a, order) result(nrm)
        !> Input 7-d matrix a(:,:,:,:,:,:,:)
        real(sp), intent(in) :: a(:,:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm
                                    
        call norm_7D_int_s(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_7D_order_int_s
    
    ! Function interface with output error
    module function stdlib_linalg_norm_7D_order_err_int_s(a, order, err) result(nrm)
        !> Input 7-d matrix a(:,:,:,:,:,:,:)
        real(sp), intent(in) :: a(:,:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm                 
                
        call norm_7D_int_s(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_7D_order_err_int_s

    ! Internal implementation: 7-d    
    pure module subroutine norm_7D_int_s(a, nrm, order, err)
        !> Input 7-d matrix a(:,:,:,:,:,:,:)
        real(sp), intent(in), target :: a(:,:,:,:,:,:,:)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(sp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_s(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_7D_int_s


    !====================================================================
    ! Norms : any rank to rank-1, with DIM specifier and int input
    !====================================================================

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_2D_to_1D_int_s(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        
        call norm_2D_to_1D_int_s(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_2D_to_1D_int_s

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_2D_to_1D_err_int_s(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        
        call norm_2D_to_1D_int_s(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_2D_to_1D_err_int_s
    
    ! Internal implementation 
    pure module subroutine norm_2D_to_1D_int_s(a, nrm, order, dim, err)
        !> Input matrix a[..]
        real(sp), intent(in) :: a(:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2
        logical :: contiguous_data
        real(sp) :: rorder
        integer(ilp), dimension(2) :: spe,spack,perm,iperm
        integer(ilp), dimension(2), parameter :: dim_range = [(lda,lda=1_ilp,2_ilp)]
        real(sp), allocatable :: apack(:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>2) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_s(lda, apack(:, j2), &
                    nrm(j2), norm_request)
            enddo            
            
        else
            
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_s(lda, a(:, j2), &
                    nrm(j2), norm_request)
            enddo               
            
        endif        
        
    end subroutine norm_2D_to_1D_int_s

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_3D_to_2D_int_s(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        
        call norm_3D_to_2D_int_s(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_3D_to_2D_int_s

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_3D_to_2D_err_int_s(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        
        call norm_3D_to_2D_int_s(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_3D_to_2D_err_int_s
    
    ! Internal implementation 
    pure module subroutine norm_3D_to_2D_int_s(a, nrm, order, dim, err)
        !> Input matrix a[..]
        real(sp), intent(in) :: a(:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3
        logical :: contiguous_data
        real(sp) :: rorder
        integer(ilp), dimension(3) :: spe,spack,perm,iperm
        integer(ilp), dimension(3), parameter :: dim_range = [(lda,lda=1_ilp,3_ilp)]
        real(sp), allocatable :: apack(:,:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>3) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_s(lda, apack(:, j2, j3), &
                    nrm(j2, j3), norm_request)
            enddo; enddo            
            
        else
            
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_s(lda, a(:, j2, j3), &
                    nrm(j2, j3), norm_request)
            enddo; enddo               
            
        endif        
        
    end subroutine norm_3D_to_2D_int_s

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_4D_to_3D_int_s(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))     
        
        call norm_4D_to_3D_int_s(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_4D_to_3D_int_s

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_4D_to_3D_err_int_s(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))     
        
        call norm_4D_to_3D_int_s(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_4D_to_3D_err_int_s
    
    ! Internal implementation 
    pure module subroutine norm_4D_to_3D_int_s(a, nrm, order, dim, err)
        !> Input matrix a[..]
        real(sp), intent(in) :: a(:,:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
            & merge(size(a, 3), size(a, 4), mask=3<dim))     
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3, j4
        logical :: contiguous_data
        real(sp) :: rorder
        integer(ilp), dimension(4) :: spe,spack,perm,iperm
        integer(ilp), dimension(4), parameter :: dim_range = [(lda,lda=1_ilp,4_ilp)]
        real(sp), allocatable :: apack(:,:,:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>4) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j4 = lbound(apack, 4), ubound(apack, 4)
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_s(lda, apack(:, j2, j3, j4), &
                    nrm(j2, j3, j4), norm_request)
            enddo; enddo; enddo            
            
        else
            
            do j4 = lbound(a, 4), ubound(a, 4)
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_s(lda, a(:, j2, j3, j4), &
                    nrm(j2, j3, j4), norm_request)
            enddo; enddo; enddo               
            
        endif        
        
    end subroutine norm_4D_to_3D_int_s

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_5D_to_4D_int_s(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim))     
        
        call norm_5D_to_4D_int_s(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_5D_to_4D_int_s

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_5D_to_4D_err_int_s(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim))     
        
        call norm_5D_to_4D_int_s(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_5D_to_4D_err_int_s
    
    ! Internal implementation 
    pure module subroutine norm_5D_to_4D_int_s(a, nrm, order, dim, err)
        !> Input matrix a[..]
        real(sp), intent(in) :: a(:,:,:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
            & merge(size(a, 3), size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim))     
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3, j4, j5
        logical :: contiguous_data
        real(sp) :: rorder
        integer(ilp), dimension(5) :: spe,spack,perm,iperm
        integer(ilp), dimension(5), parameter :: dim_range = [(lda,lda=1_ilp,5_ilp)]
        real(sp), allocatable :: apack(:,:,:,:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>5) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j5 = lbound(apack, 5), ubound(apack, 5)
            do j4 = lbound(apack, 4), ubound(apack, 4)
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_s(lda, apack(:, j2, j3, j4, j5), &
                    nrm(j2, j3, j4, j5), norm_request)
            enddo; enddo; enddo; enddo            
            
        else
            
            do j5 = lbound(a, 5), ubound(a, 5)
            do j4 = lbound(a, 4), ubound(a, 4)
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_s(lda, a(:, j2, j3, j4, j5), &
                    nrm(j2, j3, j4, j5), norm_request)
            enddo; enddo; enddo; enddo               
            
        endif        
        
    end subroutine norm_5D_to_4D_int_s

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_6D_to_5D_int_s(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6), mask=5<dim))     
        
        call norm_6D_to_5D_int_s(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_6D_to_5D_int_s

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_6D_to_5D_err_int_s(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6), mask=5<dim))     
        
        call norm_6D_to_5D_int_s(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_6D_to_5D_err_int_s
    
    ! Internal implementation 
    pure module subroutine norm_6D_to_5D_int_s(a, nrm, order, dim, err)
        !> Input matrix a[..]
        real(sp), intent(in) :: a(:,:,:,:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
            & merge(size(a, 3), size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6),&
            & mask=5<dim))     
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3, j4, j5, j6
        logical :: contiguous_data
        real(sp) :: rorder
        integer(ilp), dimension(6) :: spe,spack,perm,iperm
        integer(ilp), dimension(6), parameter :: dim_range = [(lda,lda=1_ilp,6_ilp)]
        real(sp), allocatable :: apack(:,:,:,:,:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>6) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j6 = lbound(apack, 6), ubound(apack, 6)
            do j5 = lbound(apack, 5), ubound(apack, 5)
            do j4 = lbound(apack, 4), ubound(apack, 4)
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_s(lda, apack(:, j2, j3, j4, j5, j6), &
                    nrm(j2, j3, j4, j5, j6), norm_request)
            enddo; enddo; enddo; enddo; enddo            
            
        else
            
            do j6 = lbound(a, 6), ubound(a, 6)
            do j5 = lbound(a, 5), ubound(a, 5)
            do j4 = lbound(a, 4), ubound(a, 4)
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_s(lda, a(:, j2, j3, j4, j5, j6), &
                    nrm(j2, j3, j4, j5, j6), norm_request)
            enddo; enddo; enddo; enddo; enddo               
            
        endif        
        
    end subroutine norm_6D_to_5D_int_s

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_7D_to_6D_int_s(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6), mask=5<dim),&
            & merge(size(a, 6), size(a, 7), mask=6<dim))     
        
        call norm_7D_to_6D_int_s(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_7D_to_6D_int_s

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_7D_to_6D_err_int_s(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6), mask=5<dim),&
            & merge(size(a, 6), size(a, 7), mask=6<dim))     
        
        call norm_7D_to_6D_int_s(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_7D_to_6D_err_int_s
    
    ! Internal implementation 
    pure module subroutine norm_7D_to_6D_int_s(a, nrm, order, dim, err)
        !> Input matrix a[..]
        real(sp), intent(in) :: a(:,:,:,:,:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
            & merge(size(a, 3), size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6),&
            & mask=5<dim), merge(size(a, 6), size(a, 7), mask=6<dim))     
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3, j4, j5, j6, j7
        logical :: contiguous_data
        real(sp) :: rorder
        integer(ilp), dimension(7) :: spe,spack,perm,iperm
        integer(ilp), dimension(7), parameter :: dim_range = [(lda,lda=1_ilp,7_ilp)]
        real(sp), allocatable :: apack(:,:,:,:,:,:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>7) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j7 = lbound(apack, 7), ubound(apack, 7)
            do j6 = lbound(apack, 6), ubound(apack, 6)
            do j5 = lbound(apack, 5), ubound(apack, 5)
            do j4 = lbound(apack, 4), ubound(apack, 4)
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_s(lda, apack(:, j2, j3, j4, j5, j6, j7), &
                    nrm(j2, j3, j4, j5, j6, j7), norm_request)
            enddo; enddo; enddo; enddo; enddo; enddo            
            
        else
            
            do j7 = lbound(a, 7), ubound(a, 7)
            do j6 = lbound(a, 6), ubound(a, 6)
            do j5 = lbound(a, 5), ubound(a, 5)
            do j4 = lbound(a, 4), ubound(a, 4)
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_s(lda, a(:, j2, j3, j4, j5, j6, j7), &
                    nrm(j2, j3, j4, j5, j6, j7), norm_request)
            enddo; enddo; enddo; enddo; enddo; enddo               
            
        endif        
        
    end subroutine norm_7D_to_6D_int_s

    
    ! Compute stride of a 1d array
    pure integer(ilp) function stride_1d_d(a) result(stride)
        !> Input 1-d array 
        real(dp), intent(in), target :: a(:)
        
        integer(c_intptr_t) :: a1,a2
        
        if (size(a,kind=ilp)<=1_ilp) then 
           stride = 1_ilp
        else
           a1 = transfer(c_loc(a(1)),a1)
           a2 = transfer(c_loc(a(2)),a2)
           stride = bit_size(0_c_char)*int(a2-a1, ilp)/storage_size(a, kind=ilp)
        endif
        
    end function stride_1d_d
    
    ! Private internal 1D implementation. This has to be used only internally, 
    ! when all inputs are already checked for correctness.
    pure subroutine internal_norm_1D_d(sze, a, nrm, norm_request)
        !> Input matrix length
        integer(ilp), intent(in) :: sze
        !> Input contiguous 1-d matrix a(*)
        real(dp), intent(in) :: a(sze)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Internal matrix request 
        integer(ilp), intent(in) :: norm_request
        
        integer(ilp) :: i
        real(dp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        select case(norm_request)
            case(NORM_ONE)
                nrm = asum(sze,a,incx=1_ilp)
            case(NORM_TWO)            
                nrm = nrm2(sze,a,incx=1_ilp)
            case(NORM_INF)
                i = stdlib_idamax(sze,a,incx=1_ilp)
                nrm = abs(a(i))
            case(NORM_MINUSINF)
                nrm = minval( abs(a) )
            case (NORM_POW_FIRST:NORM_POW_LAST)
                rorder = 1.0_dp / norm_request
                nrm = sum( abs(a) ** norm_request ) ** rorder
        end select
        
    end subroutine internal_norm_1D_d     
    

    !==============================================
    ! Norms : any rank to scalar
    !==============================================


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_1D_order_char_d(a, order) result(nrm)
        !> Input 1-d matrix a(:)
        real(dp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm
                                    
        call norm_1D_char_d(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_1D_order_char_d
    
    ! Function interface with output error
    module function stdlib_linalg_norm_1D_order_err_char_d(a, order, err) result(nrm)
        !> Input 1-d matrix a(:)
        real(dp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm                 
                
        call norm_1D_char_d(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_1D_order_err_char_d

    ! Internal implementation: 1-d    
    pure module subroutine norm_1D_char_d(a, nrm, order, err)
        !> Input 1-d matrix a(:)
        real(dp), intent(in), target :: a(:)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(dp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_d(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_1D_char_d


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_2D_order_char_d(a, order) result(nrm)
        !> Input 2-d matrix a(:,:)
        real(dp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm
                                    
        call norm_2D_char_d(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_2D_order_char_d
    
    ! Function interface with output error
    module function stdlib_linalg_norm_2D_order_err_char_d(a, order, err) result(nrm)
        !> Input 2-d matrix a(:,:)
        real(dp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm                 
                
        call norm_2D_char_d(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_2D_order_err_char_d

    ! Internal implementation: 2-d    
    pure module subroutine norm_2D_char_d(a, nrm, order, err)
        !> Input 2-d matrix a(:,:)
        real(dp), intent(in), target :: a(:,:)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(dp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_d(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_2D_char_d


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_3D_order_char_d(a, order) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        real(dp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm
                                    
        call norm_3D_char_d(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_3D_order_char_d
    
    ! Function interface with output error
    module function stdlib_linalg_norm_3D_order_err_char_d(a, order, err) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        real(dp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm                 
                
        call norm_3D_char_d(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_3D_order_err_char_d

    ! Internal implementation: 3-d    
    pure module subroutine norm_3D_char_d(a, nrm, order, err)
        !> Input 3-d matrix a(:,:,:)
        real(dp), intent(in), target :: a(:,:,:)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(dp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_d(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_3D_char_d


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_4D_order_char_d(a, order) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        real(dp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm
                                    
        call norm_4D_char_d(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_4D_order_char_d
    
    ! Function interface with output error
    module function stdlib_linalg_norm_4D_order_err_char_d(a, order, err) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        real(dp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm                 
                
        call norm_4D_char_d(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_4D_order_err_char_d

    ! Internal implementation: 4-d    
    pure module subroutine norm_4D_char_d(a, nrm, order, err)
        !> Input 4-d matrix a(:,:,:,:)
        real(dp), intent(in), target :: a(:,:,:,:)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(dp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_d(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_4D_char_d


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_5D_order_char_d(a, order) result(nrm)
        !> Input 5-d matrix a(:,:,:,:,:)
        real(dp), intent(in) :: a(:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm
                                    
        call norm_5D_char_d(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_5D_order_char_d
    
    ! Function interface with output error
    module function stdlib_linalg_norm_5D_order_err_char_d(a, order, err) result(nrm)
        !> Input 5-d matrix a(:,:,:,:,:)
        real(dp), intent(in) :: a(:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm                 
                
        call norm_5D_char_d(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_5D_order_err_char_d

    ! Internal implementation: 5-d    
    pure module subroutine norm_5D_char_d(a, nrm, order, err)
        !> Input 5-d matrix a(:,:,:,:,:)
        real(dp), intent(in), target :: a(:,:,:,:,:)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(dp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_d(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_5D_char_d


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_6D_order_char_d(a, order) result(nrm)
        !> Input 6-d matrix a(:,:,:,:,:,:)
        real(dp), intent(in) :: a(:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm
                                    
        call norm_6D_char_d(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_6D_order_char_d
    
    ! Function interface with output error
    module function stdlib_linalg_norm_6D_order_err_char_d(a, order, err) result(nrm)
        !> Input 6-d matrix a(:,:,:,:,:,:)
        real(dp), intent(in) :: a(:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm                 
                
        call norm_6D_char_d(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_6D_order_err_char_d

    ! Internal implementation: 6-d    
    pure module subroutine norm_6D_char_d(a, nrm, order, err)
        !> Input 6-d matrix a(:,:,:,:,:,:)
        real(dp), intent(in), target :: a(:,:,:,:,:,:)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(dp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_d(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_6D_char_d


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_7D_order_char_d(a, order) result(nrm)
        !> Input 7-d matrix a(:,:,:,:,:,:,:)
        real(dp), intent(in) :: a(:,:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm
                                    
        call norm_7D_char_d(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_7D_order_char_d
    
    ! Function interface with output error
    module function stdlib_linalg_norm_7D_order_err_char_d(a, order, err) result(nrm)
        !> Input 7-d matrix a(:,:,:,:,:,:,:)
        real(dp), intent(in) :: a(:,:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm                 
                
        call norm_7D_char_d(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_7D_order_err_char_d

    ! Internal implementation: 7-d    
    pure module subroutine norm_7D_char_d(a, nrm, order, err)
        !> Input 7-d matrix a(:,:,:,:,:,:,:)
        real(dp), intent(in), target :: a(:,:,:,:,:,:,:)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(dp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_d(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_7D_char_d


    !====================================================================
    ! Norms : any rank to rank-1, with DIM specifier and char input
    !====================================================================

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_2D_to_1D_char_d(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        
        call norm_2D_to_1D_char_d(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_2D_to_1D_char_d

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_2D_to_1D_err_char_d(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        
        call norm_2D_to_1D_char_d(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_2D_to_1D_err_char_d
    
    ! Internal implementation 
    pure module subroutine norm_2D_to_1D_char_d(a, nrm, order, dim, err)
        !> Input matrix a[..]
        real(dp), intent(in) :: a(:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2
        logical :: contiguous_data
        real(dp) :: rorder
        integer(ilp), dimension(2) :: spe,spack,perm,iperm
        integer(ilp), dimension(2), parameter :: dim_range = [(lda,lda=1_ilp,2_ilp)]
        real(dp), allocatable :: apack(:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>2) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_d(lda, apack(:, j2), &
                    nrm(j2), norm_request)
            enddo            
            
        else
            
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_d(lda, a(:, j2), &
                    nrm(j2), norm_request)
            enddo               
            
        endif        
        
    end subroutine norm_2D_to_1D_char_d

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_3D_to_2D_char_d(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        
        call norm_3D_to_2D_char_d(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_3D_to_2D_char_d

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_3D_to_2D_err_char_d(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        
        call norm_3D_to_2D_char_d(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_3D_to_2D_err_char_d
    
    ! Internal implementation 
    pure module subroutine norm_3D_to_2D_char_d(a, nrm, order, dim, err)
        !> Input matrix a[..]
        real(dp), intent(in) :: a(:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3
        logical :: contiguous_data
        real(dp) :: rorder
        integer(ilp), dimension(3) :: spe,spack,perm,iperm
        integer(ilp), dimension(3), parameter :: dim_range = [(lda,lda=1_ilp,3_ilp)]
        real(dp), allocatable :: apack(:,:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>3) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_d(lda, apack(:, j2, j3), &
                    nrm(j2, j3), norm_request)
            enddo; enddo            
            
        else
            
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_d(lda, a(:, j2, j3), &
                    nrm(j2, j3), norm_request)
            enddo; enddo               
            
        endif        
        
    end subroutine norm_3D_to_2D_char_d

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_4D_to_3D_char_d(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))     
        
        call norm_4D_to_3D_char_d(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_4D_to_3D_char_d

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_4D_to_3D_err_char_d(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))     
        
        call norm_4D_to_3D_char_d(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_4D_to_3D_err_char_d
    
    ! Internal implementation 
    pure module subroutine norm_4D_to_3D_char_d(a, nrm, order, dim, err)
        !> Input matrix a[..]
        real(dp), intent(in) :: a(:,:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
            & merge(size(a, 3), size(a, 4), mask=3<dim))     
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3, j4
        logical :: contiguous_data
        real(dp) :: rorder
        integer(ilp), dimension(4) :: spe,spack,perm,iperm
        integer(ilp), dimension(4), parameter :: dim_range = [(lda,lda=1_ilp,4_ilp)]
        real(dp), allocatable :: apack(:,:,:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>4) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j4 = lbound(apack, 4), ubound(apack, 4)
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_d(lda, apack(:, j2, j3, j4), &
                    nrm(j2, j3, j4), norm_request)
            enddo; enddo; enddo            
            
        else
            
            do j4 = lbound(a, 4), ubound(a, 4)
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_d(lda, a(:, j2, j3, j4), &
                    nrm(j2, j3, j4), norm_request)
            enddo; enddo; enddo               
            
        endif        
        
    end subroutine norm_4D_to_3D_char_d

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_5D_to_4D_char_d(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim))     
        
        call norm_5D_to_4D_char_d(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_5D_to_4D_char_d

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_5D_to_4D_err_char_d(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim))     
        
        call norm_5D_to_4D_char_d(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_5D_to_4D_err_char_d
    
    ! Internal implementation 
    pure module subroutine norm_5D_to_4D_char_d(a, nrm, order, dim, err)
        !> Input matrix a[..]
        real(dp), intent(in) :: a(:,:,:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
            & merge(size(a, 3), size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim))     
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3, j4, j5
        logical :: contiguous_data
        real(dp) :: rorder
        integer(ilp), dimension(5) :: spe,spack,perm,iperm
        integer(ilp), dimension(5), parameter :: dim_range = [(lda,lda=1_ilp,5_ilp)]
        real(dp), allocatable :: apack(:,:,:,:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>5) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j5 = lbound(apack, 5), ubound(apack, 5)
            do j4 = lbound(apack, 4), ubound(apack, 4)
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_d(lda, apack(:, j2, j3, j4, j5), &
                    nrm(j2, j3, j4, j5), norm_request)
            enddo; enddo; enddo; enddo            
            
        else
            
            do j5 = lbound(a, 5), ubound(a, 5)
            do j4 = lbound(a, 4), ubound(a, 4)
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_d(lda, a(:, j2, j3, j4, j5), &
                    nrm(j2, j3, j4, j5), norm_request)
            enddo; enddo; enddo; enddo               
            
        endif        
        
    end subroutine norm_5D_to_4D_char_d

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_6D_to_5D_char_d(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6), mask=5<dim))     
        
        call norm_6D_to_5D_char_d(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_6D_to_5D_char_d

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_6D_to_5D_err_char_d(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6), mask=5<dim))     
        
        call norm_6D_to_5D_char_d(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_6D_to_5D_err_char_d
    
    ! Internal implementation 
    pure module subroutine norm_6D_to_5D_char_d(a, nrm, order, dim, err)
        !> Input matrix a[..]
        real(dp), intent(in) :: a(:,:,:,:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
            & merge(size(a, 3), size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6),&
            & mask=5<dim))     
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3, j4, j5, j6
        logical :: contiguous_data
        real(dp) :: rorder
        integer(ilp), dimension(6) :: spe,spack,perm,iperm
        integer(ilp), dimension(6), parameter :: dim_range = [(lda,lda=1_ilp,6_ilp)]
        real(dp), allocatable :: apack(:,:,:,:,:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>6) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j6 = lbound(apack, 6), ubound(apack, 6)
            do j5 = lbound(apack, 5), ubound(apack, 5)
            do j4 = lbound(apack, 4), ubound(apack, 4)
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_d(lda, apack(:, j2, j3, j4, j5, j6), &
                    nrm(j2, j3, j4, j5, j6), norm_request)
            enddo; enddo; enddo; enddo; enddo            
            
        else
            
            do j6 = lbound(a, 6), ubound(a, 6)
            do j5 = lbound(a, 5), ubound(a, 5)
            do j4 = lbound(a, 4), ubound(a, 4)
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_d(lda, a(:, j2, j3, j4, j5, j6), &
                    nrm(j2, j3, j4, j5, j6), norm_request)
            enddo; enddo; enddo; enddo; enddo               
            
        endif        
        
    end subroutine norm_6D_to_5D_char_d

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_7D_to_6D_char_d(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6), mask=5<dim),&
            & merge(size(a, 6), size(a, 7), mask=6<dim))     
        
        call norm_7D_to_6D_char_d(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_7D_to_6D_char_d

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_7D_to_6D_err_char_d(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6), mask=5<dim),&
            & merge(size(a, 6), size(a, 7), mask=6<dim))     
        
        call norm_7D_to_6D_char_d(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_7D_to_6D_err_char_d
    
    ! Internal implementation 
    pure module subroutine norm_7D_to_6D_char_d(a, nrm, order, dim, err)
        !> Input matrix a[..]
        real(dp), intent(in) :: a(:,:,:,:,:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
            & merge(size(a, 3), size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6),&
            & mask=5<dim), merge(size(a, 6), size(a, 7), mask=6<dim))     
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3, j4, j5, j6, j7
        logical :: contiguous_data
        real(dp) :: rorder
        integer(ilp), dimension(7) :: spe,spack,perm,iperm
        integer(ilp), dimension(7), parameter :: dim_range = [(lda,lda=1_ilp,7_ilp)]
        real(dp), allocatable :: apack(:,:,:,:,:,:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>7) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j7 = lbound(apack, 7), ubound(apack, 7)
            do j6 = lbound(apack, 6), ubound(apack, 6)
            do j5 = lbound(apack, 5), ubound(apack, 5)
            do j4 = lbound(apack, 4), ubound(apack, 4)
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_d(lda, apack(:, j2, j3, j4, j5, j6, j7), &
                    nrm(j2, j3, j4, j5, j6, j7), norm_request)
            enddo; enddo; enddo; enddo; enddo; enddo            
            
        else
            
            do j7 = lbound(a, 7), ubound(a, 7)
            do j6 = lbound(a, 6), ubound(a, 6)
            do j5 = lbound(a, 5), ubound(a, 5)
            do j4 = lbound(a, 4), ubound(a, 4)
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_d(lda, a(:, j2, j3, j4, j5, j6, j7), &
                    nrm(j2, j3, j4, j5, j6, j7), norm_request)
            enddo; enddo; enddo; enddo; enddo; enddo               
            
        endif        
        
    end subroutine norm_7D_to_6D_char_d


    !==============================================
    ! Norms : any rank to scalar
    !==============================================


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_1D_order_int_d(a, order) result(nrm)
        !> Input 1-d matrix a(:)
        real(dp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm
                                    
        call norm_1D_int_d(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_1D_order_int_d
    
    ! Function interface with output error
    module function stdlib_linalg_norm_1D_order_err_int_d(a, order, err) result(nrm)
        !> Input 1-d matrix a(:)
        real(dp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm                 
                
        call norm_1D_int_d(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_1D_order_err_int_d

    ! Internal implementation: 1-d    
    pure module subroutine norm_1D_int_d(a, nrm, order, err)
        !> Input 1-d matrix a(:)
        real(dp), intent(in), target :: a(:)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(dp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_d(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_1D_int_d


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_2D_order_int_d(a, order) result(nrm)
        !> Input 2-d matrix a(:,:)
        real(dp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm
                                    
        call norm_2D_int_d(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_2D_order_int_d
    
    ! Function interface with output error
    module function stdlib_linalg_norm_2D_order_err_int_d(a, order, err) result(nrm)
        !> Input 2-d matrix a(:,:)
        real(dp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm                 
                
        call norm_2D_int_d(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_2D_order_err_int_d

    ! Internal implementation: 2-d    
    pure module subroutine norm_2D_int_d(a, nrm, order, err)
        !> Input 2-d matrix a(:,:)
        real(dp), intent(in), target :: a(:,:)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(dp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_d(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_2D_int_d


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_3D_order_int_d(a, order) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        real(dp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm
                                    
        call norm_3D_int_d(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_3D_order_int_d
    
    ! Function interface with output error
    module function stdlib_linalg_norm_3D_order_err_int_d(a, order, err) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        real(dp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm                 
                
        call norm_3D_int_d(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_3D_order_err_int_d

    ! Internal implementation: 3-d    
    pure module subroutine norm_3D_int_d(a, nrm, order, err)
        !> Input 3-d matrix a(:,:,:)
        real(dp), intent(in), target :: a(:,:,:)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(dp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_d(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_3D_int_d


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_4D_order_int_d(a, order) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        real(dp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm
                                    
        call norm_4D_int_d(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_4D_order_int_d
    
    ! Function interface with output error
    module function stdlib_linalg_norm_4D_order_err_int_d(a, order, err) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        real(dp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm                 
                
        call norm_4D_int_d(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_4D_order_err_int_d

    ! Internal implementation: 4-d    
    pure module subroutine norm_4D_int_d(a, nrm, order, err)
        !> Input 4-d matrix a(:,:,:,:)
        real(dp), intent(in), target :: a(:,:,:,:)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(dp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_d(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_4D_int_d


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_5D_order_int_d(a, order) result(nrm)
        !> Input 5-d matrix a(:,:,:,:,:)
        real(dp), intent(in) :: a(:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm
                                    
        call norm_5D_int_d(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_5D_order_int_d
    
    ! Function interface with output error
    module function stdlib_linalg_norm_5D_order_err_int_d(a, order, err) result(nrm)
        !> Input 5-d matrix a(:,:,:,:,:)
        real(dp), intent(in) :: a(:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm                 
                
        call norm_5D_int_d(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_5D_order_err_int_d

    ! Internal implementation: 5-d    
    pure module subroutine norm_5D_int_d(a, nrm, order, err)
        !> Input 5-d matrix a(:,:,:,:,:)
        real(dp), intent(in), target :: a(:,:,:,:,:)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(dp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_d(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_5D_int_d


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_6D_order_int_d(a, order) result(nrm)
        !> Input 6-d matrix a(:,:,:,:,:,:)
        real(dp), intent(in) :: a(:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm
                                    
        call norm_6D_int_d(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_6D_order_int_d
    
    ! Function interface with output error
    module function stdlib_linalg_norm_6D_order_err_int_d(a, order, err) result(nrm)
        !> Input 6-d matrix a(:,:,:,:,:,:)
        real(dp), intent(in) :: a(:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm                 
                
        call norm_6D_int_d(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_6D_order_err_int_d

    ! Internal implementation: 6-d    
    pure module subroutine norm_6D_int_d(a, nrm, order, err)
        !> Input 6-d matrix a(:,:,:,:,:,:)
        real(dp), intent(in), target :: a(:,:,:,:,:,:)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(dp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_d(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_6D_int_d


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_7D_order_int_d(a, order) result(nrm)
        !> Input 7-d matrix a(:,:,:,:,:,:,:)
        real(dp), intent(in) :: a(:,:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm
                                    
        call norm_7D_int_d(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_7D_order_int_d
    
    ! Function interface with output error
    module function stdlib_linalg_norm_7D_order_err_int_d(a, order, err) result(nrm)
        !> Input 7-d matrix a(:,:,:,:,:,:,:)
        real(dp), intent(in) :: a(:,:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm                 
                
        call norm_7D_int_d(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_7D_order_err_int_d

    ! Internal implementation: 7-d    
    pure module subroutine norm_7D_int_d(a, nrm, order, err)
        !> Input 7-d matrix a(:,:,:,:,:,:,:)
        real(dp), intent(in), target :: a(:,:,:,:,:,:,:)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(dp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_d(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_7D_int_d


    !====================================================================
    ! Norms : any rank to rank-1, with DIM specifier and int input
    !====================================================================

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_2D_to_1D_int_d(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        
        call norm_2D_to_1D_int_d(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_2D_to_1D_int_d

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_2D_to_1D_err_int_d(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        
        call norm_2D_to_1D_int_d(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_2D_to_1D_err_int_d
    
    ! Internal implementation 
    pure module subroutine norm_2D_to_1D_int_d(a, nrm, order, dim, err)
        !> Input matrix a[..]
        real(dp), intent(in) :: a(:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2
        logical :: contiguous_data
        real(dp) :: rorder
        integer(ilp), dimension(2) :: spe,spack,perm,iperm
        integer(ilp), dimension(2), parameter :: dim_range = [(lda,lda=1_ilp,2_ilp)]
        real(dp), allocatable :: apack(:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>2) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_d(lda, apack(:, j2), &
                    nrm(j2), norm_request)
            enddo            
            
        else
            
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_d(lda, a(:, j2), &
                    nrm(j2), norm_request)
            enddo               
            
        endif        
        
    end subroutine norm_2D_to_1D_int_d

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_3D_to_2D_int_d(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        
        call norm_3D_to_2D_int_d(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_3D_to_2D_int_d

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_3D_to_2D_err_int_d(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        
        call norm_3D_to_2D_int_d(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_3D_to_2D_err_int_d
    
    ! Internal implementation 
    pure module subroutine norm_3D_to_2D_int_d(a, nrm, order, dim, err)
        !> Input matrix a[..]
        real(dp), intent(in) :: a(:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3
        logical :: contiguous_data
        real(dp) :: rorder
        integer(ilp), dimension(3) :: spe,spack,perm,iperm
        integer(ilp), dimension(3), parameter :: dim_range = [(lda,lda=1_ilp,3_ilp)]
        real(dp), allocatable :: apack(:,:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>3) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_d(lda, apack(:, j2, j3), &
                    nrm(j2, j3), norm_request)
            enddo; enddo            
            
        else
            
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_d(lda, a(:, j2, j3), &
                    nrm(j2, j3), norm_request)
            enddo; enddo               
            
        endif        
        
    end subroutine norm_3D_to_2D_int_d

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_4D_to_3D_int_d(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))     
        
        call norm_4D_to_3D_int_d(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_4D_to_3D_int_d

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_4D_to_3D_err_int_d(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))     
        
        call norm_4D_to_3D_int_d(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_4D_to_3D_err_int_d
    
    ! Internal implementation 
    pure module subroutine norm_4D_to_3D_int_d(a, nrm, order, dim, err)
        !> Input matrix a[..]
        real(dp), intent(in) :: a(:,:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
            & merge(size(a, 3), size(a, 4), mask=3<dim))     
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3, j4
        logical :: contiguous_data
        real(dp) :: rorder
        integer(ilp), dimension(4) :: spe,spack,perm,iperm
        integer(ilp), dimension(4), parameter :: dim_range = [(lda,lda=1_ilp,4_ilp)]
        real(dp), allocatable :: apack(:,:,:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>4) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j4 = lbound(apack, 4), ubound(apack, 4)
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_d(lda, apack(:, j2, j3, j4), &
                    nrm(j2, j3, j4), norm_request)
            enddo; enddo; enddo            
            
        else
            
            do j4 = lbound(a, 4), ubound(a, 4)
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_d(lda, a(:, j2, j3, j4), &
                    nrm(j2, j3, j4), norm_request)
            enddo; enddo; enddo               
            
        endif        
        
    end subroutine norm_4D_to_3D_int_d

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_5D_to_4D_int_d(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim))     
        
        call norm_5D_to_4D_int_d(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_5D_to_4D_int_d

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_5D_to_4D_err_int_d(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim))     
        
        call norm_5D_to_4D_int_d(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_5D_to_4D_err_int_d
    
    ! Internal implementation 
    pure module subroutine norm_5D_to_4D_int_d(a, nrm, order, dim, err)
        !> Input matrix a[..]
        real(dp), intent(in) :: a(:,:,:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
            & merge(size(a, 3), size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim))     
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3, j4, j5
        logical :: contiguous_data
        real(dp) :: rorder
        integer(ilp), dimension(5) :: spe,spack,perm,iperm
        integer(ilp), dimension(5), parameter :: dim_range = [(lda,lda=1_ilp,5_ilp)]
        real(dp), allocatable :: apack(:,:,:,:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>5) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j5 = lbound(apack, 5), ubound(apack, 5)
            do j4 = lbound(apack, 4), ubound(apack, 4)
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_d(lda, apack(:, j2, j3, j4, j5), &
                    nrm(j2, j3, j4, j5), norm_request)
            enddo; enddo; enddo; enddo            
            
        else
            
            do j5 = lbound(a, 5), ubound(a, 5)
            do j4 = lbound(a, 4), ubound(a, 4)
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_d(lda, a(:, j2, j3, j4, j5), &
                    nrm(j2, j3, j4, j5), norm_request)
            enddo; enddo; enddo; enddo               
            
        endif        
        
    end subroutine norm_5D_to_4D_int_d

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_6D_to_5D_int_d(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6), mask=5<dim))     
        
        call norm_6D_to_5D_int_d(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_6D_to_5D_int_d

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_6D_to_5D_err_int_d(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6), mask=5<dim))     
        
        call norm_6D_to_5D_int_d(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_6D_to_5D_err_int_d
    
    ! Internal implementation 
    pure module subroutine norm_6D_to_5D_int_d(a, nrm, order, dim, err)
        !> Input matrix a[..]
        real(dp), intent(in) :: a(:,:,:,:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
            & merge(size(a, 3), size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6),&
            & mask=5<dim))     
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3, j4, j5, j6
        logical :: contiguous_data
        real(dp) :: rorder
        integer(ilp), dimension(6) :: spe,spack,perm,iperm
        integer(ilp), dimension(6), parameter :: dim_range = [(lda,lda=1_ilp,6_ilp)]
        real(dp), allocatable :: apack(:,:,:,:,:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>6) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j6 = lbound(apack, 6), ubound(apack, 6)
            do j5 = lbound(apack, 5), ubound(apack, 5)
            do j4 = lbound(apack, 4), ubound(apack, 4)
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_d(lda, apack(:, j2, j3, j4, j5, j6), &
                    nrm(j2, j3, j4, j5, j6), norm_request)
            enddo; enddo; enddo; enddo; enddo            
            
        else
            
            do j6 = lbound(a, 6), ubound(a, 6)
            do j5 = lbound(a, 5), ubound(a, 5)
            do j4 = lbound(a, 4), ubound(a, 4)
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_d(lda, a(:, j2, j3, j4, j5, j6), &
                    nrm(j2, j3, j4, j5, j6), norm_request)
            enddo; enddo; enddo; enddo; enddo               
            
        endif        
        
    end subroutine norm_6D_to_5D_int_d

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_7D_to_6D_int_d(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6), mask=5<dim),&
            & merge(size(a, 6), size(a, 7), mask=6<dim))     
        
        call norm_7D_to_6D_int_d(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_7D_to_6D_int_d

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_7D_to_6D_err_int_d(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6), mask=5<dim),&
            & merge(size(a, 6), size(a, 7), mask=6<dim))     
        
        call norm_7D_to_6D_int_d(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_7D_to_6D_err_int_d
    
    ! Internal implementation 
    pure module subroutine norm_7D_to_6D_int_d(a, nrm, order, dim, err)
        !> Input matrix a[..]
        real(dp), intent(in) :: a(:,:,:,:,:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
            & merge(size(a, 3), size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6),&
            & mask=5<dim), merge(size(a, 6), size(a, 7), mask=6<dim))     
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3, j4, j5, j6, j7
        logical :: contiguous_data
        real(dp) :: rorder
        integer(ilp), dimension(7) :: spe,spack,perm,iperm
        integer(ilp), dimension(7), parameter :: dim_range = [(lda,lda=1_ilp,7_ilp)]
        real(dp), allocatable :: apack(:,:,:,:,:,:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>7) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j7 = lbound(apack, 7), ubound(apack, 7)
            do j6 = lbound(apack, 6), ubound(apack, 6)
            do j5 = lbound(apack, 5), ubound(apack, 5)
            do j4 = lbound(apack, 4), ubound(apack, 4)
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_d(lda, apack(:, j2, j3, j4, j5, j6, j7), &
                    nrm(j2, j3, j4, j5, j6, j7), norm_request)
            enddo; enddo; enddo; enddo; enddo; enddo            
            
        else
            
            do j7 = lbound(a, 7), ubound(a, 7)
            do j6 = lbound(a, 6), ubound(a, 6)
            do j5 = lbound(a, 5), ubound(a, 5)
            do j4 = lbound(a, 4), ubound(a, 4)
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_d(lda, a(:, j2, j3, j4, j5, j6, j7), &
                    nrm(j2, j3, j4, j5, j6, j7), norm_request)
            enddo; enddo; enddo; enddo; enddo; enddo               
            
        endif        
        
    end subroutine norm_7D_to_6D_int_d

    
    ! Compute stride of a 1d array
    pure integer(ilp) function stride_1d_c(a) result(stride)
        !> Input 1-d array 
        complex(sp), intent(in), target :: a(:)
        
        integer(c_intptr_t) :: a1,a2
        
        if (size(a,kind=ilp)<=1_ilp) then 
           stride = 1_ilp
        else
           a1 = transfer(c_loc(a(1)),a1)
           a2 = transfer(c_loc(a(2)),a2)
           stride = bit_size(0_c_char)*int(a2-a1, ilp)/storage_size(a, kind=ilp)
        endif
        
    end function stride_1d_c
    
    ! Private internal 1D implementation. This has to be used only internally, 
    ! when all inputs are already checked for correctness.
    pure subroutine internal_norm_1D_c(sze, a, nrm, norm_request)
        !> Input matrix length
        integer(ilp), intent(in) :: sze
        !> Input contiguous 1-d matrix a(*)
        complex(sp), intent(in) :: a(sze)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Internal matrix request 
        integer(ilp), intent(in) :: norm_request
        
        integer(ilp) :: i
        real(sp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        select case(norm_request)
            case(NORM_ONE)
                nrm = asum(sze,a,incx=1_ilp)
            case(NORM_TWO)            
                nrm = nrm2(sze,a,incx=1_ilp)
            case(NORM_INF)
                ! The default BLAS stdlib_icamax uses |Re(.)|+|Im(.)|. Do not use it.
                i = stdlib_icmax1(sze,a,incx=1_ilp)
                nrm = abs(a(i))
            case(NORM_MINUSINF)
                nrm = minval( abs(a) )
            case (NORM_POW_FIRST:NORM_POW_LAST)
                rorder = 1.0_sp / norm_request
                nrm = sum( abs(a) ** norm_request ) ** rorder
        end select
        
    end subroutine internal_norm_1D_c     
    

    !==============================================
    ! Norms : any rank to scalar
    !==============================================


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_1D_order_char_c(a, order) result(nrm)
        !> Input 1-d matrix a(:)
        complex(sp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm
                                    
        call norm_1D_char_c(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_1D_order_char_c
    
    ! Function interface with output error
    module function stdlib_linalg_norm_1D_order_err_char_c(a, order, err) result(nrm)
        !> Input 1-d matrix a(:)
        complex(sp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm                 
                
        call norm_1D_char_c(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_1D_order_err_char_c

    ! Internal implementation: 1-d    
    pure module subroutine norm_1D_char_c(a, nrm, order, err)
        !> Input 1-d matrix a(:)
        complex(sp), intent(in), target :: a(:)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(sp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_c(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_1D_char_c


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_2D_order_char_c(a, order) result(nrm)
        !> Input 2-d matrix a(:,:)
        complex(sp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm
                                    
        call norm_2D_char_c(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_2D_order_char_c
    
    ! Function interface with output error
    module function stdlib_linalg_norm_2D_order_err_char_c(a, order, err) result(nrm)
        !> Input 2-d matrix a(:,:)
        complex(sp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm                 
                
        call norm_2D_char_c(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_2D_order_err_char_c

    ! Internal implementation: 2-d    
    pure module subroutine norm_2D_char_c(a, nrm, order, err)
        !> Input 2-d matrix a(:,:)
        complex(sp), intent(in), target :: a(:,:)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(sp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_c(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_2D_char_c


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_3D_order_char_c(a, order) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        complex(sp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm
                                    
        call norm_3D_char_c(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_3D_order_char_c
    
    ! Function interface with output error
    module function stdlib_linalg_norm_3D_order_err_char_c(a, order, err) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        complex(sp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm                 
                
        call norm_3D_char_c(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_3D_order_err_char_c

    ! Internal implementation: 3-d    
    pure module subroutine norm_3D_char_c(a, nrm, order, err)
        !> Input 3-d matrix a(:,:,:)
        complex(sp), intent(in), target :: a(:,:,:)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(sp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_c(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_3D_char_c


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_4D_order_char_c(a, order) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        complex(sp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm
                                    
        call norm_4D_char_c(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_4D_order_char_c
    
    ! Function interface with output error
    module function stdlib_linalg_norm_4D_order_err_char_c(a, order, err) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        complex(sp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm                 
                
        call norm_4D_char_c(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_4D_order_err_char_c

    ! Internal implementation: 4-d    
    pure module subroutine norm_4D_char_c(a, nrm, order, err)
        !> Input 4-d matrix a(:,:,:,:)
        complex(sp), intent(in), target :: a(:,:,:,:)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(sp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_c(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_4D_char_c


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_5D_order_char_c(a, order) result(nrm)
        !> Input 5-d matrix a(:,:,:,:,:)
        complex(sp), intent(in) :: a(:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm
                                    
        call norm_5D_char_c(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_5D_order_char_c
    
    ! Function interface with output error
    module function stdlib_linalg_norm_5D_order_err_char_c(a, order, err) result(nrm)
        !> Input 5-d matrix a(:,:,:,:,:)
        complex(sp), intent(in) :: a(:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm                 
                
        call norm_5D_char_c(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_5D_order_err_char_c

    ! Internal implementation: 5-d    
    pure module subroutine norm_5D_char_c(a, nrm, order, err)
        !> Input 5-d matrix a(:,:,:,:,:)
        complex(sp), intent(in), target :: a(:,:,:,:,:)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(sp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_c(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_5D_char_c


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_6D_order_char_c(a, order) result(nrm)
        !> Input 6-d matrix a(:,:,:,:,:,:)
        complex(sp), intent(in) :: a(:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm
                                    
        call norm_6D_char_c(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_6D_order_char_c
    
    ! Function interface with output error
    module function stdlib_linalg_norm_6D_order_err_char_c(a, order, err) result(nrm)
        !> Input 6-d matrix a(:,:,:,:,:,:)
        complex(sp), intent(in) :: a(:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm                 
                
        call norm_6D_char_c(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_6D_order_err_char_c

    ! Internal implementation: 6-d    
    pure module subroutine norm_6D_char_c(a, nrm, order, err)
        !> Input 6-d matrix a(:,:,:,:,:,:)
        complex(sp), intent(in), target :: a(:,:,:,:,:,:)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(sp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_c(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_6D_char_c


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_7D_order_char_c(a, order) result(nrm)
        !> Input 7-d matrix a(:,:,:,:,:,:,:)
        complex(sp), intent(in) :: a(:,:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm
                                    
        call norm_7D_char_c(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_7D_order_char_c
    
    ! Function interface with output error
    module function stdlib_linalg_norm_7D_order_err_char_c(a, order, err) result(nrm)
        !> Input 7-d matrix a(:,:,:,:,:,:,:)
        complex(sp), intent(in) :: a(:,:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm                 
                
        call norm_7D_char_c(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_7D_order_err_char_c

    ! Internal implementation: 7-d    
    pure module subroutine norm_7D_char_c(a, nrm, order, err)
        !> Input 7-d matrix a(:,:,:,:,:,:,:)
        complex(sp), intent(in), target :: a(:,:,:,:,:,:,:)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(sp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_c(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_7D_char_c


    !====================================================================
    ! Norms : any rank to rank-1, with DIM specifier and char input
    !====================================================================

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_2D_to_1D_char_c(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        
        call norm_2D_to_1D_char_c(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_2D_to_1D_char_c

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_2D_to_1D_err_char_c(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        
        call norm_2D_to_1D_char_c(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_2D_to_1D_err_char_c
    
    ! Internal implementation 
    pure module subroutine norm_2D_to_1D_char_c(a, nrm, order, dim, err)
        !> Input matrix a[..]
        complex(sp), intent(in) :: a(:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2
        logical :: contiguous_data
        real(sp) :: rorder
        integer(ilp), dimension(2) :: spe,spack,perm,iperm
        integer(ilp), dimension(2), parameter :: dim_range = [(lda,lda=1_ilp,2_ilp)]
        complex(sp), allocatable :: apack(:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>2) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_c(lda, apack(:, j2), &
                    nrm(j2), norm_request)
            enddo            
            
        else
            
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_c(lda, a(:, j2), &
                    nrm(j2), norm_request)
            enddo               
            
        endif        
        
    end subroutine norm_2D_to_1D_char_c

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_3D_to_2D_char_c(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        
        call norm_3D_to_2D_char_c(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_3D_to_2D_char_c

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_3D_to_2D_err_char_c(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        
        call norm_3D_to_2D_char_c(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_3D_to_2D_err_char_c
    
    ! Internal implementation 
    pure module subroutine norm_3D_to_2D_char_c(a, nrm, order, dim, err)
        !> Input matrix a[..]
        complex(sp), intent(in) :: a(:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3
        logical :: contiguous_data
        real(sp) :: rorder
        integer(ilp), dimension(3) :: spe,spack,perm,iperm
        integer(ilp), dimension(3), parameter :: dim_range = [(lda,lda=1_ilp,3_ilp)]
        complex(sp), allocatable :: apack(:,:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>3) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_c(lda, apack(:, j2, j3), &
                    nrm(j2, j3), norm_request)
            enddo; enddo            
            
        else
            
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_c(lda, a(:, j2, j3), &
                    nrm(j2, j3), norm_request)
            enddo; enddo               
            
        endif        
        
    end subroutine norm_3D_to_2D_char_c

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_4D_to_3D_char_c(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))     
        
        call norm_4D_to_3D_char_c(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_4D_to_3D_char_c

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_4D_to_3D_err_char_c(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))     
        
        call norm_4D_to_3D_char_c(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_4D_to_3D_err_char_c
    
    ! Internal implementation 
    pure module subroutine norm_4D_to_3D_char_c(a, nrm, order, dim, err)
        !> Input matrix a[..]
        complex(sp), intent(in) :: a(:,:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
            & merge(size(a, 3), size(a, 4), mask=3<dim))     
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3, j4
        logical :: contiguous_data
        real(sp) :: rorder
        integer(ilp), dimension(4) :: spe,spack,perm,iperm
        integer(ilp), dimension(4), parameter :: dim_range = [(lda,lda=1_ilp,4_ilp)]
        complex(sp), allocatable :: apack(:,:,:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>4) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j4 = lbound(apack, 4), ubound(apack, 4)
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_c(lda, apack(:, j2, j3, j4), &
                    nrm(j2, j3, j4), norm_request)
            enddo; enddo; enddo            
            
        else
            
            do j4 = lbound(a, 4), ubound(a, 4)
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_c(lda, a(:, j2, j3, j4), &
                    nrm(j2, j3, j4), norm_request)
            enddo; enddo; enddo               
            
        endif        
        
    end subroutine norm_4D_to_3D_char_c

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_5D_to_4D_char_c(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim))     
        
        call norm_5D_to_4D_char_c(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_5D_to_4D_char_c

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_5D_to_4D_err_char_c(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim))     
        
        call norm_5D_to_4D_char_c(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_5D_to_4D_err_char_c
    
    ! Internal implementation 
    pure module subroutine norm_5D_to_4D_char_c(a, nrm, order, dim, err)
        !> Input matrix a[..]
        complex(sp), intent(in) :: a(:,:,:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
            & merge(size(a, 3), size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim))     
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3, j4, j5
        logical :: contiguous_data
        real(sp) :: rorder
        integer(ilp), dimension(5) :: spe,spack,perm,iperm
        integer(ilp), dimension(5), parameter :: dim_range = [(lda,lda=1_ilp,5_ilp)]
        complex(sp), allocatable :: apack(:,:,:,:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>5) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j5 = lbound(apack, 5), ubound(apack, 5)
            do j4 = lbound(apack, 4), ubound(apack, 4)
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_c(lda, apack(:, j2, j3, j4, j5), &
                    nrm(j2, j3, j4, j5), norm_request)
            enddo; enddo; enddo; enddo            
            
        else
            
            do j5 = lbound(a, 5), ubound(a, 5)
            do j4 = lbound(a, 4), ubound(a, 4)
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_c(lda, a(:, j2, j3, j4, j5), &
                    nrm(j2, j3, j4, j5), norm_request)
            enddo; enddo; enddo; enddo               
            
        endif        
        
    end subroutine norm_5D_to_4D_char_c

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_6D_to_5D_char_c(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6), mask=5<dim))     
        
        call norm_6D_to_5D_char_c(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_6D_to_5D_char_c

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_6D_to_5D_err_char_c(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6), mask=5<dim))     
        
        call norm_6D_to_5D_char_c(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_6D_to_5D_err_char_c
    
    ! Internal implementation 
    pure module subroutine norm_6D_to_5D_char_c(a, nrm, order, dim, err)
        !> Input matrix a[..]
        complex(sp), intent(in) :: a(:,:,:,:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
            & merge(size(a, 3), size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6),&
            & mask=5<dim))     
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3, j4, j5, j6
        logical :: contiguous_data
        real(sp) :: rorder
        integer(ilp), dimension(6) :: spe,spack,perm,iperm
        integer(ilp), dimension(6), parameter :: dim_range = [(lda,lda=1_ilp,6_ilp)]
        complex(sp), allocatable :: apack(:,:,:,:,:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>6) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j6 = lbound(apack, 6), ubound(apack, 6)
            do j5 = lbound(apack, 5), ubound(apack, 5)
            do j4 = lbound(apack, 4), ubound(apack, 4)
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_c(lda, apack(:, j2, j3, j4, j5, j6), &
                    nrm(j2, j3, j4, j5, j6), norm_request)
            enddo; enddo; enddo; enddo; enddo            
            
        else
            
            do j6 = lbound(a, 6), ubound(a, 6)
            do j5 = lbound(a, 5), ubound(a, 5)
            do j4 = lbound(a, 4), ubound(a, 4)
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_c(lda, a(:, j2, j3, j4, j5, j6), &
                    nrm(j2, j3, j4, j5, j6), norm_request)
            enddo; enddo; enddo; enddo; enddo               
            
        endif        
        
    end subroutine norm_6D_to_5D_char_c

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_7D_to_6D_char_c(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6), mask=5<dim),&
            & merge(size(a, 6), size(a, 7), mask=6<dim))     
        
        call norm_7D_to_6D_char_c(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_7D_to_6D_char_c

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_7D_to_6D_err_char_c(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6), mask=5<dim),&
            & merge(size(a, 6), size(a, 7), mask=6<dim))     
        
        call norm_7D_to_6D_char_c(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_7D_to_6D_err_char_c
    
    ! Internal implementation 
    pure module subroutine norm_7D_to_6D_char_c(a, nrm, order, dim, err)
        !> Input matrix a[..]
        complex(sp), intent(in) :: a(:,:,:,:,:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
            & merge(size(a, 3), size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6),&
            & mask=5<dim), merge(size(a, 6), size(a, 7), mask=6<dim))     
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3, j4, j5, j6, j7
        logical :: contiguous_data
        real(sp) :: rorder
        integer(ilp), dimension(7) :: spe,spack,perm,iperm
        integer(ilp), dimension(7), parameter :: dim_range = [(lda,lda=1_ilp,7_ilp)]
        complex(sp), allocatable :: apack(:,:,:,:,:,:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>7) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j7 = lbound(apack, 7), ubound(apack, 7)
            do j6 = lbound(apack, 6), ubound(apack, 6)
            do j5 = lbound(apack, 5), ubound(apack, 5)
            do j4 = lbound(apack, 4), ubound(apack, 4)
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_c(lda, apack(:, j2, j3, j4, j5, j6, j7), &
                    nrm(j2, j3, j4, j5, j6, j7), norm_request)
            enddo; enddo; enddo; enddo; enddo; enddo            
            
        else
            
            do j7 = lbound(a, 7), ubound(a, 7)
            do j6 = lbound(a, 6), ubound(a, 6)
            do j5 = lbound(a, 5), ubound(a, 5)
            do j4 = lbound(a, 4), ubound(a, 4)
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_c(lda, a(:, j2, j3, j4, j5, j6, j7), &
                    nrm(j2, j3, j4, j5, j6, j7), norm_request)
            enddo; enddo; enddo; enddo; enddo; enddo               
            
        endif        
        
    end subroutine norm_7D_to_6D_char_c


    !==============================================
    ! Norms : any rank to scalar
    !==============================================


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_1D_order_int_c(a, order) result(nrm)
        !> Input 1-d matrix a(:)
        complex(sp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm
                                    
        call norm_1D_int_c(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_1D_order_int_c
    
    ! Function interface with output error
    module function stdlib_linalg_norm_1D_order_err_int_c(a, order, err) result(nrm)
        !> Input 1-d matrix a(:)
        complex(sp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm                 
                
        call norm_1D_int_c(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_1D_order_err_int_c

    ! Internal implementation: 1-d    
    pure module subroutine norm_1D_int_c(a, nrm, order, err)
        !> Input 1-d matrix a(:)
        complex(sp), intent(in), target :: a(:)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(sp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_c(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_1D_int_c


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_2D_order_int_c(a, order) result(nrm)
        !> Input 2-d matrix a(:,:)
        complex(sp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm
                                    
        call norm_2D_int_c(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_2D_order_int_c
    
    ! Function interface with output error
    module function stdlib_linalg_norm_2D_order_err_int_c(a, order, err) result(nrm)
        !> Input 2-d matrix a(:,:)
        complex(sp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm                 
                
        call norm_2D_int_c(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_2D_order_err_int_c

    ! Internal implementation: 2-d    
    pure module subroutine norm_2D_int_c(a, nrm, order, err)
        !> Input 2-d matrix a(:,:)
        complex(sp), intent(in), target :: a(:,:)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(sp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_c(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_2D_int_c


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_3D_order_int_c(a, order) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        complex(sp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm
                                    
        call norm_3D_int_c(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_3D_order_int_c
    
    ! Function interface with output error
    module function stdlib_linalg_norm_3D_order_err_int_c(a, order, err) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        complex(sp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm                 
                
        call norm_3D_int_c(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_3D_order_err_int_c

    ! Internal implementation: 3-d    
    pure module subroutine norm_3D_int_c(a, nrm, order, err)
        !> Input 3-d matrix a(:,:,:)
        complex(sp), intent(in), target :: a(:,:,:)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(sp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_c(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_3D_int_c


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_4D_order_int_c(a, order) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        complex(sp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm
                                    
        call norm_4D_int_c(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_4D_order_int_c
    
    ! Function interface with output error
    module function stdlib_linalg_norm_4D_order_err_int_c(a, order, err) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        complex(sp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm                 
                
        call norm_4D_int_c(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_4D_order_err_int_c

    ! Internal implementation: 4-d    
    pure module subroutine norm_4D_int_c(a, nrm, order, err)
        !> Input 4-d matrix a(:,:,:,:)
        complex(sp), intent(in), target :: a(:,:,:,:)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(sp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_c(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_4D_int_c


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_5D_order_int_c(a, order) result(nrm)
        !> Input 5-d matrix a(:,:,:,:,:)
        complex(sp), intent(in) :: a(:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm
                                    
        call norm_5D_int_c(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_5D_order_int_c
    
    ! Function interface with output error
    module function stdlib_linalg_norm_5D_order_err_int_c(a, order, err) result(nrm)
        !> Input 5-d matrix a(:,:,:,:,:)
        complex(sp), intent(in) :: a(:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm                 
                
        call norm_5D_int_c(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_5D_order_err_int_c

    ! Internal implementation: 5-d    
    pure module subroutine norm_5D_int_c(a, nrm, order, err)
        !> Input 5-d matrix a(:,:,:,:,:)
        complex(sp), intent(in), target :: a(:,:,:,:,:)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(sp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_c(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_5D_int_c


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_6D_order_int_c(a, order) result(nrm)
        !> Input 6-d matrix a(:,:,:,:,:,:)
        complex(sp), intent(in) :: a(:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm
                                    
        call norm_6D_int_c(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_6D_order_int_c
    
    ! Function interface with output error
    module function stdlib_linalg_norm_6D_order_err_int_c(a, order, err) result(nrm)
        !> Input 6-d matrix a(:,:,:,:,:,:)
        complex(sp), intent(in) :: a(:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm                 
                
        call norm_6D_int_c(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_6D_order_err_int_c

    ! Internal implementation: 6-d    
    pure module subroutine norm_6D_int_c(a, nrm, order, err)
        !> Input 6-d matrix a(:,:,:,:,:,:)
        complex(sp), intent(in), target :: a(:,:,:,:,:,:)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(sp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_c(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_6D_int_c


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_7D_order_int_c(a, order) result(nrm)
        !> Input 7-d matrix a(:,:,:,:,:,:,:)
        complex(sp), intent(in) :: a(:,:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm
                                    
        call norm_7D_int_c(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_7D_order_int_c
    
    ! Function interface with output error
    module function stdlib_linalg_norm_7D_order_err_int_c(a, order, err) result(nrm)
        !> Input 7-d matrix a(:,:,:,:,:,:,:)
        complex(sp), intent(in) :: a(:,:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm                 
                
        call norm_7D_int_c(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_7D_order_err_int_c

    ! Internal implementation: 7-d    
    pure module subroutine norm_7D_int_c(a, nrm, order, err)
        !> Input 7-d matrix a(:,:,:,:,:,:,:)
        complex(sp), intent(in), target :: a(:,:,:,:,:,:,:)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(sp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_c(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_7D_int_c


    !====================================================================
    ! Norms : any rank to rank-1, with DIM specifier and int input
    !====================================================================

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_2D_to_1D_int_c(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        
        call norm_2D_to_1D_int_c(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_2D_to_1D_int_c

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_2D_to_1D_err_int_c(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        
        call norm_2D_to_1D_int_c(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_2D_to_1D_err_int_c
    
    ! Internal implementation 
    pure module subroutine norm_2D_to_1D_int_c(a, nrm, order, dim, err)
        !> Input matrix a[..]
        complex(sp), intent(in) :: a(:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2
        logical :: contiguous_data
        real(sp) :: rorder
        integer(ilp), dimension(2) :: spe,spack,perm,iperm
        integer(ilp), dimension(2), parameter :: dim_range = [(lda,lda=1_ilp,2_ilp)]
        complex(sp), allocatable :: apack(:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>2) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_c(lda, apack(:, j2), &
                    nrm(j2), norm_request)
            enddo            
            
        else
            
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_c(lda, a(:, j2), &
                    nrm(j2), norm_request)
            enddo               
            
        endif        
        
    end subroutine norm_2D_to_1D_int_c

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_3D_to_2D_int_c(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        
        call norm_3D_to_2D_int_c(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_3D_to_2D_int_c

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_3D_to_2D_err_int_c(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        
        call norm_3D_to_2D_int_c(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_3D_to_2D_err_int_c
    
    ! Internal implementation 
    pure module subroutine norm_3D_to_2D_int_c(a, nrm, order, dim, err)
        !> Input matrix a[..]
        complex(sp), intent(in) :: a(:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3
        logical :: contiguous_data
        real(sp) :: rorder
        integer(ilp), dimension(3) :: spe,spack,perm,iperm
        integer(ilp), dimension(3), parameter :: dim_range = [(lda,lda=1_ilp,3_ilp)]
        complex(sp), allocatable :: apack(:,:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>3) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_c(lda, apack(:, j2, j3), &
                    nrm(j2, j3), norm_request)
            enddo; enddo            
            
        else
            
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_c(lda, a(:, j2, j3), &
                    nrm(j2, j3), norm_request)
            enddo; enddo               
            
        endif        
        
    end subroutine norm_3D_to_2D_int_c

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_4D_to_3D_int_c(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))     
        
        call norm_4D_to_3D_int_c(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_4D_to_3D_int_c

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_4D_to_3D_err_int_c(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))     
        
        call norm_4D_to_3D_int_c(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_4D_to_3D_err_int_c
    
    ! Internal implementation 
    pure module subroutine norm_4D_to_3D_int_c(a, nrm, order, dim, err)
        !> Input matrix a[..]
        complex(sp), intent(in) :: a(:,:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
            & merge(size(a, 3), size(a, 4), mask=3<dim))     
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3, j4
        logical :: contiguous_data
        real(sp) :: rorder
        integer(ilp), dimension(4) :: spe,spack,perm,iperm
        integer(ilp), dimension(4), parameter :: dim_range = [(lda,lda=1_ilp,4_ilp)]
        complex(sp), allocatable :: apack(:,:,:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>4) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j4 = lbound(apack, 4), ubound(apack, 4)
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_c(lda, apack(:, j2, j3, j4), &
                    nrm(j2, j3, j4), norm_request)
            enddo; enddo; enddo            
            
        else
            
            do j4 = lbound(a, 4), ubound(a, 4)
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_c(lda, a(:, j2, j3, j4), &
                    nrm(j2, j3, j4), norm_request)
            enddo; enddo; enddo               
            
        endif        
        
    end subroutine norm_4D_to_3D_int_c

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_5D_to_4D_int_c(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim))     
        
        call norm_5D_to_4D_int_c(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_5D_to_4D_int_c

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_5D_to_4D_err_int_c(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim))     
        
        call norm_5D_to_4D_int_c(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_5D_to_4D_err_int_c
    
    ! Internal implementation 
    pure module subroutine norm_5D_to_4D_int_c(a, nrm, order, dim, err)
        !> Input matrix a[..]
        complex(sp), intent(in) :: a(:,:,:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
            & merge(size(a, 3), size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim))     
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3, j4, j5
        logical :: contiguous_data
        real(sp) :: rorder
        integer(ilp), dimension(5) :: spe,spack,perm,iperm
        integer(ilp), dimension(5), parameter :: dim_range = [(lda,lda=1_ilp,5_ilp)]
        complex(sp), allocatable :: apack(:,:,:,:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>5) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j5 = lbound(apack, 5), ubound(apack, 5)
            do j4 = lbound(apack, 4), ubound(apack, 4)
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_c(lda, apack(:, j2, j3, j4, j5), &
                    nrm(j2, j3, j4, j5), norm_request)
            enddo; enddo; enddo; enddo            
            
        else
            
            do j5 = lbound(a, 5), ubound(a, 5)
            do j4 = lbound(a, 4), ubound(a, 4)
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_c(lda, a(:, j2, j3, j4, j5), &
                    nrm(j2, j3, j4, j5), norm_request)
            enddo; enddo; enddo; enddo               
            
        endif        
        
    end subroutine norm_5D_to_4D_int_c

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_6D_to_5D_int_c(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6), mask=5<dim))     
        
        call norm_6D_to_5D_int_c(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_6D_to_5D_int_c

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_6D_to_5D_err_int_c(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6), mask=5<dim))     
        
        call norm_6D_to_5D_int_c(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_6D_to_5D_err_int_c
    
    ! Internal implementation 
    pure module subroutine norm_6D_to_5D_int_c(a, nrm, order, dim, err)
        !> Input matrix a[..]
        complex(sp), intent(in) :: a(:,:,:,:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
            & merge(size(a, 3), size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6),&
            & mask=5<dim))     
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3, j4, j5, j6
        logical :: contiguous_data
        real(sp) :: rorder
        integer(ilp), dimension(6) :: spe,spack,perm,iperm
        integer(ilp), dimension(6), parameter :: dim_range = [(lda,lda=1_ilp,6_ilp)]
        complex(sp), allocatable :: apack(:,:,:,:,:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>6) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j6 = lbound(apack, 6), ubound(apack, 6)
            do j5 = lbound(apack, 5), ubound(apack, 5)
            do j4 = lbound(apack, 4), ubound(apack, 4)
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_c(lda, apack(:, j2, j3, j4, j5, j6), &
                    nrm(j2, j3, j4, j5, j6), norm_request)
            enddo; enddo; enddo; enddo; enddo            
            
        else
            
            do j6 = lbound(a, 6), ubound(a, 6)
            do j5 = lbound(a, 5), ubound(a, 5)
            do j4 = lbound(a, 4), ubound(a, 4)
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_c(lda, a(:, j2, j3, j4, j5, j6), &
                    nrm(j2, j3, j4, j5, j6), norm_request)
            enddo; enddo; enddo; enddo; enddo               
            
        endif        
        
    end subroutine norm_6D_to_5D_int_c

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_7D_to_6D_int_c(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6), mask=5<dim),&
            & merge(size(a, 6), size(a, 7), mask=6<dim))     
        
        call norm_7D_to_6D_int_c(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_7D_to_6D_int_c

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_7D_to_6D_err_int_c(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6), mask=5<dim),&
            & merge(size(a, 6), size(a, 7), mask=6<dim))     
        
        call norm_7D_to_6D_int_c(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_7D_to_6D_err_int_c
    
    ! Internal implementation 
    pure module subroutine norm_7D_to_6D_int_c(a, nrm, order, dim, err)
        !> Input matrix a[..]
        complex(sp), intent(in) :: a(:,:,:,:,:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
            & merge(size(a, 3), size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6),&
            & mask=5<dim), merge(size(a, 6), size(a, 7), mask=6<dim))     
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3, j4, j5, j6, j7
        logical :: contiguous_data
        real(sp) :: rorder
        integer(ilp), dimension(7) :: spe,spack,perm,iperm
        integer(ilp), dimension(7), parameter :: dim_range = [(lda,lda=1_ilp,7_ilp)]
        complex(sp), allocatable :: apack(:,:,:,:,:,:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>7) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j7 = lbound(apack, 7), ubound(apack, 7)
            do j6 = lbound(apack, 6), ubound(apack, 6)
            do j5 = lbound(apack, 5), ubound(apack, 5)
            do j4 = lbound(apack, 4), ubound(apack, 4)
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_c(lda, apack(:, j2, j3, j4, j5, j6, j7), &
                    nrm(j2, j3, j4, j5, j6, j7), norm_request)
            enddo; enddo; enddo; enddo; enddo; enddo            
            
        else
            
            do j7 = lbound(a, 7), ubound(a, 7)
            do j6 = lbound(a, 6), ubound(a, 6)
            do j5 = lbound(a, 5), ubound(a, 5)
            do j4 = lbound(a, 4), ubound(a, 4)
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_c(lda, a(:, j2, j3, j4, j5, j6, j7), &
                    nrm(j2, j3, j4, j5, j6, j7), norm_request)
            enddo; enddo; enddo; enddo; enddo; enddo               
            
        endif        
        
    end subroutine norm_7D_to_6D_int_c

    
    ! Compute stride of a 1d array
    pure integer(ilp) function stride_1d_z(a) result(stride)
        !> Input 1-d array 
        complex(dp), intent(in), target :: a(:)
        
        integer(c_intptr_t) :: a1,a2
        
        if (size(a,kind=ilp)<=1_ilp) then 
           stride = 1_ilp
        else
           a1 = transfer(c_loc(a(1)),a1)
           a2 = transfer(c_loc(a(2)),a2)
           stride = bit_size(0_c_char)*int(a2-a1, ilp)/storage_size(a, kind=ilp)
        endif
        
    end function stride_1d_z
    
    ! Private internal 1D implementation. This has to be used only internally, 
    ! when all inputs are already checked for correctness.
    pure subroutine internal_norm_1D_z(sze, a, nrm, norm_request)
        !> Input matrix length
        integer(ilp), intent(in) :: sze
        !> Input contiguous 1-d matrix a(*)
        complex(dp), intent(in) :: a(sze)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Internal matrix request 
        integer(ilp), intent(in) :: norm_request
        
        integer(ilp) :: i
        real(dp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        select case(norm_request)
            case(NORM_ONE)
                nrm = asum(sze,a,incx=1_ilp)
            case(NORM_TWO)            
                nrm = nrm2(sze,a,incx=1_ilp)
            case(NORM_INF)
                ! The default BLAS stdlib_izamax uses |Re(.)|+|Im(.)|. Do not use it.
                i = stdlib_izmax1(sze,a,incx=1_ilp)
                nrm = abs(a(i))
            case(NORM_MINUSINF)
                nrm = minval( abs(a) )
            case (NORM_POW_FIRST:NORM_POW_LAST)
                rorder = 1.0_dp / norm_request
                nrm = sum( abs(a) ** norm_request ) ** rorder
        end select
        
    end subroutine internal_norm_1D_z     
    

    !==============================================
    ! Norms : any rank to scalar
    !==============================================


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_1D_order_char_z(a, order) result(nrm)
        !> Input 1-d matrix a(:)
        complex(dp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm
                                    
        call norm_1D_char_z(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_1D_order_char_z
    
    ! Function interface with output error
    module function stdlib_linalg_norm_1D_order_err_char_z(a, order, err) result(nrm)
        !> Input 1-d matrix a(:)
        complex(dp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm                 
                
        call norm_1D_char_z(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_1D_order_err_char_z

    ! Internal implementation: 1-d    
    pure module subroutine norm_1D_char_z(a, nrm, order, err)
        !> Input 1-d matrix a(:)
        complex(dp), intent(in), target :: a(:)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(dp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_z(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_1D_char_z


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_2D_order_char_z(a, order) result(nrm)
        !> Input 2-d matrix a(:,:)
        complex(dp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm
                                    
        call norm_2D_char_z(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_2D_order_char_z
    
    ! Function interface with output error
    module function stdlib_linalg_norm_2D_order_err_char_z(a, order, err) result(nrm)
        !> Input 2-d matrix a(:,:)
        complex(dp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm                 
                
        call norm_2D_char_z(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_2D_order_err_char_z

    ! Internal implementation: 2-d    
    pure module subroutine norm_2D_char_z(a, nrm, order, err)
        !> Input 2-d matrix a(:,:)
        complex(dp), intent(in), target :: a(:,:)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(dp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_z(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_2D_char_z


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_3D_order_char_z(a, order) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        complex(dp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm
                                    
        call norm_3D_char_z(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_3D_order_char_z
    
    ! Function interface with output error
    module function stdlib_linalg_norm_3D_order_err_char_z(a, order, err) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        complex(dp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm                 
                
        call norm_3D_char_z(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_3D_order_err_char_z

    ! Internal implementation: 3-d    
    pure module subroutine norm_3D_char_z(a, nrm, order, err)
        !> Input 3-d matrix a(:,:,:)
        complex(dp), intent(in), target :: a(:,:,:)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(dp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_z(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_3D_char_z


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_4D_order_char_z(a, order) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        complex(dp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm
                                    
        call norm_4D_char_z(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_4D_order_char_z
    
    ! Function interface with output error
    module function stdlib_linalg_norm_4D_order_err_char_z(a, order, err) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        complex(dp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm                 
                
        call norm_4D_char_z(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_4D_order_err_char_z

    ! Internal implementation: 4-d    
    pure module subroutine norm_4D_char_z(a, nrm, order, err)
        !> Input 4-d matrix a(:,:,:,:)
        complex(dp), intent(in), target :: a(:,:,:,:)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(dp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_z(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_4D_char_z


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_5D_order_char_z(a, order) result(nrm)
        !> Input 5-d matrix a(:,:,:,:,:)
        complex(dp), intent(in) :: a(:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm
                                    
        call norm_5D_char_z(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_5D_order_char_z
    
    ! Function interface with output error
    module function stdlib_linalg_norm_5D_order_err_char_z(a, order, err) result(nrm)
        !> Input 5-d matrix a(:,:,:,:,:)
        complex(dp), intent(in) :: a(:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm                 
                
        call norm_5D_char_z(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_5D_order_err_char_z

    ! Internal implementation: 5-d    
    pure module subroutine norm_5D_char_z(a, nrm, order, err)
        !> Input 5-d matrix a(:,:,:,:,:)
        complex(dp), intent(in), target :: a(:,:,:,:,:)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(dp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_z(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_5D_char_z


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_6D_order_char_z(a, order) result(nrm)
        !> Input 6-d matrix a(:,:,:,:,:,:)
        complex(dp), intent(in) :: a(:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm
                                    
        call norm_6D_char_z(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_6D_order_char_z
    
    ! Function interface with output error
    module function stdlib_linalg_norm_6D_order_err_char_z(a, order, err) result(nrm)
        !> Input 6-d matrix a(:,:,:,:,:,:)
        complex(dp), intent(in) :: a(:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm                 
                
        call norm_6D_char_z(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_6D_order_err_char_z

    ! Internal implementation: 6-d    
    pure module subroutine norm_6D_char_z(a, nrm, order, err)
        !> Input 6-d matrix a(:,:,:,:,:,:)
        complex(dp), intent(in), target :: a(:,:,:,:,:,:)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(dp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_z(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_6D_char_z


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_7D_order_char_z(a, order) result(nrm)
        !> Input 7-d matrix a(:,:,:,:,:,:,:)
        complex(dp), intent(in) :: a(:,:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm
                                    
        call norm_7D_char_z(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_7D_order_char_z
    
    ! Function interface with output error
    module function stdlib_linalg_norm_7D_order_err_char_z(a, order, err) result(nrm)
        !> Input 7-d matrix a(:,:,:,:,:,:,:)
        complex(dp), intent(in) :: a(:,:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm                 
                
        call norm_7D_char_z(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_7D_order_err_char_z

    ! Internal implementation: 7-d    
    pure module subroutine norm_7D_char_z(a, nrm, order, err)
        !> Input 7-d matrix a(:,:,:,:,:,:,:)
        complex(dp), intent(in), target :: a(:,:,:,:,:,:,:)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(dp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_z(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_7D_char_z


    !====================================================================
    ! Norms : any rank to rank-1, with DIM specifier and char input
    !====================================================================

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_2D_to_1D_char_z(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        
        call norm_2D_to_1D_char_z(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_2D_to_1D_char_z

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_2D_to_1D_err_char_z(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        
        call norm_2D_to_1D_char_z(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_2D_to_1D_err_char_z
    
    ! Internal implementation 
    pure module subroutine norm_2D_to_1D_char_z(a, nrm, order, dim, err)
        !> Input matrix a[..]
        complex(dp), intent(in) :: a(:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2
        logical :: contiguous_data
        real(dp) :: rorder
        integer(ilp), dimension(2) :: spe,spack,perm,iperm
        integer(ilp), dimension(2), parameter :: dim_range = [(lda,lda=1_ilp,2_ilp)]
        complex(dp), allocatable :: apack(:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>2) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_z(lda, apack(:, j2), &
                    nrm(j2), norm_request)
            enddo            
            
        else
            
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_z(lda, a(:, j2), &
                    nrm(j2), norm_request)
            enddo               
            
        endif        
        
    end subroutine norm_2D_to_1D_char_z

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_3D_to_2D_char_z(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        
        call norm_3D_to_2D_char_z(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_3D_to_2D_char_z

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_3D_to_2D_err_char_z(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        
        call norm_3D_to_2D_char_z(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_3D_to_2D_err_char_z
    
    ! Internal implementation 
    pure module subroutine norm_3D_to_2D_char_z(a, nrm, order, dim, err)
        !> Input matrix a[..]
        complex(dp), intent(in) :: a(:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3
        logical :: contiguous_data
        real(dp) :: rorder
        integer(ilp), dimension(3) :: spe,spack,perm,iperm
        integer(ilp), dimension(3), parameter :: dim_range = [(lda,lda=1_ilp,3_ilp)]
        complex(dp), allocatable :: apack(:,:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>3) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_z(lda, apack(:, j2, j3), &
                    nrm(j2, j3), norm_request)
            enddo; enddo            
            
        else
            
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_z(lda, a(:, j2, j3), &
                    nrm(j2, j3), norm_request)
            enddo; enddo               
            
        endif        
        
    end subroutine norm_3D_to_2D_char_z

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_4D_to_3D_char_z(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))     
        
        call norm_4D_to_3D_char_z(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_4D_to_3D_char_z

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_4D_to_3D_err_char_z(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))     
        
        call norm_4D_to_3D_char_z(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_4D_to_3D_err_char_z
    
    ! Internal implementation 
    pure module subroutine norm_4D_to_3D_char_z(a, nrm, order, dim, err)
        !> Input matrix a[..]
        complex(dp), intent(in) :: a(:,:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
            & merge(size(a, 3), size(a, 4), mask=3<dim))     
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3, j4
        logical :: contiguous_data
        real(dp) :: rorder
        integer(ilp), dimension(4) :: spe,spack,perm,iperm
        integer(ilp), dimension(4), parameter :: dim_range = [(lda,lda=1_ilp,4_ilp)]
        complex(dp), allocatable :: apack(:,:,:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>4) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j4 = lbound(apack, 4), ubound(apack, 4)
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_z(lda, apack(:, j2, j3, j4), &
                    nrm(j2, j3, j4), norm_request)
            enddo; enddo; enddo            
            
        else
            
            do j4 = lbound(a, 4), ubound(a, 4)
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_z(lda, a(:, j2, j3, j4), &
                    nrm(j2, j3, j4), norm_request)
            enddo; enddo; enddo               
            
        endif        
        
    end subroutine norm_4D_to_3D_char_z

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_5D_to_4D_char_z(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim))     
        
        call norm_5D_to_4D_char_z(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_5D_to_4D_char_z

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_5D_to_4D_err_char_z(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim))     
        
        call norm_5D_to_4D_char_z(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_5D_to_4D_err_char_z
    
    ! Internal implementation 
    pure module subroutine norm_5D_to_4D_char_z(a, nrm, order, dim, err)
        !> Input matrix a[..]
        complex(dp), intent(in) :: a(:,:,:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
            & merge(size(a, 3), size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim))     
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3, j4, j5
        logical :: contiguous_data
        real(dp) :: rorder
        integer(ilp), dimension(5) :: spe,spack,perm,iperm
        integer(ilp), dimension(5), parameter :: dim_range = [(lda,lda=1_ilp,5_ilp)]
        complex(dp), allocatable :: apack(:,:,:,:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>5) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j5 = lbound(apack, 5), ubound(apack, 5)
            do j4 = lbound(apack, 4), ubound(apack, 4)
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_z(lda, apack(:, j2, j3, j4, j5), &
                    nrm(j2, j3, j4, j5), norm_request)
            enddo; enddo; enddo; enddo            
            
        else
            
            do j5 = lbound(a, 5), ubound(a, 5)
            do j4 = lbound(a, 4), ubound(a, 4)
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_z(lda, a(:, j2, j3, j4, j5), &
                    nrm(j2, j3, j4, j5), norm_request)
            enddo; enddo; enddo; enddo               
            
        endif        
        
    end subroutine norm_5D_to_4D_char_z

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_6D_to_5D_char_z(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6), mask=5<dim))     
        
        call norm_6D_to_5D_char_z(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_6D_to_5D_char_z

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_6D_to_5D_err_char_z(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6), mask=5<dim))     
        
        call norm_6D_to_5D_char_z(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_6D_to_5D_err_char_z
    
    ! Internal implementation 
    pure module subroutine norm_6D_to_5D_char_z(a, nrm, order, dim, err)
        !> Input matrix a[..]
        complex(dp), intent(in) :: a(:,:,:,:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
            & merge(size(a, 3), size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6),&
            & mask=5<dim))     
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3, j4, j5, j6
        logical :: contiguous_data
        real(dp) :: rorder
        integer(ilp), dimension(6) :: spe,spack,perm,iperm
        integer(ilp), dimension(6), parameter :: dim_range = [(lda,lda=1_ilp,6_ilp)]
        complex(dp), allocatable :: apack(:,:,:,:,:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>6) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j6 = lbound(apack, 6), ubound(apack, 6)
            do j5 = lbound(apack, 5), ubound(apack, 5)
            do j4 = lbound(apack, 4), ubound(apack, 4)
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_z(lda, apack(:, j2, j3, j4, j5, j6), &
                    nrm(j2, j3, j4, j5, j6), norm_request)
            enddo; enddo; enddo; enddo; enddo            
            
        else
            
            do j6 = lbound(a, 6), ubound(a, 6)
            do j5 = lbound(a, 5), ubound(a, 5)
            do j4 = lbound(a, 4), ubound(a, 4)
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_z(lda, a(:, j2, j3, j4, j5, j6), &
                    nrm(j2, j3, j4, j5, j6), norm_request)
            enddo; enddo; enddo; enddo; enddo               
            
        endif        
        
    end subroutine norm_6D_to_5D_char_z

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_7D_to_6D_char_z(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6), mask=5<dim),&
            & merge(size(a, 6), size(a, 7), mask=6<dim))     
        
        call norm_7D_to_6D_char_z(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_7D_to_6D_char_z

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_7D_to_6D_err_char_z(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6), mask=5<dim),&
            & merge(size(a, 6), size(a, 7), mask=6<dim))     
        
        call norm_7D_to_6D_char_z(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_7D_to_6D_err_char_z
    
    ! Internal implementation 
    pure module subroutine norm_7D_to_6D_char_z(a, nrm, order, dim, err)
        !> Input matrix a[..]
        complex(dp), intent(in) :: a(:,:,:,:,:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
            & merge(size(a, 3), size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6),&
            & mask=5<dim), merge(size(a, 6), size(a, 7), mask=6<dim))     
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3, j4, j5, j6, j7
        logical :: contiguous_data
        real(dp) :: rorder
        integer(ilp), dimension(7) :: spe,spack,perm,iperm
        integer(ilp), dimension(7), parameter :: dim_range = [(lda,lda=1_ilp,7_ilp)]
        complex(dp), allocatable :: apack(:,:,:,:,:,:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>7) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j7 = lbound(apack, 7), ubound(apack, 7)
            do j6 = lbound(apack, 6), ubound(apack, 6)
            do j5 = lbound(apack, 5), ubound(apack, 5)
            do j4 = lbound(apack, 4), ubound(apack, 4)
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_z(lda, apack(:, j2, j3, j4, j5, j6, j7), &
                    nrm(j2, j3, j4, j5, j6, j7), norm_request)
            enddo; enddo; enddo; enddo; enddo; enddo            
            
        else
            
            do j7 = lbound(a, 7), ubound(a, 7)
            do j6 = lbound(a, 6), ubound(a, 6)
            do j5 = lbound(a, 5), ubound(a, 5)
            do j4 = lbound(a, 4), ubound(a, 4)
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_z(lda, a(:, j2, j3, j4, j5, j6, j7), &
                    nrm(j2, j3, j4, j5, j6, j7), norm_request)
            enddo; enddo; enddo; enddo; enddo; enddo               
            
        endif        
        
    end subroutine norm_7D_to_6D_char_z


    !==============================================
    ! Norms : any rank to scalar
    !==============================================


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_1D_order_int_z(a, order) result(nrm)
        !> Input 1-d matrix a(:)
        complex(dp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm
                                    
        call norm_1D_int_z(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_1D_order_int_z
    
    ! Function interface with output error
    module function stdlib_linalg_norm_1D_order_err_int_z(a, order, err) result(nrm)
        !> Input 1-d matrix a(:)
        complex(dp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm                 
                
        call norm_1D_int_z(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_1D_order_err_int_z

    ! Internal implementation: 1-d    
    pure module subroutine norm_1D_int_z(a, nrm, order, err)
        !> Input 1-d matrix a(:)
        complex(dp), intent(in), target :: a(:)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(dp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_z(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_1D_int_z


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_2D_order_int_z(a, order) result(nrm)
        !> Input 2-d matrix a(:,:)
        complex(dp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm
                                    
        call norm_2D_int_z(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_2D_order_int_z
    
    ! Function interface with output error
    module function stdlib_linalg_norm_2D_order_err_int_z(a, order, err) result(nrm)
        !> Input 2-d matrix a(:,:)
        complex(dp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm                 
                
        call norm_2D_int_z(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_2D_order_err_int_z

    ! Internal implementation: 2-d    
    pure module subroutine norm_2D_int_z(a, nrm, order, err)
        !> Input 2-d matrix a(:,:)
        complex(dp), intent(in), target :: a(:,:)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(dp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_z(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_2D_int_z


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_3D_order_int_z(a, order) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        complex(dp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm
                                    
        call norm_3D_int_z(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_3D_order_int_z
    
    ! Function interface with output error
    module function stdlib_linalg_norm_3D_order_err_int_z(a, order, err) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        complex(dp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm                 
                
        call norm_3D_int_z(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_3D_order_err_int_z

    ! Internal implementation: 3-d    
    pure module subroutine norm_3D_int_z(a, nrm, order, err)
        !> Input 3-d matrix a(:,:,:)
        complex(dp), intent(in), target :: a(:,:,:)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(dp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_z(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_3D_int_z


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_4D_order_int_z(a, order) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        complex(dp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm
                                    
        call norm_4D_int_z(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_4D_order_int_z
    
    ! Function interface with output error
    module function stdlib_linalg_norm_4D_order_err_int_z(a, order, err) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        complex(dp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm                 
                
        call norm_4D_int_z(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_4D_order_err_int_z

    ! Internal implementation: 4-d    
    pure module subroutine norm_4D_int_z(a, nrm, order, err)
        !> Input 4-d matrix a(:,:,:,:)
        complex(dp), intent(in), target :: a(:,:,:,:)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(dp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_z(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_4D_int_z


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_5D_order_int_z(a, order) result(nrm)
        !> Input 5-d matrix a(:,:,:,:,:)
        complex(dp), intent(in) :: a(:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm
                                    
        call norm_5D_int_z(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_5D_order_int_z
    
    ! Function interface with output error
    module function stdlib_linalg_norm_5D_order_err_int_z(a, order, err) result(nrm)
        !> Input 5-d matrix a(:,:,:,:,:)
        complex(dp), intent(in) :: a(:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm                 
                
        call norm_5D_int_z(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_5D_order_err_int_z

    ! Internal implementation: 5-d    
    pure module subroutine norm_5D_int_z(a, nrm, order, err)
        !> Input 5-d matrix a(:,:,:,:,:)
        complex(dp), intent(in), target :: a(:,:,:,:,:)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(dp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_z(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_5D_int_z


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_6D_order_int_z(a, order) result(nrm)
        !> Input 6-d matrix a(:,:,:,:,:,:)
        complex(dp), intent(in) :: a(:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm
                                    
        call norm_6D_int_z(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_6D_order_int_z
    
    ! Function interface with output error
    module function stdlib_linalg_norm_6D_order_err_int_z(a, order, err) result(nrm)
        !> Input 6-d matrix a(:,:,:,:,:,:)
        complex(dp), intent(in) :: a(:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm                 
                
        call norm_6D_int_z(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_6D_order_err_int_z

    ! Internal implementation: 6-d    
    pure module subroutine norm_6D_int_z(a, nrm, order, err)
        !> Input 6-d matrix a(:,:,:,:,:,:)
        complex(dp), intent(in), target :: a(:,:,:,:,:,:)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(dp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_z(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_6D_int_z


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_7D_order_int_z(a, order) result(nrm)
        !> Input 7-d matrix a(:,:,:,:,:,:,:)
        complex(dp), intent(in) :: a(:,:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm
                                    
        call norm_7D_int_z(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_7D_order_int_z
    
    ! Function interface with output error
    module function stdlib_linalg_norm_7D_order_err_int_z(a, order, err) result(nrm)
        !> Input 7-d matrix a(:,:,:,:,:,:,:)
        complex(dp), intent(in) :: a(:,:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm                 
                
        call norm_7D_int_z(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_7D_order_err_int_z

    ! Internal implementation: 7-d    
    pure module subroutine norm_7D_int_z(a, nrm, order, err)
        !> Input 7-d matrix a(:,:,:,:,:,:,:)
        complex(dp), intent(in), target :: a(:,:,:,:,:,:,:)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        real(dp) :: rorder
        intrinsic :: abs, sum, sqrt, maxval, minval, conjg
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_z(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_7D_int_z


    !====================================================================
    ! Norms : any rank to rank-1, with DIM specifier and int input
    !====================================================================

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_2D_to_1D_int_z(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        
        call norm_2D_to_1D_int_z(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_2D_to_1D_int_z

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_2D_to_1D_err_int_z(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        
        call norm_2D_to_1D_int_z(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_2D_to_1D_err_int_z
    
    ! Internal implementation 
    pure module subroutine norm_2D_to_1D_int_z(a, nrm, order, dim, err)
        !> Input matrix a[..]
        complex(dp), intent(in) :: a(:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2
        logical :: contiguous_data
        real(dp) :: rorder
        integer(ilp), dimension(2) :: spe,spack,perm,iperm
        integer(ilp), dimension(2), parameter :: dim_range = [(lda,lda=1_ilp,2_ilp)]
        complex(dp), allocatable :: apack(:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>2) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_z(lda, apack(:, j2), &
                    nrm(j2), norm_request)
            enddo            
            
        else
            
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_z(lda, a(:, j2), &
                    nrm(j2), norm_request)
            enddo               
            
        endif        
        
    end subroutine norm_2D_to_1D_int_z

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_3D_to_2D_int_z(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        
        call norm_3D_to_2D_int_z(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_3D_to_2D_int_z

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_3D_to_2D_err_int_z(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        
        call norm_3D_to_2D_int_z(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_3D_to_2D_err_int_z
    
    ! Internal implementation 
    pure module subroutine norm_3D_to_2D_int_z(a, nrm, order, dim, err)
        !> Input matrix a[..]
        complex(dp), intent(in) :: a(:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3
        logical :: contiguous_data
        real(dp) :: rorder
        integer(ilp), dimension(3) :: spe,spack,perm,iperm
        integer(ilp), dimension(3), parameter :: dim_range = [(lda,lda=1_ilp,3_ilp)]
        complex(dp), allocatable :: apack(:,:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>3) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_z(lda, apack(:, j2, j3), &
                    nrm(j2, j3), norm_request)
            enddo; enddo            
            
        else
            
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_z(lda, a(:, j2, j3), &
                    nrm(j2, j3), norm_request)
            enddo; enddo               
            
        endif        
        
    end subroutine norm_3D_to_2D_int_z

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_4D_to_3D_int_z(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))     
        
        call norm_4D_to_3D_int_z(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_4D_to_3D_int_z

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_4D_to_3D_err_int_z(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))     
        
        call norm_4D_to_3D_int_z(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_4D_to_3D_err_int_z
    
    ! Internal implementation 
    pure module subroutine norm_4D_to_3D_int_z(a, nrm, order, dim, err)
        !> Input matrix a[..]
        complex(dp), intent(in) :: a(:,:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
            & merge(size(a, 3), size(a, 4), mask=3<dim))     
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3, j4
        logical :: contiguous_data
        real(dp) :: rorder
        integer(ilp), dimension(4) :: spe,spack,perm,iperm
        integer(ilp), dimension(4), parameter :: dim_range = [(lda,lda=1_ilp,4_ilp)]
        complex(dp), allocatable :: apack(:,:,:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>4) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j4 = lbound(apack, 4), ubound(apack, 4)
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_z(lda, apack(:, j2, j3, j4), &
                    nrm(j2, j3, j4), norm_request)
            enddo; enddo; enddo            
            
        else
            
            do j4 = lbound(a, 4), ubound(a, 4)
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_z(lda, a(:, j2, j3, j4), &
                    nrm(j2, j3, j4), norm_request)
            enddo; enddo; enddo               
            
        endif        
        
    end subroutine norm_4D_to_3D_int_z

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_5D_to_4D_int_z(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim))     
        
        call norm_5D_to_4D_int_z(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_5D_to_4D_int_z

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_5D_to_4D_err_int_z(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim))     
        
        call norm_5D_to_4D_int_z(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_5D_to_4D_err_int_z
    
    ! Internal implementation 
    pure module subroutine norm_5D_to_4D_int_z(a, nrm, order, dim, err)
        !> Input matrix a[..]
        complex(dp), intent(in) :: a(:,:,:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
            & merge(size(a, 3), size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim))     
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3, j4, j5
        logical :: contiguous_data
        real(dp) :: rorder
        integer(ilp), dimension(5) :: spe,spack,perm,iperm
        integer(ilp), dimension(5), parameter :: dim_range = [(lda,lda=1_ilp,5_ilp)]
        complex(dp), allocatable :: apack(:,:,:,:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>5) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j5 = lbound(apack, 5), ubound(apack, 5)
            do j4 = lbound(apack, 4), ubound(apack, 4)
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_z(lda, apack(:, j2, j3, j4, j5), &
                    nrm(j2, j3, j4, j5), norm_request)
            enddo; enddo; enddo; enddo            
            
        else
            
            do j5 = lbound(a, 5), ubound(a, 5)
            do j4 = lbound(a, 4), ubound(a, 4)
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_z(lda, a(:, j2, j3, j4, j5), &
                    nrm(j2, j3, j4, j5), norm_request)
            enddo; enddo; enddo; enddo               
            
        endif        
        
    end subroutine norm_5D_to_4D_int_z

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_6D_to_5D_int_z(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6), mask=5<dim))     
        
        call norm_6D_to_5D_int_z(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_6D_to_5D_int_z

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_6D_to_5D_err_int_z(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6), mask=5<dim))     
        
        call norm_6D_to_5D_int_z(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_6D_to_5D_err_int_z
    
    ! Internal implementation 
    pure module subroutine norm_6D_to_5D_int_z(a, nrm, order, dim, err)
        !> Input matrix a[..]
        complex(dp), intent(in) :: a(:,:,:,:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
            & merge(size(a, 3), size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6),&
            & mask=5<dim))     
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3, j4, j5, j6
        logical :: contiguous_data
        real(dp) :: rorder
        integer(ilp), dimension(6) :: spe,spack,perm,iperm
        integer(ilp), dimension(6), parameter :: dim_range = [(lda,lda=1_ilp,6_ilp)]
        complex(dp), allocatable :: apack(:,:,:,:,:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>6) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j6 = lbound(apack, 6), ubound(apack, 6)
            do j5 = lbound(apack, 5), ubound(apack, 5)
            do j4 = lbound(apack, 4), ubound(apack, 4)
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_z(lda, apack(:, j2, j3, j4, j5, j6), &
                    nrm(j2, j3, j4, j5, j6), norm_request)
            enddo; enddo; enddo; enddo; enddo            
            
        else
            
            do j6 = lbound(a, 6), ubound(a, 6)
            do j5 = lbound(a, 5), ubound(a, 5)
            do j4 = lbound(a, 4), ubound(a, 4)
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_z(lda, a(:, j2, j3, j4, j5, j6), &
                    nrm(j2, j3, j4, j5, j6), norm_request)
            enddo; enddo; enddo; enddo; enddo               
            
        endif        
        
    end subroutine norm_6D_to_5D_int_z

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_7D_to_6D_int_z(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6), mask=5<dim),&
            & merge(size(a, 6), size(a, 7), mask=6<dim))     
        
        call norm_7D_to_6D_int_z(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_7D_to_6D_int_z

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_7D_to_6D_err_int_z(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:,:,:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6), mask=5<dim),&
            & merge(size(a, 6), size(a, 7), mask=6<dim))     
        
        call norm_7D_to_6D_int_z(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_7D_to_6D_err_int_z
    
    ! Internal implementation 
    pure module subroutine norm_7D_to_6D_int_z(a, nrm, order, dim, err)
        !> Input matrix a[..]
        complex(dp), intent(in) :: a(:,:,:,:,:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
            & merge(size(a, 3), size(a, 4), mask=3<dim), merge(size(a, 4), size(a, 5), mask=4<dim), merge(size(a, 5), size(a, 6),&
            & mask=5<dim), merge(size(a, 6), size(a, 7), mask=6<dim))     
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3, j4, j5, j6, j7
        logical :: contiguous_data
        real(dp) :: rorder
        integer(ilp), dimension(7) :: spe,spack,perm,iperm
        integer(ilp), dimension(7), parameter :: dim_range = [(lda,lda=1_ilp,7_ilp)]
        complex(dp), allocatable :: apack(:,:,:,:,:,:,:)
        intrinsic :: abs, sum, sqrt, norm2, maxval, minval, conjg
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>7) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j7 = lbound(apack, 7), ubound(apack, 7)
            do j6 = lbound(apack, 6), ubound(apack, 6)
            do j5 = lbound(apack, 5), ubound(apack, 5)
            do j4 = lbound(apack, 4), ubound(apack, 4)
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_z(lda, apack(:, j2, j3, j4, j5, j6, j7), &
                    nrm(j2, j3, j4, j5, j6, j7), norm_request)
            enddo; enddo; enddo; enddo; enddo; enddo            
            
        else
            
            do j7 = lbound(a, 7), ubound(a, 7)
            do j6 = lbound(a, 6), ubound(a, 6)
            do j5 = lbound(a, 5), ubound(a, 5)
            do j4 = lbound(a, 4), ubound(a, 4)
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_z(lda, a(:, j2, j3, j4, j5, j6, j7), &
                    nrm(j2, j3, j4, j5, j6, j7), norm_request)
            enddo; enddo; enddo; enddo; enddo; enddo               
            
        endif        
        
    end subroutine norm_7D_to_6D_int_z


end submodule stdlib_linalg_norms
