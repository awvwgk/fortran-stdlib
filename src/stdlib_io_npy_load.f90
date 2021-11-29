! SPDX-Identifier: MIT


!> Implementation of loading npy files into multidimensional arrays
submodule (stdlib_io_npy) stdlib_io_npy_load
    use stdlib_error, only : error_stop
    use stdlib_strings, only : to_string, starts_with
    implicit none

contains

    !> Load a 1-dimensional array from a npy file
    module subroutine load_npy_rsp_1(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        real(sp), allocatable, intent(out) :: array(:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_rsp
        integer, parameter :: rank = 1

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                    & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                    & to_string(size(vshape))//", but expected "//&
                    & to_string(rank)
                exit catch
            end if

            call allocator(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                    & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//&
                    & msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    contains

    !> Wrapped intrinsic allocate to create an allocation from a shape array
    subroutine allocator(array, vshape, stat)
        !> Instance of the array to be allocated
        real(sp), allocatable, intent(out) :: array(:)
        !> Dimensions to allocate for
        integer, intent(in) :: vshape(:)
        !> Status of allocate
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_rsp_1
    !> Load a 2-dimensional array from a npy file
    module subroutine load_npy_rsp_2(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        real(sp), allocatable, intent(out) :: array(:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_rsp
        integer, parameter :: rank = 2

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                    & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                    & to_string(size(vshape))//", but expected "//&
                    & to_string(rank)
                exit catch
            end if

            call allocator(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                    & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//&
                    & msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    contains

    !> Wrapped intrinsic allocate to create an allocation from a shape array
    subroutine allocator(array, vshape, stat)
        !> Instance of the array to be allocated
        real(sp), allocatable, intent(out) :: array(:,:)
        !> Dimensions to allocate for
        integer, intent(in) :: vshape(:)
        !> Status of allocate
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_rsp_2
    !> Load a 3-dimensional array from a npy file
    module subroutine load_npy_rsp_3(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        real(sp), allocatable, intent(out) :: array(:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_rsp
        integer, parameter :: rank = 3

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                    & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                    & to_string(size(vshape))//", but expected "//&
                    & to_string(rank)
                exit catch
            end if

            call allocator(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                    & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//&
                    & msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    contains

    !> Wrapped intrinsic allocate to create an allocation from a shape array
    subroutine allocator(array, vshape, stat)
        !> Instance of the array to be allocated
        real(sp), allocatable, intent(out) :: array(:,:,:)
        !> Dimensions to allocate for
        integer, intent(in) :: vshape(:)
        !> Status of allocate
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2), &
            &    vshape(3)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_rsp_3
    !> Load a 4-dimensional array from a npy file
    module subroutine load_npy_rsp_4(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        real(sp), allocatable, intent(out) :: array(:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_rsp
        integer, parameter :: rank = 4

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                    & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                    & to_string(size(vshape))//", but expected "//&
                    & to_string(rank)
                exit catch
            end if

            call allocator(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                    & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//&
                    & msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    contains

    !> Wrapped intrinsic allocate to create an allocation from a shape array
    subroutine allocator(array, vshape, stat)
        !> Instance of the array to be allocated
        real(sp), allocatable, intent(out) :: array(:,:,:,:)
        !> Dimensions to allocate for
        integer, intent(in) :: vshape(:)
        !> Status of allocate
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2), &
            &    vshape(3), &
            &    vshape(4)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_rsp_4
    !> Load a 1-dimensional array from a npy file
    module subroutine load_npy_rdp_1(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        real(dp), allocatable, intent(out) :: array(:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_rdp
        integer, parameter :: rank = 1

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                    & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                    & to_string(size(vshape))//", but expected "//&
                    & to_string(rank)
                exit catch
            end if

            call allocator(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                    & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//&
                    & msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    contains

    !> Wrapped intrinsic allocate to create an allocation from a shape array
    subroutine allocator(array, vshape, stat)
        !> Instance of the array to be allocated
        real(dp), allocatable, intent(out) :: array(:)
        !> Dimensions to allocate for
        integer, intent(in) :: vshape(:)
        !> Status of allocate
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_rdp_1
    !> Load a 2-dimensional array from a npy file
    module subroutine load_npy_rdp_2(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        real(dp), allocatable, intent(out) :: array(:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_rdp
        integer, parameter :: rank = 2

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                    & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                    & to_string(size(vshape))//", but expected "//&
                    & to_string(rank)
                exit catch
            end if

            call allocator(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                    & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//&
                    & msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    contains

    !> Wrapped intrinsic allocate to create an allocation from a shape array
    subroutine allocator(array, vshape, stat)
        !> Instance of the array to be allocated
        real(dp), allocatable, intent(out) :: array(:,:)
        !> Dimensions to allocate for
        integer, intent(in) :: vshape(:)
        !> Status of allocate
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_rdp_2
    !> Load a 3-dimensional array from a npy file
    module subroutine load_npy_rdp_3(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        real(dp), allocatable, intent(out) :: array(:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_rdp
        integer, parameter :: rank = 3

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                    & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                    & to_string(size(vshape))//", but expected "//&
                    & to_string(rank)
                exit catch
            end if

            call allocator(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                    & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//&
                    & msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    contains

    !> Wrapped intrinsic allocate to create an allocation from a shape array
    subroutine allocator(array, vshape, stat)
        !> Instance of the array to be allocated
        real(dp), allocatable, intent(out) :: array(:,:,:)
        !> Dimensions to allocate for
        integer, intent(in) :: vshape(:)
        !> Status of allocate
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2), &
            &    vshape(3)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_rdp_3
    !> Load a 4-dimensional array from a npy file
    module subroutine load_npy_rdp_4(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        real(dp), allocatable, intent(out) :: array(:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_rdp
        integer, parameter :: rank = 4

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                    & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                    & to_string(size(vshape))//", but expected "//&
                    & to_string(rank)
                exit catch
            end if

            call allocator(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                    & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//&
                    & msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    contains

    !> Wrapped intrinsic allocate to create an allocation from a shape array
    subroutine allocator(array, vshape, stat)
        !> Instance of the array to be allocated
        real(dp), allocatable, intent(out) :: array(:,:,:,:)
        !> Dimensions to allocate for
        integer, intent(in) :: vshape(:)
        !> Status of allocate
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2), &
            &    vshape(3), &
            &    vshape(4)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_rdp_4
    !> Load a 1-dimensional array from a npy file
    module subroutine load_npy_iint8_1(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int8), allocatable, intent(out) :: array(:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint8
        integer, parameter :: rank = 1

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                    & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                    & to_string(size(vshape))//", but expected "//&
                    & to_string(rank)
                exit catch
            end if

            call allocator(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                    & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//&
                    & msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    contains

    !> Wrapped intrinsic allocate to create an allocation from a shape array
    subroutine allocator(array, vshape, stat)
        !> Instance of the array to be allocated
        integer(int8), allocatable, intent(out) :: array(:)
        !> Dimensions to allocate for
        integer, intent(in) :: vshape(:)
        !> Status of allocate
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_iint8_1
    !> Load a 2-dimensional array from a npy file
    module subroutine load_npy_iint8_2(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int8), allocatable, intent(out) :: array(:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint8
        integer, parameter :: rank = 2

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                    & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                    & to_string(size(vshape))//", but expected "//&
                    & to_string(rank)
                exit catch
            end if

            call allocator(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                    & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//&
                    & msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    contains

    !> Wrapped intrinsic allocate to create an allocation from a shape array
    subroutine allocator(array, vshape, stat)
        !> Instance of the array to be allocated
        integer(int8), allocatable, intent(out) :: array(:,:)
        !> Dimensions to allocate for
        integer, intent(in) :: vshape(:)
        !> Status of allocate
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_iint8_2
    !> Load a 3-dimensional array from a npy file
    module subroutine load_npy_iint8_3(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int8), allocatable, intent(out) :: array(:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint8
        integer, parameter :: rank = 3

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                    & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                    & to_string(size(vshape))//", but expected "//&
                    & to_string(rank)
                exit catch
            end if

            call allocator(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                    & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//&
                    & msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    contains

    !> Wrapped intrinsic allocate to create an allocation from a shape array
    subroutine allocator(array, vshape, stat)
        !> Instance of the array to be allocated
        integer(int8), allocatable, intent(out) :: array(:,:,:)
        !> Dimensions to allocate for
        integer, intent(in) :: vshape(:)
        !> Status of allocate
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2), &
            &    vshape(3)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_iint8_3
    !> Load a 4-dimensional array from a npy file
    module subroutine load_npy_iint8_4(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int8), allocatable, intent(out) :: array(:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint8
        integer, parameter :: rank = 4

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                    & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                    & to_string(size(vshape))//", but expected "//&
                    & to_string(rank)
                exit catch
            end if

            call allocator(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                    & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//&
                    & msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    contains

    !> Wrapped intrinsic allocate to create an allocation from a shape array
    subroutine allocator(array, vshape, stat)
        !> Instance of the array to be allocated
        integer(int8), allocatable, intent(out) :: array(:,:,:,:)
        !> Dimensions to allocate for
        integer, intent(in) :: vshape(:)
        !> Status of allocate
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2), &
            &    vshape(3), &
            &    vshape(4)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_iint8_4
    !> Load a 1-dimensional array from a npy file
    module subroutine load_npy_iint16_1(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int16), allocatable, intent(out) :: array(:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint16
        integer, parameter :: rank = 1

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                    & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                    & to_string(size(vshape))//", but expected "//&
                    & to_string(rank)
                exit catch
            end if

            call allocator(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                    & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//&
                    & msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    contains

    !> Wrapped intrinsic allocate to create an allocation from a shape array
    subroutine allocator(array, vshape, stat)
        !> Instance of the array to be allocated
        integer(int16), allocatable, intent(out) :: array(:)
        !> Dimensions to allocate for
        integer, intent(in) :: vshape(:)
        !> Status of allocate
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_iint16_1
    !> Load a 2-dimensional array from a npy file
    module subroutine load_npy_iint16_2(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int16), allocatable, intent(out) :: array(:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint16
        integer, parameter :: rank = 2

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                    & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                    & to_string(size(vshape))//", but expected "//&
                    & to_string(rank)
                exit catch
            end if

            call allocator(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                    & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//&
                    & msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    contains

    !> Wrapped intrinsic allocate to create an allocation from a shape array
    subroutine allocator(array, vshape, stat)
        !> Instance of the array to be allocated
        integer(int16), allocatable, intent(out) :: array(:,:)
        !> Dimensions to allocate for
        integer, intent(in) :: vshape(:)
        !> Status of allocate
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_iint16_2
    !> Load a 3-dimensional array from a npy file
    module subroutine load_npy_iint16_3(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int16), allocatable, intent(out) :: array(:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint16
        integer, parameter :: rank = 3

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                    & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                    & to_string(size(vshape))//", but expected "//&
                    & to_string(rank)
                exit catch
            end if

            call allocator(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                    & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//&
                    & msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    contains

    !> Wrapped intrinsic allocate to create an allocation from a shape array
    subroutine allocator(array, vshape, stat)
        !> Instance of the array to be allocated
        integer(int16), allocatable, intent(out) :: array(:,:,:)
        !> Dimensions to allocate for
        integer, intent(in) :: vshape(:)
        !> Status of allocate
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2), &
            &    vshape(3)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_iint16_3
    !> Load a 4-dimensional array from a npy file
    module subroutine load_npy_iint16_4(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int16), allocatable, intent(out) :: array(:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint16
        integer, parameter :: rank = 4

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                    & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                    & to_string(size(vshape))//", but expected "//&
                    & to_string(rank)
                exit catch
            end if

            call allocator(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                    & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//&
                    & msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    contains

    !> Wrapped intrinsic allocate to create an allocation from a shape array
    subroutine allocator(array, vshape, stat)
        !> Instance of the array to be allocated
        integer(int16), allocatable, intent(out) :: array(:,:,:,:)
        !> Dimensions to allocate for
        integer, intent(in) :: vshape(:)
        !> Status of allocate
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2), &
            &    vshape(3), &
            &    vshape(4)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_iint16_4
    !> Load a 1-dimensional array from a npy file
    module subroutine load_npy_iint32_1(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int32), allocatable, intent(out) :: array(:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint32
        integer, parameter :: rank = 1

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                    & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                    & to_string(size(vshape))//", but expected "//&
                    & to_string(rank)
                exit catch
            end if

            call allocator(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                    & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//&
                    & msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    contains

    !> Wrapped intrinsic allocate to create an allocation from a shape array
    subroutine allocator(array, vshape, stat)
        !> Instance of the array to be allocated
        integer(int32), allocatable, intent(out) :: array(:)
        !> Dimensions to allocate for
        integer, intent(in) :: vshape(:)
        !> Status of allocate
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_iint32_1
    !> Load a 2-dimensional array from a npy file
    module subroutine load_npy_iint32_2(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int32), allocatable, intent(out) :: array(:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint32
        integer, parameter :: rank = 2

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                    & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                    & to_string(size(vshape))//", but expected "//&
                    & to_string(rank)
                exit catch
            end if

            call allocator(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                    & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//&
                    & msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    contains

    !> Wrapped intrinsic allocate to create an allocation from a shape array
    subroutine allocator(array, vshape, stat)
        !> Instance of the array to be allocated
        integer(int32), allocatable, intent(out) :: array(:,:)
        !> Dimensions to allocate for
        integer, intent(in) :: vshape(:)
        !> Status of allocate
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_iint32_2
    !> Load a 3-dimensional array from a npy file
    module subroutine load_npy_iint32_3(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int32), allocatable, intent(out) :: array(:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint32
        integer, parameter :: rank = 3

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                    & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                    & to_string(size(vshape))//", but expected "//&
                    & to_string(rank)
                exit catch
            end if

            call allocator(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                    & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//&
                    & msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    contains

    !> Wrapped intrinsic allocate to create an allocation from a shape array
    subroutine allocator(array, vshape, stat)
        !> Instance of the array to be allocated
        integer(int32), allocatable, intent(out) :: array(:,:,:)
        !> Dimensions to allocate for
        integer, intent(in) :: vshape(:)
        !> Status of allocate
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2), &
            &    vshape(3)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_iint32_3
    !> Load a 4-dimensional array from a npy file
    module subroutine load_npy_iint32_4(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int32), allocatable, intent(out) :: array(:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint32
        integer, parameter :: rank = 4

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                    & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                    & to_string(size(vshape))//", but expected "//&
                    & to_string(rank)
                exit catch
            end if

            call allocator(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                    & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//&
                    & msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    contains

    !> Wrapped intrinsic allocate to create an allocation from a shape array
    subroutine allocator(array, vshape, stat)
        !> Instance of the array to be allocated
        integer(int32), allocatable, intent(out) :: array(:,:,:,:)
        !> Dimensions to allocate for
        integer, intent(in) :: vshape(:)
        !> Status of allocate
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2), &
            &    vshape(3), &
            &    vshape(4)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_iint32_4
    !> Load a 1-dimensional array from a npy file
    module subroutine load_npy_iint64_1(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int64), allocatable, intent(out) :: array(:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint64
        integer, parameter :: rank = 1

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                    & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                    & to_string(size(vshape))//", but expected "//&
                    & to_string(rank)
                exit catch
            end if

            call allocator(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                    & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//&
                    & msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    contains

    !> Wrapped intrinsic allocate to create an allocation from a shape array
    subroutine allocator(array, vshape, stat)
        !> Instance of the array to be allocated
        integer(int64), allocatable, intent(out) :: array(:)
        !> Dimensions to allocate for
        integer, intent(in) :: vshape(:)
        !> Status of allocate
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_iint64_1
    !> Load a 2-dimensional array from a npy file
    module subroutine load_npy_iint64_2(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int64), allocatable, intent(out) :: array(:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint64
        integer, parameter :: rank = 2

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                    & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                    & to_string(size(vshape))//", but expected "//&
                    & to_string(rank)
                exit catch
            end if

            call allocator(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                    & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//&
                    & msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    contains

    !> Wrapped intrinsic allocate to create an allocation from a shape array
    subroutine allocator(array, vshape, stat)
        !> Instance of the array to be allocated
        integer(int64), allocatable, intent(out) :: array(:,:)
        !> Dimensions to allocate for
        integer, intent(in) :: vshape(:)
        !> Status of allocate
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_iint64_2
    !> Load a 3-dimensional array from a npy file
    module subroutine load_npy_iint64_3(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int64), allocatable, intent(out) :: array(:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint64
        integer, parameter :: rank = 3

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                    & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                    & to_string(size(vshape))//", but expected "//&
                    & to_string(rank)
                exit catch
            end if

            call allocator(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                    & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//&
                    & msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    contains

    !> Wrapped intrinsic allocate to create an allocation from a shape array
    subroutine allocator(array, vshape, stat)
        !> Instance of the array to be allocated
        integer(int64), allocatable, intent(out) :: array(:,:,:)
        !> Dimensions to allocate for
        integer, intent(in) :: vshape(:)
        !> Status of allocate
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2), &
            &    vshape(3)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_iint64_3
    !> Load a 4-dimensional array from a npy file
    module subroutine load_npy_iint64_4(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int64), allocatable, intent(out) :: array(:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint64
        integer, parameter :: rank = 4

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                    & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                    & to_string(size(vshape))//", but expected "//&
                    & to_string(rank)
                exit catch
            end if

            call allocator(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                    & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//&
                    & msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    contains

    !> Wrapped intrinsic allocate to create an allocation from a shape array
    subroutine allocator(array, vshape, stat)
        !> Instance of the array to be allocated
        integer(int64), allocatable, intent(out) :: array(:,:,:,:)
        !> Dimensions to allocate for
        integer, intent(in) :: vshape(:)
        !> Status of allocate
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2), &
            &    vshape(3), &
            &    vshape(4)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_iint64_4
    !> Load a 1-dimensional array from a npy file
    module subroutine load_npy_csp_1(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        complex(sp), allocatable, intent(out) :: array(:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_csp
        integer, parameter :: rank = 1

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                    & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                    & to_string(size(vshape))//", but expected "//&
                    & to_string(rank)
                exit catch
            end if

            call allocator(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                    & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//&
                    & msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    contains

    !> Wrapped intrinsic allocate to create an allocation from a shape array
    subroutine allocator(array, vshape, stat)
        !> Instance of the array to be allocated
        complex(sp), allocatable, intent(out) :: array(:)
        !> Dimensions to allocate for
        integer, intent(in) :: vshape(:)
        !> Status of allocate
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_csp_1
    !> Load a 2-dimensional array from a npy file
    module subroutine load_npy_csp_2(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        complex(sp), allocatable, intent(out) :: array(:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_csp
        integer, parameter :: rank = 2

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                    & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                    & to_string(size(vshape))//", but expected "//&
                    & to_string(rank)
                exit catch
            end if

            call allocator(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                    & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//&
                    & msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    contains

    !> Wrapped intrinsic allocate to create an allocation from a shape array
    subroutine allocator(array, vshape, stat)
        !> Instance of the array to be allocated
        complex(sp), allocatable, intent(out) :: array(:,:)
        !> Dimensions to allocate for
        integer, intent(in) :: vshape(:)
        !> Status of allocate
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_csp_2
    !> Load a 3-dimensional array from a npy file
    module subroutine load_npy_csp_3(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        complex(sp), allocatable, intent(out) :: array(:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_csp
        integer, parameter :: rank = 3

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                    & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                    & to_string(size(vshape))//", but expected "//&
                    & to_string(rank)
                exit catch
            end if

            call allocator(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                    & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//&
                    & msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    contains

    !> Wrapped intrinsic allocate to create an allocation from a shape array
    subroutine allocator(array, vshape, stat)
        !> Instance of the array to be allocated
        complex(sp), allocatable, intent(out) :: array(:,:,:)
        !> Dimensions to allocate for
        integer, intent(in) :: vshape(:)
        !> Status of allocate
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2), &
            &    vshape(3)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_csp_3
    !> Load a 4-dimensional array from a npy file
    module subroutine load_npy_csp_4(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        complex(sp), allocatable, intent(out) :: array(:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_csp
        integer, parameter :: rank = 4

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                    & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                    & to_string(size(vshape))//", but expected "//&
                    & to_string(rank)
                exit catch
            end if

            call allocator(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                    & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//&
                    & msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    contains

    !> Wrapped intrinsic allocate to create an allocation from a shape array
    subroutine allocator(array, vshape, stat)
        !> Instance of the array to be allocated
        complex(sp), allocatable, intent(out) :: array(:,:,:,:)
        !> Dimensions to allocate for
        integer, intent(in) :: vshape(:)
        !> Status of allocate
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2), &
            &    vshape(3), &
            &    vshape(4)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_csp_4
    !> Load a 1-dimensional array from a npy file
    module subroutine load_npy_cdp_1(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        complex(dp), allocatable, intent(out) :: array(:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_cdp
        integer, parameter :: rank = 1

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                    & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                    & to_string(size(vshape))//", but expected "//&
                    & to_string(rank)
                exit catch
            end if

            call allocator(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                    & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//&
                    & msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    contains

    !> Wrapped intrinsic allocate to create an allocation from a shape array
    subroutine allocator(array, vshape, stat)
        !> Instance of the array to be allocated
        complex(dp), allocatable, intent(out) :: array(:)
        !> Dimensions to allocate for
        integer, intent(in) :: vshape(:)
        !> Status of allocate
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_cdp_1
    !> Load a 2-dimensional array from a npy file
    module subroutine load_npy_cdp_2(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        complex(dp), allocatable, intent(out) :: array(:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_cdp
        integer, parameter :: rank = 2

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                    & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                    & to_string(size(vshape))//", but expected "//&
                    & to_string(rank)
                exit catch
            end if

            call allocator(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                    & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//&
                    & msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    contains

    !> Wrapped intrinsic allocate to create an allocation from a shape array
    subroutine allocator(array, vshape, stat)
        !> Instance of the array to be allocated
        complex(dp), allocatable, intent(out) :: array(:,:)
        !> Dimensions to allocate for
        integer, intent(in) :: vshape(:)
        !> Status of allocate
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_cdp_2
    !> Load a 3-dimensional array from a npy file
    module subroutine load_npy_cdp_3(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        complex(dp), allocatable, intent(out) :: array(:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_cdp
        integer, parameter :: rank = 3

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                    & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                    & to_string(size(vshape))//", but expected "//&
                    & to_string(rank)
                exit catch
            end if

            call allocator(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                    & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//&
                    & msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    contains

    !> Wrapped intrinsic allocate to create an allocation from a shape array
    subroutine allocator(array, vshape, stat)
        !> Instance of the array to be allocated
        complex(dp), allocatable, intent(out) :: array(:,:,:)
        !> Dimensions to allocate for
        integer, intent(in) :: vshape(:)
        !> Status of allocate
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2), &
            &    vshape(3)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_cdp_3
    !> Load a 4-dimensional array from a npy file
    module subroutine load_npy_cdp_4(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        complex(dp), allocatable, intent(out) :: array(:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_cdp
        integer, parameter :: rank = 4

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                    & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                    & to_string(size(vshape))//", but expected "//&
                    & to_string(rank)
                exit catch
            end if

            call allocator(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                    & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//&
                    & msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    contains

    !> Wrapped intrinsic allocate to create an allocation from a shape array
    subroutine allocator(array, vshape, stat)
        !> Instance of the array to be allocated
        complex(dp), allocatable, intent(out) :: array(:,:,:,:)
        !> Dimensions to allocate for
        integer, intent(in) :: vshape(:)
        !> Status of allocate
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2), &
            &    vshape(3), &
            &    vshape(4)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_cdp_4


    !> Read the npy header from a binary file and retrieve the descriptor string.
    subroutine get_descriptor(io, vtype, vshape, stat, msg)
        !> Unformatted, stream accessed unit
        integer, intent(in) :: io
        !> Type of data saved in npy file
        character(len=:), allocatable, intent(out) :: vtype
        !> Shape descriptor of the
        integer, allocatable, intent(out) :: vshape(:)
        !> Status of operation
        integer, intent(out) :: stat
        !> Associated error message in case of non-zero status
        character(len=:), allocatable, intent(out) :: msg

        integer :: major, header_len, i
        character(len=:), allocatable :: dict
        character(len=8) :: header
        character :: buf(4)
        logical :: fortran_order

        read(io, iostat=stat) header
        if (stat /= 0) return

        call parse_header(header, major, stat, msg)
        if (stat /= 0) return

        read(io, iostat=stat) buf(1:merge(4, 2, major > 1))
        if (stat /= 0) return

        if (major > 1) then
            header_len = ichar(buf(1)) &
                &      + ichar(buf(2)) * 2**8 &
                &      + ichar(buf(3)) * 2**16 &
                &      + ichar(buf(4)) * 2**32
        else
            header_len = ichar(buf(1)) &
                &      + ichar(buf(2)) * 2**8
        end if
        allocate(character(header_len) :: dict, stat=stat)
        if (stat /= 0) return

        read(io, iostat=stat) dict
        if (stat /= 0) return

        if (dict(header_len:header_len) /= nl) then
            stat = 1
            msg = "Descriptor length does not match"
            return
        end if

        if (scan(dict, achar(0)) > 0) then
            stat = 1
            msg = "Nul byte not allowed in descriptor string"
            return
        end if

        call parse_descriptor(trim(dict(:len(dict)-1)), vtype, fortran_order, vshape, &
            & stat, msg)
        if (stat /= 0) return

        if (.not.fortran_order) then
            vshape = [(vshape(i), i = size(vshape), 1, -1)]
        end if
    end subroutine get_descriptor


    !> Parse the first eight bytes of the npy header to verify the data
    subroutine parse_header(header, major, stat, msg)
        !> Header of the binary file
        character(len=*), intent(in) :: header
        !> Major version of the npy format
        integer, intent(out) :: major
        !> Status of operation
        integer, intent(out) :: stat
        !> Associated error message in case of non-zero status
        character(len=:), allocatable, intent(out) :: msg

        integer :: minor

        if (header(1:1) /= magic_number) then
            stat = 1
            msg = "Expected z'93' but got z'"//to_string(ichar(header(1:1)))//"' "//&
                & "as first byte"
            return
        end if

        if (header(2:6) /= magic_string) then
            stat = 1
            msg = "Expected identifier '"//magic_string//"'"
            return
        end if

        major = ichar(header(7:7))
        if (.not.any(major == [1, 2, 3])) then
            stat = 1
            msg = "Unsupported format major version number '"//to_string(major)//"'"
            return
        end if

        minor = ichar(header(8:8))
        if (minor /= 0) then
            stat = 1
            msg = "Unsupported format version "// &
                & "'"//to_string(major)//"."//to_string(minor)//"'"
            return
        end if
    end subroutine parse_header

    !> Parse the descriptor in the npy header. This routine implements a minimal
    !> non-recursive parser for serialized Python dictionaries.
    subroutine parse_descriptor(input, vtype, fortran_order, vshape, stat, msg)
        !> Input string to parse as descriptor
        character(len=*), intent(in) :: input
        !> Type of the data stored, retrieved from field `descr`
        character(len=:), allocatable, intent(out) :: vtype
        !> Whether the data is in left layout, retrieved from field `fortran_order`
        logical, intent(out) :: fortran_order
        !> Shape of the stored data, retrieved from field `shape`
        integer, allocatable, intent(out) :: vshape(:)
        !> Status of operation
        integer, intent(out) :: stat
        !> Associated error message in case of non-zero status
        character(len=:), allocatable, intent(out) :: msg

        enum, bind(c)
            enumerator :: invalid, string, lbrace, rbrace, comma, colon, &
                lparen, rparen, bool, literal, space
        end enum

        type :: token_type
            integer :: first, last, kind
        end type token_type

        integer :: pos, last
        character(len=:), allocatable :: key
        type(token_type) :: token
        logical :: has_descr, has_shape, has_fortran_order

        has_descr = .false.
        has_shape = .false.
        has_fortran_order = .false.
        pos = 0
        call next_token(input, pos, token, [lbrace], stat)
        if (stat /= 0) return

        last = comma
        do while (pos < len(input))
            call get_token(input, pos, token)
            select case(token%kind)
            case(space)
                continue
            case(comma)
                if (token%kind == last) then
                    stat = pos
                    return
                end if
                last = comma
            case(rbrace)
                exit
            case(string)
                key = input(token%first:token%last)
                call next_token(input, pos, token, [colon], stat)
                if (stat /= 0) return

                select case(key)
                case("descr")
                    if (has_descr) then
                        stat = 1
                        msg = "Duplicate descriptor"
                    end if
                    call next_token(input, pos, token, [string], stat)
                    if (stat /= 0) return

                    vtype = input(token%first:token%last)
                    has_descr = .true.

                case("fortran_order")
                    if (has_fortran_order) then
                        stat = 1
                        msg = "Duplicate fortran_order"
                    end if
                    call next_token(input, pos, token, [bool], stat)
                    if (stat /= 0) return

                    fortran_order = input(token%first:token%last) == "True"
                    has_fortran_order = .true.

                case("shape")
                    if (has_shape) then
                        stat = 1
                        msg = "Duplicate shape"
                    end if
                    call parse_tuple(input, pos, vshape, stat)

                    has_shape = .true.

                case default
                    stat = pos
                    return
                end select
                last = string
            case default
                stat = pos
                return
            end select
        end do

        if (.not.has_descr) then
            stat = 1
            msg = "Missing descriptor"
        end if

        if (.not.has_shape) then
            stat = 1
            msg = "Missing shape"
        end if

        if (.not.has_fortran_order) then
            stat = 1
            msg = "Missing fortran_order"
        end if

    contains

    !> Parse a tuple of integers into an array of integers
    subroutine parse_tuple(input, pos, tuple, stat)
        !> Input string to parse
        character(len=*), intent(in) :: input
        !> Offset in the input, will be advanced after reading
        integer, intent(inout) :: pos
        !> Array representing tuple of integers
        integer, allocatable, intent(out) :: tuple(:)
        !> Status of operation
        integer, intent(out) :: stat

        type(token_type) :: token
        integer :: last, itmp

        allocate(tuple(0), stat=stat)
        if (stat /= 0) return

        call next_token(input, pos, token, [lparen], stat)
        if (stat /= 0) return

        last = comma
        do while (pos < len(input))
            call get_token(input, pos, token)
            select case(token%kind)
            case(space)
                continue
            case(literal)
                if (token%kind == last) then
                    stat = pos
                    return
                end if
                last = token%kind
                read(input(token%first:token%last), *, iostat=stat) itmp
                if (stat /= 0) then
                    return
                end if
                tuple = [tuple, itmp]
            case(comma)
                if (token%kind == last) then
                    stat = pos
                    return
                end if
                last = token%kind
            case(rparen)
                exit
            case default
                stat = pos
                return
            end select
        end do
    end subroutine parse_tuple

    !> Get the next allowed token
    subroutine next_token(input, pos, token, allowed_token, stat)
        !> Input string to parse
        character(len=*), intent(in) :: input
        !> Current offset in the input string
        integer, intent(inout) :: pos
        !> Last token parsed
        type(token_type), intent(out) :: token
        !> Tokens allowed in the current context
        integer, intent(in) :: allowed_token(:)
        !> Status of operation
        integer, intent(out) :: stat

        stat = pos
        do while (pos < len(input))
            call get_token(input, pos, token)
            if (token%kind == space) then
                continue
            else if (any(token%kind == allowed_token)) then
                stat = 0
                exit
            else
                stat = pos
                exit
            end if
        end do
    end subroutine next_token

    !> Tokenize input string
    subroutine get_token(input, pos, token)
        !> Input strin to tokenize
        character(len=*), intent(in) :: input
        !> Offset in input string, will be advanced
        integer, intent(inout) :: pos
        !> Returned token from the next position
        type(token_type), intent(out) :: token

        character :: quote

        pos = pos + 1
        select case(input(pos:pos))
        case("""", "'")
            quote = input(pos:pos)
            pos = pos + 1
            token%first = pos
            do while (pos <= len(input))
                if (input(pos:pos) == quote) then
                    token%last = pos - 1
                    exit
                else
                    pos = pos + 1
                end if
            end do
            token%kind = string
        case("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
            token%first = pos
            do while (pos <= len(input))
                if (.not.any(input(pos:pos) == ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"])) then
                    pos = pos - 1
                    token%last = pos
                    exit
                else
                    pos = pos + 1
                end if
            end do
            token%kind = literal
        case("T")
            if (starts_with(input(pos:), "True")) then
                token = token_type(pos, pos+3, bool)
                pos = pos + 3
            else
                token = token_type(pos, pos, invalid)
            end if
        case("F")
            if (starts_with(input(pos:), "False")) then
                token = token_type(pos, pos+4, bool)
                pos = pos + 4
            else
                token = token_type(pos, pos, invalid)
            end if
        case("{")
            token = token_type(pos, pos, lbrace)
        case("}")
            token = token_type(pos, pos, rbrace)
        case(",")
            token = token_type(pos, pos, comma)
        case(":")
            token = token_type(pos, pos, colon)
        case("(")
            token = token_type(pos, pos, lparen)
        case(")")
            token = token_type(pos, pos, rparen)
        case(" ", nl)
            token = token_type(pos, pos, space)
        case default
            token = token_type(pos, pos, invalid)
        end select

    end subroutine get_token

    end subroutine parse_descriptor

end submodule stdlib_io_npy_load
