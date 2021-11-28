! SPDX-Identifier: MIT


submodule (stdlib_io_npy) stdlib_io_npy_load
    implicit none

contains

    module subroutine load_npy_rsp_1(filename, array, iostat, iomsg)
        character(len=*), intent(in) :: filename
        real(sp), allocatable, intent(out) :: array(:)
        integer, intent(out), optional :: iostat
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_rsp
        integer, parameter :: rank = 1

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            integer :: i
            character(len=:), allocatable :: this_type
            logical :: fortran_order
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, fortran_order, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (.not.fortran_order) then
                vshape = [(vshape(i), i = size(vshape), 1, -1)]
            end if

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

    subroutine allocator(array, vshape, stat)
        real(sp), allocatable, intent(out) :: array(:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_rsp_1
    module subroutine load_npy_rsp_2(filename, array, iostat, iomsg)
        character(len=*), intent(in) :: filename
        real(sp), allocatable, intent(out) :: array(:,:)
        integer, intent(out), optional :: iostat
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_rsp
        integer, parameter :: rank = 2

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            integer :: i
            character(len=:), allocatable :: this_type
            logical :: fortran_order
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, fortran_order, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (.not.fortran_order) then
                vshape = [(vshape(i), i = size(vshape), 1, -1)]
            end if

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

    subroutine allocator(array, vshape, stat)
        real(sp), allocatable, intent(out) :: array(:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_rsp_2
    module subroutine load_npy_rsp_3(filename, array, iostat, iomsg)
        character(len=*), intent(in) :: filename
        real(sp), allocatable, intent(out) :: array(:,:,:)
        integer, intent(out), optional :: iostat
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_rsp
        integer, parameter :: rank = 3

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            integer :: i
            character(len=:), allocatable :: this_type
            logical :: fortran_order
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, fortran_order, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (.not.fortran_order) then
                vshape = [(vshape(i), i = size(vshape), 1, -1)]
            end if

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

    subroutine allocator(array, vshape, stat)
        real(sp), allocatable, intent(out) :: array(:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2), &
            &    vshape(3)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_rsp_3
    module subroutine load_npy_rsp_4(filename, array, iostat, iomsg)
        character(len=*), intent(in) :: filename
        real(sp), allocatable, intent(out) :: array(:,:,:,:)
        integer, intent(out), optional :: iostat
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_rsp
        integer, parameter :: rank = 4

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            integer :: i
            character(len=:), allocatable :: this_type
            logical :: fortran_order
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, fortran_order, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (.not.fortran_order) then
                vshape = [(vshape(i), i = size(vshape), 1, -1)]
            end if

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

    subroutine allocator(array, vshape, stat)
        real(sp), allocatable, intent(out) :: array(:,:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2), &
            &    vshape(3), &
            &    vshape(4)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_rsp_4
    module subroutine load_npy_rdp_1(filename, array, iostat, iomsg)
        character(len=*), intent(in) :: filename
        real(dp), allocatable, intent(out) :: array(:)
        integer, intent(out), optional :: iostat
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_rdp
        integer, parameter :: rank = 1

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            integer :: i
            character(len=:), allocatable :: this_type
            logical :: fortran_order
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, fortran_order, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (.not.fortran_order) then
                vshape = [(vshape(i), i = size(vshape), 1, -1)]
            end if

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

    subroutine allocator(array, vshape, stat)
        real(dp), allocatable, intent(out) :: array(:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_rdp_1
    module subroutine load_npy_rdp_2(filename, array, iostat, iomsg)
        character(len=*), intent(in) :: filename
        real(dp), allocatable, intent(out) :: array(:,:)
        integer, intent(out), optional :: iostat
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_rdp
        integer, parameter :: rank = 2

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            integer :: i
            character(len=:), allocatable :: this_type
            logical :: fortran_order
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, fortran_order, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (.not.fortran_order) then
                vshape = [(vshape(i), i = size(vshape), 1, -1)]
            end if

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

    subroutine allocator(array, vshape, stat)
        real(dp), allocatable, intent(out) :: array(:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_rdp_2
    module subroutine load_npy_rdp_3(filename, array, iostat, iomsg)
        character(len=*), intent(in) :: filename
        real(dp), allocatable, intent(out) :: array(:,:,:)
        integer, intent(out), optional :: iostat
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_rdp
        integer, parameter :: rank = 3

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            integer :: i
            character(len=:), allocatable :: this_type
            logical :: fortran_order
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, fortran_order, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (.not.fortran_order) then
                vshape = [(vshape(i), i = size(vshape), 1, -1)]
            end if

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

    subroutine allocator(array, vshape, stat)
        real(dp), allocatable, intent(out) :: array(:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2), &
            &    vshape(3)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_rdp_3
    module subroutine load_npy_rdp_4(filename, array, iostat, iomsg)
        character(len=*), intent(in) :: filename
        real(dp), allocatable, intent(out) :: array(:,:,:,:)
        integer, intent(out), optional :: iostat
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_rdp
        integer, parameter :: rank = 4

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            integer :: i
            character(len=:), allocatable :: this_type
            logical :: fortran_order
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, fortran_order, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (.not.fortran_order) then
                vshape = [(vshape(i), i = size(vshape), 1, -1)]
            end if

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

    subroutine allocator(array, vshape, stat)
        real(dp), allocatable, intent(out) :: array(:,:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2), &
            &    vshape(3), &
            &    vshape(4)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_rdp_4
    module subroutine load_npy_iint8_1(filename, array, iostat, iomsg)
        character(len=*), intent(in) :: filename
        integer(int8), allocatable, intent(out) :: array(:)
        integer, intent(out), optional :: iostat
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint8
        integer, parameter :: rank = 1

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            integer :: i
            character(len=:), allocatable :: this_type
            logical :: fortran_order
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, fortran_order, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (.not.fortran_order) then
                vshape = [(vshape(i), i = size(vshape), 1, -1)]
            end if

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

    subroutine allocator(array, vshape, stat)
        integer(int8), allocatable, intent(out) :: array(:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_iint8_1
    module subroutine load_npy_iint8_2(filename, array, iostat, iomsg)
        character(len=*), intent(in) :: filename
        integer(int8), allocatable, intent(out) :: array(:,:)
        integer, intent(out), optional :: iostat
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint8
        integer, parameter :: rank = 2

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            integer :: i
            character(len=:), allocatable :: this_type
            logical :: fortran_order
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, fortran_order, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (.not.fortran_order) then
                vshape = [(vshape(i), i = size(vshape), 1, -1)]
            end if

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

    subroutine allocator(array, vshape, stat)
        integer(int8), allocatable, intent(out) :: array(:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_iint8_2
    module subroutine load_npy_iint8_3(filename, array, iostat, iomsg)
        character(len=*), intent(in) :: filename
        integer(int8), allocatable, intent(out) :: array(:,:,:)
        integer, intent(out), optional :: iostat
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint8
        integer, parameter :: rank = 3

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            integer :: i
            character(len=:), allocatable :: this_type
            logical :: fortran_order
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, fortran_order, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (.not.fortran_order) then
                vshape = [(vshape(i), i = size(vshape), 1, -1)]
            end if

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

    subroutine allocator(array, vshape, stat)
        integer(int8), allocatable, intent(out) :: array(:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2), &
            &    vshape(3)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_iint8_3
    module subroutine load_npy_iint8_4(filename, array, iostat, iomsg)
        character(len=*), intent(in) :: filename
        integer(int8), allocatable, intent(out) :: array(:,:,:,:)
        integer, intent(out), optional :: iostat
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint8
        integer, parameter :: rank = 4

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            integer :: i
            character(len=:), allocatable :: this_type
            logical :: fortran_order
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, fortran_order, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (.not.fortran_order) then
                vshape = [(vshape(i), i = size(vshape), 1, -1)]
            end if

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

    subroutine allocator(array, vshape, stat)
        integer(int8), allocatable, intent(out) :: array(:,:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2), &
            &    vshape(3), &
            &    vshape(4)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_iint8_4
    module subroutine load_npy_iint16_1(filename, array, iostat, iomsg)
        character(len=*), intent(in) :: filename
        integer(int16), allocatable, intent(out) :: array(:)
        integer, intent(out), optional :: iostat
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint16
        integer, parameter :: rank = 1

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            integer :: i
            character(len=:), allocatable :: this_type
            logical :: fortran_order
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, fortran_order, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (.not.fortran_order) then
                vshape = [(vshape(i), i = size(vshape), 1, -1)]
            end if

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

    subroutine allocator(array, vshape, stat)
        integer(int16), allocatable, intent(out) :: array(:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_iint16_1
    module subroutine load_npy_iint16_2(filename, array, iostat, iomsg)
        character(len=*), intent(in) :: filename
        integer(int16), allocatable, intent(out) :: array(:,:)
        integer, intent(out), optional :: iostat
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint16
        integer, parameter :: rank = 2

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            integer :: i
            character(len=:), allocatable :: this_type
            logical :: fortran_order
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, fortran_order, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (.not.fortran_order) then
                vshape = [(vshape(i), i = size(vshape), 1, -1)]
            end if

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

    subroutine allocator(array, vshape, stat)
        integer(int16), allocatable, intent(out) :: array(:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_iint16_2
    module subroutine load_npy_iint16_3(filename, array, iostat, iomsg)
        character(len=*), intent(in) :: filename
        integer(int16), allocatable, intent(out) :: array(:,:,:)
        integer, intent(out), optional :: iostat
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint16
        integer, parameter :: rank = 3

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            integer :: i
            character(len=:), allocatable :: this_type
            logical :: fortran_order
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, fortran_order, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (.not.fortran_order) then
                vshape = [(vshape(i), i = size(vshape), 1, -1)]
            end if

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

    subroutine allocator(array, vshape, stat)
        integer(int16), allocatable, intent(out) :: array(:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2), &
            &    vshape(3)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_iint16_3
    module subroutine load_npy_iint16_4(filename, array, iostat, iomsg)
        character(len=*), intent(in) :: filename
        integer(int16), allocatable, intent(out) :: array(:,:,:,:)
        integer, intent(out), optional :: iostat
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint16
        integer, parameter :: rank = 4

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            integer :: i
            character(len=:), allocatable :: this_type
            logical :: fortran_order
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, fortran_order, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (.not.fortran_order) then
                vshape = [(vshape(i), i = size(vshape), 1, -1)]
            end if

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

    subroutine allocator(array, vshape, stat)
        integer(int16), allocatable, intent(out) :: array(:,:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2), &
            &    vshape(3), &
            &    vshape(4)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_iint16_4
    module subroutine load_npy_iint32_1(filename, array, iostat, iomsg)
        character(len=*), intent(in) :: filename
        integer(int32), allocatable, intent(out) :: array(:)
        integer, intent(out), optional :: iostat
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint32
        integer, parameter :: rank = 1

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            integer :: i
            character(len=:), allocatable :: this_type
            logical :: fortran_order
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, fortran_order, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (.not.fortran_order) then
                vshape = [(vshape(i), i = size(vshape), 1, -1)]
            end if

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

    subroutine allocator(array, vshape, stat)
        integer(int32), allocatable, intent(out) :: array(:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_iint32_1
    module subroutine load_npy_iint32_2(filename, array, iostat, iomsg)
        character(len=*), intent(in) :: filename
        integer(int32), allocatable, intent(out) :: array(:,:)
        integer, intent(out), optional :: iostat
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint32
        integer, parameter :: rank = 2

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            integer :: i
            character(len=:), allocatable :: this_type
            logical :: fortran_order
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, fortran_order, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (.not.fortran_order) then
                vshape = [(vshape(i), i = size(vshape), 1, -1)]
            end if

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

    subroutine allocator(array, vshape, stat)
        integer(int32), allocatable, intent(out) :: array(:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_iint32_2
    module subroutine load_npy_iint32_3(filename, array, iostat, iomsg)
        character(len=*), intent(in) :: filename
        integer(int32), allocatable, intent(out) :: array(:,:,:)
        integer, intent(out), optional :: iostat
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint32
        integer, parameter :: rank = 3

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            integer :: i
            character(len=:), allocatable :: this_type
            logical :: fortran_order
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, fortran_order, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (.not.fortran_order) then
                vshape = [(vshape(i), i = size(vshape), 1, -1)]
            end if

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

    subroutine allocator(array, vshape, stat)
        integer(int32), allocatable, intent(out) :: array(:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2), &
            &    vshape(3)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_iint32_3
    module subroutine load_npy_iint32_4(filename, array, iostat, iomsg)
        character(len=*), intent(in) :: filename
        integer(int32), allocatable, intent(out) :: array(:,:,:,:)
        integer, intent(out), optional :: iostat
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint32
        integer, parameter :: rank = 4

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            integer :: i
            character(len=:), allocatable :: this_type
            logical :: fortran_order
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, fortran_order, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (.not.fortran_order) then
                vshape = [(vshape(i), i = size(vshape), 1, -1)]
            end if

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

    subroutine allocator(array, vshape, stat)
        integer(int32), allocatable, intent(out) :: array(:,:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2), &
            &    vshape(3), &
            &    vshape(4)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_iint32_4
    module subroutine load_npy_iint64_1(filename, array, iostat, iomsg)
        character(len=*), intent(in) :: filename
        integer(int64), allocatable, intent(out) :: array(:)
        integer, intent(out), optional :: iostat
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint64
        integer, parameter :: rank = 1

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            integer :: i
            character(len=:), allocatable :: this_type
            logical :: fortran_order
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, fortran_order, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (.not.fortran_order) then
                vshape = [(vshape(i), i = size(vshape), 1, -1)]
            end if

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

    subroutine allocator(array, vshape, stat)
        integer(int64), allocatable, intent(out) :: array(:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_iint64_1
    module subroutine load_npy_iint64_2(filename, array, iostat, iomsg)
        character(len=*), intent(in) :: filename
        integer(int64), allocatable, intent(out) :: array(:,:)
        integer, intent(out), optional :: iostat
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint64
        integer, parameter :: rank = 2

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            integer :: i
            character(len=:), allocatable :: this_type
            logical :: fortran_order
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, fortran_order, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (.not.fortran_order) then
                vshape = [(vshape(i), i = size(vshape), 1, -1)]
            end if

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

    subroutine allocator(array, vshape, stat)
        integer(int64), allocatable, intent(out) :: array(:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_iint64_2
    module subroutine load_npy_iint64_3(filename, array, iostat, iomsg)
        character(len=*), intent(in) :: filename
        integer(int64), allocatable, intent(out) :: array(:,:,:)
        integer, intent(out), optional :: iostat
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint64
        integer, parameter :: rank = 3

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            integer :: i
            character(len=:), allocatable :: this_type
            logical :: fortran_order
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, fortran_order, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (.not.fortran_order) then
                vshape = [(vshape(i), i = size(vshape), 1, -1)]
            end if

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

    subroutine allocator(array, vshape, stat)
        integer(int64), allocatable, intent(out) :: array(:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2), &
            &    vshape(3)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_iint64_3
    module subroutine load_npy_iint64_4(filename, array, iostat, iomsg)
        character(len=*), intent(in) :: filename
        integer(int64), allocatable, intent(out) :: array(:,:,:,:)
        integer, intent(out), optional :: iostat
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint64
        integer, parameter :: rank = 4

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            integer :: i
            character(len=:), allocatable :: this_type
            logical :: fortran_order
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, fortran_order, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (.not.fortran_order) then
                vshape = [(vshape(i), i = size(vshape), 1, -1)]
            end if

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

    subroutine allocator(array, vshape, stat)
        integer(int64), allocatable, intent(out) :: array(:,:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2), &
            &    vshape(3), &
            &    vshape(4)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_iint64_4
    module subroutine load_npy_csp_1(filename, array, iostat, iomsg)
        character(len=*), intent(in) :: filename
        complex(sp), allocatable, intent(out) :: array(:)
        integer, intent(out), optional :: iostat
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_csp
        integer, parameter :: rank = 1

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            integer :: i
            character(len=:), allocatable :: this_type
            logical :: fortran_order
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, fortran_order, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (.not.fortran_order) then
                vshape = [(vshape(i), i = size(vshape), 1, -1)]
            end if

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

    subroutine allocator(array, vshape, stat)
        complex(sp), allocatable, intent(out) :: array(:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_csp_1
    module subroutine load_npy_csp_2(filename, array, iostat, iomsg)
        character(len=*), intent(in) :: filename
        complex(sp), allocatable, intent(out) :: array(:,:)
        integer, intent(out), optional :: iostat
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_csp
        integer, parameter :: rank = 2

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            integer :: i
            character(len=:), allocatable :: this_type
            logical :: fortran_order
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, fortran_order, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (.not.fortran_order) then
                vshape = [(vshape(i), i = size(vshape), 1, -1)]
            end if

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

    subroutine allocator(array, vshape, stat)
        complex(sp), allocatable, intent(out) :: array(:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_csp_2
    module subroutine load_npy_csp_3(filename, array, iostat, iomsg)
        character(len=*), intent(in) :: filename
        complex(sp), allocatable, intent(out) :: array(:,:,:)
        integer, intent(out), optional :: iostat
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_csp
        integer, parameter :: rank = 3

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            integer :: i
            character(len=:), allocatable :: this_type
            logical :: fortran_order
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, fortran_order, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (.not.fortran_order) then
                vshape = [(vshape(i), i = size(vshape), 1, -1)]
            end if

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

    subroutine allocator(array, vshape, stat)
        complex(sp), allocatable, intent(out) :: array(:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2), &
            &    vshape(3)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_csp_3
    module subroutine load_npy_csp_4(filename, array, iostat, iomsg)
        character(len=*), intent(in) :: filename
        complex(sp), allocatable, intent(out) :: array(:,:,:,:)
        integer, intent(out), optional :: iostat
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_csp
        integer, parameter :: rank = 4

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            integer :: i
            character(len=:), allocatable :: this_type
            logical :: fortran_order
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, fortran_order, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (.not.fortran_order) then
                vshape = [(vshape(i), i = size(vshape), 1, -1)]
            end if

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

    subroutine allocator(array, vshape, stat)
        complex(sp), allocatable, intent(out) :: array(:,:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2), &
            &    vshape(3), &
            &    vshape(4)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_csp_4
    module subroutine load_npy_cdp_1(filename, array, iostat, iomsg)
        character(len=*), intent(in) :: filename
        complex(dp), allocatable, intent(out) :: array(:)
        integer, intent(out), optional :: iostat
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_cdp
        integer, parameter :: rank = 1

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            integer :: i
            character(len=:), allocatable :: this_type
            logical :: fortran_order
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, fortran_order, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (.not.fortran_order) then
                vshape = [(vshape(i), i = size(vshape), 1, -1)]
            end if

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

    subroutine allocator(array, vshape, stat)
        complex(dp), allocatable, intent(out) :: array(:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_cdp_1
    module subroutine load_npy_cdp_2(filename, array, iostat, iomsg)
        character(len=*), intent(in) :: filename
        complex(dp), allocatable, intent(out) :: array(:,:)
        integer, intent(out), optional :: iostat
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_cdp
        integer, parameter :: rank = 2

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            integer :: i
            character(len=:), allocatable :: this_type
            logical :: fortran_order
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, fortran_order, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (.not.fortran_order) then
                vshape = [(vshape(i), i = size(vshape), 1, -1)]
            end if

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

    subroutine allocator(array, vshape, stat)
        complex(dp), allocatable, intent(out) :: array(:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_cdp_2
    module subroutine load_npy_cdp_3(filename, array, iostat, iomsg)
        character(len=*), intent(in) :: filename
        complex(dp), allocatable, intent(out) :: array(:,:,:)
        integer, intent(out), optional :: iostat
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_cdp
        integer, parameter :: rank = 3

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            integer :: i
            character(len=:), allocatable :: this_type
            logical :: fortran_order
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, fortran_order, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (.not.fortran_order) then
                vshape = [(vshape(i), i = size(vshape), 1, -1)]
            end if

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

    subroutine allocator(array, vshape, stat)
        complex(dp), allocatable, intent(out) :: array(:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2), &
            &    vshape(3)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_cdp_3
    module subroutine load_npy_cdp_4(filename, array, iostat, iomsg)
        character(len=*), intent(in) :: filename
        complex(dp), allocatable, intent(out) :: array(:,:,:,:)
        integer, intent(out), optional :: iostat
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_cdp
        integer, parameter :: rank = 4

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            integer :: i
            character(len=:), allocatable :: this_type
            logical :: fortran_order
            integer, allocatable :: vshape(:)

            call get_descriptor(io, this_type, fortran_order, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (.not.fortran_order) then
                vshape = [(vshape(i), i = size(vshape), 1, -1)]
            end if

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

    subroutine allocator(array, vshape, stat)
        complex(dp), allocatable, intent(out) :: array(:,:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
            &    vshape(1), &
            &    vshape(2), &
            &    vshape(3), &
            &    vshape(4)), &
            &    stat=stat)

    end subroutine allocator

    end subroutine load_npy_cdp_4


    subroutine get_descriptor(io, vtype, fortran_order, vshape, stat, msg)
        integer, intent(in) :: io
        character(len=:), allocatable, intent(out) :: vtype
        logical, intent(out) :: fortran_order
        integer, allocatable, intent(out) :: vshape(:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        integer :: major, header_len
        character(len=:), allocatable :: dict
        character(len=8) :: header
        character :: buf(4)

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

        if (scan(dict, char(0)) > 0) then
            stat = 1
            msg = "Nul byte not allowed in descriptor string"
            return
        end if

        call parse_descriptor(trim(dict(:len(dict)-1)), vtype, fortran_order, vshape, &
            & stat, msg)
        if (stat /= 0) return
    end subroutine get_descriptor


    subroutine parse_header(header, major, stat, msg)
        character(len=*), intent(in) :: header
        integer, intent(out) :: major
        integer, intent(out) :: stat
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

    subroutine parse_descriptor(input, vtype, fortran_order, vshape, stat, msg)
        character(len=*), intent(in) :: input
        character(len=:), allocatable, intent(out) :: vtype
        logical, intent(out) :: fortran_order
        integer, allocatable, intent(out) :: vshape(:)
        integer, intent(out) :: stat
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
                    call parse_tuple(input, pos, token, vshape, stat)

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

    subroutine parse_tuple(input, pos, token, tuple, stat)
        character(len=*), intent(in) :: input
        integer, intent(inout) :: pos
        type(token_type), intent(out) :: token
        integer, allocatable, intent(out) :: tuple(:)
        integer, intent(out) :: stat

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

    subroutine next_token(input, pos, token, allowed_token, stat)
        character(len=*), intent(in) :: input
        integer, intent(inout) :: pos
        type(token_type), intent(out) :: token
        integer, intent(in) :: allowed_token(:)
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

    subroutine get_token(input, pos, token)
        character(len=*), intent(in) :: input
        integer, intent(inout) :: pos
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
                    token%last = pos - 1
                    pos = pos - 1
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
        case(" ", char(10))
            token = token_type(pos, pos, space)
        case default
            token = token_type(pos, pos, invalid)
        end select

    end subroutine get_token

    end subroutine parse_descriptor

end submodule stdlib_io_npy_load
