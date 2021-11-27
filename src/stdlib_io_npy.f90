! SPDX-Identifer: MIT


!> Description of the npy format taken from
!> https://numpy.org/doc/stable/reference/generated/numpy.lib.format.html
!>
!>## Format Version 1.0
!>
!> The first 6 bytes are a magic string: exactly \x93NUMPY.
!>
!> The next 1 byte is an unsigned byte:
!> the major version number of the file format, e.g. \x01.
!>
!> The next 1 byte is an unsigned byte:
!> the minor version number of the file format, e.g. \x00.
!> Note: the version of the file format is not tied to the version of the numpy package.
!>
!> The next 2 bytes form a little-endian unsigned short int:
!> the length of the header data HEADER_LEN.
!>
!> The next HEADER_LEN bytes form the header data describing the array’s format.
!> It is an ASCII string which contains a Python literal expression of a dictionary.
!> It is terminated by a newline (\n) and padded with spaces (\x20) to make the total
!> of len(magic string) + 2 + len(length) + HEADER_LEN be evenly divisible by 64 for
!> alignment purposes.
!>
!> The dictionary contains three keys:
!>
!> - “descr”: dtype.descr
!>   An object that can be passed as an argument to the numpy.dtype constructor
!>   to create the array’s dtype.
!>
!> - “fortran_order”: bool
!>   Whether the array data is Fortran-contiguous or not. Since Fortran-contiguous
!>   arrays are a common form of non-C-contiguity, we allow them to be written directly
!>   to disk for efficiency.
!>
!> - “shape”: tuple of int
!>   The shape of the array.
!>
!> For repeatability and readability, the dictionary keys are sorted in alphabetic order.
!> This is for convenience only. A writer SHOULD implement this if possible. A reader MUST
!> NOT depend on this.
!>
!> Following the header comes the array data. If the dtype contains Python objects
!> (i.e. dtype.hasobject is True), then the data is a Python pickle of the array.
!> Otherwise the data is the contiguous (either C- or Fortran-, depending on fortran_order)
!> bytes of the array. Consumers can figure out the number of bytes by multiplying the
!> number of elements given by the shape (noting that shape=() means there is 1 element)
!> by dtype.itemsize.
!>
!>## Format Version 2.0
!>
!> The version 1.0 format only allowed the array header to have a total size of 65535 bytes.
!> This can be exceeded by structured arrays with a large number of columns.
!> The version 2.0 format extends the header size to 4 GiB. numpy.save will automatically
!> save in 2.0 format if the data requires it, else it will always use the more compatible
!> 1.0 format.
!>
!> The description of the fourth element of the header therefore has become:
!> “The next 4 bytes form a little-endian unsigned int: the length of the header data
!> HEADER_LEN.”
!>
!>## Format Version 3.0
!>
!> This version replaces the ASCII string (which in practice was latin1) with a
!> utf8-encoded string, so supports structured types with any unicode field names.
module stdlib_io_npy
    use stdlib_error, only : error_stop
    use stdlib_kinds, only : int8, int16, int32, int64, sp, dp, xdp, qp
    use stdlib_strings, only : to_string
    implicit none
    private

    public :: save_npy


    !> Save multidimensional array in npy format
    interface save_npy
        module procedure save_npy_rsp_1
        module procedure save_npy_rsp_2
        module procedure save_npy_rsp_3
        module procedure save_npy_rsp_4
        module procedure save_npy_rdp_1
        module procedure save_npy_rdp_2
        module procedure save_npy_rdp_3
        module procedure save_npy_rdp_4
        module procedure save_npy_iint8_1
        module procedure save_npy_iint8_2
        module procedure save_npy_iint8_3
        module procedure save_npy_iint8_4
        module procedure save_npy_iint16_1
        module procedure save_npy_iint16_2
        module procedure save_npy_iint16_3
        module procedure save_npy_iint16_4
        module procedure save_npy_iint32_1
        module procedure save_npy_iint32_2
        module procedure save_npy_iint32_3
        module procedure save_npy_iint32_4
        module procedure save_npy_iint64_1
        module procedure save_npy_iint64_2
        module procedure save_npy_iint64_3
        module procedure save_npy_iint64_4
        module procedure save_npy_csp_1
        module procedure save_npy_csp_2
        module procedure save_npy_csp_3
        module procedure save_npy_csp_4
        module procedure save_npy_cdp_1
        module procedure save_npy_cdp_2
        module procedure save_npy_cdp_3
        module procedure save_npy_cdp_4
    end interface save_npy


    character(len=*), parameter :: nl = char(10)

    character(len=*), parameter :: &
        type_iint8 = "<i1", type_iint16 = "<i2", type_iint32 = "<i4", type_iint64 = "<i8", &
        type_rsp = "<f4", type_rdp = "<f8", type_rxdp = "<f10", type_rqp = "<f16", &
        type_csp = "<c8", type_cdp = "<c16", type_cxdp = "<c20", type_cqp = "<c32"

    character(len=*), parameter :: &
        & magic_number = char(int(z"93")), &
        & magic_string = "NUMPY"

contains


    !> Generate magic header string for npy format
    pure function magic_header(major, minor) result(str)
        !> Major version of npy format
        integer, intent(in) :: major
        !> Minor version of npy format
        integer, intent(in) :: minor
        !> Magic string for npy format
        character(len=8) :: str

        str = magic_number // magic_string // char(major) // char(minor)
    end function magic_header


    !> Generate header for npy format
    pure function npy_header(vtype, vshape) result(str)
        !> Type of variable
        character(len=*), intent(in) :: vtype
        !> Shape of variable
        integer, intent(in) :: vshape(:)
        !> Header string for npy format
        character(len=:), allocatable :: str

        integer, parameter :: len_v10 = 8 + 2, len_v20 = 8 + 4

        str = &
            "{'descr': '"//vtype//&
            "', 'fortran_order': True, 'shape': "//&
            shape_str(vshape)//", }"

        if (len(str) + len_v10 >= 65535) then
            str = str // &
                & repeat(" ", 16 - mod(len(str) + len_v20 + 1, 16)) // nl
            str = magic_header(2, 0) // to_bytes_i4(int(len(str))) // str
        else
            str = str // &
                & repeat(" ", 16 - mod(len(str) + len_v10 + 1, 16)) // nl
            str = magic_header(1, 0) // to_bytes_i2(int(len(str))) // str
        end if
    end function npy_header

    !> Write integer as byte string in little endian encoding
    pure function to_bytes_i4(val) result(str)
        !> Integer value to convert to bytes
        integer, intent(in) :: val
        !> String of bytes
        character(len=4), allocatable :: str

        str = char(mod(val, 2**8)) // &
            & char(mod(val, 2**16) / 2**8) // &
            & char(mod(val, 2**32) / 2**16) // &
            & char(val / 2**32)
    end function to_bytes_i4


    !> Write integer as byte string in little endian encoding, 2-byte truncated version
    pure function to_bytes_i2(val) result(str)
        !> Integer value to convert to bytes
        integer, intent(in) :: val
        !> String of bytes
        character(len=2), allocatable :: str

        str = char(mod(val, 2**8)) // &
            & char(mod(val, 2**16) / 2**8)
    end function to_bytes_i2


    !> Print array shape as tuple of int
    pure function shape_str(vshape) result(str)
        !> Shape of variable
        integer, intent(in) :: vshape(:)
        !> Shape string for npy format
        character(len=:), allocatable :: str

        integer :: i

        str = "("
        do i = 1, size(vshape)
            str = str//to_string(vshape(i))//", "
        enddo
        str = str//")"
    end function shape_str


    !> Save 1-dimensional array in npy format
    subroutine save_npy_rsp_1(filename, array, iostat)
        character(len=*), intent(in) :: filename
        real(sp), intent(in) :: array(:)
        integer, intent(out), optional :: iostat
        character(len=*), parameter :: vtype = type_rsp

        integer :: io, stat

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write(io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write(io, iostat=stat) array
        end if
        close(io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

    end subroutine save_npy_rsp_1
    !> Save 2-dimensional array in npy format
    subroutine save_npy_rsp_2(filename, array, iostat)
        character(len=*), intent(in) :: filename
        real(sp), intent(in) :: array(:,:)
        integer, intent(out), optional :: iostat
        character(len=*), parameter :: vtype = type_rsp

        integer :: io, stat

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write(io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write(io, iostat=stat) array
        end if
        close(io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

    end subroutine save_npy_rsp_2
    !> Save 3-dimensional array in npy format
    subroutine save_npy_rsp_3(filename, array, iostat)
        character(len=*), intent(in) :: filename
        real(sp), intent(in) :: array(:,:,:)
        integer, intent(out), optional :: iostat
        character(len=*), parameter :: vtype = type_rsp

        integer :: io, stat

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write(io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write(io, iostat=stat) array
        end if
        close(io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

    end subroutine save_npy_rsp_3
    !> Save 4-dimensional array in npy format
    subroutine save_npy_rsp_4(filename, array, iostat)
        character(len=*), intent(in) :: filename
        real(sp), intent(in) :: array(:,:,:,:)
        integer, intent(out), optional :: iostat
        character(len=*), parameter :: vtype = type_rsp

        integer :: io, stat

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write(io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write(io, iostat=stat) array
        end if
        close(io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

    end subroutine save_npy_rsp_4
    !> Save 1-dimensional array in npy format
    subroutine save_npy_rdp_1(filename, array, iostat)
        character(len=*), intent(in) :: filename
        real(dp), intent(in) :: array(:)
        integer, intent(out), optional :: iostat
        character(len=*), parameter :: vtype = type_rdp

        integer :: io, stat

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write(io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write(io, iostat=stat) array
        end if
        close(io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

    end subroutine save_npy_rdp_1
    !> Save 2-dimensional array in npy format
    subroutine save_npy_rdp_2(filename, array, iostat)
        character(len=*), intent(in) :: filename
        real(dp), intent(in) :: array(:,:)
        integer, intent(out), optional :: iostat
        character(len=*), parameter :: vtype = type_rdp

        integer :: io, stat

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write(io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write(io, iostat=stat) array
        end if
        close(io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

    end subroutine save_npy_rdp_2
    !> Save 3-dimensional array in npy format
    subroutine save_npy_rdp_3(filename, array, iostat)
        character(len=*), intent(in) :: filename
        real(dp), intent(in) :: array(:,:,:)
        integer, intent(out), optional :: iostat
        character(len=*), parameter :: vtype = type_rdp

        integer :: io, stat

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write(io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write(io, iostat=stat) array
        end if
        close(io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

    end subroutine save_npy_rdp_3
    !> Save 4-dimensional array in npy format
    subroutine save_npy_rdp_4(filename, array, iostat)
        character(len=*), intent(in) :: filename
        real(dp), intent(in) :: array(:,:,:,:)
        integer, intent(out), optional :: iostat
        character(len=*), parameter :: vtype = type_rdp

        integer :: io, stat

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write(io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write(io, iostat=stat) array
        end if
        close(io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

    end subroutine save_npy_rdp_4
    !> Save 1-dimensional array in npy format
    subroutine save_npy_iint8_1(filename, array, iostat)
        character(len=*), intent(in) :: filename
        integer(int8), intent(in) :: array(:)
        integer, intent(out), optional :: iostat
        character(len=*), parameter :: vtype = type_iint8

        integer :: io, stat

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write(io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write(io, iostat=stat) array
        end if
        close(io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

    end subroutine save_npy_iint8_1
    !> Save 2-dimensional array in npy format
    subroutine save_npy_iint8_2(filename, array, iostat)
        character(len=*), intent(in) :: filename
        integer(int8), intent(in) :: array(:,:)
        integer, intent(out), optional :: iostat
        character(len=*), parameter :: vtype = type_iint8

        integer :: io, stat

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write(io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write(io, iostat=stat) array
        end if
        close(io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

    end subroutine save_npy_iint8_2
    !> Save 3-dimensional array in npy format
    subroutine save_npy_iint8_3(filename, array, iostat)
        character(len=*), intent(in) :: filename
        integer(int8), intent(in) :: array(:,:,:)
        integer, intent(out), optional :: iostat
        character(len=*), parameter :: vtype = type_iint8

        integer :: io, stat

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write(io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write(io, iostat=stat) array
        end if
        close(io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

    end subroutine save_npy_iint8_3
    !> Save 4-dimensional array in npy format
    subroutine save_npy_iint8_4(filename, array, iostat)
        character(len=*), intent(in) :: filename
        integer(int8), intent(in) :: array(:,:,:,:)
        integer, intent(out), optional :: iostat
        character(len=*), parameter :: vtype = type_iint8

        integer :: io, stat

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write(io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write(io, iostat=stat) array
        end if
        close(io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

    end subroutine save_npy_iint8_4
    !> Save 1-dimensional array in npy format
    subroutine save_npy_iint16_1(filename, array, iostat)
        character(len=*), intent(in) :: filename
        integer(int16), intent(in) :: array(:)
        integer, intent(out), optional :: iostat
        character(len=*), parameter :: vtype = type_iint16

        integer :: io, stat

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write(io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write(io, iostat=stat) array
        end if
        close(io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

    end subroutine save_npy_iint16_1
    !> Save 2-dimensional array in npy format
    subroutine save_npy_iint16_2(filename, array, iostat)
        character(len=*), intent(in) :: filename
        integer(int16), intent(in) :: array(:,:)
        integer, intent(out), optional :: iostat
        character(len=*), parameter :: vtype = type_iint16

        integer :: io, stat

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write(io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write(io, iostat=stat) array
        end if
        close(io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

    end subroutine save_npy_iint16_2
    !> Save 3-dimensional array in npy format
    subroutine save_npy_iint16_3(filename, array, iostat)
        character(len=*), intent(in) :: filename
        integer(int16), intent(in) :: array(:,:,:)
        integer, intent(out), optional :: iostat
        character(len=*), parameter :: vtype = type_iint16

        integer :: io, stat

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write(io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write(io, iostat=stat) array
        end if
        close(io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

    end subroutine save_npy_iint16_3
    !> Save 4-dimensional array in npy format
    subroutine save_npy_iint16_4(filename, array, iostat)
        character(len=*), intent(in) :: filename
        integer(int16), intent(in) :: array(:,:,:,:)
        integer, intent(out), optional :: iostat
        character(len=*), parameter :: vtype = type_iint16

        integer :: io, stat

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write(io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write(io, iostat=stat) array
        end if
        close(io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

    end subroutine save_npy_iint16_4
    !> Save 1-dimensional array in npy format
    subroutine save_npy_iint32_1(filename, array, iostat)
        character(len=*), intent(in) :: filename
        integer(int32), intent(in) :: array(:)
        integer, intent(out), optional :: iostat
        character(len=*), parameter :: vtype = type_iint32

        integer :: io, stat

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write(io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write(io, iostat=stat) array
        end if
        close(io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

    end subroutine save_npy_iint32_1
    !> Save 2-dimensional array in npy format
    subroutine save_npy_iint32_2(filename, array, iostat)
        character(len=*), intent(in) :: filename
        integer(int32), intent(in) :: array(:,:)
        integer, intent(out), optional :: iostat
        character(len=*), parameter :: vtype = type_iint32

        integer :: io, stat

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write(io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write(io, iostat=stat) array
        end if
        close(io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

    end subroutine save_npy_iint32_2
    !> Save 3-dimensional array in npy format
    subroutine save_npy_iint32_3(filename, array, iostat)
        character(len=*), intent(in) :: filename
        integer(int32), intent(in) :: array(:,:,:)
        integer, intent(out), optional :: iostat
        character(len=*), parameter :: vtype = type_iint32

        integer :: io, stat

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write(io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write(io, iostat=stat) array
        end if
        close(io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

    end subroutine save_npy_iint32_3
    !> Save 4-dimensional array in npy format
    subroutine save_npy_iint32_4(filename, array, iostat)
        character(len=*), intent(in) :: filename
        integer(int32), intent(in) :: array(:,:,:,:)
        integer, intent(out), optional :: iostat
        character(len=*), parameter :: vtype = type_iint32

        integer :: io, stat

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write(io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write(io, iostat=stat) array
        end if
        close(io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

    end subroutine save_npy_iint32_4
    !> Save 1-dimensional array in npy format
    subroutine save_npy_iint64_1(filename, array, iostat)
        character(len=*), intent(in) :: filename
        integer(int64), intent(in) :: array(:)
        integer, intent(out), optional :: iostat
        character(len=*), parameter :: vtype = type_iint64

        integer :: io, stat

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write(io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write(io, iostat=stat) array
        end if
        close(io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

    end subroutine save_npy_iint64_1
    !> Save 2-dimensional array in npy format
    subroutine save_npy_iint64_2(filename, array, iostat)
        character(len=*), intent(in) :: filename
        integer(int64), intent(in) :: array(:,:)
        integer, intent(out), optional :: iostat
        character(len=*), parameter :: vtype = type_iint64

        integer :: io, stat

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write(io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write(io, iostat=stat) array
        end if
        close(io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

    end subroutine save_npy_iint64_2
    !> Save 3-dimensional array in npy format
    subroutine save_npy_iint64_3(filename, array, iostat)
        character(len=*), intent(in) :: filename
        integer(int64), intent(in) :: array(:,:,:)
        integer, intent(out), optional :: iostat
        character(len=*), parameter :: vtype = type_iint64

        integer :: io, stat

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write(io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write(io, iostat=stat) array
        end if
        close(io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

    end subroutine save_npy_iint64_3
    !> Save 4-dimensional array in npy format
    subroutine save_npy_iint64_4(filename, array, iostat)
        character(len=*), intent(in) :: filename
        integer(int64), intent(in) :: array(:,:,:,:)
        integer, intent(out), optional :: iostat
        character(len=*), parameter :: vtype = type_iint64

        integer :: io, stat

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write(io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write(io, iostat=stat) array
        end if
        close(io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

    end subroutine save_npy_iint64_4
    !> Save 1-dimensional array in npy format
    subroutine save_npy_csp_1(filename, array, iostat)
        character(len=*), intent(in) :: filename
        complex(sp), intent(in) :: array(:)
        integer, intent(out), optional :: iostat
        character(len=*), parameter :: vtype = type_csp

        integer :: io, stat

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write(io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write(io, iostat=stat) array
        end if
        close(io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

    end subroutine save_npy_csp_1
    !> Save 2-dimensional array in npy format
    subroutine save_npy_csp_2(filename, array, iostat)
        character(len=*), intent(in) :: filename
        complex(sp), intent(in) :: array(:,:)
        integer, intent(out), optional :: iostat
        character(len=*), parameter :: vtype = type_csp

        integer :: io, stat

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write(io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write(io, iostat=stat) array
        end if
        close(io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

    end subroutine save_npy_csp_2
    !> Save 3-dimensional array in npy format
    subroutine save_npy_csp_3(filename, array, iostat)
        character(len=*), intent(in) :: filename
        complex(sp), intent(in) :: array(:,:,:)
        integer, intent(out), optional :: iostat
        character(len=*), parameter :: vtype = type_csp

        integer :: io, stat

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write(io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write(io, iostat=stat) array
        end if
        close(io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

    end subroutine save_npy_csp_3
    !> Save 4-dimensional array in npy format
    subroutine save_npy_csp_4(filename, array, iostat)
        character(len=*), intent(in) :: filename
        complex(sp), intent(in) :: array(:,:,:,:)
        integer, intent(out), optional :: iostat
        character(len=*), parameter :: vtype = type_csp

        integer :: io, stat

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write(io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write(io, iostat=stat) array
        end if
        close(io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

    end subroutine save_npy_csp_4
    !> Save 1-dimensional array in npy format
    subroutine save_npy_cdp_1(filename, array, iostat)
        character(len=*), intent(in) :: filename
        complex(dp), intent(in) :: array(:)
        integer, intent(out), optional :: iostat
        character(len=*), parameter :: vtype = type_cdp

        integer :: io, stat

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write(io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write(io, iostat=stat) array
        end if
        close(io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

    end subroutine save_npy_cdp_1
    !> Save 2-dimensional array in npy format
    subroutine save_npy_cdp_2(filename, array, iostat)
        character(len=*), intent(in) :: filename
        complex(dp), intent(in) :: array(:,:)
        integer, intent(out), optional :: iostat
        character(len=*), parameter :: vtype = type_cdp

        integer :: io, stat

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write(io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write(io, iostat=stat) array
        end if
        close(io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

    end subroutine save_npy_cdp_2
    !> Save 3-dimensional array in npy format
    subroutine save_npy_cdp_3(filename, array, iostat)
        character(len=*), intent(in) :: filename
        complex(dp), intent(in) :: array(:,:,:)
        integer, intent(out), optional :: iostat
        character(len=*), parameter :: vtype = type_cdp

        integer :: io, stat

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write(io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write(io, iostat=stat) array
        end if
        close(io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

    end subroutine save_npy_cdp_3
    !> Save 4-dimensional array in npy format
    subroutine save_npy_cdp_4(filename, array, iostat)
        character(len=*), intent(in) :: filename
        complex(dp), intent(in) :: array(:,:,:,:)
        integer, intent(out), optional :: iostat
        character(len=*), parameter :: vtype = type_cdp

        integer :: io, stat

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write(io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write(io, iostat=stat) array
        end if
        close(io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

    end subroutine save_npy_cdp_4


end module stdlib_io_npy
