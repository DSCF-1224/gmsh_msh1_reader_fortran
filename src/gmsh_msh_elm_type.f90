!> Implementation of a derived type to hold `elm-type` of Gmsh MSH file format
module gmsh_msh_elm_type_fortran

    use, intrinsic :: iso_c_binding, only: c_int



    implicit none



    private



    public :: gmsh_msh_elm_type

    public :: operator(.eq.)
    public :: read(formatted)

    public :: export
    public :: initialize



    !> Version: experimental
    !> Derived type to hold `elm-type` of Gmsh MSH file format
    type :: gmsh_msh_elm_type

        integer(c_int), private :: expression

    end type



    !> Version: experimental
    !> Compares two `elm-type` for equality.
    interface operator(.eq.)
        module procedure :: is_equal
    end interface operator(.eq.)

    !> Version: experimental
    !> Read an `elm-type` from a connected formatted unit.
    interface read(formatted)
        module procedure :: read_formatted
    end interface

    !> Version: experimental
    !> Export the `elm-type` as an integer.
    interface export
        module procedure :: export_expression
    end interface export

    !> Version: experimental
    !> Initialize the expression to hold the `elm-type`.
    interface initialize
        module procedure :: initialize_gmsh_msh_elm_type
    end interface initialize



    contains



    !> Version: experimental
    !> Export the `elm-type` as an integer.
    elemental function export_expression(elm_type) result(expression)

        type(gmsh_msh_elm_type), intent(in) :: elm_type

        integer :: expression



        expression = elm_type%expression

    end function export_expression



    !> Version: experimental
    !> Compares two `elm-type` for equality.
    elemental function is_equal(type1, type2)

        type(gmsh_msh_elm_type), intent(in) :: type1, type2

        logical :: is_equal



        is_equal = type1%expression .eq. type2%expression

    end function is_equal



    !> Version: experimental
    !> Read an `elm-type` from a connected formatted unit.
    subroutine read_formatted(elm_type, unit, iotype, v_list, iostat, iomsg)

        class(gmsh_msh_elm_type), intent(inout) :: elm_type

        integer, intent(in) :: unit

        character(*), intent(in) :: iotype

        integer, intent(in) :: v_list(:)

        integer, intent(out) :: iostat

        character(*), intent(inout) :: iomsg



        iomsg = ''



        if ( size( v_list(:) ) .gt. 0 ) then

            error stop "gmsh_msh_elm_type does NOT support v_list formatters."

        end if



        select case(iotype)

            case("LISTDIRECTED")

                read( &!
                unit   = unit     , &!
                fmt    = *        , &!
                iostat = iostat   , &!
                iomsg  = iomsg(:)   &!
                ) &!
                elm_type%expression

            case default

                error stop "gmsh_msh_elm_type only supports the `LISTDIRECTED` iotype."

        end select

    end subroutine read_formatted



    !> Version: experimental
    !> Initialize the expression to hold the `elm-type`.
    elemental subroutine initialize_gmsh_msh_elm_type(elm_type)

        type(gmsh_msh_elm_type), intent(out) :: elm_type



        elm_type%expression = 0

    end subroutine initialize_gmsh_msh_elm_type

end module gmsh_msh_elm_type_fortran
