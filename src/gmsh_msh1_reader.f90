!> Fortran module for reading |GmshReferenceManualTop| |GmshReferenceManualMsh1|
module gmsh_msh1_reader

    use, intrinsic :: iso_fortran_env , &!
        only: iostat_eor , &!
        &     iostat_end , &!
        &     real64

    use, intrinsic :: ieee_arithmetic , &!
        only: ieee_is_finite     , &!
        &     ieee_signaling_nan , &!
        &     ieee_value



    implicit none



    private



    public :: operator(.eq.)
    public :: export_elm_number
    public :: export_elm_type
    public :: export_node_number
    public :: export_node_number_list
    public :: export_reg_elem
    public :: export_reg_phys
    public :: gmsh_msh1_data_type
    public :: gmsh_msh1_element_type
    public :: gmsh_msh1_elm_number_type
    public :: gmsh_msh1_node_type
    public :: gmsh_msh1_node_number_type
    public :: gmsh_msh1_reg_elem_type
    public :: gmsh_msh1_reg_phys_type
    public :: lookup_element
    public :: lookup_node
    public :: output_elm_number
    public :: output_elm_type
    public :: output_node_number
    public :: output_node_number_list
    public :: output_number_of_elements
    public :: output_number_of_nodes
    public :: output_reg_elem
    public :: output_reg_phys
    public :: output_x_coord
    public :: output_y_coord
    public :: output_z_coord
    public :: read_gmsh_msh1_file
    public :: validate
    public :: write_stat_msg_gmsh_msh1_file



    !> Whether found the node section header<br>
    !> (Initial value)
    logical, parameter :: initial_flag_nod_section_header = .false.

    !> Whether found the number of nodes<br>
    !> (Initial value)
    logical, parameter :: initial_flag_number_of_nodes = .false.

    !> Whether deallocated the array to read nodes
    !> (Initial value)
    logical, parameter :: initial_flag_deallocation_nodes = .false.

    !> Whether allocated the array to read nodes
    !> (Initial value)
    logical, parameter :: initial_flag_allocation_nodes = .false.

    !> Whether the read was successful: node
    !> (Initial value)
    logical, parameter :: initial_flag_reading_node = .false.

    !> Whether found the node section footer<br>
    !> (Initial value)
    logical, parameter :: initial_flag_nod_section_footer = .false.

    !> Whether found the element section header<br>
    !> (Initial value)
    logical, parameter :: initial_flag_elm_section_header = .false.

    !> Whether found the number of elements<br>
    !> (Initial value)
    logical, parameter :: initial_flag_number_of_elements = .false.

    !> Whether deallocated the array to read elements
    !> (Initial value)
    logical, parameter :: initial_flag_deallocation_elements = .false.

    !> Whether allocated the array to read elements
    !> (Initial value)
    logical, parameter :: initial_flag_allocation_elements = .false.

    !> Whether the read was successful: element
    !> (Initial value)
    logical, parameter :: initial_flag_reading_elements = .false.

    !> Whether found the element section footer<br>
    !> (Initial value)
    logical, parameter :: initial_flag_elm_section_footer = .false.



    !> `iostat` value when an I/O statement executes successfully
    integer, parameter :: iostat_success = 0

    !> version: experimental
    integer, parameter :: minval_location = 1

    !> version: experimental
    !> message length
    integer, parameter :: msg_len = 512

    !> `stat` value when a statement executes successfully
    integer, parameter :: stat_success = 0



    character(len=*), parameter :: elm_section_header = '$ELM'
    character(len=*), parameter :: elm_section_footer = '$ENDELM'

    character(len=*), parameter :: nod_section_header = '$NOD'
    character(len=*), parameter :: nod_section_footer = '$ENDNOD'



    !> version: experimental
    !> Derived type to for reading  
    !> - |DescGmshMsh1ElmNumber|  
    !> - |DescGmshMsh1NodeNumber|
    !>
    !> @warning
    !> |WarnGmshMsh1NumberType|
    !> @endwarning
    !>
    !> @note
    !> The [[gmsh_msh1_number_type:number]] do not necessarily have to form a dense nor an ordered sequence.
    !> @endnote
    type, abstract :: gmsh_msh1_number_type

        integer, private :: number

    end type gmsh_msh1_number_type



    !> version: experimental
    !> Derived type to for reading |DescGmshMsh1ElmNumber|
    type, extends(gmsh_msh1_number_type) :: gmsh_msh1_elm_number_type
    end type gmsh_msh1_elm_number_type



    !> version: experimental
    !> Derived type to for reading |DescGmshMsh1ElmType|
    type :: gmsh_msh1_elm_type

        integer, private :: expression

    end type gmsh_msh1_elm_type



    !> version: experimental
    !> Derived type to for reading  
    !> - |DescGmshMsh1RegElem|  
    !> - |DescGmshMsh1RegPhys|
    type, abstract :: gmsh_msh1_tag_type

        integer, private :: expression

    end type gmsh_msh1_tag_type



    !> version: experimental
    !> Derived type to for reading |DescGmshMsh1RegElem|
    !>
    !> @warning
    !> |WarnGmshMsh1RegElem|
    !> @endwarning
    type, extends(gmsh_msh1_tag_type) :: gmsh_msh1_reg_elem_type
    end type gmsh_msh1_reg_elem_type



    !> version: experimental
    !> Derived type to for reading |DescGmshMsh1RegPhys|
    !>
    !> @warning
    !> |WarnGmshMsh1RegPhys|
    !> @endwarning
    !>
    !> @note
    !> |NoteGmshMsh1RegPhys|
    !> @endnote
    type, extends(gmsh_msh1_tag_type) :: gmsh_msh1_reg_phys_type
    end type gmsh_msh1_reg_phys_type



    !> version: experimental
    !> Derived type to for reading the *n*-th element in the |GmshReferenceManualTop| |GmshReferenceManualMsh1|
    type :: gmsh_msh1_element_type

        private

        !> |DescGmshMsh1ElmNumber|
        type(gmsh_msh1_elm_number_type) :: elm_number

        !> |DescGmshMsh1ElmType|
        type(gmsh_msh1_elm_type) :: elm_type

        !> |DescGmshMsh1RegPhys|
        type(gmsh_msh1_reg_phys_type) :: reg_phys

        !> |DescGmshMsh1RegElem|
        type(gmsh_msh1_reg_elem_type) :: reg_elem

        !> the list of the `number_of_nodes` node numbers of the *n*-th element.
        type(gmsh_msh1_node_number_type), allocatable, dimension(:) :: node_number_list

    end type gmsh_msh1_element_type



    !> version: experimental
    !> Derived type to for reading |DescGmshMsh1NodeNumber|
    type, extends(gmsh_msh1_number_type) :: gmsh_msh1_node_number_type
    end type gmsh_msh1_node_number_type


    !> version: experimental
    !> Derived type to for reading
    !> the *n*-th node in the
    !> |GmshReferenceManualTop|
    !> |GmshReferenceManualMsh1|
    type :: gmsh_msh1_node_type

        private

        !> |DescGmshMsh1NodeNumber|
        type(gmsh_msh1_node_number_type) :: node_number

        !> The floating point values giving the X coordinates of the *n*-th node.
        real(real64) :: x_coord

        !> The floating point values giving the Y coordinates of the *n*-th node.
        real(real64) :: y_coord

        !> The floating point values giving the Z coordinates of the *n*-th node.
        real(real64) :: z_coord

    end type gmsh_msh1_node_type



    !> version: experimental
    type :: gmsh_msh1_status_unit_type

        integer :: code

        character(len = msg_len) :: msg

    end type gmsh_msh1_status_unit_type



    !> version: experimental
    type :: gmsh_msh1_status_type

        type(gmsh_msh1_status_unit_type) :: err, io

    end type gmsh_msh1_status_type



    !> version: experimental
    !> Derived type to for reading
    !> |GmshReferenceManualTop|
    !> |GmshReferenceManualMsh1|
    type :: gmsh_msh1_data_type

        private

        !> Whether found the node section header
        logical :: flag_nod_section_header = initial_flag_nod_section_header

        !> Whether found the number of nodes
        logical :: flag_number_of_nodes = initial_flag_number_of_nodes

        !> Whether deallocated the array to read nodes
        logical :: flag_deallocation_nodes = initial_flag_deallocation_nodes

        !> Whether allocated the array to read nodes
        logical :: flag_allocation_nodes = initial_flag_allocation_nodes

        !> Whether the read was successful: node
        logical :: flag_reading_nodes = initial_flag_reading_node

        !> Whether found the node section footer
        logical :: flag_nod_section_footer = initial_flag_nod_section_footer

        !> Whether found the element section header
        logical :: flag_elm_section_header = initial_flag_elm_section_header

        !> Whether found the number of elements
        logical :: flag_number_of_elements = initial_flag_number_of_elements

        !> Whether deallocated the array to read elements
        logical :: flag_deallocation_elements = initial_flag_deallocation_elements

        !> Whether allocated the array to read elements
        logical :: flag_allocation_elements = initial_flag_allocation_elements

        !> Whether the read was successful: element
        logical :: flag_reading_elements = initial_flag_reading_elements

        !> Whether found the element section footer
        logical :: flag_elm_section_footer = initial_flag_elm_section_footer



        type(gmsh_msh1_status_type) :: status

        !> the nodes in the mesh
        type(gmsh_msh1_node_type), allocatable, dimension(:) :: node

        !> the elements in the mesh
        type(gmsh_msh1_element_type), allocatable, dimension(:) :: element

    end type gmsh_msh1_data_type



    !> version: experimental
    interface operator(.eq.)
        module procedure :: is_equal_gmsh_msh1_elm_number_type
        module procedure :: is_equal_gmsh_msh1_elm_type
        module procedure :: is_equal_gmsh_msh1_node_number_type
        module procedure :: is_equal_gmsh_msh1_reg_elem_type
        module procedure :: is_equal_gmsh_msh1_reg_phys_type
    end interface operator(.eq.)



    !> version: experimental
    !> |DescExportElmNumber|
    interface export_elm_number
        module procedure :: export_elm_number_gmsh_msh1_element
    end interface export_elm_number



    !> version: experimental
    !> |DescExportElmType|
    interface export_elm_type 
        module procedure :: export_elm_type_gmsh_msh1_element
    end interface export_elm_type



    !> version: experimental
    !> |DescExportNodeNumber|
    interface export_node_number
        module procedure :: export_node_number_gmsh_msh1_node
    end interface export_node_number



    !> version: experimental
    !> |DescExportNodeNumberList|
    interface export_node_number_list
        module procedure :: export_node_number_list_gmsh_msh1_element
    end interface export_node_number_list



    !> version: experimental
    !> |DescExportRegElem|
    interface export_reg_elem
        module procedure :: export_reg_elem_gmsh_msh1_element
    end interface export_reg_elem



    !> version: experimental
    !> |DescExportRegPhys|
    interface export_reg_phys
        module procedure :: export_reg_phys_gmsh_msh1_element
    end interface export_reg_phys



    !> version: experimental
    interface findloc
        module procedure :: findloc_gmsh_msh1_node_number
    end interface findloc



    !> version: experimental
    !> |DescLookupElement|
    interface lookup_element
        module procedure :: lookup_element_by_loc_gmsh_msh1_file
    end interface lookup_element



    !> version: experimental
    !> |DescLookupNode|
    interface lookup_node
        module procedure :: lookup_node_by_loc_gmsh_msh1_file
        module procedure :: lookup_node_by_num_gmsh_msh1_file
    end interface lookup_node



    !> version: experimental
    !> |DescOutputElmNumber|
    interface output_elm_number
        module procedure :: output_elm_number_gmsh_msh1_element
    end interface output_elm_number



    !> version: experimental
    !> |DescOutputElmType|
    interface output_elm_type
        module procedure :: output_elm_type_gmsh_msh1_element
    end interface output_elm_type



    !> version: experimental
    !> |DescOutputNodeNumber|
    interface output_node_number
        module procedure :: output_node_number_gmsh_msh1_element
        module procedure :: output_node_number_gmsh_msh1_node
    end interface output_node_number



    !> version: experimental
    !> |DescOutputNodeNumberList|
    interface output_node_number_list
        module procedure :: output_node_number_list_gmsh_msh1_element
    end interface output_node_number_list



    !> version: experimental
    !> |DescOutputNumberOfElements|
    interface output_number_of_elements
        module procedure :: output_number_of_elements_gmsh_msh1_file
    end interface output_number_of_elements



    !> version: experimental
    !> |DescOutputNumberOfNodes|.
    interface output_number_of_nodes
        module procedure :: output_number_of_nodes_gmsh_msh1_element
        module procedure :: output_number_of_nodes_gmsh_msh1_file
    end interface output_number_of_nodes



    !> version: experimental
    !> |DescOutputRegElem|
    interface output_reg_elem
        module procedure :: output_reg_elem_gmsh_msh1_element
    end interface output_reg_elem



    !> version: experimental
    !> |DescOutputRegPhys|
    interface output_reg_phys
        module procedure :: output_reg_phys_gmsh_msh1_element
    end interface output_reg_phys



    !> version: experimental
    !> |DescOutputXCoord|
    interface output_x_coord
        module procedure :: output_x_coord_gmsh_msh1_node
    end interface output_x_coord



    !> version: experimental
    !> |DescOutputYCoord|
    interface output_y_coord
        module procedure :: output_y_coord_gmsh_msh1_node
    end interface output_y_coord



    !> version: experimental
    !> |DescOutputZCoord|
    interface output_z_coord
        module procedure :: output_z_coord_gmsh_msh1_node
    end interface output_z_coord



    !> version: experimental
    !> |DescValidate|
    interface validate
        module procedure :: validate_gmsh_msh1_element_without_mesh_data
        module procedure :: validate_gmsh_msh1_file
        module procedure :: validate_gmsh_msh1_node
        module procedure :: validate_gmsh_msh1_number
        module procedure :: validate_gmsh_msh1_reg_elem
        module procedure :: validate_gmsh_msh1_reg_phys
    end interface validate



    contains



    !> version: experimental
    !> If any of the flags is `.false.`, it indicates that the file read operation failed.
    elemental function all_flag(mesh_data)

        type(gmsh_msh1_data_type), intent(in) :: mesh_data

        logical :: all_flag



        all_flag =     mesh_data%flag_nod_section_header    &!
        &        .and. mesh_data%flag_number_of_nodes       &!
        &        .and. mesh_data%flag_deallocation_nodes    &!
        &        .and. mesh_data%flag_allocation_nodes      &!
        &        .and. mesh_data%flag_reading_nodes         &!
        &        .and. mesh_data%flag_nod_section_footer    &!
        &        .and. mesh_data%flag_elm_section_header    &!
        &        .and. mesh_data%flag_number_of_elements    &!
        &        .and. mesh_data%flag_deallocation_elements &!
        &        .and. mesh_data%flag_allocation_elements   &!
        &        .and. mesh_data%flag_reading_elements      &!
        &        .and. mesh_data%flag_elm_section_footer

    end function all_flag



    !> version: experimental
    !> |DescExportElmNumber|
    elemental function export_elm_number_gmsh_msh1_element(element) result(elm_number)

        type(gmsh_msh1_element_type), intent(in) :: element

        integer :: elm_number



        elm_number = element%elm_number%number

    end function export_elm_number_gmsh_msh1_element



    !> version: experimental
    !> |DescExportElmType|
    elemental function export_elm_type_gmsh_msh1_element(element) result(elm_type)

        type(gmsh_msh1_element_type), intent(in) :: element

        integer :: elm_type



        elm_type = element%elm_type%expression

    end function export_elm_type_gmsh_msh1_element



    !> version: experimental
    !> |DescExportNodeNumber|
    elemental function export_node_number_gmsh_msh1_node(node) result(node_number)

        type(gmsh_msh1_node_type), intent(in) :: node

        integer :: node_number



        node_number = node%node_number%number

    end function export_node_number_gmsh_msh1_node



    !> version: experimental
    !> |DescExportNodeNumberList|
    pure function export_node_number_list_gmsh_msh1_element(element) result(node_number_list)

        type(gmsh_msh1_element_type), intent(in) :: element

        integer, dimension( output_number_of_nodes(element) ) :: node_number_list



        node_number_list(:) = element%node_number_list(:)%number

    end function export_node_number_list_gmsh_msh1_element



    !> version: experimental
    !> |DescExportRegElem|
    elemental function export_reg_elem_gmsh_msh1_element(element) result(reg_elem)

        type(gmsh_msh1_element_type), intent(in) :: element

        integer :: reg_elem



        reg_elem = element%reg_elem%expression

    end function export_reg_elem_gmsh_msh1_element



    !> version: experimental
    !> |DescExportRegPhys|
    elemental function export_reg_phys_gmsh_msh1_element(element) result(reg_phys)

        type(gmsh_msh1_element_type), intent(in) :: element

        integer :: reg_phys



        reg_phys = element%reg_phys%expression

    end function export_reg_phys_gmsh_msh1_element



    !> version: experimental
    !> Returns the location of the [[gmsh_msh1_node_type]] corresponding to the [[findloc_gmsh_msh1_node_number:node_number]] argument.
    !> @warning
    !> If no [[gmsh_msh1_node_type]] corresponding to the [[findloc_gmsh_msh1_node_number:node_number]] argument exists,
    !> **zero** will be returned.
    elemental function findloc_gmsh_msh1_node_number(msh1_data, node_number) result(location)

        type(gmsh_msh1_data_type), intent(in) :: msh1_data

        type(gmsh_msh1_node_number_type), intent(in) :: node_number

        integer :: location



        integer :: itr_node



        location = 0



        if ( .not. allocated(msh1_data%node) ) return



        do itr_node = 1, output_number_of_nodes(msh1_data)

            if ( msh1_data%node(itr_node)%node_number .eq. node_number ) then

                location = itr_node

                return

            end if

        end do

    end function findloc_gmsh_msh1_node_number



    !> version: experimental
    elemental function is_iostat_failure(status)

        type(gmsh_msh1_status_type), intent(in) :: status

        logical :: is_iostat_failure



        is_iostat_failure = (status%io%code .ne. iostat_success)

    end function is_iostat_failure



    !> version: experimental
    elemental function is_iostat_success(status)

        type(gmsh_msh1_status_type), intent(in) :: status

        logical :: is_iostat_success



        is_iostat_success = (status%io%code .eq. iostat_success)

    end function is_iostat_success



    !> version: experimental
    elemental function is_equal_gmsh_msh1_elm_number_type(number1, number2) result(is_equal)

        type(gmsh_msh1_elm_number_type), intent(in) :: number1, number2

        logical :: is_equal



        is_equal = number1%number .eq. number2%number

    end function is_equal_gmsh_msh1_elm_number_type



    !> version: experimental
    elemental function is_equal_gmsh_msh1_elm_type(type1, type2) result(is_equal)

        type(gmsh_msh1_elm_type), intent(in) :: type1, type2

        logical :: is_equal



        is_equal = type1%expression .eq. type2%expression

    end function is_equal_gmsh_msh1_elm_type



    !> version: experimental
    elemental function is_equal_gmsh_msh1_node_number_type(number1, number2) result(is_equal)

        type(gmsh_msh1_node_number_type), intent(in) :: number1, number2

        logical :: is_equal



        is_equal = number1%number .eq. number2%number

    end function is_equal_gmsh_msh1_node_number_type



    !> version: experimental
    elemental function is_equal_gmsh_msh1_reg_elem_type(reg_elem1, reg_elem2) result(is_equal)

        type(gmsh_msh1_reg_elem_type), intent(in) :: reg_elem1, reg_elem2

        logical :: is_equal



        is_equal = reg_elem1%expression .eq. reg_elem2%expression

    end function is_equal_gmsh_msh1_reg_elem_type



    !> version: experimental
    elemental function is_equal_gmsh_msh1_reg_phys_type(reg_phys1, reg_phys2) result(is_equal)

        type(gmsh_msh1_reg_phys_type), intent(in) :: reg_phys1, reg_phys2

        logical :: is_equal



        is_equal = reg_phys1%expression .eq. reg_phys2%expression

    end function is_equal_gmsh_msh1_reg_phys_type



    !> version: experimental
    elemental function is_stat_failure(status)

        type(gmsh_msh1_status_type), intent(in) :: status

        logical :: is_stat_failure



        is_stat_failure = (status%err%code .ne. stat_success)

    end function is_stat_failure



    !> version: experimental
    !> |DescLookupNode|
    !> @warning
    !> If no [[gmsh_msh1_node_type]] corresponding to the [[lookup_node_by_loc_gmsh_msh1_file:location]] argument exists,
    !> a [[gmsh_msh1_node_type]] initialized by [[initialize_gmsh_msh1_node]] will be returned.
    elemental function lookup_node_by_loc_gmsh_msh1_file(mesh_data, location) result(node)

        type(gmsh_msh1_data_type), intent(in) :: mesh_data

        !> location in [[gmsh_msh1_data_type:node]]
        integer, intent(in) :: location

        type(gmsh_msh1_node_type) :: node



        if (location .lt. minval_location) then

            call initialize_gmsh_msh1_node(node)

        else if ( output_number_of_nodes(mesh_data) .lt. location ) then

            call initialize_gmsh_msh1_node(node)

        else

            node = mesh_data%node(location)

        end if

    end function lookup_node_by_loc_gmsh_msh1_file



    !> version: experimental
    !> |DescLookupNode|
    !> @warning
    !> If no [[gmsh_msh1_node_type]] corresponding to the [[lookup_node_by_num_gmsh_msh1_file:node_number]] argument exists,
    !> a [[gmsh_msh1_node_type]] initialized by [[initialize_gmsh_msh1_node]] will be returned.
    elemental function lookup_node_by_num_gmsh_msh1_file(mesh_data, node_number) result(node)

        type(gmsh_msh1_data_type), intent(in) :: mesh_data

        type(gmsh_msh1_node_number_type), intent(in) :: node_number

        type(gmsh_msh1_node_type) :: node



        integer :: location



        location = findloc(mesh_data, node_number)

        if (location .lt. minval_location) then

            call initialize_gmsh_msh1_node(node)

        else

            node = mesh_data%node(location)

        end if

    end function lookup_node_by_num_gmsh_msh1_file



    !> version: experimental
    !> |DescOutputElmNumber|
    elemental function output_elm_number_gmsh_msh1_element(element) result(elm_number)

        type(gmsh_msh1_element_type), intent(in) :: element

        type(gmsh_msh1_elm_number_type) :: elm_number



        elm_number = element%elm_number

    end function output_elm_number_gmsh_msh1_element



    !> version: experimental
    !> |DescOutputElmType|
    elemental function output_elm_type_gmsh_msh1_element(element) result(elm_type)

        type(gmsh_msh1_element_type), intent(in) :: element

        type(gmsh_msh1_elm_type) :: elm_type



        elm_type = element%elm_type

    end function output_elm_type_gmsh_msh1_element



    !> version: experimental
    !> |DescOutputNodeNumber|
    !> @warning
    !> If no [[gmsh_msh1_node_number_type]] corresponding to the [[output_node_number_gmsh_msh1_element:location]] argument exists,
    !> a [[gmsh_msh1_node_number_type]] initialized by [[initialize_gmsh_msh1_number]] will be returned.
    elemental function output_node_number_gmsh_msh1_element(element, location) result(node_number)

        type(gmsh_msh1_element_type), intent(in) :: element

        !> location in [[gmsh_msh1_element_type:node_number_list]]
        integer, intent(in) :: location

        type(gmsh_msh1_node_number_type) :: node_number



        if (location .lt. minval_location) then

            call initialize_gmsh_msh1_number(node_number)

        else if ( output_number_of_nodes(element) .lt. location ) then

            call initialize_gmsh_msh1_number(node_number)

        else

            node_number = element%node_number_list(location)

        end if

    end function output_node_number_gmsh_msh1_element



    !> version: experimental
    !> |DescOutputNodeNumber|
    elemental function output_node_number_gmsh_msh1_node(node) result(node_number)

        type(gmsh_msh1_node_type), intent(in) :: node

        type(gmsh_msh1_node_number_type) :: node_number



        node_number = node%node_number

    end function output_node_number_gmsh_msh1_node



    !> version: experimental
    !> |DescOutputNodeNumberList|
    pure function output_node_number_list_gmsh_msh1_element(element) result(node_number_list)

        type(gmsh_msh1_element_type), intent(in) :: element

        type(gmsh_msh1_node_number_type), dimension( output_number_of_nodes(element) ) :: node_number_list



        node_number_list(:) = element%node_number_list(:)

    end function output_node_number_list_gmsh_msh1_element



    !> version: experimental
    !> |DescOutputNumberOfElements|
    elemental function output_number_of_elements_gmsh_msh1_file(mesh_data) result(number_of_elements)

        type(gmsh_msh1_data_type), intent(in) :: mesh_data

        integer :: number_of_elements



        number_of_elements = size( mesh_data%element(:) )

    end function output_number_of_elements_gmsh_msh1_file



    !> version: experimental
    !> |DescOutputNumberOfNodes| in the [[gmsh_msh1_element_type]].
    elemental function output_number_of_nodes_gmsh_msh1_element(element) result(number_of_nodes)

        type(gmsh_msh1_element_type), intent(in) :: element

        integer :: number_of_nodes



        number_of_nodes = size( element%node_number_list(:) )

    end function output_number_of_nodes_gmsh_msh1_element



    !> version: experimental
    !> |DescOutputNumberOfNodes| in the [[gmsh_msh1_data_type]].
    elemental function output_number_of_nodes_gmsh_msh1_file(mesh_data) result(number_of_nodes)

        type(gmsh_msh1_data_type), intent(in) :: mesh_data

        integer :: number_of_nodes



        number_of_nodes = size( mesh_data%node(:) )

    end function output_number_of_nodes_gmsh_msh1_file



    !> version: experimental
    !> |DescOutputRegElem|
    elemental function output_reg_elem_gmsh_msh1_element(element) result(reg_elem)

        type(gmsh_msh1_element_type), intent(in) :: element

        type(gmsh_msh1_reg_elem_type) :: reg_elem



        reg_elem = element%reg_elem

    end function output_reg_elem_gmsh_msh1_element



    !> version: experimental
    !> |DescOutputRegPhys|
    elemental function output_reg_phys_gmsh_msh1_element(element) result(reg_phys)

        type(gmsh_msh1_element_type), intent(in) :: element

        type(gmsh_msh1_reg_phys_type) :: reg_phys



        reg_phys = element%reg_phys

    end function output_reg_phys_gmsh_msh1_element



    !> version: experimental
    !> |DescOutputXCoord|
    elemental function output_x_coord_gmsh_msh1_node(node) result(x_coord)

        type(gmsh_msh1_node_type), intent(in) :: node

        real(real64) :: x_coord



        x_coord = node%x_coord

    end function output_x_coord_gmsh_msh1_node



    !> version: experimental
    !> |DescOutputYCoord|
    elemental function output_y_coord_gmsh_msh1_node(node) result(y_coord)

        type(gmsh_msh1_node_type), intent(in) :: node

        real(real64) :: y_coord



        y_coord = node%y_coord

    end function output_y_coord_gmsh_msh1_node



    !> version: experimental
    !> |DescOutputZCoord|
    elemental function output_z_coord_gmsh_msh1_node(node) result(z_coord)

        type(gmsh_msh1_node_type), intent(in) :: node

        real(real64) :: z_coord



        z_coord = node%z_coord

    end function output_z_coord_gmsh_msh1_node



    !> version: experimental
    !> |DescValidate|
    !> @note
    !> This function does **NOT** verify that referenced [[gmsh_msh1_node_type]] exist in the mesh.
    !> @endnote
    elemental function validate_gmsh_msh1_element_without_mesh_data(element) result(is_valid)

        type(gmsh_msh1_element_type), intent(in) :: element

        logical :: is_valid



        is_valid = validate(element%elm_number)

        if (.not. is_valid) return



        is_valid = validate(element%reg_elem)

        if (.not. is_valid) return



        is_valid = validate(element%reg_phys)

        if (.not. is_valid) return



        is_valid = allocated(element%node_number_list)

        if (.not. is_valid) return



        is_valid = all( validate( element%node_number_list(:) ) )

    end function validate_gmsh_msh1_element_without_mesh_data



    !> version: experimental
    !> |DescValidate|
    elemental function validate_gmsh_msh1_file(mesh_data) result(is_valid)

        type(gmsh_msh1_data_type), intent(in) :: mesh_data

        logical :: is_valid



        logical :: is_invalid



        is_invalid = &!
            &              is_stat_failure   ( mesh_data%status )   &!
            & .or.         is_iostat_failure ( mesh_data%status )   &!
            & .or. ( .not. all_flag          ( mesh_data        ) )

        is_valid = .not. is_invalid

    end function validate_gmsh_msh1_file



    !> version: experimental
    !> |DescValidate|
    !> @note
    !> Verification by ieee_is_finite is not required by the gmsh documentation.
    !> @endnote
    elemental function validate_gmsh_msh1_node(node) result(is_valid)

        type(gmsh_msh1_node_type), intent(in) :: node

        logical :: is_valid



        is_valid = validate(node%node_number)

        if ( .not. is_valid ) return

        is_valid =     ieee_is_finite(node%x_coord) &!
        &        .and. ieee_is_finite(node%y_coord) &!
        &        .and. ieee_is_finite(node%z_coord)

    end function validate_gmsh_msh1_node



    !> version: experimental
    !> |DescValidate|
    !> @warning
    !> |WarnGmshMsh1NumberType|
    !> @endwarning
    elemental function validate_gmsh_msh1_number(number) result(is_valid)

        class(gmsh_msh1_number_type), intent(in) :: number

        logical :: is_valid



        is_valid = number%number .gt. 0

    end function validate_gmsh_msh1_number



    !> version: experimental
    !> |DescValidate|
    !> @warning
    !> |WarnGmshMsh1RegElem|
    !> @endwarning
    elemental function validate_gmsh_msh1_reg_elem(reg_elem) result(is_valid)

        type(gmsh_msh1_reg_elem_type), intent(in) :: reg_elem

        logical :: is_valid



        is_valid = reg_elem%expression .gt. 0

    end function validate_gmsh_msh1_reg_elem



    !> version: experimental
    !> |DescValidate|
    !>
    !> @warning
    !> |WarnGmshMsh1RegPhys|
    !> @endwarning
    !>
    !> @note
    !> |NoteGmshMsh1RegPhys|
    !> @endnote
    elemental function validate_gmsh_msh1_reg_phys(reg_phys) result(is_valid)

        type(gmsh_msh1_reg_phys_type), intent(in) :: reg_phys

        logical :: is_valid



        is_valid = reg_phys%expression .ge. 0

    end function validate_gmsh_msh1_reg_phys



    !> version: experimental
    subroutine clear_msg(msg)

        character(len = msg_len), intent(inout) :: msg

        msg(:) = repeat( ' ' , msg_len )

    end subroutine clear_msg



    !> version: experimental
    subroutine initialize_gmsh_msh1_element(element, stat, errmsg)

        type(gmsh_msh1_element_type), intent(inout) :: element

        integer, intent(out) :: stat

        character(len=*), intent(inout) :: errmsg



        call initialize_gmsh_msh1_number(element%elm_number)

        element%elm_type%expression = 0
        element%reg_elem%expression = 0
        element%reg_phys%expression = 0

        if ( allocated(element%node_number_list) ) then

            deallocate( &!
            element%node_number_list , &!
            stat   = stat            , &!
            errmsg = errmsg(:)         &!
            )

        end if

    end subroutine initialize_gmsh_msh1_element



    !> version: experimental
    elemental subroutine initialize_gmsh_msh1_node(node)

        type(gmsh_msh1_node_type), intent(out) :: node



        call initialize_gmsh_msh1_number(node%node_number)

        node%x_coord = ieee_value( node%x_coord, ieee_signaling_nan )
        node%y_coord =             node%x_coord
        node%z_coord =             node%x_coord

    end subroutine initialize_gmsh_msh1_node



    !> version: experimental
    elemental subroutine initialize_gmsh_msh1_number(number)

        class(gmsh_msh1_number_type), intent(inout) :: number



        number%number = 0

    end subroutine initialize_gmsh_msh1_number



    !> version: experimental
    !> |DescLookupElement|
    !> @warning
    !> If no element corresponding to the [[lookup_element_by_loc_gmsh_msh1_file:location]] argument exists,
    !> a element initialized by [[initialize_gmsh_msh1_element]] will be returned.
    subroutine lookup_element_by_loc_gmsh_msh1_file(mesh_data, location, element, stat, errmsg)

        type(gmsh_msh1_data_type), intent(in) :: mesh_data

        !> location in [[gmsh_msh1_data_type:element]]
        integer, intent(in) :: location

        type(gmsh_msh1_element_type), intent(inout) :: element

        integer, intent(out) :: stat

        character(len=*), intent(inout) :: errmsg



        if (location .lt. minval_location) then

            call initialize_gmsh_msh1_element( element, stat, errmsg(:) )

        else if ( output_number_of_elements(mesh_data) .lt. location ) then

            call initialize_gmsh_msh1_element( element, stat, errmsg(:) )

        else

            element = mesh_data%element(location)

        end if

    end subroutine lookup_element_by_loc_gmsh_msh1_file



    !> version: experimental
    subroutine read_gmsh_msh1_element(file_unit, itr_element, text_line, element, status, flag)

        integer, intent(in) :: file_unit

        integer, intent(in) :: itr_element

        character(len=*), intent(inout) :: text_line

        type(gmsh_msh1_element_type), intent(inout) :: element

        type(gmsh_msh1_status_type), intent(inout) :: status

        logical, intent(out) :: flag



        !> the number of nodes for the *n*-th element.
        integer :: number_of_nodes



        call initialize_gmsh_msh1_element(&!
        element = element           , &!
        stat    = status%err%code   , &!
        errmsg  = status%err%msg(:)   &!
        )

        if ( is_stat_failure(status) ) then

            write( &!
            unit = status%err%msg(:) , &!
            fmt  = '(A,I0,*(1X,A))'    &!
            ) &!
            'Failed to deallocate No.' , &!
            itr_element                , &!
            '`node_number_list`:'      , &!
            trim( status%io%msg(:) )

            return

        end if



        read( &!
        unit   = file_unit        , &!
        fmt    = '(A)'            , &!
        iostat = status%io%code   , &!
        iomsg  = status%io%msg(:)   &!
        ) &!
        text_line(:)

        if ( is_iostat_failure(status) ) then

            status%err%code = status%io%code

            write( &!
            unit = status%err%msg(:) , &!
            fmt  = '(A,I0,*(1X,A))'    &!
            ) &!
            'Failed to read No.'     , &!
            itr_element              , &!
            'element as string:'     , &!
            trim( status%io%msg(:) )

            call clear_msg(status%io%msg)

            return

        end if



        read( &!
        unit   = text_line(:)     , &!
        fmt    = *                , &!
        iostat = status%io%code   , &!
        iomsg  = status%io%msg(:)   &!
        ) &!
        element%elm_number      , &!
        element%elm_type        , &!
        element%reg_phys        , &!
        element%reg_elem        , &!
                number_of_nodes

        if ( is_iostat_failure(status) ) then

            status%err%code = status%io%code

            write( &!
            unit = status%err%msg(:) , &!
            fmt  = '(A,I0,*(1X,A))'    &!
            ) &!
            'Failed to read No.'     , &!
            itr_element              , &!
            'element''s'             , &!
            '`elm_number`, '         , &!
            '`elm_type`,'            , &!
            '`reg_phys`'             , &!
            '`reg_elem`'             , &!
            'or'                     , &!
            '`number_of_nodes`:'     , &!
            trim( status%io%msg(:) )

            call clear_msg(status%io%msg)

            return

        end if



        flag = ( number_of_nodes .ge. 0 )

        if ( .not. flag ) then

            write( &!
            unit = status%err%msg(:)      , &!
            fmt  = '(A,I0,2(1X,A),1X,I0)'   &!
            ) &!
            'Failed to read No.'                      , &!
            itr_element                               , &!
            'element: `number_of_nodes` is negative:' , &!
            'read value:'                             , &!
            number_of_nodes

            return

        end if



        allocate( &!
        element%node_number_list(number_of_nodes) , &!
        stat   = status%err%code                  , &!
        errmsg = status%err%msg(:)                  &!
        )

        if ( is_stat_failure(status) ) then

            write( &!
            unit = status%err%msg(:) , &!
            fmt  = '(A,I0,*(1X,A))'    &!
            ) &!
            'Failed to deallocate No.' , &!
            itr_element                , &!
            '`node_number_list`:'      , &!
            trim( status%io%msg(:) )

            return

        end if



        read( &!
        unit   = text_line(:)     , &!
        fmt    = *                , &!
        iostat = status%io%code   , &!
        iomsg  = status%io%msg(:)   &!
        ) &!
        element%elm_number          , &!
        element%elm_type            , &!
        element%reg_phys            , &!
        element%reg_elem            , &!
                number_of_nodes     , &!
        element%node_number_list(:)

        if ( is_iostat_failure(status) ) then

            status%err%code = status%io%code

            write( &!
            unit = status%err%msg(:) , &!
            fmt  = '(A,I0,*(1X,A))'    &!
            ) &!
            'Failed to read No.'     , &!
            itr_element              , &!
            'element''s'             , &!
            '`node_number_list`:'    , &!
            trim( status%io%msg(:) )

            call clear_msg(status%io%msg)

            return

        end if

    end subroutine read_gmsh_msh1_element



    !> version: experimental
    subroutine read_gmsh_msh1_file(mesh_data, msh1_file)

        !> The read data will be stored in this argument
        type(gmsh_msh1_data_type), intent(inout) :: mesh_data

        !> File path to read from
        character(len=*), intent(in) :: msh1_file



        integer :: file_unit



        mesh_data%flag_nod_section_header    = initial_flag_nod_section_header
        mesh_data%flag_number_of_nodes       = initial_flag_number_of_nodes
        mesh_data%flag_deallocation_nodes    = initial_flag_deallocation_nodes
        mesh_data%flag_allocation_nodes      = initial_flag_allocation_nodes
        mesh_data%flag_nod_section_footer    = initial_flag_nod_section_footer

        mesh_data%flag_elm_section_header    = initial_flag_elm_section_header
        mesh_data%flag_number_of_elements    = initial_flag_number_of_elements
        mesh_data%flag_deallocation_elements = initial_flag_deallocation_elements
        mesh_data%flag_allocation_elements   = initial_flag_allocation_elements
        mesh_data%flag_reading_elements      = initial_flag_reading_elements
        mesh_data%flag_elm_section_footer    = initial_flag_elm_section_footer

        call clear_msg( mesh_data%status% err %msg(:) )
        call clear_msg( mesh_data%status% io  %msg(:) )



        open( &!
        newunit = file_unit                  , &!
        file    = msh1_file(:)               , &!
        action  = 'read'                     , &!
        form    = 'formatted'                , &!
        status  = 'old'                      , &!
        iostat  = mesh_data%status%io%code   , &!
        iomsg   = mesh_data%status%io%msg(:)   &!
        )

        if ( is_iostat_failure(mesh_data%status) ) return



        call read_gmsh_msh1_file_kernel(mesh_data, file_unit)



        close( &!
        unit   = file_unit                  , &!
        iostat = mesh_data%status%io%code   , &!
        iomsg  = mesh_data%status%io%msg(:)   &!
        )



        mesh_data%status%err%code = stat_success

    end subroutine read_gmsh_msh1_file



    !> version: experimental
    subroutine read_gmsh_msh1_file_kernel(mesh_data, file_unit)

        !> The read data will be stored in this argument
        type(gmsh_msh1_data_type), intent(inout) :: mesh_data

        integer, intent(in) :: file_unit



        !> the number of elements in the mesh
        integer :: number_of_elements

        !> the number of nodes in the mesh
        integer :: number_of_nodes

        !> version: experimental
        !> A string for reading a line of text.<br>
        !> The length of this string is 2048,<br>
        !> which is a provisional value.
        character(len=2048) :: text_line



        read_nod_section_header: &!
        do

            call read_gmsh_msh1_header_footer( &!
            file_unit     = file_unit                            , &!
            header_footer =                nod_section_header(:) , &!
            text_line     = text_line(:)                         , &!
            status        = mesh_data%status                     , &!
            flag          = mesh_data%flag_nod_section_header      &!
            )

            if ( mesh_data%flag_nod_section_header   ) exit
            if ( is_iostat_success(mesh_data%status) ) cycle

            return

        end do &!
        read_nod_section_header



        read_number_of_nodes: &!
        block

            call read_gmsh_msh1_number_of_items( &!
            file_unit       = file_unit                      , &!
            item_name       = 'number_of_nodes'              , &!
            text_line       = text_line(:)                   , &!
            number_of_items = number_of_nodes                , &!
            status          = mesh_data%status               , &!
            flag            = mesh_data%flag_number_of_nodes   &!
            )

            if ( .not. mesh_data%flag_number_of_nodes ) return

        end block &!
        read_number_of_nodes



        read_nodes: &!
        block

            integer :: itr_node



            if ( allocated(mesh_data%node) ) then

                deallocate(&!
                mesh_data%node                       , &!
                stat   = mesh_data%status%err%code   , &!
                errmsg = mesh_data%status%err%msg(:)   &!
                )

                if ( is_stat_failure(mesh_data%status) ) return

            end if

            mesh_data%flag_deallocation_nodes = .true.



            allocate( &!
            mesh_data%node(number_of_nodes)      , &!
            stat   = mesh_data%status%err%code   , &!
            errmsg = mesh_data%status%err%msg(:)   &!
            )

            if ( is_stat_failure(mesh_data%status) ) return

            mesh_data%flag_allocation_nodes = .true.

            call initialize_gmsh_msh1_node( mesh_data%node(:) )


            do itr_node = 1, number_of_nodes

                call read_gmsh_msh1_node( &!
                file_unit = file_unit                , &!
                itr_node  =                itr_node  , &!
                node      = mesh_data%node(itr_node) , &!
                status    = mesh_data%status           &!
                )

                if ( is_iostat_failure(mesh_data%status) ) return

            end do

            mesh_data%flag_reading_nodes = .true.

        end block &!
        read_nodes



        read_nod_section_footer: &!
        block

            call read_gmsh_msh1_header_footer( &!
            file_unit     = file_unit                            , &!
            header_footer =                nod_section_footer(:) , &!
            text_line     = text_line(:)                         , &!
            status        = mesh_data%status                     , &!
            flag          = mesh_data%flag_nod_section_footer      &!
            )

            if ( .not. mesh_data%flag_nod_section_footer ) return

        end block &!
        read_nod_section_footer



        read_elm_section_header: &!
        do

            call read_gmsh_msh1_header_footer( &!
            file_unit     = file_unit                            , &!
            header_footer =                elm_section_header(:) , &!
            text_line     = text_line(:)                         , &!
            status        = mesh_data%status                     , &!
            flag          = mesh_data%flag_elm_section_header      &!
            )

            if ( mesh_data%flag_elm_section_header   ) exit
            if ( is_iostat_success(mesh_data%status) ) cycle

            return

        end do &!
        read_elm_section_header



        read_number_of_elements: &!
        block

            call read_gmsh_msh1_number_of_items( &!
            file_unit       = file_unit                         , &!
            item_name       = 'number_of_elements'              , &!
            text_line       = text_line(:)                      , &!
            status          = mesh_data%status                  , &!
            number_of_items = number_of_elements                , &!
            flag            = mesh_data%flag_number_of_elements   &!
            )

            if ( .not. mesh_data%flag_number_of_elements ) return

        end block &!
        read_number_of_elements



        read_elements: &!
        block

            integer :: itr_element



            if ( allocated(mesh_data%element) ) then

                deallocate(&!
                mesh_data%element                    , &!
                stat   = mesh_data%status%err%code   , &!
                errmsg = mesh_data%status%err%msg(:)   &!
                )

                if ( is_stat_failure(mesh_data%status) ) return

            end if

            mesh_data%flag_deallocation_elements = .true.



            allocate( &!
            mesh_data%element(number_of_elements) , &!
            stat   = mesh_data%status%err%code    , &!
            errmsg = mesh_data%status%err%msg(:)    &!
            )

            if ( is_stat_failure(mesh_data%status) ) return

            mesh_data%flag_allocation_elements = .true.



            if ( number_of_elements .gt. 0 ) then

                do itr_element = 1, number_of_elements

                    call read_gmsh_msh1_element( &!
                    file_unit   = file_unit                       , &!
                    itr_element =                   itr_element   , &!
                    text_line   = text_line(:)                    , &!
                    element     = mesh_data%element(itr_element)  , &!
                    status      = mesh_data%status                , &!
                    flag        = mesh_data%flag_reading_elements   &!
                    )

                    if ( is_iostat_failure (mesh_data%status)  ) return
                    if ( is_stat_failure   (mesh_data%status)  ) return
                    if ( .not. mesh_data%flag_reading_elements ) return

                end do

            else

                mesh_data%flag_reading_elements = .true.

            end if

        end block &!
        read_elements



        read_elm_section_footer: &!
        block

            call read_gmsh_msh1_header_footer( &!
            file_unit     = file_unit                            , &!
            header_footer =                elm_section_footer(:) , &!
            text_line     = text_line(:)                         , &!
            status        = mesh_data%status                     , &!
            flag          = mesh_data%flag_elm_section_footer      &!
            )

        end block &!
        read_elm_section_footer

    end subroutine read_gmsh_msh1_file_kernel



    !> version: experimental
    subroutine read_gmsh_msh1_header_footer(file_unit, header_footer, text_line, status, flag)

        integer, intent(in) :: file_unit

        character(len=*), intent(in) :: header_footer

        character(len=*), intent(inout) :: text_line

        type(gmsh_msh1_status_type), intent(inout) :: status

        logical, intent(out) :: flag



        read( &!
        unit   = file_unit        , &!
        fmt    = '(A)'            , &!
        iostat = status%io%code   , &!
        iomsg  = status%io%msg(:)   &!
        ) &!
        text_line(:)



        select case(status%io%code)

            case(iostat_success) 

                flag = trim( text_line(:) ) .eq. header_footer(:)

                if (flag) then
                    status%err%msg(:) = ' '
                else
                    status%err%msg(:) = 'The read text line is not `' // header_footer(:) // '`.'
                end if

            case(iostat_end)

                status%err%msg(:) = 'Failed to found the `' // header_footer(:) // '`'

            case default

                status%err%msg(:) = 'Failed to read the `' // header_footer(:) // '`'

        end select

    end subroutine read_gmsh_msh1_header_footer



    !> version: experimental
    subroutine read_gmsh_msh1_node(file_unit, itr_node, node, status)

        integer, intent(in) :: file_unit

        integer, intent(in) :: itr_node

        type(gmsh_msh1_node_type), intent(out) :: node

        type(gmsh_msh1_status_type), intent(inout) :: status



        read( &!
        unit   = file_unit        , &!
        fmt    = *                , &!
        iostat = status%io%code   , &!
        iomsg  = status%io%msg(:)   &!
        ) &!
        node%node_number , &!
        node%x_coord     , &!
        node%y_coord     , &!
        node%z_coord

        if ( is_iostat_failure(status) ) then

            status%err%code = status%io%code

            write( &!
            unit = status%err%msg(:) , &!
            fmt  = '(A,I0,*(1X,A))'    &!
            ) &!
            'Failed to read No.'     , &!
            itr_node                 , &!
            'node:'                  , &!
            trim( status%io%msg(:) )

            call clear_msg(status%io%msg)

        end if

    end subroutine read_gmsh_msh1_node



    !> version: experimental
    subroutine read_gmsh_msh1_number_of_items(file_unit, item_name, text_line, number_of_items, status, flag)

        integer, intent(in) :: file_unit

        character(len=*), intent(in) :: item_name

        character(len=*), intent(inout) :: text_line

        integer, intent(out) :: number_of_items

        type(gmsh_msh1_status_type), intent(inout) :: status

        logical, intent(out) :: flag



        character(1) :: dummy



        read( &!
        unit   = file_unit        , &!
        fmt    = '(A)'            , &!
        iostat = status%io%code   , &!
        iomsg  = status%io%msg(:)   &!
        ) &!
        text_line(:)

        if ( is_iostat_failure(status) ) then

            status%err%code   = status%io%code
            status%err%msg(:) = status%io%msg(:)

            return

        end if



        read( &!
        unit   = text_line(:)     , &!
        fmt    = *                , &!
        iostat = status%io%code   , &!
        iomsg  = status%io%msg(:)   &!
        ) &!
        number_of_items, dummy

        if ( is_iostat_success(status) ) then

            write( &!
            unit = status%err%msg(:) , &!
            fmt  = '(3A)'              &!
            ) &!
            'Extra data was detected while reading `' , &!
            item_name                                 , &!
            '`.'

            return

        end if



        read( &!
        unit   = text_line(:)     , &!
        fmt    = *                , &!
        iostat = status%io%code   , &!
        iomsg  = status%io%msg(:)   &!
        ) &!
        number_of_items

        if ( is_iostat_failure(status) ) then

            status%err%code   = status%io%code
            status%err%msg(:) = status%io%msg(:)

            return

        end if

        if (number_of_items .lt. 0) then

            write( &!
            unit = status%err%msg(:)   , &!
            fmt  = '(2(A,1X),I0,1X,A)'   &!
            ) &!
            'The number of'            , &!
            item_name                  , &!
            number_of_items            , &!
            'must be greater than -1.'

            return

        end if



        flag = .true.

    end subroutine read_gmsh_msh1_number_of_items



    !> version: experimental
    subroutine write_stat_msg_gmsh_msh1_file(mesh_data, write_unit)

        type(gmsh_msh1_data_type), intent(in) :: mesh_data

        integer, intent(in) :: write_unit



        write( write_unit, '(L1,6X,":",1X,A)' ) &!
            mesh_data%flag_nod_section_header    , &!
                           nod_section_header(:)

        write( write_unit, '(L1,6X,":",1X,A)' ) &!
            mesh_data%flag_number_of_nodes  , &!
                          'number_of_nodes'

        write( write_unit, '(L1,6X,":",1X,A)' ) &!
            mesh_data%flag_deallocation_nodes  , &!
                          'deallocation_nodes'

        write( write_unit, '(L1,6X,":",1X,A)' ) &!
            mesh_data%flag_allocation_nodes  , &!
                          'allocation_nodes'

        write( write_unit, '(L1,6X,":",1X,A)' ) &!
            mesh_data%flag_reading_nodes  , &!
                          'reading_nodes'

        write( write_unit, '(L1,6X,":",1X,A)' ) &!
            mesh_data%flag_nod_section_footer    , &!
                           nod_section_footer(:)

        write( write_unit, '(L1,6X,":",1X,A)' ) &!
            mesh_data%flag_elm_section_header    , &!
                           elm_section_header(:)

        write( write_unit, '(L1,6X,":",1X,A)' ) &!
            mesh_data%flag_number_of_elements  , &!
                          'number_of_elements'

        write( write_unit, '(L1,6X,":",1X,A)' ) &!
            mesh_data%flag_deallocation_elements  , &!
                          'deallocation_elements'

        write( write_unit, '(L1,6X,":",1X,A)' ) &!
            mesh_data%flag_allocation_elements  , &!
                          'allocation_elements'

        write( write_unit, '(L1,6X,":",1X,A)' ) &!
            mesh_data%flag_reading_elements  , &!
                          'reading_elements'

        write( write_unit, '(L1,6X,":",1X,A)' ) &!
            mesh_data%flag_elm_section_footer    , &!
                           elm_section_footer(:)

        write( write_unit, "(A,I0)" ) "iostat : ",       mesh_data%status% io  %code
        write( write_unit, "(A,A )" ) "iomsg  : ", trim( mesh_data%status% io  %msg(:) )
        write( write_unit, "(A,I0)" ) "stat   : ",       mesh_data%status% err %code
        write( write_unit, "(A,A )" ) "errmsg : ", trim( mesh_data%status% err %msg(:) )

    end subroutine  write_stat_msg_gmsh_msh1_file

end module gmsh_msh1_reader
