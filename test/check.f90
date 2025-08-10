program check

    use, intrinsic :: iso_fortran_env , &!
        only: compiler_options , &!
        &     compiler_version , &!
        &     error_unit       , &!
        &     iostat_end       , &!
        &     output_unit       
        
    use, non_intrinsic :: gmsh_msh1_reader



    implicit none



    print *, compiler_version()
    print *, compiler_options()



    call test_read_gmsh_msh1_file( "test/empty.msh1" )

    call test_read_gmsh_msh1_file( "test/2d_tri_order01.msh1" )
    call test_read_gmsh_msh1_file( "test/2d_tri_order02.msh1" )
    call test_read_gmsh_msh1_file( "test/2d_tri_order03.msh1" )
    call test_read_gmsh_msh1_file( "test/2d_tri_order04.msh1" )
    call test_read_gmsh_msh1_file( "test/2d_tri_order05.msh1" )
    call test_read_gmsh_msh1_file( "test/2d_tri_order06.msh1" )
    call test_read_gmsh_msh1_file( "test/2d_tri_order07.msh1" )
    call test_read_gmsh_msh1_file( "test/2d_tri_order08.msh1" )
    call test_read_gmsh_msh1_file( "test/2d_tri_order09.msh1" )
    call test_read_gmsh_msh1_file( "test/2d_tri_order10.msh1" )

    call test_read_gmsh_msh1_file( "test/2d_qua_order01.msh1" )
    call test_read_gmsh_msh1_file( "test/2d_qua_order02.msh1" )
    call test_read_gmsh_msh1_file( "test/2d_qua_order03.msh1" )
    call test_read_gmsh_msh1_file( "test/2d_qua_order04.msh1" )
    call test_read_gmsh_msh1_file( "test/2d_qua_order05.msh1" )
    call test_read_gmsh_msh1_file( "test/2d_qua_order06.msh1" )
    call test_read_gmsh_msh1_file( "test/2d_qua_order07.msh1" )
    call test_read_gmsh_msh1_file( "test/2d_qua_order08.msh1" )
    call test_read_gmsh_msh1_file( "test/2d_qua_order09.msh1" )
    call test_read_gmsh_msh1_file( "test/2d_qua_order10.msh1" )

    call test_read_gmsh_msh1_file( "test/3d_tet_order01.msh1" )
    call test_read_gmsh_msh1_file( "test/3d_tet_order02.msh1" )
    call test_read_gmsh_msh1_file( "test/3d_tet_order03.msh1" )
    call test_read_gmsh_msh1_file( "test/3d_tet_order04.msh1" )
    call test_read_gmsh_msh1_file( "test/3d_tet_order05.msh1" )
    call test_read_gmsh_msh1_file( "test/3d_tet_order06.msh1" )
    call test_read_gmsh_msh1_file( "test/3d_tet_order07.msh1" )
    call test_read_gmsh_msh1_file( "test/3d_tet_order08.msh1" )
    call test_read_gmsh_msh1_file( "test/3d_tet_order09.msh1" )
    call test_read_gmsh_msh1_file( "test/3d_tet_order10.msh1" )



    contains



    function compare_files_binary(file1, file2) result(match)

        character(len=*), intent(in) :: file1, file2

        logical :: match



        integer, parameter :: buffer_size = 1024



        integer :: iostat1, iostat2

        integer :: read_unit1, read_unit2

        character(len=1) :: buffer1(buffer_size), buffer2(buffer_size)



        match = .false.



        if ( .not. compare_files_size( file1(:), file2(:) ) ) return

        match = .true.

        return



        open(&!
        newunit = read_unit1    , &!
        file    = file1(:)      , &!
        access  = 'stream'      , &!
        form    = 'unformatted' , &!
        status  = 'old'         , &!
        iostat  = iostat1         &!
        )

        open(&!
        newunit = read_unit2    , &!
        file    = file2(:)      , &!
        access  = 'stream'      , &!
        form    = 'unformatted' , &!
        status  = 'old'         , &!
        iostat  = iostat2         &!
        )

        if ( iostat1 .ne. 0 ) return
        if ( iostat2 .ne. 0 ) return



        do

            read( unit = read_unit1, iostat = iostat1 ) buffer1(:)
            read( unit = read_unit2, iostat = iostat2 ) buffer2(:)

            if (      iostat1    .ne. iostat2      ) exit
            if ( any( buffer1(:) .ne. buffer2(:) ) ) exit

            if ( iostat1 .eq. iostat_end ) then

                match = .true.

                exit

            end if

        end do



        close(read_unit1)
        close(read_unit2)

    end function compare_files_binary



    function compare_files_size(file1, file2) result(match_size)

        character(len=*), intent(in) :: file1, file2

        logical :: match_size



        logical :: exists1 , exists2

        integer :: size1   , size2



        inquire( file = file1(:), exist = exists1, size = size1 )
        inquire( file = file2(:), exist = exists2, size = size2 )

        match_size = exists1 .and. exists2 .and. (size1 .eq. size2)

    end function compare_files_size



    subroutine compare_files(file1, file2)

        character(len=*), intent(in) :: file1, file2



        if ( compare_files_binary( file1(:) , file2(:) ) ) then

            write( output_unit, "(A,*(1X,A))" ) &!
                "Files"          , &!
                file1(:) , "and" , &!
                file2(:)         , &!
                "are identical"

        else

            write( error_unit, "(A,*(1X,A))" ) &!
                "Files"          , &!
                file1(:) , "and" , &!
                file2(:)         , &!
                "differ"

            error stop

        end if
    
    end subroutine



    subroutine handle_io_error(iostat, iomsg, msh1_file)

        integer, intent(in) :: iostat

        character(len=*), intent(in) :: iomsg, msh1_file



        if ( iostat .eq. 0 ) return

        write( error_unit, "(A)"    ) msh1_file(:)
        write( error_unit, "(A,I0)" ) "iostat : " , iostat
        write( error_unit, "(A,A)"  ) "iomsg  : " , iomsg(:)

        error stop

    end subroutine handle_io_error



    subroutine test_read_gmsh_msh1_file(msh1_file)

        character(len=*), intent(in) :: msh1_file



        integer :: cmdstat, exitstat

        character(len=256) :: command, read_msh1_file, rewrite_msh1_file, cmdmsg

        type(gmsh_msh1_data_type) :: msh1_data



        cmdmsg(:) = ' '



        call read_gmsh_msh1_file( msh1_data, msh1_file )

        if ( is_read_successful(msh1_data) ) then

            call test_read_gmsh_msh1_file_kernel( &!
            unit      = output_unit  , &!
            msh1_file = msh1_file(:) , &!
            msh1_data = msh1_data      &!
            )

        else

            call test_read_gmsh_msh1_file_kernel( &!
            unit      = error_unit   , &!
            msh1_file = msh1_file(:) , &!
            msh1_data = msh1_data      &!
            )

            error stop

        end if



        if ( .not. validate(msh1_data) ) then

            write( error_unit, "(A,1X,A)" ) "INVALID:", msh1_file(:)

            error stop

        end if



        write( read_msh1_file(:), "(*(A))" ) "test/", "read_", msh1_file(6:)

        call write_gmsh_msh1_file( trim( read_msh1_file(:) ), msh1_data )



        write( rewrite_msh1_file(:), "(*(A))" ) &!
            "test/"       , &!
            "rewrite_"    , &!
            msh1_file(6:)

        write( command(:), "(A,*(1X,A))" ) &!
            "gmsh"                       , &!
            "-parse_and_exit"            , &!
            trim( read_msh1_file(:) )    , &!
            "-o"                         , &!
            trim( rewrite_msh1_file(:) ) , &!
            "-save"

        call execute_command_line( &!
        command  = command(:) , &!
        exitstat = exitstat   , &!
        cmdstat  = cmdstat    , &!
        cmdmsg   = cmdmsg(:)    &!
        )

        if ( (exitstat .ne. 0) .or. (cmdstat .ne. 0) ) then

            write( error_unit, "(A)"    ) trim(    read_msh1_file(:) )
            write( error_unit, "(A)"    ) trim( rewrite_msh1_file(:) )
            write( error_unit, "(A,I0)" ) "exitstat : " , exitstat
            write( error_unit, "(A,I0)" ) "cmdstat  : " , cmdstat
            write( error_unit, "(A,A)"  ) "cmdmsg   : " , trim( cmdmsg(:) )

            error stop

        end if



        call compare_files( msh1_file(:) , trim( rewrite_msh1_file(:) ) )

    end subroutine test_read_gmsh_msh1_file



    subroutine test_read_gmsh_msh1_file_kernel(unit, msh1_file, msh1_data)

        integer, intent(in) :: unit

        character(len=*), intent(in) :: msh1_file

        type(gmsh_msh1_data_type), intent(in) :: msh1_data



        write( unit, * ) new_line(""), msh1_file(:)

        call write_stat_msg_gmsh_msh1_file( msh1_data, unit )

    end subroutine test_read_gmsh_msh1_file_kernel



    subroutine write_gmsh_msh1_file(msh1_file, msh1_data)

        character(len=*), intent(in) :: msh1_file

        type(gmsh_msh1_data_type), intent(in) :: msh1_data



        logical :: flag_error_stop

        integer :: stat

        integer :: write_unit

        character(len=256) :: msg



        flag_error_stop = .false.



        open( &!
        newunit = write_unit   , &!
        file    = msh1_file(:) , &!
        action  = "write"      , &!
        form    = "formatted"  , &!
        iostat  = stat         , &!
        iomsg   = msg(:)         &!
        )

        call handle_io_error( stat, msg(:), msh1_file(:) )



        call write_gmsh_msh1_file_kernel( &!
        write_unit = write_unit   , &!
        msh1_file  = msh1_file(:) , &!
        msh1_data  = msh1_data    , &!
        stat       = stat         , &!
        msg        = msg(:)         &!
        )

        flag_error_stop = (stat .ne. 0)



        close( &!
        unit   = write_unit , &!
        iostat = stat       , &!
        iomsg  = msg(:)       &!
        )

        call handle_io_error( stat, msg(:), msh1_file(:) )

        if (flag_error_stop) error stop

    end subroutine write_gmsh_msh1_file



    subroutine write_gmsh_msh1_file_kernel(write_unit, msh1_file, msh1_data, stat, msg)

        integer, intent(in) :: write_unit

        character(len=*), intent(in) :: msh1_file

        type(gmsh_msh1_data_type), intent(in) :: msh1_data

        integer, intent(out) :: stat

        character(len=*), intent(inout) :: msg



        integer :: itr

        type(gmsh_msh1_element_type) :: element

        type(gmsh_msh1_node_type) :: node



        write( write_unit, "(A)" ) "$NOD"

        write( write_unit, "(I0)" ) output_number_of_nodes(msh1_data)

        do itr = 1, output_number_of_nodes(msh1_data)

            node = lookup_node( msh1_data, itr )

            write( write_unit, '(I0,3ES24.16)' ) &!
                export_node_number (node) , &!!
                output_x_coord     (node) , &!!
                output_y_coord     (node) , &!!
                output_z_coord     (node)

        end do

        write( write_unit, "(A)" ) "$ENDNOD"



        write( write_unit, "(A)" ) "$ELM"

        write( write_unit, "(I0)" ) output_number_of_elements(msh1_data)

        do itr = 1, output_number_of_elements(msh1_data)

            call lookup_element( &!
            mesh_data = msh1_data , &!
            location  = itr       , &!
            element   = element   , &!
            stat      = stat      , &!
            errmsg    = msg(:)      &!
            )

            if (stat .ne. 0) then

                write( error_unit, "(A)"    ) msh1_file(:)
                write( error_unit, "(A,I0)" ) "stat   : " , stat
                write( error_unit, "(A,A)"  ) "errmsg : " , msg(:)

                return

            end if

            write( write_unit, '(I0,*(1X,I0))' ) &!
                export_elm_number       (element) , &!
                export_elm_type         (element) , &!
                export_reg_phys         (element) , &!
                export_reg_elem         (element) , &!
                output_number_of_nodes  (element) , &!
                export_node_number_list (element)

        end do

        write( write_unit, "(A)" ) "$ENDELM"

    end subroutine write_gmsh_msh1_file_kernel

end program check
