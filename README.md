# gmsh_msh1_reader_fortran

[![CI](https://github.com/DSCF-1224/gmsh_msh1_reader_fortran/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/DSCF-1224/gmsh_msh1_reader_fortran/actions/workflows/CI.yml)  
[![pages-build-deployment](https://github.com/DSCF-1224/gmsh_msh1_reader_fortran/actions/workflows/pages/pages-build-deployment/badge.svg)](https://github.com/DSCF-1224/gmsh_msh1_reader_fortran/actions/workflows/pages/pages-build-deployment)

Fortran module for reading [Gmsh][GmshReferenceManualTop] [MSH file format version 1][GmshReferenceManualMsh1]

> [!IMPORTANT]
> This library does not support new [Gmsh][GmshReferenceManualTop] MSH format versions.
> - [MSH file format version 2][GmshReferenceManualMsh2]
> - [MSH file format version 4][GmshReferenceManualMsh4]

## Requirements

Support for the following intrinsic modules:

- `iso_fortran_env`
  - `iostat_eor`
  - `iostat_end`
  - `real64`
- `ieee_arithmetic`
  - `ieee_value`
  - `ieee_signaling_nan`
- `iso_c_binding`

## How to use

### Manual Compilation

This library is composed of a single Fortran `module` written in one [`.f90`][ThisModule] file.  
To use the library, include this [source file][ThisModule] in your compilation process.

### Using this `module` with [`fpm`][FpmGitHubRepository]

To use this `module` within your [`fpm`][FpmGitHubRepository]

To use `stdlib` within your `fpm` project, add the following lines to your `fpm.toml` file:
```toml
[dependencies]
gmsh_msh1_reader_fortran = { git="https://github.com/DSCF-1224/gmsh_msh1_reader_fortran" }
```

## How to read [Gmsh][GmshReferenceManualTop] [MSH file format version 1][GmshReferenceManualMsh1]

### Basic Usage

#### 1. Module Import and Variable Declaration

- [`gmsh_msh1_reader` Module](https://dscf-1224.github.io/gmsh_msh1_reader_fortran/module/gmsh_msh1_reader.html)
- [`gmsh_msh1_data_type` Derived Type](https://dscf-1224.github.io/gmsh_msh1_reader_fortran/type/gmsh_msh1_data_type.html)

```fortran
use gmsh_msh1_reader
implicit none

type(gmsh_msh1_data_type) :: mesh_data
```

#### 2. Read [Gmsh][GmshReferenceManualTop] [MSH file format version 1][GmshReferenceManualMsh1]

- [`read_gmsh_msh1_file` Subroutine](https://dscf-1224.github.io/gmsh_msh1_reader_fortran/proc/read_gmsh_msh1_file.html)

```fortran
call read_gmsh_msh1_file(mesh_data, 'your_mesh_file.msh1')
```

> [!NOTE]
> The module does not validate file extensions.


#### 3. Error Checking

- [interface `is_invalid`](https://dscf-1224.github.io/gmsh_msh1_reader_fortran/interface/is_invalid.html)
- [`write_stat_msg_gmsh_msh1_file` Subroutine](https://dscf-1224.github.io/gmsh_msh1_reader_fortran/proc/write_stat_msg_gmsh_msh1_file.html)

```fortran
if ( is_invalid(mesh_data) ) then
    write(error_unit,*) 'Error: Failed to read mesh file'
    call write_stat_msg_gmsh_msh1_file(mesh_data, error_unit)
    error stop
endif
```

## Documentation

Documentation is available at [https://dscf-1224.github.io/gmsh_msh1_reader_fortran/](https://dscf-1224.github.io/gmsh_msh1_reader_fortran/).

[FpmGitHubRepository]: https://github.com/fortran-lang/fpm
[GmshReferenceManualTop]: https://gmsh.info/doc/texinfo/gmsh.html
[GmshReferenceManualMsh1]: https://gmsh.info/doc/texinfo/gmsh.html#MSH-file-format-version-1-_0028Legacy_0029
[GmshReferenceManualMsh2]: https://gmsh.info/doc/texinfo/gmsh.html#MSH-file-format-version-2-_0028Legacy_0029
[GmshReferenceManualMsh4]: https://gmsh.info/doc/texinfo/gmsh.html#MSH-file-format
[ThisModule]: src/gmsh_msh1_reader.f90
