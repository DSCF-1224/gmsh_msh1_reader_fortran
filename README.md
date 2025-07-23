# gmsh_msh1_reader_fortran
Fortran module for reading [Gmsh][GmshReferenceManualTop] [MSH file format version 1][GmshReferenceManualMsh1]

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

## Documentation

Documentation is available at [https://dscf-1224.github.io/gmsh_msh1_reader_fortran/](https://dscf-1224.github.io/gmsh_msh1_reader_fortran/).

[FpmGitHubRepository]: https://github.com/fortran-lang/fpm
[GmshReferenceManualTop]: https://gmsh.info/doc/texinfo/gmsh.html
[GmshReferenceManualMsh1]: https://gmsh.info/doc/texinfo/gmsh.html#MSH-file-format-version-1-_0028Legacy_0029
[ThisModule]: src/gmsh_msh1_reader.f90
