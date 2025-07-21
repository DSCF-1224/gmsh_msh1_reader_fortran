// -----------------------------------------------------------------------------
// How to run
// gmsh -parse_and_exit 2d_tri.geo
// -----------------------------------------------------------------------------
Include "2d.geo";

// This line is enabled in "2d_qua.geo".
// Recombine Surface "*";

Mesh 2;

SetOrder  1; Save "2d_tri_order01.msh1";
SetOrder  2; Save "2d_tri_order02.msh1";
SetOrder  3; Save "2d_tri_order03.msh1";
SetOrder  4; Save "2d_tri_order04.msh1";
SetOrder  5; Save "2d_tri_order05.msh1";
SetOrder  6; Save "2d_tri_order06.msh1";
SetOrder  7; Save "2d_tri_order07.msh1";
SetOrder  8; Save "2d_tri_order08.msh1";
SetOrder  9; Save "2d_tri_order09.msh1";
SetOrder 10; Save "2d_tri_order10.msh1";
