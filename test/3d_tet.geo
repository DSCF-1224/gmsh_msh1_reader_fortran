// -----------------------------------------------------------------------------
// How to run
// gmsh -parse_and_exit 3d_tet.geo
// -----------------------------------------------------------------------------
Point(1) = {0, 0, 0, 1.0};

Extrude {1, 0, 0} { Point{1};   }
Extrude {0, 1, 0} { Curve{1};   }
Extrude {0, 0, 1} { Surface{5}; }

Mesh 3;

SetOrder  1; Save "3d_tet_order01.msh1";
SetOrder  2; Save "3d_tet_order02.msh1";
SetOrder  3; Save "3d_tet_order03.msh1";
SetOrder  4; Save "3d_tet_order04.msh1";
SetOrder  5; Save "3d_tet_order05.msh1";
SetOrder  6; Save "3d_tet_order06.msh1";
SetOrder  7; Save "3d_tet_order07.msh1";
SetOrder  8; Save "3d_tet_order08.msh1";
SetOrder  9; Save "3d_tet_order09.msh1";
SetOrder 10; Save "3d_tet_order10.msh1";
