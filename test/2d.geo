Point(1) = {0, 0, 0, 1.0};

Extrude {1, 0, 0} { Point{1}; }
Extrude {0, 1, 0} { Curve{1}; }
