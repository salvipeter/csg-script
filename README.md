# CSG-script

A simple implicit modelling test system scriptable in Scheme.

It is based on my [sample 3D framework](http://github.com/salvipeter/sample-framework),
which uses [Qt](http://qt-project.org/) and [OpenMesh](http://www.openmesh.org/),
while tessellation is done with my
[dual contouring library](http://github.com/salvipeter/dual-contouring).
Scripting support is added via [Guile](http://www.gnu.org/software/guile/).

# Usage

The only extra command is `show`:

```scheme
(show <distance-function> <bounding-box> <resolution>)
```

Here `distance-function` is a function that takes a 3D point (a list of 3 numbers) and returns its signed distance to the implicit surface. The bounding box is given as a list of two points, representing the minimum and maximum corners, and `resolution` is an integer which corresponds to the number of quads on an edge when the bounding box is a cube (the actual resolution is always adjusted to the aspect ratio of the bounding box).

See `example.scm` for an example.
