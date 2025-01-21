# CSG-script

A simple implicit modelling test system scriptable in Scheme.

It is based on my [sample 3D framework](http://github.com/salvipeter/sample-framework),
which uses [Qt](http://qt-project.org/) and [OpenMesh](http://www.openmesh.org/),
while tessellation is done with my
[dual contouring library](http://github.com/salvipeter/dual-contouring).
Alternative tessellation is done with my
[marching cubes library](http://github.com/salvipeter/marching).
Scripting support is added via [Guile](http://www.gnu.org/software/guile/).

# Usage

The only extra commands are `show` and `mc`. The original display function was `show`,
which uses a kind of dual contouring:

```scheme
(show <distance-function> <bounding-box> <resolution>)
```

Here `distance-function` is a function that takes a 3D point (a list of 3 numbers) and returns its signed distance to the implicit surface. The bounding box is given as a list of two points, representing the minimum and maximum corners, and `resolution` is an integer which corresponds to the number of quads on an edge when the bounding box is a cube (the actual resolution is always adjusted to the aspect ratio of the bounding box).

Alternatively, `mc` uses marching cubes:

```scheme
(mc <distance-function> <bbox-center> <radius> '(<min-depth> <max-depth>))
```

This tessellates the surface in a regular axis-aligned box centered at `bbox-center`,
and having edges of length `2*radius`. Evaluation is adaptive, with the given
minimum and maximum `depth` values.

See `example.scm` for an example.
