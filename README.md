Source code for a [visualization of the Petersen graph][1].

[1]: http://www.amotlpaa.org/math/petersen-graph.html

Model: A Platonic dodecahedron in a sphere, with opposite vertices
identified.  (It turns out that under this identification, the
dodecahedral edge graph becomes the Petersen graph.)

View: Stereographically project the southern hemisphere onto the
equatorial plane via the north pole.  Map points in the northern
hemisphere to the same points as their antipodes.

Controller: Dragging one point to another on the screen causes a
corresponding rotation of the sphere (represented as unit quaternions).
