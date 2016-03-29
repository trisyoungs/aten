---
title: X
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---


# Transform Window

For a selection of atoms, rotational or matrix-based transformations can be applied through the Atom Transform window.

## Rotation About Arbitrary Axis

<figure>
  <image>img/window_transform_axisrotate.png</image>
  <caption>Transform Window – Axis Rotate page</caption>
</figure>

The **Rotate** page allows an origin and a rotation axis about this origin to be defined about which to rotate atom selections. The origin and axis may be entered manually, can be determined from a current atom selection (**Define** buttons), or defined by the click-selection of two atoms (**Pick** button). Defining the rotation axis from the current selection will set the axis to the vector between the currently-defined origin and the centre of geometry of the current atom selection. Rotations of atom selections about this axis/origin combination are then made by defining the **Angle** of rotation (in degrees) and then applying the rotation either clockwise or anticlockwise.

## Matrix Transformation

<figure>
  <image>img/window_transform_matrix.png</image>
  <caption>Transform Window – Matrix Transform page</caption>
</figure>

From here a 3x3 transformation matrix can be applied to the current atom selection, and with a specific coordinate origin (not necessarily the centre of geometry of the selection). The **Pick** buttons allow selection of the various axes through click-selection of two atoms (not necessarily in the same model as the current atom selection), while the **Normalise** buttons will normalise each of the currently-defined axes to be of unit length. Finally, the **Orthogonalise** and **Generate** buttons allow particular axes to be orthogonalised relative to, or generated from, the other defined axes.

## Matrix Conversion

<figure>
  <image>img/window_transform_matrix.png</image>
  <caption>Transform Window – Matrix Convert page</caption>
</figure>

It is possible to transform the orientation of a given set of atoms if a suitable pair of source and destination matrices are defined. The **Source** matrix defines what should be considered the current frame of reference for the current set of coordinates. Once the **Convert** button is pressed, a rotation matrix is generated such that, when applied to the **Source** matrix (and the current atom selection) it will be transformed into the **Target** matrix.

As for the matrix convert page, the **Pick**, **Normalise**, **Orthogonalise**, and **Generate** buttons allow each axis to be selected, normalised, orthogonalised relative to the others, and generated from the others respectively.


