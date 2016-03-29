---
title: X
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---


# Position Window

Tools for the absolute positioning of atoms are available here. All work on the current selection of atoms in the current model.

<figure>
  <image>img/window_position_centre.png</image>
  <caption>Position Window – Centre page</caption>
</figure>

The **Centre** page allows the centre of geometry of the current selection to be positioned at absolute coordinates. The desired position is entered in the three input boxes, or can be defined from the geometric centre of a selection of atoms (prior to the positioning of a different set). Any (or all) of the Cartesian axes may be locked preventing coordinate adjustment along particular directions.

<figure>
  <image>img/window_position_flip.png</image>
  <caption>Position Window – Flip page</caption>
</figure>

The **Flip** page mirrors the positions of atoms in the current selection through its centre of geometry in either the X, Y, or Z directions. Note that this tool currently works only along the Cartesian axes, and does not take into account the shape of any defined cell.

<figure>
  <image>img/window_position_translate.png</image>
  <caption>Position Window – Translate page</caption>
</figure>

Translations of atoms within model (local), world (view) and cell frames of reference can be performed in the **Translate** page. The group of directional buttons move the selected atoms along the relevant axis within the selected frame of reference, and by the amount specified in the **Shift** control. For model and world reference frames the **Shift** control specifies the number of Angstroms moved along the axis in each step. For the cell reference frame it defines the fractional cell distance moved in each direction.

<figure>
  <image>img/window_position_shift.png</image>
  <caption>Position Window – Shift page</caption>
</figure>

The vector along which to move the current selection is defined on the left hand side of the **Shift** page. Furthermore, the axis may be defined by **Pick**ing two atoms in the main window. The supplied vector does not need to be normalised, but thus may be performed through the **Normalise** button. The defined shift value dictates the multiple of the defined vector by which selected atoms are shifted.

<figure>
  <image>img/window_position_reposition.png</image>
  <caption>Position Window – Reposition page</caption>
</figure>

The reposition page allows the centre of geometry of a selection of atoms to be moved from a reference coordinate (defined by the **Reference** panel) to a destination coordinate (defined by the **Target** panel). Either coordinate may be set from the centre of geometry of the current atom selection by pressing the relevant **Define** button.


