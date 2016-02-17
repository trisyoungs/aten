---
title: X
taxonomy:
  category: docs
template: docs
docroot: /aten/docs
header_class: alt
---


# Build Window

The primary function of the Build window is to allow for drawing, deletion, and transmuting of individual atoms and bonds using the mouse, along with drawing and automatic creation of bonds.

<figure>
  <image>img/window_build_edit.png</image>
  <caption>Build Window - Edit controls</caption>
</figure>

The top half of the **Edit** page of the window provides tools to draw individual atoms, chains of atoms, and molecular fragments, allow the deletion and transmutation of atoms, and provide the ability to add hydrogen atoms automatically to atoms (or the whole model). Select the relevant tool, and simply click on atoms in the main view. The element of new atoms when drawing atoms or chains (as well as the transmutation target element) is determined by the currently-selected element button. The bottom half provides tools to draw individual bonds between atoms, or to calculate bonds automatically. Drawing a bond requires pairs of atoms to be clicked sequentially once the tool is activated.

<figure>
  <image>img/window_build_tools.png</image>
  <caption>Build Window - Tools page</caption>
</figure>

The **Add Atom** tool allows atoms to be created at specific positions in the model. Coordinates are entered and the atom created (with element defined by the current selected element on the Edit page) by pressing the Add button. If the **Fractional Coords** checkbox is ticked the coordinates are assumed to be fractional and are converted to cell coordinates as the atom is added. For example, setting coordinates to {0.5,0.5,0.5} will create an atom in the centre of the current unit cell.

<figure>
  <image>img/window_build_options.png</image>
  <caption>Build Window - Options page</caption>
</figure>

Finally, the Options page allows various aspects of building to be adjusted. 

When transforming atoms in a periodic system with the mouse (i.e. rotating or translating them) if any move outside the unit cell as a result of the transformation they are automatically folded back in to the confines of the cell. If the **Prevent Folding** checkbox is ticked, these folding operations will not occur.

All rebonding operations employ a tolerance or scaling factor in the calculation of distances between atoms, which can be adjusted with the Bond Tolerance slider. Larger values increase the maximum distance between atoms that will be recognised as a bond. Use restraint, however, as too large values will generate irregular bonding patterns.


