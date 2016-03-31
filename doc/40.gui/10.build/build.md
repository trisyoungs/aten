---
title: Build Panel
brief: Selection, drawing, and rebonding tools
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

## Select Group <a id="select"></a>

![Select Group](select.png){.imgfull}

### Atoms

The primary selection mode, this allows individual atoms to be clicked or a selection-box to be drawn around multiple atoms in order to select them. With the **Shift** key pressed the selection state of the atoms is toggled, and if **Ctrl** is held down the atoms are removed from the current selection.

### Bound

Select individual bound fragments - clicking on any atom in a molecule will select the entire molecule by performing a recursive tree-select from that atom. With the **Shift** key pressed the selection state of the fragment is toggled, and if **Ctrl** is held down the fragment is removed from the current selection.

### Element

Select all elements of the same type as the clicked atom. Again, holding **Shift** toggles the selection while holding **Ctrl** removes them from the selection.

## Draw Group <a id="draw"></a>

![Draw Group](draw.png){.imgfull}

### Draw

Sketch individual atoms and bonds with the **Draw** tool. New atoms are created as the currently-selected [**Element**](/aten/docs/gui/build#element).

### Fragment

Draw molecular fragments, and append molecular fragments to existing atoms. Long-pressing the **Fragment** button shows the fragment selection popup, from which the available drawing fragments can be browsed and one selected for drawing.

In this mode a copy of the selected fragment is 'attached' to the mouse cursor and can be drawn in any desired position, but will snap to existing atoms that possess an available binding site (according to their current bond involvement). When snapped, holding **Shift** will replace the clicked atom rather than creating a new bond between it and the attachment point (TODO Check this!). **Ctrl** ??? Finally, **Alt** cycles over the possible attachment points on the fragment.

To define new fragments, see the topic on [Fragments](/aten/docs/topics/fragments).

### Delete

### Transmute

### Add H

### Grow

### Prevent Fold

When transforming atoms in a periodic system with the mouse (i.e. rotating or translating them) if any move outside the unit cell as a result of the transformation they are automatically folded back in to the confines of the cell. If **Prevent Fold** is enabled these folding operations will not occur.

### Keep Centered

## Element Group <a id="element"></a>

![Element Group](element.png){.imgfull}

### Table

### Common

## Building Group <a id="bonding"></a>

![Building Group](building.png){.imgfull}

### Rebond

### Augment

### Clear
