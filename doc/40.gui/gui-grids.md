---
title: X
taxonomy:
  category: docs
template: docs
docroot: /aten/docs
header_class: alt
---


# Grids Window

The **Grids** window provides management for grid data sets owned by the different models. All grid data sets held by the current model are displayed here. The appearance of individual grids may be changed, and axes / cutoffs changed. Grid data can also be cut, copied, and pasted to different models.

A ‘grid’ in **Aten**’s world can be more or less any kind of volumetric or surface data. For instance, orbital densities, 2D height maps, molecule probability densities etc.

<figure>
  <image>img/window_grids_data.png</image>
  <caption>Grids Window – Data / Cutoff page</caption>
</figure>

## General Grid Management

Each grid in the list has associated with it a checkbox that determines whether or not it is currently visible. Below the list the minimum and maximum data values contained within the grid, and the current cutoffs (i.e. the values for which the isosurface is drawn at) for the primary and (optional) secondary surfaces to use when rendering the data. The **File** and **Edit** menus allow new grids to be loaded into the current model, and to cut, paste, and delete grids between models. Multiple grids may be selected at once in the listbox – in such a case any alterations made using the other controls are applied to all of the selected grids. In this way all cutoff values, for instace, may be set simultaneously to the same values for any number of loaded grids.

By default, only those grids associated to the current (active) model are visible, but all grids which exist within all models can be displayed with the **Show All Grids** checkbox. 

The **Secondary** checkbox specified that a second isosurface should be created for the current grid. This is useful, for instance, to draw a second, transparent surface at a lower cutoff than the primary, or to display an isosurface encompassing the negated cutoff range of the primary (e.g. to display both signs of the wavefunction in orbital data).

<figure>
  <image>img/window_grids_origin.png</image>
  <caption>Grids Window – Origin / Axes page</caption>
</figure>

The associated origin and axes to the currently selected grid can be modified here. In this way grid data may be arbitrarily flipped, stretched, and sheared, and its position in the local space of the model changed. All changes made are reflected immediately in the main view.

<figure>
  <image>img/window_grids_style.png</image>
  <caption>Grids Window – Style page</caption>
</figure>

The general appearance of the selected grid can be modified in the Style page, with the main rendering style set with the combo box (see Section 16.10 for a list of possible styles). Optionally, a bounding box around the grid data may be drawn by selecting the **Outline Volume** checkbox. The **Periodic** checkbox influences the way 3D surfaces are drawn close to the boundaries of the grid. If unchecked, a ‘gap’ will appear near to the edges of the grid volume since there is insufficient data with which to generate gradient information. If checked, data points on the opposite site of the grid will be used in this region (useful, for instance, in the case of orbital information calculated in periodic systems). For volumetric data, sometimes it is useful to see the filled volume rather than the enclosing surface – checking **Fill Volume** will do just that.

Colours for the primary and (if active) secondary sufaces can be set to simple (single) colours by selecting **Internal Colours** and choosing whatever colour is desired from the colour selection buttons. Should a more useful colouring scheme be desired than the surfaces may be rendered using a colour scale (see Section 10.1), in which case the isovalue (if volumetric) or height (if a 2D grid) determines the colour of the data point. To do so, select **From Colourscale**, and choose the ID of the colour scale to use (from the ten definable in the program).

<figure>
  <image>img/window_grids_shift.png</image>
  <caption>Grids Window – Shift page</caption>
</figure>

The **Shift** page can be used to ‘translate’ the grid data points along each axis of the data set allowing, for instance, isosurface features of interest in a periodic grid to be displayed in a more central position. Note that no modification of the actual grid data is performed – the effect is entirely visual. The **Atom Shift** group allows translation of selected/all atoms in the parent model to be moved at the same time, so as to correlate them with the new positions of the grid data.

## Orbitals Page

Not implemented yet.


