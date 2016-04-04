---
title: Grids Panel
brief: Load and display grid-based data files
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

The **Grids** panel provides management for grid data owned by the current model, and allows their definition and appearance to be defined.

![Grids Panel](panel.png){.imgfull}

## Manage Group <a id="manage"></a>

![Manage Group](manage.png){.imgfull}

All grids owned by the current model are listed here - each has a checkbox determining the visibility of any surfaces set for the data. The entry selected in this list reflects the current grid, and the target of all other controls on the panel.

Each grid in the list has associated with it a checkbox that determines whether or not it is currently visible. Below the list the minimum and maximum data values contained within the grid, and the current cutoffs (i.e. the values for which the isosurface is drawn at) for the primary and (optional) secondary surfaces to use when rendering the data. The **File** and **Edit** menus allow new grids to be loaded into the current model, and to cut, paste, and delete grids between models. Multiple grids may be selected at once in the listbox – in such a case any alterations made using the other controls are applied to all of the selected grids. In this way all cutoff values, for instace, may be set simultaneously to the same values for any number of loaded grids.

### Open

Open and associate a new grid file with the current model. Long-pressing reveals the recent grids list.

### Remove

Remove the selected grid in the list from the current model.

## Define Group <a id="define"></a>

![Define Group](define.png){.imgfull}

## Primary / Secondary Surface Groups <a id="surface"></a>

![Primary Group](primary.png){.imgfull}
![Secondary Group](secondary.png){.imgfull}

## Options Group <a id="options"></a>

![Options Group](options.png){.imgfull}

## Transform Group <a id="transform"></a>

![Transform Group](transform.png){.imgfull}

