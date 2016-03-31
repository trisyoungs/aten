---
title: Model Interaction
brief: Interacting with models in the Main View
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

At its most basic **Aten**’s main view acts as a visualiser allowing models to be rotated, zoomed in and out, and drawn in various different styles. By default, the right mouse button is used to rotate the model in the plane of the screen (right-click and hold on an empty area of the canvas and move the mouse) and the mouse wheel zooms in and out. Note that right-clicking on an atom brings up the atom context menu (which mirrors the functionality of the [Selection panel](/aten/docs/gui/selection)). The middle mouse button translates the model in the plane of the screen – again, click-hold and drag.

These rotation and translation operate only the position and orientation of the camera, with no modifications made to the actual coordinates of the model. The view can be reset at any time from the [View group](/aten/docs/gui/home#view), or by pressing **Ctrl-R**. The [Appearance group](/aten/docs/gui/home#appearance) of the [Home panel](/aten/docs/gui/home) and the shortcuts **F1** to **F5** allow the drawing style of models to be changed between stick, tube, sphere, scaled sphere, and individual. The last option allows different view styles to be set for different atoms.

The **Ctrl** key changes the normal behaviour of the rotation and translation operations and forces them to be performed on the coordinates of the current atom selection instead of the camera. The centroid of rotation is the geometric centre of the selected atoms.

| Button | Key Modifier | Action |
|--------|--------------|--------|
| **Left** | None | Click on individual atoms to select exclusively <br/> Click-hold-drag to exclusively select all atoms within rectangular region <br/> Click in empty region to deselect all atoms |
|          | Shift | Click on individual atoms to toggle selection state <br/> Click-hold-drag to inclusively select all atoms within rectangular region |
|          | Ctrl | Click on individual atoms to toggle selection state <br/> Click-hold-drag to inclusively deselect all atoms within rectangular region |
| **Right** | None | Click-hold-drag to rotate camera around model <br/> Click on atom to show context (Selection) menu |
|           | Shift | Click-hold-drag to rotate view around z-axis (perpendicular to plane of screen) |
|           | Ctrl | Click-hold-drag to rotate selection in local (model) space |
|           | Ctrl+Shift | Click-hold-drag to rotate selection around z-axis in local (model) space |
| **Middle** | None | Click-hold-drag to translate camera |
|            | Ctrl | Click-hold-drag to translate selection in local (model) space |

Note that actions for the left mouse button listed above are for the [**Select** tool](/aten/docs/gui/select), and will change depending on the currently-selected interaction mode. The actions of the current mode are always displayed in the [**Statusbar**](/aten/docs/gui/mainwindow#statusbar).
