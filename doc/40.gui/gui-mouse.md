---
title: X
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---


# Mouse Control

Each of the mouse buttons has a different ‘style’ of action on the canvas, each of which can be set to the user’s taste in the preferences (menu item **Settings→Preferences** on Linux/Windows). In addition the **Shift**, **Ctrl**, and **Alt** keys modify or augment these default actions performed by the mouse. Standard settings out of the box are:

<table>
  <title>Mouse Button Actions</title>
 <header>
  <column>Button</column>
  <column>key Modifier</column>
  <column>Action</column>
 </header>
 <row>
  <column>**Left**</column>
  <column>None</column>
  <column>Click on individual atoms to select exclusively
  Click-hold-drag to exclusively select all atoms within rectangular region</column>
 </row>
 <row>
  <column></column>
  <column>Shift</column>
  <column>Click on individual atoms to toggle selection state
  Click-hold-drag to inclusively select all atoms within rectangular region</column>
 </row>
 <row>
  <column></column>
  <column>Ctrl</column>
  <column>Click on individual atoms to toggle selection state
  Click-hold-drag to inclusively deselect all atoms within rectangular region</column>
 </row>
 <row>
  <column>**Right**</column>
  <column>None</column>
  <column>Click-hold-drag to rotate camera around model
  Click on atom to show context (Selection) menu</column>
 </row>
 <row>
  <column></column>
  <column>Shift</column>
  <column>Click-hold-drag to rotate view around z-axis (perpendicular to plane of screen)</column>
 </row>
 <row>
  <column></column>
  <column>Ctrl</column>
  <column>Click-hold-drag to rotate selection in local (model) space</column>
 </row>
 <row>
  <column></column>
  <column>Ctrl+Shift</column>
  <column>Click-hold-drag to rotate selection around z-axis in local (model) space</column>
 </row>
 <row>
  <column>**Middle**</column>
  <column>None</column>
  <column>Click-hold-drag to translate camera</column>
 </row>
 <row>
  <column></column>
  <column>Ctrl</column>
  <column>Click-hold-drag to translate selection in local (model) space</column>
 </row>
</table>

## Manipulating the View

At its most basic **Aten**’s main view acts as a visualiser allowing models to be rotated, zoomed in and out, and drawn in various different styles. By default, the right mouse button is used to rotate the model in the plane of the screen (right-click and hold on an empty area of the canvas and move the mouse) and the mouse wheel zooms in and out. Note that right-clicking on an atom brings up the atom context menu (equivalent to main window’s **Selection** menu). The middle mouse button translates the model in the plane of the screen – again, click-hold and drag.

These rotation and translation operate only the position and orientation of the camera, with no modifications made to the actual coordinates of the model. The view can be reset at any time from the main menu (**View→Reset**) or by pressing **Ctrl-R**. Both the main menu (**View→Style**), the main toolbar, and the shortcuts **Ctrl-1** to **Ctrl-5** allow the drawing style of models to be changed between stick, tube, sphere, scaled sphere, and individual. The last option allows different view styles to be set for different atoms.

The **Ctrl** key changes the normal behaviour of the rotation and translation operations and forces them to be performed on the coordinates of the current atom selection instead of the camera. The centroid of rotation is the geometric centre of the selected atoms.

## Atom Selection

Atom selection or picking is performed with the left mouse button by default – single-click on any atom to highlight (select) it. Single-clicks perform ‘exclusive’ selections; that is, all other atom(s) are deselected before the clicked atom is (re)selected. Clicking in an empty region of the canvas deselects all atoms. Clicking on an empty space in the canvas, holding, and dragging draws a rectangular selection region – releasing the mouse button then selects all atoms within this area. Again, this selection operation is exclusive. Inclusive selections (where already-selected atoms are not deselected) are performed by holding the **Shift** key while performing the above operations. Furthermore, single-clicking on a selected atom while holding **Shift** will deselect the atom.


