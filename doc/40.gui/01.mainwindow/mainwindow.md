---
title: The Main Window
brief: Basic layout of the main window
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

The main window of **Aten** (in version 2.0) mimicks the 'ribbon' style toolbar (**1**) of popular office-related software. Much of the primary focus of the main window is on the model viewing canvas (**4**) which also doubles as a message display area.

![The Main Window](mainwindow_annotated.png){.imgfull}

## Toolbar (1)

All of **Aten**'s core functionality, tools, and actions are encapsulated in the toolbar at the top of the main window (**1**). Each set of tools is grouped by a specific panel, and within that panel further separated and grouped by specific functionality. Each panel and the tools it contains is described in detail in the forthcoming sections.

## Model List (2)

All model's currently loaded in are displayed in the **Model List** (**2**) as miniature icons of their contents. Selecting a model here will make it the current model, and the primary focus of the main view (**4**). Multiple models can be selected at once using `Shift` and/or `Alt` in combination with the left mouse button, in which case all are displayed in the main view in a grid layout. Only one model is ever the current model, however, and the target of all editing operations. The **Model List** can be shown and hidden by clicking the associated large vertical button.

## Atom List (3)

For the current model, the **Atom List** (**3**) provides details on every atom present within it, showing coordinates, element information etc. The item selection in the **Atom List** also directly reflects the atom selection in the model, and thus the target of many editing operations (e.g. cut, paste, translate, etc.). Similarly, changing the atom selection via interaction with the model in the Main View (**4**) alters the selection state of items in the **Atom List**. As with the **Model List**, the **Atom List** can be shown and hidden by clicking the associated large vertical button.

Exactly what information is shown for each atom is controlled with the various toggle buttons at the bottom-right of the list. In descending order, they represent the integer atom index (ID), element (E), assigned forcefield atom type (FF), coordinates (X, Y, Z), and assigned charge (Q). If patterns have been defined for the model, and the **View by pattern** checkbox at the bottom of the list is enabled, the atoms in the list will be grouped according to their encompassing pattern, otherwise they are listed in one continuous run by ascending ID. If patterns **are** defined, (de)selecting the pattern name in the list (de)selects all atoms making up the pattern in the model.

The **Atom List** also allows atoms to reordered with the four arrow buttons at the top right of the list. These operations affect the current atom selection, shifting the atoms to the start of the list, up one place in the list (to lower indices), down one place in the list (to higher indices), and to the end of the list respectively. In the case of shifting multiple atoms to the beginning / end of the list, the relative order of atoms is preserved.

## Main View (4)

The Main View (**4**) is where the current model(s) is (are) displayed - exactly which of the currently-loaded models are displayed is controlled by the selection in the **Model List** (**2**). If multiple models are displayed simultaneously, the current model (the target of all editing operations) is indicated with a 'halo' in the background of the model view. The **Main View** also shows all messaging output - by default this is displayed in the background, behind all model views. The space bar cycles the visibility of model views and the messages, changing whether one is drawn on top of the other, or one is omitted completely. The message buffer itself can be cleared, copied, and scrolled with the controls on the far right of the **Main View**.

### Mouse Control

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

The edges of the main window, around the canvas, are standard (Qt4) dock areas, in which the toolbox and any of the tool windows can be placed. Once you have the various subwindows set up how you like them, press **Settings→Store Default Window State **to remember these settings for when **Aten** next starts.


**Aten**’s main window is predominantly taken up with the rendering canvas where model(s) are displayed. Multiple models may be displayed simultaneously – the ‘current’ model (i.e. the one to which all editing / data operations are send) always has a black box drawn around it. A single toolbar sits above the canvas providing quick access to file, edit, and select actions. All other functionality is contained with various tool windows. These tool windows are accessed by the ToolBox widget, which by default is the only window displayed on startup (if it is not visible, you can raise and centre it on the main window from the menu item **Settings→Show Toolbox**). At the foot of the window is a status bar reflecting the content of the current model, listing the number of atoms and the number of selected atoms (bold value in parentheses, but only if there are selected atoms), the mass of the model, and the cell type and density (if the model is periodic).
