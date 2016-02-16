---
title: Grid Commands
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

Add gridded data to the current model.

Grid window for management of grids in the GUI.

---

## addGridPoint <a id="addgridpoint"></a>

_Syntax:_

**void** **addGridPoint** ( **int** _ix_, **int** _iy_, **int** _iz_, **double** _value_ )

Set a specific data point in the current grid.

For example:


```
addGridPoint(4, 1, 15, 4.123);
```


set the grid value at point { 4,1,15 } in the dataset to 4.123.

---

## addNextGridPoint <a id="addnextgridpoint"></a>

_Syntax:_

**void** **addNextGridPoint** ( **double** _value_ )

Add the next sequential grid point, starting at (1,1,1) and proceeding through dimensions as defined by the **gridLoopOrder** command (default is x→y→z, i.e. (1,1,1) is set first, then (2,1,1), (3,1,1) etc.).

For example:


```
addNextGridPoint(20.0);
```


sets the next grid point value to be 20.0.

---

## finaliseGrid <a id="finalisegrid"></a>

_Syntax:_

**void** **finaliseGrid** ( )

Perform internal post-load operations on the grid. Must be called for every new grid, after all data has been read in.

For example:


```
finaliseGrid();
```


---

## gridAlpha <a id="gridalpha"></a>

_Syntax:_

**double** **gridAlpha** ( )

**double** **gridAlpha** ( **double** _newAlpha_ )

Set the alpha value (transparency of the current surface), with 0.0 being fully opaque and 1.0 being fully transparent (i.e. invisible), or simply return the current alpha value if no new value is provided. Note that this command sets the alpha values for both the primary and secondary surface colours.

For example:


```
gridAlpha(0.5);
```


---

## gridAxes <a id="gridaxes"></a>

_Syntax:_

**void** **gridAxes** ( **double** _ax_, **double** _ay_, **double** _az_, **double** _bx_, **double** _by_, **double** _bz_, **double** _cx_, **double** _cy_, **double** _cz_ )

Set the axes of the current grid, specified as three vectors.

For example:


```
gridAxes(1, 0, 0, 0, 1, 0, 0, 0, 1);
```


sets a cubic system of axes for the current grid.


```
gridAxes(0.8, 0, 0, 0.1, 0.6, 0, 0, 0, 0.7);
```


sets a monoclinic system of axes for the current grid.

---

## gridColour <a id="gridcolour"></a>

_Syntax:_

**void** **gridColour** ( **double** _r_, **double** _g_, **double** _b_, **double** _a_ = 1.0 )

Set the internal colour of the primary grid surface to the RGB(A) value (each component of which should be in the range 0.0 to 1.0 inclusive).

For example:


```
gridColour(1.0, 1.0, 0.0);
```


sets the primary surface colour to yellow.

---

## gridColourSecondary <a id="gridcoloursecondary"></a>

_Syntax:_

**void** **gridColourSecondary** ( **double** _r_, **double** _g_, **double** _b_, **double** _a_ = 1.0 )

Set the internal colour of secondary grid surface to the RGB(A) value supplied (each component of which should be in the range 0.0 to 1.0 inclusive).

For example:


```
gridColourSecondary(0.9, 0.9, 0.9);
```


sets the secondary surface colour to off-white.

---

## gridColourScale <a id="gridcolourscale"></a>

_Syntax:_

**void** **gridColourScale** ( **int** _id_ )

Set the colourscale to use for the current grid to the colourscale ID specified, which should be in the range 1-10. If '0' is given as the argument, the internal colour of the grid data is used. Linking a colourscale to a grid will result in the minimum and maximum ranges of the grid being recalculated to ensure all points in the grid are covered by the scale, whose range is adjusted if necessary.

For example:


```
gridColourScale(4);
```


colours the grid data according to colourscale 4.


```
gridColourScale(0);
```


uses the internal colour(s) specified for the grid.

---

## gridCubic <a id="gridcubic"></a>

_Syntax:_

**void** **gridCubic** ( **double** _l_ )

Sets up a cubic system of axes for the current grid.

For example:


```
gridCubic(0.5);
```


sets up a cubic system of axes, each grid point 0.5 Å apart in all directions.

---

## gridCutoff <a id="gridcutoff"></a>

_Syntax:_

**void** **gridCutoff** ( **double** _lowercut_, **double** _uppercut_ = (none) )

Sets the lower and (if supplied) upper cutoff values for the current grid.

For example:


```
gridCutoff(0.002,0.005);
```


sets the lower grid cutoff for the current grid to 0.002, and the upper grid cutoff to 0.005.

---

## gridCutoffSecondary <a id="gridcutoffsecondary"></a>

_Syntax:_

**void** **gridCutoffSecondary** ( **double** _lowercut_, **double** _uppercut_ = (none) )

Sets the lower and (if supplied) upper secondary cutoff values for the current grid.

For example:


```
gridCutoffSecondary(0.0014);
```


sets the secondary lower grid cutoff for the current grid to 0.0014, leaving the upper secondary cutoff unchanged.

---

## gridLoopOrder <a id="gridlooporder"></a>

_Syntax:_

**void** **gridLoopOrder** ( **string** _order_ )

Set the grid loop order to use with addnextgridpoint, affecting in which order the dimensions of the data grid are filled. _order_ should be given as a string of three characters, e.g. ‘xyz’ (equivalent to ‘123’), ‘yxz’ (equivalent to ‘213’), etc.

For example:


```
gridLoopOrder("zyx");
```


sets the loop order to the reverse of the default, so that the z-index is filled first.

---

## gridOrigin <a id="gridorigin"></a>

_Syntax:_

**void** **gridOrigin** ( **double** _x_, **double** _y_, **double** _z_ )

Sets the origin of the grid data, in Å.

For example:


```
gridOrigin(0, 10, 0);
```


sets the grid origin to be offset 10 Å along the y-axis.

---

## gridOrtho <a id="gridortho"></a>

_Syntax:_

**void** **gridOrtho** ( **double** _a_, **double** _b_, **double** _c_ )

Sets up an orthorhombic system of axes for the grid data.

For example:


```
gridOrtho(0.5, 0.5, 0.8);
```


sets up a system of axes elongated in the z-axis.

---

## gridStyle <a id="gridstyle"></a>

_Syntax:_

**void** **gridStyle** ( **string** _style_ )

Determines how the current grid data is drawn on-screen. Valid _style_s are listed in the (GridStyle)[/aten/docs/enumerations/gridstyle] enum.

For example:


```
gridStyle("triangles");
```


draws the current grid as a triangle mesh.

---

## gridUseZ <a id="gridusez"></a>

_Syntax:_

**int** **gridUse****Z** ( )

**int** **gridUseZ** ( **bool** _usez_ )

For two-dimensional grid (i.e. surface) data this option controls whether the data value is used as the height (z) component of the surface, or if no data value is used and the surface is flat. If called with no arguments the current status of the option is returned (0 being off, and 1 being on).

For example:


```
gridUseZ("on");
```


---

## gridVisible <a id="gridvisible"></a>

_Syntax:_

**int** **gridVisible** ( )

**int** **gridVisible** ( **bool** _visible_ )

Set the visibility of the current grid (i.e. whether it is drawn on screen).

For example:


```
gridVisible(FALSE);
```


---

## initGrid <a id="initgrid"></a>

_Syntax:_

**void** **initGrid** ( **string** _type_, **int** _nx_, **int** _ny_, **int** _nz_ )

Initialises the current grid to be of the specified grid type and with the dimensions specified (if the _type_ requires it). This must be called before any calls to addpoint or addnextgridpoint are issued.

For example:


```
initGrid("regularxyz", 64, 128, 64);
```


initialises the current grid to be a regularly-spaced and hold a total of (gets calculator...) 524,288 points.

---

## loadGrid <a id="loadgrid"></a>

_Syntax:_

[Grid](/aten/docs/scripting/variabletypes/grid) **loadGrid** ( **string** _filename_ )

Load an existing grid from the specified file, and add it to the current model. If successfully loaded, a reference to the new grid is returned.

For example:


```
Grid density = loadGrid("density.pdens");
```


loads a grid called "density.pdens" and attaches it to current model.

---

## newGrid <a id="newgrid"></a>

_Syntax:_

[Grid](/aten/docs/scripting/variabletypes/grid) **newGrid** ( **string** _name_ )

Creates a new, empty grid with the provided 'name' in the current model, and returns a reference to it.

For example:


```
Grid g = newGrid("charlie");
```


creates a new grid called, for some reason, "charlie".


