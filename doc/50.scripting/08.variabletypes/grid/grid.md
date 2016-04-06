---
title: Grid
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

The [**Grid**](/aten/docs/scripting/variabletypes/grid) type stores all information for a single grid structure associated to a model.
 
| Member | Type | RW | Description |
|--------|------|----|-------------|
| axes | [**Matrix**](/aten/docs/scripting/variabletypes/matrix) | | Axes system for the grid, stored in a unit cell structure |
| colour | **double**[4] | • | The primary colour of the grid data |
| colourScale | **int** | • | The colourscale in use by the grid (if relevant) |
| cutoff | **double** | • | The lower primary cutoff value |
| dataMaximum | [**Vector**](/aten/docs/scripting/variabletypes/vector) | • | The maximum value of the grid axes |
| dataMinimum | [**Vector**](/aten/docs/scripting/variabletypes/vector) | • | The minimum value of the grid axes |
| loopOrder | **int**[3] | • | Loop ordering used to read in point data |
| name | **string** | • | Name associated to the grid data |
| nx | **int** | | Size of grid data in the x-dimension |
| ny | **int** | | Size of grid data in the y-dimension |
| nz | **int** | | Size of grid data in the z-dimension |
| nPoints | **int** | | Total number of datapoints present in the grid |
| origin | [**Vector**](/aten/docs/scripting/variabletypes/vector) | • | Coordinates of the origin (lower left-hand corner) of the grid data |
| outlineVolume | **int** | • | Flag specifying whether a bounding volume cuboid should be drawn for the grid |
| periodic | **int** | • | Flag specifying whether the grid is periodic (e.g. at the edges data from opposite side should be used to form grid) |
| secondaryColour | **double**[4] | • | The secondary colour of the grid data |
| secondaryCutoff | **double** | • | The lower secondary cutoff value |
| secondaryStyle | **string** | • | Rendering style of the secondary surface |
| secondaryUpperCutoff | **double** | • | The upper secondary cutoff value |
| shiftX | **int** | • | Shift value (in points) for the grid along its x-axis |
| shiftY | **int** | • | Shift value (in points) for the grid along its y-axis |
| shiftZ | **int** | • | Shift value (in points) for the grid along its z-axis |
| style | **string** | • | Rendering style of the primary surface |
| type | **string** |   | Type of grid data |
| upperCutoff | **double** | • | The upper primary cutoff value |
| useColourScale | **int** | • | Whether a colourscale is being used to colour the associated surface(s) |
| useDataForZ | **int** | • | For 2D grid data, whether to consider the value as the 'height' of the surface |
| visible | **int** | • | Flag specifying whether the grid is visible (i.e. should be drawn in the model) |

## Grid Type Functions

### data <a id="data"></a>

_Syntax:_

**double** **data** ( **int** _i_, **int** _j_, **int** _k_ = -1 )

Return the value of the datapoint at grid ‘coordinates’ (_i_,_j_,_k_).

---

### initialise <a id="initialise"></a>

_Syntax:_

**int** **initialise** ( **string** _type_, **int** _nx_, **int** _ny_, **int** _nz_ = -1 )

Initialise the grid structure to be of the specified [grid type](/aten/docs/enums/gridtype), and with the number of points given in each direction.

---

### shift <a id="shift"></a>

_Syntax:_

**void** **data** ( **int** _dx_, **int** _dy_, **int** _dz_, **bool** _shiftAtoms_ = FALSE )

Shift the data contained within the grid by the number of cells specified in each of the x, y, and z directions.  If the _shiftAtoms_ parameter is TRUE then all atoms within the parent model are shifted by the corresponding amount in real space.


