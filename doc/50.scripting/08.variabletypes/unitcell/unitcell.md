---
title: UnitCell
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

The [**UnitCell**](/aten/docs/scripting/variabletypes/unitcell) type contains a full unit cell specification for a model, including any associated spacegroup.
 
| Member | Type | RW | Description |
|--------|------|----|-------------|
| a | **double** | • | Length of cell axis A |
| alpha | **double** | • | Angle between cell axes B and C |
| b | **double** | • | Length of cell axis B |
| beta | **double** | • | Angle between cell axes B and C |
| c | **double** | • | Length of cell axis C |
| ax | **double** | • | x-component of cell axis A |
| ay | **double** | • | y-component of cell axis A |
| az | **double** | • | z-component of cell axis A |
| bx | **double** | • | x-component of cell axis B |
| by | **double** | • | y-component of cell axis B |
| bz | **double** | • | z-component of cell axis B |
| centreX | **double** | | x-coordinate at centre of defined cell |
| centreY | **double** | | y-coordinate at centre of defined cell |
| centreZ | **double** | | z-coordinate at centre of defined cell |
| cx | **double** | • | x-component of cell axis C |
| cy | **double** | • | y-component of cell axis C |
| cz | **double** | • | z-component of cell axis C |
| density | **double** | | Density of the current cell |
| gamma | **double** | • | Angle between cell axes A and B |
| matrix | **double**[9] | • | Cell axis matrix containing all three cell vectors. For example, ax = matrix[1], ay = matrix[2], etc.) |
| sgId | **int** | • | Integer ID of the current spacegroup |
| sgName | **string** | • | Symbol of the current spacegroup |
| type | **string** | | Type of the current unit cell (see Cell Types in Section 16.4) |
| volume | **double** | | Volume of the cell in cubic Å |

## UnitCell Type Functions

### copy <a id="copy"></a>

_Syntax:_

**void** **copy** ( [**UnitCell**](/aten/docs/scripting/variabletypes/unitcell) _source_ )

Copy all information from the source [**UnitCell**](/aten/docs/scripting/variabletypes/unitcell) into this [**UnitCell**](/aten/docs/scripting/variabletypes/unitcell).

---

### fracToReal <a id="fractoreal"></a>

_Syntax:_

[**Vector**](/aten/docs/scripting/variabletypes/vector) **fracToReal** ( **double** _fracx_, **double** _fracy_, **double** _fracz_ )

Returns the real-space coordinates of the supplied fractional (cell) coordinates.

---

### mim <a id="mim"></a>

_Syntax:_

[**Vector**](/aten/docs/scripting/variabletypes/vector) **mim** ( [**Atom**](/aten/docs/scripting/variabletypes/atom) _i_, [**Atom**](/aten/docs/scripting/variabletypes/atom) _j_ )

Returns a vector containing the minimum image coordinates of the atom _i_ with respect to the reference atom _j_. Note that the coordinates of both _i_ and _j_ are not modified in any way.

---

### mimVector <a id="mimvector"></a>

_Syntax:_

[**Vector**](/aten/docs/scripting/variabletypes/vector) **mimVector** ( [**Atom**](/aten/docs/scripting/variabletypes/atom) _i_, [**Atom**](/aten/docs/scripting/variabletypes/atom) _j_ )

Returns the minimum image vector from atom _i_ to atom _j_. Note that the coordinates of both _i_ and _j_ are not modified in any way.

---

### realToFrac <a id="realtofrac"></a>

_Syntax:_

[**Vector**](/aten/docs/scripting/variabletypes/vector) **realToFrac** ( **double** _x_, **double** _y_, **double** _z_ )

Returns the fractional coordinates of the supplied real-space coordinates.

---

### translateAtom <a id="translateatom"></a>

_Syntax:_

[**Vector**](/aten/docs/scripting/variabletypes/vector) **translateAtom** ( [**Atom**](/aten/docs/scripting/variabletypes/atom) _i_, **double** _dx_, **double** _dy_, **double** _dz_ )

Returns a vector corresponding to the original coordinates of [**Atom**](/aten/docs/scripting/variabletypes/atom) _i_ translated by the specified fractional cell amounts in each cell axis direction. Note that the existing coordinates of _i_ are not modified in any way.


