---
title: Atom
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

The [**Atom**](/aten/docs/scripting/variabletypes/atom) type encompasses the complete description of a single atom in a model or frame.
 
| Member | Type | RW | Description |
|--------|------|----|-------------|
| bit | **int** | • | Temporary integer value, with associated bit-setting functions (see Section 8.2.5) |
| bonds | [**Bond**](/aten/docs/scripting/variabletypes/bond)[] | | List of bonds the atom is involved in |
| colour | **double**[4] | • | Custom colour of the atom (used when the [Colouring Scheme](/aten/docs/enums/colourscheme) is set to ‘custom’) |
| data | **string** | • | Temporary character data stored on the atom (for use in filters etc.) |
| element | [**Element**](/aten/docs/scripting/variabletypes/element) | • | Returns a pointer to the assigned element data of the atom |
| fixed | **int** | • | Whether the atom's position is fixed (1) or not (0) |
| f | [**Vector**](/aten/docs/scripting/variabletypes/vector) | • | Force vector |
| fracx | **double** | • | Position x-component in fractional cell coordinates |
| fracy | **double** | • | Position y-component in fractional cell coordinates |
| fracz | **double** | • | Position z-component in fractional cell coordinates |
| fx | **double** | • | Force x-component |
| fy | **double** | • | Force y-component |
| fz | **double** | • | Force z-component |
| hidden | **int** | • | Whether the atom is hidden (1) or visible (0) |
| id | **int** | | Numerical ID of the atom within its parent model |
| mass | **double** | | Atomic mass of the atom |
| name | **string** | | Element name of the atom |
| q | **double** | • | Atomic charge associated to the atom |
| r | [**Vector**](/aten/docs/scripting/variabletypes/vector) | • | Position vector |
| rx | **double** | • | Position x-component |
| ry | **double** | • | Position y-component |
| rz | **double** | • | Position z-component |
| selected | **int** | • | Whether the atom is selected (1) or unselected (0) |
| style | **string** | • | The current Drawing Style of the atom (see Section 16.7) |
| symbol | **string** | | Element symbol of the atom |
| type | [**FFAtom**](/aten/docs/scripting/variabletypes/ffatom) | • | Forcefield type of the atom |
| v | [**Vector**](/aten/docs/scripting/variabletypes/vector) | • | Velocity vector |
| vx | **double** | • | Velocity x-component |
| vy | **double** | • | Velocity y-component |
| vz | **double** | • | Velocity z-component |
| z | **int** | • | Atomic number of the atom |

## Atom Type Functions

### addBit <a id="addbit"></a>

_Syntax:_

**void** **addBit** ( **int** _bit_ )

Add (set) the specified bit for this [**Atom**](/aten/docs/scripting/variabletypes/atom).  If it is desired to set the [**Atom**](/aten/docs/scripting/variabletypes/atom)’s bit to a certain value, use the accessor instead.

---

### copy <a id="copy"></a>

_Syntax:_

**void** **copy** ( [**Atom**](/aten/docs/scripting/variabletypes/atom) _source_ )

Copy all information from the source [**Atom**](/aten/docs/scripting/variabletypes/atom) into this [**Atom**](/aten/docs/scripting/variabletypes/atom), except for its id.

---

### findBond <a id="findbond"></a>

_Syntax:_

[**Bond**](/aten/docs/scripting/variabletypes/bond) **findBond** ( [**Atom**](/aten/docs/scripting/variabletypes/atom) _i_ )

Return the [**Bond**](/aten/docs/scripting/variabletypes/bond) (if any) between this and the specified [**Atom**](/aten/docs/scripting/variabletypes/atom) _i_.

---

### hasBit <a id="hasbit"></a>

_Syntax:_

**int** **hasBit** ( **int** _bit_ )

Return whether this [**Atom**](/aten/docs/scripting/variabletypes/atom) has the specified _bit_ set.

---

### removeBit <a id="removebit"></a>

_Syntax:_

**void** **removeBit** ( **int** _bit_ )

Remove the specified _bit_ from the [**Atom**](/aten/docs/scripting/variabletypes/atom)’s bit variable.


