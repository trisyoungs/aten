---
title: Selection Commands
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

Select atoms or groups of atoms within the current model.

---

## deSelect <a id="deselect"></a>

_Syntax:_

**int** **deSelect** ( [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int**|**string** _selection_, ... )

Deselect atoms in the current model, returning the number of atoms deselected by the provided selection arguments. One or more arguments may be supplied, and each may be of the type int, atom, or string. In the case of the first two types, individual atoms (or those corresponding to the integer id) are deselected. On the other hand, strings may contain ranges of atom IDs and element symbols permitting atoms to be deselected in groups. Ranges are specified as '_a_-_b_' where _a_ and _b_ are either both atom IDs or both element symbols. In addition, the '+' symbol can be used before ('+a') or after ('a+') an atom ID or element symbol to mean either ‘everything up to and including this’ or ‘this and everything after’. Within a string argument, one or more selection ranges may be separated by commas.

For example:

```aten
deSelect(5);
```

deselects the 5th atom.

```aten
deSelect("1-10,N");
```

deselects the first ten atoms, and all nitrogen atoms.

```aten
int n = deSelect("Sc-Zn");
```

deselects the first transition series of elements, returning the number of atoms that were deselected in the process.

```aten
deSelect("C+");
```

deselects all elements carbon and above.

```aten
deSelect(1, 2, 5, "8+");
```

deselects the first, second, and fifth atoms, as well as the eighth atom and all that occur after it.

---

## deSelectCode <a id="deselectcode"></a>

_Syntax:_

**int** **deSelectCode** ( **string** _code_ )

Evaluates the _code_ snippet for each atom, deselecting any atom for which it evaluates to **true**. Atom information can be accessed through a variable _i_ of type [**Atom**](/aten/docs/scripting/variabletypes/atom).

For example:

```aten
deSelectCode("(i.z == 6);");
```

will deselect all carbon atoms in the current model.

---

## deSelectF <a id="deselectf"></a>

_Syntax:_

**int** **deSelectF** ( **string** _format_, ... )

Deselect atoms using the same syntax as the **deSelect** command, but constructing the string using a C-style **printf** approach. Useful when two integer numbers defining a range of atoms to deselect are stored in two local variables, for instance, or when the selection range must change dynamically in a loop.

For example:
```aten
int i = 10;
deSelectF("%i-%i", i, i+10);
```

deselects atom ids 10 to 20 inclusive.

---

## deSelectName <a id="deselectname"></a>

_Syntax:_

**int** **deSelectName** ( **string** _name_ )

Deselect all atoms in the current model whose assigned type names (not equivalent names) match the supplied string. The string may contain wildcards ('*').

For example:

```aten
deSelectName("O*");
```

will deselect all atoms whose assigned type names begin with 'O'.

---

## deSelectType <a id="deselecttype"></a>

_Syntax:_

**int** **deSelectType** ( **string**|**int**|[**Element**](/aten/docs/scripting/variabletypes/element) _el_, **string** _neta_ )

Deselect all atoms in the current model matching the element and NETA type description specified.

For example:

```aten
deSelectType(H, "-O(-C)");
```

will deselect all hydrogen atoms bound to oxygen atoms which are, in turn, bound to carbon atoms (i.e. all hydroxyl hydrogens).

---

## expand <a id="expand"></a>

_Syntax:_

**int** **expand** ( )
**int** **expand** ( **int** _repeat_ )

Expand the current selection of atoms by selecting any atoms that are directly bound to an already-selected atom. The number of atoms added to the previous selection is returned. If a _repeat_ number is given the expansion is repeated that number of times. If `-1` is passed as the _repeat_, then expansion of the selection continues until no more atoms can be selected.

For example:

```aten
expand();
```

---

## invert <a id="invert"></a>

_Syntax:_

**int** **invert** ( )

Inverts the selection of all atoms in the current model. Returns the number of atoms selected.

For example:

```aten
invert();
```

---

## select <a id="select"></a>

_Syntax:_

**int** **select** ( [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int**|**string** _selection_, ... )

Select atoms in the current model, keeping any previous selection of atoms. See the **deSelect** command for a full description of the syntax. The number of atoms added to the existing selection is returned.

For example:

```aten
select("+5");
```

selects the first five atoms.

```aten
int n = select("+5,H");
```

selects the first five atoms and all hydrogens, storing the number of new atoms selected in the variable _n_.

---

## selectAll <a id="selectall"></a>

_Syntax:_

**int** **selectAll** ( )

Select all atoms in the current model. The number of selected atoms is returned.

For example:

```aten
selectAll();
```

---

## selectCode <a id="selectcode"></a>

_Syntax:_

**int** **selectCode** ( **string** _code_ )

Select atoms using the supplied _code_ snippet, which is evaluated for each atom. See the [**deSelectCode**](/aten/docs/scripting/selection/deselectcode) command for a full description.

---

## selectFFType <a id="selectfftype"></a>

_Syntax:_

**int** **selectFFType** ( **string** _ffType_ )

Select all atoms with forcefield type _ffType_ in the current model. The number of atoms selected is returned.

For example:

```aten
selectFFType("CT");
```

selects all atoms that have been assigned the forcefield type ‘CT’.

---

## selectF <a id="selectf"></a>

_Syntax:_

**int** **selectF** ( **string** _format_, ... )

Selects atoms according to a string generated from a C-style **printf** call. See the **deSelectF** command for a full description.

---

## selectInsideCell <a id="selectinsidecell"></a>

_Syntax:_

**int** **selectInsideCell** ( **bool** _useCog_ = **FALSE** )

Select all atoms whose coordinates are currently inside the confines of the unit cell (if one exists). If _useCog_ is **TRUE** whole molecules are selected if their centre of geometry is within the unit cell. The number of newly-selected atoms is returned.

For example:

```aten
int n = selectInsideCell();
```

---

## selectionCog <a id="selectioncog"></a>

_Syntax:_

[**Vector**](/aten/docs/scripting/variabletypes/vector) **selectionCog** ( )

Return the centre of geometry of the current atom selection.

For example:
```aten
Vector v = selectionCog();
printf("Centre of geometry of current selection is: %f %f %f\n", v.x, v.y, v.z);
```

calculates and prints the centre-of-geometry of the current selection.

---

## selectionCom <a id="selectioncom"></a>

_Syntax:_

[**Vector**](/aten/docs/scripting/variabletypes/vector) **selectionCom** ( )

Return the centre of mass of the current atom selection.

For example:
```aten
Vector v = selectionCom();
newAtom(Be, v.x, v.y, v.z);
```

calculates the centre-of-mass of the current selection and creates a beryllium atom at those coordinates.

---

## selectLine <a id="selectline"></a>

_Syntax:_

**int** **selectLine** ( **double** _lx_, **double** _ly_, **double** _lz_, **double** _x_, **double** _y_, **double** _z_, **double** _radius_ )

Selects all atoms that are within a distance _radius_ from a line whose direction is {_lx_,_ly_,_lz_} and which passes through the point {_x_,_y_,_z_}. The number of newly-selected atoms is returned.

For example:

```aten
selectLine(0,0,1,0,0,0,5.0);
```

selects all atoms within 5 Å of a line running through the origin along the Cartesian z direction.

---

## selectMiller <a id="selectmiller"></a>

_Syntax:_

**int** **selectMiller** ( **int** _h_, **int** _k_, **int** _l_, **int** _inside_ = **FALSE** )

Select all atoms that are ‘outside’ the specified Miller plane (and its mirror, if it has one). If the final parameter is specified as **TRUE** then atoms inside the specified Miller plane (and its mirror) are selected.

For example:

```aten
selectMiller(1, 1, 1);
```

selects all atoms located beyond the (111) plane in the unit cell.

---

## selectMolecule <a id="selectmolecule"></a>

_Syntax:_

**int** **selectMolecule** ( [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _target_ )

Select all atoms in the molecule / fragment to which the supplied _target_ atom belongs.

For example:

```aten
selectMolecule(5);
```

selects the bound fragment in which atom number 5 exists.

---

## selectName <a id="selectname"></a>

_Syntax:_

**int** **selectName** ( **string** _name_ )

Selects all atoms in the current model whose assigned type names (not equivalent names) match the supplied string. The string may contain wildcards ('*').

For example:

```aten
selectName("O*");
```

will select all atoms whose assigned type names begin with 'O'.

---

## selectNone <a id="selectnone"></a>

_Syntax:_

**void** **selectNone** ( )

Deselect all atoms in the current model.

For example:

```aten
selectNone();
```

---

## selectOverlaps <a id="selectoverlaps"></a>

_Syntax:_

**int** **selectOverlaps** ( **double** _dist_ = 0.2 )

Select all atoms that are within a certain distance of another, or the default of 0.2 Å if no argument is provided. The number of selected overlapping atoms is returned.

For example:

```aten
int noverlaps = selectOverlaps("0.1");
```

selects all atoms that are less than 0.1 Å away from another.

---

## selectOutsideCell <a id="selectoutsidecell"></a>

_Syntax:_

**int** **selectOutsideCell** ( **bool** _useCog_ = **FALSE** )

Select all atoms whose coordinates are currently outside the confines of the unit cell (if one exists). If _useCog_ is **TRUE** whole molecules are selected if their centre of geometry is outside the unit cell. The number of newly-selected atoms is returned.

For example:

```aten
int n = selectOutsideCell();
```

---

## selectPattern <a id="selectpattern"></a>

_Syntax:_

**int** **selectPattern** ( **int** _id_ | **string** _name_ | [**Pattern**](/aten/docs/scripting/variabletypes/pattern) _p_ )

Selects all atoms in the current (or named/specified) pattern. Returns the number of atoms added to the existing selection.

For example:

```aten
selectPattern(2);
```

select all atoms in the second pattern of the current model.

```aten
selectPattern("bubble");
```

select all atoms in the pattern "bubble" of the current model.

---

## selectRadial <a id="selectradial"></a>

_Syntax:_

**int** **selectRadial** ( [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _id_, **double** _r_ )

Select all atoms within _r_ Å of the supplied target atom (which is also selected). Returns the number of atoms added to the existing selection.

For example:

```aten
int nclose = selectRadial(10, 4.5);
```

selects all atoms within 4.5 Å of atom 10, and returns the number selected.

---

## selectTree <a id="selecttree"></a>

_Syntax:_

**int** **selectTree** ( [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _i_, [**Bond**](/aten/docs/scripting/variabletypes/bond) _exclude_ = (NULL) )

Select all atoms which are reachable by following chemical bonds, starting from (and including) atom _i_. If a bond to _exclude_ is provided, then this connection will not be followed during the selection process. This is useful when one wishes to select a ‘headgroup' fragment attached to an atom, without selecting the rest of the molecule. The number of atoms selected by the process is returned.

For example:

```aten
int nclose = selectTree(99);
```

selects all atoms reachable by following chemical bonds from (and including) atom 99.

---

## selectType <a id="selecttype"></a>

_Syntax:_

**int** **selectType** ( **int**|**string** _element_, **string** _neta_ )

Selects all atoms of the given _element_ that also match the NETA description (see Section 12.5) given, allowing selections to be made based on the connectivity and local environment of atoms. The number of (previously unselected) atoms matched is returned.

For example:

```aten
int nch2 = selectType("C", "-H(n=2)");
```

selects all carbon atoms that are bound to two hydrogens.

