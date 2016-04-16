---
title: Build Commands
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

Tools to build molecules from scratch, or finalise unfinished models. When creating atoms using the commands listed below, if the coordinates of the new atom are not specified then it is placed at the current pen position. In addition, the reference frame of the pen position is represented as a set of three orthogonal vectors defining the pen's local coordinate system (set initially to the Cartesian axes) centred at an arbitrary origin (the pen position). Subsequent rotations operate on these coordinate axes. Think of it as a 3D version of the old-school turtle.

---

## addHydrogen <a id="addhydrogen"></a>

_Syntax:_

**void** **addHydrogen** ( )

**void** **addHydrogen** ( [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _i_ )

Satisfy the valencies of all atoms in the current model by adding hydrogens to heavy atoms. If an integer id or atom reference is provided as the argument then the addition of hydrogen is restricted to the specified atom.

For example:

```aten
addHydrogen();
```

add hydrogens to all atoms in the current model.

```aten
addHydrogen(10);
```

add hydrogens to atom 10 only.

---

## bohr <a id="bohr"></a>

_Syntax:_

**void** **bohr** ( **object** _x_, ... )

Converts the specified object(s) data to Ǻ, assuming that it is currently specified in Bohr.

For example:
```aten
Atom i = aten.model.atoms[2];
bohr(i);
```

converts the coordinates of the supplied atom from Bohr to Ǻ.

---

## chain <a id="chain"></a>

_Syntax:_

[**Atom**](/aten/docs/scripting/variabletypes/atom) **chain** ( **int**|**string** _el_ )

[**Atom**](/aten/docs/scripting/variabletypes/atom) **chain** ( **int**|**string** _el_, **int**|**string** _bondtype_ )

[**Atom**](/aten/docs/scripting/variabletypes/atom) **chain** ( **int**|**string** _el_, **double** _x_, **double** _y_, **double** _z_ )

[**Atom**](/aten/docs/scripting/variabletypes/atom) **chain** ( **int**|**string** _el_, **double** _x_, **double** _y_, **double** _z_, **int**|**string** _bondtype_ )

Create a new atom of element _el_ at the current pen position (or the specified coordinates) bound to the last drawn atom with a single bond (or of type _bondtype_ if it was specified). The element can be provided as a character string containing the element symbol or element name instead of the integer atomic number. A reference to the new atom is returned.

For example:

```aten
Atom i = chain("C");
```

places a carbon atom at the current pen coordinates, and creates a single bond with the last drawn atom.

```aten
Atom i = chain(8, "double");
```

places an oxygen atom at the current pen coordinates, and creates a double bond with the last drawn atom.

```aten
Atom i = chain("Cl", 4.0, 5.0, 6.0, "single");
```

creates a chlorine at coordinates { 4.0, 5.0, 6.0 }, joined by a single bond to the last drawn atom.

---

## endChain <a id="endchain"></a>

_Syntax:_

**void** **endChain** ( )

Ends the current chain (so that the next atom drawn with ‘**chain**’ will be unbound).

For example:

```aten
endChain();
```

---

## growAtom <a id="growatom"></a>

_Syntax:_

[**Atom**](/aten/docs/scripting/variabletypes/atom) **growAtom** ( [**Element**](/aten/docs/scripting/variabletypes/element) _el_, [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** [var]*i[/var], **string** _geometry_ = "tetrahedral", **double** _distance_ = (auto), **bool** _bond_ = **true** )

Grows a new atom of the specified element from the specified target atom _i_. The position of the new atom will conform to the required geometry, and will have the specified _distance_ from the target atom.  If the supplied _distance_ is negative, then the atomic radii of the target and new atoms are used to calculate the new bond length.  If the target atom already has enough (or too many) atoms to prevent a new position from being calculated, then no atom is added.  If a new atom is successfully added, it is returned. The default is to create a bond between the target and new atoms, but this can be restricted by passing **false** as the _bond_ argument.

For example:
```aten
Atom i = newAtom(P);
for (int n=0; n<6; ++n) growAtom(F, i, "octahedral");
```

creates a PF6 (anion), with a perfect octahedral arrangement of F atoms, and calculating the bond distance from the atomic radii of P and F atoms.

---

## locate <a id="locate"></a>

_Syntax:_

**void** **locate** ( **double** _x_, **double** _y_, **double** _z_ )

Sets the pen position to the coordinates specified (in Ǻ).

For example:

```aten
locate(0.0, 0.0, 0.0);
```

moves the pen back to the coordinate origin.

---

## move <a id="move"></a>

_Syntax:_

**void** **move** ( **double** _x_, **double** _y_, **double** _z_ )

Moves the pen position by the amounts specified (in Ǻ).

For example:

```aten
move(1.0, 1.0, 0.0);
```

moves the pen one Ǻ in both the positive x and y directions.

---

## moveToEnd <a id="movetoend"></a>

_Syntax:_

**void** **moveToEnd** ( )

Move the current atom selection to the end of the list. The relative order of atoms in the selection is preserved.

For example:
```aten
select(H);
moveToEnd();
```

moves all hydrogen atoms to the end of the atom list.

---

## moveToStart <a id="movetostart"></a>

_Syntax:_

**void** **moveToStart** ( )

Move the current atom selection to the start of the list. The relative order of the atoms in the selection is preserved.

For example:
```aten
selectType(O, "nbonds=2,-H(n=2)");
moveToStart();
```

moves all water molecules to the start of the atom list.

---

## newAtom <a id="newatom"></a>

_Syntax:_

**void** **newAtom** ( **int**|**string** _el_ )

**void** **newAtom** ( **int**|**string** _el_, **double** _x_, **double** _y_, **double** _z_ )

**void** **newAtom** ( **int**|**string** _el_, **double** _x_, **double** _y_, **double** _z_, **double** _vx_, **double** _vy_, **double** _vz_ )

**void** **newAtom** ( **int**|**string** _el_, **double** _x_, **double** _y_, **double** _z_, **double** _vx_, **double** _vy_, **double** _vz_, **double** _fx_, **double** _fy_, **double** _fz_ )

Create a new atom of element _el_ at the current pen position or, if provided, the specified coordinates (and optional velocities or velocities and forces). Either the integer atomic number or the symbol/name of the element may be used to identify the desired element. A reference to the new atom is returned.

For example:

```aten
Atom i = newAtom("N");
```

places a nitrogen atom at the current pen coordinates.

```aten
Atom i = newAtom(18, 5.2, 0, 0);
```

places an argon atom at the coordinates { 5.2, 0.0, 0.0 }.

---

## newAtomFrac <a id="newatomfrac"></a>

_Syntax:_

**void** **newAtomFrac** ( **int**|**string** _el_, **double** _x_, **double** _y_, **double** _z_ )

**void** **newAtomFrac** ( **int**|**string** _el_, **double** _x_, **double** _y_, **double** _z_, **double** _vx_, **double** _vy_, **double** _vz_ )

**void** **newAtomFrac** ( **int**|**string** _el_, **double** _x_, **double** _y_, **double** _z_, **double** _vx_, **double** _vy_, **double** _vz_, **double** _fx_, **double** _fy_, **double** _fz_ )

Create a new atom of element _el_ at the specified fractional coordinates (velocities and forces are optional). Either the integer atomic number or the symbol/name of the element may be used to identify the desired element. A reference to the new atom is returned.

For example:

```aten
Atom i = newAtomFrac("C", 0.5, 0.5, 0.5);
```

places a carbon atom at the centre of the model’s cell.

---

## reorder <a id="reorder"></a>

_Syntax:_

**void** **reorder** ( )

Adjust the ordering of atoms in the current selection such that atoms in bound fragments/molecules have successive IDs. Useful to recover 'molecularity' in order to apply a suitable pattern description to the system.

For example:

```aten
reorder();
```

---

## rotX <a id="rotx"></a>

_Syntax:_

**void** **rotX** ( **double** _angle_ )

Rotates the reference coordinate system about the x axis by _angle_ degrees.

For example:

```aten
rotX(90.0);
```

rotates around the x axis by 90 degrees.

---

## rotY <a id="roty"></a>

_Syntax:_

**void** **rotY** ( **double** _angle_ )

Rotates the reference coordinate system about the y axis by _angle_ degrees.

For example:

```aten
rotY(45.0);
```

rotates around the y axis by 45 degrees.

---

## rotZ <a id="rotz"></a>

_Syntax:_

**void** **rotZ** ( **double** _angle_ )

Rotates the reference coordinate system about the z axis by _angle_ degrees.

For example:

```aten
rotZ(109.5);
```

rotates around the z axis by 109.5 degrees.

---

## selectionAddHydrogen <a id="selectionaddhydrogen"></a>

_Syntax:_

**void** **selectionAddHydrogen** ( )

Adds hydrogen atoms to the current atom selection only.

---

## selectionGrowAtom <a id="selectiongrowatom"></a>

_Syntax:_

**void** **selectionGrowAtom** ( [**Element**](/aten/docs/scripting/variabletypes/element) _el_, **string** _geometry_ = "tetrahedral", **double** _distance_ = (auto), **bool** _bond_ = **true** )

Grows a new atom on each of the currently selected atoms, conforming to the specified geometry and distance (if provided). See the [**growAtom**](/aten/docs/scripting/commands/building#growatom) command for more information.

---

## shiftDown <a id="shiftdown"></a>

_Syntax:_

**void** **shiftDown** ( )

**void** **shiftDown** ( **int** _n_ )

Move the current atom selection one (or _n_) places down in the atom list (i.e. towards higher IDs).

For example:

```aten
shiftDown(4);
```

moves the current atom selection down four places.

---

## shiftUp <a id="shiftup"></a>

_Syntax:_

**void** **shiftUp** ( )

**void** **shiftUp** ( **int** _n_ )

Move the current atom selection one (or _n_) places up in the atom list (i.e. towards lower IDs).

For example:

```aten
shiftUp();
```

moves the current atom selection up one place.

---

## transmute <a id="transmute"></a>

_Syntax:_

**void** **transmute** ( **int**|**string** _el_ )

Transmute the current atom selection to the specified element.

For example:

```aten
transmute("F");
```

changes all atoms in the current selection to fluorine.

```aten
transmute(Cl);
```

changes all atoms in the current selection to chlorine.

```aten
transmute(6);
```

changes all atoms in the current selection to carbon

