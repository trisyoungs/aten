---
title: Atom Commands
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

Define and set properties of atoms.

---

## atomStyle <a id="atomstyle"></a>

_Syntax:_

**void** **atomStyle** ( **string** _style_ )

Sets the individual drawing _style_ for the current atom selection, used when the global drawing style is individual.

For example:

```aten
atomStyle("tube");
```

sets the current atom selection to be drawn in the ‘tube’ style. See the [**DrawStyle**](/aten/docs/enums/drawstyle) enum for a list of all available styles.

---

## currentAtom <a id="currentatom"></a>

_Syntax:_

[**Atom**](/aten/docs/scripting/variabletypes/atom) **currentAtom** ( )

[**Atom**](/aten/docs/scripting/variabletypes/atom) **currentAtom** ( [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _id_ )

Return a reference to the current atom. If a new atom/id is provided the current atom is set before being returned.

For example:

```aten
Atom i = currentAtom(1);
```

makes the first atom in the current model the current atom, and returns a reference to it.

---

## fix <a id="fix"></a>

_Syntax:_

**void** **fix** ( [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _id_ = 0 )

Fix the positions of the current atom selection (or individual atom specified) so that they remain in the same position following various methods (e.g. minimisations).

For example:

```aten
fix();
```

fixes the positions of all selected atoms.

```aten
for (int n=1; n<=10; ++n) fix(n);
```

fixes the positions of the first 10 atoms in the current model.

---

## free <a id="free"></a>

_Syntax:_

**void** **free** ( [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _id_ = 0 )

Free the positions of previously fixed atoms in the current selection (or the individual atom specified).

For example:

```aten
free(5);
```

allows the fifth atom in the current model to be moved again.

---

## getAtom <a id="getatom"></a>

_Syntax:_

[**Atom**](/aten/docs/scripting/variabletypes/atom) **getAtom** ( [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _id_ )

Return a reference to the atom specified.

For example:

```aten
Atom i = getAtom(3);
```

returns a reference to the third atom in the current model.

---

## hide <a id="hide"></a>

_Syntax:_

**void** **hide** ( [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _id_ = 0 )

Hides the current selection of atoms (or the supplied atom) from view, meaning they cannot be selected by direct clicking/highlighting in the GUI. They are still subject to transformation if they are selected by other means.

For example:
```aten
select(H);
hide();
```

selects and hides all hydrogen atoms in the current model.

---

## setCharge <a id="setcharge"></a>

_Syntax:_

**void** **setCharge** ( **double** _q_ )

**void** **setCharge** ( **double** _q_, **int** _id_ )

Set the atomic charge of the current (or specified) atom.

---

## setCoords <a id="setcoords"></a>

_Syntax:_

**void** **setCoords** ( **double** _x_, **double** _y_, **double** _z_ )

**void** **setCoords** ( **double** _x_, **double** _y_, **double** _z_, **int** _id_ )

Set the coordinates of the current (or specified) atom.

---

## setElement <a id="setelement"></a>

_Syntax:_

**void** **setElement** ( **string**|**int** _element_ )

**void** **setElement** ( **string**|**int** _element_, **int** _id_ )

Set the element of the current (or specified) atom.

---

## setForces <a id="setforces"></a>

_Syntax:_

**void** **setForces** ( **double** _fx_, **double** _fy_, **double** _fz_ )

**void** **setForces** ( **double** _fx_, **double** _fy_, **double** _fz_, **int** _id_ )

Set the forces of the current (or specified) atom.

---

## setFx <a id="setfx"></a>

_Syntax:_

**void** **setFx** ( **double** _d_ )

**void** **setFx** ( **double** _d_, **int** _id_ )

Set the x force of the current (or specified) atom.

---

## setFy <a id="setfy"></a>

_Syntax:_

**void** **setFy** ( **double** _d_ )

**void** **setFy** ( **double** _d_, **int** _id_ )

Set the y force of the current (or specified) atom.

---

## setFz <a id="setfz"></a>

_Syntax:_

**void** **setFz** ( **double** _d_ )

**void** **setFz** ( **double** _d_, **int** _id_ )

Set the z force of the current (or specified) atom.

---

## setRx <a id="setrx"></a>

_Syntax:_

**void** **setRx** ( **double** _d_ )

**void** **setRx** ( **double** _d_, **int** _id_ )

Set the x coordinate of the current (or specified) atom.

---

## setRy <a id="setry"></a>

_Syntax:_

**void** **setRy** ( **double** _d_ )

**void** **setRy** ( **double** _d_, **int** _id_ )

Set the y coordinate of the current (or specified) atom.

---

## setRz <a id="setrz"></a>

_Syntax:_

**void** **setRz** ( **double** _d_ )

**void** **setRz** ( **double** _d_, **int** _id_ )

Set the z coordinate of the current (or specified) atom.

---

## setVelocities <a id="setvelocities"></a>

_Syntax:_

**void** **setVelocities** ( **double** _vx_, **double** _vy_, **double** _vz_ )

**void** **setVelocities** ( **double** _vx_, **double** _vy_, **double** _vz_, **int** _id_ )

Set the velocity components of the current (or specified) atom.

---

## setVx <a id="setvx"></a>

_Syntax:_

**void** **setVx** ( **double** _d_ )

**void** **setVx** ( **double** _d_, **int** _id_ )

Set the x velocity of the current (or specified) atom.

---

## setVy <a id="setvy"></a>

_Syntax:_

**void** **setVy** ( **double** _d_ )

**void** **setVy** ( **double** _d_, **int** _id_ )

Set the y velocity of the current (or specified) atom.

---

## setVz <a id="setvz"></a>

_Syntax:_

**void** **setVz** ( **double** _d_ )

**void** **setVz** ( **double** _d_, **int** _id_ )

Set the z velocity of the current (or specified) atom.

---

## show <a id="show"></a>

_Syntax:_

**void** **show** ( [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _id_ = 0 )

Makes the current selection of atoms (or the supplied atom) visible again if they were previously hidden.

