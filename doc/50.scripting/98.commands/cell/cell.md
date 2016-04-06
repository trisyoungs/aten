---
title: Cell Commands
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

Create, remove, modify, pack, and replicate the model's unit cell.

---

## adjustCell <a id="adjustcell"></a>

_Syntax:_

**void** **adjustCell** ( **string** _parameter_, **double** _value_ )

Adjust a single unit cell parameter (one of _a_, _b_, _c_, _alpha_, _beta_, _gamma_, or one of the matrix elements _ax_, _ay_, _az_, ..., _cz_) by the given _value_. This does not set the specified parameter to the given _value_; instead the supplied _value_ is added to the existing value of the parameter.

For example:


```
adjustCell("alpha",5.0);
```


increases the cell angle ‘alpha’ by 5 degrees.


```
adjustcell("c",-10.0);
```


decreases the cell length ‘c’ by 10 Ǻ.

---

## cell <a id="cell"></a>

_Syntax:_

**void** **cell** ( **double** _a_, **double** _b_, **double** _c_, **double** _alpha_, **double** _beta_, **double** _gamma_ )

Set cell lengths and angles of current model. This command will modify an existing cell or add a new cell to a model currently without a unit cell specification.

For example:


```
cell(20.0, 10.0, 10.0, 90.0, 90.0, 90.0);
```


sets the model's cell to be orthorhombic with side lengths 20x10x10 Ǻ.

---

## cellAxes <a id="cellaxes"></a>

_Syntax:_

**void** **cellAxes** ( **double** _ax_, **double** _ay_, **double** _az_, **double** _bx_, **double** _by_, **double** _bz_, **double** _cx_, **double** _cy_, **double** _cz_ )

Set cell axes of current model. This command will modify an existing cell or add a new cell to a model currently without a unit cell specification.

For example:


```
cellAxes(15, 0, 0, 0, 15, 0, 0, 0, 15);
```


sets the model’s cell to be cubic with side length 15 Ǻ.

---

## fold <a id="fold"></a>

_Syntax:_

**void** **fold** ( )

Fold all atoms so they are within the boundaries of the unit cell.

For example:


```
fold();
```


---

## foldMolecules <a id="foldmolecules"></a>

_Syntax:_

**void** **foldMolecules** ( )

Fold all pattern molecules so the are unbroken across cell boundaries.

For example:


```
foldMolecules();
```


---

## millerCut <a id="millercut"></a>

_Syntax:_

**void** **millerCut** ( **int** _h_, **int** _k_, **int** _l_, **bool** _inside_ = **FALSE** )

Remove all atoms from the unit cell that lay ‘outside’ the specified Miller plane (and its mirror, if it has one). If the final parameter is given as TRUE, then atoms 'inside' the bounding Miller plane(s) are selected.

For example:


```
millerCut(1,2,1,TRUE);
```


removes all atoms inside the two enclosing (121) planes.

---

## noCell <a id="nocell"></a>

_Syntax:_

**void** **noCell** ( )

Clears any cell description (removes periodic boundary conditions) from the current model.

For example:


```
noCell();
```


---

## pack <a id="pack"></a>

_Syntax:_

**void** **pack** ( )

Perform spacegroup packing on the current model.

For example:


```
pack();
```


---

## printCell <a id="printcell"></a>

_Syntax:_

**void** **printCell** ( )

Prints the cell parameters of the current model.

For example:


```
printCell();
```


---

## replicate <a id="replicate"></a>

_Syntax:_

**void** **replicate** ( **double** _negx_, **double** _negy_, **double** _negz_, **double** _posx_, **double** _posy_, **double** _posz_ , **bool** _fold_ = **false**, **bool** _trim_ = **false** )

Create a supercell of the current model, creating copies of the cell in each of the three cell axis directions. The original cell is considered to span {0,0,0} to {1,1,1}, so the number of additional cells should be specified beyond these initial numbers. Thus:


```
replicate(0, 0, 0, 1, 1, 1);
```


will do nothing at all to the model, while:


```
replicate(-5, 0, 0, 6, 1, 1);
```

will result in a supercell that consists of eleven copies of the original cell along the x-axis direction. Similarly,

```
replicate(0, 0, 0, 5, 5, 5);
replicate(-2, -2, -2, 3, 3, 3);
```

will both create a 5x5x5 arrangement of the original cell.

Optionally, atoms can be folded into the original unit cell before replicating by passing **true** as the _fold_ argument. Similarly, excess atoms that sit outside the unit cell after replication can be removed by passing **true** as the _trim_ argument.

---

## scale <a id="scale"></a>

_Syntax:_

**void** **scale** ( **double** _x_, **double** _y_, **double** _z_ )

Scale unit cell and its constituent atoms by the scale factors _x_, _y_, and _z_.

For example:


```
scale(1.0, 2.0, 1.0);
```


doubles the length of the y-axis of the system. x- and z-axes remain unchanged.

---

## scaleMolecules <a id="scalemolecules"></a>

_Syntax:_

**void** **scaleMolecules** ( **double** _x_, **double** _y_, **double** _z_ )

Scale unit cell and centres-of-geometry of molecules within it by the scale factors _x_, _y_, and _z_. Within individual molecules the relative distances between atoms stays the same, but the centres-of-geometry of other molecules do not.

For example:


```
scaleMolecules(0.5, 0.5, 0.5);
```


halves the lengths of all axes, scaling the positions of the molecules to reflect the new size.

---

## setCell <a id="setcell"></a>

_Syntax:_

**void** **setCell** ( **string** _parameter_, **double** _value_ )

Set a single unit cell parameter (one of _a_, _b_, _c_, _alpha_, _beta_, _gamma_, or one of the matrix elements _ax_, _ay_, _az_, ..., _cz_) to the given _value_.

For example:


```
setCell("beta", 101.0);
```


sets the cell angle _beta_ to 101 degrees.


```
setCell("a", 15.5);
```


sets the cell length _a_ to 15.5 Ǻ.

---

## spacegroup <a id="spacegroup"></a>

_Syntax:_

**void** **spacegroup** ( **int**|**string** _sg_ )

Sets the spacegroup of the model, used for crystal packing.

For example:


```
spacegroup(12);
```


sets the model spacegroup to be C2/m (number 12).


```
spacegroup("P1/m");
```


sets the model spacegroup to P1/m.


