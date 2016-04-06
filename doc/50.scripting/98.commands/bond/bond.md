---
title: Bond Commands
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

Create bonds and perform automatic bonding operations.

---

## augment <a id="augment"></a>

_Syntax:_

**void** **augment** ( )

Augments bonds in the current model, automatically determining multiple bonds based on the valency of atoms, and aromaticity based on double bonds in rings.

For example:


```
augment();
```


Augment method for a description of the rebonding algorithm implemented in Aten

---

## bondTolerance <a id="bondtolerance"></a>

_Syntax:_

**double** **bondTolerance** ( )

**double** **bondTolerance** ( **double** _tol_ )

Adjust the bond calculation tolerance. It may often be necessary to tweak the bond tolerance in order to get Aten to recognise patterns within models correctly. The current or new bond tolerance is returned.

For example:


```
bondTolerance(1.20);
```


sets the bonding tolerance to 1.2.


```
double tol = bondTolerance();
```


retrieve the current bond tolerance. See Section 15.1.5 for a description of the rebonding algorithm implemented in Aten

---

## clearBonds <a id="clearbonds"></a>

_Syntax:_

**void** **clearBonds** ( )

Delete all bonds in the current model.

For example:


```
clearBonds();
```


---

## clearSelectedBonds <a id="clearselectedbonds"></a>

_Syntax:_

**void** **clearSelectedBonds** ( )

Delete all bonds in the current atom selection.

For example:


```
clearSelectedBonds();
```


---

## newBond <a id="newbond"></a>

_Syntax:_

**void** **newBond** ( [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _i_, [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _j_, )

**void** **newBond** ( [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _i_, [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _j_, **string**|**int** _bondtype_ )

Create a new bond in the model between the specified atoms. The optional _bondtype_ argument specified the type of bond: e.g. single (default), double, or triple. Alternatively, an integer number representing the bond order may be given.

For example:


```
newBond(4, 5, "double");
```


creates a double bond between the fourth and fifth atoms in the model.


```
newBond(1, 2, 3);
```


creates a triple bond between the first and second atoms in the model.

```
Atom i,j;
i = newAtom("C", 0, 0, 0);
j = newAtom("H", 0, 1.08, 0);
newBond(i, j, "single");
```

creates a new single bond between two atoms, supplied as references.

---

## rebond <a id="rebond"></a>

_Syntax:_

**void** **rebond** ( )

Calculate bonding in the current model.

For example:


```
rebond();
```


---

## rebondPatterns <a id="rebondpatterns"></a>

_Syntax:_

**void** **rebondPatterns** ( )

Calculate bonding in the current model, but restrict bond creation to between atoms in individual molecules of defined patterns.

For example:


```
rebondPatterns();
```


This command is useful when molecules in a system are too close together to have the correct bonding detected. In such a case, bonds and any old patterns in the model may be cleared, new patterns created by hand, and then **rebondPatterns** used to calculate bonds only between the atoms of individual molecules in the defined patterns in order to recreate the original molecules.

For example:

```
# Delete existing bonds in model
clearBonds();

# Delete any existing patterns in the model 
clearPatterns();

# Add new pattern: 100 molecules of benzene (12 atoms), followed by... 
newPattern("benzene", 100, 12);

# ...50 molecules of diethyl-ether (15 atoms)
newPattern("ether", 50, 15);

# Calculate bonds within individual benzene and ether molecules
rebondPatterns();
```

---

## rebondSelection <a id="rebondselection"></a>

_Syntax:_

**void** **rebondSelection** ( )

Calculate bonding between atoms in the current atom selection.

For example:


```
rebondSelection();
```



