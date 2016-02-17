---
title: Charge Commands
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

Assign partial charges to models, atoms, and patterns.

---

## charge <a id="charge"></a>

_Syntax:_

**double** **charge** ( )

**double** **charge** ( **double** _q_ )

Assigns a charge of _q_ to each selected atom in the current model, or returns the total charge of the current selection if no value is supplied.

For example:


```
charge(1.0);
```


gives each atom in the current model's selection a charge of 1.0.

## chargeFF <a id="chargeff"></a>

_Syntax:_

**void** **chargeFF** ( )

Assigns charges to all atoms in the current model based on the forcefield associated to the model and the current types of the atoms.

For example:


```
chargeFF();
```


---

## chargeFromModel <a id="chargefrommodel"></a>

_Syntax:_

**void** **chargeFromModel** ( )

Copies charges of all atoms in the current model to the atoms of the current trajectory frame.

For example:


```
chargeFromModel();
```


---

## chargePAtom <a id="chargepatom"></a>

_Syntax:_

**void** **chargePAtom** ( **int** _id_, **double** _q_ )

Assigns a charge of _q_ to atom _id_ in each molecule of the current pattern.

For example:


```
chargePAtom(3, 0.1);
```


assigns a charge of 0.1 to the third atom in each molecule of the current pattern.

---

## chargeType <a id="chargetype"></a>

_Syntax:_

**void** **chargeType** ( **string** _fftype_, **double** _q_ )

Assigns a charge of _q_ to each atom that is of type _fftype_ in the current model.

For example:


```
chargeType("OW",-0.8);
```


gives a charge of -0.8 to every atom that has an assigned typename of _OW_.

---

## clearCharges <a id="clearcharges"></a>

_Syntax:_

**void** **clearCharges** ( )

Clears all charges in the current model, setting them to zero.

For example:


```
clearCharges();
```



