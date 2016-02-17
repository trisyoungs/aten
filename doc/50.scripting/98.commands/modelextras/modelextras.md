---
title: Model Extra Commands
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

Store and manipulate molecular orbital data, vibrations, and z-matrix elements.

---

## newBasisShell <a id="newbasisshell"></a>

_Syntax:_

[BasisShell](/aten/docs/scripting/variabletypes/basisshell) **newBasisShell** ( [Atom](/aten/docs/scripting/variabletypes/atom)|**int** _id_, **string** _type_ )

Adds a new basis shell definition to the current model, returning the generated structure. The atomic centre on which the basis function exists must be provided either as an integer or an atom pointer (from which the integer ID is extracted). For allowed _type_s see the (BasisShellType)[/aten/docs/enumerations/basisshelltype].

For example:


```
BasisShell = newBasisShell(15, "D");
```


creates a new D-orbital shell centred on atom 15.

---

## newEigenvector <a id="neweigenvector"></a>

_Syntax:_

[Eigenvector](/aten/docs/scripting/variabletypes/eigenvector) **newEigenvector** ( **int** _size_ = (auto) )

Adds a new, empty eigenvector to the current model. If the _size_ argument is given the eigenvector array will contain this many elements. Otherwise, the size of the array is determined by the total number of cartesian basis functions implied by the current basis shell definitions of the model.

For example:


```
Eigenvector = newEigenvector(180);
```


creates a new eigenvector which will contain 180 coefficients.

---

## newVibration <a id="newvibration"></a>

_Syntax:_

[Vibration](/aten/docs/scripting/variabletypes/vibration) **newVibration** ( **string** _name_ = (auto) )

Adds a new, empty vibration definition to the current model.

---

## printZMatrix <a id="printzmatrix"></a>

_Syntax:_

**void** **printZMatrix** ( )

Prints a Z-matrix for the current model to the console, creating one first if necessary.


