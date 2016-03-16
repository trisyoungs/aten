---
title: BasisShell
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

The [**BasisShell**](/aten/docs/scripting/variabletypes/basisshell) type contains information about a basis shell centred on a specific atom in a model, allowing a full basis set specification to be associated with a system.

| Member | Type | RW | Description |
|--------|------|----|-------------|
| atomId | **int** | • | Atom ID on which the basis shell is centred |
| nPrimitives | **int** | | Number of primitives defined for the basis shell |
| primitives | [**BasisPrimitive**](/aten/docs/scripting/variabletypes/basisprimitive)[] | | List of primitives defined for the basis shell |
| type | **string** | • | The type (shape) of the basis shell. See Basis Shell Types in Section 16.1 for a list. |

## BasisShell Type Functions

### addPrimitive <a id="addprimitive"></a>

_Syntax:_

[**BasisPrimitive**](/aten/docs/scripting/variabletypes/basisprimitive) **addprimitive** ( **double** _exponent_, **double** _coeff1_ = 0.0 ... )

Add a new primitive to the basis shell, with the specified exponent and (optional) coefficients given.


