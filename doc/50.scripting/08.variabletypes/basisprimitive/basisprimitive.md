---
title: BasisPrimitive
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

The [**BasisPrimitive**](/aten/docs/scripting/variabletypes/basisprimitive) type provides access to basis primitive coefficient and exponent information in a basis shell.

| Member | Type | RW | Description |
|--------|------|----|-------------|
| exponent | **double** | • | Exponent of the basis primitive |
| coefficients | **double**\[\] | • | Coefficients of the basis primitive |

# BasisPrimitive Type Functions

## addCoefficient <a id="addcoefficient"></a>

_Syntax:_

**void** **addCoefficient** ( **double** _coeff_ )

Add a new coefficient to the basis primitive.


