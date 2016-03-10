---
title: FFBound
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

The [**FFBound**](/aten/docs/scripting/variabletypes/ffbound) type stores parameter data relevant to a specific bound interaction within a pattern. It differs from the [**Bound**](/aten/docs/scripting/variabletypes/bound) type in that no atom ID information is stored.
 
| Member | Type | RW | Description |
|--------|------|----|-------------|
| data | **double**[] | • | Parameter data for the potential |
| dataKeyword | **string**[] | | Keyword names of the associated parameters, up to 'nparams' |
| dataNames | **string**[] | | Proper names of the associated parameters, up to 'nparams' |
| eScale | **double**[] | • | For torsion-type interactions, the electrostatic scaling factor between atoms 1 and 4 |
| form | **string** | • | Functional form of intramolecular potential - see the Section 13 for lists of allowable functional forms for each intramolecular interaction type |
| nAtoms | **int** | | Number of atoms involved in the bound interaction |
| nParams | **int** | | The number of parameters used by the functional form of the interaction |
| type | **string** | | Actual type of the bound interaction (bond, angle, etc.) |
| typeNames | **string**[] | • | Names of the atom types the interaction is relevant to |
| vScale | **double**[] | • | For torsion-type interactions, the VDW scaling factor between atoms 1 and 4 |

## FFBound Type Functions

### parameter <a id="parameter"></a>

_Syntax:_

**double** **parameter** ( **string** _name_ )

Search for and return the value of the parameter _name_ within the bound interaction


