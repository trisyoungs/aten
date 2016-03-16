---
title: Bound
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

The [**Bound**](/aten/docs/scripting/variabletypes/bound) type is used by the pattern type, and defines a single bound interaction (e.g. a bond, angle, or torsion) between a group of atoms within the pattern. It differs from the [**FFBound**](/aten/docs/scripting/variabletypes/ffbound) type since no forcefield information is stored locally in the structure.
 
| Member | Type | RW | Description |
|--------|------|----|-------------|
| data | **double** | | Parameters describing the bound interaction |
| eScale | **double** | | Electrostatic 1-4 scaling factor (for torsion interactions) |
| form | **string** | | Functional form of the bound interaction |
| id | **int**[] | | Array of atom IDs involved in the interaction |
| termId | **int** | | Array index of forcefield term in relevant list (ffangles, ffbonds, or fftorsions) in local pattern |
| type | **string** | | Returns the Bound Type (Section 16.3) of the interaction. |
| typeNames | **string**[] | | Array of typenames involved in the interaction |
| vScale | **double** | | Short-range 1-4 scaling factor (for torsion interactions) |

## Bound Type Functions

### parameter <a id="parameter"></a>

_Syntax:_

**double** **parameter** ( **string** _keyword_ )

Search for and return the named parameter of the bound interaction


