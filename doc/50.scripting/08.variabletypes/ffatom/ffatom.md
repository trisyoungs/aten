---
title: FFAtom
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

The [**FFAtom**](/aten/docs/scripting/variabletypes/ffatom) type stores parameter data relevant to a specific atom type in a forcefield.
 
| Member | Type | RW | Description |
|--------|------|----|-------------|
| charge | **double** | • | Charge associated to the type |
| data | **double**[6] | • | Parameter data for short-range potential |
| dataKeyword | **string**\[\] | | Keyword names of the associated parameters, up to 'nParams' |
| dataNames | **string**\[\] | | Proper names of the associated parameters, up to 'nParams' |
| description | **string** | • | Text data describing the type |
| equivalent | **string** | • | Equivalent name for the type |
| form | **string** | • | Functional form of short-range potential |
| id | **int** | | Internal ID of the atom type within its parent forcefield |
| mass | **double** | | Return the mass associated to the FFAtom, especially in the case of a united-atom type |
| name | **string** | • | Name of the type |
| neta | **string** | | The original type description used to identify the type |
| nParams | **int** | | The number of parameters used by the functional form of the interaction |
| ff | [**Forcefield**](/aten/docs/scripting/variabletypes/forcefield) | | Parent forcefield containing the type |
| z | **int** | • | Element id (Z) corresponding to the target element of this type |

# FFAtom Type Functions

## dataD <a id="datad"></a>

_Syntax:_

**double** **dataD** ( **string** _varName_ )

Return the value of the defined data item _varName_ as a double if it has been defined in a **data** block in its encompassing forcefield.

---

## dataI <a id="datai"></a>

_Syntax:_

**int** **dataI** ( **string** _varName_ )

Return the value of the defined data item _varName_ as an integer if it has been defined in a **data** block in its encompassing forcefield.

---

## dataS <a id="datas"></a>

_Syntax:_

**string** **dataS** ( **string** _varName_ )

Return the value of the defined data item _varName_ as a string if it has been defined in a **data** block in its encompassing forcefield.

---

## parameter <a id="parameter"></a>

_Syntax:_

**double** **parameter** ( **string** _name_ )

Return the value of the parameter _name_ in the associated vdW data. See VDW Functional Forms in Section 13.1 for a list of parameter names in the supported vdW functions.


