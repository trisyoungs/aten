---
title: Forcefield
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

The [**Forcefield**](/aten/docs/scripting/variabletypes/forcefield) type stores a complete set of atom types and ffbound interaction data.
 
| Member | Type | RW | Description |
|--------|------|----|-------------|
| energyGenerators | **int**\[\] | • | Array of integers flagging which generator data are ‘energetic’ and should be converted |
| filename | **string** | • | Filename if the forcefield was loaded from a file |
| name | **string** | • | Name of the forcefield |
| nAngles | **integer** | | Number of angle terms defined in the forcefield |
| nAtomtypes | **integer** | | Number of atomtypes defined in the forcefield |
| nBonds | **integer** | | Number of bond terms defined in the forcefield |
| nImpropers | **integer** | | Number of improper dihedral terms defined in the forcefield |
| nTorsions | **integer** | | Number of torsion terms defined in the forcefield |

# Forcefield Type Functions

## addAngle <a id="addangle"></a>

_Syntax:_

[**FFBound**](/aten/docs/scripting/variabletypes/ffbound) **addAngle** ( **string** _form_, **string** _type_i_, **string** _type_j_, **string** _type_k_, **double** _data1_>, ... )

Create a new angle definition in the forcefield. See the **angleDef** command for a full description.

---

## addBond <a id="addbond"></a>

_Syntax:_

[**FFBound**](/aten/docs/scripting/variabletypes/ffbound) **addBond** ( **string** _form_, **string** _type_i_, **string** _type_j_, **double** _data1_, ... ) 

Create a new bond definition in the forcefield. See the **bondDef** command for a full description.

---

## addInter <a id="addinter"></a>

_Syntax:_

[**FFBound**](/aten/docs/scripting/variabletypes/ffbound) **addInter** ( **string** _form_, **int** _typeid_, **double** _charge_, **double** _data1_, ... ) 

Create a new interatomic definition in the forcefield. See the **interDef** command for a full description.

---

## addTorsion <a id="addtorsion"></a>

_Syntax:_

[**FFBound**](/aten/docs/scripting/variabletypes/ffbound) **addTorsion** ( **string** _form_, **string** _type_i_, **string** _type_j_, **string** _type_k_, **string** _type_l_, **double** _data1_, ... ) 

Create a new torsion definition in the forcefield. See the **torsionDef** command for a full description.

---

## addType <a id="addtype"></a>

_Syntax:_

[**FFAtom**](/aten/docs/scripting/variabletypes/ffatom) **addType** ( **int** _typeid_, **string** _name_, **string** _equiv_, **string|int** _element_, **string** _neta_, **string** _description_ = &quot;&quot; ) 

Create a new type definition in the forcefield. See the **typeDef** command for a full description.

---

## finalise <a id="finalise"></a>

_Syntax:_

**void** **finalise** ( )

Finalise the forcefield. See the **finaliseFF** command for a full description.

---

## findAngle <a id="findangle"></a>

_Syntax:_

[**FFBound**](/aten/docs/scripting/variabletypes/ffbound) **findAngle** ( **string** _type_i_, **string** _type_j_, **string** _type_k_ ) 

Search for an existing angle definition in the forcefield between the type names supplied.

---

## findBond <a id="findbond"></a>

_Syntax:_

[**FFBound**](/aten/docs/scripting/variabletypes/ffbound) **findBond** ( **string** _type_i_, **string** _type_j_ ) 

Search for an existing bond definition in the forcefield between the type names supplied.

---

## findImproper <a id="findimproper"></a>

_Syntax:_

[**FFBound**](/aten/docs/scripting/variabletypes/ffbound) **findImproper** ( **string** _type_i_, **string** _type_j_, **string** _type_k_, **string** _type_l_ ) 

Search for an existing improper torsion definition in the forcefield between the type names supplied.

---

## findTorsion <a id="findtorsion"></a>

_Syntax:_

[**FFBound**](/aten/docs/scripting/variabletypes/ffbound) **findTorsion** ( **string** _type_i_, **string** _type_j_, **string** _type_k_, **string** _type_l_ )

Search for an existing torsion definition in the forcefield between the type names supplied.

---

## findUreyBradley <a id="findureybradley"></a>

_Syntax:_

[**FFBound**](/aten/docs/scripting/variabletypes/ffbound) **findUreyBradley** ( **string** _type_i_, **string** _type_j_, **string** _type_k_ ) 

Search for an existing Urey-Bradley definition in the forcefield between the type names supplied.


