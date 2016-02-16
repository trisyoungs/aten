---
title: Forcefield Commands
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

Forcefield management and manual term creation.

---

## angleDef <a id="angledef"></a>

_Syntax:_

**void** **angleDef** ( **string** _form_, **string** _type_i_, **string** _type_j_, **string** _type_k_, **double** _data1_ ... )

Add an angle definition to the current forcefield. _form_ should correspond to one of the implemented angle functional forms, while the three _types_ refer to either type or equivalent names of defined atom types. Up to ten data parameters may be supplied.

---

## autoConversionUnit <a id="autoconversionunit"></a>

_Syntax:_

**void** **autoConversionUnit** ( **string** _unit_ = "" )

Sets the target energy unit for automatic conversion of energetic forcefield parameters when writing out expresisons. Can only be used within a file filter definition. The 'unit' parameter should correspond to one of the energy units recognised by Aten (see energy units) or may be omitted to specify that no conversion of parameters from the current internal unit of energy should take place. Note that the conversion of energetic forcefield term parameters is performed only when accessing data through either the 'data' member or 'parameter' function of the forcefield atom, forcefield bound or bound variable types.

For example:


```
autoConversionUnit("kcal");
```


indicates that, no matter what the current internal unit of energy is, all energetic forcefield parameters, when accessed by the means listed above, will be automatically converted into units of kcal.

---

## bondDef <a id="bonddef"></a>

_Syntax:_

**void** **bondDef** ( **string** _form_, **string** _type_i_, **string** _type_j_, **double** _data1_ ... )

Add a bond definition to the current forcefield. _form_ should correspond to one of the implemented bond functional forms, while the two _types_ refer to either type or equivalent names of defined atom types. Up to ten data parameters may be supplied.

---

## clearExportMap <a id="clearexportmap"></a>

_Syntax:_

**void** **clearExportMap** ( )

Clear any manual export typemap definitions.

For example:


```
clearExportMap();
```


---

## clearExpression <a id="clearexpression"></a>

_Syntax:_

**void** **clearExpression** ( )

Removes any forcefield expression defined for the current model.

For example:


```
clearExpression();
```


---

## clearMap <a id="clearmap"></a>

_Syntax:_

**void** **clearMap** ( )

Clear any manual typemap definitions.

For example:


```
clearMap();
```


---

## clearTypes <a id="cleartypes"></a>

_Syntax:_

**void** **clearTypes** ( )

Clear any previously-assigned atom types from the current model.

For example:


```
clearTypes();
```


---

## createExpression <a id="createexpression"></a>

_Syntax:_

**int** **createExpression** ( **bool** _noIntra_ = **FALSE**, **bool** _noDummy_ = **FALSE**, **bool** _assignCharges_ = **TRUE** )

Creates a suitable energy description for the current model. The optional flags control exactly what is in the created expression, or how it is created. The _noIntra_ flag can be used to force the creation of an expression containing only atomtype (i.e. van der Waals) terms - in such a case, patterns will contain no definitions of intramolecular bonds, angles, and torsions whatsoever. The _noDummy_ option indicates whether dummy intramolecular terms (of simple functional form and with all parameters set to zero) should be automatically added to the energy expression should no suitable terms be found in the associated forcefield(s). Finally, _assignCharges_ specifies whether to assign charges to atoms from their assigned forcefield types (**TRUE**) or to leave atomic charges as they currently are (**FALSE**).

For example:


```
createExpression();
```


---

## currentFF <a id="currentff"></a>

_Syntax:_

**void** **currentFF** ( **string**|**int**|[Forcefield](/aten/docs/scripting/variabletypes/forcefield) _ff_ )

Delete the specified forcefield (i.e. unload it) and remove all reference to it in all models.

---

## deleteFF <a id="deleteff"></a>

_Syntax:_

**void** **deleteFF** ( **string**|**int**|[Forcefield](/aten/docs/scripting/variabletypes/forcefield) _ff_ )

Delete the specified forcefield (i.e. unload it) and remove all reference to it in all models.

---

## equivalents <a id="equivalents"></a>

_Syntax:_

**void** **equivalents** ( **string** _name_, **string** [var]typeName(s)[/var], ... )

Define equivalent terms in the current forcefield. _name_ is the new typename to which the list of quoted _typenames_ are linked, for subsequent use in intramolecular term definitions. See the equivalents forcefield keyword for more information.

---

## exportMap <a id="exportmap"></a>

_Syntax:_

**void** **exportMap** ( **string** _maps_ )

Set up manual mappings that convert atomtype names when expression are exported. Works in the opposite way to the **map** command.

For example:


```
exportMap("CT=Ctet,N3=N");
```


converts the atomtype names _CT_ and _N3_ so that they appear as _Ctet_ and _N_ in any expression files written out.

---

## ffModel <a id="ffmodel"></a>

_Syntax:_

**void** **ffModel** ( )

Associates current forcefield to the current model.

For example:


```
ffModel();
```


---

## ffPattern <a id="ffpattern"></a>

_Syntax:_

**void** **ffPattern** ( **string** _pattern_ )

**void** **ffPattern** ( **int** _patternid_ )

**void** **ffPattern** ( [Pattern](/aten/docs/scripting/variabletypes/pattern) _p_ )

Associates current forcefield to the current pattern, or one specified by either a reference, integer ID in the current model, or a pattern pointer.

For example:


```
ffPattern();
```


associates the current forcefield to the current pattern.


```
ffPattern("bulk");
```


associates the current forcefield to a pattern named _bulk_ in the current model.

---

## finaliseFF <a id="finaliseff"></a>

_Syntax:_

**void** **finaliseFF** ( )

Perform necessary operations on the current forcefield once all data has been added. Must be called!

---

## fixType <a id="fixtype"></a>

_Syntax:_

**void** **fixType** ( **int** _typeId_, [Atom](/aten/docs/scripting/variabletypes/atom)|**int** _id_ = 0 )

Set the current atom selection, or the specified atom, to have the type id (in the current forcefield) specified. Types set in this manner will not be overwritten by tha typing routines, allowing specific types to be applied above the normal rules. Note that the type’s NETA description is not checked, and so any (even types not matching the base element) may be applied in this way.

For example:

```
typeDef(99, "NX", "NX", N, "-C(n=4)"); 
select(C);
fixType(99); 
```

assigns newly-created type _99_ (specific to nitrogen) to all carbons in the model.

---

## freeType <a id="freetype"></a>

_Syntax:_

**void** **freeType** ( [Atom](/aten/docs/scripting/variabletypes/atom)|**int** _id_ = 0 )

For the current atom selection, or the specified atom, free any previously-fixed types

For example:


```
freeType(14);
```


frees any previously-set type on atom 14.

---

## generateAngle <a id="generateangle"></a>

_Syntax:_

 [FFBound](/aten/docs/scripting/variabletypes/ffbound) **generateAngle** ( [Atom](/aten/docs/scripting/variabletypes/atom)|**int** _i_, [Atom](/aten/docs/scripting/variabletypes/atom)|**int** _j_, [Atom](/aten/docs/scripting/variabletypes/atom)|**int** _k_)

Attempt to generate, from defined generator information in the current Forcefield, expression information for the angle between the specified atoms. The newly (or previously) generated term is returned.

For example:

```
newAtom(O);
addHydrogen();
FFBound generateAngle(2, 1, 3);
```

attemps to generate an angle term for the newly-created water molecule.

---

## generateBond <a id="generatebond"></a>

_Syntax:_

 [FFBound](/aten/docs/scripting/variabletypes/ffbound) **generateBond** ( [Atom](/aten/docs/scripting/variabletypes/atom)|**int** _i_, [Atom](/aten/docs/scripting/variabletypes/atom)|**int** _j_ )

Attempt to generate, from defined generator information in the current Forcefield, expression information for the bond between the specified atoms. The newly (or previously) generated term is returned.

---

## generateTorsion <a id="generatetorsion"></a>

_Syntax:_

 [FFBound](/aten/docs/scripting/variabletypes/ffbound) **generateTorsion** ( [Atom](/aten/docs/scripting/variabletypes/atom)|**int** _i_, [Atom](/aten/docs/scripting/variabletypes/atom)|**int** _j_, [Atom](/aten/docs/scripting/variabletypes/atom)|**int** _k_, [Atom](/aten/docs/scripting/variabletypes/atom)|**int** _l_ )

Attempt to generate, from defined generator information in the current Forcefield, expression information for the torsion  between the specified atoms. The newly (or previously) generated term is returned.

---

## generateVdw <a id="generatevdw"></a>

_Syntax:_

[FFAtom](/aten/docs/scripting/variabletypes/ffatom) **generateVdw** ( [Atom](/aten/docs/scripting/variabletypes/atom)|**int** _i_ )

Attempt to generate, from defined generator information in the current Forcefield, a van der Waals term for the specified atom. The newly (or previously) generated term is returned.

---

## getCombinationRule <a id="getcombinationrule"></a>

_Syntax:_

**string** **getCombinationRule** ( **string** _form_, **string** _parameter_ )

Returns the combination rule in use for the specifed parameter of the given functional form. The _form_ and related _parameter_ should correspond to those given in the VDW functional forms table. A string corresponding to one of the available combination rule options is returned.

For example:


```
string cr = getCombinationRule("lj", "epsilon");
```


---

## getFF <a id="getff"></a>

_Syntax:_

[Forcefield](/aten/docs/scripting/variabletypes/forcefield) **getFF** ( **string** _name_ | **int** _id_ )

Select the named forcefield (or forcefield with the specified _id_) and make it current, returning a reference to it in the process.

For example:


```
Forcefield uff = getFF("uff");
```


makes the loaded forcefield named uff the current one, and stores a reference to it.

---

## interDef <a id="interdef"></a>

_Syntax:_

**void** **interDef** ( **string** _form_, **int** _typeId_, **double** _charge_, **double** _data1_ ... )

Add a new short-range data definition to a type in the current forcefield. _form_ should correspond to one of the implemented VDW functional forms. Up to ten parameters for the VDW potential may be given.

---

## loadFF <a id="loadff"></a>

_Syntax:_

[Forcefield](/aten/docs/scripting/variabletypes/forcefield) **loadFF** ( **string** _file_, **string** _name_ )

Load a forcefield from file and reference it by _name_. Becomes the current forcefield.

For example:


```
loadFF("/home/foo/complex.ff", "waterff");
```


loads a forcefield called 'complex.ff' and names it _waterff_.

---

## map <a id="map"></a>

_Syntax:_

**void** **map** ( **string** _map_, ... )

Set up manual typename mappings for atom names that do not readily correspond to element symbols, forcefield types etc. All atoms that are subsequently created using _name_ as the element are automatically converted to the corresponding element.

For example:


```
map("CT1=C,CT2=C");
```


converts atoms with names _CT1_ and _CT2_ to carbon.

---

## newFF <a id="newff"></a>

_Syntax:_

[Forcefield](/aten/docs/scripting/variabletypes/forcefield) **newFF** ( **string** _name_ )

Create a new, empty forcefield with the given _name_ and make it current. Returns a reference to the new forcefield.

For example:


```
Forcefield ff = newFF("testff");
```


---

## printSetup <a id="printsetup"></a>

_Syntax:_

**void** **printSetup** ( )

Prints the current expression setup.

For example:


```
printSetup();
```


---

## printType <a id="printtype"></a>

_Syntax:_

**void** **printType** ( **int** _id_ )

Prints the NETA description of type _id_ in the current forcefield.

For example:


```
printType(99);
```


prints the NETA description of typ id 99.

---

## recreateExpression <a id="recreateexpression"></a>

_Syntax:_

**void** **recreateExpression** ( **bool** _noIntra_ = **FALSE**, **bool** _noDummy_ = **FALSE**, **bool** _assignCharges_ = **TRUE** )

Delete and recreate a suitable energy description for the current model. The optional _noIntra_, _noDummy_, and _assignCharges_ flags control various aspects of parameter generation, as described in the createExpression.

For example:


```
recreateExpression();
```


---

## saveExpression <a id="saveexpression"></a>

_Syntax:_

**int** **saveExpression** ( **string** _filter_, **string** _filename_ )

Export the forcefield expression for the current model in the format determined by the _filter_ nickname, to the _filename_ specified. Return value is _1_ for successful write, or _0_ otherwise.

For example:


```
saveExpression("dlpoly", "data.FIELD");
```


---

## setCombinationRule <a id="setcombinationrule"></a>

_Syntax:_

**void** **setCombinationRule** ( **string** _form_, **string** _parameter_, **string** _rule_ )

Sets the combination rule to use for the specifed parameter of the given functional form. The _form_ and related _parameter_ should correspond to those given in the VDW functional forms table, while _rule_ should correspond to one of the available combination rule options.

For example:


```
setCombinationRule("lj", "sigma", "geometric");
```


---

## torsionDef <a id="torsiondef"></a>

_Syntax:_

**void** **torsionDef** ( **string** _form_, **string** _type_i_, **string** _type_j_, **string** _type_k_, **string** _type_l_, **double** _data1_ ... )

Add a torsion definition to the current forcefield. _form_ should correspond to one of the implemented torsion functional forms, while the four _types_ refer to either type or equivalent names of defined atom types. Up to ten real-valued parameter values for the function may be provided.

---

## typeDef <a id="typedef"></a>

_Syntax:_

**int** **typeDef** ( **int** _typeid_, **string** _name_, **string** _equiv_, string|**int** _element_, **string** _neta_, **string** _description_ = "" )

Add a new atom type definition to the current forcefield, with the identifying _typeid_ and called _name_, with the equivalent typename _equiv_. The basic element of the new type is given as _element_, and _neta_ is the NETA definition of the type. An optional string describing the type in more detail can be given in _description_. The command returns ‘1’ if the model was typed successfully or ‘0’ otherwise.

For example:


```
typeDef(101, "Ctet", C, "nbonds=4", "Standard tetrahedral carbon");
```


creates a new simple type for a carbon atom with four bonds.

---

## typeModel <a id="typemodel"></a>

_Syntax:_

**int** **typeModel** ( )

Perform atom typing on the current model. Returns _1_ if atom typing was performed successfully or _0_ otherwise.

For example:


```
int success = typeModel();
```


---

## typeTest <a id="typetest"></a>

_Syntax:_

**int** **typeTest** ( **int** _typeId_, [Atom](/aten/docs/scripting/variabletypes/atom)|**int** _id_ )

Test the current forcefield's atomtype _typeId_ on the atom specified, returning the type score of the match (zero indicating no match).

For example:


```
int score = typeTest(112, 10);
```


tests type id 112 on the tenth atom in the model.

---

## units <a id="units"></a>

_Syntax:_

**void** **units** ( **string** _unit_ )

Sets the units in which energetic parameters are given for the current forcefield. For a list of available units see Section 16.8.



