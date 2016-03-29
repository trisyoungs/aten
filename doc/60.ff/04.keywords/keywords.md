---
title: Keywords
brief: Recognised forcefield section keywords
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

All allowable keywords in a forcefield (*.ff) file are given here.

---

# General Keyword Reference <a id="general"></a>

General keywords are simple keywords that take single items of data as arguments.

## name <a id="name"></a>

_Syntax:_

```
**name** _forcefieldName_
```

Sets the name of the forcefield as it appears in the program.

For example:

```
name "Test Forcefield"
```

sets the name of the forcefield to "Test Forcefield".

---

## units <a id="units"></a>

_Syntax:_

```
**units** _energyUnit_
```

Specifies the units of energy used for energetic forcefield parameters. Any energetic parameters specified in the forcefield are converted from the units specified here into the internal units of energy once loading of the forcefield has completed.

For example:

```
units kcal
```

indicates that any energetic values supplied in the forcefield are in kilocalories per mole.

---

## convert <a id="convert"></a>

_Syntax:_

```
**convert** _name_ \[_name_ ...\]
```

Only relevant if a [data](/aten/docs/ff/keywords#data) block exists, the **convert** keyword takes a list of parameter names defined in the data block(s), and marks them as being (or containing) a unit of energy. When **Aten** converts between energy units, these marked parameters will be converted also.

For example:

```
convert Dij e_sep
```

indicates that the defined data variables Dij and [value]e_sep[/value] are energy-based and should be converted when necessary.

# Block Keyword Reference <a id="block"></a>

All lists of terms, types, and extraneous data are specified in the form of blocks. Each keyword in this section marks the start of block, and must at some point be terminated by an **end** keyword. Blocks contain one or more lines of data, the contents of which is detailed in each section. In addition each keyword may take one or more (optional or required) values.

---

## angles <a id="angles"></a>

_Syntax:_

```
**angles** _form_
  _typeNameI_ _typeNameJ_ _typeNameK_ _data1_ \[_data2_ ...\]
**end**
```

Definitions of intramolecular angle terms are given in **angles** blocks. Multiple **angles** blocks may exist, each defining a subset of terms, and each possessing a different functional form, specified by the _form_ argument (see the section on [angle functional forms](/aten/docs/ff/forms-angle) for a list of valid forms).

The three typenames identify the particular angle to which the parameters are relevant. Note that typenames given for an angle [value]i-j-k[/value] will also match an angle [value]k-j-i[/value]. Data parameters should be provided in the required order for the specified form.

For example:

```
angles harmonic
HT  CT  HT   80.0   109.4
end
```

provides parameters for an [value]H-C-H[/value] angle using the harmonic potential.

---

## bonds <a id="bonds"></a>

_Syntax:_

```
**bonds** _form_
  _typeNameI_ _typeNameJ_ _data1_ \[_data2_ ... \]
**end**
```

Definitions of intramolecular bond terms are given in **bonds** blocks. Multiple **bonds** blocks may exist, each defining a subset of terms, and each possessing a different functional form, specified by the form argument (see the section on [angle functional forms](/aten/docs/ff/forms-bond) for a list of valid forms).

The two typenames identify the particular bond to which the parameters are relevant. Note that typenames given for a bond [value]i-j[/value] will also match a bond [value]j-i[/value]. Data parameters should be provided in the required order for the specified form.

For example:

```
bonds constraint
HT  CT  4000.0  1.06
end
```

provides parameters for an H-C bond using the constraint potential.

---

## data <a id="data"></a>

_Syntax:_

```
**data** "**type** _name_, ..."
 _typeid_ _typename_ _data1_ \[_data2_ ... \]
**end**
```

The **data** block defines additional generic data for each atom type. The additional data can be accessed through the [dataD](/aten/docs/scripting/variabletypes/ffatom#datad), [dataI](/aten/docs/scripting/variabletypes/ffatom#datai), and [dataS](/aten/docs/scripting/variabletypes/ffatom#datas) members of the [**FFAtom**](/aten/docs/scripting/variabletypes/ffatom) type.

The quoted argument supplied to the block defines the data types and names to be expected for each specified atom type, and furthermore strictly defines the order in which they must be given. Any of the standard simple variable types **int**, **double**, and **string** may be used.

Following the identifying typeid and typename data items are given one after the other and in the order they are declared in the block keyword argument.

For example:

```
data "string dogtype, int bridge, int likesPasta, double numToes"
1     OW     "redsetter"   1      0      9
2     HW     "dalmation"   1      1      64
end
```

This defines a quartet of extra data (albeit random, odd data...) for each of the specified atom types.

For forcefields which rely on functions to generate the necessary function data, the **data** block should be used to define additional data for each atom type. For example, the GAFF forcefield is able to generate extra intramolecular terms if the relevant definitions are not already defined in the forcefield, and the UFF and DREIDING forcefields contain no pre-defined intramolecular terms whatsoever.

---

## defines <a id="defines"></a>

_Syntax:_

```
**defines**
_name_ "NETA"
**end**
```

The **defines** block makes it possible to state commonly-used or lengthy chunks of NETA that do not belong to any specific atom type, and which can then be reused multiple times in many different atom type descriptions. Each definition in the block is given an identifying unique name which allows it to be referenced with the ‘$’ symbol.

The NETA descriptions provided for each definition **must** be valid, or the forcefield will not be loaded. In subsequent NETA definitions for atom types the definitions may be inserted by stating `$name`. For example:

```
defines
  water_oxygen  "-O(nh=2,nbonds=2)"
end

...

types
1       HW2     H       "nbonds=1,$water_oxygen"              "Water hydrogen"
end
```

---

## equivalents <a id="equivalents"></a>

_Syntax:_

```
**equivalents**
_alias_ _typeName_ ...
**end**
```

In forcefields, the most detailed information provided is typically the short-range intermolecular and charge parameters, with different values (or at least different charges) given to each individual atom type. Usually, intramolecular terms are more general and don't depend so much on the exact atom type. For example, given a tetrahedral carbon CT and three different aliphatic hydrogens H1, H2, and H3, the bond between the carbon and any of the three hydrogen types will likely be the same with respect to the force constant, equilibrium distance etc.

So, the forcefield will have to contain three intramolecular bond definitions covering [value]CT-H1[/value], [value]CT-H2[/value], and [value]CT-H3[/value], each having the same parameters, right? Not necessarily. While this is perfectly acceptable, for large forcefields the number of redundant terms may become quite large, and even for small forcefields with only a handful of terms, adding in duplicate data might irk the more obsessive amongst us. On these occasions, atomtype equivalents can be defined, which effectively link a set of atomtypes to a single identifying name that can then be used in any intramolecular parameter definitions.

In the waffle above, aliasing the three hydrogens H1, H2, and H3 to a single typename H1 can be done as follows:

```
equivalents
H1     H2  H3
end
```

Note that the aliased name does not have to be an atomtype name already defined in a **types** section.

---

## function <a id="function"></a>

_Syntax:_

```
**function**
**type** _functionName_( _argument list_ )
{
  ...
}
```

The **function** block contains all the function definitions relevant to rule-based forcefields (Section 1.1). The function(s) should be written in the standard command language style (Section 8.1.1).

For example:

```
function
int generateBond(FFBbound data, Atom i, Atom j)
{
      # Calculate bond potential parameters between supplied atoms i and j
      data.form = "morse";
      return 1;
}
end
```

defines the function to be used when generation of a bond term is required. Only functions with certain names will be recognised and used properly by **Aten**. See the functions section (12.4.1) in rule-based forcefields for more information and a list of valid function declarations that may be made.

---

## inter <a id="inter"></a>

_Syntax:_

```
**inter** _form_
_typeid_ _typename_ _charge_ _data1_ ...
**end**
```

Intermolecular van der Waals parameters and the charge associated with each atom type belong in the **inter** section. There may be multiple **inter** sections within the same forcefield file, but parameters for an individual atomtype may be defined only once.

The **inter** keyword begins a block of intermolecular parameter definitions, and the single argument _form_ should specify the functional form of the intermolecular interactions contained within. typeid and typename refer to a single type defined in a [types](/aten/docs/ff/keywords#types) section, charge is the atomic charge of this atomtype, and then follows the data describing the interaction. The order of the values given should correspond to the order of parameters expected for the specified functional form.

For example, the Lennard-Jones potential takes two parameters – ‘epsilon’ and ‘sigma’, in that order. For a chloride atomtype with ID 24, if ‘epsilon’ = 0.5, ‘sigma’ equals 3.0, and the charge on the atomtype is -1 _e_, the corresponding entry in the **inter** block will be:

```
24   Cl    -1.0   0.5   3.0
```

Some functional forms have default values for some parameters, and need not be specified (if there are any, these are indicated in the accompanying lists). For this reason, it is important not to add any unnecessary extra data to the entries in the **inter** block, since this may overwrite a default parameter that results in literal chaos.

---

## torsions <a id="torsions"></a>

_Syntax:_

```
**torsions** _form_ \[ [_escale_ _vscale \]
 _typeNameI_ _typeNameJ_ _typeNameK_ _typeNameL_ _data1_ \[_data2_ ... \]
**end**
```

Definitions of intramolecular torsion terms are given in **torsions** blocks. Multiple **torsions** blocks may exist, each defining a subset of terms, and each possessing a different functional form, specified by the _form_ argument (see [torsion functional forms](/aten/docs/ff/forms-torsions) for a list of valid forms). For torsions the electrostatic and VDW 1-4 interactions (i.e. those between atoms i and l in a torsion [value]i-j-k-l[/value]) are scaled by some factor between 0.0 and 1.0. The optional escale and vscale arguments specify these scaling factors – if they are not provided, they each default to 0.5.

The four typenames identify the particular torsion to which the parameters are relevant. Note that typenames given for a torsion [value]i-j-k-l[/value]
will also match a torsion [value]l-k-j-i[/value]. Data parameters should be provided in the required order for the specified form.

For example:

```
torsions cos
HT  CT  OC  HO   3.0   5.0   0.0
end
```

provides parameters for an [value]H-C-O-H[/value] torsion using the cosine potential.

```
torsions cos3 0.8333333 0.25
CT  CT  CT  O1   1.0  -2.0   0.0
CT  CT  CT  O2   0.5  -1.4   1.0
end
```

defines two [value]C-C-C-O[/value] torsions of the triple cosine form, and with custom scale factors.

---

## types <a id="types"></a>

_Syntax:_

```
**types**
_typeid_ _typename_ _element_ _"NETA"_ \[_description_\]
**end**
```

The core of the forcefield file is the **types** section, listing the ids, names, and elements of the different atom types present in the forcefield, as well as a description telling **Aten** how to recognise them.

The _typeid_ is an integer number used to identify the type. It should be positive, and must be unique amongst all those defined in a single forcefield. _typename_ is the actual name of the atom type (OW, C1, [value]N_ar[/value] etc.), and is referred to in the other sections of the forcefield file, and _element_ is the type’s element symbol as found in the periodic table. The string _NETA_ defines how **Aten** should recognise this particular type, optionally followed by a short text _description_ of the type (which appears in lists within the program to help identify particular types). Atom types may be defined over multiple **types** blocks within the same file if necessary, but while more than one **types** block may exist, but all type IDs must be unique over all such blocks.

For example:

```
types
35  CT  C  "nbonds=4"  "Simple tetrahedral carbon"
end
```

describes a bog-standard tetrahedral carbon called CT, and assigns it an ID of 35.

---

## uatypes <a id="uatypes"></a>

_Syntax:_

```
**uatypes**
   _typeid_ _typename_ _element_ _mass_ _NETA_ \[description\]
**end**
```

The **uatypes** section contains exactly the same information as the [types](/aten/docs/ff/keywords#types) block except that a mass must also be provided. In the [types](/aten/docs/ff/keywords#types) block it is assumed that the character element of the type also implicitly defines the mass (as would be expected). In the case of united-atom forcefields, this is usually not the case. Thus, the **uatypes** block allows a mass to be associated in order to account for the light atoms subsumed into the heavy atom’s mass. This information can be accessed through the **mass** accessor of the [**FFAtom**](/aten/docs/scripting/variabletypes/ffatom) variable type.

For example:

```
uatypes
10  CH2  C  14.0265  "nbonds=2,nh=0"  "United atom methylene carbon"
end
```

describes a united-atom methylene carbon, with mass of 14.0265 (C+2H).

---

## ureybradleys <a id="ureybradleys"></a>

_Syntax:_

```
**ureybradleys** _form_
TODO
**end**
```

TODO

# Typename Wildcards

In any of the typenames given in the specification of intramolecular interactions, a wildcard character '*' may be used to ‘finish off’ any or all of the typenames (or replace individual typenames entirely). In doing so, a single definition is able to match more than one set of typenames.

For example:

```
bonds harmonic
CT    H*    4184.0    1.06
end
```

will describe bonds between CT and any other atom beginning with an H.

Using a '*' on its own will match any typename in that position. As an extreme example:

```
angles harmonic
*     *     *     418.4      109.4
end
```

will match any angle. Be careful - when **Aten** is creating expressions and searching for specific interactions between atom types, as soon as an intramolecular definition is found that matches it is used, and no further searching is done. So, loose definitions involving wildcards should be put near to the end of the block in which they occur.


