---
title: NETA Reference
brief: Descriptions of all <strong>NETA</strong> keywords
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

This section lists all the available commands that may make up a type description. Many keywords only make sense within the bracketed parts of keywords that expand the depth of the description. For instance, it is meaningless to specify the connection type with the [`bond`](/aten/docs/ff/neta#bond) keyword in the root of a typing command since no connections are relevant at this point.

---

## Basic Keywords

### anybondto <a id="~X (any bond to X)"></a>

`~X` specifies that a connection to X must exist, but makes no demand of the type (bond order) of the connection. X may be an element symbol, an id for another type specifier, or a list in square brackets containing one or both of these to allow more flexible specifications. Specifying an unknown connection with `~X` is often useful in, for example, aromatic rings or conjugated systems where the connection might be either a double or single bond.

| Command | Meaning |
|---------|---------|
| `~C` | Any bond to a carbon atom |
| `~&amp;101` | Any bond to an atom which matches type ID 101 (see the section on [reusing types](/aten/docs/ff/typing#reuse)) |
| `~[N,S,P]` | Any connection to either nitrogen, sulfur, or phosphorous |
| `!~O` | Explicitly states that there should not be a bond to an oxygen atom |

---

### singlebondto <a id="–X (single bond to X)"></a>

`-X` specifies that a single bond to X must exist. X may be either an element symbol, an id for another type specifier, or a list containing one or both of these to allow more flexible specifications. If used inside the bracketed part of a [`ring`](/aten/docs/ff/neta#ring) description this only indicates that the atom/type should be present within the cycle – the connection to the target atom is unimportant. If used in a [`chain`](/aten/docs/ff/neta#chain) keyword the connection type is honoured.

| Command | Meaning |
|---------|---------|
| `-H` | A single bond to a hydrogen atom |
| `-&amp;120` | A single bond to an atom which matches type ID 120 (see the section on [reusing types](/aten/docs/ff/typing#reuse)) |
| `-[C,N]` | A single bond to either a carbon or a nitrogen |
| `-[F,Cl,Br,I,At]` | A single bond to any halogen atom |
| `-[&amp;10,&amp;11,&amp;18,-Kr]` | A single bond to an atom with type ID 10, 11, or 18, or a krypton atom |

---

### dublebondto <a id="=X (double bond to X)"></a>

`=X` specifies that a double bond to X must exist. X may be either an element symbol, an id for another type specifier, or a list containing one or both of these to allow more flexible specifications. If used inside the bracketed part of a [`ring`](/aten/docs/ff/neta#ring) description this only indicates that the atom/type should be present within the cycle – the connection to the target atom is unimportant. If used in a [`chain`](/aten/docs/ff/neta#chain) keyword the connection type is honoured.

| Command | Meaning |
|---------|---------|
| `=O` | A double bond to an oxygen atom |
| `=&amp;4` | A double bond to an atom which matches type ID 4 (see the section on [reusing types](/aten/docs/ff/typing#reuse)) |

---

### bond <a id="bond"></a>

The `bond` keyword defines the specific _type_ of the connection (see the [bond types](/aten/docs/enums/bondtype) enum) required for a bound atom. The keyword should be used inside bracketed parts of bound atom descriptions. It is important to note that the bond keyword should only be used in conjunction with the `~X` (any bond to) specifier, since the specific connection demanded by the `-X` and `=X` specifiers will override any `bond` declarations.

| Command | Meaning |
|---------|---------|
| `~O(bond=double)` | A double bond to an oxygen atom |
| `~&amp;55(bond=triple)` | A triple bond to an atom which matches type ID 55 (see the section on [reusing types](/aten/docs/ff/typing#reuse)) |
| `~C(bond=single)` | A single bond to a carbon atom (a very explicit way of writing simply `-C`) |

---

### chain <a id="chain"></a>

The `chain` command provides an easy was of specifying a linear sequence of atoms from the current atom forward. Within the bracketed part, a sequence of connections are listed in the order in which they are to appear. Atoms in the chain are specified in the same way as other connections (e.g. `-C(nbonds=2)`) but should _not_ be separated by commas. Note that, if all atoms specified for the chain are matched, but the actual chain in the model is longer, a positive match will be returned. Thus, it is usually a good idea to define the last atom in the chain more explicitly to prevent false matches.

| Command | Meaning |
|---------|---------|
| `chain(-C,-C,-C,-C)` | Specifies a (minimum) four-carbon chain of any degree of saturation |
| `chain(-C(nh=2),-C(nh=2),-C(nh=2),-C(nh=3))` | Explicity specifies an all-atom butyl chain |
| `chain(-C,-C,-O,-H,n=2)` | Requests two ethanolic chains |

---

### geometry <a id="geometry"></a>

The `geometry` command permits some control of typing (or selection) based on the physical coordinates of the atoms under consideration. It allows a bond distance, angle, or torsion to be evaluated against a supplied value and tolerance. The specification is similar to that of the [`chain`](/aten/docs/ff/neta#chain) command, with the exception that the first two arguments in the bracketed part are the required value and the tolerance respectively.

| Command | Meaning |
|---------|---------|
| `geometry(1.4,0.05,-N)` | Requires that a nitrogen atom is within 1.4 +/- 0.05 Angstroms from the target atom |
| `geometry(90.0,5.0,-C,-C)` | Requires that an angle of 90 +/- 5 degrees is formed between the current atom and two carbon atoms |
| `geometry(120.0,10.0,-C,-N,-C)` | Requires that a torsion angle of 120 +/- 10 degrees exists between the current atom and a local C-N-C chain |

---

### n <a id="n"></a>

The `n` keyword, when placed in the bracketed parts of bound atom, ring, and chain descriptions requires that they are matched a number of times rather than just once.

| Command | Meaning |
|---------|---------|
| `-C(n=4,-H(n=3))` | Describes the central carbon in neopentane (2,2-dimethylpropane) which is bound to four methyl groups |
| `ring(size=4,n=3)` | Specifies that the target atom should be present in three unique four-membered rings |
| `chain(-C-C-C~N(bond=triple),n=4)` | Requests that the atom has four cyanoethyl groups hanging off it |

---

### nbonds <a id="nbonds"></a>

Specifies the exact number of connections that an atom must possess.

| Command | Meaning |
|---------|---------|
| `nbonds=2` | Demand that the atom has exactly two connections |
| `~N(nbonds=1)` | Describes a nitrogen with only one bond, perhaps an sp1 nitrogen with a triple bond |

---

### nh <a id="nh"></a>

The `nh` keyword is shorthand for explicitly specifying the number of attached hydrogens to the target atom or a bound atom. It is equivalent to stating `-H(n=m)`.

| Command | Meaning |
|---------|---------|
| `-C(nh=2)` | Atom is bound to a methylene carbon with exactly two hydrogens on it |

---

### aromatic <a id="aromatic"></a>

Indicates either that an atom must be present in an aromatic environment (e.g. in an aromatic ring), or that a ring should itself be aromatic.

| Command | Meaning |
|---------|---------|
| `~N(aromatic)` | Specifies a nitrogen (connected by any bond) which is present in an aromatic ring |
| `-C(ring(aromatic))` | Single bond to a carbon atom which is in an aromatic ring |

---

### noring <a id="noring"></a>

Specifies that the atom must not be present in **any** cyclic structures.

| Command | Meaning |
|---------|---------|
| `-O(noring)` | Single bond to an oxygen which is not present in a ring |

---

### notprev <a id="notprev"></a>

Specifies that the current atom must not be the previous atom in the current path. This can be used to prevent, for instance, a sequence of bound specifiers 'going back' on itself.

---

### notself <a id="notself"></a>

Specifies that the current atom must not be the current target of the matching process, i.e. it cannot be the root atom.

---

### planar <a id="planar"></a>

The `planar` keyword specifies that the atom target should be planar, which is to say no bond from the atom may be more than 15° out of the plane formed by the first two bonds to the atom)

| Command | Meaning |
|---------|---------|
| `-C(planar)` | Single bond to a carbon atom which is roughly planar |

---

### ring <a id="ring"></a>

 denotes that the target atom (if specified in the root of the description) or a bound atom (if used inside the associated bracketed part) should be present in a ring structure of some kind. The `ring` keyword alone simply demands that the atom is present inside a ring of some size, but may take an optional bracketed part describing more fully the number of atoms in the ring, and the individual nature of each of these atoms. Within the bracketed part, bound atoms may be specified as usual in the contained NETA description, but the connection type is irrelevant as it is only the presence of those particular atoms within the ring that is considered important.

Bound atom descriptions given inside the bracketed part should again be listed in order of decreasing complexity. Multiple rings may be specified with separate `ring` keywords, allowing the location of fused ring atoms.

| Command | Meaning |
|---------|---------|
| `ring(size=6,~C(n=6),aromatic)` | Benzene-style carbon |
| `-C(ring(size=6,-C(n=6,nh=2)))` | Carbon atom in cyclohexane |

---

### size <a id="size"></a>

Only relevant in the bracketed part of a [`ring`](/aten/docs/ff/neta#ring) keyword, `size` sets the exact number of atoms comprising the cycle. Note that this may be used independently of the implicit size suggested by the number of atom descriptions supplied, or in conjunction with a partial list of atoms.

| Command | Meaning |
|---------|---------|
| `ring(size=7)` | Specifies a 7-membered ring |

## Geometry Keywords

These keywords requests that the target atom (if specified in the root of the type description) or a bound atom (if used inside the associated bracketed part) should possess a certain physical geometry in terms of its connections. The number of bonds to the target atom and the angles between them are used to determine the geometry.

Note that, for some geometry types, there are multiple ways for an atom to be detected as being in that particular environment.

---

### unbound <a id="unbound"></a>

An unbound atom, i.e. one with zero bonds to other atoms. Only makes sense when used in the root of a type description.

---

### onebond <a id="onebond"></a>

Requests that the atom has exactly one bond (of any type). 

---

### linear <a id="linear"></a>

Two bonds to the atom in a linear arrangement (angle `i-j-k` > 170°).

---

### tshape <a id="tshape"></a>

Three bonds to the atom in a T-shape geometry (with two bonds making an angle > 170°).

---

### trigonal <a id="trigonal"></a>

Three bonds in a trigonal planar arrangement, with the largest of the three angles between 115 and 125°.

---

### tetrahedral <a id="tetrahedral"></a>

Tetrahedral geometries are possible for atoms with; exactly two bonds making an angle between 100 and 115°; exactly three bonds with the largest of the angles between 100 and 115°, and; exactly four bonds to the atom, with the average of the angles laying between 100 and 115°.

---

### sqplanar <a id="sqplanar"></a>

Four bonds to the atom in a square planar arrangement, with the average of the angles laying between 115 and 125°.

---

### tbp <a id="tbp"></a>

Five bonds to the atom are assumed to be trigonal bipyramidal geometry.

---

### octahedral <a id="octahedral"></a>

Six bonds to the atom are assumed to be in an octahedral arrangement.

