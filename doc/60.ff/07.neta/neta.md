---
title: NETA Reference
brief: Descriptions of all <strong>NETA</strong> keywords
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

This section lists all the available commands that may make up a type description. Many keywords only make sense within the bracketed parts of keywords that expand the depth of the description. For instance, it is meaningless to specify the connection type with the  keyword in the root of a typing command since no connections are relevant at this point.

---

## anybondto <a id="~X (any bond to X)"></a>

[name]~X[/name] specifies that a connection to X must exist, but makes no demand of the type (bond order) of the connection. X may be an element symbol, an id for another type specifier, or a list in square brackets containing one or both of these to allow more flexible specifications. Specifying an unknown connection with [value]~X[/value] is often useful in, for example, aromatic rings or conjugated systems where the connection might be either a double or single bond.

<table>
 <title>NETA ‘~X’ Keyword Examples</title>
 <header>
    <column>Command</column>
    <column>Meaning</column>
 </header>
 <row>
  <column>[value]~C[/value]</column>
  <column>Any bond to a carbon atom</column>
 </row>
 <row>
  <column>[value]~&amp;101[/value]</column>
  <column>Any bond to an atom which matches type ID 101 (see the section on [reusing types](/aten/docs/ff/typing#reuse))</column>
 </row>
 <row>
  <column>[value]~[N,S,P][/value]</column>
  <column>Any connection to either nitrogen, sulfur, or phosphorous</column>
 </row>
 <row>
  <column>[value]!~O[/value]</column>
  <column>Explicitly states that there should not be a bond to an  oxygen atom</column>
 </row>
</table>

---

## singlebondto <a id="–X (single bond to X)"></a>

`-X` specifies that a single bond to X must exist. X may be either an element symbol, an id for another type specifier, or a list containing one or both of these to allow more flexible specifications. If used inside the bracketed part of a [ring](/aten/docs/ff/neta#ring) description this only indicates that the atom/type should be present within the cycle – the connection to the target atom is unimportant. If used in a [chain](/aten/docs/ff/neta#chain) keyword the connection type is honoured.

<table>
 <title>NETA ‘–X’ Keyword Examples</title>
 <header>
  <column>Command</column>
    <column>Meaning</column>
 </header>
 <row>
  <column>[value]-H[/value]</column>
  <column>A single bond to a hydrogen atom</column>
 </row>
 <row>
  <column>[value]-&amp;120[/value]</column>
  <column>A single bond to an atom which matches type ID 120 (see the section on [reusing types](/aten/docs/ff/typing#reuse))</column>
 </row>
 <row>
  <column>[value]-[C,N][/value]</column>
  <column>A single bond to either a carbon or a nitrogen</column>
 </row>
 <row>
  <column>[value]-[F,Cl,Br,I,At][/value]</column>
  <column>A single bond to any halogen atom</column>
 </row>
 <row>
  <column>[value]-[&amp;10,&amp;11,&amp;18,-Kr][/value]</column>
  <column>A single bond to an atom with type ID 10, 11, or 18, or a   krypton atom</column>
 </row>
</table>

---

## dublebondto <a id="=X (double bond to X)"></a>

[name]=X[/name] specifies that a double bond to X must exist. X may be either an element symbol, an id for another type specifier, or a list containing one or both of these to allow more flexible specifications. If used inside the bracketed part of a [ring](/aten/docs/ff/neta#ring) description this only indicates that the atom/type should be present within the cycle – the connection to the target atom is unimportant. If used in a [chain](/aten/docs/ff/neta#chain) keyword the connection type is honoured.

<table>
 <title>NETA ’=X’ Keyword Examples</title>
 <header>
    <column>Command</column>
    <column>Meaning</column>
 </header>
 <row>
  <column>[value]=O[/value]</column>
  <column>A double bond to an oxygen atom</column>
 </row>
 <row>
  <column>[value]=&amp;4[/value]</column>
  <column>A double bond to an atom which matches type ID 4 (see the section on [reusing types](/aten/docs/ff/typing#reuse))</column>
 </row>
</table>

---

## bond <a id="bond"></a>

The  keyword defines the specific _type_ of the connection (see the [bond types](/aten/docs/enums/bondtype) enum) required for a bound atom. The keyword should be used inside bracketed parts of bound atom descriptions. It is important to note that the bond keyword should only be used in conjunction with the [name]~X[/name] (any bond to) specifier, since the specific connection demanded by the `-X` and [name]=X[/name] specifiers will override any  declarations.

<table>
 <title>NETA 'bond' Keyword Examples</title>
 <header>
  <column>Command</column>
  <column>Meaning</column>
 </header>
 <row>
  <column>[value]~O(bond=double)[/value]</column>
  <column>A double bond to an oxygen atom</column>
 </row>
 <row>
  <column>[value]~&amp;55(bond=triple)[/value]</column>
  <column>A triple bond to an atom which matches type ID 55 (see the section on [reusing types](/aten/docs/ff/typing#reuse))</column>
 </row>
 <row>
  <column>[value]~C(bond=single)[/value]</column>
  <column>A single bond to a carbon atom (a very explicit way of writing simply [value]-C[/value])</column>
 </row>
</table>

---

## chain <a id="chain"></a>

The  command provides an easy was of specifying a linear sequence of atoms from the current atom forward. Within the bracketed part, a sequence of connections are listed in the order in which they are to appear. Atoms in the chain are specified in the same way as other connections (e.g. [value]-C(nbonds=2)[/value]) but should _not_ be separated by commas. Note that, if all atoms specified for the chain are matched, but the actual chain in the model is longer, a positive match will be returned. Thus, it is usually a good idea to define the last atom in the chain more explicitly to prevent false matches.

<table>
 <title>NETA ‘chain’ Keyword Examples</title>
 <header>
  <column>Command</column>
  <column>Meaning</column>
 </header>
 <row>
  <column>[value]chain(-C,-C,-C,-C)[/value]</column>
  <column>Specifies a (minimum) four-carbon chain of any degree of saturation</column>
 </row>
 <row>
  <column>[value]chain(-C(nh=2),-C(nh=2),-C(nh=2),-C(nh=3))[/value]</column>
  <column>Explicity specifies an all-atom butyl chain</column>
 </row>
 <row>
  <column>[value]chain(-C,-C,-O,-H,n=2)[/value]</column>
  <column>Requests two ethanolic chains</column>
 </row>
</table>

---

## m <a id="m"></a>

The  command permits some control of typing (or selection) based on the physical coordinates of the atoms under consideration. It allows a bond distance, angle, or torsion to be evaluated against a supplied value and tolerance. The specification is similar to that of the [chain](/aten/docs/ff/neta#chain) command, with the exception that the first two arguments in the bracketed part are the required value and the tolerance respectively.

<table>
 <title>NETA ‘m’ Keyword Examples</title>
 <header>
  <column>Command</column>
  <column>Meaning</column>
 </header>
 <row>
  <column>[value]m(90.0,5.0,-C,-C)[/value]</column>
  <column>Requires that an angle of 90 +/- 5 degrees is formed between the current atom and two carbon atoms</column>
 </row>
 <row>
  <column>[value]m(120.0,10.0,-C,-N,-C)[/value]</column>
  <column>Requires that a torsion angle of 120 +/- 10 degrees exists between the current atom and a local C-N-C chain</column>
 </row>
</table>

---

## n <a id="n"></a>

The  keyword, when placed in the bracketed parts of bound atom, ring, and chain descriptions requires that they are matched a number of times rather than just once.

<table>
 <title>NETA ‘n’ Keyword Examples</title>
 <header>
  <column>Command</column>
  <column>Meaning</column>
 </header>
 <row>
  <column>[value]-C(n=4,-H(n=3))[/value]</column>
  <column>Describes the central carbon in neopentane (2,2-dimethylpropane) which is bound to four methyl groups</column>
 </row>
 <row>
  <column>[value]ring(size=4,n=3)[/value]</column>
  <column>Specifies that the target atom should be present in three unique four-membered rings</column>
 </row>
 <row>
  <column>[value]chain(-C-C-C~N(bond=triple),n=4)[/value]</column>
  <column>Requests that the atom has four cyanoethyl groups hanging off it</column>
 </row>
</table>

---

## nbonds <a id="nbonds"></a>

 specifies the exact number of connections that an atom must possess.

<table>
 <title>NETA ‘nbonds’ Keyword Examples</title>
 <header>
  <column>Command</column>
  <column>Meaning</column>
 </header>
 <row>
  <column>[value]nbonds=2[/value]</column>
  <column>Demand that the atom has exactly two connections</column>
 </row>
 <row>
  <column>[value]~N(nbonds=1)[/value]</column>
  Describes a nitrogen with only one bond, perhaps an sp1 nitrogen with a triple bond
 </row>
</table>

---

## nh <a id="nh"></a>

The  keyword is shorthand for explicitly specifying the number of attached hydrogens to the target atom or a bound atom. It is equivalent to stating [value]-H(n=m)[/value].

<table>
 <title>NETA ‘nh’ Keyword Examples</title>
 <header>
  <column>Command</column>
  <column>Meaning</column>
 </header>
 <row>
  <column>[value]-C(nh=2)[/value]</column>
  <column>Atom is bound to a methylene carbon with exactly two hydrogens on it</column>
 </row>
</table>

---

## aromatic <a id="aromatic"></a>

 indicates either that an atom must be present in an aromatic environment (e.g. in an aromatic ring), or that a ring should itself be aromatic.

<table>
 <title>NETA ‘aromatic’ Keyword Examples</title>
 <header>
  <column>Command</column>
  <column>Meaning</column>
 </header>
 <row>
  <column>[value]~N(aromatic)[/value]</column>
  <column>Specifies a nitrogen connected by any bond which is   present in an aromatic environment</column>
 </row>
 <row>
  <column>[value]-C(ring(aromatic))[/value]</column>
  <column>Single bond to a carbon atom which is in an aromatic ring</column>
 </row>
</table>

---

## noring <a id="noring"></a>

 indicates that the atom must not be present in any rings.

<table>
 <title>NETA ‘noring’ Keyword Examples</title>
 <header>
  <column>Command</column>
  <column>Meaning</column>
 </header>
 <row>
  <column>[value]-O(noring)[/value]</column>
  <column>Single bond to an oxygen which is not present in a ring</column>
 </row>
</table>

---

## notprev <a id="notprev"></a>

 indicates that the current atom must not be the previous atom in the current path. This can be used to prevent, for instance, a sequence of bound specifiers 'going back' on itself.

---

## notself <a id="notself"></a>

 indicates that the current atom must not be the current target of the matching process, i.e. it cannot be the root atom.

---

## planar <a id="planar"></a>

The  keyword specifies that the atom target should be planar, which is to say no bond from the atom may be more than 15° out of the plane formed by the first two bonds to the atom)

<table>
 <title>NETA ‘planar’ Keyword Examples</title>
 <header>
  <column>Command</column>
  <column>Meaning</column>
 </header>
 <row>
  <column>[value]-C(planar)[/value]</column>
  <column>Single bond to a carbon atom which is roughly planar</column>
 </row>
</table>

---

## ring <a id="ring"></a>

 denotes that the target atom (if specified in the root of the description) or a bound atom (if used inside the associated bracketed part) should be present in a ring structure of some kind. The  keyword alone simply demands that the atom is present inside a ring of some size, but may take an optional bracketed part describing more fully the number of atoms in the ring, and the individual nature of each of these atoms. Within the bracketed part, bound atoms may be specified as usual in the contained NETA description, but the connection type is irrelevant as it is only the presence of those particular atoms within the ring that is considered important.

Bound atom descriptions given inside the bracketed part should again be listed in order of decreasing complexity. Multiple rings may be specified with separate  keywords, allowing the location of fused ring atoms.

<table>
 <title>NETA ‘ring’ Keyword Examples</title>
 <header>
  <column>Command</column>
  <column>Meaning</column>
 </header>
 <row>
  <column>[value]ring(size=6,~C(n=6),aromatic)[/value]</column>
  <column>Benzene-style carbon</column>
 </row>
 <row>
  <column>[value]-C(ring(size=6,-C(n=6,nh=2)))[/value]</column>
  <column>Carbon atom in cyclohexane</column>
 </row>
</table>

---

## size <a id="size"></a>

Only relevant in the bracketed part of a  keyword,  requests the exact number of atoms comprising the cycle. Note that this may be used independently of the implicit size suggested by the number of atom descriptions supplied, or in conjunction with a partial list of atoms.

<table>
 <title>NETA ‘size’ Keyword Examples</title>
 <header>
  <column>Command</column>
  <column>Meaning</column>
 </header>
 <row>
  <column>[value]ring(size=7)[/value]</column>
  <column>Specifies a 7-membered ring</column>
 </row>
</table>

# Geometry Keywords

These keywords requests that the target atom (if specified in the root of the type description) or a bound atom (if used inside the associated bracketed part) should possess a certain physical geometry in terms of its connections. The number of bonds to the target atom and the angles between them are used to determine the geometry.

Note that, for some geometry types, there are several ways for the atom to have this geometry. 

---

## unbound <a id="unbound"></a>

An  atom, i.e. one with zero bonds to other atoms. Only makes sense when used in the root of a type
description.

---

## onebond <a id="onebond"></a>

 requests that the atom has exactly one bond (of any type). 

---

## linear <a id="linear"></a>

Two bonds to the atom in a  arrangement (angle [value]i-j-k[/value] > 170°).

---

## tshape <a id="tshape"></a>

Three bonds to the atom in a  geometry (with two bonds making an angle > 170°).

---

## trigonal <a id="trigonal"></a>

Three bonds in a  planar arrangement, with the largest of the three angles between 115 and 125°.

---

## tetrahedral <a id="tetrahedral"></a>

 geometries are possible for atoms with; exactly two bonds making an angle between 100 and 115°; exactly three bonds with the largest of the angles between 100 and 115°, and; exactly four bonds to the atom, with the average of the angles laying between 100 and 115°.

---

## sqplanar <a id="sqplanar"></a>

Four bonds to the atom in a square planar arrangement, with the average of the angles laying between 115 and 125°.

---

## tbp <a id="tbp"></a>

Five bonds to the atom are assumed to be trigonal bipyramidal geometry.

---

## octahedral <a id="octahedral"></a>

Six bonds to the atom are assumed to be in an octahedral arrangement.


