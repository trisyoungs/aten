---
title: Typing
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

We are all familiar with talking about atoms being chemically different depending on the functional group in which they exist - e.g. ether, carbonyl, and alcoholic oxygens - and this categorisation of atoms forms basis of forcefield writing. That is, a large number of different molecules and types of molecule should be described by a small set of different atoms, i.e. atom _types_. At the simplest level, the connectivity of an atom is enough to uniquely identify its specific type.

Some methods to use this information to uniquely assign types to atomic centres involve deriving a unique integer from the local connectivity of the atom (e.g. the SATIS method REF XXX), but including information beyond second neighbours is rather impractical. Others use a typing 'language' to describe individual elements of the topology of atoms in molecules, and are flexible enough to be able to describe complex situations in a more satisfactory way (e.g. that employed in Vega ref XXX). **Aten** uses the latter style and provides a clear, powerful, and chemically-intuitive way of describing atom types in, most importantly, a readable and easily comprehended style.

Type descriptions are used primarily for assigning forcefield types, but also make for an extremely useful way to select specific atoms as well.

## Language Examples

Type descriptions in **Aten** use connectivity to other atoms as a basis, extending easily to rings (and the constituent atoms), lists of allowable elements in certain connections, atom hybridicities, and local atom geometries. Descriptions can be nested to arbitrary depth since the algorithm is recursive, and may be re-used in other atom's type descriptions to simplify their identification. Time to jump straight in with some examples. Note that these examples only serve to illustrate the concepts of describing chemical environment at different levels. They may not provide the most elegant descriptions to the problem at hand, don't take advantage of reusing types (see Section 12.5.4), and certainly aren’t the only ways of writing the descriptions.

### Example 1 - Water

<figure>
  <image>img/typing_water.png</image>
  <caption></caption>
</figure>

Consider a water molecule. If you were describing it in terms of its structure to someone who understands the concept of atoms and bonds, but has no idea what the water molecule looks like, you might say:

```
A water molecule contains an oxygen that is connected two hydrogen atoms by single bonds
```

...or even...

```
It's an oxygen atom with two hydrogens on it
```

Given this degree-level knowledge, to describe the individual oxygen and hydrogen atoms in the grand scheme of the water molecule exactly, you might say:

```
A 'water oxygen' is an oxygen atom that is connected to two hydrogen atoms through single bonds
```

...and...

```
A 'water hydrogen' is a hydrogen that is connected by a single bond to an oxygen atom that itself is connected by a single bond to another (different) hydrogen atom
```

The extra information regarding the second hydrogen is necessary because otherwise we could apply the description of the 'water hydrogen' to the hydrogen in any alcohol group as well. Similarly, we might mistake the oxygen in the hydroxonium ion (H3O+) as being a ‘water oxygen’, when in fact it is quite different. In this case, we could extend the description to:

```
A 'water oxygen' is an oxygen atom that is connected to exactly two hydrogen atoms through single bonds, and nothing else
```

An atom description in **Aten** is a string of comma-separated commands that describe this kind of information. So, to tell the program how to recognise a water oxygen and a water hydrogen, we could use the following type descriptions (written in the proper forcefield input style for the [types](/aten/docs/ff/keywords#types) block:

```
1     OW     O      "nbonds=2,-H,-H"           # Water oxygen
2     HW     H      "-O(nbonds=2,-H,-H)"       # Water hydrogen
```

**Aten** now recognises that a water oxygen (OW) is "an oxygen atom that has exactly two bonds &amp; is bound to a hydrogen &amp; is bound to another hydrogen". Similarly, a water hydrogen HW is "a hydrogen bound to an oxygen atom that; has two bonds to it, and is bound to a hydrogen, and is bound to another hydrogen". In the type descriptions above the dash (-) is short-hand for saying ‘is bound to’, while the bracketed part afterwards (-O) in the water hydrogen description describes the required local environment of the attached oxygen. Using brackets to describe more fully the attached atoms is a crucial part of atom typing, and may be used to arbitrary depth (so, for example, we could add a bracketed description to the hydrogen atoms as well, if there was anything left to describe). If necessary, descriptions can be written that uniquely describe every single atom in a complex molecule by specifying completely all other connections within the molecule. This should not be needed for normal use, however, and short descriptions of atom environment up to first or second neighbours will usually suffice.

### Example 2 - 3-hydroxypropanoic acid

<figure>
  <image>img/typing_propanoic.png</image>
</figure>

Assuming that the OH group in the carboxylic acid functionalisation will have different forcefield parameters to the primary alcohol at the other end of the molecule, here we must describe the first and second neighbours of the oxygen atoms to differentiate them.

To begin, we can describe the carbon atoms as either two or three different types – either methylene/carboxylic acid, or carboxylic acid/adjacent to a carboxylic acid/adjacent to alcohol. For both, we only need describe the first neighbours of the atoms. For the first:

```
3     C(H2)  C      "nbonds=4,-H,-H,-C"                      # Methylene Carbon
4     C_cbx  C      "nbonds=3,-O(bond=double),-O,-C"         # Carboxylic Acid C
```

Note the ordering of the oxygen connections for the carboxylic acid carbon, where the most qualified carbon is listed first. This is to stop the doubly-bound oxygen being used to match [value]-O[/value], subsequently preventing a successful match. This is a general lesson – bound atoms with the most descriptive terms should appear at the beginning of the type description (as it is read left-to-right) and those with the least left until the end.

Where all three carbons need to be identified separately, we may write:

```
5     C(OH)  C      "nbonds=4,-H,-H,-C,-O"                   # CH2 adjacent to OH
6     C(COOH)       C      "nbonds=4,-H,-H,-C,-C"            # CH2 adjacent to COOH
7     C_cbx  C      "nbonds=3,-O(bond=double),-O,-C"         # Carboxylic Acid C
```

Let us now assume that the hydrogens within the alcohol and carboxylic acid groups must also be seen as different types. In this case, the second neighbours of the atoms must be considered:

```
8     HO     H      "-O(-C(-H,-H))"                   # Alcoholic H
9     H_cbx  H      "-O(-C(-O(bond=double)))"         # Carboxylic acid H
```

The assignment is thus based entirely on the nature of the carbon atom to which the OH group is bound since this is the next available source of connectivity information. The determination of the three different oxygen atoms is similar:

```
10    OH     O      "-H,-C(-H,-H)"                    # Alcoholic O
11    O_cbx  O      "-C(-O(-H))"                      # Carboxylic acid =O
12    OH_cbx O      "-H,-C(-O(bond=double))"          # Carboxylic acid O(H)
```

Of course, we could just have specified [value]nbonds=1[/value] for the doubly-bound oxygen of the carboxylic acid group, but this ‘hides’ information as to the true connectivity of the atom.

### Example 3 - N,N,2,5-tetramethylpyridin-4-amine

<figure>
  <image>img/typing_nn25tmp4amine.png</image>
</figure>

At last, a proper problem - an asymmetric substituted pyridine. Lets assume that we need to distinguish between every non-hydrogen atom – we’ll skip describing the hydrogen atoms for now, but note that this is most easily achieved by specifying directly the atomtype that the H is bound to (see later on). Let’s start with the pyridine nitrogen. We basically need to say that its in a 6-membered aromatic ring:

```
13    N_py   N      "ring(size=6,aromatic)"                  # Pyridine N
```

TODO

## Description Depth

Many subtleties related to the form of type descriptions are perhaps evident from the examples given above. It is useful to think of type descriptions as having many different ‘depths’, loosely corresponding to the number of bonds followed away from a central atom (the target atom, or the one currently being tested against the type description). The target atom of the type description is the root of the description since all connections are defined relative to this atom. A type description requiring specific connections to this target atom is using the target atom’s bonds in order to identify it - atoms to a depth of 1 bond are being used to describe the atom.  If these bound atoms are in turn described by their bound neighbours then atoms to a depth of two bonds are being used.

For example:

<table>
 <title>Description Depth Examples</title>
 <header>
  <column>Example Description</column>
  <column>Effective Depth</column>
 </header>
 <row>
  <column>[value]nbonds=4[/value]</column>
  <column>Zero - contains specifications relevant to the root atom only</column>
 </row>
 <row>
  <column>[value]nbonds=4,-H,-C[/value]</column>
  <column>1 - root commands and first bound neighbours</column>
 </row>
 <row>
  <column>[value]nbonds=4,-H,-C(-H(n=3))[/value]</column>
  <column>2 - root commands, first and second bound neighbours</column>
 </row>
</table>

Any depth of description can be handled by **Aten**, becoming as complex as is necessary to uniquely identify the target atom.

## Type Scores

In a forcefield with many defined types, more than one type may match an atom in a molecule. In order to best assign types to atoms, **Aten** scores each type description according to the number of terms defined within it, one point for each term satisfied. Once a matching type for an atom is located in the forcefield it is assigned to that atom, but the search for a better-scoring type continues. If a match with a higher score is found it replaces the previously-assigned type. If a match with the same score is found, the previous type is _not_ replaced.

### Non-Matching Types (Score = -1)

When a type description is tested for a given atom, it accumulates points for each term in the description that is satisfied by the environment of the atom. As soon as a term is found that is not satisfied, however, the score is reset to -1 and the match will fail. All terms in a type description must be satisfied in order for the type to be assigned to an atom.

### Empty Types (Score = 1)

A type description containing no terms has a maximum score of 1 (coming from a match of the element type). Hence:

```
99    Cgen   C      ""                   # Generic carbon
```

matches any carbon in any system, but will be replaced fairly easily by other types since it has such a low score.

### Normal Types (Score &gt; 1)

For a type in which all terms are matched successfully, one point is scored for each individual term. All of the following types have a potential maximum score of 3 (don't forget, one point comes from matching the element):

```
100   C1     C      "nbonds=2,linear"           # Carbon A
101   C2     C      "-C,-C"                     # Carbon B
102   C3     C      "-C(n=2)"                   # Carbon C
102   C4     C      "=C"                        # Carbon D
```

Moreover, they all potentially match the same atom (for example the central carbon in 1,2-propadiene). Since they have the same score, the first type C1 will match and persist over the other three, since only types with higher (not equivalent) scores can replace it.

## reuse <a id="Reusing Types"></a>

Once a complex NETA definition has been made for a given atom type, it is often useful to be able to reference this type in order to save repeating the same description for a closely-bound atom. The ampersand symbol allows you to do this, and specifies an integer type id to match rather than an element. As an example, consider the following definitions for trifluoroethanol from the OPLS-AA forcefield where the environment for each atom is described in full for each type:

```
160   CT     C      "-H,-H,-O(-H),-C(-F(n=3))"        "CH2 in trifluoroethanol"
161   CT     C      "-F(n=3),-C(nh=2,-O(-H))"         "CF3 in trifluoroethanol"
162   OH     O      "-H,-C(nh=2,-C(-F(n=3)))"         "OH in trifluoroethanol"
163   HO     H      "-O(-C(nh=2,-C(-F(n=3))))"        "HO in trifluoroethanol"
164   F      F      "-C(-F(n=3),-C(nh=2,-O(-H)))"     "F in trifluoroethanol"
165   HC     H      "-C(nh=2,-O(-H),-C(-F(n=3)))"     "H in trifluoroethanol"
```

For each atom type, the whole of the trifluoroethanol molecule is described, but each type tends to share a common chunk of NETA definitions with the other types. As an alternative, then, we can define one or two of the involved types explicitly as above, and then specify the rest of the types relative to this one:

```
160   CT     C      "-H,-H,-O(-H),-C(-F(n=3))"        "CH2 in trifluoroethanol"
161   CT     C      "-&amp;160"                           "CF3 in trifluoroethanol"
162   OH     O      "-&amp;160"                           "OH in trifluoroethanol"
163   HO     H      "-O(-&amp;160)"                       "HO in trifluoroethanol"
164   F      F      "-C(-&amp;160)"                       "F in trifluoroethanol"
165   HC     H      "-&amp;160"                           "H in trifluoroethanol"
```

Much neater! Or should that be ‘not much NETA’? Hmmm. Anyway, reusing types in this way is a powerful way to reduce the complexity of type descriptions required for a given molecule or fragment. Typically it is advisable to pick an atom that is fairly central to the molecule and bears a lot of connections, and provide an unambiguous type description.



