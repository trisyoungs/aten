---
title: Glyph Commands
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

Add glyphs to atoms in the model.

---

## autoEllipsoids <a id="autoellipsoids"></a>

_Syntax:_

**void** **autoEllipsoids** ( )

Note: Experimental Feature!

Using the current atom selection, this command creates ellipsoid glyphs that cover (or represent) individual bound fragments within the selection. An ellipsoid glyph is added for each bound fragment within the selection, positioned at the geometric centre of the bound fragment, and scaled in an attempt to cover all atoms within the bound fragment. Such things are useful when wanting to represent molecules by simple geometric shapes, rather than by their fine-grained atomic positions.

For instance, given a box full of benzene molecules:
```aten
selectAll();
autoEllipsoids();
```

will add on a flattened ellipsoid to each individual molecule. To do the same thing but using only the ring carbons to generate the ellipsoids:
```aten
select("C");
autoEllipsoids();
```

Now the ellipsoids will cover the carbon atoms in each ring, leaving the hydrogens poking out.

---

## autoPolyhedra <a id="autopolyhedra"></a>

_Syntax:_

**void** **autoPolyhedra** ( **string** _options_ = "" )

Note: Very Experimental Feature!

In a similar way to the **autoEllipsoids** command, **autoPolyhedra** adds triangle glyphs to the current selection in an attempt to enclose certain atoms within solid structures. There are two principal modes of operation. The first (the default) assumes that the current atom selection consists of individual atoms that should be enclosed in a polyhedron made up from triangles added between triplets of bound neighbours. The carbon atom at the centre of methane would make a good example. The alternative mode (requested with the ‘fragments’ option) assumes that atoms within individual bound fragments in the current selection should be used as the vertices to form an enclosed shell.

Possible _options_ are:

<table>
  <title>AutoPolyhedra Options</title>
  <header>
     <column>Option</column>
     <column>Effect</column>
    </header>
    <row>
     <column>_cenrowes_</column>
     <column>Assume that the current selection consists of individual atomic cenrowes that should be enclosed (the default)</column>
    </row>
    <row>
     <column>_fragments_</column>
     <column>Use individual bound fragments instead of assuming individual cenrowes</column>
    </row>
    <row>
     <column>_nolink_</column>
     <column>Do not link the coordinates of generated glyphs to coordinates of the atoms (the default is to link glyph coordinates to atoms)</column>
    </row>
    <row>
     <column>_rcut_ = distance</column>
     <column>Specifies the maximum distance allowed between vertices of a triangle</column>
    </row>
</table>

---

## glyphAtomF <a id="glyphatomf"></a>

_Syntax:_

**void** **glyphAtomF** ( **int** _n_ )

**void** **glyphAtomF** ( **int** _n_, [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _sourceAtom_ )

Set current (or specified) atom’s forces as data _n_ in the current glyph.

For example:

```aten
glyphAtomF(1);
```

links the current atoms forces to the first datum in the current glyph.

---

## glyphAtomR <a id="glyphatomr"></a>

_Syntax:_

**void** **glyphAtomR** ( **int** _n_ )

**void** **glyphAtomR** ( **int** _n_, [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _sourceAtom_ )

Set current (or specified) atom's position as data _n_ in the current glyph.

For example:

```aten
glyphAtomR(3, 55);
```

links the 55th atom’s position to the third datum in the current glyph.

---

## glyphAtomV <a id="glyphatomv"></a>

_Syntax:_

**void** **glyphAtomV** ( **int** _n_ )

**void** **glyphAtomV** ( **int** _n_, [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _sourceAtom_ )

Set current (or specified) atom's velocity as data _n_ in the current glyph.

For example:
```aten
Atom i = newAtom("H");
glyphAtomV(2,i);
```

links the velocity of new atom _i_ to the second datum in the current glyph.

---

## glyphAtomsF <a id="glyphatomsf"></a>

_Syntax:_

**void** **glyphAtomsF** ( [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _sourceAtom_ ... )

Accepts one or more atoms, setting consecutive data in the current glyph to the forces of the atoms / atom IDs provided.

For example:

```aten
glyphAtomsF(1, 2, 3);
```

links the forces of atoms 1, 2, and 3 to the first three glyph data.

---

## glyphAtomsR <a id="glyphatomsr"></a>

_Syntax:_

**void** **glyphAtomsR** ( [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _sourceAtom_ ... )

Accepts one or more atoms, setting consecutive data in the current glyph to the positions of the atoms / atom IDs provided.

For example:

```aten
glyphAtomsR(3, 10);
```

links the positions of atoms 3 and 10 to the first two glyph
data.

---

## glyphAtomsV <a id="glyphatomsv"></a>

_Syntax:_

**void** **glyphAtomsV** ( [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _sourceAtom_ ... )

Accepts one or more atoms, setting consecutive data in the current glyph to the velocities of the atoms / atom IDs provided.

For example:

```aten
glyphAtomsV(9, 11, 13);
```

links the velocities of atoms 9, 11, and 13 to first three glyph data.

---

## glyphColour <a id="glyphcolour"></a>

_Syntax:_

**void** **glyphColour** ( **int** _n_, **double** _r_, **double** _g_, **double** _b_, **double** _a_ = 1.0 )

Set the colour of vertex _n_ for the current glyph to the RGB(A) colour provided (each component of which should be in the range 0.0 to 1.0 inclusive).

For example:

```aten
glyphColour(1, 1.0, 0.0, 0.0);
```

sets the colour of the first vertex in the current glyph to red.

---

## glyphData <a id="glyphdata"></a>

_Syntax:_

**void** **glyphData** ( **int** _n_, **double** _r_, **double** _g_, **double** _b_ )

Set vector data _n_ for the current glyph to the fixed values provided.

For example:

```aten
glyphData(1, 0.0, 5.0, 2.4);
```

sets the first positional data in the glyph to {0.0, 5.0, 2.4}.

---

## glyphSolid <a id="glyphsolid"></a>

_Syntax:_

**void** **glyphSolid** ( **bool** _isSolid_ )

Sets the drawing style of the current glyph to solid (true) or wireframe (false) (if the glyph style permits).

For example:

```aten
glyphSolid("true");
```

---

## glyphText <a id="glyphtext"></a>

_Syntax:_

**void** **glyphText** ( **string** _text_ )

Set the text data in the current glyph. For text-style glyphs, this is a necessary piece of data. 

For example:

```aten
glyphText("Coordinate Origin");
```

---

## newGlyph <a id="newglyph"></a>

_Syntax:_

[**Glyph**](/aten/docs/scripting/variabletypes/glyph) **newGlyph** ( **string** _style_, **string** _options_ = "" )

Create a new glyph of the specified style, and make it current. The colour of the glyph is set using the default glyph colour set in the global preferences. Valid glyph styles are listed in glyph types. Positional / size / scale vector data should be set afterwards with appropriate [name]glyphAtom*[/name] and [name]glyphData*[/name] commands.

One or more options may be given to the command. The list of possible _options_ is:

<table>
 <header>
  <column>Option></column>
  <column>Effect</column>
 </header>
 <row>
  <column>_solid_</column>
  <column>Render the glyph in solid mode (if the glyph supports it). Same as calling **glyphSolid**(**TRUE**); after creation</column>
 </row>
 <row>
  <column>_text_ = string</column>
  <column>Set the character data associated to the glyph to string. Valid only for glyphs which display text.</column>
 </row>
 <row>
  <column>_wire_</column>
  <column>Render the glyph in wireframe mode (if the glyph supports it). Same as calling **glyphSolid**(**FALSE**); after creation.</column>
 </row>
</table>

For example:

```aten
newGlyph("cube");
```

creates a new cube in the model.

```aten
newGlyph("text","text=\"I am some text\"");
```

creates a new text glyph in the model, reading "I am some text". Note the need to escape the quotes surrounding the text.

```aten
newGlyph("tetrahedron", "wire");
```

creates a new wireframe tetrahedron in the model.

