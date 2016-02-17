---
title: GlyphData
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

The [**GlyphData**](/aten/docs/scripting/variabletypes/glyphdata) type stores colour and position data for a single point in a [**Glyph**](/aten/docs/scripting/variabletypes/glyph).

| Member | Type | RW | Description |
|--------|------|----|-------------|
| atom | [**Atom**](/aten/docs/scripting/variabletypes/atom) | • | Atom (if any) from which positional data is retrieved |
| atomData | **int** | • | Type of data to retrieve from specified atom (if any) - 0 = position, 1 = force, 2 = velocity |
| colour | **double**[4] | • | RGBA colour of the vertex (components ranging from 0.0 1.0)  |
| vector  | [**Vector**](/aten/docs/scripting/variabletypes/vector)  | •  | Vertex coordinates  |



