---
title: Glyph
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

The [**Glyph**](/aten/docs/scripting/variabletypes/glyph) type contains information describing a single glyph within a model.
 
| Member | Type | RW | Description |
|--------|------|----|-------------|
| data | [**GlyphData**](/aten/docs/scripting/variabletypes/glyphdata) | | Individual data for each vertex of the glyph |
| nData | **int** | | How many data points (vertices) are associated with this glyph's type |
| rotated | **int** | • | Flag to indicate whether the glyph’s rotation matrix has been modified (i.e. the glyph has been rotated). Setting this to '0' resets (and removes) any current rotation matrix (same as calling member function 'resetrotation'). |
| rotation | **double**[9] | • | Rotation matrix for the glyph. Note that not all glyphs can be rotated (see the topic on <a href="#_Glyphs">Glyphs</a> for more information. |
| selected | **int** | • | Whether the glyph is currently selected |
| solid | **int** | • | Specifies whether the glyph is drawn as a filled (solid) shape or in wireframe |
| text | **string** | • | Text data associated to the glyph. Not all glyphs use text |
| type | **string** | • | Style of the glyph – see Glyph Types (Section 16.9) for a list |
| visible | **int** | • | Flag indicating whether the glyph is currently visible |

# Glyph Type Functions

## recolour <a id="recolour"></a>

_Syntax:_

**void** **recolour** ( **double** _r_, **double** _g_, **double** _b_, **double** _a_ = 1.0 )

Recolour all data vertices of the glyph to the specified RGB(A) value, each component of which should be in the range 0.0 to 1.0 inclusive.

---

## resetRotation <a id="resetRotation"></a>

_Syntax:_

**void** **resetRotation** ( )

Reset any rotation applied to the glyph

---

## rotateX <a id="rotateX"></a>

_Syntax:_

**void** **rotateX** ( **double** _angle_ )

Rotates the glyph by _angle_ degrees about its x axis

---

## rotateY <a id="rotateY"></a>

_Syntax:_

**void** **rotateY** ( **double** _angle_ )

Rotates the glyph by _angle_ degrees about its y axis

---

## rotateZ <a id="rotateZ"></a>

_Syntax:_

**void** **rotateZ** ( **double** _angle_ )

Rotates the glyph by _angle_ degrees about its z axis


