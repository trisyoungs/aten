---
title: ColourScale
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

The [**ColourScale**](/aten/docs/scripting/variabletypes/colourscale) type allows direct access to the points defined inside a colourscale.
 
| Member | Type | RW | Description |
|--------|------|----|-------------|
| nPoints | **int** | | Number of points contained in the colourscale |
| points | [**ColourScalePoint**](/aten/docs/scripting/variabletypes/colourscalepoint)\[\] | | Array of points in the colourscale |

# ColourScale Type Functions

## addPoint <a id="addpoint"></a>

_Syntax:_

[**ColourScalePoint**](/aten/docs/scripting/variabletypes/colourscalepoint) **addPoint** ( **double** _value_, **double** _r_, **double** _g_, **double** _b_, **double** _a_ = 1.0 )

Add a new point to the colourscale at point _value_ and RGB(A) colour specified, the components of which should have values ranging from 0.0 to 1.0 inclusive). 

---

## clear <a id="clear"></a>

_Syntax:_

**void** **clear** ( )

Clear all points contained in the colourscale

---

## colour <a id="colour"></a>

_Syntax:_

**void** **colour** ( **double** _value_, **double** &amp;_r_, **double** &amp;_g_, **double** &amp;_b_, **double** &amp;_a_ )

Retrieve the colour components for the _value_ specified.  The RGBA values of the colour will be placed in the supplied variables.


