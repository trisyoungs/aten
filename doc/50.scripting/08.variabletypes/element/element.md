---
title: Element
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

The [**Element**](/aten/docs/scripting/variabletypes/element) type stores all information describing a given chemical element, the full array of which is stored in the [**Aten**](/aten/docs/scripting/variabletypes/aten) type.
 
| Member | Type | RW | Description |
|--------|------|----|-------------|
| ambient | **double**[4] | • | Ambient colour of the element |
| colour | **double**[4] | • | Returns the ambient colour of the element. Set this property to define both ambient (supplied values) and diffuse (0.75*supplied values) components simultaneously |
| diffuse | **double**[4] | • | Diffuse colour of the element |
| mass | **double** | | Atomic mass of the element |
| name | **string** | | Capitalised name of the element |
| radius | **double** | • | Atomic radius of the element. Affects the scaled sphere rendering style and bond calculation. |
| symbol | **string** | | Atomic symbol of the element |
| z | **int** | • | Atomic number of the element |



