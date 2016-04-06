---
title: Matrix
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

The **Matrix** type is a simple 4x4-matrix class containing sixteen **double** values.
 
| Member | Type | RW | Description |
|--------|------|----|-------------|
| xx | **double** | • | The x component of the X column |
| xy | **double** | • | The y component of the X column |
| xz | **double** | • | The z component of the X column |
| xw | **double** | • | The w component of the X column |
| yx | **double** | • | The x component of the Y column |
| yy | **double** | • | The y component of the Y column |
| yz | **double** | • | The z component of the Y column |
| yw | **double** | • | The w component of the Y column |
| zx | **double** | • | The x component of the Z column |
| zy | **double** | • | The y component of the Z column |
| zz | **double** | • | The z component of the Z column |
| zw | **double** | • | The w component of the Z column |
| wx | **double** | • | The x component of the W column |
| wy | **double** | • | The y component of the W column |
| wz | **double** | • | The z component of the W column |
| ww | **double** | • | The w component of the W column |

## Matrix Type Functions

### setIdentity <a id="setIdentity"></a>

_Syntax:_

**void** **setIdentity** ( )

Reset the matrix to the identity matrix.

