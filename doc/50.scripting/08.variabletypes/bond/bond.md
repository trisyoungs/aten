---
title: Bond
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

The [**Bond**](/aten/docs/scripting/variabletypes/bond) type represents a chemical connection of some defined order between two atoms.
 
| Member | Type | RW | Description |
|--------|------|----|-------------|
| i | [**Atom**](/aten/docs/scripting/variabletypes/atom) | | First atom involved in the bond |
| j | [**Atom**](/aten/docs/scripting/variabletypes/atom) | | Second atom involved in the bond |
| order | **double** | | Bond order |
| type | **string** | • | Bond Type – see Section 16.2 for a list |

# Bond Type Functions

## partner <a id="partner"></a>

_Syntax:_

[**Atom**](/aten/docs/scripting/variabletypes/atom) **partner** ( [**Atom**](/aten/docs/scripting/variabletypes/atom) _i_ )

Return the other atom (i.e. not _i_) involved in the bond


