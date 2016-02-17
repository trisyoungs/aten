---
title: Measurement
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

The [**Measurement**](/aten/docs/scripting/variabletypes/measurement) type stores the atom indexes and current value of a single measurement currently displayed on a model.
 
| Member | Type | RW | Description |
|--------|------|----|-------------|
| atoms | [**Atom**]\[\](/aten/docs/scripting/variabletypes/atom) | | Array of atoms involved in the measurement |
| i | [**Atom**](/aten/docs/scripting/variabletypes/atom) | | First atom involved in the measurement |
| j | [**Atom**](/aten/docs/scripting/variabletypes/atom) | | Second atom involved in the measurement |
| k | [**Atom**](/aten/docs/scripting/variabletypes/atom) | | Third atom involved in the measurement (if any) |
| l | [**Atom**](/aten/docs/scripting/variabletypes/atom) | | Fourth atom involved in the measurement (if any) |
| literal | **double** | | The literal value (e.g. distance, angle, or torsion angle) of the measurement using atom coordinates as-is without applying minimum image criteria in periodic systems |
| value | **double** | | Value (e.g. distance, angle, or torsion angle) of the measurement |



