---
title: EnergyStore
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

The **EnergyStore** type stores the last set of energy values calculated for a model or frame.
 
| Member | Type | RW | Description |
|--------|------|----|-------------|
| angle | double | | Total angle energy |
| bond | double | | Total bond energy |
| electrostatic | double | | Total electrostatic energy (from Coulomb or Ewald sums) |
| torsion | double | | Total torsion energy |
| total | double | | Total of all energy components |
| ureyBradley | double | | Total Urey-Bradley energy |
| vdw  | double | | Total van der Waals energy |

