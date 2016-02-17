---
title: Eigenvector
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

The [**Eigenvector**](/aten/docs/scripting/variabletypes/eigenvector) type stores the coefficients of a complete molecular orbital for a specific model.
 
| Member | Type | RW | Description |
|--------|------|----|-------------|
| eigenvalue | **double** | • | Associated eigenvalue of the eigenvector |
| name | **string** | • | Name of the eigenvector, e.g. orbital symmetry |
| occupancy | **double** | • | Associated occupancy of the eigenvector |
| size | **int** | • | Current size of the eigenvector array. Can be set to reinitialise the array. |
| vector | **double**\[\]  | • | The eigenvector data array |



