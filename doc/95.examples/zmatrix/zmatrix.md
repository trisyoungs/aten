---
title: Z-Matrix Printing
brief: Printing out the full z-matrix of a model
taxonomy:
  category: docs
  example: [zmatrix, z-matrix, CLI, scripting]
visible: true
template: manpage
docroot: /aten/docs
header_class: alt
---

This example shows how to print out the z-matrix for the current model, using the relevant accessors for the [Model variable](/aten/docs/scripting/variabletypes/model).

```aten
# Get the current model and then its z-matrix
Model m = aten.model;
	ZMatrix zmat = m.zMatrix;

# Print out z-matrix connectivities
for (ZMatrixElement zel = zmat.elements; zel; ++zel)
{
	if (zel.torsionAtom) printf("%-4s %-4i %-6s %-4i %-6s %-4i %-6s\n", zel.atom[1].symbol, zel.atom[2].id, zel.distanceName, zel.atom[3].id, zel.angleName, zel.atom[4].id, zel.torsionName);
	else if (zel.angleAtom) printf("%-4s %-4i %-6s %-4i %-6s\n", zel.atom[1].symbol, zel.atom[2].id, zel.distanceName, zel.atom[3].id, zel.angleName);
	else if (zel.distanceAtom) printf("%-4s %-4i %-6s\n", zel.atom[1].symbol, zel.atom[2].id, zel.distanceName);
	else printf("%-4s\n", zel.atom[1].symbol);
}

# Blank line for formatting purposes
printf("\n");

# Print out z-matrix variables
for (int n=1; n<=zmat.nDistances; ++n) printf("%-6s  %f\n", zmat.distanceNames[n], zmat.distances[n]);
for (int n=1; n<=zmat.nAngles; ++n) printf("%-6s  %f\n", zmat.angleNames[n], zmat.angles[n]);
for (int n=1; n<=zmat.nTorsions; ++n) printf("%-6s  %f\n", zmat.torsionNames[n], zmat.torsions[n]);
```
