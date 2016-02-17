---
title: Files
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

The basic forcefield file format is designed to be as readable as possible by both machine and user. Lines are free format, meaning that any number of tabs, commas, and spaces may separate items. Text items should be enclosed with either double or single quotes if they themselves contain these delimiters (in particular, this applies to NETA descriptions). There is no terminating symbol at the end of a line, c.f. the command language where a ‘;’ typically ends every command.

The majority of data is contained within blocks in the file. Blocks begin with a specific keyword, contain one or more lines of definitions, and are terminated by an 'end' keyword. Forcefield files may contain many blocks of the same type, permitting terms of the same type but with differing functional forms to be defined easily.

More advanced forcefields may contain [generator sections and functions](/aten/docs/ff/rules) that enable them to either generate all their parameters on the fly from a basic set of data, or fill in missing terms that are not defined in any blocks.

## Example - SPC Water

The format is keyword-based and as simple as possible. Most input is enclosed within blocks, beginning with a keyword and terminated after several lines of data with and 'end' statement. As an example of the overall structure of a forcefield, consider the simple point charge (SPC) forcefield for water as is provided with **Aten**:

```
name "SPC Water"
units kj

types
1       HW      H       "nbonds=1"
2       OW      O       "-H,-H"
end

inter  lj
1       HW      0.41    0.0     0.0
2       OW      -0.82   0.650   3.166
end

bonds constraint
HW      OW      4184.0  1.000
end

angles bondconstraint
HW      OW      HW      4184.0  1.62398
end
```

After giving the forcefield a name and defining the energy units used for the parameters, the two types of atom described by the forcefield ( and ) are listed. A unique id, type name, base element, and type description are provided, providing **Aten** with all it needs to be able to recognise and type these atoms within a model. For each of these types the van der Waals data are provided in the Lennard-Jones style (in the `inter lj` section) – again, the type id and type name are specified, followed by the atomic charge and the epsilon and sigma values. Note here that there are default combination rules set for each functional form - see [VDW Functional Forms](/aten/docs/ff/forms-vdw) for a list. Finally, the single bond and angle within the molecule are defined. The type names involved in the interactions are given, followed by the necessary parameters for the functional form specified (‘constraint’ for the bond, and ‘bondconstraint’ for the angle). And that's it. Detailed explanations of each section follow. A more complete test forcefield supplied with **Aten** can be found in `data/ff/test.ff`.



