---
title: Disorder Commands
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

Set up the disordered builder to create systems from individual components using Monte Carlo methods.

---

## disorder <a id="disorder"></a>

_Syntax:_

**void** **disorder** ( **string** _scheme_, **bool** _fixedCell_ = **TRUE** )

Start the disordered builder on the current model (which must already possess a unit cell) employing the current _scheme_ (which should correspond to the name of an available partitioning scheme). The size of the unit cell of the current model is, by default, not modified by the disorder builder, but this can be overridden by specifying fixedCell as FALSE. From the GUI, the equivalent of doing the latter is to specify the basic relative cell lengths and let the builder determine the final cell size. 

For example:

```aten
disorder("CylinderX", FALSE);
```

runs the disorder builder using the ‘CylinderX’ partitioning scheme, and allows the cell size to change.

---

## listComponents <a id="listcomponents"></a>

_Syntax:_

**void** **listComponents** ( )

Prints a list of the currently requested populations, densities, and destination partitions for all models to be added during the disordered building process.

For example:

```aten
listComponents();
```

---

## setupComponent <a id="setupcomponent"></a>

_Syntax:_

**void** **setupComponent** ( **string** _policy_, **int** _partition_ = 1, **int** _population_ = 0, **double** _density_ = 0.0, **bool** _rotate_ = **TRUE** )

Instead of setting up a model for insertion by the disorder builder by setting its relevant variables, the **setupComponent** command allows all variables for the current model to be set simultaneously. The _policy_ should correspond to one of "none", "number", "density", "both", or "relative", and determines the final population of the model in the resulting disordered model.  See Section 7.11.5 for more information on the various insertion policy types. The partition id refers to the partition number into which the model should be inserted, as defined by the partitioning scheme selected when using the **disorder** command.

For example:

```aten
setupComponent("both", 2, 0, 0.8);
```

sets up the corrent model to be added into partition number 2 (in the example given in the **disorder** command given above, for example, this would correspond to the cylindrical region) with a final density of 0.8 g/cm3.

