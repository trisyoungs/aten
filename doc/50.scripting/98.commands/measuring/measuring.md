---
title: Measurement Commands
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

Make measurements of distances, angles, and torsion angles (dihedrals) in models. Note that there are two distinct sets of commands – those which 'measure' and those which calculate 'geometry'. The former create visible measurements within the model (which can then be viewed in the GUI), while the latter simply determine and return geometric values. Both sets take a variable number of arguments which determine whether a distance, angle, or torsion is measured/determined.

---

## clearMeasurements <a id="clearmeasurements"></a>

_Syntax:_

**void** **clearMeasurements** ( )

Clear all measurements in the current model.

For example:

```aten
clearMeasurements();
```

---

## geometry <a id="geometry"></a>

_Syntax:_

**double** **geometry** ( [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _i_, [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _j_, [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _k_ = 0, [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _l_ = 0 )

This command is a general measuring tool, able to measure distances, angles, and torsions in the model, depending on how many arguments are supplied. Note that, unlike the measure command, the resulting measurement is _not_ added to the Model’s internal list, and thus will not be displayed in the model.

For example:
```aten
double rij[50];
for (int i=1; i<=50; ++i) rij_ = geometry(1,i); 
```

calculates the distances between the first 50 atoms in the model and the first, regardless of whether they are bound or not

---

## listMeasurements <a id="listmeasurements"></a>

_Syntax:_

**void** **listMeasurements** ( )

List all measurements in the current model.

For example:

```aten
listMeasurements();
```

prints out a list of measurements made so far.

---

## measure <a id="measure"></a>

_Syntax:_

**double** **measure** ( [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _i_, [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _j_, [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _k_ = 0, [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _l_ = 0 )

This command is a general measuring tool, able to measure distances, angles, and torsions in the model, depending on how many arguments are supplied. Note that the resulting measurement is added to the Model's internal list, and will be displayed in the model. Also, note that measuring the same thing between the same atoms twice will remove the measurement from the Model.

For example:

```aten
double rij = measure(1, 2);
```

returns the distance between atoms 1 and 2.

```aten
double theta = measure(10, 20, 30);
```

returns the angle between atoms 10, 20, and 30.
```aten
double phi = measure(9, 8, 7, 6);
measure(9,8,7,6);
```

returns the torsion angle made between atoms 9, 8, 7, and 6, and then instantly removes it from the model by measuring it again.

---

## measureSelected <a id="measureselected"></a>

_Syntax:_

**void** **measureSelected** ( **int** _natoms_ )

This command is a general measuring tool to measure all of one particular type of interaction (i.e. bond distances, angles, or torsions) within the current atom selection. The single argument specifies the type of interaction to calculate by specifying thu number of atoms involved in the interaction – i.e. 2, 3, or 4 for bond distances, angles, and torsions respectively.

For example:

```aten
measureSelected(3);
```

calculates and displays all bond angles in the current atom selection.

