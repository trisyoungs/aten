---
title: Energy Commands
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

Calculate energies for models and trajectory frames. All printing commands refer to the last energy calculated for either the model or a trajectory frame.

---

## elec <a id="elec"></a>

_Syntax:_

**void** **elec** ( **string** _type_ = "none" )

**void** **elec** ( "coulomb" )

**void** **elec** ( "ewald", **double** _alpha_, **int** _kx_, **int** _ky_, **int** _kz_ )

**void** **elec** ( "ewaldauto", **double** _precision_ )

Set the style of electrostatic energy calculation to use, either no electrostatics, coulombic (non-periodic) electrostatics, or Ewald-based electrostatics. For the latter, either the various parameters may be defined explicitly (when "ewald" is the chosen method) or may be estimated for the current system by using "ewaldauto".

---

## frameEnergy <a id="frameenergy"></a>

_Syntax:_

**double** **frameEnergy** ( )

Calculate energy of the current frame of the trajectory associated with the current model.

For example:

```aten
double energy = frameEnergy();
```

---

## modelEnergy <a id="modelenergy"></a>

_Syntax:_

**double** **modelEnergy** ( )

Calculate the energy of the current model, which can then be printed out (in whole or by parts) by the other subcommands.

For example:

```aten
double e = modelEnergy();
```

---

## printElec <a id="printelec"></a>

_Syntax:_

**void** **printElec** ( )

Prints out the electrostatic energy decomposition matrix.

For example:

```aten
printElec();
```

---

## printEwald <a id="printewald"></a>

_Syntax:_

**void** **printEwald** ( )

Prints the components of the Ewald sum energy.

For example:

```aten
printEwald();
```

---

## printInter <a id="printinter"></a>

_Syntax:_

**void** **printInter** ( )

Prints out the total inter-pattern energy decomposition matrix.

For example:

```aten
printInter();
```

---

## printIntra <a id="printintra"></a>

_Syntax:_

**void** **printIntra** ( )

Prints out the total intramolecular energy decomposition matrix.

For example:

```aten
printIntra();
```

---

## printEnergy <a id="printenergy"></a>

_Syntax:_

**void** **printEnergy** ( )

Prints the elements of the calculated energy in a list.

For example:

```aten
printEnergy();
```

---

## printSummary <a id="printsummary"></a>

_Syntax:_

**void** **printSummary** ( )

Print out a one-line summary of the calculated energy.

For example:

```aten
printSummary();
```

---

## printVdw <a id="printvdw"></a>

_Syntax:_

**void** **printVdw** ( )

Prints out the VDW energy decomposition matrix.

For example:

```aten
printVdw();
```


