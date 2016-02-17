---
title: Minimser Commands
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

Perform energy minimisation on models.

---

## cgMinimise <a id="cgminimise"></a>

_Syntax:_

**void** **cgMinimise** ( **int** _maxsteps_ )

Geometry optimises the current model using the conjugate gradient method.

For example:


```
cgMinimise(20);
```


runs a conjugate gradient geometry optimisation for a maximum of 20 cycles.

Literature methods for details on the conjugate gradient method as it is implemented in Aten.

---

## converge <a id="converge"></a>

_Syntax:_

**void** **converge** ( **double** _econv_, **double** _fconv_ )

Sets the convergence criteria of the minimisation methods. Energy and force convergence values are given in the current working unit of energy in the program.

For example:


```
converge(1e-6, 1e-4);
```


sets the energy and RMS force convergence criteria to 1.0E-6 and 1.0E-4 respectively.

---

## lineTol <a id="linetol"></a>

_Syntax:_

**void** **lineTol** ( **double** _tolerance_ )

Sets the tolerance of the line minimiser.

For example:


```
lineTol(1e-5);
```


sets the line tolerance to 1.0E-5.

---

## mcMinimise <a id="mcminimise"></a>

_Syntax:_

**void** **mcMinimise** ( **int** _maxsteps_ )

Optimises the current model using a molecular Monte Carlo minimisation method.

For example:


```
mcMinimise(20);
```


runs a geometry optimisation for a maximum of 20 cycles.

Monte Carlo Minimiser method for details on the Monte Carlo minimisation method as it is implemented in Aten.

---

## mopacMinimise <a id="mopacminimise"></a>

_Syntax:_

**void** **mopacMinimise** ( **string** _options_ = [value]"BFGS PM6 RHF SINGLET"[/value] )

Optimises the current model using the external MOPAC program (Copyright 2007, Stewart Computational Chemistry). Note that the program must be installed correctly as per the instructions provided with it, and the path to the MOPAC executable must be set in Aten’s preferences, as well as a suitable temporary working directory. The optional argument allows a specific MOPAC command to be provided for the minimisation, but sensible defaults are used if this is not provided.

For example:


```
mopacMinimise();
```


minimises the current model with the default options listed above.


```
mopacMinimise("UHF TRIPLET PM6-DH2");
```


minimises the current model assuming a triplet state with the UHF method and the PM6-DH2 hamiltonian.

---

## sdMinimise <a id="sdminimise"></a>

_Syntax:_

**void** **sdMinimise** ( **int** _maxsteps_ )

Optimises the current model using the Steepest Descent method.

For example:


```
sdMinimise(100);
```


minimises the current model for a maximum of 100 steps with a simple steepest descent minimiser.


