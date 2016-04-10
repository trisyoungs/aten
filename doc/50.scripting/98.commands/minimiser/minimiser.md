---
title: Minimiser Commands
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

**double** **cgMinimise** ( **int** _maxsteps_ = 100, **double** _eConverge_ = 1.0e-3, **double** _fConverge_ = 1.0e-2, **double** _lineTolerance_ = 1.0e-4 )

Geometry optimises the current model using the conjugate gradient method. The final total energy of the model is returned.

For example:


```
cgMinimise(20);
```


runs a conjugate gradient geometry optimisation for a maximum of 20 cycles.

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

**double** **sdMinimise** ( **int** _maxsteps_ = 100, **double** _eConverge_ = 1.0e-3, **double** _fConverge_ = 1.0e-2, **double** _lineTolerance_ = 1.0e-4, **bool** _simple_ = **false** )

Optimises the current model using the Steepest Descent method. The final total energy of the model is returned.

For example:


```
sdMinimise(100);
```


minimises the current model for a maximum of 100 steps with a simple steepest descent minimiser.


