---
title: MonteCarlo
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

The [**MonteCarlo**](/aten/docs/scripting/variabletypes/montecarlo) type stores various quantities which affect both the Monte Carlo minimiser and the disorder builder (see Section 15.1.6). Variables of thie type cannot be created by the user – the sole instance of it exists as part of the [**Aten**](/aten/docs/scripting/variabletypes/aten) global variable.

| Member | Type | RW | Description |
|--------|------|----|-------------|
| disorderAccuracy | **double** | • | Strictness of adherence to requested component populations and densities |
| disorderDeltaAngle | **double** | • | Maximum angle for molecular rotations |
| disorderDeltaDistance | **double** | • | Maximim distance for molecular translations |
| disorderMaxCycles | **double** | • | Maximum number of cycles to perform |
| disorderMaxFailures | **double** | • | Maximum failure rate per component before scale factor is reduced |
| disorderMaximumScaleFactor | **double** | • | Maximum scale factor to employ at start of build, and target scale factor for recovery |
| disorderMinimumScaleFactor | **double** | • | Minimum scale factor to allow in build |
| disorderNTweaks | **int** | • | Number of molecule tweaks to perform per disorder cycle |
| disorderRecoveryMaxCycles | **int** | • | Maximum number of recovery cycles to perform |
| disorderRecoveryMaxTweaks | **int** | • | Maximum number of tweaks to perform per cycle in recovery |
| disorderRecoveryThreshold | **double** | • | TODO |
| disorderReductionFactor | **double** | • | TODO |
| nCycles | **int** | • | Maximum number of Monte Carlo cycles to perform |
| temperature | **double** | • | Temperature for Monte Carlo ‘simulation’ |

## MonteCarlo Type Functions

### eAccept <a id="eaccept"></a>

_Syntax:_

**double** **eAccept** ( **string** _moveType_, **double** _newValue_ = (none) )

Return the current acceptance energy threshold for the specified _moveType_, setting to the _newValue_ if one is provided. See Section 16.13 for a list of move types.

---

### maxStep <a id="maxstep"></a>

_Syntax:_

**double** **maxStep** ( **string** _moveType_, **double** _newValue_ = (none) )

Return the current maximum step size for the specified _moveType_ (if applicable), setting to the newValue if one is provided. See Section 16.13 for a list of move types.

---

### moveAllowed <a id="moveallowed"></a>

_Syntax:_

**int** **moveAllowed** ( **string** _moveType_, **int** _newValue_ = (none) )

Return whether the specified _moveType_ is allowed to take place, setting to the _newValue_ if one is provided. See Section 16.13 for a list of move types.

---

### nTrials <a id="ntrials"></a>

_Syntax:_

**double** **nTrials** ( **string** _moveType_, **int** _newValue_ = (none) )

Return the current number of trials for each _moveType_, setting to the _newValue_ if one is provided. See Section 16.13 for a list of move types.


