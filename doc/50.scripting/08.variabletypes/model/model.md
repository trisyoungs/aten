---
title: Model
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

The [**Model**](/aten/docs/scripting/variabletypes/model) type contains all the information describing a single model within Aten, including bonds, grid data, measurements, etc..
 
| Member | Type | RW | Description |
|--------|------|----|-------------|
| angles | [**Measurement**](/aten/docs/scripting/variabletypes/measurement)[]  | | List of current angle measurements in the model |
| atoms | [**Atom**](/aten/docs/scripting/variabletypes/atom)[] | | Array of atoms in the model |
| bonds | [**Bond**](/aten/docs/scripting/variabletypes/bond)[] | | Array of bonds defined in the model |
| cell | [**UnitCell**](/aten/docs/scripting/variabletypes/unitcell) | • | The model’s unit cell |
| componentDensity | **double** | • | Requested density of the model |
| componentPartition | **int** | • | The integer index of the partition which this model will be added to in the disordered builder |
| componentPolicy | **string** | • | Insertion policy for this model |
| componentPopulation | **int** | • | The number of times this model will be added to the specified partition in the disordered builder |
| distances | [**Measurement**](/aten/docs/scripting/variabletypes/measurement)[] | | List of current distance measurements in the model |
| eigenvectors | [**Eigenvector**](/aten/docs/scripting/variabletypes/eigenvector)[] | | List of current eigenvectors stored in the model |
| energy | [**EnergyStore**](/aten/docs/scripting/variabletypes/energystore) | | The model’s energy store, containing the total calculated energy and all associated contributions |
| ff | [**Forcefield**](/aten/docs/scripting/variabletypes/forcefield) | • | Forcefield associated to the model (if any) |
| ffAngles | [**FFBound**](/aten/docs/scripting/variabletypes/ffbound)[] | | List of unique forcefield angle terms in the model |
| ffBonds | [**FFBound**](/aten/docs/scripting/variabletypes/ffbound)[] | | List of unique forcefield bond terms in the model |
| ffMass | **double** | | Forcefield mass of the current model, which can differ from the actual mass if united-atom types have been assigned |
| ffTorsions | [**FFBound**](/aten/docs/scripting/variabletypes/ffbound)[] | | List of unique forcefield torsion terms in the model |
| ffTypes | [**FFAtom**](/aten/docs/scripting/variabletypes/ffatom)[] | | Array of unique atom types used in the model |
| frame | [**Model**](/aten/docs/scripting/variabletypes/model) | | The current frame in the model's trajectory (if it has one) |
| frames | [**Model**](/aten/docs/scripting/variabletypes/model)[] | | Array of trajectory frame pointers (only if the trajectory is cached) |
| glyphs | [**Glyph**](/aten/docs/scripting/variabletypes/glyph)[] | | List of glyphs owned by the model |
| grids | [**Grid**](/aten/docs/scripting/variabletypes/grid)[] | | List of grids owned by the model |
| id | **int** | | The index of the model in Aten's internal list of loaded models |
| mass | **double** | | Mass of the current model |
| name | **string** | • | Name of the model |
| nAngles | **int** | | Number of angle measurements in the model |
| nAtoms | **int** | | Number of atoms in the model |
| nBasisCartesians | **int** | | Number of cartesian basis functions defined in stored basis shells |
| nBasisShells | **int** | | Number of basis shells defined in the model |
| nBonds | **int** | | Number of bonds in the model |
| nDistances | **int** | | Number of distance measurements in the model |
| nFFAngles | **int** | | Number of unique angle terms used in the model |
| nFFBonds | **int** | | Number of unique bond terms used in the model |
| nFFTorsions | **int** | | Number of unique torsion terms used in the model |
| nFFTypes | **int** | | Number of unique atom types used in the model |
| nFrames | **int** | | Number of frames in associated trajectory |
| nGlyphs | **int** | | Number of glyphs owned by the model |
| nGrids | **int** | | Number of grids owned by the model |
| nPatterns | **int** | | Number of patterns defined for the model |
| nSelected | **int** | | Number of atoms selected in the model |
| nTorsions | **int** | | Number of torsion angle measurements in the model |
| nUnknown | **int** | | Number of atoms in the model that are of unknown element |
| patterns | [**Pattern**](/aten/docs/scripting/variabletypes/pattern)[] | | Array of patterns currently defined for the model |
| selection | [**Atom**](/aten/docs/scripting/variabletypes/atom)[] | | A list of atoms in representing the current atom selection of the model |
| torsions | [**Measurement**](/aten/docs/scripting/variabletypes/measurement)[] | | List of current torsion angle measurements in the model |

## Model Type Functions

### atomWithBit <a id="atomwithbit"></a>

_Syntax:_

[**Atom**](/aten/docs/scripting/variabletypes/atom) **atomWithBit** ( **int** _bit_ )

Returns the first [**Atom**](/aten/docs/scripting/variabletypes/atom) in the [**Model**](/aten/docs/scripting/variabletypes/model) which has the specified value of bit (see the relevant accessor above).

---

### copy <a id="copy"></a>

_Syntax:_

**void** **copy** ( [**Model**](/aten/docs/scripting/variabletypes/model) _source_ )

Copy all information from the source [**Model**](/aten/docs/scripting/variabletypes/model) into this [**Model**](/aten/docs/scripting/variabletypes/model).

---

### addHydrogen <a id="addhydrogen"></a>

_Syntax:_

**void** **addHydrogen** ( )

Hydrogen satisfy all atoms in the model. See the [**addHydrogen**](/aten/docs/scripting/commands/building#addhydrogen) command for more details.

---

### angleEnergy <a id="angleenergy"></a>

_Syntax:_

**double** **angleEnergy** ( )

Calculates and returns the total angle energy for the current model.

---

### augment <a id="augment"></a>

_Syntax:_

**void** **augment** ( )

Automatically detect and add multiple bonds in the system. See the [**augment**](/aten/docs/scripting/commands/bond#augment) command for more details.

---

### bondEnergy <a id="bondenergy"></a>

_Syntax:_

**double** **bondEnergy** ( )

Calculates and returns the total bond energy (including Urey-Bradley contributions) for the current model.

---

### charge <a id="charge"></a>

_Syntax:_

**void** **charge** ( )

Assign charges to the model from the current forcefield. See the [**chargeFF**](/aten/docs/scripting/commands/charges#charge) command for more details.

---

### clearBonds <a id="clearbonds"></a>

_Syntax:_

**void** **clearBonds** ( )

Remove all bonds from the model. See the [**clearBonds**](/aten/docs/scripting/commands/bond#clearbonds) command for more details.

---

### clearCharges <a id="clearcharges"></a>

_Syntax:_

**void** **clearCharges** ( )

Remove al charges from the model, setting them to zero. See the [**clearCharges**](/aten/docs/scripting/commands/charges#clearcharges) command for more details.

---

### clearSelectedBonds <a id="clearselectedbonds"></a>

_Syntax:_

**void** **clearSelectedBonds** ( )

Remove all bonds from the current atom selection. See the [**clearSelectedBonds**](/aten/docs/scripting/commands/bond#clearselectedbonds) command for more details.

---

### copy <a id="copy"></a>

_Syntax:_

**void** **copy** ( )

Copy the current atom selection to the clipboard. See the [**copy**](/aten/docs/scripting/commands/edit#copy) command for more details.

---

### cut <a id="cut"></a>

_Syntax:_

**void** **cut** ( )

Cut the current atom selection to the clipboard. See the [**cut**](/aten/docs/scripting/commands/edit#cut) command for more details.

---

### delete <a id="delete"></a>

_Syntax:_

**void** **delete** ( )

Delete the current atom selection. See the [**delete**](/aten/docs/scripting/commands/edit#delete) command for more details.

---

### elecEnergy <a id="elecenergy"></a>

_Syntax:_

**double** **elecEnergy** ( )

Calculates and returns the total electrostatic energy for the current model, using the calculation method specified in the preferences.

---

### expand <a id="expand"></a>

_Syntax:_

**void** **expand** ( )

Expand the current atom selection along bonds. See the [**expand**](/aten/docs/scripting/commands/selection#expand) command for more details.

---

### finalise <a id="finalise"></a>

_Syntax:_

**void** **finalise** ( )

Finalise the current model. See the [**finaliseModel**](/aten/docs/scripting/commands/model#finalisemodel) command for more details.

---

### interEnergy <a id="interenergy"></a>

_Syntax:_

**double** **interEnergy** ( )

Calculates and returns the total intermolecular (i.e. combined van der Waals and electrostatic) energy for the current model.

---

### intraEnergy <a id="intraenergy"></a>

_Syntax:_

**double** **intraEnergy** ( )

Calculates and returns the total intramolecular (i.e. combined bond, angle, and torsion) energy for the current model.

---

### moveToEnd <a id="movetoend"></a>

_Syntax:_

**void** **moveToEnd** ( )

Move the current atom selection to the bottom (highest IDs) of the atom list. See the [**moveToEnd**](/aten/docs/scripting/commands/building#movetoend) command for more details.

---

### moveToStart <a id="movetostart"></a>

_Syntax:_

**void** **moveToStart** ( )

Move the current atom selection to the top (lowest IDs) of the atom list. See the [**moveToStart**](/aten/docs/scripting/commands/building#movetostart) command for more details.

---

### newAtom <a id="newatom"></a>

_Syntax:_

[**Atom**](/aten/docs/scripting/variabletypes/atom) **newAtom** ( **int|string** _el_, **double** _x_ = 0.0, **double** _y_ = 0.0, **double** _z_ = 0.0, **double** _vx_ = 0.0, **double** _vy_ = 0.0, **double** _vz_ = 0.0, **double** _fx_ = 0.0, **double** _fy_ = 0.0, **double** _fz_ )

Create a new atom in the model. See the [**newAtom**](/aten/docs/scripting/commands/building#newatom) command for more details.

---

### newAtomFrac <a id="newatomfrac"></a>

_Syntax:_

[**Atom**](/aten/docs/scripting/variabletypes/atom) **newAtomFrac** ( **int|string** _el_, **double** _x_, **double** _y_, **double** _z_, **double** _vx_ = 0.0, **double** _vy_ = 0.0, **double** _vz_ = 0.0, **double** _fx_ = 0.0, **double** _fy_ = 0.0, **double** _fz_ )

Create a new atom in the model, in fractional coordinates. See the [**newAtomFrac**](/aten/docs/scripting/commands/building#newatomfrac) command for more details.

---

### newBasisShell <a id="newbasisshell"></a>

_Syntax:_

[**BasisShell**](/aten/docs/scripting/variabletypes/basisshell) **newBasisShell** ( [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _i_, **string** _type_ )

Create a new basis shell definition in the model, centred on the specified atom/id, and of the given shell type. See the [**newBasisShell**](/aten/docs/scripting/commands/modelx#newbasisshell) command for more details.

---

### newBond <a id="newbond"></a>

_Syntax:_

[**Bond**](/aten/docs/scripting/variabletypes/bond) **newBond** ( [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _i_, [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _j_, **string|int** _bondtype_ = &quot;&quot; )

Create a new bond between atoms in the model. See the [**newBond**](/aten/docs/scripting/commands/bond#newbond) command for more details.

---

### newBondId <a id="newbondid"></a>

_Syntax:_

[**Bond**](/aten/docs/scripting/variabletypes/bond) **newBondId** ( **int** _id_i_, **int** _id_j_, **string|int** _bondtype_ = &quot;&quot; )

Create a new bond between atom IDs in the model. See the [**newBond**](/aten/docs/scripting/commands/bond#newbondid) command for more details.

---

### newEigenvector <a id="neweigenvector"></a>

_Syntax:_

[**Eigenvector**](/aten/docs/scripting/variabletypes/eigenvector) **newEigenvector** ( **int** _size_ = (auto) )

Create a new eigenvector in the model of the specified size. If the size is not specified, the vector length is set to match the number of cartesian basis functions store with the current basis shell definitions in the model. See the [**newEigenvector**](/aten/docs/scripting/commands/modelx#neweigenvector) command for more details.

---

### newGlyph <a id="newglyph"></a>

_Syntax:_

[**Glyph**](/aten/docs/scripting/variabletypes/glyph) **newGlyph** ( **string** _style_, **string** _options_ = &quot;&quot; )

Create a new glyph in the model. See the [**newGlyph**](/aten/docs/scripting/commands/glyph#newglyph) command for more details.

---

### newGrid <a id="newgrid"></a>

_Syntax:_

[**Grid**](/aten/docs/scripting/variabletypes/grid) **newGrid** ( **string** _name_ )

Create a new gridh in the model. See the [**newGrid**](/aten/docs/scripting/commands/grid#newgrid) command for more details.

---

### paste <a id="paste"></a>

_Syntax:_

**void** **paste** ( )

Paste the current clipboard contents into the model. See the [**paste**](/aten/docs/scripting/commands/edit#paste) command for more details.

---

### rebond <a id="rebond"></a>

_Syntax:_

**void** **rebond** ( )

Calculate bonds in the model. See the [**rebond**](/aten/docs/scripting/commands/bond#rebond) command for more details.

---

### rebondPatterns <a id="rebondpatterns"></a>

_Syntax:_

**void** **rebondPatterns** ( )

Calculate bonds within patterns in the model. See the [**rebondPatterns**](/aten/docs/scripting/commands/bond#rebondpatterns) command for more details.

### rebondSelection <a id="rebondselection"></a>

_Syntax:_

**void** **rebondSelection** ( )

Calculate bonds in the current selection. See the [**rebondSelection**](/aten/docs/scripting/commands/bond#rebondselection) command for more details.

---

### redo <a id="redo"></a>

_Syntax:_

**void** **redo** ( )

Redo the last undone change in the model. See the [**redo**](/aten/docs/scripting/commands/edit#redo) command for more details.

---

### reorder <a id="reorder"></a>

_Syntax:_

**void** **reorder** ( )

Reorder atoms so bound atoms have adjacent IDs. See the [**reorder**](/aten/docs/scripting/commands/building#reorder) command for more details.

---

### saveBitmap <a id="savebitmap"></a>

_Syntax:_

**void** **saveBitmap** ( **string** _format_, **string** _filename_, **int** _width_ = (auto), **int** _height_ = (auto), **int** _quality_ = 100 )

Save a bitmap image of the current model view. See the [**saveBitmap**](/aten/docs/scripting/commands/image#savebitmap) command for more details.

---

### selectAll <a id="selectall"></a>

_Syntax:_

**void** **selectAll** ( )

Select all atoms in the model. See the [**selectAll**](/aten/docs/scripting/commands/selection#selectall) command for more details.

---

### selectionAddHydrogen <a id="selectionaddhydrogen"></a>

_Syntax:_

**void** **selectionAddHydrogen** ( )

Hydrogen satisfy all atoms in the current selection. See the [**selectionAddHydrogen**](/aten/docs/scripting/commands/building#selectionaddhydrogen) command for more details.

---

### selectNone <a id="selectnone"></a>

_Syntax:_

**void** **selectNone** ( )

Deselect all atoms in the model. See the [**selectNone**](/aten/docs/scripting/commands/selection#selectnone) command for more details.

---

### selectTree <a id="selecttree"></a>

_Syntax:_

**int** **selectTree** ( [**Atom**](/aten/docs/scripting/variabletypes/atom) _i_, [**Bond**](/aten/docs/scripting/variabletypes/bond) _exclude_ = **NULL** )

Select all atoms from atom _i_ reachable by following any number of chemical bonds. See the [**selectNone**](/aten/docs/scripting/commands/selection#selecttree) command for more details.

---

### shiftDown <a id="shiftdown"></a>

_Syntax:_

**void** **shiftDown** ( **int** _n_ = 1 )

Shift the current atom selection down one (or more) places in the atom list (towards higher IDs). See the [**shiftDown**](/aten/docs/scripting/commands/building#shiftdown) command for more details.

---

### shiftUp <a id="shiftup"></a>

_Syntax:_

**void** **shiftUp** ( **int** _n_ = 1 )

Shift the current atom selection up one (or more) places in the atom list (towards lower IDs). See the [**shiftUp**](/aten/docs/scripting/commands/building#shiftup) command for more details.

---

### showAll <a id="showall"></a>

_Syntax:_

**void** **showAll** ( )

Unhides any hidden atoms in the model. See the [**showAll**](/aten/docs/scripting/commands/model#showall) command for more details.

---

### toAngstroms <a id="toangstroms"></a>

_Syntax:_

**void** **toAngstroms** ( )

Converts cell specification and atomic coordinates in the model from (assumed units of) Bohr into Angstroms. No changes to associated trajectory frames or grid data is made.

---

### torsionEnergy <a id="torsionenergy"></a>

_Syntax:_

**double** **torsionEnergy** ( )

Calculates and returns the total torsion energy (including improper terms) for the current model.

---

### transmute <a id="transmute"></a>

_Syntax:_

**void** **transmute** ( **int|string** el )

Transmute all selected atoms to the specified element. See the [**transmute**](/aten/docs/scripting/commands/building#transmute) command for more details.

---

### undo <a id="undo"></a>

_Syntax:_

**void** **undo** ( )

Undo the last change made to the model. See the [**undo**](/aten/docs/scripting/commands/edit#undo) command for more details.

---

### vdwEnergy <a id="vdwenergy"></a>

_Syntax:_

**double** **vdwEnergy** ( )

Calculates and returns the total van der Waals energy for the current model.


