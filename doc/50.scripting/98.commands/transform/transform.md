---
title: Transform Commands
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

Commands to transform the current selection of the model.

---

## axisRotate <a id="axisrotate"></a>

_Syntax:_

**void** **axisRotate** ( [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _i_, [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _i_, **double** _theta_ )

**void** **axisRotate** ( [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _i_, [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _i_, **double** _theta_, **double** _ox_, **double** _oy_, **double** _oz_ )

**void** **axisRotate** ( **double** _x_, **double** _y_, **double** _z_, **double** _theta_ )

**void** **axisRotate** ( **double** _x_, **double** _y_, **double** _z_, **double** _theta_, **double** _ox_, **double** _oy_, **double** _oz_ )

Rotate the current selection by an angle _theta_ (in degrees) about an axis defined either by the vector between two atom IDs or the vector components provided. If supplied, the rotation is performed using the coordinate origin specified by _ox_, _oy_, and _oz_, otherwise {0,0,0} is assumed.

For example:

```aten
axisRotate(4, 5, 90.0);
```

rotates the current selection 90 degrees about the axis formed by the vector between atom ids 4 and 5 (4→5).

```aten
axisRotate(0, 1, 0, -52.0);
```

rotates the current selection -52 degrees about the y-axis.

```aten
axisRotate(0, 1, 0, -52.0, 4.0, 4.0, 4.0);
```

rotates the current selection -52 degrees about the y-axis, but with the rotation centre at {4.0,4.0,4.0}.

---

## centre <a id="centre"></a>

_Syntax:_

**void** **centre** ( **double** _x_, **double** _y_, **double** _z_, **bool** _lockx_ = **FALSE**, **bool** _locky_ = **FALSE**, **bool** _lockz_ = **FALSE** )

Centre the current atom selection at the specified coordinates. The three optional arguments _lockx_, _locky_, and _lockz_ specify one or more atomic coordinates that are to remain unchanged during the transformation.

For example:

```aten
centre(0.0, 0.0, 15.0);
```

centres the current selection at the coordinates (0 0 15).

---

## flipX <a id="flipx"></a>

_Syntax:_

**void** **flipX** ( )

Flip (negate) the x-coordinates of the current selection.

For example:

```aten
flipX();
```

---

## flipY <a id="flipy"></a>

_Syntax:_

**void** **flipY** ( )

Flip (negate) the y-coordinates of the current selection.

For example:

```aten
flipY();
```

---

## flipZ <a id="flipz"></a>

_Syntax:_

**void** **flipZ** ( )

Flip (negate) the z-coordinates of the current selection.

For example:

```aten
flipZ();
```

---

## matrixConvert <a id="matrixconvert"></a>

_Syntax:_

**void** **matrixConvert** ( **int** _i_sx_, **int** _j_sx_, **int** _i_sy_, **int** _j_sy_, **int** _i_sz_, **int** _j_sz_, **int** _i_tx_, **int** _j_tx_, **int** _i_ty_, **int** _j_ty_, **int** _i_tz_, **int** _j_tz_ )

**void** **matrixConvert** ( **int** _i_sx_, **int** _j_sx_, **int** _i_sy_, **int** _j_sy_, **int** _i_sz_, **int** _j_sz_, **int** _i_tx_, **int** _j_tx_, **int** _i_ty_, **int** _j_ty_, **int** _i_tz_, **int** _j_tz_, **double** _ox_, **double** _oy_, **double** _oz_ )

**void** **matrixConvert** ( **double** _s_ax_, **double** _s_ay_, **double** _s_az_, **double** _s_bx_, **double** _s_by_, **double** _s_bz_, **double** _s_cx_, **double** _s_cy_, **double** _s_cz_, **double** _t_ax_, **double** _t_ay_, **double** _t_az_, **double** _t_bx_, **double** _t_by_, **double** _t_bz_, **double** _t_cx_, **double** _t_cy_, **double** _t_cz_ )

**void** **matrixConvert** ( **double** _s_ax_, **double** _s_ay_, **double** _s_az_, **double** _s_bx_, **double** _s_by_, **double** _s_bz_, **double** _s_cx_, **double** _s_cy_, **double** _s_cz_, **double** _t_ax_, **double** _t_ay_, **double** _t_az_, **double** _t_bx_, **double** _t_by_, **double** _t_bz_, **double** _t_cx_, **double** _t_cy_, **double** _t_cz_, **double** _ox_, **double** _oy_, **double** _oz_ )

From a defined frame of reference (i.e. a set of axes defining the spatial orientation), rotate the current selection from this frame into the second frame, using the coordinate origin supplied or {0,0,0} by default. In the first form six pairs of atom IDs define each matrix, with the vectors taken to be i→j in all cases (normalised to 1.0), and specifying the x, y, and z axes in turn. In the second, the matrices are given as two sets of nine numbers that define the vectors of the axes.

When supplying atom IDs, the x axis is taken to be absolute, the y-axis is orthogonalised w.r.t. the x-axis, and the z-axis is taken as the cross product between the x and y axes. Note that providing the definition of the z axis is still important, however, since the vector cross product is adjusted (if necessary) to point along the same direction as this supplied z-axis. When supplying the complete matrices no orthogonalisation or normalisation of the axes is performed (permitting arbitrary scale and shear operations).

For example:

```aten
matrixConvert(1, 2, 1, 3, 1, 4, 10, 11, 12, 13, 14, 15);
```

defines a the current selection's frame of reference as (in terms of atom IDs) X = (1→2), Y = (1→3), and Z = (1→4), which will be rotated such that it corresponds to the new frame of reference (again defined by atom IDs) X = (10→11), Y = (12→13), and Z = (14→15).

```aten
matrixConvert(-0.7348, -0.0192, -0.678, 0.4979, 0.6635, -0.5584, 0.4606, -0.7479, -0.47801, 1, 0, 0, 0, 1, 0, 0, 0, 1)
```

defines a the current selection's frame of reference as the vectors X={-0.7348, -0.0192, -0.678}, Y={0.4979, 0.6635, -0.5584}, and Z={0.4606, -0.7479, -0.47801}, which will be rotated into the standard reference frame.

---

## matrixTransform <a id="matrixtransform"></a>

_Syntax:_

**void** **matrixTransform** ( **double** _ax_, **double** _ay_, **double** _az_, **double** _bx_, **double** _by_, **double** _bz_, **double** _cx_, **double** _cy_, **double** _cz_ )

**void** **matrixTransform** ( **double** _ax_, **double** _ay_, **double** _az_, **double** _bx_, **double** _by_, **double** _bz_, **double** _cx_, **double** _cy_, **double** _cz_, **double** _ox_, **double** _oy_, **double** _oz_ )

Transform the current selection by applying the defined matrix to each coordinate, operating about the supplied origin (or {0,0,0} by default). No orthogonalisation or normalisation of the defined axes is performed.

For example:

```aten
matrixTransform(1, 0, 0, 0, 1, 0, 0, 0, 1);
```

does absolutely nothing (multiplying by the identity matrix).

```aten
matrixTransform(1, 0, 0, 0, 1, 0, 0, 0, -1);
```

mirrors the current selection in the xy plane.

```aten
matrixTransform(0.5, 0, 0, 0, 0.5, 0, 0, 0, -1);
```

scales the x and y-coordinates of all selected atoms by 0.5, leaving the z coordinates intact.

---

## reorient <a id="reorient"></a>

_Syntax:_

**void** **reorient** ( [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _i_sx_, [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _j_sx_, [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _i_sy_, [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _j_sy_, [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _i_sz_, [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _j_sz_, **double** _t_ax_, **double** _t_ay_, **double** _t_az_, **double** _t_bx_, **double** _t_by_, **double** _t_bz_, **double** _t_cx_, **double** _t_cy_, **double** _t_cz_ )

**void** **reorient** ( [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _i_sx_, [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _j_sx_, [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _i_sy_, [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _j_sy_, [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _i_sz_, [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _j_sz_, **double** _t_ax_, **double** _t_ay_, **double** _t_az_, **double** _t_bx_, **double** _t_by_, **double** _t_bz_, **double** _t_cx_, **double** _t_cy_, **double** _t_cz_, **double** _ox_, **double** _oy_, **double** _oz_ )

Operates in exactly the same manner as [matrixTransform()](/aten/docs/scripting/commands/transform#matrixtransform) except that the source matrix is defined from atoms (or their IDs) and the destination matrix is provided as a matrix.

---

## setAngle <a id="setangle"></a>

_Syntax:_

**void** **setAngle** ( [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _i_, [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _j_, [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _k_, **double** _angle_ )

Adjusts the angle made between atoms _i_-_j_-_k_ so that it becomes the target _value_, moving the atom _k_ and all its direct and indirectly-bound neighbours (except _i_ and _j_). The coordinates of atom _i_ and _j_ remain unaffected.

This operation can only be performed when atoms _j_ and _k_ are not present in the same cyclic structure. The atoms _i_, _j_, and _k_ do not have to be bound, so it is possible to move separate fragments relative to each other using this method.

For example:

```aten
setAngle(10,11,12,109.5);
```

sets the angle made between atoms 10, 11, and 12 to be 109.5°.

---

## setAngles <a id="setangles"></a>

_Syntax:_

**void** **setAngles** ( **double** _newAngle_, **string** _moveType_ = "high", **bool** _nudge_ = **false** )

Sets (or nudges, if _nudge_ is **true**) the angles of all bonds in the current atom selection. Attached atoms at either end of the angle are also moved along with the atoms  at the extreme ends of the angle. The _moveType_ determines exactly which atoms are moved in the process:

+ **low** : moves only the atom with the lowest ID (and its attached atoms)
+ **high** : moves only the atom with the highest ID (and its attached atoms)
+ **light** : moves only the atom with the lowest mass (and its attached atoms)
+ **heavy** : moves only the atom with the highest mass (and its attached atoms)
+ **both** : both atoms (and their attached atoms) are moved equal amounts

---

## setDistance <a id="setdistance"></a>

_Syntax:_

**void** **setDistance** ( [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _i_, [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _j_, **double** _dist_ )

Shifts the atom _j_ and all its direct and indirectly-bound neighbours (except _i_) so that the distance between _i_ and _j_ is _dist_. The coordinates of atom _i_ remain unaffected.

This operation can only be performed for atoms which are not present in the same cyclic structure, e.g. trying to set the distance of two atoms in a benzene ring is not allowed. However, note that the two atoms _i_ and _j_ do not have to be bound, so it is possible to move separate fragments further apart by this method.

For example:

```aten
setDistance(1,4,4.9);
```

sets the distance between atoms 1 and 4 to be 4.9 Å.

---

## setDistances <a id="setdistances"></a>

_Syntax:_

**void** **setDistances** ( **double** _newDistance_, **string** _moveType_ = "high", **bool** _nudge_ = **false** )

Sets (or nudges, if _nudge_ is **true**) the distances (lengths) of all bonds in the current atom selection. Attached atoms at either end of the bond are also moved along with the atoms in the bonds. The _moveType_ determines exactly which atoms are moved in the process (see the [setAngles() command](/aten/docs/scripting/commands/transform#setangles) for a description of the possible options.

---

## setTorsion <a id="settorsion"></a>

_Syntax:_

**void** **setTorsion** ( [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _i_, [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _j_, [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _k_, [**Atom**](/aten/docs/scripting/variabletypes/atom)|**int** _l_, **double** _angle_ )

Adjusts the atoms _i_ and _l_ (and all their attached neighbours) to give the torsion angle specified. The coordinates of atoms _j_ and _k_ remain unaffected.

This operation can only be performed for atoms which are not present in the same cyclic structure, e.g. trying to set the distance of two atoms in a benzene ring is not allowed.

---

## setTorsionsi <a id="settorsions"></a>

_Syntax:_

**void** **setTorsions** ( **double** _newTorsion_, **string** _moveType_ = "high", **bool** _nudge_ = **false** )

Sets (or nudges, if _nudge_ is **true**) all the torsion angles in the current atom selection. Attached atoms at either end of the torsion are also moved along with the atoms at the extreme ends of the torsions. The _moveType_ determines exactly which atoms are moved in the process (see the [setAngles() command](/aten/docs/scripting/commands/transform#setangles) for a description of the possible options.

---

## translate <a id="translate"></a>

_Syntax:_

**void** **translate** ( **double** _dx_, **double** _dy_, **double** _dz_ )

Translates the current selection by the specified vector.

For example:

```aten
translate(1, 0, 0);
```

moves the current selection 1 Å along the x axis.

---

## translateAtom <a id="translateatom"></a>

_Syntax:_

**void** **translateAtom** ( **double** _dx_, **double** _dy_, **double** _dz_ )

Translates the current atom by the specified vector.

For example:

```aten
translateAtom(1, 0, -1);
```

translates the current atom 1 Å along x and -1 Å along z.

---

## translateCell <a id="translatecell"></a>

_Syntax:_

**void** **translateCell** ( **double** _fracX_, **double** _fracY_, **double** _fracZ_ )

Translates the current selection by the fractional cell vectors specified. The model must have a unit cell for this command to work.

For example:

```aten
translateCell(0, 0.5, 0);
```

translates the current selection by an amount equivalent to half of the current cell’s B vector.

---

## translateWorld <a id="translateworld"></a>

_Syntax:_

**void** **translateWorld** ( **double** _dx_, **double** _dy_, **double** _dz_ )

Translates the current selection by the Å amounts specified, with the XY plane parallel to the monitor screen.

For example:

```aten
translateWorld(0, 0, 10);
```

translates the current selection 10 Å ‘into’ the screen.

---

## mirror <a id="mirror"></a>

_Syntax:_

**void** **mirror** ( **string** _axis_ )

Mirror the current selection in the specified plane about its geometric centre.

For example:

```aten
mirror("y");
```

mirrors the current selection about the y axis.

