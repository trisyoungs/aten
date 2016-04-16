---
title: Pores Commands
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

---

## createScheme <a id="createscheme"></a>

_Syntax:_

**int** **createScheme** ( **string** _name_, **int** _nx_ = 50, **int** _ny_ = 50, **int** _nz_ = 50, **double** _minSizePcnt_ = 0.05, **int** _atomExtent_ = 2, **bool** _copyToBuilder_ = **TRUE** )

Create a partitioning scheme (called _name_) from empty space in the current model. An _nx_×_ny_×_nz_ grid is created and scheme partitions generated based on adjacent free cells in the grid once those containing atoms have been removed. Cells adjacent to those containing atoms are also removed, based on the _atomExtent_, which can be viewed as an integer defining the radius (in cells) of ‘spherical’ region around each atom.  The defaults offer sensible values, but these may need to be tweaked when, for example, small pores are present. The minimum size (in grid cells) allowable for any discovered partition is governed by the _minSizePcnt_ parameter multiplied by the total number of grid points. The final argument, _copyToBuilder_, determines whether the generated scheme is immediately copied to the Disorder builder, ready for use.  The default is **TRUE**, but this can be set to **FALSE** if, for instance, you wish to adjust the other parameters in order to get the number and size of partitions you require first, before committing a scheme to the Disorder builder. The number of partitions found is returned.

For example:

```aten
createScheme("", 50, 50, 50, 0.02, 2, TRUE);
```

---

## drillPores <a id="drillpores"></a>

_Syntax:_

**void** **drillPores** ( **string** _geometry_, **double** _sizeParameter_, **int** _nA_, **int** _nB_, **int** _originFace_ = 3, **double** _vx_ = 0.0, **double** _vy_ = 0.0, **double** _vz_ = 1.0 )

Drills an _nA_ × _nB_ array of pores of the specified _geometry_ in the current model. The _originFace_ of the pores defaults to the XY (AB) plane, but the YZ (BC) and XZ (AC) faces may be selected instead by specifying _originFace_ as 1 or 2 respectively. Similarly, the vector along which the individual pores are drilled defaults to be normal to the z axis (0.0,0.0,1.0), but may be specified to any other vector suitable.  The vector is normalised before use, so a vector of any magnitude may be specified (for example, the components of a cell axis vector, making drilling pores along crystal axes quite simple).

At present, the only implemented geometry is "cylindrical".

For example:

```aten
drillPores("cylindrical", 5.0, 3, 3);
```

creates a 3x3 array of cylindrical pores of radius 5.0 A along the z-axis (the default).
```aten
loadModel("data/test/amorphous-silica.ato");
drillPores("cylindrical", 5.0, 1, 3, 2, 25.0, 43.3013, 0.0);
```

creates a 1×3 array of cylindrical pores of radius 5.0 Å along the example amorphous silica model cell’s B axis.

---

## selectPores <a id="selectpores"></a>

_Syntax:_

**int** **selectPores** ( **string** _geometry_, **double** _sizeParameter_, **int** _nA_, **int** _nB_, **int** _originFace_ = 3, **double** _vx_ = 0.0, **double** _vy_ = 0.0, **double** _vz_ = 1.0 )

This acts as a complement to the **drillPores** command, operating in exactly the same way except that atoms which would be in the pores are not deleted from the model. Obviously, this can be used to assess the exact positions of pores before they are cut from the model.  Furthermore, once the pores have been drilled proper, by increasing the size parameter and using **selectPores** atoms in the pore wall can be selected and then OH terminated with the **terminate** command.  The number of atoms selected is returned. Note that any existing atom selection is cleared before the new pore atoms are selected.

For example:
```aten
drillPores("cylindrical", 5.0, 3, 3);
selectPores("cylindrical", 7.0, 3, 3);
```

As in the **drillPores** example, a 3×3 array of pores is drilled, and then all atoms in a 2 Å ‘wall slice’ are selected around the edge of each pore.

---

## terminate <a id="terminate"></a>

_Syntax:_

**void** **terminate** ( )

Adds hydrogen atoms and OH groups to atoms in the current selection, in order to completely satisfy the bonding requirements of the selected atoms. Only affects oxygen and silicon atoms at present.

For example:

```aten
terminate();
```


