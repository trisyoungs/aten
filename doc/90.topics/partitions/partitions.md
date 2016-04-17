---
title: Partitioning Schemes
brief: Using and creating partitioning schemes for disordered building
visible: true
template: manpage
taxonomy:
  category: docs
docroot: /aten/docs
header_class: alt
---

Partitions (or 'partitioning schemes') are used in the [Disorder Builder](/aten/docs/gui/disorder) to allow target molecules to be restricted into specific volumes in space, providing a means to define arbitrary functions by which to determine these volumes.

**Aten** comes with a set of basic partitioning schemes, located in the `partitions` directory of the [main data directory](/aten/docs/installation/data). User-defined partitions can be placed in the [user data directory](/aten/docs/user/location).

## Scheme Blueprint

A partition file defines several variables and functions:

```aten
# Name / Description
string name = "ShortName";
string description = "Scheme Description";
int nPartitions = 2;

# Options dialog (optional)
int partitionOptions() { ... }

# Point test function
int partition(double px, double py, double pz) { ... }

# Partition name function
string partitionName(int id) { ... }
```

The `name` and `description` **string** variables define the short name and descriptive text of the scheme, as displayed in the GUI and used when referencing schemes from the [**disorder**](/aten/docs/scripting/commands/disorder#disorder) command. Importantly, the total number of partitions defined within the scheme is defined by `nPartitions`. Any number of partitions may be defined, but the entire unit cell must be covered. Usually this means defining a number of 'regions of interest', and whatever else is left out is the 'unit cell'.

A scheme may provide options to the user through **Aten**'s ability to create dialogs by providing a `partitionOptions()` function. This function should create its own dialog and then store any necessary values used by the other functions in global variables.

The two primary functions in the definition are the integer function `partition` and the string function `partitionName`. `partition` represents the main function of the scheme. It is called repeatedly with fractional (i.e. ranging from 0.0 to 1.0 inclusive) coordinates `{px,py,pz}`, and its job is to return the integer index (from zero to `nPartitions`) of the partition that this point exists in. The purpose of `partitionName` is to retrieve the names of each different partition defined in the scheme - given an integer index from zero to `nPartitions` inclusive, it should return a descriptive name for that partition. 

## Full Example

A full example is probably useful at this point, so here is a definition of a spherical partition:

```aten
# Name / Description
string name = "Sphere";
string description = "Simple spherical region";
int nPartitions = 2;

# Partition Options
global double x = 0.5, y = 0.5, z = 0.5, r = 0.3;
int partitionOptions()
{
	Dialog ui = createDialog("Sphere Scheme Options");
	ui.verticalFill = TRUE;
	ui.addDoubleSpin("x", "x", 0.0, 1.0, 0.1, x);
	ui.addDoubleSpin("y", "Y", 0.0, 1.0, 0.1, y);
	ui.addDoubleSpin("z", "Z", 0.0, 1.0, 0.1, z);
	ui.addDoubleSpin("r", "R", 0.001, 1.0, 0.1, r);
	if (ui.show())
	{
		x = ui.asDouble("x");
		y = ui.asDouble("y");
		z = ui.asDouble("z");
		r = ui.asDouble("r");
		return TRUE;
	}
	else return FALSE;
}

# Main partition function
int partition(double px, double py, double pz)
{
	# When constructing the insertion grid, Aten will call this function to determine which points on the grid fall within which partitions.
	# Unit cell coordinates are always given. The function should return the integer ID number of the partition in which the point
	# falls, or zero to mean the point falls in no partition at all (i.e. it is elsewhere in the cell)
	# Partitions *must* be numbered consecutively from zero upwards.
	// Check 1 - Is the point inside the defined sphere (region 1)
	if (( (px-x)*(px-x) + (py-y)*(py-y) + (pz-z)*(pz-z) ) <= (r*r)) return 1;

	// Not inside any defined regions, so return '0' for 'inside rest of cell'
	return 0;
}

# Partition names
string partitionName(int id)
{
	switch(id)
	{
		case (0):
			return "Unit Cell";
		case (1):
			return "Sphere";
		default:
			return "UNKNOWN";
	}
}
```

The scheme defines two partitions (`nPartitions` = 2) - inside the sphere, and outside the sphere (the remainder of the unit cell). The options dialog allows the position of the spherical region to be controlled, along with the sphere radius. Note that four global **double** variables - `x`, `y`, `z`, and `r` - are created and set to what will be the default values of the sphere position (in fractional coordinates - in this case the centre of the cell) and radius. They are defined as global because they are used by the `partition` function in the sphere equation.

The main `partition` function makes one simple test - whether the supplied point is inside a sphere whose centre is located at `{x,y,z}` and which has radius `r`. If it **is** inside the sphere then the function returns partition index `1`. If not, then `0` is returned to denote 'in the rest of the cell'. Correspondingly, the `partitionName` function returns the name "Unit Cell" for partition zero, and "Sphere" for partition 1.
