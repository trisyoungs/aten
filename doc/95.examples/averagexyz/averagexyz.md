---
title: Average Coordinates Script
brief: Script to calculate average coordinates (no PBC) of the current model
taxonomy:
  category: docs
  example: [scripting, calculate]
visible: true
template: manpage
docroot: /aten/docs
header_class: alt
---

This example takes the supplied water trajectory and determines the average coordinates of all the individual water molecules. Note that no accounting for the periodicity of the system is taken into account.

```aten
# Create array for coordinates, and get number of frames / atoms
Vector xyz[aten.model.nAtoms];
int nFrames = aten.model.nFrames;
int nAtoms = aten.model.nAtoms;

printf("Number of frames is %i\n", nFrames);

# Loop over trajectory frames
for (Model f = aten.model.frames; f; ++f)
{
      for (int n=1; n<=nAtoms; ++n) xyz[n] += f.atoms[n].r;
}

# Divide coordinates through by number of frames
for (int n=1; n<=nAtoms; ++n) xyz[n] = xyz[n] / nFrames;

# Create a new model to display the results
# Base it on the first frame of the trajectory
Model f = aten.model.frames;
newModel("Average");
for (int n=1; n<= nAtoms; ++n)
{
      newAtom(f.atoms[n].z, xyz[n].x, xyz[n].y, xyz[n].z);
}
```

Copy and save this to a file named `avgeom.txt`.

To run the example:

```
bob@pc:~> aten --zmap singlealpha data/test/water66-spcfw.CONFIG -t data/test/water66-spcfw.HISu -s avgeom.txt
```


