---
title: File Formats
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

A list of formats currently supported by **Aten** follows as well as the file extensions and assigned filter IDs. Remember, adding support for other codes and formats is in your hands with **Aten**’s [filters](XXXfilters).
		
## Model Formats

| Format | Extension(s) | Nickname | ID | R/W | Notes |
|--------|--------------|----------|----|-----|-------|
| **Aten** Keyword Format | *.akf | akf | 1 | RW | Plain-text format used by **Aten** |
| Cambridge Structural Database Service | *.dat|*.fdat | csd | 7 | RO | |
| Chem3D Cartesian Coordinates | *.cc1 | *.cc2 | cc | 34 | RO | |
| Crystallographic Information File | *.cif | cif | 8 | RO | |
| DL_POLY Configuration | *CONFIG | *REVCON | dlpoly | 2 | RW | |
| EPSR ATO Files | *.ato | ato | 18 | RW | |
| GAMESS-US Output (Log | *.log | gamuslog | 11 | RO | |
| GAMESS-US Input File | *.inp | gamusinp | 5 | RW | Cartesian coordinates and Z-Matrices |
| Gaussian Input File | *.gjf | gjf | 17 | WO | Cartesian coordinates and Z-Matrices | 
| Gromacs Configuration | *.gro | gro | 14 | RW | | 
| Mopac Archive File | *.arc | arc | 16 | RO | |
| Mopac Control File | *.mop | mop | 4 | RW | Including periodic systems. Cartesian coordinates only |
| MDL Molfile | *.mol | mol | 10 | RO | |
| MSI (Cerius2) Model File | *.msi | msi | 12 | RO | |
| Protein Databank (PDB | *.pdb | pdb | 13 | RO | | 
| Quantum Espresso | *.in | in | 15 | RW | |
| SIESTA Flexible Data Format | *.fdf | siesta | 9 | RW | |
| Tripos Sybyl Mol2 | *.mol2 | mol2 | 6 | RW | | 
| XMol XYZ | *.xyz | xyz | 3 | RW | |

## Trajectory Formats

| Format | Extension(s) | Nickname | Notes |
|--------|--------------|----------|-------|
| DL_POLY Formatted &amp; Unformatted Trajectories | *HISu *HISf HISTORY | dlpoly | |
| GAMESS-US Trj File | *.trj | trj | QM atoms in MD and IRC runs |
| PDB Trajectory (Multiple PDB file | *.pdb | pdb | |
| Siesta XV | *.XV | xv | | 
| XYZ Trajectory (Multiple XYZ file | *.xyz | xyz | |

## Grid Data Formats

| Format | Extension(s) | Nickname | ID | Notes |
|--------|--------------|----------|----|-------|
| Gaussian Cube | *.cube | cube | 1 | Also exists as importmodel filter |
| Probability density | *.pdens | pdens | 2 | Simple 3D volumetric data format |
| Surface | *.surf | surf | 3 | Simple 2D surface data format |

## Expression Data Formats

| Format | Extension(s | Nickname | ID | Read | Write | Notes |
|--------|-------------|----------|----|------|-------|-------|
| DL_POLY FIELD file | *.FIELD FIELD | dlpoly | 1 | no | yes | |
| Gromacs RTP file | *.rtp | rtp |    | no | yes | Preliminary version |
| Gromacs TOP file | *.top | top | 14 | no | yes | |

