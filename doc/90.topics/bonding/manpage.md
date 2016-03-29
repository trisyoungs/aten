---
title: Bonding
brief: How Aten calculates and augments bonding patterns
visible: true
template: manpage
taxonomy:
  category: docs
docroot: /aten/docs
header_class: alt
---

## Bond Calculation <a id="bonding"></a>

The most common means of determining connectivity between a collection of atoms is based on simple check of the actual distance between two atoms and the sum of their assigned radii:

The two σ represent the radii of atoms _i_ and _j_, and the parameter α is an adjustable tolerance value to enable fine-tuning, and using **Aten**’s set of built-in radii[1] usually lays between 1.0 and 2.0. For molecules or periodic systems of modest size the method can be used as is, but for large systems of many atoms the use of a double loop over atoms results in a very slow algorithm.

**Aten** overcomes this slowdown for larger systems by partitioning the system up into a series of overlapping ‘cuboids’. For a system of _N_ particles in a periodic box (or an isolated system with an orthorhombic pseudo-box determined by the extreme positions of atoms), the volume is partitioned into a number of subvolumes of some minimum size in each direction. The minimum size of any one of the subvolume’s dimensions is chosen relative to the maximum bond length possible given the largest elemental radius and the current bond tolerance α. A single loop over atoms is then performed to associate them to these subvolumes. Each atom belongs to at least one cuboid, determined by its absolute position in the system, and commonly belongs to one other cuboid, determined by adding half of the cuboids dimensions on to the atoms position. While a little counterintuitive, potentially adding atoms to a neighbouring cuboid along this diagonal vector allows the final calculation of distances between pairs of atoms to consider only eight neighbouring (more correctly, overlapping) subvolumes rather than the 26 needed if each atom belongs exclusively to only one cuboid. For atoms that exist in subvolumes along the edges of the whole volume, these are also added to the subvolume(s) on the opposite side(s) to account for minimum image effects in periodic systems.

Once the effort has been made to assign atoms to cuboids, the final loops to calculate distances runs over a much reduced subset of atom pairs owing to the partitioning. A loop over cuboids is performed, first considering all atom pairs within the same cuboid, and then extending this to consider distances between a particular atom of this central cuboid and its eight ‘overlapping’ neighbours.

There is some redundancy of atom pairs since the same pair may be considered twice when taking into account the overlapping cuboids.  However, in the interests of facile book-keeping this is not checked for explicitly during the running of the algorithm.

## Bond Augmentation <a id="augmentation"></a>

Augmentation of bonds, as far as **Aten** is concerned, means to take a collection of atoms with basic connectivity (i.e. all single bonds, as per the result of rebonding) and assign multiple bonds where necessary. The method is based loosely on previously described algorithms.[2]

The basis of the method involves modifying the bond order of a particular connection to best satisfy the bonding requirements of the two involved atoms, for example making sure all carbon atoms possess an optimal total bond order of 4. However, many atoms (in particular S and P) happily exist with more than one total bond order (e.g. P) - the methodology borrowed from [3] solves this problem by scoring the total bond order for each particular element ranging from zero (meaning ‘natural’ or ‘no penalty’) to some positive number. The higher the positive number, the more ‘unhappy’ the element is with this number of bonds. For example, hydrogen atoms score 0 for a total bond order of 1, a small positive number (2) for no bonds (hydrogen ion) and a very large positive value (here, 32) for any other bond order. In this way we penalise the total bond orders that an atom does not naturally take on, and always tend towards the lowest score (i.e. the natural total bond order) wherever possible. When modifying the bond order of a particular connection, the total bond order scores of both atoms are calculated once for the current connection and again for the potential new bond order of the connection. If the new score is lower, the change of bond order is accepted.

### Pattern Detection

As with many other routines in **Aten**, a suitable pattern description is first detected for the system in order to isolate individual molecular species and make the algorithm as efficient as possible.

### Augmentation of Terminal Bonds

Bonds that involve a heavy (i.e. non-hydrogen) atom connected to no other atoms (e.g. C=O in a ketone) are treated before all others. The bond order is modified such that the total bond order score for both atoms is as low as possible.

### Augmentation of Other Bonds

Following optimisation of terminal bonds, all other bonds are modified using exactly the same procedure.

### Second Stage Augmentation

The above two steps are enough to correctly determine multiple bonds in a chemically-correct molecule, provided no cyclic moities are present in the system. The second stage is designed to correct improper augmentations within cycles, or shift existing augmentations around cycles such that other (missing) multiple bonds may be created.

For each existing multiple bond in each cyclic structure in each pattern's molecule, a simple re-augmentation of the constituent bonds is first attempted in order to try and lower the total bond order score for the whole ring (i.e. the sum of the individual bond order scores of every atom present in the cycle). Then, each bond in the ring is considered in sequence.  If the bond is a double bond, then we attempt to convert this into a single bond and make the two adjacent bonds in the ring double bonds in an attempt to ‘aromaticise’ the ring. The total bond order score is checked and, if lower than the previous score, the change is accepted. If not, the change is reversed and the next bond is considered. By performing these secondary adjustments the double-bond pattern of many complex (poly)aromatics can be correctly (and fully automatically) detected.


## References

[1] "Covalent radii revisited", B. Cordero, V. Gómez, A. E.  Platero-Prats, M. Revés, J. Echeverría, E. Cremades, F. Barragán and S. Alvarez, _Dalton Trans._ (2008) (DOI: [http://dx.doi.org/10.1039/b801115j]).
[2] "Automatic atom type and bond type perception in molecular mechanical calculations", J. Wang, W. Wang, P. A. Kollman, and D. A. Case, _J. Mol. Graphics Model._ _25_ (2), 247-260 (2006).
[3] Pedretti, A.; Villa, L.; Vistoli, G. Theor. Chem. Acc. _109_, 229-232 (2003).


