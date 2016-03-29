---
title: Supplied FFs
brief: List of forcefields supplied with <strong>Aten</strong>
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

A handful of forcefields ready-formatted for import into **Aten** are provided with the code and are listed here. It should be a relatively straightforward process to convert others, unless the functional forms used are not yet implemented (but sure, if you ask then I will add them). If you export an expression from **Aten**, **please** check the parameters in the file are what you actually want. **Aten** is designed to ease the pain of setting up a simulation in this manner, but is **not** intended as a black box.

## Canongia-Lopes & Padua Ionic Liquids (cldp-il.ff)

All-atom ionic liquids forcefield of Canongia Lopes _et al._ covering various cation/anion combinations.

_References:_

+ J. N. Canongia Lopes, A. A. H. Padua, _J. Phys. Chem. B_, 110 (39), 19586-19592 (2006)
+ J. N. Canongia Lopes, A. A. H. Padua, _J. Phys. Chem. B_, 108 (43), 16893-16898 (2004)
+ J. N. Canongia Lopes, J. Deschamps, A. A. H. Padua, _J. Phys. Chem. B_, 108 (30), 11250 (2004)
+ J. N. Canongia Lopes, J. Deschamps, A. A. H. Padua, _J. Phys. Chem. B_, 108 (6), 2038-2047 (2004)

## Youngs, Kohanoff, &amp; Del Pópolo [dmim]Cl (dmimcl-fm.ff)

Force-matched model for the ionic liquid dimethylimidazolium chloride only. Integer charges on ions.

_References:_

+ T. G. A. Youngs, J. Kohanoff, and M. G. Del Pópolo, _J. Phys. Chem. B_, 110 (11), 5697-5707 (2006)

## Youngs &amp; Hardacre [dmim]Cl (dmimcl-fm2.ff)

Second force-matched model for the ionic liquid dimethylimidazolium chloride only. Non-integer charges on ions.

_References:_

+ T. G. A. Youngs and C. Hardacre, _ChemPhysChem_, 9 (11), 1548-1558 (2008)

## Jorgensen at al. OPLS-AA (oplsaa.ff)

Original OPLS-AA forcefield of Jorgensen et al. Thanks to W. Jorgensen for supplying the parameter data.

_References:_

+ W. L. Jorgensen, D. S. Maxwell, and J. Tirado-Rives, _J. Am. Chem. Soc._ 118, 11225-11236 (1996).
+ W. L. Jorgensen and N. A. McDonald, _Theochem_ 424, 145-155 (1998).
+ W. L. Jorgensen and N. A. McDonald, _J. Phys. Chem. B_ 102, 8049-8059 (1998).
+ R. C. Rizzo and W. L. Jorgensen, _J. Am. Chem. Soc._ 121, 4827-4836 (1999).
+ M. L. Price, D. Ostrovsky, and W. L. Jorgensen, _J. Comp. Chem._ 22 (13), 1340-1352 (2001).
+ E. K. Watkins and W. L. Jorgensen, _J. Phys. Chem. A_ 105, 4118-4125 (2001).

**Note: NETA definitions have been written for a large number of types in the forcefield, but not all.**

## Berendsen _et al._ Simple Point Charge Water (spc.ff)

Rigid, simple point charge model for water

_References:_

+ H. J. C. Berendsen, J. P. M. Postma, W. F. van Gunsteren and J. Hermans, in Intermolecular Forces, B. Pullman (ed.), Reidel, Dordrecht, 1981, p331.

## Berendsen _et al._ Extended Simple Point Charge Water (spce.ff)

Simple point charge model for water, modified to reproduce molecular dipole in the liquid phase.

_References:_

+ J. C. Berendsen, J. R. Grigera and T. P. Straatsma, _J. Phys. Chem._ 91, 6269-6271 (1987)

## Rappe _et al._ Universal Forcefield (uff.ff)

Universal forcefield for the whole periodic table by Rappe et al.

_References:_

+ A. K. Rappe, C. J. Casewit, K. S. Colwell, W. A. Goddard III, and W. M. Skiff, _J. Am. Chem. Soc._ 114, 10024-10039 (1992)

_Notes:_

+ Generated terms should be checked by hand if forcefield expressions are exported.
+ Detection of some atomtypes, namely transition metals, is imperfect.
+ Warning: Generation of terms (especially angles) needs to undergo proper testing! If you wish to help, please contact me.

## Mayo, Olafson &amp; Goddard II's Generic Forcefield (testing/dreiding.ff)

Universal forcefield for the whole periodic table.

_References:_

+ S.L. Mayo, B.D. Olafson, and W.A. Goddard III, _J. Phys. Chem._ 94, 8897-8909 (1990).

_Notes:_

+ dreiding.ff currently lives in the testing/ directory since it is a rule-based forcefield and is currently being rewritten.
+ Generated terms should be checked by hand if forcefield expressions are exported.
+ Detection of some atomtypes, namely transition metals, is imperfect.
+ Warning: Generation of terms (especially angles) needs to undergo proper testing! If you wish to help, please contact me.

## General Amber Forcefield (testing/gaff.ff)

General Amber forcefield containing precalculated data for most intramolecular terms, and a generator section for any that are missing.

_References:_

+ J. Wang, R. M. Wolf, J. W. Caldwell, P. A. Kollman, and D. A. Case, _J. Comp. Chem._ 25, 1157-1174 (2004)

_Notes:_

+ gaff.ff currently lives in the testing/ directory since it's rule-based part has not been implemented yet.

## Liu, Wu &amp; Wang's United-Atom Ionic Liquids Forcefield (lww-il.ff)

United atom ionic liquids forcefield for a handful of cations and anions.

_References:_

+ X. Zhang, F. Huo, Z. Liu, W. Wang, W. Shi, and E. J. Maginn, _J. Phys. Chem. B_ 113, 7591-7598 (2009)
+ Z. Liu, X. Wu, and W. Wang, _Phys. Chem. Chem. Phys._ 8, 1096-1104 (2006)


