---
title: X
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---


# Cell Transform Window

Transformations of the unit cell and its contents can be made here, encompassing geometric scaling of the cell (and atoms contained within) and replication of the system in three dimensions.

<figure>
  <image>img/window_celltransform_replicate.png</image>
  <caption>Cell Transform Window – Replicate page</caption>
</figure>

The **Replicate **page allows the current cell to be replicated along its three principal axes in both positive and negative directions. The six inputs represent negative and positive replication values for each direction – most of the time its probably only useful to consider the positive (right-most) replication directions. Note that the numbers define the _additional_ cells that will be created in addition to the original one. So, if all numbers are left at zero the original cell will remain untouched. Entering a value of 1 for each positive direction will give a 2×2×2 supercell of the original cell, and so on. The representative unit cells of replicated and partially replicated copies of the current cell are drawn onto the current model.

Atoms in the model are folded into the unit cell prior to replication, unless the **Fold Before** checkbox is unticked. Similarly, atoms that exist outside of the cell after replication are trimmed unless the **Trim After** checkbox is unchecked.

<figure>
  <image>img/window_celltransform_scale.png</image>
  <caption>Cell Transform Window – Scale page</caption>
</figure>

The **Scale** page allows the principal axes of the current unit cell to be arbitrarily scaled, along with the cell's contents. If a valid pattern description exists for the model, then the positions of individual molecules or bound fragments within the cell are scaled relative to their centres of geometry – all intramolecular distances within molecules remains the same as before the scaling. If this is undesirable (or unintended) then performing the scaling with no pattern definition will scale the position of each atom separately.

<figure>
  <image>img/window_celltransform_rotate.png</image>
  <caption>Cell Transform Window – Rotate page</caption>
</figure>

Not currently implemented.

<figure>
  <image>img/window_celltransform_miller.png</image>
  <caption>Cell Transform Window – Miller page</caption>
</figure>

The **Miller** page allows the model to be cut so that various Miller surfaces are left behind. The _hkl_ indices of the desired Miller plane should be entered in the three spin boxes, and the resulting plane(s) will be drawn onto the current model. The deletion of atoms can be done in one of two ways, removing either those atoms that are ‘inside’ or those atoms that are ‘outside’ of the defined Miller plane and its periodic or symmetric equivalent.


