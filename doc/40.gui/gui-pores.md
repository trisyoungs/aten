---
title: X
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---


# Pore Builder Window

The **Pore Builder Window** offers some tools to make the construction of (regular) porous material systems a little easier, including when it comes to filling pores.

<figure>
  <image>img/window_pores_drill.png</image>
  <caption>Pore Buider Window – Drill page</caption>
</figure>

The **Drill** page provides tools to select and cut atoms from an array or pores of specified geometry in the current model. The **Origin Plane** determines the crystal face from which the pores originate, but from there the direction of drilling (the **Pore Vector**) may be set to anything although something coincident with the normal of the origin plane is probably sensible. The magnitude of this vector is unimportant, since it is normalised before use.  Once all quantities are suitably defined, the atoms making up the pores may be simply selected (with **Select Atoms**) or deleted from the model (with **Cut Pore(s)**). The reason for providing both these actions is to allow for selection of atoms at the surfaces resulting from pore cutting, simply by increasing the **Pore Size** parameter a little.

<figure>
  <image>img/window_pores_terminate.png</image>
  <caption>Pore Builder Window – Terminate page</caption>
</figure>

At present, the **Terminate** page provides a single button which terminates (adds H or OH to) any atoms in the current selection which have fewer than their normal quota of bonds as a result of pore drilling (or by other means). H or OH are added to atoms until the correct number of bonds per atom is restored. Only O and Si are considered at present.

<figure>
  <image>img/window_pores_scheme.png</image>
  <caption>Pore Builder Window – Scheme page</caption>
</figure>

The **Scheme** page permits a partitioning scheme (see Section 10.4) to be created from free space in the current model (not necessarily space created by pore drilling). The **Grid Size** determines how finely to divide up space in the current model – the defaults should be fine for most purposes, but exotic or very small shapes may require higher values.  The minimum allowable size for a partition is given as a percentage of the total number of cells in the grid (**Minimum Partition Size**), below which any discovered regions of space are ignored. In order to correctly generate partitions it is often necessary to tweak the **Atom Extent** parameter.  Any grid cell in which an atom exists is considered ‘full’ and not part of any partition, but in most cases this is not sufficient and more space must be excluded to account for the real space-filling of the atoms. The **Atom Extent** value defines a radius, in cells, within which additional cells around atoms will also be removed.

The **Generate** button will search for partitions in the model and overlay these partitions onto the main display, but will not copy the data to the Disorder builder.  Once you are satisfied that the correct / relevant partitions have been discovered, the data can be made available in Disorder building by clicking **Copy to Builder**.


