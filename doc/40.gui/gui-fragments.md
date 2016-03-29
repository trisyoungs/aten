---
title: X
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---


# Fragments Window

<figure>
  <image>img/window_fragments.png</image>
  <caption>Fragments Window</caption>
</figure>

When in fragment drawing mode, the **Fragments** window presents a list of all the available fragment models (in either **List** or **Grid** formats), and whichever is selected represents the current fragment to add in to the model. The current fragment is ‘attached’ to the mouse pointer when moving over the main canvas, and shows a preview of what will be the orientation and position of the fragment when a left-click is made.

If a fragment is drawn (attached) to an existing atom in a model, the resulting bond length is adjusted to match the two elements involved if the **Adjust Bond Length on Paste** checkbox is ticked.

## Fragment Models

Fragment models are just normal models stored in specific places. The only real difference is that atoms with unknown element (integer ‘0’, or string ‘XX’) are slightly more useful than usual. These can act as anchor points for the fragment that do not correspond to the position of any ‘proper’ atom to allow for more control over the placement of structures - for instance, such an atom may be placed at the centre of a ring. It is important to note that all atoms with unknown element type are removed when the fragment is added to the current model.

## Anchor Points

At any time, a single atom in the selected fragment represents the attachment point and appears directly under the mouse pointer. Any atom in the fragment can be selected as the current anchor point, and may be cycled through by pressing <strong>Alt</strong>. If the atom acting as the anchor point has at least one bond to another atom then a reference vector is constructed allowing the fragment to be oriented along vectors dependent on the current geometry of existing atoms, for example.

## Placing Fragments in Free Space

Single-clicking in free space (i.e. not over an existing atom) places a copy of the current fragment in the current orientation in the current model. Click-dragging allows the fragment to be freely rotated about the anchor point before the placement is final. On placement, the anchor atom is pasted too <em>unless</em> it is of unknown element (see above).

## Attaching to Existing Atoms

When moving over an existing atom with an anchor point that has at least one bond to another atom, the fragment is attached at a suitable geometry, provided the valency of the atom allows (if not, the atom will be surrounded by a crossed-out box and the fragment will temporarily disappear). A single-click will place the fragment in the shown position and orientation, while click-dragging rotates the fragment about the newly-formed bond to allow fine adjustment before the final placement. If the anchor point has no bonds to other atoms, the fragment is placed in no specific orientation, and click-dragging freely rotates the molecule about the anchor point. In both cases, the anchor atom is <em>always</em> removed when being added to the model since the existing atom which was clicked on will replace it in terms of position.

If the existing atom in the model has one or more other atoms attached to it, holding <strong>Shift</strong> will orient the fragment along an existing bond vector. In this case, both the anchor atom and the atom at the other end of the bond are removed when the fragment is placed. Pressing <strong>Ctrl</strong>
cycles over the bonds of the existing atom.


