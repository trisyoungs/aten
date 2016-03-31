---
title: Fragments
brief: Adding user fragments for use in the builder
visible: true
template: manpage
taxonomy:
  category: docs
docroot: /aten/docs
header_class: alt
---

Fragments are useful things when building complex molecules, or when it is just desirable to quickly create a simple molecule for use in the [Disorder Builder](/aten/docs/XXX), for instance.

**Aten** comes with a modest array of fragments in it's global `data` directory, but it is easy to add more. A fragment model is just like any other model, except that it deliberately contains one or more atoms of 'unknown' element (XX). It is these atoms that are recognised as potential attachment points in the molecule, and subsequently in the [Fragment tool](/aten/docs/gui/build#draw).

The recommended way of creating a user fragment is as follows:

# Draw or import the desired fragment
# Convert one or more atoms to unknown element XX (by using the [**Set to Unknown**] option on the [**Selection** panel](/aten/docs/gui/selection) or on the atom context menu.
# Save the model in Aten Keyword Format (akf) since this format retains all bond information exactly
# Place the model file in your user data directory in the `fragments` folder
# Reload **Aten** and your fragment(s) will be available in the [Fragment tool](/aten/docs/gui/build#draw).
