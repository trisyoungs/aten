---
title: X
taxonomy:
  category: docs
template: docs
docroot: /aten/docs
header_class: alt
---



# Forcefields Window

The Forcefields window is the place to go to load in and edit forcefields, perform atom typing on models, and calculate / minimise the energies of models. As well as being able to perform standard steepest descent and conjugate gradient minimisations, **Aten** also provides a molecular Monte Carlo minimiser, and the ability to run MOPAC directly from the GUI.

## Energy Minimisation

<doublefigure>
  <leftimage>
	  <image>img/window_ff_energy1.png</image>
  <caption>Forcefields Window – Energy minimisation controls (FF)</caption>
  </leftimage>
  <rightimage>
	  <image>img/window_ff_energy2.png</image>
	<caption>Forcefields Window – Energy minimisation controls (MOPAC)</caption>
</rightimage>
</doublefigure>

The Energy page provides the means to perform geometry optimisations on loaded models using one of several methods. The forcefield selected in the Forcefields page is used to perform the minimisation, unless one has previously been explicitly associated to the model. The current energy and forces may also be calculated (akin to single-point energy calculations). The MOPAC minimiser requires that the locations of a valid MOPAC executable and temporary directory are defined – see Section 14.2 for more information. A job file is automatically written to disk and MOPAC executed when ‘Minimise’ is clicked, and the results loaded back in. Output of the program is buffered to the Messages window (see Section 7.17).

## Forcefield Management

<figure>
  <image>img/window_ff_ff.png</image>
  <caption>Forcefields Window – Forcefields page</caption>
</figure>

Forcefield files are managed through the **Forcefields** page. A list of currently-loaded forcefields is provided in the form of a drop-down list at the top; the selected item is the current default forcefield, and is used whenever one is required by a process but none has been linked to the target model. The forcefield selected in the list is the current forcefield, and the one used by all other actions on the page. Forcefields are loaded, unloaded, and edited with the buttons immediately underneath the list. 

The Associate panel links the selected forcefield to one or more models and their patterns; the **Current Model** button links the current forcefield to the current model, while the **All Models** button links the current forcefield to all loaded models. The **Pattern in Current Model** button brings up a dialog listing the patterns of the current model, from which one is selected to link the forcefield to. A forcefield associated to an individual pattern will be used in preference to the forcefield associated with its parent model (and, if none is assigned, the default forcefield).

Automatic atom typing can also be performed (or removed) from here. Finally, you may check that a full expression is available for the current model by pressing the **Create** button in the **Expression** panel at the very bottom.  The nearby checkbox determines whether atomic charges should be assigned from atom type information, or whether the current charges (if any) should be left intact.

## Assigning Atom Types

<figure>
  <image>img/window_ff_typing.png</image>
  <caption>Forcefields Window – Manual typing page</caption>
</figure>

Manual assignment of forcefield types can be performed in the **Manual Typing** page. The list gives all atom types in the current forcefield that are relevant to the element entered just above the list. One can be selected from the list and be manually assigned (forced) onto the current atom selection with the Set button. Such assignments will not be overwritten by subsequent automatic typings. Manual typings can be removed with the **Clear** button, and the currently selected atomtype can be tested for suitability on the current selection of atoms with the **Test** button.


