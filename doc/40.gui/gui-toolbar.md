---
title: X
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---


# The Main Toolbar


  [img]projects/aten/manual/img/button_dotspacer.png[/img]
  [img]projects/aten/manual/img/button_file_new.png[/img]
  [img]projects/aten/manual/img/button_file_open.png[/img]
  [img]projects/aten/manual/img/button_file_save.png[/img]
  [img]projects/aten/manual/img/button_file_saveas.png[/img]
  [img]projects/aten/manual/img/button_file_close.png[/img]
  [img]projects/aten/manual/img/button_spacer.png[/img]
  [img]projects/aten/manual/img/button_edit_cut.png[/img]
  [img]projects/aten/manual/img/button_edit_copy.png[/img]
  [img]projects/aten/manual/img/button_edit_paste.png[/img]
  [img]projects/aten/manual/img/button_edit_delete.png[/img]
  [img]projects/aten/manual/img/button_spacer.png[/img]
  [img]projects/aten/manual/img/button_style_stick.png[/img]
  [img]projects/aten/manual/img/button_style_tube.png[/img]
  [img]projects/aten/manual/img/button_style_sphere.png[/img]
  [img]projects/aten/manual/img/button_style_scaled.png[/img]
  [img]projects/aten/manual/img/button_style_individual.png[/img]
  [img]projects/aten/manual/img/button_spacer.png[/img]
  [img]projects/aten/manual/img/button_select_box.png[/img]
  [img]projects/aten/manual/img/button_select_molecule.png[/img]
  [img]projects/aten/manual/img/button_select_element.png[/img]
  [img]projects/aten/manual/img/button_select_expand.png[/img]
  [img]projects/aten/manual/img/button_select_invert.png[/img]


The main toolbar provides quick access to model load / save, edit, and selection operations. Left to right these icons are:

<table>
 <header>
  <column>Icon</column>
  <column>Action</column>
  <column>Shortcut</column>
  <column>Description</column>
 </header>
 <row>
  <column>[img]projects/aten/manual/img/button_file_new.png[/img]</column>
  <column>New</column>
  <column>Ctrl-N</column>
  <column>Create a new, empty model</column>
 </row>
 <row>
  <column>[img]projects/aten/manual/img/button_file_open.png[/img]</column>
  <column>Open</column>
  <column>Ctrl-O</column>
  <column>Load an existing model into **Aten**</column>
 </row>
 <row>
  <column>[img]projects/aten/manual/img/button_file_save.png[/img]</column>
  <column>Save</column>
  <column>Ctrl-S</column>
  <column>Save the current model under its original filename</column>
 </row>
 <row>
  <column>[img]projects/aten/manual/img/button_file_saveas.png[/img]</column>
  <column>Save As</column>
  <column></column>
  <column>Save the current model under a different filename</column>
 </row>
 <row>
  <column>[img]projects/aten/manual/img/button_file_close.png[/img]</column>
  <column>Close</column>
  <column></column>
  <column>Close the current model (prompting to save first if changes have been made)</column>
 </row>
 <row>
  <column></column>
  <column></column>
  <column></column>
  <column></column>
 </row>
 <row>
  <column>[img]projects/aten/manual/img/button_edit_copy.png[/img]</column>
  <column>Copy</column>
  <column>Ctrl-C</column>
  <column>Copy the current atom selection to **Aten**’s internal clipboard</column>
 </row>
 <row>
  <column>[img]projects/aten/manual/img/button_edit_cut.png[/img]</column>
  <column>Cut</column>
  <column>Ctrl-X</column>
  <column>Copy the current atom selection to the clipboard and then delete it</column>
 </row>
 <row>
  <column>[img]projects/aten/manual/img/button_edit_paste.png[/img]</column>
  <column>Paste</column>
  <column>Ctrl-V</column>
  <column>Paste the contents of the clipboard to the current model at the original coordinates. The pasted atoms then become the current selection. Note that pasted atoms are not translated to avoid overlap with existing atoms.</column>
 </row>
 <row>
  <column>[img]projects/aten/manual/img/button_edit_delete.png[/img]</column>
  <column>Delete</column>
  <column>Ctrl-Delete</column>
  <column>Delete the current atom selection</column>
 </row>
 <row>
  <column></column>
  <column></column>
  <column></column>
  <column></column>
 </row>
 <row>
  <column>[img]projects/aten/manual/img/button_style_stick.png[/img]</column>
  <column>Stick</column>
  <column>Ctrl-1</column>
  <column>Atoms are not explicitly drawn unless they possess no bonds, bonds are drawn using simple lines</column>
 </row>
 <row>
  <column>[img]projects/aten/manual/img/button_style_tube.png[/img]</column>
  <column>Tube</column>
  <column>Ctrl-2</column>
  <column>Atoms and bonds are drawn as tubes</column>
 </row>
 <row>
  <column>[img]projects/aten/manual/img/button_style_sphere.png[/img]</column>
  <column>Sphere</column>
  <column>Ctrl-3</column>
  <column>Atoms are drawn as uniformly-sized spheres, with bonds drawn as tubes</column>
 </row>
 <row>
  <column>[img]projects/aten/manual/img/button_style_scaled.png[/img]</column>
  <column>Scaled</column>
  <column>Ctrl-4</column>
  <column>Atoms are drawn as spheres whose size depends on their atomic radii, bonds are drawn as tubes</column>
 </row>
 <row>
  <column>[img]projects/aten/manual/img/button_style_individual.png[/img]</column>
  <column>Individual</column>
  <column>Ctrl-5</column>
  <column>Each atom is drawn according to its own assigned style</column>
 </row>
 <row>
  <column></column>
  <column></column>
  <column></column>
  <column></column>
 </row>
 <row>
  <column>[img]projects/aten/manual/img/button_select_box.png[/img]</column>
  <column>Box Select</column>
  <column>Escape</column>
  <column>Atoms may be (de)selected by clicking on them individually, or selected en masse with a click-hold-drag</column>
 </row>
 <row>
  <column>[img]projects/aten/manual/img/button_select_molecule.png[/img]</column>
  <column>Molecule Select</column>
  <column></column>
  <column>Bound fragments are selected by clicking on a single atom within that fragment</column>
 </row>
 <row>
  <column>[img]projects/aten/manual/img/button_select_element.png[/img]</column>
  <column>Element Select</column>
  <column></column>
  <column>Clicking on a single atom selects all atoms of the same element</column>
 </row>
 <row>
  <column>[img]projects/aten/manual/img/button_select_expand.png[/img]</column>
  <column>Expand Selection</column>
  <column></column>
  <column>The current selection is expanded by following bonds attached to any select atoms</column>
 </row>
 <row>
  <column>[img]projects/aten/manual/img/button_select_invert.png[/img]</column>
  <column>Invert Selection</column>
  <column>Ctrl-I</column>
  <column>Toggle the selection state of all atoms</column>
 </row>
</table>

## The Mouse Toolbar

There is only one other toolbar in (newer versions of) **Aten** – the Mouse toolbar. For multi-button mice each button can be assigned an invidual action (select, rotate model etc.). For those who use single-button rats, this toolbar changes the function of the first (or left) button between select / interact, rotate model, and translate model. Selecting these buttons overwrite the stored action for the left button in the Preferences.


  [img]projects/aten/manual/img/button_dotspacer.png[/img]
  [img]projects/aten/manual/img/button_mouse_interact.png[/img]
  [img]projects/aten/manual/img/button_mouse_rotate.png[/img]
  [img]projects/aten/manual/img/button_mouse_translate.png[/img]


<table>
  <title>Mouse Toolbar Icons</title>
 <header>
  <column>Icon</column>
  <column>Shortcut</column>
  <column>Action</column>
 </header>
 <row>
  <column>[img]projects/aten/manual/img/button_mouse_interact.png[/img] </column>
  <column>F1</column>
  <column>The left button selects and interacts with atoms</column>
 </row>
 <row>
  <column>[img]projects/aten/manual/img/button_mouse_rotate.png[/img] </column>
  <column>F2</column>
  <column>The left button rotates the view or selection</column>
 </row>
 <row>
  <column>[img]projects/aten/manual/img/button_mouse_translate.png[/img] </column>
  <column>F3</column>
  <column>The left button translates the view or selection</column>
 </row>
</table>


