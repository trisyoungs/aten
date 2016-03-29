---
title: X
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---


# Examples - Creating an NaCl/Water Two-Phase System (GUI)

It’s often necessary to create a larger system from a simple (or complex) unit cell, for example to generate bulk supercells of crystals, sufaces etc.  The method outlined below shows how to do this for a simple crystal, and then extends this system to create a solid liquid interface.

## Create the Template Model

First off, we will create a basic FCC template model which has a unit cell of exactly 1 Å, with atoms at {0,0,0}, {0.5,0.5,0}, {0.0,0.5,0.5}, and {0.5,0.0,0.5}, representing the basic positions of atoms in a face-centred cubic lattice.

<table>
 <row>
  <column>[img]aten/manual/img/toolbox_celldefine.png[/img]</column>
  <column>Open the **Cell Define Window**</column>
 </row>
 <row>
  <column></column>
  <column>Add a unit cell to the model by checking the **Has Cell?** checkbox, then go to the **Define/View ABC** page and set the cell lengths _a_, _b_, and _c_ to 1.0, then click **Define**.</column>
 </row>
 <row>
  <column>[img]aten/manual/img/toolbox_build.png[/img]</column>
  <column>Add the following four atoms to the model at the coordinates specified. Change the current element to Na on the **Edit** page in the **Build Window**, and use the **Add Atom** panel in the **Tools** page to create the atoms.</column>
  </row>
 <row>
  <column></column>
  <column>x = 0.0, y = 0.0, z = 0.0</column>
  </row>
 <row>
  <column></column>
  <column>x = 0.5, y = 0.5, z = 0.0</column>
  </row>
 <row>
  <column></column>
  <column>x = 0.0, y = 0.5, z = 0.5</column>
  </row>
 <row>
  <column></column>
  <column>x = 0.5, y = 0.0, z = 0.5</column>
  </row>
</table>

<figure>
  <image>img/examples-nacl-1.jpg</image>
</figure>

## Create Interpenetrating Secondary Lattice

We need a second FCC lattice on which the chlorine atoms will sit. We could add them all by hand, but there are easier ways.

<table>
 <row>
  <column>_Ctrl-A_</column>
  <column>Select all the atoms in the model with the **Ctrl-A** shortcut, or go to the **Edit** menu and choose **Select All**.</column>
 </row>
 <row>
  <column>_Ctrl-C_</column>
  <column>Copy the current atom selection with **Ctrl-C**, or **Edit→Copy.**</column>
 </row>
 <row>
  <column>_Ctrl-V_</column>
  <column>Paste the copied atoms with **Ctrl-V** or **Edit→Paste.**</column>
 </row>
 <row>
  <column>[img]aten/manual/img/toolbox_position.png[/img]</column>
  <column>Open the **Position Window** and select the **Flip** page. Here we can mirror the coordinates of the current atom selection about its centre of geometry. So, mirror it once in any one of the x, y, or z directions. Do not deselect the atoms afterwards, since we still need to transmute them into chlorines.</column>
 </row>
 <row>
  <column>[img]aten/manual/img/toolbox_build.png[/img]</column>
  <column>On the Edit page in the Build Window, set the current element to chlorine, and then click **Transmute Selection**.
 </row>
</table>

<figure>
  <image>img/examples-nacl-2.jpg</image>
</figure>

## Scale and Replicate Unit Cell

We wish to scale the cubic unit cell of the model (currently with side length _l_ = 1.0 Å) to correspond to the unit cell of sodium chloride (_l_ = 5.628 Å). The cell can be scaled by a different amount in each Cartesian axis, but since we want to end up with a cubic cell we must scale each axis by the same amount.

Once the model represents proper the basic sodium chloride unit cell we can replicate it to create a larger system. **Aten** can replicate a basic cell by any integer or non-integer amount along each of the three principal cell axes, but here we will stick to integer amounts. The Replicate method also allows specification of both negative and positive replication amounts for each direction. Note that the values given in the **Replicate** page represent the _total_ size which we require, so input values (negative/positive) of {0,0,0} and {1,1,1} will result in an unchaged cell.

<table>
 <row>
  <column>[img]aten/manual/img/toolbox_celltransform.png[/img]</column>
  <column>On the **Cell Transform Window** go to the **Scale** page and enter a scale factor of 5.628 for each of _x_, _y_, and _z_.  Press the **Scale** button to resize the unit cell.</column>
  </row>
  <row>
    <column></column>
    <column>Now, select the **Replicate** page and enter positive replication values of 5.0 for both _x_ and _z_, and 2.0 for _y_, and press **Replicate** to create the supercell.</column>
 </row>
</table>

<figure>
  <image>img/examples-nacl-3.jpg</image>
</figure>

## Create an Interface

It's a simple job to create an interface from the current system - all we need do is increase the cell dimension along the _y_ direction (the cell’s _b_ length).

<table>
 <row>
  <column>[img]aten/manual/img/toolbox_celldefine.png[/img]</column>
  <column>Open the **Cell Define Window**, and either change the central number of the **Matrix** page or the _b_ value on the **Define/View ABC** page. If you replicated the _y_ direction by 2.0 earlier, then the current value of _b_ should be 11.238 Å. Change it to 22.5 to roughly double the volume of the cell and create an interface in the _xz_ plane.</column>
 </row>
</table>

## Make a Water Model

We will now create a water molecule in a separate model so we can add it in to the NaCl cell using the Disorder Builder.

<table>
 <row>
  <column>_Main Toolbar_</column>
  <column>Create a new, empty model (if you don’t have one already) with **New**.</column>
 </row>
 <row>
  <column>[img]aten/manual/img/toolbox_build.png[/img]</column>
  <column>In the **Build Window** change the active element to oxygen on the **Edit** page.</column>
 </row>
 <row>
  <column></column>
  <column>Select the draw single atoms tool **<img border=0 width=14 height=14 id="Picture 169" src="manual_files/image006.gif" alt="build_atom"> Atom** and click once somewhere in the empty model to create an oxygen</column>
 </row>
 <row>
  <column></column>
  <column>Add hydrogens to the model</column>
licking"manual_files/image034.gif" alt="build_addh">**AddH Model**</column>
 </row>
</table>

width=431 height=352 id="Picture 171" src="manual_files/image035.jpg">

## Add Water to the NaCl Cell

It’s time to run the disorder builder on the system. We’ll instruct the builder to add water in to the extended NaCl cell, but only into the part which is empty.

<table class=MsoNormalTable border=0 cellspacing=0 cellpadding=0 width="95%"
 style='width:95.52%;background:#DBE5F1;border-collapse:collapse'>
 <row>
  <td width="18%" valign=top style='width:18.9%;padding:0cm 5.4pt 0cm 5.4pt'>
  <img border=0 width=83 height=20 id="Picture 172"
  src="manual_files/image036.gif" alt="window_disorder">
  </column>
  <td width="81%" valign=top style='width:81.1%;padding:0cm 5.4pt 0cm 5.4pt'>
  Run the **Disorder Builder** wizard
  </column>
 </row>
</table>

<p class=MsoNormal align=center style='text-align:center'><img border=0
width=268 height=249 id="Picture 4" src="manual_files/image037.jpg">     <img
border=0 width=268 height=249 id="Picture 184" src="manual_files/image038.jpg">

We wish to add in water to an existing system in this
example, so choose the top option **(Use Existing Model**) and press **Next**.
You then need to choose the target model for the builder, which is the extended
NaCl system we just created.  Select it and press **Next**.

&nbsp;

&nbsp;

&nbsp;

<p class=MsoNormal align=center style='text-align:center'><img border=0
width=268 height=249 id="Picture 185" src="manual_files/image039.jpg"><img
border=0 width=170 height=80 id="Picture 186" src="manual_files/image040.jpg">

Now we choose the partitioning scheme for the system. We could be lazy and just choose “None”, since the NaCl lattice should ‘reject’ any water molecule we attempt to add over it. However, here we will choose an appropriate partitioning scheme for the task, “SlabXZ”. This will allow us to restrict water molecules to a specific _y_-range in the unit cell. Select “SlabXZ” and press the **Scheme Options** button to bring up the options dialog for the scheme. There you will see the start and end values of _y_ for the slab (in fractional cell coordinates). The initial minimum limit of 0.4 is, luckily, appropriate for the system, but the upper bound needs to be set to 1.0. Press **OK** when done and then **Next**.

<p class=MsoNormal align=center style='text-align:center'><img border=0
width=268 height=249 id="Picture 187" src="manual_files/image041.jpg">     <img
border=0 width=269 height=250 id="Picture 188" src="manual_files/image042.jpg">

Finally, model selection and preparation. Select the water molecule from the list and press **Next** to get to the component setup page.  We must change the **Target Partition **of the water molecule to 2 (the slab we defined earlier), and then request that a **Specific Density** of molecules be inserted into this partition (the default of 1.0 g/cm3 is fine). Once this information has been entered, press **Finish** to start the build.

width=431 height=352 id="Picture 173" src="manual_files/image043.jpg">

## Script

The script below creates the water molecule as the first step to make life a little easier.

```
newModel("Water");
newAtom(O);
addHydrogen();
setupComponent("density", 2, 0, 1.0);

newModel("fcc");
cell(1,1,1,90,90,90);
newAtom(Na,0,0,0);
newAtom(Na,0.5,0.5,0);
newAtom(Na,0.5,0,0.5);
newAtom(Na,0,0.5,0.5);
selectAll();
copy();
paste();
flipX();
transmute(Cl);
scale(5.628,5.628,5.628);
replicate(0,0,0,5,2,5);
setCell("b",28.12);
disorder("SlabXZ,end=1.0");
```


