---
title: X
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---


# Example - Building Ice Ih from Crystal Information (GUI)

Similar in spirit to the [a]alumina example,examples-alumina[/a], here we create a single ice I<sub>h</sub> crystal from the crystal information, and then replicate it to form a larger supercell. The crystal information used below is from Leadbetter _et al._, _J. Chem. Phys._, **82**, 424 (1985), Table II (Structural parameters of ice at 5 K).

## Create the Basic Unit Cell

First, we create the unit cell, which from the paper is orthorhombic with side lengths _a_ = 4.5019, _b_ = 7.7978, and _c_ = 7.328 Å. There are five symmetry-unique atoms to add into the cell. This might seem odd given that this doesn't add up to a whole number of water molecules, but one of the water molecules lays with its oxygen on a mirror plane, and so only needs one hydrogen to be specified. Atom positions in the paper are given in fractional coordinates - we will create the atoms using these coordinates which will be converted by **Aten** into their real (cell) coordinates automatically.

<table>
  <row>
    <column>[img]aten/manual/img/toolbox_celldefine.png[/img]</column>
    <column>Open the **Cell Define Window**</column>
  </row>
  <row>
    <column></column>
    <column>Add a unit cell to the model by checking the **Has Cell?** Checkbox, then go to the **Define/View ABC** page and set the cell lengths _a_, _b_, and _c_ to 4.5019, 7.7978, and 7.328 respectively. Leave the cell angles all at 90°, and click **Define**.</column>
  </row>
  <row>
    <column>[img]aten/manual/img/toolbox_build.png[/img]</column>
    <column>In the **Build Window** change the active element on the **Edit** page to oxygen by clicking the red **O** button.</column>
  </row>
  <row>
    <column> </column>
    <column>
      On the **Add Atom** panel in the **Tools** page make sure **Fractional Coordinates** is checked and then enter the following sets of coordinates, clicking **Add** after each set is entered:
      x = 0.0, y = 0.6648, z = 0.0631
      x = 0.5, y = 0.8255, z = -0.0631
    </column>
  </row>
  <row>
    <column></column>
    <column>
      Go back to the the **Edit** page and change the active element to hydrogen by clicking the white **H**, return to the **Tools** page and add three more atoms at the following fractional coordinates:
      x = 0.0, y = 0.6636, z = 0.1963
      x = 0.0, y = 0.5363, z = 0.0183
      x = 0.6766, y = -0.2252, z = -0.0183
    </column>
  </row>
</table>

<figure>
  <image>img/examples-ice-1.jpg</image>
</figure>

## Set the Spacegroup and Pack

To complete the model the spacegroup of the crystal must be set so that generation of the symmetry-related atoms can be performed.

<table>
  <row>
    <column>[img]aten/manual/img/toolbox_celldefine.png[/img]</column>
    <column>Open the **Cell Define Window **again, and on the **Spacegroup** page enter the spacegroup as “Cmc21” or “36” and press **Set** to assign the spacegroup to the model. Then, generate symmetry equivalent atoms by pressing the **Pack** button</column>
  </row>
</table>

## Replicate Cell and Calculate Bonding

The basic cell of ice Ih isn’t particularly interesting by itself, so we will replicate the cell to create a larger supercell, and then calculate bonds in the new model so that the hexagonal structure is clear to see.

<table>
  <row>
    <column>[img]aten/manual/img/toolbox_celltransform.png[/img]</column>
    <column>On the **Cell Transform Window** go to the **Scale** page and enter a scale factor of 5.628 for each of _x_, _y_, and _z_.  Press the **Scale** button to resize the unit cell.</column>
  </row>
  <row>
    <column></column>
    <column>Now, select the **Replicate** page and enter positive replication values of 5.0 for both _x_ and _z_, and 2.0 for _y_, and press **Replicate** to create the supercell.</column>
  </row>
  <row>
    <column>[img]aten/manual/img/toolbox_build.png[/img]</column>
    <column>Back in the **Build Window**, go to the **Edit** page and **Rebond** the model.</column>
  </row>
</table>

## Script

```
newModel("ice");
cell(4.5019,7.7978,7.3280,90,90,90);
newAtomFrac(O,0,0.6648,0.0631);
newAtomFrac(O,0.5,0.8255,-0.0631);
newAtomFrac(H,0,0.6636,0.1963);
newAtomFrac(H,0,0.5363,0.0183);
newAtomFrac(H,0.6766,-0.2252,-0.0183);
spacegroup("Cmc21");
pack();
replicate(0,0,0,4,4,4);
rebond();
```


