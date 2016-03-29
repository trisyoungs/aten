---
title: X
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---


# 
</span>Creating a Bulk Water Model (GUI)</a></h2>

Bulk (periodic) systems are the staple diet of the molecular dynamicist,
particularly systems that are isotropic. This example illustrates how to
generate a simple bulk configuration of liquid water.

<h4>Make a Water Molecule</h4>

We need a water molecule. We <em>could</em> load one in, but its marginally
more interesting to build one from scratch. So, we’ll place an oxygen atom down
somewhere and automatically add hydrogens to it.

<table class=MsoNormalTable border=0 cellspacing=0 cellpadding=0 width="95%"
 style='width:95.52%;background:#DBE5F1;border-collapse:collapse'>
 <tr>
  <td width="18%" valign=top style='width:18.9%;padding:0cm 5.4pt 0cm 5.4pt'>
  <em>Main Toolbar</em>
  </td>
  <td width="81%" valign=top style='width:81.1%;padding:0cm 5.4pt 0cm 5.4pt'>
  Create a new, empty model (if you don’t have one already)
  with <img border=0 width=14 height=14 id="Picture 181"
  src="manual_files/image003.gif" alt="file_new">
  </td>
 </tr>
 <tr>
  <td width="18%" valign=top style='width:18.9%;padding:0cm 5.4pt 0cm 5.4pt'>
  <img border=0 width=82 height=19 id="Picture 224"
  src="manual_files/image004.gif" alt="window_build">
  </td>
  <td width="81%" valign=top style='width:81.1%;padding:0cm 5.4pt 0cm 5.4pt'>
  In the <b>Build Window</b> change the active element to
  oxygen on the Edit page by clicking <img border=0 width=14 height=14
  id="Picture 178" src="manual_files/image005.gif" alt="build_o">
  Select the draw single atoms tool <img border=0 width=14
  height=14 id="Picture 179" src="manual_files/image006.gif" alt="build_atom"> and
  click once somewhere in the empty model to draw an oxygen atom
  Add hydrogens to the model by clicking <img border=0
  width=14 height=14 id="Picture 180" src="manual_files/image007.gif"
  alt="build_addh"><b>AddH Model</b>
  </td>
 </tr>
</table>

<p class=MsoNormal align=center style='text-align:center'><img border=0
width=431 height=352 id="Picture 182" src="manual_files/image008.jpg">

<h4>Run the Disorder Builder</h4>

The Disordered Builder adds molecules into a new or existing model with a
unit cell. The only requirement of the Disorder Builder is that all the models
you wish to add must be loaded into **Aten** before the builder is run.  Everything
else is handled by a wizard which guides you through the process. 

<table class=MsoNormalTable border=0 cellspacing=0 cellpadding=0 width="95%"
 style='width:95.52%;background:#DBE5F1;border-collapse:collapse'>
 <tr>
  <td width="18%" valign=top style='width:18.9%;padding:0cm 5.4pt 0cm 5.4pt'>
  <img border=0 width=83 height=20 id="Picture 189"
  src="manual_files/image009.gif" alt="window_disorder">
  </td>
  <td width="81%" valign=top style='width:81.1%;padding:0cm 5.4pt 0cm 5.4pt'>
  Run the <b>Disorder Builder</b> wizard
  </td>
 </tr>
</table>

<p class=MsoNormal align=center style='text-align:center'><img border=0
width=269 height=250 id="Picture 192" src="manual_files/image010.jpg">

Step 1 is to select either an existing model (which has a
unit cell) or choose to create a new one. Since the water molecule is the only
one loaded in this example, the existing model option is grayed out in the
graphic above.  When creating a new model, there are two options for the
generation of the unit cell – either you can specify the cell lengths and
angles explicitly to get exactly the cell you want, or you can specify just the
angles and _relative_ lengths of the cell, which will then be enlarged
automatically to contain whatever you choose to put inside it. For this
example, we will do the latter, so select the last option and click <b>Next</b>.

<p class=MsoNormal align=center style='text-align:center'><img border=0
width=269 height=250 id="Picture 193" src="manual_files/image011.jpg">

As mentioned, here we define exacly the angles of the cell,
but any lengths we specify are relative lengths which will be scaled according
to the number and density of the molecules we choose to put in the cell. We
will leave them as they are for now, so we will end up with a cubic cell with
some side length _l_.

<p class=MsoNormal align=center style='text-align:center'><img border=0
width=269 height=250 id="Picture 194" src="manual_files/image012.jpg">

The Disorder Builder normally permits a partitioning scheme
(see Section <a>XXX</a><span class=MsoCommentReference><span style='font-size:
8.0pt'><a class=msocomanchor id="_anchor_4"
onmouseover="msoCommentShow('_anchor_4','_com_4')"
onmouseout="msoCommentHide('_com_4')" href="#_msocom_4" language=JavaScript
name="_msoanchor_4">[TY4]</a>&nbsp;</span></span>) to be selected in order to
partition molecules up into different regions in the cell. When generating a
cell, however, this is not possible, so we must choose the basic cell option
and click <b>Next</b>.

<p class=MsoNormal align=center style='text-align:center'><img border=0
width=269 height=250 id="Picture 195" src="manual_files/image013.jpg">

Next we must choose which models or _components_ we
wish to add into the new system. Here there is only one choice, the water model
we created earlier, so select it ans press <b>Next</b>.

<p class=MsoNormal align=center style='text-align:center'><img border=0
width=269 height=250 id="Picture 196" src="manual_files/image014.jpg">

Finally, the most important step of the wizard is to define
a policy for the insertion of each model we selected in the last step.  There
are usually four options here; either a specific number of molecules or a
specific density (more correctly a specific _partition_ density), and
exact number and density, or a relative number and exact density. Since our
cell size is to be automatically determined, only one of those four options is
available, <b>Exact Number / Density</b>, since both pieces of information are
required in order to work out the final cell volume. So, choose the number of
water molecules to add, say 200, and the density you want them to have in the
cell. Press <b>Finish</b> and the system will then be built.

<p class=MsoNormal align=center style='text-align:center'><img border=0
width=431 height=352 id="Picture 191" src="manual_files/image015.jpg">

&nbsp;

<h4>Script</h4>

When run from the command line the disorder builder always requires a model
with a unit cell to be defined – only through the GUI is the model created
automatically. This script defines a basic cubic cell in a new model before the
disorder builder is run with the second argument set to FALSE (which indicates
that the cell size is not fixed and should be adjusted to suit the defined
molecule contents) and also minimises the system a little before saving out
coordinates and a forcefield expression for DL_POLY.

```
newModel("water");
newAtom(O);
addHydrogen();
setupComponent("both", 1, 200, 1.0);
<p class=ExampleCodeCxSpMiddle style='margin-left:0cm'>&nbsp;

&nbsp;
newModel("box");
cell(1.0,1.0,1.0,90,90,90);
disorder("None", FALSE);
&nbsp;
loadFF("spce.ff");
mcMinimise(20);
sdMinimise(20);
&nbsp;
<p class=ExampleCodeCxSpMiddle>saveExpression("dlpoly",
"water.FIELD");

<p class=ExampleCodeCxSpMiddle>saveModel("dlpoly",
"water.CONFIG");

&nbsp;
quit();
```

</div>

<b><span style='font-size:18.0pt;font-family:"Times New Roman","serif"'><br
clear=all style='page-break-before:always'>
</span></b>

<div class=WordSection11>

<h2><a name="_Toc309643492"></a><a name="_Ref295132175">5.2.<span
style='font:7.0pt "Times New Roman"'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;


