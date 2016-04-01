---
title: Chloroform Builder
brief: Self-contained liquid chloroform builder
taxonomy:
  category: docs
  example: [scripting, forcefield, building, disorder]
visible: true
template: manpage
docroot: /aten/docs
header_class: alt
---

This script (installed in the `$ATENDATA/scripts` directory) is an example of a completely self-contained builder, depending on no external model or forcefield files. The chloroform model is built by hand, and the necessary forcefield is constructed in the script itself, before being used to create a box of liquid chloroform. Only the density of the liquid and the desired number of molecules are required in order to build the system, since the molecular volume is worked out from the density and the mass.

Once created, the configuration is minimised with a little molecular Monte Carlo, and then saved in a format suitable for use with DL_POLY.

```
# First, create chloroform model by transmuting a methane molecule.
newModel("chloroform");
newAtom(C);
addHydrogen();
select(3,4,5);
transmute(Cl);
# Set correct C-Cl distances
for (int n=3; n<6; ++n) setDistance(1,n,1.758);

# Set number of molecules and density required (g/cm3)
setupComponent("both", 1, 100, 1.483);

# Create a model with a basic unit cell - the disorder builder will adjust its size as necessary
newModel("box");
cell(1.0,1.0,1.0,90,90,90);
disorder("None", FALSE);

# Construct new forcefield (using AMBER params)
# Parameters taken from files at http://www.pharmacy.manchester.ac.uk/bryce/amber
newFF("chloroformff");
units("kj");

# Atom types
typeDef(1,"CZ","CZ",C,"-Cl(n=3),-H","Chloroform carbon");
typeDef(2,"HZ","HZ",H,"-&amp;1","Chloroform hydrogen");
typeDef(3,"Cl","Cl",Cl,"-&amp;1","Chloroform chlorine");

# Atom van der Waals and charge
interDef("lj",1,-0.3847,4.184*0.1094, 1.9080*2.0/2.0^(1.0/6.0));
interDef("lj",2,0.2659,4.184*0.0157, 1.187*2.0/2.0^(1.0/6.0));
interDef("lj",3,0.0396,4.184*0.3250, 2.0*2.0/2.0^(1.0/6.0));

# Intramolecular terms
bondDef("harmonic","CZ","HZ",2845.12,1.1);
bondDef("harmonic","CZ","Cl",1944.7232,1.758);
angleDef("harmonic","HZ","CZ","Cl",318.8208,107.68);
angleDef("harmonic","Cl","CZ","Cl",650.1936,111.3);

# Finalise the forcefield and associated it to the CCl4 model
finaliseFF();
ffModel();

# Minimise our new system a little
mcMinimise(100);

saveModel("dlpoly", "chloroform.CONFIG");

saveExpression("dlpoly", "chloroform.FIELD");
```


