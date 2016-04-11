---
title: Torsion Energy Profile
brief: Calculating a torsion energy profile from the CLI
taxonomy:
  category: docs
  example: [forcefield, calculate, CLI]
visible: true
template: manpage
docroot: /aten/docs
header_class: alt
---

This example demonstrates a simple energy analysis procedure in which we load a model and perform a scan of a geometric parameter, calculating the energy at each step. The methanol model and OPLS-AA forcefield (both supplied with **Aten**) are used by the command, but of course can easily be substituted with your own choices. The command first sets the electrostatic calculation method to be the simple Coulomb sum (since the methanol model is non-periodic) and prior to the loop starting a text header is written. The loop iterates a variable _phi_ over the range -180 to +180 degrees in steps of 5, and is used to set the torsion angle between atoms 2, 1, 4, and 6 (corresponding to one of the H–C–O–H torsions in the model). A line of output is written for each torsion angle considered, providing the total torsion, van der Waals, and electrostatic energy of the system at this geometry.

```aten
bob@pc:~> aten --ff oplsaa.ff data/test/methanol.inp -q -c 'aten.prefs.elecMethod = "coulomb";
             printf("Torsion Angle   E(Torsion)     E(VDW)     E(Coulomb)\n");
             Model m = aten.model;
             for (double phi = -180.0; phi <= 180.0; phi += 5) { 
                 setTorsion(2,1,4,6,phi);
                 printf("%12.6f %12.6f %12.6f %12.6f\n", phi, m.torsionEnergy(), m.vdwEnergy(), m.elecEnergy());
             }
             quit();'
```

Note that the use of the [`-q` switch](/aten/docs/cli/switches#q) option means that only fundamental error messages and user output (through the [**printf**](/aten/docs/scripting/commands/messaging#printf) statements in the command) is printed – all of **Aten**’s other working information is suppressed.

