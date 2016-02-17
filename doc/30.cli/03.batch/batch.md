---
title: Batch Mode
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

**Aten** has several batch or ‘offline’ processing modes that do not need the GUI to be invoked. These permit calculations, analyses or processes to be performed on multiple models in one simple command. Most work by storing any commands or command sequences supplied with [`--command`](/aten/docs/cli/switches#switch_command) until all models are loaded, and then running the commands on each loaded model in sequence. The modes are as follows:

## Batch Mode

Invoked by the [`--batch`](/aten/docs/cli/switches#switch_batch) switch, this mode runs all commands provided on all models, once the last model has been loaded. The models are then saved in their original format to the same filename. Note that, if the an export filter does not exist for the original model file format, changes to that model will not be saved. It is advisable to work on a copy of the model files when using this command, or to use batch export mode to save to a different format in order to preserve the original files. The GUI is not automatically started in batch mode.

For example, to transmute all iron atoms into cobalt for a series of xyz files named [name]complex_001.xyz[/name], [name]complex_002.xyz[/name] etc.

```
bob@pc:~&gt; aten --batch -c 'select(Fe); transmute(Co);' complex_*.xyz
```

## Export Mode

Invoked by the [`--export`](/aten/docs/cli/switches#switch_export) switch, in export mode each model file specified on the command line is loaded and immediately saved in the format specified by the provided nickname, allowing multiple files to be converted to a different format at once. The GUI is not automatically started in export mode.

For instance, to convert three DL_POLY CONFIG files and an xyz into mol2 format:

```
bob@pc:~&gt; aten --export mol2 bio1.CONFIG bio2.CONFIG watercell.CONFIG random.xyz
```

If specified in conjunction with the [`--batch`](/aten/docs/cli/switches#switch_batch) switch, batch export mode is entered instead, and any supplied commands are executed on each loaded model file before it is saved. The original model files are not modified.

## Batch Export Mode

Invoked by providing the [`--batch`](/aten/docs/cli/switches#switch_batch) and [`--export`](/aten/docs/cli/switches#switch_export) switches together, batch export allows a series of commands to be run on a set of loaded models, the results of which are then saved in new files in the model format provided to the [`--export`](/aten/docs/cli/switches#switch_export) switch.  The GUI is not automatically started in batch export mode.

Let’s say that you have a directory full of xyz files that you wish to energy minimise with MOPAC2009 (see [a]External Programs - Mopac,extern-mopac[/a]), centre at zero, and then save as input to GAMESS-US. This can be achieved with the following command:

```
bob@pc:~&gt; aten --export gamusinp --batch –c 'mopacMinimise(); selectAll(); centre();' *.xyz
```

Various export options for the GAMESS-US filter (e.g. method type, basis set) can be set at the same time. See how to set filter options in Section 11.1.5, and Section 5.11 for an example.

## Process Mode

Similar to the [`--batch`](/aten/docs/cli/switches#switch_batch) switch, in that all commands supplied with [`--command`](/aten/docs/cli/switches#switch_command) are executed on each model, but in this case the results are <b>not</b> saved, and the GUI starts once processing is complete.


