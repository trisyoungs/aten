---
title: X
taxonomy:
  category: docs
template: docs
docroot: /aten/docs
header_class: alt
---


# Example - Saving GAMESS-US Input with Options (CLI)

Batch conversion or saving between molecule formats which contain purely coordinate and cell information is relatively trivial, but when saving to file formats which contain many user-adjustable parameters (e.g.  Gaussian or GAMESS-US input) it may also be desirable to set these options explicitly on the command-line with **Aten**, rather than editing the files afterwards.

For export filters which contain GUI dialog definitions allowing the user to set these options it is possible to modify the values of all the controls defined in the dialog from the command-line, simply by specifying the control name and new value the control should take on. The following example loads the supplied water model (in  format), performs a MOPAC minimisation on the model (with [a]`--batch`,cli-switches#switch_batch[/a] mode), and then saves it as a GAMESS-US input file. The [a]`--export`,cli-switches#switch_export[/a] switch takes just the nickname of the export filter at its most basic, but can also be supplied with a comma-separated list of control names and values to set before writing the file(s). In this case, we request that a DFT calculation be performed with the B3LYP functional, using the MCP-DZP basis set, and allowing for 200 steps in the geometry optimisation. Note that all the GUI controls in the GAMESS-US export filter have been given names which relate directly to the block and keyword which they relate to in the input file.

```
bob@pc:~&gt; aten data/test/water.xyz --export "gamusinp,contrl_dfttyp=B3LYP,basis_gbasis=MCP-DZP,statpt_nstep=200" --batch -c 'mopacminimise();'
```

Alternatively, one may specify the [a]`--dialogs`,cli-switches#switch_dialogs[/a] switch to force the export filter’s dialog box to be raised when running **Aten** from the command-line, allowing options to be set using the dialog (but without raising the full GUI).


