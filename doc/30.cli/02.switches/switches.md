---
title: Switches
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

## A

`--atendata <dir>`<a id="atendata"></a>

Tells **Aten** to use the specified _dir_ as its data directory (where filters etc. are stored). This overrides the **ATENDATA** shell variable.

## B

`-b, --bohr`<a id="b"></a>

Specifies that the unit of length used in any models and grid data that follow is Bohr rather than Å, and should be converted to the latter.

`--batch`<a id="batch"></a>

Enter batch processing  mode, modifying and saving models to their original filenames. See Section 6.3 for details and a list of other modes. 

`--bond`<a id="bond"></a>

Force recalculation of bonding in loaded models, regardless of whether the filter used any of the rebond commands (see the list of [a]bond-related commands,command-bond[/a]).

## C

`-c <commands>, --command <commands>`<a id="c"></a>

Provides a command or compound command to execute. Commands should be enclosed in single quotes (to prevent the shell from misquoting any character strings) and individual commands separated with semicolons. Commands provided in this way can be used to set up **Aten** in the way you want it from the command line, perform operations on model files before loading the GUI, or perform operations on model files without loading the GUI at all.

For example, to start the GUI with a new model named ‘cube’ that has a cubic cell of 30 Å side length:

```
bob@pc:~> aten -c 'newModel("cube"); cell(30,30,30,90,90,90);'
```

Similarly, to load a model and make a duplicate copy of the atoms (pasted into the same model):

```
bob@pc:~> aten original.xyz -c 'selectAll(); copy(); paste(10,10,10);'
```

In both cases the GUI initialises itself without being told, but this can be prevented with the [a]quit,command-system#quit[/a] command. Consider the last example – to save the newly-expanded model and quit without ever launching the GUI:

```
bob@pc:~> aten original.xyz -c 'selectAll; copy(); paste(10,10,10); saveModel("xyz", "pasted.xyz"); quit;'
```

Multiple sets of commands may be given:

```
bob@pc:~> aten source.xyz -c 'selectAll; copy();' target.xyz -c 'paste(10,10,10);'
```

Take care here, since the commands provided act on the current model, i.e. the one that was most recently loaded. Commands are available that select between the loaded models – see the list of model-related commands in Section 9.22.

`--cachelimit <limit>`<a id="cachelimit"></a>

Sets the size limit for trajectory loading, in kilobytes. If an entire trajectory will fit into this cache, all frames in the trajectory are loaded immediately. If not, frames will be read from disk as and when required.

`--centre`<a id="centre"></a>

Force translation of non-periodic models centre-of-geometry to the origin, even if the [a]centre,command-transform#centre[/a] command was not used in the corresponding filter.

## D

`-d [<type>], --debug [type]`<a id="d"></a>

Enables debugging of subroutine calls so that program execution can be traced, or enables extra debug output from specific types of routines (if a _type_ is given). Warning - this creates a lot of output, most of which is incomprehensible to people with their sanity still intact, but is useful to track the program up to the point of, say, a hideous crash. Valid _type_ values are listed under [a]Output Enums,enums-output[/a].

`--dialogs`<a id="dialogs"></a>

By default, filter and script user dialogs are not shown when performing functions on the command-line. This switch forces dialogs to be shown.

`--double <name=value>`<a id="double"></a>

Creates a ‘floating’ variable _name_ which is of type [type]double[/type] and that can be accessed from any subsequent script, command, or filter. Note that declarations of variables with the same name made in scripts, commands and filters will override any passed value names in order to avoid conflicts and breaking of existing filters and scripts. The intended use is to be able to pass values easily from the command-line into scripts or one-line commands.

For example, in a bash shell:

```
bob@pc:~> for num in 10.0 50.5 100.0; do aten --double d=$num -c 'printf("Value is %f\n", d); quit();'; done
```

## E

`--export <nickname>`<a id="export"></a>

Enter export  mode, where each model specified on the command line is loaded and saved in the format corresponding to the _nickname_ specified. If specified in conjunction with [--batch](/aten/docs/cli/switches#batch), batch export mode is entered instead, with commands run on models before being saved to the new format. See Section 6.3 for details and a list of other modes. 

`--exportmap <name=element,...>`<a id="exportmap"></a>

Manually map assigned atom typenames in an expression to the names defined here when expressions are written to a file. For example:

```
bob@pc:~> aten --ff spc.ff data/test/water.xyz --exportmap "OW=Ospc,H=Hspc" -c 'saveExpression("dlpoly", "water.FIELD"); quit();'
```

writes the water forcefield with the **OW** and **HW** atomtype names mapped to **Ospc** and **Hspc** respectively.

`--expression <file>`<a id="expression"></a>

Loads the specified _file_ as if it were an expression.

## F

`-f <nickname>, --format <nickname>`<a id="f"></a>

For any forthcoming model files provided as arguments on the command line, the specified model import filter is used to load them, regardless of their filename extension (or, indeed, actual format). Since **Aten** tends not to determine file formats by looking at their content, this is useful for when you know that file is in a particular format, but with an extension that doesn't help **Aten** recognise it as such.

`--ff <file>`<a id="ff"></a>

Loads the specified forcefield file, making it the current forcefield. If the desired forcefield is present in either **Aten**’s installed **data/** directory or in your own **~/.aten/ff** directory (see the quick guide on [a]Locations,quick-locations[/a]), then just the filename need be given as **Aten** searches these locations by default.

`--filter <file>`<a id="filter"></a>

Load the specified **file** as if it were a filter file, installing any filters defined within it. Any filters already loaded that have the same ‘nickname’, ‘id’ etc. will be hidden by those loaded from **file**. See [a]overriding existing filters,filters-overriding[/a] for more information.

`--fold`<a id="fold"></a>

Force folding of atoms to within the boundaries of the unit cell (if one is present) in loaded models, even if the command [a]fold,command-cell#fold[/a] was not used in the corresponding filter.

## G

`-g <file>, --grid <file>`<a id="g"></a>

Loads the specified grid data **file**, associating it to the current model, and making it the current grid. A model (even an empty one) must exist for a grid to be loaded.

## H

`-h, --help `<a id="h"></a>

Show the possible command-line switches and a short description of their meaning.

## I

`-i, --interactive`<a id="i"></a>

Starts **Aten** in interactive mode, where commands are typed and immediately executed. The GUI is not started by default, but may be invoked.

`--int <name=value>`<a id="int"></a>

Creates a floating integer variable _name_. See the [--double](/aten/docs/cli/switches#d) switch for a full description.

## K

`-k, --keepview`<a id="k"></a>

Preserves the last stored view of models when the GUI starts, retaining any model rotations and camera transformations performed in scripts or on the command line (normally, the view is reset to display the entire model on startup).

`--keepnames`<a id="keepnames"></a>

If specified, for each model loaded the original atom names in the file will be preserved as a series of forcefield types generated within a new forcefield created specifically for the model. Elements are still determined from conversion of the atom names, and may still be mapped with the [--map](/aten/docs/cli/switches#map) option. This option is useful for quickly creating a skeleton set of forcefield types from an existing model with type names, or to allow quick import and export of typed configurations without requiring the original forcefield file to be loaded.

Note that the [--keeptypes](/aten/docs/cli/switches#keeptypes) and [--keepnames](/aten/docs/cli/switches#keepnames) switches are mutually exclusive.

`--keeptypes`<a id="keeptypes"></a>

If specified, for each atom name converted to an element using a forcefield name match, the corresponding forcefield type will be assigned to the atom and fixed. Like the [--keepnames](/aten/docs/cli/switches#keepnames) switch, this is useful for preserving atom type data when importing certain models which do not store element information.

Note that the [--keeptypes](/aten/docs/cli/switches#keeptypes) and [--keepnames](/aten/docs/cli/switches#keepnames) switches are mutually exclusive.

## M

`-m <name=element,...>, --map <name=element,...>`<a id="m"></a>

Manually map atom typenames occurring in model files to elements according to the rules defined here. For example:

```
bob@pc:~> aten --map 'CX=C,N_=P'
```

will result in atoms called **CX** being mapped to carbon, and atoms called **N_** mapped to phosphorus (for whatever reason). These mappings are attempted prior to any z-mapping scheme defined in the filter, and so will take precedence over standard typename-to-element conversions.

## N

`-n`<a id="newmodel"></a>

Create a new, empty model.

`--nicknames`<a id="nicknames"></a>

Print a list of all available import/output filter nicknames and quit.

`--nobond`<a id="nobond"></a>

Prevent recalculation of bonding in loaded models, overriding filter directives. This basically means that, if a filter tries to run the [a]rebond,command-bond#rebond[/a] command, then specifying this switch will prevent it.

`--nocentre`<a id="nocentre"></a>

Prevent translation of non-periodic models centre-of-geometry to the origin, overriding filter directives.

`--nofold`<a id="nofold"></a>

Prevent initial folding of atoms to within the boundaries of the unit cell (if one is present) in loaded models, overriding the use of the [a]fold,command-cell#fold[/a] command in the corresponding filters.

`--nofragments`<a id="nofragments"></a>

Prevent loading of fragments from both standard and user locations on startup.

`--nofragmenticons`<a id="nofragmenticons"></a>

Prevent generation of fragment icons, used in the Fragment Library Window (see Section 7.13).

`--noincludes`<a id="noincludes"></a>

Prevent loading of global includes on startup.

`--nolists`<a id="nolists"></a>

Prevent the use of OpenGL display lists for rendering.  Simple vertex arrays will be used instead. Try this option out if rendering is corrupt or **Aten** crashed unexpectedly on startup. The rendering will be slower, but more compatible.

`--nopack`<a id="nopack"></a>

Prevent generation of symmetry-equivalent atoms from spacegroup information in loaded models, overriding any occurrences of the [a]pack,command-cell#pack[/a] command is used in the corresponding filter.

`--nopartitions`<a id="nopartitions"></a>

Prevents loading of partitions on startup.

`--noqtsettings`<a id="noqtsettings"></a>

Don’t read in any system-stored Qt settings on startup (such as window positions, toolbar visibilities etc.) using the defaults instead.

## P

`--pack`<a id="pack"></a>

Force generation of symmetry-equivalent atoms from spacegroup information in loaded models, even if the [a]pack,command-cell#pack[/a] command was not used in the corresponding filter.

`--pipe`<a id="pipe"></a>

Read and execute commands from piped input on startup.

`--process`<a id="process"></a>

Enter process mode, where commands are run on models but no changes are saved – instead, the GUI is started once all commands have been executed. See Section 6.3 for details and a list of other modes. 

## Q

`-q, --quiet`<a id="q"></a>

Prevents nearly all text output from **Aten**, including error messages and the like, but does allow printing of user output via the [a]printf,command-messaging#printf[/a] command in scripts and commands passed with [`--command`](/aten/docs/cli/switches#c). Useful in order to print clean data to a file or standard output.

## S

`-s <file>, --script <file>`<a id="s"></a>

Specifies that the script file is to be loaded and run before moving on to the next command-line argument. A script file is just a plain text file that contains sequence of commands to be executed, written in the [a]command language style,cmdlang[/a].

`--string <name=value>`<a id="string"></a>

Creates a floating string variable _name_. See the [--double](/aten/docs/cli/switches#d) switch for a full description.

## T

`-t <file>, --trajectory <file>`<a id="t"></a>

Associates a trajectory file with the last loaded / current model.

## U

`-u <nlevels>, --undolevels <nlevels>`<a id="u"></a>

Set the maximum number of undo levels per model, or -1 for unlimited (the default).

## V

`-v, --verbose`<a id="v"></a>

Switch on verbose reporting of program actions.

`--vbo`<a id="vbo"></a>

Attempt to use OpenGL vertex buffer objects when rendering, for maximum performance.

## Z

`-z <maptype>, --zmap <maptype>`<a id="z"></a>

Override the names to elements z-mapping style defined in file filters. For a list of possible mapping types see [a]ZMapping Types,enums-zmapping[/a].


