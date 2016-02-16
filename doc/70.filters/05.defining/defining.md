---
title: Defining
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

Filter definitions are made in a filter file in a similar way to declaring a user subroutine or function (see Section 8.1.7). The **filter** keyword marks the start of a filter definition, and contains a list of properties in parentheses that define the subsequent filter, its name, and how to recognise the files (from their filenames and/or contents) that it is designed for. The definition of the filter to import XYZ-style model data is as follows:

```
filter(type="importmodel", name="XMol XYZ Coordinates", nickname="xyz", extension="xyz", glob="*.xyz", id=3)
{
      commands
      ...
}
```

The comma-separated list of properties defines the type of filter ('[name]type="importmodel"[/name]') and how to recognise files of that type (e.g., '[name]extension="xyz"[/name]'), amongst other things. 

The full list of possible properties is as follows:

<table>
 <title>Filter Definition Keyword Summary</title>
 <header>
  <column>Property</column>
  <column>Description</column>
 </header>
 <row>
  <column></column>
  <column>Comma-separated list of filenames that are of this type</column>
 </row>
 <row>
  <column></column>
  <column>Comma-separated list of filename extensions that indicate files of this type</column>
 </row>
 <row>
  <column></column>
  <column>Shell-style glob to use in file fialogs in order to filter out files of the described type</column>
 </row>
 <row>
  <column></column>
  <column>Numerical ID of the filter to enable partnering of import/export filters for files of the same type</column>
 </row>
 <row>
  <column></column>
  <column>Descriptive name for the filter, shown in file dialogs etc.</column>
 </row>
 <row>
  <column></column>
  <column>Short name used by commands in order to identify specific filters</column>
 </row>
 <row>
  <column></column>
  <column>Provides a string to search for in the file. If the string is found, the file is identified as being readable by this filter type. The number of lines searched is governed by the  property</column>
 </row>
 <row>
  <column></column>
  <column>Defines the kind of filter that is described (i.e. if it loads/saves, acts on models/grid data etc.) so that **Aten** knows when to use it. This must always be defined!</column>
 </row>
 <row>
  <column></column>
  <column>Specifies the number of lines to search for any supplied  strings</column>
 </row>
 <row>
  <column></column>
  <column>Determines which zmapping style to employ when converting atom names from the file</column>
 </row>
</table>

---

## exact <a id="exact"></a>

_Syntax:_

_exact_="name1,name2,..."

Occasionally (and annoyingly) files have no extension at all, instead having short, fixed names, which must be checked for literally when probing files. This command defines one or more explicit filenames that identify files targeted by this filter. Multiple names may be given, separated by commas or whitespace. Exact filename matching is case-insensitive.

For example:

```
exact="coords"
```

associates any file called 'coords' to this filter.

```
exact="results,output"
```

associates any files called 'results' or '_output_' to this filter.

---

## extension <a id="extension"></a>

_Syntax:_

_extension_="extension1,extension2,..."

Sets the filename extension(s) that identify files to be read / written by this filter. When files are being probed for their type, in the first instance the filename is examined and the extension (everything after the last '.') is compared to those defined in filter sections by this command. Multiple file extensions may be given, separated by commas or whitespace. File extension matching is case-insensitive.

For example:

```
extension="xyz"
```

means that files with extension '.xyz' will be recognised by this filter.

```
extension="xyz,abc,foo"
```

means that files with extensions '.xyz', '.abc', and '.foo' will be recognised by this filter.

---

## glob <a id="glob"></a>

_Syntax:_

_glob_="*|*.ext"

Sets the file dialog filter extension to use in the GUI, and should be provided as a shell-style glob.

For example:

```
glob="*.doc"
```

filters any file matching '[var]*.doc[/var]' in the relevant GUI file selector dialogs.

---

## id <a id="id"></a>

_Syntax:_

_id_=n

When separate import and export filters for a given file type have been provided it is prudent to associate the pair together so that **Aten** knows how to save the data that has just been loaded in. Each filter has a user-definable integer ID associated with it that can be used to link import and export filters together. For example, if a model import filter has an ID of 7, and a model export filter also has this ID, then it will be assumed that the two are linked, and that a model saved with export filter 7 can be subsequently loaded with import filter 7. If the ID for a filter is not set it defaults to -1, and it is assumed that no partner exists and the file cannot be directly saved back into this format.

For example:

```
id=13
```

See the [list of supported formats](/aten/docs/introduction/fileformats) to find which ids are currently in use.

---

## name <a id="name"></a>

_Syntax:_

_name_="name of filter"

Sets the long name of the filter, to be used as the filetype description of files identified by the filter. This name will appear in the file type lists of file dialogs in the GUI, and also in the program output when reading / writing files of the type.

For example:

```
name="SuperHartree Coordinates File"
```

---

## nickname <a id="nickname"></a>

_Syntax:_

_nickname_="short name of filter"

Sets a nickname for the filter, which allows it to be identified easily in the command language and, importantly, from the command line. It should be a short name or mnemonic that easily identifies the filter. No checking is made to see if a filter using the supplied nickname already exists.

For example:

```
nickname="shart"
```

sets the nickname of the filter to 'shart'.

```
nickname="zyx"
```

sets the nickname of the filter to 'zyx'.

---

## search <a id="search"></a>

_Syntax:_

_search_="string to search"

Occasionally, checking the contents of the file is the easiest way to determining its type, and is probably of most use for the output of codes where the choice of filename for the results is entirely user-defined. For example, most codes print out a whole load of blurb and references at the very beginning, and usually searching for the program name within this region is enough to identify it. For files that are only easily identifiable from their contents and not their filename, plain text searches within files can be made to attempt to identify them. Individual strings can be given to the  keyword, and may be specified multiple times. The default is to search the first 10 lines of the file for one or more of the defined search strings, but this can be changed with the **within** command.
property.

For example:

```
search="UberCode Version 20.0"
```

matches the filter to any file containing the string 'UberCode Version 20.0' within its first 10 lines (the default).

```
search="SIESTA"
```

searches the first 10 lines of the file for the string 'SIESTA'.

```
search=""GAMESS VERSION = 11 APR 2008 (R1)"
```

attempts to identify output from a specific version of GAMESS-US.

---

## type <a id="type"></a>

_Syntax:_

_type_="filtertype"

The 'type' keyword must be provided an all filter definitions - an error will be raised if it is not. It specifies which class of data the filter targets (e.g. models, grid data etc.) and whether it is an import or export filter. A given filter may only have one  specified, for which the possible values are:

<table>
 <title>Filter Types</title>
 <header>
  <column>Type</column>
  <column>Description</column>
 </header>
 <row>
  <column></column>
  <column>Describes how to export forcefield descriptions (expressions) for models</column>
 </row>
 <row>
  <column></column>
  <column>Describes how to export grid-style data</column>
 </row>
 <row>
  <column></column>
  <column>Describes how to write out model data</column>
 </row>
 <row>
  <column></column>
  <column>Filter suitable for the export of trajectory data</column>
 </row>
 <row>
  <column></column>
  <column>Describes how to load in forcefield-style expressions</column>
 </row>
 <row>
  <column></column>
  <column>Describes how to read gridded volumetric or surface data from files. Any grids created in these sections must have the [finaliseGrid](/aten/docs/scripting/commands/grid#finalisegrid) command called on them, otherwise they will not be registered properly within the program.</column>
 </row>
 <row>
  <column></column>
  <column>Describes how to import model data, including atoms, cell and spacegroup data, bonds, glyphs etc. Any models created in 'importmodel' filters must have the [finaliseModel](/aten/docs/scripting/commands/model#finaliseModel) command called on them, otherwise they will not be registered properly within the program.</column>
 </row>
 <row>
  <column></column>
  <column>Read frames from trajectory files. See the section on trajectories (Section 11.2) for additional information on how trajectories are handled within **Aten**.</column>
 </row>
</table>

For example:

```
type="importgrid"
```

...or...

```
type="exportmodel"
```

---

## within <a id="within"></a>

_Syntax:_

_within_=n

Defines the maximum number of lines at the beginning of the file that will be searched for string definitions (default is 10).

For example:

```
within=50
```

specifies that the first 50 lines should be searched for identifying strings.

---

## zmap <a id="zmap"></a>

_Syntax:_

_zmap_="zmaptype"

By default, it is assumed that the commands which create new atoms will be given a proper element symbol from which to determine the atomic number. Case is unimportant, so , , and  will all be interpreted as atomic number 11 (sodium). Where element symbols are not used in the model file, there are several alternative options that tell these commands how to convert the element data they are passed into atomic numbers. For example, the ff style is particularly useful when loading in coordinate files which contain forcefield atom type names that do not always correspond trivially to element information (e.g. DL_POLY configurations).

For example:

```
zmap="numeric"
```

indicates that atomic numbers are provided in place of element names and no conversion should be performed. See the [ZMapping enum](/aten/docs/enums/zmaptype) for a list of available z-mapping methods.


