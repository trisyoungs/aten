---
title: Exporting in Bohr
brief: Modifying a filter to export coordinates in Bohr rather than Angstroms
taxonomy:
  category: docs
  example: [filters]
visible: true
template: manpage
docroot: /aten/docs
header_class: alt
---

**Aten** works in units of Angstroms, but of course this does not suit every other computational code out there.  Many physics code allow (or require) input in units of Bohr. The simplest way of generating coordinates in Bohr rather than Angstroms is to write a custom filter to output the data in the units that you want, converting to/from Angstroms on the fly. This example takes the existing xyz filter and creates a new ‘xyzbohr’ filter that reads and writes in Bohr.

Firstly, copy the `xyz` filter that comes with **Aten**, probably in `/usr/share/aten/data/filters` on Linux/Mac, or `C:\Program Files\Aten N.MMM` on Windows, and copy this to your user filter directory. On Linux/Mac this is `~/.aten/filters`, while on Windows this will be a directory called `aten` in your user home in Documents and Settings. Rename your copy of the file to `xyzbohr`.

To make this new filter file read and write in units of Bohr we need to simply divide or multiply by 0.5292, depending on whether we’re writing or reading the coordinates. So, begin by changing the **importmodel** section (around line 23) to read:

```
i = newAtom(e, rx*0.5292, ry*0.5292, rz*0.5292);
```

This just converts the units of Bohr in the file back to Angstroms (which **Aten** expects). The **exportmodel** section must be modified accordingly (around line 38):

```
for (Atom i=m.atoms; i; ++i) writeLineF("%-8s  %12.6f %12.6f %12.6f %12.6f\n",i.symbol,i.rx*0.5292,i.ry*0.5292,i.rz*0.5292,i.q);
```

The **importtrajectory** section can be modified in a similar way, or can be removed if it is not required.

As it stands, this new  filter will ‘hide’ the original `xyz` filter since both the nicknames and IDs are the same, and so they must be changed in the function headers. Simply changing all occurrences of ‘xyz’ to something like ‘xyzb’ is enough, as well as choosing an ID for the new filter which is not currently in use (something above 100 should be safe, but consult the [full list](/aten/docs/introduction/fileformats) to make sure).

The next time **Aten** is run the user filter directory will be searched and the new `xyzbohr` filter will be loaded ready for use, just as the normal stock of filters are.

The entire new filter file looks like this:

```aten
# Bohr XYZ coordinates files (for v1.2+)
# Created:       19/07/2011
# Last modified: 19/07/2011
# ChangeLog:

filter(type="importmodel",name="XMol XYZ Coordinates (in Bohr)", nickname="xyzb", extension="xyzb", glob="*.xyzb", id=100)
{
        # Variable declaration
        int natoms,n,m;
        string e,title;
        double rx,ry,rz,q;
        Atom i;

        # Read data
        while (!eof())
        {
                readLine(natoms);
                getLine(title);
                newModel(title);
                for (n=1; n<=natoms; ++n)
                {
                        readLine(e,rx,ry,rz,q);
                        i = newAtom(e, rx*0.529, ry*0.529, rz*0.529);
                        i.q = q;
                }
                rebond();
                finaliseModel();
        }
}

filter(type="exportmodel",name="XMol XYZ Coordinates (in Bohr)", extension="xyzb", glob="*.xyzb", nickname="xyzb", id=100)
{
        # Variable declaration
        Model m = aten.frame;

        writeLine(m.natoms);
        writeLine(m.name);
        for (Atom i=m.atoms; i; ++i) writeLineF("%-8s  %12.6f %12.6f %12.6f %12.6f\n", i.symbol, i.rx/0.529, i.ry*0.529, i.rz/0.529,i.q);
}
```


