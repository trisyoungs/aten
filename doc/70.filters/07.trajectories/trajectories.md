---
title: Trajectory Filters
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

Trajectory files usually mean one of two things – either a sequence of frames from a molecular dynamics simulation trajectory, or a sequence of configurations from a geometry optimisation of some kind. Either way, both boil down to the same thing from **Aten**'s perspective, that is a set of models in a sequence. In terms of displaying such a set of models, either they may be loaded as individual models (i.e. having a separate tab in the GUI) or the sequence of models may be associated to a single 'parent' model. Most commonly, the latter is the preferred method (especially when large numbers of models are present in the trajectory).

There are two related ways to get this data into **Aten**. From the perspective of molecular dynamics simulations, the parent model or configuration and the trajectory frames are stored in separate files. In this case, the model can be loaded first, and then the trajectory file attached or associated to this model afterwards. From the perspective of geometry optimisations, for example, the parent configuration (i.e. the starting point of the optimisation) and the sequence of coordinates are most often stored in the same output file. In this case, the importmodel filter can detect the presence of the additional trajectory frames and manually attach them to the parent model. The following sections explain the details of how both methods work.

## Trajectories in Separate Files

DL_POLY, being a molecular dynamics code, stores its configuration and trajectory data in separate files. Filters are supplied with **Aten** that read in DL_POLY trajectories, and so this example will revolve around those filters.

### Necessity for a Master Configuration?

Thus far, it has been implicity stated that the 'master' configuration and the trajectory files come necessarily as a pair - the master configuration is read in, and then the trajectory associated to it. This implies that the trajectory file is somehow tied to the master configuration - perhaps the trajectory file does not contain information such as the number of atoms or element data, and so a 'reference' configuration is necessary? Of course, this may not always be the case, and it is possible that some trajectory formats will store all the necessary information needed in order to fully generate the trajectory configurations. DL_POLY trajectories do, in fact, contain all the necessary data, but even so the master configuration is still used as a template for the trajectory data, and is used to check that the correct number of atoms are present etc. It comes down to a matter of preference as to whether the master configuration should be demanded, or whether the trajectory can itself be associated to any (even an empty) model. Remember, [filters](/aten/docs/filters) always attach the trajectory data to an existing model.

## ImportTrajectory Filters

An  filter is written in a slightly different way to other filter types. Since trajectory files may contain header data which is written once at the beginning of the file, preceeding the frame data, there are potentially two separate sets of data to read from trajectory files - header data and (individual) frame data. So, rather than putting the code to read this data directly in the main body of the filter, two functions should be defined instead, one for reading the header (which must be called **readHeader**), and one for reading an individual frame (which must be called **readFrame**). Note that, if a given trajectory format does not contain a header, the corresponding function may be left empty, but must still be defined and should return a value of '1'. Both functions take no arguments, and must return an integer. A template for an importTrajectory filter is thus:

```
filter(type="importtrajectory", name="Example Filter Template")
{
      int readHeader()
      {
             // Code to read header data goes here
             ...
      }
      
      int readFrame()
      {
             // Code to read frame data goes here
             ...
      }
}
```

The functions must take responsibility for informing **Aten** when the desired data cannot be read. Both should return a value of '1' if the data was read successfully, and should return '0' if an error is encountered.

## Header Data

When opening a trajectory file with a filter, the first thing **Aten** does is attempt to read any header information from the file by calling the **readHeader** function defined in the filter. Since a trajectory file may not contain a header, and consists simply of individual frames back to back, in these situations the **readHeader** function defined in the filter should not read any data. The filter definition then becomes simply:

```
filter(type='importtrajectory', name="Example Filter Template")
{
      int readHeader()
      {
             // No header, so just return
             return 1;
      }
      
      int readFrame()
      {
             // Code to read frame data goes here
             ...
      }
}
```

Here, the function always succeeds, so **Aten** always thinks it has successfully read a header.

## Frame Data

If **readHeader** is successful, **Aten** proceeds to read the first frame (by calling the **readFrame** function) in order to get an idea of the size of an individual frame, and hence the total number of frames in the trajectory. Of course, this assumes that all frames take up the same number of bytes in the file, and may not always be the case, especially for plain-text trajectory files. Thus, the frame estimate output by **Aten** should not necessarily be taken as gospel.

Unless an error is encountered when reading the test frame (i.e. **readFrame** returns '0' or FALSE) the trajectory file is then rewound to the end of the header section (start of the frame data). One of two things then happens. Since trajectory files are typically enormous (hundreds or thousands of megabytes) then it is unwise to try and load the whole trajectory into memory at once. **Aten** knows this, and from the estimated frame size also knows roughly how big the whole trajectory is. If the total trajectory file size is greater than an internally-defined limit (the “trajectory cache size") then only a single frame is stored at any one point. If the total size is smaller then this limit, the whole trajectory is cached in memory. Both have their advantages and disadvantages, as listed in the following sections.

## Uncached Frames

If the trajectory is too big to be stored in memory, **Aten** only holds a single frame in memory at any one time. This means that:

<ul>
 <li>Memory use is minimised since only a single frame is loaded at any time</li>
 <li>Performance is slower – moving between frames means data must be read from disk</li>
 <li>Edits are forgotten – changes (both atomic and stylistic) made to the loaded frame are forgotten when a different frame is read</li>
</ul>

**Aten** tries to minimise the seek time between frames by storing file offsets of frames it has already read in. However, since trajectory frames can be different sizes **Aten** never tries to 'jump' ahead in the file based on the size of a single frame. Skipping immediately to the final frame in the trajectory will, thus, read all frames along the way and store file offsets for all frames. Then, seeking to any individual frame is a much quicker process.

Although style and editing changes are forgotten between frames, the overall camera view of the model is linked to that of the master configuration and so is retained between frames. If the trajectory cannot be cached and you require changes (edits or styles) to be made to each frame (e.g. for the purposes of making a movie of the trajectory) then a script is the way to go (load frame, apply edits, save image, etc.).

## Cached Frames

If the trajectory is small enough to be stored in memory, **Aten** reads in all frames at once. This means that:

+ Memory use is increased
+ Performance is optimal - speed of moving between frames is fast because all frames are in memory
+ Edits are retained - edits can be made to individual frames and will be remembered on moving to a different frame

The size of the cache can be adjusted either from the command line with the [`--cachelimit`](/aten/docs/cli/switches#cachelimit) switch or by setting the _cachelimit_ member of the [**Prefs**](/aten/docs/scripting/variabletypes/prefs) variable within [**Aten**](/aten/docs/scripting/variabletypes/aten). No check is made of the new cache limit with respect to the memory available on the machine on which **Aten** is running, so use with care.

In the current versions of **Aten**, the total trajectory size is determined from the size of the frame on disk, whereas it would be more appropriate to use the size of the frame in memory. This will change in a future release.


