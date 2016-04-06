---
title: Prefs
visible: true
taxonomy:
  category: docs
template: manpage
docroot: /aten/docs
header_class: alt
---

The [**Prefs**](/aten/docs/scripting/variabletypes/prefs) type contains all preferences and settings information detailing the inner bits and pieces of Aten. It exists as a single instance, owned by and available through the [**Aten**](/aten/docs/scripting/variabletypes/aten) master type.
 
| Member | Type | RW | Description |
|--------|------|----|-------------|
| angleLabelFormat | **string** | • | The C-style format to use for the numerical value of angle labels |
| aromaticRingColour | **double**[4] | • | Colour of aromatic ring circles |
| atomStyleRadius | **double**[4] | • | The atom radii used for selection and rendering in the available [Drawing Styles](/aten/docs/enums/drawstyle) |
| backCull | **int** | • | Whether culling of backward-facing polygons should be performed |
| backGroundColour | **double**[4] | • | Background colour of the main canvas on which models are drawn |
| bondStyleRadius | **double**[4] | • | The bond radii used for selection and rendering in the available [Drawing Styles](/aten/docs/enums/drawstyle) |
| bondTolerance | **double** | • | Tolerance used in automatic calculation of bonds between atoms |
| cacheLimit | **int** | • | The trajectory cache size (in kilobytes) - trajectory files calculated to have more than this amount of data will not be cached in memory |
| calculateIntra | **int** | • | Controls whether intramolecular contributions to the energy/forces are calculated |
| calculateVdw | **int** | • | Controls whether short-range van der Waals contributions to the energy/forces are calculated |
| chargeLabelFormat | **string** | • | C-style format for atomic charge labels |
| clipFar | **double** | • | The far clipping distance used when rendering |
| clipNear | **double** | • | The near clipping distance used when rendering |
| colourScales | [**ColourScale**][10](/aten/docs/scripting/variabletypes/colourscale) | | List of colourscales |
| colourScheme | **string** | • | The current [Colour Scheme](/aten/docs/enums/colourscheme) used to colour atoms and bonds |
| combinationRule | **string** | • | Lennard-Jones parameter combination rule equations. See [Combination Rules](/aten/docs/enums/combinationrule) for a list |
| commonElements | **string** | • | Comma-separated list of common elements that appear in the Select Element dialog |
| correctTransparentGrids | **int** | • | Whether to automatically recreate transparent grid surfaces after view manipulation |
| dashedAromatics | **int** | • | Whether to render solid or dashed rings for aromatics |
| densityUnit | **string** | • | The unit of density to used when displaying cell densities |
| depthCue | **int** | • | Enables/disables depth cueing |
| depthFar | **int** | • | The far fog distance used when rendering (if depth cueing is enabled) |
| depthNear | **int** | • | The near fog distance used when rendering (if depth cueing is enabled) |
| distanceLabelFormat | **string** | • | The C-style format to use for the numerical value of distance labels |
| elecCutoff | **double** | • | The electrostatic cutoff distance |
| elecMethod | **string** | • | The method of electrostatic energy/force calculation |
| energyUnit | **double** | • | Set the unit of energy to use |
| ewaldAlpha | **double** | • | Convergence parameter in Ewald sum |
| ewaldKMax | **int**[3] | • | Vector of Ewald reciprocal space vector limits (kmax) |
| ewaldPrecision | **double** | • | Precision parameter to use when generating parameters in EwaldAuto |
| forceRhombohedral | **int** | • | For spacegroups that are detected to have a hexagonal basis, force packing to use generators in a rhombohedral basis |
| foregroundColour | **double**[4] | • | Colour of foreground items (e.g. rendered text) |
| globeAxesColour | **double**[4] | • | Colour of axis pointers on the rotation globe |
| globeColour | **double**[4] | • | Colour of the actual rotation globe |
| globeSize | **int** | • | Size, in pixels, of the rotation globe in the lower-right-hand corner of the screen |
| glyphColour | **double**[4] | • | Default colour of all created glyphs |
| hDistance | **double** | • | Default H-X bond distance to use when adding hydrogen atoms |
| imageQuality | **int** | • | The general rendering quality (i.e. number of triangles used to generate primitive objects) of the images, used if 'reusequality' is set to FALSE (otherwise the current 'quality' value is used). |
| keyAction | **string**[3] | • | Current actions of the modifier keys Shift, Ctrl, and Alt |
| labelSize | **int** | • | Font pointsize for label text |
| lineAliasing | **int** | • | Enables/disables line aliasing |
| maxRings | **int** | • | Maximum allowable number of rings to detect within any single pattern |
| maxRingSize | **int** | • | Maximum size of ring to detect when atom typing |
| maxUndo | **int** | • | Maximum number of undo levels remembered for each model (-1 = unlimited) |
| mopacExe | **string** | • | Location of MOPAC executable (including full path) |
| mouseAction | _string_[4] | • | Current actions of the Left, Middle, Right, and Wheel mouse buttons |
| mouseMoveFilter | **int** | • | Sets the degree to which mouse move events are filtered, with 1 being no filtering. Use this to reduce update lag on sluggish systems. |
| multiSampling | **int** | • | Enables/disables multisampling (hardware aliasing) |
| noQtSettings | **int** | • | Flag controlling whether OS-stored Qt settings are loaded on startup |
| partitionGrid | **int**[3] | • | Grid size to use for partitioning schemes |
| perspective | **int** | • | Whether perspective view is enabled |
| perspectiveFOV | **double** | • | Field of vision angle to use for perspective rendering |
| polygonAliasing | **int** | • | Enables/disables polygon aliasing |
| renderStyle | **string** | • | The current model drawing style |
| quality | **int** | • | The general rendering quality (i.e. number of triangles used to generate primitive objects) of the main view. Higher values give rounder atoms but make rendering slower. See also the ‘imagequality’ setting. |
| reuseQuality | **int** | • | Flag specifying whether to use the current rendering 'quality' value when saving images (FALSE) or the 'imagequality' value (TRUE). |
| selectionScale | **double** | • | Multiple of the standard atom radius to use for selection spheres |
| shininess | **int** | • | The shininess of atoms (value must be between 0 and 127 inclusive) |
| specularColour | **double**[4] | • | Colour of all specular reflections |
| spotlight | **int** | • | Whether the spotlight is on or off |
| spotlightAmbient | **double**[4] | • | The ambient colour component of the spotlight |
| spotlightDiffuse | **double**[4] | • | The diffuse colour component of the spotlight |
| spotlightPosition | **double**[4] | • | Spotlight coordinates (in Å) |
| spotlightSpecular | **double**[4] | • | The specular colour component of the spotlight |
| stickNormalWidth | **double** | • | Line width of unselected stick atoms |
| stickSelectedWidth | **double** | • | Line width of selected stick atoms |
| tempDir | **string** | • | Temporary (working) directory for some operations (e.g. execution of MOPAC jobs) |
| unitCellAxesColour | **double**[4] | • | Colour of unit cell axis pointers |
| unitCellColour | **double**[4] | • | Colour of unit cell |
| vdwCut | **double** | • | The VDW cutoff distance |
| vibrationColour | **double**[4] | • | Colour of vibration vector arrows |
| viewerFontFileName | **string** | • | Truetype font to use for text rendering (overrides internal font) |
| viewRotationGlobe | **int** | • | Whether to draw a rotation globe in the lower right-hand corner for each model |
| zoomThrottle | **double** | • | Zooming ‘throttle’ value used to calm down or increase the distance jumped when zooming with the mouse |


