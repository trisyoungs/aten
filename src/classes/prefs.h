/*
	*** Preferences storage
	*** src/base/prefs.h
	Copyright T. Youngs 2007-2010

	This file is part of Aten.

	Aten is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	Aten is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with Aten.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef ATEN_PREFS_H
#define ATEN_PREFS_H

#include "ff/forms.h"
#include "base/atom.h"
#include "base/doubleexp.h"
#include "base/elements.h"
#include "classes/colourscale.h"
#include "base/dnchar.h"

// Forward declarations
class Cell;

// Prefs
class Prefs
{
	/*
	// Enumerations
	*/
	public:
	// Mouse buttons
	enum MouseButton { LeftButton, MiddleButton, RightButton, WheelButton, nMouseButtons };
	static MouseButton mouseButton(const char *name, bool reporterror = 0);
	static const char *mouseButton(MouseButton);
	// Mouse Actions
	enum MouseAction { NoAction, RotateAction, TranslateAction, InteractAction, ZoomAction, ZRotateAction, nMouseActions };
	static MouseAction mouseAction(const char *name, bool reporterror = 0);
	static const char *mouseAction(MouseAction);
	// Modifier keys
	enum ModifierKey { ShiftKey, CtrlKey, AltKey, nModifierKeys };
	static ModifierKey modifierKey(const char *name, bool reporterror = 0);
	static const char *modifierKey(ModifierKey);
	// Modifier actions
	enum KeyAction { NoKeyAction, ManipulateKeyAction, ZrotateKeyAction, nKeyActions };
	static KeyAction keyAction(const char *name, bool reporterror = 0);
	static const char *keyAction(KeyAction);
	// Property/Object Colours
	enum ObjectColour { AromaticRingColour, BackgroundColour, FixedAtomColour, GlobeAxesColour, GlobeColour, GlyphDefaultColour, SpecularColour, TextColour, UnitCellAxesColour, UnitCellColour, VibrationArrowColour, nObjectColours };
	static const char *objectColour(ObjectColour);
	static const char *objectColourName(ObjectColour);
	static ObjectColour objectColour(const char *name, bool reporterror = 0);
	// Energy Units
	enum EnergyUnit { Joules, KiloJoules, Calories, KiloCalories, Kelvin, ElectronVolts, Hartree, nEnergyUnits };
	static const char *energyUnit(EnergyUnit);
	static EnergyUnit energyUnit(const char *name, bool reporterror = 0);
	// Density calculation units
	enum DensityUnit { GramsPerCm, AtomsPerAngstrom, nDensityUnits };
	static const char *densityUnit(DensityUnit);
	static DensityUnit densityUnit(const char *name, bool reporterror = 0);
	// View Objects
	enum ViewObject { ViewAtoms, ViewCell, ViewCellAxes, ViewCellRepeat, ViewForceArrows, ViewGlobe, ViewLabels, ViewMeasurements, ViewRegions, ViewSurfaces, nViewObjects };
	static ViewObject viewObject(const char *name, bool reporterror = 0);
	static const char *viewObject(ViewObject);
	// Atom colouring scheme
	enum ColouringScheme { ChargeScheme, ElementScheme, ForceScheme, VelocityScheme, CustomScheme, nColouringSchemes };
	static ColouringScheme colouringScheme(const char *name, bool reporterror = 0);
	static const char *colouringScheme(ColouringScheme cs);
	// Filter override switches
	enum FilterSwitch { SwitchAsFilter, SwitchOff, SwitchOn };
	// Drawing guide geometry
	enum GuideGeometry { SquareGuide, HexagonalGuide, nGuideGeometries };
	// Spotlight Components
	enum ColourComponent { AmbientComponent, DiffuseComponent, SpecularComponent, nColourComponents };

	public:
	// Constructor
	Prefs();
	// Load preferences from file
	bool load(const char *filename);
	// Save preferences to file
	bool save(const char *filename);
	// Friend class
	friend class PreferencesVariable;


	/*
	// Rendering - View Objects
	*/
	private:
	// List of visibilities of renderable objects on screen and off-screen (on image)
	int screenObjects_, offScreenObjects_;
	// Repeat units in positive xyz directions
	Vec3<int> repeatCellsPos_;
	// Repeat units in negative xyz directions
	Vec3<int> repeatCellsNeg_;
	// Size in pixels of the viewport to draw the rotation globe in.
	int globeSize_;
	// Rendering style of models
	Atom::DrawStyle renderStyle_;
	// General quality of primitives
	int primitiveQuality_;
	// Whether to use separate primitive quality for saved images
	bool reusePrimitiveQuality_;
	// General quality of primitives on saved images
	int imagePrimitiveQuality_;
	// Number of levels of detail for rendering primitives
	int levelsOfDetail_;
	// Level of detail start z-distance
	double levelOfDetailStartZ_;
	// Level of detail slice 'width'
	double levelOfDetailWidth_;
	// Whether transparency correction is enabled
	bool transparencyCorrect_;
	// Number of bins to use in transparency sorting
	int transparencyNBins_;
	// Starting Z-depth of transparency bins
	double transparencyBinStartZ_;
	// Width of individual transparency Z-bin
	double transparencyBinWidth_;

	public:
	// Set the visibility of an object on-screen
	void setVisibleOnScreen(ViewObject vo, bool b);
	// Set the visibility of an object off-screen
	void setVisibleOffScreen(ViewObject vo, bool b);
	// Return whether the specified object is visible (i.e. should be rendered)
	bool isVisibleOnScreen(ViewObject vo);
	// Return whether the specified object is visible (i.e. should be rendered) offscreen
	bool isVisibleOffScreen(ViewObject vo);
	// Return screenobjects bitvector
	int screenObjects() const;
	// Set screenobjects bitvector
	void setScreenObjects(int i);
	// Return screenobjects bitvector
	int offScreenObjects() const;
	// Set imageobjects bitvector
	void setOffScreenObjects(int i);
	// Return the styled radius of an atom calculated from the element and draw style
	double styleRadius(Atom*) const;
	// Set the drawing style of models
	void setRenderStyle(Atom::DrawStyle ds);
	// Return the current drawing style of models
	Atom::DrawStyle renderStyle() const;
	// Return the current rotation globe size in pixels
	int globeSize() const;
	// Set the current rotation globe size in pixels
	void setGlobeSize(int i);
	// Set positive repeat cell value
	void setRepeatCellsPos(int i, int r);
	// Get positive repeat cell value
	int repeatCellsPos(int i) const;
	// Set negative repeat cell value
	void setRepeatCellsNeg(int i, int r);
	// Get negative repeat cell value
	int repeatCellsNeg(int i) const;
	// Sets the general primitive quality
	void setPrimitiveQuality(int n);
	// Return the current primitive quality
	int primitiveQuality() const;
	// Set whether to use separate primitive quality for saved images
	void setReusePrimitiveQuality(bool b);
	// Whether to use separate primitive quality for saved images
	bool reusePrimitiveQuality() const;
	// Sets the saved image primitive quality
	void setImagePrimitiveQuality(int n);
	// Return the current save image primitive quality
	int imagePrimitiveQuality() const;
	// Set number of levels of detail for rendering primitives
	void setLevelsOfDetail(int n);
	// Return number of levels of detail for rendering primitives
	int levelsOfDetail();
	// Set level of detail starting z-distance
	void setLevelOfDetailStartZ(double z);
	// Return level of detail starting z-distance
	double levelOfDetailStartZ();
	// Set level of detail slice 'width'
	void setLevelOfDetailWidth(double width);
	// Return level of detail slice 'width'
	double levelOfDetailWidth();
	// Return whether transparency correction is enabled
	bool transparencyCorrect();
	// Return whether transparency correction is enabled
	void setTransparencyCorrect(bool b);
	// Return number of bins to use in transparency sorting
	int transparencyNBins();
	// Set number of bins to use in transparency sorting
	void setTransparencyNBins(int nbins);
	// Return starting Z-depth of transparency bins
	double transparencyBinStartZ();
	// Set starting Z-depth of transparency bins
	void setTransparencyBinStartZ(double startz);
	// Return width of individual transparency Z-bin
	double transparencyBinWidth();
	// Set width of individual transparency Z-bin
	void setTransparencyBinWidth(double width);


	/*
	// Rendering - Style
	*/
	private:
	// Atom colouring style
	Prefs::ColouringScheme colourScheme_;
	// Atom sizes / radii
	GLdouble atomStyleRadius_[Atom::nDrawStyles];
	// Bond radii
	GLdouble bondStyleRadius_[Atom::nDrawStyles];
	// Size scaling for atom selection transparency
	GLdouble selectionScale_;
	// Whether to use a perspective (TRUE) or orthographic (FALSE) projection
	bool perspective_;
	// Viewing angle for perspective projection
	GLdouble perspectiveFov_;
	// Whether the spotlight is on
	bool spotlightActive_;
	// Spotlight components
	double spotlightColour_[Prefs::nColourComponents][4];
	// Spotlight position
	double spotlightPosition_[4];

	public:
	// Sets the specified atom size to the given value
	void setAtomStyleRadius(Atom::DrawStyle ds, double f);
	// Return the specified atom radius
	GLdouble atomStyleRadius(Atom::DrawStyle ds) const;
	// Sets the bond radius used in Scaled and Sphere styles
	void setBondStyleRadius(Atom::DrawStyle ds, double f);
	// Return the bond radius used in Scaled and Sphere styles
	GLdouble bondStyleRadius(Atom::DrawStyle ds) const;
	// Sets the scale of selected atoms
	void setSelectionScale(double f);
	// Return the scale of selected atoms
	GLdouble selectionScale() const;
	// Return whether perspective viewing is enabled
	bool hasPerspective() const;
	// Sets perspective viewing on/off
	void setPerspective(bool b);
	// Set the perspective field of view angle
	void setPerspectiveFov(double fov);
	// Return the perspective field of view angle
	double perspectiveFov() const;
	// Set status of spotlight
	void setSpotlightActive(bool status);
	// Return status of spotlight
	bool spotlightActive() const;
	// Set spotlight colour component
	void setSpotlightColour(ColourComponent sc, int i, double value);
	void setSpotlightColour(ColourComponent sc, double r, double g, double b);
	// Return spotlight colour component
	double *spotlightColour(ColourComponent sc);
	// Return spotlight colour component in provided array
	void copySpotlightColour(ColourComponent sc, GLfloat *col);
	// Set spotlight position
	void setSpotlightPosition(double r, double g, double b);
	void setSpotlightPosition(int component, double f);
	// Return spotlight position
	double *spotlightPosition();
	// Return spotlight position in provided array
	void copySpotlightPosition(GLfloat *col);
	// Set atom colour scheme
	void setColourScheme(Prefs::ColouringScheme sc);
	// Return atom colour scheme
	Prefs::ColouringScheme colourScheme() const;
	// Set number of segments in colour scale
	void setScaleSegments(int nsegments);
	// Get number of segments in colour scale
	int nScaleSegments() const;


	/*
	// Rendering - Options
	*/
	private:
        // C-style format for distance label values
        Dnchar distanceLabelFormat_;
        // C-style format for angle label values
        Dnchar angleLabelFormat_;
	// Pointsize for labels
	int labelSize_;
	// Use QGlWidget::renderText (FALSE) or QPainter::drawText (TRUE) for labels etc.
	bool useNiceText_;
	// Flag to manually perform swapBuffers
	bool manualSwapBuffers_;
	// Flag to use framebuffer for image saving rather than the renderPixmap() method
	bool useFrameBuffer_;
	// Whether to use solid or dashed circles for aromatic ring rendering
	bool renderDashedAromatics_;
	// Mouse move event filter ratio
	int mouseMoveFilter_;

	public:
        // Set C-style format for distance label values
        void setDistanceLabelFormat(const char *format);
        // Return C-style format for distance label values
        const char *distanceLabelFormat();
        // Set C-style format for angle label values
        void setAngleLabelFormat(const char *format);
        // Return C-style format for angle label values
        const char *angleLabelFormat();
	// Set the pointsize of labels in the model
	void setLabelSize(int size);
	// Return the current label pointsize
	int labelSize() const;
	// Set whether to use nice text rendering
	void setUseNiceText(bool b);
	// Return whether to use nice text rendering
	bool useNiceText() const;
	// Set manual swapbuffers
	void setManualSwapBuffers(bool on);
	// Return whether manual buffer swapping is enabled
	bool manualSwapBuffers() const;
	// Set usage of framebuffer in image saving
	void setUseFrameBuffer(bool on);
	// Return whether to use framebuffer for image saving
	bool useFrameBuffer() const;
	// Return whether to use solid or dashed circles for aromatic ring rendering
	bool renderDashedAromatics();
	// Set  whether to use solid or dashed circles for aromatic ring rendering
	void setRenderDashedAromatics(bool b);
	// Return mouse move event filter ratio
	int mouseMoveFilter();
	// Set mouse move event filter ratio
	void setMouseMoveFilter(int i);

	/*
	// GL Options
	*/
	private:
	// Depth cue flag
	bool depthCue_;
	// Line aliasing flag
	bool lineAliasing_;
	// Multisampling flag
	bool multiSampling_;
	// Polygon aliasing flag
	bool polygonAliasing_;
	// Backface culling flag
	bool backfaceCulling_;
	// Shininess of 3D objects
	GLint shininess_;
	// Fog start and finish depths
	GLint depthNear_, depthFar_;
	// Near and far clipping planes for glPerspective() and glFrustum();
	GLdouble clipNear_, clipFar_;

	public:
	// Set status of fog (depth cueing)
	void setDepthCue(bool status);
	// Return status of depth cueing
	bool depthCue() const;
	// Sets the start depth of depth cueing
	void setDepthNear(int i);
	// Return depth cue start depth
	GLint depthNear() const;
	// Sets the end depth of depth cueing
	void setDepthFar(int i);
	// Return depth cue end depth
	GLint depthFar() const;
	// Set status of line aliasing
	void setLineAliasing(bool status);
	// Return status of line aliasing
	bool lineAliasing() const;
	// Set status of polygon aliasing
	void setPolygonAliasing(bool status);
	// Return status of polygon aliasing
	bool polygonAliasing() const;
	// Set status of multisampling
	void setMultiSampling(bool status);
	// Return status of multisampling
	bool multiSampling() const;
	// Set status of backface culling
	void setBackfaceCulling(bool status);
	// Return status of backface culling
	bool backfaceCulling() const;
	// Return the Z depth of the near clipping plane
	GLdouble clipNear() const;
	// Set the Z-depth of the near clipping plane
	void setClipNear(double d);
	// Return the Z depth of the far clipping plane
	GLdouble clipFar() const;
	// Set the Z-depth of the far clipping plane
	void setClipFar(double d);
	// Sets the shininess of GL objects
	void setShininess(int n);
	// Return the current shininess of GL objects
	GLint shininess() const;


	/*
	// Colours
	*/
	private:
	// RGB colour values
	double colours_[Prefs::nObjectColours][4];

	public:
	// Set the specified colour to the integer RGBA values supplied
	void setColour(ObjectColour c, double r, double g, double b, double a);
	// Set the supplied element of the specified colour
	void setColour(ObjectColour c, int i, double value);
	// Copy the specified colour
	void copyColour(ObjectColour c, GLfloat *col) const;
	// Return a pointer to the specified colour
	double *colour(ObjectColour c);
	// User-definable colour scales
	ColourScale colourScale[10];


	/*
	// File Preferences
	*/
	private:
	// Recalculate bonding when model has loaded
	FilterSwitch bondOnLoad_;
	// Centre non-periodic models on load
	FilterSwitch centreOnLoad_;
	// Fold atomic positions after model load
	FilterSwitch foldOnLoad_;
	// Whether to apply symmetry operators to get crystal packing on load
	FilterSwitch packOnLoad_;
	// Whether to load in all coordinate sets from a file
	bool loadAllCoords_;
	// Convert coordinates from Bohr to Angstrom on load
	bool coordsInBohr_;
	// Size limit (kbytes) for caching trajectory frames
	int cacheLimit_;
	// Type of name->Z mapping to use
	ElementMap::ZMapType zMapType_;
	// Whether the zmapping type is currently fixed (i.e. unchangeable)
	bool fixedZMapType_;
	// Whether to retain file atom type names on load (in a new forcefield)
	bool keepNames_;
	// Whether to retain and fix atom types on load (when converting names using forcefield types)
	bool keepTypes_;
	// Whether to retain view when GUI starts (i.e. don't reset it)
	bool keepView_;

	public:
	// Sets whether to calculate bonding on model load
	void setBondOnLoad(FilterSwitch s);
	// Whether bonding should be recalculated on model load
	FilterSwitch bondOnLoad() const;
	// Sets whether to centre molecule on load
	void setCentreOnLoad(FilterSwitch s);
	// Whether molecule should be centred on model load
	FilterSwitch centreOnLoad() const;
	// Sets whether to fold atomic positions after model load
	void setFoldOnLoad(FilterSwitch s);
	// Whether atoms should be folded after model load
	FilterSwitch foldOnLoad() const;
	// Sets whether to apply symmetry operators (pack) on load
	void setPackOnLoad(FilterSwitch s);
	// Whether atoms should be packed (with symmetry operations) after model load
	FilterSwitch packOnLoad() const;
	// Set the cache limit (in kb) for trajectory files
	void setCacheLimit(int i);
	// Return the cache limit for trajectory files
	int cacheLimit() const;
	// Sets the style of element conversion to use
	void setZMapType(ElementMap::ZMapType i);
	void setZMapType(ElementMap::ZMapType i, bool fixed);
	// Return the style of element conversion in use
	ElementMap::ZMapType zMapType() const;
	// Sets whether to convert coords from Bohr to Angstrom on load
	void setCoordsInBohr(bool b);
	// Whether coordinates should be converted from Bohr to Angstrom
	bool coordsInBohr() const;
	// Set whether to keep file type names on load
	void setKeepNames(bool b);
	// Return whether to keep file type names on load
	bool keepNames() const;
	// Set whether to assign and fix type names on load
	void setKeepTypes(bool b);
	// Return whether to assign and fix type names on load
	bool keepTypes() const;
	// Set whether to keep view on GUI start
	void setKeepView(bool b);
	// Return whether to keep view on GUI start
	bool keepView() const;


	/*
	// Editing Preferences
	*/
	private:
	// Bonding tolerance for automatic calculation
	double bondTolerance_;
	// Depth for drawing guide
	double drawDepth_;
	// Spacing of grid on drawing guide
	double guideSpacing_;
	// Extent (+- guide_spacing in xy plane) of drawing guide 
	int guideExtent_;
	// Number of ticks between gridpoints of guide
	int guideTicks_;
	// Whether to show the drawing guide
	bool showGuide_;
	// Geometry of the grid in the drawing guide
	Prefs::GuideGeometry guideShape_;
	// Hydrogen add distance
	double hydrogenDistance_;

	public:
	// Sets the bonding tolerance
	void setBondTolerance(double v);
	// Return the bonding tolerance for automatic calculation
	double bondTolerance() const;
	// Sets the position of the drawing guide
	void setDrawDepth(double v);
	// Return the current position of the drawing guide
	double drawDepth() const;
	// Spacing of grid on drawing guide
	void setGuideSpacing(double spacing);
	// Spacing of grid on drawing guide
	double guideSpacing() const;
	// Extent (+- guide_spacing in xy plane) of drawing guide 
	void setGuideExtent(int extent);
	// Return extent (+- guide_spacing in xy plane) of drawing guide 
	int guideExtent() const;
	// Number of ticks between gridpoints of guide
	void setGuideTicks(int nticks);
	// Number of ticks between gridpoints of guide
	int guideTicks() const;
	// Sets the visibility of the drawing guide
	void setGuideVisible(bool b);
	// Return whether the draw guide is visible
	bool isGuideVisible() const;
	// Sets the shape of the drawing guide
	void setGuideShape(Prefs::GuideGeometry g);
	// Return guide shape
	Prefs::GuideGeometry guideShape() const;
	// Set hydrogen add distance
	void setHydrogenDistance(double d);
	// Return hydrogen add distance
	double hydrogenDistance() const;


	/*
	// Interaction Preferences
	*/
	private:
	// User-definable mouse button actions
	MouseAction mouseAction_[Prefs::nMouseButtons];
	Dnchar mouseActionTexts_[Prefs::nMouseButtons];
	// User-definable key modifier actions
	KeyAction keyAction_[Prefs::nModifierKeys];
	Dnchar keyActionTexts_[Prefs::nModifierKeys];
	// Zoom 'throttle'
	double zoomThrottle_;

	public:
	// Sets the action for the specified mouse button
	void setMouseAction(MouseButton mb, MouseAction ma);
	// Return the action associated with the specified mouse button
	MouseAction mouseAction(MouseButton mb) const;
	// Sets the modifier key for the specified action
	void setKeyAction(ModifierKey mk, KeyAction ka);
	// Return the action associated with the specified keymod button
	KeyAction keyAction(ModifierKey mk) const;
	// Sets the zoom throttle
	void setZoomThrottle(double throtvalue);
	// Returns the zoom throttle
	double zoomThrottle() const;


	/*
	// General Program / Method Preferences
	*/
	private:
	// Main modelview update and energy output frequencies
	int modelUpdate_, energyUpdate_;
	// Maximum ring size in ring search algorithm
	int maxRingSize_;
	// Maximum number of rings to detect per pattern
	int maxRings_;
	// Maximum number of bonding cuboids in each direction
	int maxCuboids_;
	// Whether to fold atoms before replication
	bool replicateFold_;
	// Whether to trim atoms after replication
	bool replicateTrim_;
	// Maximum number of undo levels (-1 for unlimited)
	int maxUndoLevels_;
	// List of common drawing elements to put in SelectElement dialog
	Dnchar commonElements_;
	// Whether to load Qt window/toolbar settings on startup
	bool loadQtSettings_;
	// Maximum distance allowed between consecutive improper torsion atoms
	double maxImproperDist_;
	// Whether to augment when rebonding
	bool augmentAfterRebond_;
	// Force spacegroups that are in hexagonal basis to be in rhombohedral basis
	bool forceRhombohedral_;
	// Display warning for version 1.5 (r1056) newness
	bool warning1056_;
	// Whether to load filters on startup
	bool loadFilters_;
	// Whether to load includes on startup
	bool loadIncludes_;
	// Whether to load fragments on startup
	bool loadFragments_;
	// Whether to generate icons for loaded fragments
	bool generateFragmentIcons_;

	public:
	// Set the model update frequency
	void setModelUpdate(int n);
	// Return the model update frequency
	int modelUpdate() const;
	// Return whether to update the model, given the cycle number
	bool shouldUpdateModel(int n);
	// Set the energy update frequency
	void setEnergyUpdate(int n);
	// Return the energy update frequency
	int energyUpdate() const;
	// Return whether to update the energy, given the cycle number
	bool shouldUpdateEnergy(int n);
	// Return the maximum ring size allowed
	int maxRingSize() const;
	// Set the maximum ring size allowed
	void setMaxRingSize(int i);
	// Return the maximum number of rings to detect per pattern
	int maxRings() const;
	// Set the maximum number of rings to detect per pattern
	void setMaxRings(int i);
	// Return maximum number of bonding cuboids in each direction
	int maxCuboids() const;
	// Set maximum number of bonding cuboids in each direction
	void setMaxCuboids(int i);
	// Set whether to fold atoms before replication
	void setReplicateFold(bool b);
	// Return whether to fold atoms before replication
	bool replicateFold() const;
	// Set whether to trim atoms after replication
	void setReplicateTrim(bool b);
	// Return whether to trim atoms after replication
	bool replicateTrim() const;
	// Set the maximum number of undo levels allowed
	void setMaxUndoLevels(int n);
	// Return the maximum number of undo levels allowed
	int maxUndoLevels() const;
	// Set list of common elements in SelectElement dialog
	void setCommonElements(const char *s);
	// Return list of common elements to use in SelectElement dialog
	const char *commonElements() const;
	// Return whether to load Qt window/toolbar settings on startup
	bool loadQtSettings();
	// set whether to load Qt window/toolbar settings on startup
	void setLoadQtSettings(bool b);
	// Return maximum distance allowed between consecutive improper torsion atoms
	double maxImproperDist() const;
	// Set maximum distance allowed between consecutive improper torsion atoms
	void setMaxImproperDist(double r);
	// Return whether to augment when rebonding
	bool augmentAfterRebond() const;
	// Set whether to augment when rebonding
	void setAugmentAfterRebond(bool b);
	// Return whether rhombohedral (over hexagonal) spacegroup basis is to be forced
	bool forceRhombohedral() const;
	// Set whether rhombohedral (over hexagonal) spacegroup basis is to be forced
	void setForceRhombohedral(bool b);
	// Whether to display warning for version 1.5 (r1056) newness
	bool warning1056() const;
	// Set whether to display warning for version 1.5 (r1056) newness
	void setWarning1056(bool b);
	// Whether to load filters on startup
	bool loadFilters() const;
	// Set whether to load filters on startup
	void setLoadFilters(bool b);
	// Whether to load includes on startup
	bool loadIncludes() const;
	// Set whether to load includes on startup
	void setLoadIncludes(bool b);
	// Whether to load fragments on startup
	bool loadFragments() const;
	// Set whether to load fragments on startup
	void setLoadFragments(bool b);
	// Whether to generate icons for loaded fragments
	bool generateFragmentIcons() const;
	// Set whether to generate icons for loaded fragments
	void setGenerateFragmentIcons(bool b);


	/*
	// Units and Conversion
	*/
	private:
	// Internal density units to use
	DensityUnit densityUnit_;
	// Internal energy units to use for forcefield storage, energy calculation etc.
	EnergyUnit energyUnit_;
	// Energy unit to use for automatic conversion of forcefield parameters when accessed through filters (if any)
	EnergyUnit autoConversionUnit_;
	// Conversion factors for energy units
	double energyConversions_[Prefs::nEnergyUnits];
	// Factor to convert from atomic units to internal units
	double elecConvert_;

	public:
	// Sets the current internal energy unit
	void setEnergyUnit(Prefs::EnergyUnit eu);
	// Return the working energy units
	EnergyUnit energyUnit() const;
	// Set the density unit to use
	void setDensityUnit(Prefs::DensityUnit du);
	// Return the current density units to use
	DensityUnit densityUnit() const;
	// Set energy unit to use for automatic conversion of forcefield parameters when accessed through filters
	void setAutoConversionUnit(Prefs::EnergyUnit eu);
	// Return energy unit to use for automatic conversion of forcefield parameters when accessed through filters
	EnergyUnit autoConversionUnit() const;
	// Convert the units of the given quantity to the specified unit (default is current internal unit)
	double convertEnergy(double energy, Prefs::EnergyUnit fromUnit, Prefs::EnergyUnit toUnit = Prefs::nEnergyUnits) const;
	// Return the electrostastic energy conversion factor
	double elecConvert() const;
	// Return the gas constant in the current unit of energy
	double gasConstant() const;


	/*
	// Forcefield / Expression
	*/
	private:
	// Method of electrostatic calculation
	Electrostatics::ElecMethod electrostaticsMethod_;
	// Whether to calculate VDW interactions
	bool calculateVdw_;
	// Whether to calculate electrostatic interactions
	bool calculateElec_;
	// Whether to calculate intramolecular interactions
	bool calculateIntra_;
	// Ewald sum extent
	Vec3<int> ewaldKMax_;
	// Ewald sum gaussian width and (for auto option) precision
	double ewaldAlpha_;
	// Ewald sum precision for automatic parameter estimation
	DoubleExp ewaldPrecision_;
	// Cutoff distances for VDW and electrostatics
	double vdwCutoff_, elecCutoff_;
	// Scale factor for VDW radii (used in disorder build)
	double vdwScale_;
	// Whether the automatic Ewald setup is valid
	bool validEwaldAuto_;
	// Combination rule equations
	Dnchar combinationRules_[Combine::nCombinationRules];


	public:
	// Sets the electrostatic model to use in energy/force calculation
	void setElectrostaticsMethod(Electrostatics::ElecMethod em);
	// Return the type of electrostatic treatment to use
	Electrostatics::ElecMethod electrostaticsMethod() const;
	// Sets whether to calculate intramolecular interactions
	void setCalculateIntra(bool b);
	// Return whether to calculate intramolocular interactions
	bool calculateIntra() const;
	// Sets whether to calculate VDW interactions
	void setCalculateVdw(bool b);
	// Return whether to calculate VDW interactions
	bool calculateVdw() const;
	// Sets whether to calculate electrostatic interactions
	void setCalculateElec(bool b);
	// Return whether to calculate electrostatic interactions
	bool calculateElec() const;
	// Sets the Ewald k-vector extents
	void setEwaldKMax(int element, int i);
	void setEwaldKMax(int a, int b, int c);
	void setEwaldKMax(Vec3<int> v);
	// Return the Ewald k-vector extents
	Vec3<int> ewaldKMax() const;
	// Return the Ewald precision (structure)
	DoubleExp &ewaldPrecision();
	// Set the Gaussian width to use in the Ewald sum
	void setEwaldAlpha(double d);
	// Return the Ewald alpha value
	double ewaldAlpha() const;
	// Set the short-range and electrostatic cutoffs
	void setCutoffs(double vcut, double ecut);
	// Estimate Ewald sum parameters for the current unit cell
	void estimateEwaldParameters(Cell*);
	// Return the validity of automatic Ewald params (invalidated on cell change)
	bool hasValidEwaldAuto() const;
	// Flag the Ewald auto params as invalid
	void invalidateEwaldAuto();
	// Sets the VDW cutoff radius to use
	void setVdwCutoff(double d);
	// Return the VDW cutoff radius
	double vdwCutoff() const;
	// Sets the electrostatic cutoff radius to use
	void setElecCutoff(double d);
	// Return the electrostatic cutoff radius
	double elecCutoff() const;
	// Sets the vdw radius scaling factor
	void setVdwScale(double d);
	// Return the VDW radius scaling factor
	double vdwScale() const;
	// Set combination rule equation
	void setCombinationRule(Combine::CombinationRule cr, const char *s);
	// Return combination rule equation
	const char *combinationRule(Combine::CombinationRule cr) const;


	/*
	// External Programs
	*/
	private:
	// Location of temporary directory
	Dnchar tempDir_;
	// Location of MOPAC executable
	Dnchar mopacExe_;

	public:
	// Set temp directory
	void setTempDir(const char *path);
	// Return the temp directory path
	const char *tempDir() const;
	// Location of MOPAC executable
	void setMopacExe(const char *path);
	// Return the location of the MOPAC executable
	const char *mopacExe() const;
};

extern Prefs prefs;

#endif
