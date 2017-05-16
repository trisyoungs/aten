/*
        *** AKF Model Plugin Functions
        *** src/plugins/io_akf/akf_funcs.cpp
        Copyright T. Youngs 2016-2017

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

#include "plugins/io_akf/akf.hui"
#include "model/model.h"

// Keyword Enum
QStringList AKFKeywords = QStringList() << "angle" << "atom" << "bond" << "cell" << "cellmatrix" << "distance" << "glyph" << "grid" << "title" << "torsion";
AKFModelPlugin::AKFKeyword AKFModelPlugin::akfKeyword(QString s)
{
	QString lowerQuery = s.toLower();
	for (int i=0; i< AKFKeywords.count(); ++i) if (lowerQuery == AKFKeywords.at(i)) return (AKFModelPlugin::AKFKeyword) i;

	return AKFModelPlugin::nKeywords;
}
QString AKFModelPlugin::akfKeyword(AKFModelPlugin::AKFKeyword keyword)
{
	return AKFKeywords.at(keyword);
}

// Glyph Keyword Enum
QStringList AKFGlyphKeywords = QStringList() << "endglyph" << "invisible" << "rotation" << "text" << "wireframe";
AKFModelPlugin::AKFGlyphKeyword AKFModelPlugin::akfGlyphKeyword(QString s)
{
	QString lowerQuery = s.toLower();
	for (int i=0; i< AKFGlyphKeywords.count(); ++i) if (lowerQuery == AKFGlyphKeywords.at(i)) return (AKFModelPlugin::AKFGlyphKeyword) i;

	return AKFModelPlugin::nGlyphKeywords;
}
QString AKFModelPlugin::akfGlyphKeyword(AKFGlyphKeyword keyword)
{
	return AKFGlyphKeywords.at(keyword);
}

// Grid Keyword Enum
QStringList AKFGridKeywords = QStringList() << "axes" << "data" << "endgrid" << "origin";
AKFModelPlugin::AKFGridKeyword AKFModelPlugin::akfGridKeyword(QString s)
{
	QString lowerQuery = s.toLower();
	for (int i=0; i< AKFGridKeywords.count(); ++i) if (lowerQuery == AKFGridKeywords.at(i)) return (AKFModelPlugin::AKFGridKeyword) i;

	return AKFModelPlugin::nGridKeywords;
}
QString AKFModelPlugin::akfGridKeyword(AKFGridKeyword keyword)
{
	return AKFGridKeywords.at(keyword);
}

// Constructor
AKFModelPlugin::AKFModelPlugin()
{
}

// Destructor
AKFModelPlugin::~AKFModelPlugin()
{
}

/*
 * Instance Handling
 */

// Return a copy of the plugin object
BasePluginInterface* AKFModelPlugin::makeCopy() const
{
	return new AKFModelPlugin;
}

/*
 * Definition
 */

// Return type of plugin
PluginTypes::PluginType AKFModelPlugin::type() const
{
	return PluginTypes::FilePlugin;
}

// Return category of plugin
int AKFModelPlugin::category() const
{
	return PluginTypes::ModelFilePlugin;
}

// Name of plugin
QString AKFModelPlugin::name() const
{
	return QString("Aten Keyword Format Files");
}

// Nickname of plugin
QString AKFModelPlugin::nickname() const
{
	return QString("akf");
}

// Return whether the plugin is enabled
bool AKFModelPlugin::enabled() const
{
	return true;
}

// Description (long name) of plugin
QString AKFModelPlugin::description() const
{
	return QString("Import/export for Aten Keyword Format files");
}

// Related file extensions
QStringList AKFModelPlugin::extensions() const
{
	return QStringList() << "akf";
}

// Exact names
QStringList AKFModelPlugin::exactNames() const
{
	return QStringList();
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool AKFModelPlugin::canImport() const
{
	return true;
}

// Import data from the specified file
bool AKFModelPlugin::importData()
{
	int n, nData;
	double data;
	AKFModelPlugin::AKFKeyword keyword;
	AKFModelPlugin::AKFGlyphKeyword glyphKeyword;
	AKFModelPlugin::AKFGridKeyword gridKeyword;
	Matrix matrix;
	Atom* i;
	Glyph* glyph;
	Grid* grid;
	Model* targetModel = createModel(fileParser_.filename());

	// Read file we run out of data
	while (!fileParser_.eofOrBlank())
	{
		// Parse line into keyword and 'rest of line'
		if (!fileParser_.parseLine(Parser::SkipBlanks)) break;

		// First argument is the keyword
		keyword = AKFModelPlugin::akfKeyword(fileParser_.argc(0));
		switch (keyword)
		{
			case (AKFModelPlugin::AngleKeyword):
				targetModel->addAngleMeasurement(fileParser_.argi(1)-1, fileParser_.argi(2)-1, fileParser_.argi(3)-1, true);
				break;
			case (AKFModelPlugin::AtomKeyword):
				i = createAtom(targetModel, fileParser_.argc(2), fileParser_.arg3d(3));
				i->setCharge(fileParser_.argd(6));
				if (fileParser_.hasArg(7)) i->setStyle(Prefs::drawStyle(fileParser_.argc(7)));
				if (fileParser_.hasArg(11)) i->setColour(fileParser_.argd(8), fileParser_.argd(9), fileParser_.argd(10), fileParser_.argd(11));
				if (fileParser_.hasArg(12)) i->setPositionFixed(fileParser_.argb(12));
				if (fileParser_.hasArg(13) && (fileParser_.argc(13) != "*"))
				{
					ForcefieldAtom* ffa = targetModel->addAtomName(i->element(), fileParser_.argc(13));
					i->setType(ffa);
				}
				break;
			case (AKFModelPlugin::BondKeyword):
				targetModel->bondAtoms(fileParser_.argi(1)-1, fileParser_.argi(2)-1, Bond::bondType(fileParser_.argc(3)));
				break;
			case (AKFModelPlugin::CellKeyword):
				targetModel->setCell(fileParser_.arg3d(1), fileParser_.arg3d(4));
				break;
			case (AKFModelPlugin::CellMatrixKeyword):
				matrix.setColumn(0, fileParser_.arg3d(1), 0.0);
				matrix.setColumn(1, fileParser_.arg3d(4), 0.0);
				matrix.setColumn(2, fileParser_.arg3d(7), 0.0);
				targetModel->setCell(matrix);
				break;
			case (AKFModelPlugin::DistanceKeyword):
				targetModel->addDistanceMeasurement(fileParser_.argi(1)-1, fileParser_.argi(2)-1, true);
				break;
			case (AKFModelPlugin::GlyphKeyword):
				glyph = targetModel->addGlyph(Glyph::glyphType(fileParser_.argc(1)));
				// First 'n' lines (specified in argument 2) are the 'n' data points for the glyph
				// Format:   <DataNo> <AtomID or 0 for vector> <AtomData Type> <vx> <vy> <vz> <r> <g> <b> <a>
				nData = fileParser_.argi(2);
				for (n=0; n<nData; ++n)
				{
					if (!fileParser_.parseLine()) break;
					if (fileParser_.argi(1) == 0) glyph->data(n)->setVector(fileParser_.arg3d(3));
					else glyph->data(n)->setAtom(targetModel->atom(fileParser_.argi(1) - 1));
					glyph->data(n)->setAtomData( (GlyphData::GlyphDataType) fileParser_.argi(2));
				}
				// Additional data now...
				do
				{
					if (!fileParser_.parseLine()) break;
					glyphKeyword = AKFModelPlugin::akfGlyphKeyword(fileParser_.argc(0));
					switch (glyphKeyword)
					{
						case (AKFModelPlugin::RotationKeyword):
							matrix.setIdentity();
							matrix.setColumn(0, fileParser_.arg3d(1), 0.0);
							matrix.setColumn(1, fileParser_.arg3d(4), 0.0);
							matrix.setColumn(2, fileParser_.arg3d(7), 0.0);
							glyph->setRotation(matrix);
							break;
						case (AKFModelPlugin::InvisibleKeyword):
							glyph->setVisible(false);
							break;
						case (AKFModelPlugin::TextKeyword):
							glyph->setText(fileParser_.argc(1));
							break;
						case (AKFModelPlugin::WireFrameKeyword):
							glyph->setSolid(true);
							break;
						default:
							if (glyphKeyword != AKFModelPlugin::EndGlyphKeyword) Messenger::print("Unrecognised glyph property '" + fileParser_.argc(0) + "' - ignored...");
					}
				} while (glyphKeyword != AKFModelPlugin::EndGlyphKeyword);
				break;
			case (AKFModelPlugin::GridKeyword):
				grid = targetModel->addGrid();
				grid->initialise(Grid::gridType(fileParser_.argc(1)), fileParser_.arg3i(2));
				do
				{
					if (!fileParser_.parseLine()) break;
					gridKeyword = AKFModelPlugin::akfGridKeyword(fileParser_.argc(0));
					switch (gridKeyword)
					{
						case (AKFModelPlugin::AxesKeyword):
							matrix.setIdentity();
							matrix.setColumn(0, fileParser_.arg3d(1), 0.0);
							matrix.setColumn(1, fileParser_.arg3d(4), 0.0);
							matrix.setColumn(2, fileParser_.arg3d(7), 0.0);
							grid->setAxes(matrix);
							break;
						case (AKFModelPlugin::OriginKeyword):
							grid->setOrigin(fileParser_.arg3d(1));
							break;
						case (AKFModelPlugin::DataKeyword):
							for (n=0; n<grid->nPoints(); ++n)
							{
								if (!fileParser_.readLineAsDouble(data))
								{
									Messenger::error("Couldn't read point %i from file.\n", n+1);
									break;
								}
								grid->setNextData(data);
							}
							break;
						default:
							if (gridKeyword != AKFModelPlugin::EndGridKeyword) Messenger::print("Unrecognised grid property '" + fileParser_.argc(0) + "' - ignored...");
							break;
					}
				} while (gridKeyword != AKFModelPlugin::EndGridKeyword);
				break;
			case (AKFModelPlugin::TitleKeyword):
				targetModel->setName(fileParser_.argc(1));
				break;
			case (AKFModelPlugin::TorsionKeyword):
				targetModel->addTorsionMeasurement(fileParser_.argi(1)-1, fileParser_.argi(2)-1, fileParser_.argi(3)-1, fileParser_.argi(4)-1, true);
				break;
			default:
				Messenger::print("Unrecognised keyword in akf file '" + fileParser_.filename() + "'. Ignoring it...");
				break;
		}
	}

// 	finaliseModel();

	return true;
}

// Return whether this plugin can export data
bool AKFModelPlugin::canExport() const
{
	return true;
}

// Export data to the specified file
bool AKFModelPlugin::exportData()
{
	// Get the current model pointer containing the data we are to export
	Model* sourceModel = targetModel();

	// Title
	if (!fileParser_.writeLine("title  '" + sourceModel->name() + "'")) return false;

	// Atom data
	for (Atom* i = sourceModel->atoms(); i != NULL; i = i->next) if (!fileParser_.writeLineF("atom   %i %s %f %f %f %f %s %f %f %f %f %i %s", i->id()+1, ElementMap::symbol(i->element()), i->r().x, i->r().y, i->r().z, i->charge(), Prefs::drawStyle(i->style()), i->colour()[0], i->colour()[1], i->colour()[2], i->colour()[3], i->isPositionFixed(), i->type() ? qPrintable(i->type()->name()) : "*")) return false;

	// Bond data
	for (Bond* b = sourceModel->bonds(); b != NULL; b = b->next) if (!fileParser_.writeLineF("bond   %i %i %s", b->atomI()->id()+1, b->atomJ()->id()+1, Bond::bondType(b->type()))) return false;

	// Cell data
	if (sourceModel->isPeriodic())
	{
		Matrix axes = sourceModel->cell().axes();
		if (!fileParser_.writeLineF("cellmatrix   %f %f %f %f %f %f %f %f %f",  axes[0], axes[1], axes[2], axes[4], axes[5], axes[6], axes[8], axes[9], axes[10])) return false;
	}

	// Measurements
	for (Measurement* m = sourceModel->distanceMeasurements(); m != NULL; m = m->next) if (!fileParser_.writeLineF("distance %i %i\t# %f", m->atom(0)->id()+1, m->atom(1)->id()+1,  m->value())) return false;
	for (Measurement* m = sourceModel->angleMeasurements(); m != NULL; m = m->next) if (!fileParser_.writeLineF("angle %i %i %i\t# %f", m->atom(0)->id()+1, m->atom(1)->id()+1, m->atom(2)->id()+1, m->value())) return false;
	for (Measurement* m = sourceModel->torsionMeasurements(); m != NULL; m = m->next) if (!fileParser_.writeLineF("torsion %i %i %i %i\t# %f", m->atom(0)->id()+1, m->atom(1)->id()+1, m->atom(2)->id()+1, m->atom(3)->id()+1, m->value())) return false;

	// Glyphs
	for (Glyph* g = sourceModel->glyphs(); g != NULL; g = g->next)
	{
		if (!fileParser_.writeLineF("glyph  %s  %i", Glyph::glyphType(g->type()), g->nData())) return false;
		// First 'n' lines are the 'n' data points for the glyph
		for (int n=0; n<g->nData(); ++n)
		{
			if (g->data(n)->atom())
			{
				if (!fileParser_.writeLineF("  %i  %i  %i  0.0 0.0 0.0  %f %f %f %f", n, g->data(n)->atom()->id()+1, g->data(n)->atomData(), g->data(n)->colour()[0], g->data(n)->colour()[1], g->data(n)->colour()[2], g->data(n)->colour()[3])) return false;
			}
			else if (!fileParser_.writeLineF("  %i  -1  -1  %f %f %f  %f %f %f %f", n, g->data(n)->vector().x, g->data(n)->vector().y, g->data(n)->vector().z, g->data(n)->colour()[0], g->data(n)->colour()[1], g->data(n)->colour()[2], g->data(n)->colour()[3])) return false;
		}
		// Additional data now...
		if (g->isRotated())
		{
			Matrix matrix = g->rotation();
			if (!fileParser_.writeLineF("  rotation  %f %f %f %f %f %f %f %f %f", matrix[0], matrix[1], matrix[2], matrix[4], matrix[5], matrix[6], matrix[8], matrix[9], matrix[10])) return false;
		}
		if (!g->isVisible()) if (!fileParser_.writeLineF("  invisible")) return false;
		if (!g->text().isEmpty()) if (!fileParser_.writeLine("  text  '" + g->text() + "'")) return false;
		if (!g->isSolid()) if (!fileParser_.writeLineF("  wireframe")) return false;
		if (!fileParser_.writeLineF("endglyph")) return false;
	}

	// Grids
	for (Grid* g = sourceModel->grids(); g != NULL; g = g->next)
	{
		if (!fileParser_.writeLineF("grid  %s  %i  %i  %i", Grid::gridType(g->type()), g->nXYZ().x, g->nXYZ().y, g->nXYZ().z)) return false;
		Matrix axes = g->axes();
		if (!fileParser_.writeLineF("  axes       %f  %f  %f  %f  %f  %f  %f  %f  %f", axes[0], axes[1], axes[2], axes[4], axes[5], axes[6], axes[8], axes[9], axes[10])) return false;
		if (!fileParser_.writeLineF("  origin     %f  %f  %f", g->origin().x, g->origin().y, g->origin().z)) return false;
		if (!fileParser_.writeLineF("  data")) return false;
		int x, y, z;
		if (g->type() == Grid::RegularXYData)
		{
			for (x=0; x<g->nXYZ().x; ++x)
				for (y=0; y<g->nXYZ().y; ++y) if (!fileParser_.writeLineF("    %f", g->data2d()[x][y])) return false;
		}
		else if (g->type() == Grid::RegularXYZData)
		{
			for (x=0; x<g->nXYZ().x; ++x)
				for (y=0; y<g->nXYZ().y; ++y)
					for (z=0; z<g->nXYZ().z; ++z) if (!fileParser_.writeLineF("    %f", g->data3d()[x][y][z])) return false;
		}
		if (!fileParser_.writeLineF("endgrid")) return false;
	}

	return true;
}

// Import next partial data chunk
bool AKFModelPlugin::importNextPart()
{
	return false;
}

// Skip next partial data chunk
bool AKFModelPlugin::skipNextPart()
{
	return false;
}

/*
 * Options
 */

// Return whether the plugin has import options
bool AKFModelPlugin::hasImportOptions() const
{
	return false;
}

// Show import options dialog
bool AKFModelPlugin::showImportOptionsDialog(KVMap& targetOptions) const
{
	return false;
}

// Return whether the plugin has export options
bool AKFModelPlugin::hasExportOptions() const
{
	return false;
}

// Show export options dialog
bool AKFModelPlugin::showExportOptionsDialog(KVMap& targetOptions) const
{
	return false;
}

 
