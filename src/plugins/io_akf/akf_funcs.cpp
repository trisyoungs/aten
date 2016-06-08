/*
        *** AKF Plugin Functions
        *** src/plugins/io_akf/akf_funcs.cpp
        Copyright T. Youngs 2016-2016

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

#include "plugins/io_akf/akf.h"
#include "model/model.h"
#include "base/sysfunc.h"

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
AKFModelPlugin::AKFGridKeyword akfGridKeyword(QString s)
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
 * Core
 */

// Return a copy of the plugin object
FilePluginInterface* AKFModelPlugin::duplicate()
{
	return new AKFModelPlugin;
}

/*
 * AKF Model Import / Export Plugin
 */

// Return category of plugin
PluginTypes::FilePluginCategory AKFModelPlugin::category() const
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
bool AKFModelPlugin::canImport()
{
	return true;
}

// Import data from the specified file
bool AKFModelPlugin::importData(FileParser& parser, const KVMap standardOptions)
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
	Model* targetModel = createModel(parser.filename());

	// Read file we run out of data
	while (!parser.eofOrBlank())
	{
		// Parse line into keyword and 'rest of line'
		if (!parser.parseLine(Parser::SkipBlanks)) break;

		// First argument is the keyword
		keyword = AKFModelPlugin::akfKeyword(parser.argc(0));
		switch (keyword)
		{
			case (AKFModelPlugin::AngleKeyword):
				targetModel->addAngleMeasurement(parser.argi(1)-1, parser.argi(2)-1, parser.argi(3)-1, true);
				break;
			case (AKFModelPlugin::AtomKeyword):
				i = targetModel->addAtom(Elements().find(parser.argc(2)), parser.arg3d(3));
				i->setCharge(parser.argd(6));
				if (parser.hasArg(7)) i->setStyle(Prefs::drawStyle(parser.argc(7)));
				if (parser.hasArg(11)) i->setColour(parser.argd(8), parser.argd(9), parser.argd(10), parser.argd(11));
				if (parser.hasArg(12)) i->setPositionFixed(parser.argb(12));
				break;
			case (AKFModelPlugin::BondKeyword):
				targetModel->bondAtoms(parser.argi(1)-1, parser.argi(2)-1, Bond::bondType(parser.argc(3)));
				break;
			case (AKFModelPlugin::CellKeyword):
				targetModel->setCell(parser.arg3d(1), parser.arg3d(4));
				break;
			case (AKFModelPlugin::CellMatrixKeyword):
				matrix.setColumn(0, parser.arg3d(1), 0.0);
				matrix.setColumn(1, parser.arg3d(4), 0.0);
				matrix.setColumn(2, parser.arg3d(7), 0.0);
				targetModel->setCell(matrix);
				break;
			case (AKFModelPlugin::DistanceKeyword):
				targetModel->addDistanceMeasurement(parser.argi(1)-1, parser.argi(2)-1, true);
				break;
			case (AKFModelPlugin::GlyphKeyword):
				glyph = targetModel->addGlyph(Glyph::glyphType(parser.argc(1)));
				// First 'n' lines (specified in argument 2) are the 'n' data points for the glyph
				// Format:   <DataNo> <AtomID or 0 for vector> <AtomData Type> <vx> <vy> <vz> <r> <g> <b> <a>
				nData = parser.argi(2);
				for (n=0; n<nData; ++n)
				{
					if (!parser.parseLine()) break;
					if (parser.argi(1) == 0) glyph->data(n)->setVector(parser.arg3d(3));
					else glyph->data(n)->setAtom(targetModel->atom(parser.argi(1) - 1));
					glyph->data(n)->setAtomData( (GlyphData::GlyphDataType) parser.argi(2));
				}
				// Additional data now...
				do
				{
					if (!parser.parseLine()) break;
					glyphKeyword = AKFModelPlugin::akfGlyphKeyword(parser.argc(0));
					switch (glyphKeyword)
					{
						case (AKFModelPlugin::RotationKeyword):
							matrix.setIdentity();
							matrix.setColumn(0, parser.arg3d(1), 0.0);
							matrix.setColumn(1, parser.arg3d(4), 0.0);
							matrix.setColumn(2, parser.arg3d(7), 0.0);
							glyph->setRotation(matrix);
							break;
						case (AKFModelPlugin::InvisibleKeyword):
							glyph->setVisible(false);
							break;
						case (AKFModelPlugin::TextKeyword):
							glyph->setText(parser.argc(1));
							break;
						case (AKFModelPlugin::WireFrameKeyword):
							glyph->setSolid(true);
							break;
						default:
							if (glyphKeyword != AKFModelPlugin::EndGlyphKeyword) Messenger::print("Unrecognised glyph property '" + parser.argc(0) + "' - ignored...");
					}
				} while (glyphKeyword != AKFModelPlugin::EndGlyphKeyword);
				break;
			case (AKFModelPlugin::GridKeyword):
				grid = targetModel->addGrid();
				grid->initialise(Grid::gridType(parser.argc(1)), parser.arg3i(2));
				do
				{
					if (!parser.parseLine()) break;
					gridKeyword = AKFModelPlugin::akfGridKeyword(parser.argc(0));
					switch (gridKeyword)
					{
						case (AKFModelPlugin::AxesKeyword):
							matrix.setIdentity();
							matrix.setColumn(0, parser.arg3d(1), 0.0);
							matrix.setColumn(1, parser.arg3d(4), 0.0);
							matrix.setColumn(2, parser.arg3d(7), 0.0);
							grid->setAxes(matrix);
							break;
						case (AKFModelPlugin::OriginKeyword):
							grid->setOrigin(parser.arg3d(1));
							break;
						case (AKFModelPlugin::DataKeyword):
							for (n=0; n<grid->nPoints(); ++n)
							{
								if (!parser.readLineAsDouble(data))
								{
									Messenger::error("Couldn't read point %i from file.\n", n+1);
									break;
								}
								grid->setNextData(data);
							}
							break;
						default:
							if (gridKeyword != AKFModelPlugin::EndGridKeyword) Messenger::print("Unrecognised grid property '" + parser.argc(0) + "' - ignored...");
							break;
					}
				} while (gridKeyword != AKFModelPlugin::EndGridKeyword);
				break;
			case (AKFModelPlugin::TitleKeyword):
				targetModel->setName(parser.argc(1));
				break;
			case (AKFModelPlugin::TorsionKeyword):
				targetModel->addTorsionMeasurement(parser.argi(1)-1, parser.argi(2)-1, parser.argi(3)-1, parser.argi(4)-1, true);
				break;
			default:
				Messenger::print("Unrecognised keyword in akf file '" + parser.filename() + "'. Ignoring it...");
				break;
		}
	}

// 	finaliseModel();

	return true;
}

// Return whether this plugin can export data
bool AKFModelPlugin::canExport()
{
	return true;
}

// Export data to the specified file
bool AKFModelPlugin::exportData(FileParser& parser, const KVMap standardOptions)
{
	// Get the current model pointer containing the data we are to export
	Model* targetModel = parser.targetModel();

	// Title
	parser.writeLine("title  '" + targetModel->name() + "'");

	// Atom data
	for (Atom* i = targetModel->atoms(); i != NULL; i = i->next) parser.writeLineF("atom   %i %s %f %f %f %f %s %f %f %f %f %i", i->id()+1, Elements().symbol(i->element()), i->r().x, i->r().y, i->r().z, i->charge(), Prefs::drawStyle(i->style()), i->colour()[0], i->colour()[1], i->colour()[2], i->colour()[3], i->isPositionFixed() );

	// Bond data
	for (Bond* b = targetModel->bonds(); b != NULL; b = b->next) parser.writeLineF("bond   %i %i %s", b->atomI()->id()+1, b->atomJ()->id()+1, Bond::bondType(b->type()));

	// Cell data
	if (targetModel->isPeriodic())
	{
		Matrix axes = targetModel->cell().axes();
		parser.writeLineF("cellmatrix   %f %f %f %f %f %f %f %f %f",  axes[0], axes[1], axes[2], axes[4], axes[5], axes[6], axes[8], axes[9], axes[10]);
	}

	// Measurements
	for (Measurement* m = targetModel->distanceMeasurements(); m != NULL; m = m->next) parser.writeLineF("distance %i %i\t# %f", m->atom(0)->id()+1, m->atom(1)->id()+1,  m->value());
	for (Measurement* m = targetModel->angleMeasurements(); m != NULL; m = m->next) parser.writeLineF("angle %i %i %i\t# %f", m->atom(0)->id()+1, m->atom(1)->id()+1, m->atom(2)->id()+1, m->value());
	for (Measurement* m = targetModel->torsionMeasurements(); m != NULL; m = m->next) parser.writeLineF("torsion %i %i %i %i\t# %f", m->atom(0)->id()+1, m->atom(1)->id()+1, m->atom(2)->id()+1, m->atom(3)->id()+1, m->value());

	// Glyphs
	for (Glyph* g = targetModel->glyphs(); g != NULL; g = g->next)
	{
		parser.writeLineF("glyph  %s  %i", Glyph::glyphType(g->type()), g->nData());
		// First 'n' lines are the 'n' data points for the glyph
		for (int n=0; n<g->nData(); ++n)
		{
			if (g->data(n)->atom()) parser.writeLineF("  %i  %i  %i  0.0 0.0 0.0  %f %f %f %f", n, g->data(n)->atom()->id()+1, g->data(n)->atomData(), g->data(n)->colour()[0], g->data(n)->colour()[1], g->data(n)->colour()[2], g->data(n)->colour()[3]);
			else parser.writeLineF("  %i  -1  -1  %f %f %f  %f %f %f %f", n, g->data(n)->vector().x, g->data(n)->vector().y, g->data(n)->vector().z, g->data(n)->colour()[0], g->data(n)->colour()[1], g->data(n)->colour()[2], g->data(n)->colour()[3]);
		}
		// Additional data now...
		if (g->isRotated())
		{
			Matrix matrix = g->rotation();
			parser.writeLineF("  rotation  %f %f %f %f %f %f %f %f %f", matrix[0], matrix[1], matrix[2], matrix[4], matrix[5], matrix[6], matrix[8], matrix[9], matrix[10]);
		}
		if (!g->isVisible()) parser.writeLineF("  invisible");
		if (!g->text().isEmpty()) parser.writeLine("  text  '" + g->text() + "'");
		if (!g->isSolid()) parser.writeLineF("  wireframe");
		parser.writeLineF("endglyph");
	}

	// Grids
	for (Grid* g = targetModel->grids(); g != NULL; g = g->next)
	{
		parser.writeLineF("grid  %s  %i  %i  %i", Grid::gridType(g->type()), g->nXYZ().x, g->nXYZ().y, g->nXYZ().z);
		Matrix axes = g->axes();
		parser.writeLineF("  axes       %f  %f  %f  %f  %f  %f  %f  %f  %f", axes[0], axes[1], axes[2], axes[4], axes[5], axes[6], axes[8], axes[9], axes[10]);
		parser.writeLineF("  origin     %f  %f  %f", g->origin().x, g->origin().y, g->origin().z);
		parser.writeLineF("  data");
		int x, y, z;
		if (g->type() == Grid::RegularXYData)
		{
			for (x=0; x<g->nXYZ().x; ++x)
				for (y=0; y<g->nXYZ().y; ++y) parser.writeLineF("    %f", g->data2d()[x][y]);
		}
		else if (g->type() == Grid::RegularXYZData)
		{
			for (x=0; x<g->nXYZ().x; ++x)
				for (y=0; y<g->nXYZ().y; ++y)
					for (z=0; z<g->nXYZ().z; ++z) parser.writeLineF("    %f", g->data3d()[x][y][z]);
		}
		parser.writeLineF("endgrid");
	}

	return true;
}
