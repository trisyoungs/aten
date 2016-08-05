/*
	*** Common functions for DL_POLY plugins
	*** src/plugins/io_dlpoly/common.cpp
	Copyright T. Youngs 2016-2016
	Copyright A. M. Elena 2016-2016

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

#include "plugins/interfaces/fileplugin.h"
#include "plugins/io_dlpoly/common.h"
#include "model/model.h"

#include <QDebug>
ATEN_USING_NAMESPACE

// Read single CONFIG model from file
bool DLPOLYPluginCommon::readCONFIGModel ( FilePluginInterface* plugin, FileParser& parser, Model* targetModel, DLPOLYPluginCommon::DLPOLYVersion version, const bool inTraj )
{
  QString name;
  int levcfg,imcon,nAtoms;

  if (inTraj) {
    if ( !parser.parseLine() ) {
      return false;
    }
    name = parser.argc(1);
    targetModel->setName ( name );
    levcfg=parser.argi ( 3 );
    imcon=parser.argi ( 4 );
    nAtoms = parser.argi ( 2 );

    if (version == DLPOLYPluginCommon::DLPOLY2) {
      if ( imcon != 0 ) {
        Matrix cell;
        for ( int i = 0; i<3; ++i ) {
          if ( !parser.parseLine() ) {
            return false;
          }
          cell.setColumn ( i, parser.argd ( 0 ),parser.argd ( 1 ),parser.argd ( 2 ));
        }
        targetModel->setCell ( cell );
      }
    } else {
      Matrix cell;
      for ( int i = 0; i<3; ++i ) {
        if ( !parser.parseLine() ) {
          return false;
        }
        cell.setColumn ( i, parser.argd ( 0 ),parser.argd ( 1 ),parser.argd ( 2 ));
      }
      targetModel->setCell ( cell );
    }

  }else{
    // Check target model
    if ( targetModel == NULL ) {
      Messenger::error ( "NULL Model pointer passed to DLPOLYPluginCommon::readCONFIGModel." );
      return false;
    }

    // First line is name of model
    if ( !parser.readLine ( name ) ) {
      return false;
    }
    targetModel->setName ( name );

    // Read config level, periodicity and if present number of atoms from file
    // Need to be careful here, since in DL_POLY2 the third number was NOT number of atoms
    if ( !parser.parseLine() ) {
      return false;
    }
    levcfg=parser.argi ( 0 );
    imcon=parser.argi ( 1 );
    nAtoms = (version == DLPOLYPluginCommon::DLPOLY2 ? 0 : parser.argi ( 2 ));

    if ( imcon != 0 ) {
      Matrix cell;
      for ( int i = 0; i<3; ++i ) {
        if ( !parser.parseLine() ) {
          return false;
        }
        cell.setColumn ( i, parser.argd ( 0 ),parser.argd ( 1 ),parser.argd ( 2 ));
      }
      targetModel->setCell ( cell );
    }
  }

  int n=0;
  do {
    if ( !parser.parseLine() ) {
      break;
    }
    QString el = parser.argc ( 0 );
    if ( !parser.parseLine() ) {
      break;
    }
    // Create the new atom
    Vec3<double> r = parser.arg3d ( 0 );
    Vec3<double> v,f;
    if ( levcfg > 0 ) {
      if ( !parser.parseLine() ) {
        break;
      }
      v=parser.arg3d ( 0 );
    }
    if ( levcfg > 1 ) {
      if ( !parser.parseLine() ) {
        break;
      }
      f=parser.arg3d ( 0 );
    }

    plugin->createAtom ( targetModel, el, r, v, f );
    n++;
    if (n==nAtoms) break;
  } while ( (!parser.eofOrBlank()) );

  // Shift atoms by half-cell
  bool shift = FilePluginInterface::toBool(plugin->pluginOptions().value("shiftCell"));
  if (shift && targetModel->isPeriodic())
  {
    targetModel->selectAll(true);
    targetModel->translateSelectionLocal(targetModel->cell().centre(), true);
    targetModel->selectNone(true);
  }

	// Fold model
	if (!plugin->standardOptions().preventFolding()) {
    targetModel->foldAllAtoms();
  }

  // Rebond the model
  if ( !plugin->standardOptions().preventRebonding() ) {
    targetModel->calculateBonding ( true );
  }

  // Check the number of atoms we read in 
  if ( ( n==nAtoms ) || ( nAtoms==0 ) ) {
    return true;
  } else {
    return false;
  } 
}

// Skip single Frame in file
bool DLPOLYPluginCommon::skipFrameModel ( FilePluginInterface* plugin, FileParser& parser, DLPOLYVersion version )
{

  // Read number of atoms from file
  if ( !parser.parseLine() ) {
    return false;
  }
  int nAtoms = parser.argi(2);
  int levcfg = parser.argi(3);
  int imcon = parser.argi(4);
  // Next line is the cell/// this is different between 2 and 4 
  // shall be handled correctly once things work for 4
  if ( ((version == DLPOLYPluginCommon::DLPOLY2) && (imcon>0)) || (version == DLPOLYPluginCommon::DLPOLY4)) {
    if ( !parser.skipLines ( 3 ) ) {
      return false;
    }
  }

  // Now atoms/velocities and forces if present
  if ( !parser.skipLines ( (levcfg+2)*nAtoms ) ) {
    return false;
  }

  return true;
}

// Write single DL_POLY_4 model to file
bool DLPOLYPluginCommon::writeCONFIGModel (FilePluginInterface* plugin, FileParser& parser, Model* sourceModel, DLPOLYPluginCommon::DLPOLYVersion version)
{
  int levcfg = plugin->pluginOptions().value("levcfg").toInt();
  // Write title line
  if ( !parser.writeLineF("%-72s",qPrintable(sourceModel->name()) ) ) {
    return false;
  }

  // Write levcfg, pbc and number atoms line (what about fancy pbc?)
  int imcon=sourceModel->cell().type();
  if ( !parser.writeLineF ( "%10i%10i%10i%-32s", levcfg,imcon, sourceModel->nAtoms() ," ")) {
    return false;
  }

  if (imcon != 0 ){
    Matrix cell=sourceModel->cell().axes();
    for(int i=0;i<9;i=i+4){ 
      Vec3<double> a=cell.rowAsVec3(i);
      if (!parser.writeLineF("%20.10f%20.10f%20.10f%-12s",cell[i+0],cell[i+1],cell[i+2]," ") ){
        return false;
      }
    }
  } 

  // Write atom information
  int k=1;
  bool useTypeNames = FilePluginInterface::toBool(plugin->pluginOptions().value("useTypeNames"));
  bool shift = (FilePluginInterface::toBool(plugin->pluginOptions().value("shiftCell")) && sourceModel->isPeriodic());
  Vec3<double> r;
 
  for ( Atom* i = sourceModel->atoms(); i != NULL; i = i->next ) {
    if (levcfg>=0){
      if (useTypeNames && i->type()){
        if (!parser.writeLineF("%-8s%10i%-54s", qPrintable(i->type()->name()), k++," ")) {
          return false;
        }
      }
      else if (!parser.writeLineF("%-8s%10i%-54s", ElementMap().symbol(i->element()), k++," ")) {
        return false;
      }

      r = i->r();
      if (shift) r -= sourceModel->cell().centre();
      if (!parser.writeLineF("%20.10f%20.10f%20.10f%-12s",r.x,r.y,r.z," ") ){
        return false;
      }
    }
    if (levcfg>=1){
      if (!parser.writeLineF("%20.10f%20.10f%20.10f%-12s",i->v().x,i->v().y,i->v().z," ") ){
        return false;
      }
    }
    if (levcfg>=2){
      if (!parser.writeLineF("%20.10f%20.10f%20.10f%-12s",i->f().x,i->f().y,i->f().z," ") ){
        return false;
      }
    }
  }

  return true;
}

// Determine whether trajectory file is unformatted
bool DLPOLYPluginCommon::determineHISTORYFormat(FilePluginInterface* plugin, FileParser& parser, bool& unformatted, bool& hasHeader, DLPOLYPluginCommon::DLPOLYVersion version)
{
	// Get unformatted datatype sizes
	int integerSize = plugin->pluginOptions().value("integerSize").toInt();
	int realSize = plugin->pluginOptions().value("realSize").toInt();
	int recordLength;

	/*
	 * 1) Try an unformatted read to get the Fortran integer record size
	 *    The result of this will be either 40 or 80 for DL_POLY2, depending on whether the HISTORY file is restarted or not
	 */
	if (!parser.readRawInteger(recordLength, integerSize)) return false;
	if (recordLength == 40)
	{
		Messenger::print("DL_POLY HISTORY file appears to be unformatted and does not contain a header.");
		unformatted = true;
		hasHeader = false;
		parser.rewind();
		return true;
	}
	else if (recordLength == 80)
	{
		Messenger::print("DL_POLY HISTORY file appears to be unformatted and contains a header.");
		unformatted = true;
		hasHeader = true;
		parser.rewind();
		return true;
	}

	/*
	 * 2) Doesn't appear to be unformatted - make sure its formatted...
	 */
	parser.rewind();
	QString line;
	if (!parser.readLine(line)) return false;
	if (line.startsWith("timestep "))
	{
		Messenger::print("DL_POLY HISTORY file appears to be formatted and does not contain a header.");
		unformatted = false;
		hasHeader = false;
		parser.rewind();
		return true;
	}
	else
	{
		// Must skip next line and then check for "timestep" line again
		if (!parser.skipLines(1)) return false;
		if (!parser.readLine(line)) return false;
		if (line.startsWith("timestep "))
		{
			Messenger::print("DL_POLY HISTORY file appears to be formatted and contains a header.");
			unformatted = false;
			hasHeader = true;
			parser.rewind();
			return true;
		}
	}

	Messenger::error("Failed to determine format of DL_POLY HISTORY file.");

	parser.rewind();
	return false;
}

// Read single unformatted frame from file
bool DLPOLYPluginCommon::readUnformattedFrame(FilePluginInterface* plugin, FileParser& parser, Model* targetModel, DLPOLYPluginCommon::DLPOLYVersion version, int integerSize, int realSize, QStringList unformattedAtomNames, Array<double> unformattedCharges)
{
	// Variables
	double tempDouble, timeStep;
	double axes[9];
	int recordLength, nAtoms, nStep, keytrj, imcon;

	// First data for frame is : nstep, natoms, keytrj, imcon, tstep, all as doubles 
	if (!parser.readRawInteger(recordLength, integerSize)) return false;
	if (recordLength != 5*realSize)
	{
		Messenger::error("Error reading start of trajectory frame.");
		return false;
	}
	if (!parser.readRawDouble(tempDouble, realSize)) return false;
	nStep = floor(tempDouble+0.1);
	if (!parser.readRawDouble(tempDouble, realSize)) return false;
	nAtoms = floor(tempDouble+0.1);
	if (!parser.readRawDouble(tempDouble, realSize)) return false;
	keytrj = floor(tempDouble+0.1);
	if (!parser.readRawDouble(tempDouble, realSize)) return false;
	imcon = floor(tempDouble+0.1);
	if (!parser.readRawDouble(timeStep, realSize)) return false;

	if (!parser.readRawInteger(recordLength, integerSize)) return false;

	Messenger::print(Messenger::Verbose, "nstep = %i, natoms = %i, keytrj = %i, imcon = %i, tstep = %f\n", nStep, nAtoms, keytrj, imcon, timeStep);
	targetModel->setName(QString("t = %1, n = %2").arg(timeStep).arg(nStep));

	// Create temporary data arrays for reading
	Array<double> x, y, z;

	// Unit cell
	if (imcon != 0)
	{
		if (!parser.readRawInteger(recordLength, integerSize)) return false;
		if (recordLength != 9*realSize)
		{
			Messenger::print("Error reading cell info from trajectory frame.");
			return false;
		}
		if (!parser.readRawDoubleArray(axes, 9)) return false;
		if (!parser.readRawInteger(recordLength, integerSize)) return false;

		Matrix mat;
		mat.setColumn(0, axes[0], axes[1], axes[2]);
		mat.setColumn(1, axes[3], axes[4], axes[5]);
		mat.setColumn(2, axes[6], axes[7], axes[8]);
		targetModel->setCell(mat);
	}

	// Coordinates
	if (!parser.readRawInteger(recordLength, integerSize)) return false;
	if (recordLength != nAtoms*realSize)
	{
		Messenger::error("Error reading coordinate info from trajectory frame.");
		return false;
	}
	if (!parser.readRawDoubleArray(x, nAtoms)) return false;
	if (!parser.readRawInteger(recordLength, integerSize)) return false;

	if (!parser.readRawInteger(recordLength, integerSize)) return false;
	if (!parser.readRawDoubleArray(y, nAtoms)) return false;
	if (!parser.readRawInteger(recordLength, integerSize)) return false;

	if (!parser.readRawInteger(recordLength, integerSize)) return false;
	if (!parser.readRawDoubleArray(z, nAtoms)) return false;
	if (!parser.readRawInteger(recordLength, integerSize)) return false;

	// Create atoms - if we read in atom information in the header we will have the names and masses
	// If these arrays are empty, use the parent model. If it doesn't contain enough atoms, create a dummy atom
	Atom* i = targetModel->parent()->atoms(), *j;
	bool shiftCell = FilePluginInterface::toBool(plugin->pluginOptions().value("shiftCell"));
	Vec3<double> r;
	for (int n = 0; n<nAtoms; ++n)
	{
		r.set(x[n], y[n], z[n]);
		if (shiftCell && (imcon != 0)) r += targetModel->cell().centre();

		// If possible, use information from header or parent model
		if (n < unformattedAtomNames.size())
		{
			j = plugin->createAtom(targetModel, unformattedAtomNames.at(n), r);
			j->setCharge(unformattedCharges[n]);
		}
		else if (i)
		{
			j = targetModel->addAtom(i->element(), r);
			j->copyStyle(i);
		}
		else targetModel->addAtom(0, r);

		if (i) i = i->next;
	}

	// Velocities
	if (keytrj > 0)
	{
		if (!parser.readRawInteger(recordLength, integerSize)) return false;
		if (recordLength != nAtoms*realSize)
		{
			Messenger::error("Error reading velocities from trajectory frame.");
			return false;
		}
		if (!parser.readRawDoubleArray(x, nAtoms)) return false;
		if (!parser.readRawInteger(recordLength, integerSize)) return false;

		if (!parser.readRawInteger(recordLength, integerSize)) return false;
		if (!parser.readRawDoubleArray(y, nAtoms)) return false;
		if (!parser.readRawInteger(recordLength, integerSize)) return false;

		if (!parser.readRawInteger(recordLength, integerSize)) return false;
		if (!parser.readRawDoubleArray(z, nAtoms)) return false;
		if (!parser.readRawInteger(recordLength, integerSize)) return false;

		int n = 0;
		Atom* i = targetModel->atoms();
		while (i)
		{
			i->v().set(x[n], y[n], z[n]);
			i = i->next;
			++n;
		}
	}

	// Forces
	if (keytrj > 1)
	{
		if (!parser.readRawInteger(recordLength, integerSize)) return false;
		if (recordLength != nAtoms*realSize)
		{
			Messenger::error("Error reading forces from trajectory frame.");
			return false;
		}
		if (!parser.readRawDoubleArray(x, nAtoms)) return false;
		if (!parser.readRawInteger(recordLength, integerSize)) return false;

		if (!parser.readRawInteger(recordLength, integerSize)) return false;
		if (!parser.readRawDoubleArray(y, nAtoms)) return false;
		if (!parser.readRawInteger(recordLength, integerSize)) return false;

		if (!parser.readRawInteger(recordLength, integerSize)) return false;
		if (!parser.readRawDoubleArray(z, nAtoms)) return false;
		if (!parser.readRawInteger(recordLength, integerSize)) return false;

		int n = 0;
		Atom* i = targetModel->atoms();
		while (i)
		{
			i->f().set(x[n], y[n], z[n]);
			i = i->next;
			++n;
		}
	}

	// Rebond (if requested)
	if (!plugin->standardOptions().preventRebonding()) targetModel->calculateBonding(true);

	return true;
}

// Skip single unformatted frame in file
bool DLPOLYPluginCommon::skipUnformattedFrame(FilePluginInterface* plugin, FileParser& parser, DLPOLYVersion version, int integerSize, int realSize)
{
	// Variables
	double tempDouble, timeStep;
	int recordLength, nAtoms, nStep, keytrj, imcon;

	// First data for frame is : nstep, natoms, keytrj, imcon, tstep, all as doubles 
	if (!parser.readRawInteger(recordLength, integerSize)) return false;
	if (recordLength != 5*realSize)
	{
		Messenger::error("Error reading start of trajectory frame.");
		return false;
	}
	if (!parser.readRawDouble(tempDouble, realSize)) return false;
	nStep = floor(tempDouble+0.1);
	if (!parser.readRawDouble(tempDouble, realSize)) return false;
	nAtoms = floor(tempDouble+0.1);
	if (!parser.readRawDouble(tempDouble, realSize)) return false;
	keytrj = floor(tempDouble+0.1);
	if (!parser.readRawDouble(tempDouble, realSize)) return false;
	imcon = floor(tempDouble+0.1);
	if (!parser.readRawDouble(timeStep, realSize)) return false;

	if (!parser.readRawInteger(recordLength, integerSize)) return false;

	// Unit cell
	if (imcon != 0)
	{
		if (!parser.readRawInteger(recordLength, integerSize)) return false;
		if (recordLength != 9*realSize)
		{
			Messenger::print("Error reading cell info from trajectory frame.");
			return false;
		}
		parser.skipChars(recordLength);
		if (!parser.readRawInteger(recordLength, integerSize)) return false;
	}

	// Coordinates
	if (!parser.readRawInteger(recordLength, integerSize)) return false;
	if (recordLength != nAtoms*realSize)
	{
		Messenger::error("Error reading coordinate info from trajectory frame.");
		return false;
	}
	parser.skipChars(recordLength);
	if (!parser.readRawInteger(recordLength, integerSize)) return false;

	if (!parser.readRawInteger(recordLength, integerSize)) return false;
	parser.skipChars(recordLength);
	if (!parser.readRawInteger(recordLength, integerSize)) return false;

	if (!parser.readRawInteger(recordLength, integerSize)) return false;
	parser.skipChars(recordLength);
	if (!parser.readRawInteger(recordLength, integerSize)) return false;

	// Velocities
	if (keytrj > 0)
	{
		if (!parser.readRawInteger(recordLength, integerSize)) return false;
		if (recordLength != nAtoms*realSize)
		{
			Messenger::error("Error reading velocities from trajectory frame.");
			return false;
		}
		parser.skipChars(recordLength);
		if (!parser.readRawInteger(recordLength, integerSize)) return false;

		if (!parser.readRawInteger(recordLength, integerSize)) return false;
		parser.skipChars(recordLength);
		if (!parser.readRawInteger(recordLength, integerSize)) return false;

		if (!parser.readRawInteger(recordLength, integerSize)) return false;
		parser.skipChars(recordLength);
		if (!parser.readRawInteger(recordLength, integerSize)) return false;
	}

	// Forces
	if (keytrj > 1)
	{
		if (!parser.readRawInteger(recordLength, integerSize)) return false;
		if (recordLength != nAtoms*realSize)
		{
			Messenger::error("Error reading forces from trajectory frame.");
			return false;
		}
		parser.skipChars(recordLength);
		if (!parser.readRawInteger(recordLength, integerSize)) return false;

		if (!parser.readRawInteger(recordLength, integerSize)) return false;
		parser.skipChars(recordLength);
		if (!parser.readRawInteger(recordLength, integerSize)) return false;

		if (!parser.readRawInteger(recordLength, integerSize)) return false;
		parser.skipChars(recordLength);
		if (!parser.readRawInteger(recordLength, integerSize)) return false;
	}

	return true;
}

