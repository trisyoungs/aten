/*
 *** Common functions for DL_POLY_4 plugins
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

ATEN_USING_NAMESPACE

// Read single CONFIG model from file
bool DLPOLYPluginCommon::readCONFIGModel ( FilePluginInterface* plugin, FileParser& parser, Model* targetModel, DLPOLYPluginCommon::DLPOLYVersion version )
{
  QString name;

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
  int levcfg=parser.argi ( 0 );
  int imcon=parser.argi ( 1 );
  int nAtoms = (version == DLPOLYPluginCommon::DLPOLY2 ? 0 : parser.argi ( 2 ));
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
  int n = 0;
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
  } while ( !parser.eofOrBlank() );

  // Shift atoms by half-cell
  bool shift = FilePluginInterface::toBool(plugin->pluginOptions().value("shiftCell"));
  if (shift && targetModel->isPeriodic())
  {
	  targetModel->selectAll(true);
	  targetModel->translateSelectionLocal(targetModel->cell().centre(), true);
	  targetModel->selectNone(true);
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
bool DLPOLYPluginCommon::skipFrameModel ( FileParser& parser )
{
  int nAtoms;

  // Read number of atoms from file
  if ( !parser.readLineAsInteger ( nAtoms ) ) {
    return false;
  }

  // Next line is name of model
  if ( !parser.skipLines ( 1 ) ) {
    return false;
  }

  // Now atoms...
  if ( !parser.skipLines ( nAtoms ) ) {
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
  int k=0;
	bool useTypeNames = plugin->pluginOptions().value("useTypeNames") == "true";
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

      if (!parser.writeLineF("%20.10f%20.10f%20.10f%-12s",i->r().x,i->r().y,i->r().z," ") ){
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
