/*
	*** Main Window - Update Functions
	*** src/gui/mainwindow_update.cpp
	Copyright T. Youngs 2007-2017

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

#include "gui/mainwindow.h"
#include "main/version.h" 
#include "main/aten.h"
#include "gui/vibrations.h"
#include "gui/glyphs.h"
#include "gui/tmenubutton.hui"
#include "render/fontinstance.h"
#include <QtWidgets/QMessageBox>

// Update GUI after model change (or different model selected)
void AtenWindow::updateMainWindow()
{
	// Get current model
	Model* currentModel = aten_.currentModel();

	// Update status bar
	QString modelLabelText, cellInfoText, selectionText, massText, densityText, atomsLabelText;
	if (currentModel)
	{
		// Parent / frame information
		if (currentModel->hasTrajectory())
		{
			if (currentModel->renderSourceModel() == currentModel) modelLabelText = "[Parent of " + QString::number(currentModel->nTrajectoryFrames()) + "]";
			else modelLabelText = "[Frame " + QString::number(currentModel->trajectoryFrameIndex()+1) + " of " + QString::number(currentModel->nTrajectoryFrames()) + "]";
		}
		else modelLabelText = "[Model]";

		// Get current render target for remainder of information
		currentModel = currentModel->renderSourceModel();

		// Atoms label
		if (currentModel->nAtoms() == 1) atomsLabelText = "1 Atom";
		else atomsLabelText = QString::number(currentModel->nAtoms()) + " Atoms";
		if (currentModel->nUnknownAtoms() != 0) atomsLabelText += " (<b>? = " + QString::number(currentModel->nUnknownAtoms()) + "</b>)";

		// Mass
		massText = QString::number(currentModel->mass()) + " g mol<sup>-1</sup>";

		// Selection
		if (currentModel->nSelected() == 0) selectionText = "0 / --";
		else selectionText = QString::number(currentModel->nSelected()) + " / " + currentModel->selectionEmpirical(false, false, true);

		// Cell information
		UnitCell::CellType ct = currentModel->cell().type();
		if (ct != UnitCell::NoCell)
		{
			cellInfoText = UnitCell::cellType(ct);

			densityText = QString::number(currentModel->density());
			switch (prefs.densityUnit())
			{
				case (Prefs::GramsPerCm):
					densityText += " g cm<sup>-3</sup>";
					break;
				case (Prefs::AtomsPerAngstrom):
					densityText += " atoms &#8491;<sup>-3</sup>";
					break;
				default:
					break;
			}
		}
		else
		{
			cellInfoText = "Non-periodic";
			densityText = "--";
		}
	}

	// Set labels
	cellInfoLabel_->setText(cellInfoText);
	atomsLabel_->setText(atomsLabelText);
	massLabel_->setText(massText);
	densityLabel_->setText(densityText);
	selectionLabel_->setText(selectionText);
	modelLabel_->setText(modelLabelText);

	// Update main window title
	QString title = QString("Aten (v%1)").arg(ATENVERSION);
	if (currentModel) title += QString(" - %1 (%2)%3").arg(currentModel->name(), currentModel->filename().isEmpty() ? "<<no filename>>" : currentModel->filename(), currentModel->isModified() ? " [Modified]" : "");
	else title += " [[[ No Current Model ]]]";
	setWindowTitle(title);
}

// Update and show
void AtenWindow::initialUpdateAndShow()
{
	Messenger::enter("AtenWindow::initialUpdateAndShow");

	// Display message box warning if there are no plugins
	if (aten_.pluginStore().nFilePlugins() == 0)
	{
		QMessageBox::warning(NULL, "Aten", "Plugins could not be found.\nNo import/export will be possible.\nSet either the ATENDATA or ATENPLUGINS environment variable to point to the location where the plugins are stored, or use the --atendata or --atenplugins CLI switches.", QMessageBox::Ok, QMessageBox::Ok);
	}

	// Show the window
	show();
	shown_ = true;

	// Load font - must do this *after* the mainwindow is shown on some systems (OSX)
	if (!FontInstance::setup(prefs.viewerFontFileName())) QMessageBox::warning(0, "Font Error", "Failed to setup font '" + prefs.viewerFontFileName() + "'.");

	// Update the fragments widget and icons
	ReturnValue rv;
	aten_.updateFragmentIcons();
	ui.BuildDrawFragmentButton->callPopupMethod("updateFragments", rv);

	// Add Tool plugins to a new tab (if there are any)
	if (aten_.pluginStore().nToolPlugins() != 0)
	{
		// For each tool plugin we will check its group name - this will determine the QGridLayout in the tab into which the button is added
		RefList<QGridLayout,QString> groups;
		const RefList<ToolPluginInterface, KVMap>& toolPlugins = aten_.pluginStore().toolPlugins(PluginTypes::GeneralToolPlugin);
		for (RefListItem<ToolPluginInterface, KVMap>* plugin = toolPlugins.first(); plugin != NULL; plugin = plugin->next)
		{
			ToolPluginInterface* toolPlugin = plugin->item;

			// For this plugins groupName see if we have already have a layout set up for it
			QGridLayout* groupLayout = NULL;
			for (RefListItem<QGridLayout,QString>* ri = groups.first(); ri != NULL; ri = ri->next) if (ri->data == toolPlugin->groupName())
			{
				groupLayout = ri->item;
				break;
			}
			// Did we find a layout?
			if (groupLayout == NULL)
			{
				groupLayout = new  QGridLayout();
				groupLayout->setContentsMargins(0,0,0,0);
				groupLayout->setSpacing(2);
				groups.add(groupLayout, toolPlugin->groupName());
			}
			
			// OK, now add a tool button to the layout
			TMenuButton* menuButton = new TMenuButton();
			menuButton->setText(toolPlugin->buttonLabel());
			menuButton->setSizePolicy(QSizePolicy::Minimum, QSizePolicy::Fixed);
			menuButton->setMinimumSize(40, 66);
			menuButton->setMaximumSize(128, 66);
			menuButton->setIcon(toolPlugin->buttonIcon());
			menuButton->setIconSize(QSize(30, 30));
			menuButton->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
			menuButton->setToolPluginInterface(toolPlugin);
			menuButton->setToolTip(toolPlugin->description());
			groupLayout->addWidget(menuButton, 0, groupLayout->columnCount());
		}

		// Grid layouts have been assembled, so add on group labels
		for (RefListItem<QGridLayout,QString>* ri = groups.first(); ri != NULL; ri = ri->next)
		{
			QGridLayout* groupLayout = ri->item;
			QLabel* label = new QLabel(ri->data);
			label->setAlignment(Qt::AlignHCenter);
			groupLayout->addWidget(label, 1, 0, 1, groupLayout->columnCount());
		}

		// Finally, create and setup a widget and layout for the new tab, and add our QGridayout(s) to it
		QWidget* toolsTab = new QWidget();
		QHBoxLayout* toolsLayout = new QHBoxLayout;
		toolsLayout->setContentsMargins(2,2,2,2);
		toolsLayout->setSpacing(4);
		for (RefListItem<QGridLayout,QString>* ri = groups.first(); ri != NULL; ri = ri->next)
		{
			// Add a vertical line spacer?
			if (ri != groups.first())
			{
				QFrame* line = new QFrame();
				line->setFrameStyle(QFrame::VLine | QFrame::Sunken);
				toolsLayout->addWidget(line);
			}
			toolsLayout->addLayout(ri->item);
		}
		// Add a horizontal spacer at the end
		toolsLayout->addStretch();

		// Finally, add the layout to the widget, and create a new tab whose page is the widget we created earlier
		toolsTab->setLayout(toolsLayout);
		ui.ToolPanels->addTab(toolsTab, "Plugins");
	}

	// Update everything else
	updateWidgets(AtenWindow::AllTargets);

	// Finally, set the progress dialog pointer in Messenger, and tell it to stop printing to stdout
	Messenger::setAtenProgress(&progressDialog_);
	Messenger::setPrintToConsole(false);

	Messenger::exit("AtenWindow::initialUpdateAndShow");
}

// Return whether main window has been shown
bool AtenWindow::shown()
{
	return shown_;
}

// Update GUI after model change (or different model selected) (accessible wrapper to call AtenWindow's function)
void AtenWindow::updateWidgets(int targets)
{
	Messenger::enter("AtenWindow::updateWidgets");

	refreshing_ = true;

	// Always update main window bottom-left status info, menu items, and titlebar
	updateMainWindow();

	// Update main view (always)
	ui.MainView->update();

	// Update model list
	if (targets&AtenWindow::ModelsListTarget) updateModelsList();

	// Get current model
	Model* currentModel = aten_.currentModelOrFrame();

	// Always update Home panel
	updateHomePanel(currentModel);

	// Enable / disable Selection panel
	ui.SelectionPanel->setEnabled(currentModel && currentModel->nSelected());

	// Update atoms table
	if (targets&AtenWindow::AtomsTableTarget) updateAtomsTable(currentModel);

	// Panels
	if (targets&AtenWindow::BuildPanelTarget) updateBuildPanel(currentModel);
	if (targets&AtenWindow::CellPanelTarget) updateCellPanel(currentModel);
	if (targets&AtenWindow::CalculatePanelTarget) updateCalculatePanel(currentModel);
	if (targets&AtenWindow::TransformPanelTarget) updateTransformPanel(currentModel);
	if (targets&AtenWindow::GridsPanelTarget) updateGridsPanel(currentModel);
	if (targets&AtenWindow::TrajectoryPanelTarget) updateTrajectoryPanel(currentModel);
	if (targets&AtenWindow::SelectPanelTarget) updateSelectPanel(currentModel);
	if (targets&AtenWindow::SelectionPanelTarget) updateSelectionPanel(currentModel);
	if (targets&AtenWindow::ToolsPanelTarget) updateToolsPanel(currentModel);
	if (targets&AtenWindow::ForcefieldsPanelTarget) updateForcefieldsPanel(currentModel);

	if (targets&AtenWindow::VibrationsTarget) vibrationsWidget->refresh();

	// Update contents of the glyph list
	if (targets&AtenWindow::GlyphsTarget) glyphsWidget->refresh();

	// Main window statusbar
	if (targets&AtenWindow::StatusBarTarget)
	{
		// Update mode help text
		QString text;
		text.sprintf("<b>%s:</b> %s", UserActions[selectedMode_].name, UserActions[selectedMode_].unModified);
		if (UserActions[selectedMode_].shiftModified[0] != '\0') text += ", <b>+shift</b> " + QString(UserActions[selectedMode_].shiftModified);
		if (UserActions[selectedMode_].ctrlModified[0] != '\0') text += ", <b>+ctrl</b> " + QString(UserActions[selectedMode_].ctrlModified);
		if (UserActions[selectedMode_].altModified[0] != '\0') text += ", <b>+alt</b> " + QString(UserActions[selectedMode_].altModified);

		this->setMessageLabel(text);
	}

	refreshing_ = false;

	Messenger::exit("AtenWindow::updateWidgets");
}
