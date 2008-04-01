# Defines a constant that is used later by the spec file.
%define shortname aten

# Name, brief description, and version 
Summary: Aten - Atomic configuration builder and editor
Name: %{shortname}
Version: 0.95.3
Release: 1
License: GPL
%define fullname %{name}-%{version}

# Software group
Group: Applications/Editors

# Source tar ball.
Source: %{fullname}.tar.gz

# Location of the project's home page.
URL: http://www.projectaten.org

# Owner of the product.
Vendor: Tristan Youngs

# Packager of the product.
Packager: Tristan Youngs

# Boolean that specifies if you want to automatically determine some
# dependencies.
AutoReq: yes

BuildRequires: gcc-c++ libqt4 libqt4-devel Mesa-devel

# In-depth description.
%description
Aten provides a clean graphical user interface allowing the intuitive editing and creation of input coordinates for computational chemistry / physics codes. It allows periodic (i.e. condensed) and non-periodic (i.e. gas-phase) models and systems to be created either from scratch or from existing coordinate files. Molecular mechanics forcefields of standard functional forms may be loaded and used to minimise existing systems or create (through Monte Carlo techniques) new multi-component configurations. Periodic systems may be stretched, scaled, and repeated, and symmetry operators applied. Molecular dynamics trajectories may be loaded, viewed, frames edited, and properties calculated.

%prep
%setup -q

%build
./configure
make

%install
make install

%clean
#rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root)
%doc README TODO COPYING ChangeLog
/usr/local/bin/aten
/usr/local/share/aten/

%changelog
* Tue Mar 01 2008 Tristan Youngs <tris@projectaten.com> 
- added dependencies list and long description.
* Sun Mar 30 2008 Tristan Youngs <tris@projectaten.com> 
- installation target points to local dir.
* Mon Mar 24 2008 Tristan Youngs <tris@projectaten.com> 
- initial version.
