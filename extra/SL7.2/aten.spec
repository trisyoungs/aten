# Defines a constant that is used later by the spec file.
%define shortname aten

# Name, brief description, and version 
Summary: Aten - Atomic configuration builder and editor
Name: %{shortname}
Version: 2.1.6
Release: 1
License: GPL
%define fullname %{name}-%{version}
# norootforbuild

# Define buildroot
BuildRoot: /var/tmp/%{fullname}

# Software group
Group: Productivity/Scientific/Chemistry

# Source tar ball.
Source: %{fullname}.tar.gz

# Location of the project's home page.
URL: http://www.projectaten.net

# Owner of the product.
Vendor: Tristan Youngs

# Packager of the product.
Packager: Tristan Youngs

# Boolean that specifies if you want to automatically determine some dependencies
AutoReq: yes

# Build dependencies
BuildRequires: gcc-c++ bison readline-devel ftgl-devel qt5-qtbase-devel qt5-qtsvg-devel

# In-depth description.
%description
Aten provides a clean graphical user interface allowing the intuitive editing and creation of input coordinates for computational chemistry / physics codes. It allows periodic (i.e. condensed) and non-periodic (i.e. gas-phase) models and systems to be created either from scratch or from existing coordinate files. Molecular mechanics forcefields of standard functional forms may be loaded and used to minimise existing systems or create (through Monte Carlo techniques) new multi-component configurations. Periodic systems may be stretched, scaled, and repeated, and symmetry operators applied. Molecular dynamics trajectories may be loaded, viewed, frames edited, and properties calculated.

%prep
%setup -q

%build

# Configure and make
./configure --with-build-dir=$RPM_BUILD_ROOT --with-install-dir=/usr --prefix=$RPM_BUILD_ROOT/usr 
make

%install
make install DESTDIR=%{buildroot}

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root)
%doc README TODO COPYING ChangeLog
/usr/share/applications/Aten.desktop
/usr/share/pixmaps/aten.png
/usr/bin/aten
/usr/share/aten/
%{_libdir}/aten/plugins/*.so
%exclude %{_libdir}/aten/plugins/*.la

%changelog
* Wed Apr 02 2008 Tristan Youngs <tristan.youngs@stfc.ac.uk> 
- added checks to build on different distros with the SuSE build service.
* Tue Apr 01 2008 Tristan Youngs <tristan.youngs@stfc.ac.uk>
- added dependencies list and long description.
* Sun Mar 30 2008 Tristan Youngs <tristan.youngs@stfc.ac.uk>
- installation target points to local dir.
* Mon Mar 24 2008 Tristan Youngs <tristan.youngs@stfc.ac.uk> 
- initial version.
