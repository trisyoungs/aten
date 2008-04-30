# Defines a constant that is used later by the spec file.
%define shortname aten

# Name, brief description, and version 
Summary: Aten - Atomic configuration builder and editor
Name: %{shortname}
Version: 0.97
Release: 1
License: GPL
%define fullname %{name}-%{version}
# norootforbuild

# Define buildroot
BuildRoot: /var/tmp/%{fullname}

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

# Basic package dependencies are listed here. For RedHat-based distros, libqt4 = qt4, and libqt4-devel = qt4-devel
BuildRequires: gcc-c++

%if 0%{?suse_version}
BuildRequires: libqt4 libqt4-devel Mesa-devel readline-devel
%endif
# For Ubuntu, lowercase 'm' for Mesa.
%if 0%{?ubuntu_version}
BuildRequires: libqt4-gui libqt4-core libqt4-dev libreadline5-dev libgl1-mesa-dev
%endif
# For RedHat-based distros, libqt4 = qt4, and libqt4-devel = qt4-devel
%if 0%{?fedora_version} || 0%{?rhel_version} || 0%{?centos_version}
BuildRequires: Mesa-devel qt4 qt4-devel readline-devel
%endif

# In-depth description.
%description
Aten provides a clean graphical user interface allowing the intuitive editing and creation of input coordinates for computational chemistry / physics codes. It allows periodic (i.e. condensed) and non-periodic (i.e. gas-phase) models and systems to be created either from scratch or from existing coordinate files. Molecular mechanics forcefields of standard functional forms may be loaded and used to minimise existing systems or create (through Monte Carlo techniques) new multi-component configurations. Periodic systems may be stretched, scaled, and repeated, and symmetry operators applied. Molecular dynamics trajectories may be loaded, viewed, frames edited, and properties calculated.

%prep
%setup -q

# For the build, RedHat distros seem to need the path to the Qt4 binaries set explicitly. SuSE is fine.
%build
./configure --with-build-dir=$RPM_BUILD_ROOT --prefix=$RPM_BUILD_ROOT/usr \
%if 0%{?fedora_version} || 0%{?rhel_version} || 0%{?centos_version}
%ifarch x86_64
--with-qtdir=/usr/lib64/qt4/bin
%else
--with-qtdir=/usr/lib/qt4/bin
%endif
%else

%endif

make

%install
make install

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root)
%doc README TODO COPYING ChangeLog
/usr/bin/aten
/usr/share/aten/

%changelog
* Wed Apr 02 2008 Tristan Youngs <tris@projectaten.org> 
- added checks to build on different distros with the SuSE build service.
* Tue Apr 01 2008 Tristan Youngs <tris@projectaten.org>
- added dependencies list and long description.
* Sun Mar 30 2008 Tristan Youngs <tris@projectaten.org>
- installation target points to local dir.
* Mon Mar 24 2008 Tristan Youngs <tris@projectaten.com> 
- initial version.
