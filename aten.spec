# Defines a constant that is used later by the spec file.
%define shortname aten

# Name, brief description, and version 
Summary: Aten - Atomic configuration builder and editor
Name: %{shortname}
Version: 1.909
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

# Boolean that specifies if you want to automatically determine some
# dependencies.
AutoReq: yes

# Basic package dependencies are listed here. For RedHat-based distros, libqt4 = qt4, and libqt4-devel = qt4-devel
BuildRequires: gcc-c++

# For OpenSUSE distros
%if 0%{?suse_version}
BuildRequires: libqt4 libqt4-devel Mesa-devel readline-devel
%endif

# For SLES-based distros, libqt4 = qt4, and libqt4-devel = qt4-devel
%if 0%{?sles_version} == 11
BuildRequires: Mesa-devel libqt4 libqt4-devel readline-devel
%endif
%if 0%{?sles_version} == 10
BuildRequires: Mesa-devel libqt4-devel readline-devel qt4
%endif

# For RedHat-based distros, libqt4 = qt4, and libqt4-devel = qt4-devel
%if 0%{?fedora} || 0%{?rhel_version} || 0%{?centos_version}
BuildRequires: Mesa-devel qt4 qt4-devel readline-devel
%endif

# For Mandriva Linux
%if 0%{?mandriva_version} > 2006
BuildRequires: qt4-devel mesa-common-devel readline-devel
%endif

# In-depth description.
%description
Aten provides a clean graphical user interface allowing the intuitive editing and creation of input coordinates for computational chemistry / physics codes. It allows periodic (i.e. condensed) and non-periodic (i.e. gas-phase) models and systems to be created either from scratch or from existing coordinate files. Molecular mechanics forcefields of standard functional forms may be loaded and used to minimise existing systems or create (through Monte Carlo techniques) new multi-component configurations. Periodic systems may be stretched, scaled, and repeated, and symmetry operators applied. Molecular dynamics trajectories may be loaded, viewed, frames edited, and properties calculated.

%prep
%setup -q

# For the build, RedHat distros seem to need the path to the Qt4 binaries set explicitly. SuSE is fine.
%build

# Configure

%if 0%{?suse_version}
    export LIBS="$LIBS -lGL"
    ./configure --with-build-dir=$RPM_BUILD_ROOT --with-install-dir=/usr --prefix=$RPM_BUILD_ROOT/usr 
%endif

%if 0%{?sles_version}
    ./configure --with-build-dir=$RPM_BUILD_ROOT --with-install-dir=/usr --prefix=$RPM_BUILD_ROOT/usr 
%endif


%if 0%{?mandriva_version}
    %ifarch x86_64
        ./configure --with-build-dir=$RPM_BUILD_ROOT --with-install-dir=/usr --prefix=$RPM_BUILD_ROOT/usr --with-qtmoc=/usr/lib/qt4/bin/moc --with-qtrcc=/usr/lib/qt4/bin/rcc --with-qtuic=/usr/lib/qt4/bin/uic
    %else
        ./configure --with-build-dir=$RPM_BUILD_ROOT --with-install-dir=/usr --prefix=$RPM_BUILD_ROOT/usr --with-qtmoc=/usr/lib/qt4/bin/moc --with-qtrcc=/usr/lib/qt4/bin/rcc --with-qtuic=/usr/lib/qt4/bin/uic
    %endif
%endif

%if 0%{?fedora}
    %ifarch x86_64
        export LDFLAGS="-lGL"
        ./configure --with-build-dir=$RPM_BUILD_ROOT --with-install-dir=/usr --prefix=$RPM_BUILD_ROOT/usr --with-qtmoc=moc-qt4 --with-qtrcc=rcc --with-qtuic=uic-qt4
    %else
        export LDFLAGS="-lGL"
        ./configure --with-build-dir=$RPM_BUILD_ROOT --with-install-dir=/usr --prefix=$RPM_BUILD_ROOT/usr --with-qtmoc=moc-qt4 --with-qtrcc=rcc --with-qtuic=uic-qt4
    %endif
%endif

%if 0%{?rhel_version}
    %ifarch x86_64
        ./configure --with-build-dir=$RPM_BUILD_ROOT --with-install-dir=/usr --prefix=$RPM_BUILD_ROOT/usr --with-qtmoc=/usr/lib64/qt4/bin/moc --with-qtrcc=/usr/lib64/qt4/bin/rcc --with-qtuic=/usr/lib64/qt4/bin/uic --with-oldqt
    %else
        ./configure --with-build-dir=$RPM_BUILD_ROOT --with-install-dir=/usr --prefix=$RPM_BUILD_ROOT/usr --with-qtmoc=/usr/lib/qt4/bin/moc --with-qtrcc=/usr/lib/qt4/bin/rcc --with-qtuic=/usr/lib/qt4/bin/uic --with-oldqt
    %endif
%endif

%if 0%{?centos_version}
    %ifarch x86_64
        ./configure --with-build-dir=$RPM_BUILD_ROOT --with-install-dir=/usr --prefix=$RPM_BUILD_ROOT/usr --with-qtmoc=/usr/lib64/qt4/bin/moc --with-qtrcc=/usr/lib64/qt4/bin/rcc --with-qtuic=/usr/lib64/qt4/bin/uic --with-oldqt
    %else
        ./configure --with-build-dir=$RPM_BUILD_ROOT --with-install-dir=/usr --prefix=$RPM_BUILD_ROOT/usr --with-qtmoc=/usr/lib/qt4/bin/moc --with-qtrcc=/usr/lib/qt4/bin/rcc --with-qtuic=/usr/lib/qt4/bin/uic --with-oldqt
    %endif
%endif

%if 0%{?ubuntu_version}
    %ifarch x86_64
        ./configure --with-build-dir=$RPM_BUILD_ROOT --with-install-dir=/usr --prefix=$RPM_BUILD_ROOT/usr --with-qtmoc=moc-qt4 --with-qtrcc=rcc-qt4 --with-qtuic=uic-qt4
    %else
        ./configure --with-build-dir=$RPM_BUILD_ROOT --with-install-dir=/usr --prefix=$RPM_BUILD_ROOT/usr --with-qtmoc=moc-qt4 --with-qtrcc=rcc-qt4 --with-qtuic=uic-qt4
    %endif
%endif

make

%install
make install

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root)
%doc README TODO COPYING ChangeLog
/usr/share/applications/Aten.desktop
/usr/bin/aten
/usr/share/aten/

%changelog
* Wed Apr 02 2008 Tristan Youngs <tris@projectaten.net> 
- added checks to build on different distros with the SuSE build service.
* Tue Apr 01 2008 Tristan Youngs <tris@projectaten.net>
- added dependencies list and long description.
* Sun Mar 30 2008 Tristan Youngs <tris@projectaten.net>
- installation target points to local dir.
* Mon Mar 24 2008 Tristan Youngs <tris@projectaten.net> 
- initial version.
