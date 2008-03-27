# Defines a constant that is used later by the spec file.
%define shortname aten

# Name, brief description, and version 
Summary: Aten - Atomic configuration builder and editor
Name: %{shortname}
Version: 0.95
Release: 3
License: GPL
%define fullname %{name}-%{version}

# Software group
Group: Applications/Editors

# Source tar ball.
Source: %{fullname}.tar.gz

# Location of the project's home page.
URL: http://www.projectaten.org

# Owner of the product.
Vendor: T. Youngs

# Packager of the product.
Packager: T. Youngs

# Allows you to specify a directory as the root for building and installing the
# new package (from http://www.rpm.org/RPM-HOWTO/build.html).
BuildRoot : %{_tmppath}/%{fullname}

# Boolean that specifies if you want to automatically determine some
# dependencies.
AutoReqProv: yes

BuildRequires: bzip2

# In-depth description.
%description
Aten does some stuff.
It really does.

%prep
%setup -q

%build
./autogen.sh
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
* Mon Mar 24 2008 Tristan Youngs <tris@projectaten.com> 
- initial version.
