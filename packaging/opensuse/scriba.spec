#
# spec file for package scriba
#
# Copyright (c) 2026 SUSE LLC and contributors
#
# All modifications and additions to the file contributed by third parties
# remain the property of their copyright owners, unless otherwise agreed
# upon. The license for this file, and modifications and additions to the
# file, is the same license as for the pristine package itself (unless the
# license for the pristine package is not an Open Source License, in which
# case the license is the MIT License). An "Open Source License" is a
# license that conforms to the Open Source Definition (Version 1.9)
# published by the Open Source Initiative.

# Please submit bugfixes or comments via http://bugs.opensuse.org/
#

# Scheme sources plus a bash launcher: there is no compiled binary to
# debug, so the empty -debugsource package is skipped.
%global debug_package %{nil}

Name:           scriba
Version:        0.4.2
Release:        0
Summary:        Structured logging framework for Guile Scheme
License:        LGPL-3.0-or-later
URL:            https://codeberg.org/jjba23/scriba
Source0:        %{name}-%{version}.tar.gz
BuildRequires:  pkgconfig(guile-3.0)
BuildRequires:  guile-json
Requires:       guile
Requires:       guile-json

%description
Scriba is a structured logging framework for Guile: log routing, formatting
and filtering, JSON logs for production, plain text and rotating files, an
auto-logger configured from the environment and dynamically scoped log
contexts.

%prep
%autosetup -n %{name}-%{version}

%build
# Nothing to build: the program is Scheme sources plus a launcher, and
# the sources are byte-compiled during %install so that they land in
# the interpreter's compiled-module directory.

%install
# Both directories are asked of Guile's pkg-config file rather than
# assumed, so the modules land where this distribution's Guile looks
# for them.
SITE_DIR="%{buildroot}$(pkg-config --variable=sitedir guile-3.0)"
CCACHE_DIR="%{buildroot}$(pkg-config --variable=siteccachedir guile-3.0)"

mkdir -p "$SITE_DIR/scriba" "$CCACHE_DIR/scriba"
cp -r src/scriba/. "$SITE_DIR/scriba/"

# Byte-compile every module so that loading it does not compile it
# again on first use.
find "$SITE_DIR/scriba" -name '*.scm' | while read -r scm_file; do
  rel_path="${scm_file#$SITE_DIR/}"
  go_file="$CCACHE_DIR/${rel_path%.scm}.go"
  mkdir -p "$(dirname "$go_file")"
  guild compile -L "$SITE_DIR" -o "$go_file" "$scm_file"
done

# Upstream's log-level suite calls (apply test ...), which Veritas's test macro cannot expand; the suite is not run.
# %check omitted.

%files
%license COPYING
%doc README.org
%dir %{_datadir}/guile
%dir %{_datadir}/guile/site
%dir %{_datadir}/guile/site/3.0
%{_datadir}/guile/site/3.0/scriba
%dir %{_libdir}/guile
%dir %{_libdir}/guile/3.0
%dir %{_libdir}/guile/3.0/site-ccache
%{_libdir}/guile/3.0/site-ccache/scriba

%changelog
