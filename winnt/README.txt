This directory contains files to compile Gauche with VisualC++ on Windows.
As of 0.8.12, the core and several modules work, though we still need to
tweak a few major modules (notably, gauche.net).

If you get the source tree from the standard tarball (Gauche-x.x.x.tgz)
or from CVS, you have to preprocess the source tree.  You need a unix-
compatible environment (such as cygwin) to do that.

If you build from CVS, run this command at the toplevel dir:

$ ./DIST winvc

If you build from tarball, run this command at the toplevel dir:

$ sh winnt/winvc-prep.sh


[HOW TO BUILD]

Open Gauche.sln and run "build solution".

[HOW TO TEST]


[HOW TO INSTALL]



[HOW TO ADD NEW EXTENSION PROJECT]

We need to have separate project files per each extension dll.  If the
main distribution added new extensions in its bundle, corresponding 
vcproj file(s) should be added.

The project file name should be ext-something, where "something"
is the extension directory name (i.e. the source is under ext/something).
If the extension generates more than one dll, we need separate project
files for each.  Each project file should be named like ext-something-xxx.
The examples are ext-digest-md5 and ext-digest-sha1.

Here's the procedure.

First, open the solution file Gauche.sln and add a new project.  Select
Win32 project and put the appropriate name.  We put the project file
ext-something.vcproj in winnt/, the same directory as Gauche.sln.

In Win32 Application Wizard, choose DLL as application type and
check Empty project.  Then finish the wizard.
At this moment, Visual Studio creates a project file as
ext-something/ext-something.vcproj.  We don't want the extra ext-something
directory, though.  We quit VS and move ext-something.vcproj under
winnt manually, and adjust pathname in Gauche.sln.  If anybody knows
a better way, let us know.

Open VS again.  We don't use the default folders (Header Files etc.)
Just delete them.  Then add the necessary source.  (Make sure to do
this after vcproj path adjustment, or all the relative pathnames will
be messed up).

A project property sheet is created to keep the common settings.
Open property manager, select ext-something project, and choose
"Add Existing Property Sheet" from right-click menu.  Open ext.vsprops
file (it's under winnt directory).

The open the property of ext-something project.  At least you have to
change the following part:

- Add reference to libgauche (and ext-uvector if necessary).
- In Configuration Properties - General,
-- Output Directory: Choose <inherit from parent or project defaults>
-- Intermediate Directory: Choose <inherit from parent or project defaults>
- In Configuration Properties - Linker - General,
-- Set output file to $(OutDir)\something.dll  (substitute "something" 
   to the desirable dll name).  The default of this field is 
   $(OutDir)\$(ProjectName).dll, but we surely don't want ext-something.dll.

It should now compile.






