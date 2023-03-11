## Basic Sphinx (Example Project)

Looking for a simple template project to bootstrap a first project,
the [Basic Sphinx](https://github.com/readthedocs-examples/example-sphinx-basic/) project 
suggested at ReadTheDocs [Example Projects](https://docs.readthedocs.io/en/stable/examples.html) was chosen. 
Note, the description of steps below continues on from activating a virtual environment [here]().

So, after activating a local Python virtual environment first:

---
# Install required Python dependencies (Sphinx etc.)
pip install -r docs/requirements.txt

This command uses pip to install the Python dependencies required by a project, as specified in the requirements.txt file located in the docs directory.

The -r flag tells pip to read the list of dependencies from a file, instead of specifying them directly on the command line. The docs/requirements.txt argument specifies the path to the requirements.txt file containing the list of dependencies.

When you run this command, pip will read the requirements.txt file and install all the Python dependencies listed in the file. This is a convenient way to ensure that all the required dependencies are installed, without having to manually install each one separately.

The requirements.txt file typically includes a list of package names, along with their version numbers or version constraints, that are required for the project to run correctly. By installing the dependencies from this file, you can ensure that you have the correct versions of the required packages installed, which is important for maintaining compatibility and avoiding conflicts between different packages.

---
# Enter the Sphinx project
cd docs/

# Run the raw sphinx-build command
sphinx-build -M html . _build/

This is a Sphinx documentation generator command.

'sphinx-build' is the command-line tool that comes with Sphinx for building documentation. 
The -M option specifies the build mode, which in this case is 'html'. 
This means that Sphinx will build the documentation in HTML format.

The . in the command specifies the source directory where Sphinx will look for the documentation files.

The _build/ directory is the destination directory where Sphinx will output the generated documentation.

So, when you run the command sphinx-build -M html . _build/, 
Sphinx will look in the current directory for documentation files, build the documentation in HTML format, and output the generated files to the _build/ directory.

