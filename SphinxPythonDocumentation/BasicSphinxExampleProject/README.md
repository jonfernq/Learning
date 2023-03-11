## Basic Sphinx (Example Project)

Looking for a simple template project to bootstrap a first project,
the [Basic Sphinx](https://github.com/readthedocs-examples/example-sphinx-basic/) project 
suggested at ReadTheDocs [Example Projects](https://docs.readthedocs.io/en/stable/examples.html) was chosen. 
Note, the description of steps below continues on from activating a virtual environment [here]().

So, after activating a local Python virtual environment first:

---
### Install required Python dependencies (Sphinx etc.)
```
pip install -r docs/requirements.txt
```

This command uses pip to install the Python dependencies required by a project, as specified in the requirements.txt file located in the docs directory.

The -r flag tells pip to read the list of dependencies from a file, instead of specifying them directly on the command line. The docs/requirements.txt argument specifies the path to the requirements.txt file containing the list of dependencies.

When you run this command, pip will read the requirements.txt file and install all the Python dependencies listed in the file. This is a convenient way to ensure that all the required dependencies are installed, without having to manually install each one separately.

The requirements.txt file typically includes a list of package names, along with their version numbers or version constraints, that are required for the project to run correctly. By installing the dependencies from this file, you can ensure that you have the correct versions of the required packages installed, which is important for maintaining compatibility and avoiding conflicts between different packages.

---
### Enter the Sphinx project
```
cd docs/
```

### Run the raw sphinx-build command
```
sphinx-build -M html . _build/
```

This is a Sphinx documentation generator command.

'sphinx-build' is the command-line tool that comes with Sphinx for building documentation. 
The -M option specifies the build mode, which in this case is 'html'. 
This means that Sphinx will build the documentation in HTML format.

The . in the command specifies the source directory where Sphinx will look for the documentation files.

The _build/ directory is the destination directory where Sphinx will output the generated documentation.

So, when you run the command sphinx-build -M html . _build/, 
Sphinx will look in the current directory for documentation files, build the documentation in HTML format, and output the generated files to the _build/ directory.

---
### Dealing with Error 

> Configuration error: config directory doesn't contain a conf.py file (D:\GITHUB_MY\SPHINX\example)

Even though this was set up ahead of time to work flawlessly, an error did occur and the fix is straightforward. 

The error message indicates that Sphinx is looking for the conf.py file in the wrong directory, which is causing the configuration error.

To resolve this issue, you can try specifying the path to the conf.py file explicitly when running the sphinx-build command. For example, if your conf.py file is located in the docs directory, you can run the following command from the root directory of your project:

```bash
sphinx-build -M html docs docs/_build
```

This command tells Sphinx to build the HTML documentation using the conf.py file located in the docs directory, and to output the generated files to the docs/_build directory.

If you still get the same error, you can check that the docs directory contains a conf.py file and that it is spelled correctly. You can also try specifying the full path to the conf.py file in the sphinx-build command to ensure that Sphinx can find it:

```bash
sphinx-build -M html /path/to/docs/conf.py docs/_build
```

By specifying the full path to the conf.py file, you can be sure that Sphinx will find it and the configuration error will be resolved.

### Shorter Command

You can also build the documentation locally with make:

Enter the Sphinx project
```
cd docs/
```

Build with make
```
make html
```

Open with your preferred browser, pointing it to the documentation index page
```
firefox _build/html/index.html
```


