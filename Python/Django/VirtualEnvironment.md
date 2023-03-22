## Virtual Environments

### Creating a Virtual Environment

"venv (for Python 3)... allow you to manage separate package installations for different projects. 
They essentially allow you to create a “virtual” isolated Python installation and install packages into that virtual installation. 
When you switch projects, you can simply create a new virtual environment and not have to worry about breaking the packages installed in the other environments. 
It is always recommended to use a virtual environment while developing Python applications." 
(Source: [Python Documentation](https://packaging.python.org/en/latest/guides/installing-using-pip-and-virtual-environments/#creating-a-virtual-environment))

```
python -m venv env
```

The second argument is the location to create the virtual environment. 
Generally, you can just create this in your project and call it env.

venv will create a virtual Python installation in the env folder.

You should exclude your virtual environment directory from your version control system using .gitignore or similar.

### Activating a virtual environment

Before you can start installing or using packages in your virtual environment you’ll need to activate it. 
Activating a virtual environment will put the virtual environment-specific python and pip executables into your shell’s PATH.

Activate the virtual environment by running the following command:

On Windows:

```
myenv\Scripts\activate.bat
```

On macOS or Linux:

```bash
source myenv/bin/activate
```

After running this command, your command prompt should change to indicate that you are now working within the virtual environment.

You can confirm you’re in the virtual environment by checking the location of your Python interpreter:

```
which python
```

This should output the path to the Python executable inside your virtual environment. 

As long as your virtual environment is activated pip will install packages into that specific environment 
and you’ll be able to import and use packages in your Python application.

When you want to exit the virtual environment, run the deactivate command.

Next step is to get an [example project](https://github.com/jonfernq/Learning/tree/main/SphinxPythonDocumentation/BasicSphinxExampleProject) working.

---
### Explanation: 

> So using virtual environments means the path environmental variable is redirected to your virtual environment, using the version of Python there and the packages that have been installed there? 

Yes, that's correct. When you activate a virtual environment, the PATH environment variable is temporarily modified to include the path to the Python executable and any other executables specific to that virtual environment. This means that any commands you run, including running Python scripts, will use the version of Python and the packages installed in that virtual environment, rather than the global Python installation.

In addition to modifying the PATH variable, activating a virtual environment also modifies the shell prompt to indicate that you are working within a virtual environment. This helps avoid confusion and makes it easier to keep track of which virtual environment you are currently using.

Overall, using virtual environments is a great way to manage dependencies for your Python projects, since it allows you to keep the dependencies for each project separate and avoid conflicts between different projects that may require different versions of the same package.

---
> However, if dependencies are not present at the local virtual environment level, then they will be searched for in the global environment ? 

Yes, that's correct. When you activate a virtual environment, any packages or libraries you install will be installed within that virtual environment, and will be available only within that environment. If a package or library is not installed within the virtual environment, then Python will search for it in the global environment.

This means that if you are working within a virtual environment and try to import a package or library that is not installed within that environment, Python will search for it in the global environment. If the package or library is not installed in the global environment either, then you will get an error.

This is why it's important to make sure that you activate the correct virtual environment before running your Python code or installing packages, and to ensure that all the required packages are installed within the virtual environment. You can use tools like pip to install packages within a virtual environment, and it's a good practice to create a requirements.txt file to keep track of the packages and versions required for your project.


