## Virtual Environments

"venv (for Python 3)... allow you to manage separate package installations for different projects. 
They essentially allow you to create a “virtual” isolated Python installation and install packages into that virtual installation. 
When you switch projects, you can simply create a new virtual environment and not have to worry about breaking the packages installed in the other environments. 
It is always recommended to use a virtual environment while developing Python applications." 
(Source: [Python Documentation](https://packaging.python.org/en/latest/guides/installing-using-pip-and-virtual-environments/#creating-a-virtual-environment))

```
python3 -m venv env
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

As long as your virtual environment is activated pip will install packages into that specific environment and you’ll be able to import and use packages in your Python application.

If you want to exit the virtual environment later, run the deactivate command.
