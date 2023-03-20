## Modules and Packages

A set of short answer questions with answers to thoroughly test knowledge of Python modules and packages and please number them as well. 

> What is a module in Python?

A module is a file containing Python definitions and statements. It can define functions, classes, and variables, and can be imported and used in other Python files.

> How do you import a module in Python?

You can import a module in Python using the import statement followed by the name of the module. For example, to import the math module, you can use the statement import math.

> What is a package in Python?

A package is a collection of related modules that are organized in a directory hierarchy. A package can contain sub-packages and modules.

> How do you create a package in Python?

To create a package in Python, you can create a directory with an __init__.py file. The directory should contain the modules and sub-packages of the package.

> How do you import a module from a package in Python?

You can import a module from a package in Python using the dot notation. For example, to import the mymodule module from the mypackage package, you can use the statement from mypackage import mymodule.

> What is the purpose of the __init__.py file in a package directory?

The __init__.py file in a package directory is executed when the package is imported. It can contain initialization code for the package, such as setting default values for variables or importing modules that are used by the package.

> What is the purpose of the __name__ variable in a module?

The __name__ variable in a module contains the name of the module. When a module is imported, the __name__ variable is set to the name of the module. If the module is the main program, the __name__ variable is set to the string "__main__".

> What is the difference between a local import and a global import in Python?

A local import is an import statement inside a function or a block of code. The imported module is only available within the function or block. A global import is an import statement outside of any function or block. The imported module is available throughout the entire program.

> How do you use the from ... import * statement in Python?

The from ... import * statement in Python imports all the names defined in the specified module or package into the current namespace. This can lead to name clashes and is generally discouraged.

> What is the purpose of the sys.path variable in Python?

The sys.path variable in Python contains a list of directories that the Python interpreter searches for modules when you import them. You can modify the sys.path variable to add directories that contain your own modules or packages.

