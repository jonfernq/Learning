## Todo List App (Django) 

Table of Contents: 
 
1. Virtual Environment: First, setup a virtual environment to work in. 
    * [Create Virtual Environment](#create_virtual_environment)
    * [Activate Virtual Environment](#activate_virtual_environment)
    * [Explanation_of Virtual Environments](#explanation_of_virtual_environments)
    * [Install Django](#install_django): Install Django in the new virtual environment.
    * [Requirements](#requirements): Save project dependencies into requirements.txt 
2. [Create Project](#create_project)
3. [Create App](#create_app)
4. [Directory Structure](#directory-structure): Familiarize oneself with standard directory structure. 
5. [Questions](#questions): Review and memorize with some questions. 

Create a simple todo-list application as a starter example project.

---
<a id="create_virtual_environment"></a>
### Create Virtual Environment

The virtual environment command ("venv (for Python 3)) allows you to manage separate package installations for different projects. 

Virtual environments essentially allow you to create a “virtual” isolated Python installation and install packages into that virtual installation.

When you switch projects, you can simply create a new virtual environment and not have to worry about breaking the packages installed in the other environments. 

It is always recommended to use a virtual environment while developing Python applications. (Source: [Python Documentation](https://packaging.python.org/en/latest/guides/installing-using-pip-and-virtual-environments/#creating-a-virtual-environment))

Here is the command: 
```
python -m venv env
```

The second argument 'env' is the location to create the virtual environment. 

Generally, you can just create this in your project and call it 'env' or 'myenv'.

In the command above, 'venv' will create a virtual Python installation in the 'env' folder. 

The command is issued in the project directory 'myproject' resulting in the directory structure:

```markdown
myproject/
    env/ 
        ....
        more sub-directories and files 
```

You should also exclude your virtual environment directory from your version control system using the .gitignore file.

---
<a id="activate_virtual_environment"></a>
### Activate Virtual Environment {#activate_virtual_environment}

Before you can start installing or using packages in your virtual environment you’ll need to activate it. 

Activating a virtual environment will put the virtual environment-specific python and pip executables into your shell’s PATH.

Activate the virtual environment by running the following command:

On Windows:

```
env\Scripts\activate.bat
```

On macOS or Linux:

```bash
source env/bin/activate
```

After running this command, your command prompt should change to indicate that you are now working within the virtual environment:

![env_cursor_change](https://user-images.githubusercontent.com/68504324/226779778-e08418b9-8043-4f13-acc4-32519676fe11.jpg)

You can confirm you’re in the virtual environment by checking the location of your Python interpreter:

```
where python
```

This should output the paths to your Python executables, including the path inside your virtual environment:

![where_python](https://user-images.githubusercontent.com/68504324/226780642-685ca533-2f2d-4d91-bafc-7785009e075b.jpg)

As long as your virtual environment is activated pip will install packages into that specific environment 
and you’ll be able to import and use packages in your Python application.

When you want to exit the virtual environment, run the 'deactivate' command.

Next step is to get to work on your project. 

---
<a id="explanation_of_virtual_environments"></a>
### Explanation of VIrtual Environments  

> So using virtual environments means the path environmental variable is redirected to your virtual environment, using the version of Python there and the packages that have been installed there? 

Yes, that's correct. When you activate a virtual environment, the PATH environment variable is temporarily modified to include the path to the Python executable and any other executables specific to that virtual environment. This means that any commands you run, including running Python scripts, will use the version of Python and the packages installed in that virtual environment, rather than the global Python installation.

In addition to modifying the PATH variable, activating a virtual environment also modifies the shell prompt to indicate that you are working within a virtual environment. This helps avoid confusion and makes it easier to keep track of which virtual environment you are currently using.

Overall, using virtual environments is a great way to manage dependencies for your Python projects, since it allows you to keep the dependencies for each project separate and avoid conflicts between different projects that may require different versions of the same package.

---
> However, if dependencies are not present at the local virtual environment level, then they will be searched for in the global environment ? 

Yes, that's correct. When you activate a virtual environment, any packages or libraries you install will be installed within that virtual environment, and will be available only within that environment. If a package or library is not installed within the virtual environment, then Python will search for it in the global environment.

This means that if you are working within a virtual environment and try to import a package or library that is not installed within that environment, Python will search for it in the global environment. If the package or library is not installed in the global environment either, then you will get an error.

This is why it's important to make sure that you activate the correct virtual environment before running your Python code or installing packages, and to ensure that all the required packages are installed within the virtual environment. You can use tools like pip to install packages within a virtual environment, and it's a good practice to create a requirements.txt file to keep track of the packages and versions required for your project.

---
<a id="install_django"></a>
### Install Django 

You need to install Django for every virtual environment you create because virtual environments are isolated Python environments, which means they have their own Python executable and their own set of installed packages.

When you create a new virtual environment, it is essentially a fresh Python installation with no packages installed. This means that you need to install Django and any other packages you need for that specific project within the virtual environment.

To install Django within a virtual environment, activate the virtual environment and then use pip to install Django.

The short form of the command is 'pip install django'. This will install the latest version of Django in your virtual environment, which you can then use for your project. It's generally considered good practice to use the longer, more explicit form of the pip command when installing packages in Python virtual environments: 

```
python pip install django~=4.0.0
```

Here's why:

- **Ensuring the correct version of Python is used:** When you use the longer form of the pip command (python -m pip install <package>), you are explicitly telling Python to use the pip module that is associated with the current Python installation. This can be important if you have multiple versions of Python installed on your system, or if you are using a virtual environment with a specific version of Python.

- **Specifying the package version:** By specifying the package version using the comparison operator ~=, you are telling pip to install a version of the package that is compatible with the version you have specified. In this case, ~=4.0.0 means "install version 4.x.x of Django, where x can be any number, as long as it is greater than or equal to 0." This ensures that you get the latest security updates for version 4 of Django, without breaking any existing code that relies on the 4.x.x version.

- **Reproducibility:** By specifying the package version and using the longer form of the pip command, you are ensuring that your virtual environment is reproducible. That is, anyone else who creates a virtual environment using the same package version and pip command will get the same environment as you. This is important for collaborative projects, where multiple developers need to work on the same codebase.

In summary, using the longer form of the pip command with the comparison operator is a best practice that ensures you get the correct version of a package installed in your virtual environment, while also ensuring reproducibility and compatibility with other developers.

---
 <a id="requirements"></a>
### requirements.txt

It's a good practice to keep track of the packages you have installed in each virtual environment, so you can easily reproduce the same environment on another machine or share it with other developers. You can do this by creating a requirements.txt file that lists all the packages installed in the virtual environment, and then using that file to install the same packages in another environment. You can create a requirements.txt file by running the following command:

```
pip freeze > requirements.txt
```
 
This will create a requirements.txt file in the current directory, which lists all the packages installed in the virtual environment. You can then use this file to install the same packages in another environment by running the following command:

```
pip install -r requirements.txt
```
 
--- 
<a id="create_project"></a>
### Create Project

Now to start creating the project itself, create a new Django project by running the following command in the project directory:

```
django-admin startproject myproject
```
---
> So i am a little confused, the Django command: "django-admin startproject myproject" creates two sub-directories named 'myproject" , so you issue this command in the parent directory of the project directory you are creating ? 

Yes, that's correct. When you run the command django-admin startproject myproject, it will create a new Django project in a subdirectory named myproject. This subdirectory will contain a number of files and directories, including a manage.py file and another subdirectory with the same name as the project.

You should run this command in the parent directory where you want the myproject directory to be created. For example, if you want to create the myproject directory in 'home_directory', you would run the command in the terminal while you are in 'home_directory':

```bash
cd D:\home_directory
django-admin startproject myproject
```

This will create a new directory named myproject in your home directory, containing the files and directories needed for a new Django project.

---
 <a id="create_app"></a>
### Create App

After you create a project, then create a new Django app within your project by running the following command:

```
python manage.py startapp todo
```

Create a new model in your todo/models.py file to define the structure of your to-do list items:

```python

from django.db import models

class TodoItem(models.Model):
    title = models.CharField(max_length=200)
    description = models.TextField()
    completed = models.BooleanField(default=False)

    def __str__(self):
        return self.title
```

This model defines a to-do list item with a title, description, and completion status.

Create a new view in your todo/views.py file to display a list of to-do items:

```python
from django.shortcuts import render
from .models import TodoItem

def todo_list(request):
    items = TodoItem.objects.all()
    return render(request, 'todo/todo_list.html', {'items': items})
```

This view fetches all the to-do items from the database and passes them to a template for rendering.

Create a new template in your todo/templates/todo/todo_list.html file to display the list of to-do items:

```html
{% extends 'base.html' %}

{% block content %}
  <h1>To-Do List</h1>
  <ul>
    {% for item in items %}
      <li>{{ item.title }} - {{ item.description }} - {{ item.completed }}</li>
    {% endfor %}
  </ul>
{% endblock %}
```

This template extends a base template and loops through all the to-do items passed from the view, displaying each item's title, description, and completion status.

Add a new URL pattern in your todo/urls.py file to route requests to your new view:

```python
from django.urls import path
from . import views

urlpatterns = [
    path('', views.todo_list, name='todo_list'),
]
```

This URL pattern maps requests to the root URL (/) to your todo_list view.

Finally, add your new app and its URL patterns to your project's main urls.py file:

```python
from django.contrib import admin
from django.urls import path, include

urlpatterns = [
    path('admin/', admin.site.urls),
    path('', include('todo.urls')),
]
```

This includes your todo app's URL patterns under the root URL (/), so requests to that URL will be handled by your todo_list view.

That's it! You should now be able to run your Django app with the following command:

```
python manage.py runserver
```

And visit http://localhost:8000/ in your browser to see your simple to-do list app in action.

---
<a id="directory_structure"></a>
### Directory Structure

> In the instructions above the following files were specified: 

- todo/models.py
- todo/views.py
- todo/urls.py
- todo/templates/todo/todo_list.html

Please describe the directory structure for the project and app,
where the files are located in this directory structure,
and also the directory where commands are issued.  

---
Here's an explanation of the directory structure for a Django project and app, based on the files specified in the previous question:

Assuming you have created a Django project named myproject, the directory structure for the project would look like this:

```markdown
myproject/
    manage.py
    myproject/
        __init__.py
        settings.py
        urls.py
        asgi.py
        wsgi.py
```

Here's what each of these files represents:

- **manage.py:** A command-line utility that allows you to interact with your Django project. You'll run commands like python manage.py runserver from the command line to start your development server, run database migrations, and more.
- **myproject/:** This is the directory that contains the actual project code. It's also the top-level Python package for the project, which means that any modules or apps that you create will be located inside this directory.
- **myproject/__init__.py:** An empty file that tells Python to treat the myproject directory as a Python package.
- **myproject/settings.py:** A Python module that contains all the settings for your Django project, such as database configuration, installed apps, middleware, and more.
- **myproject/urls.py:** A Python module that contains the URL configuration for your project. This file is responsible for mapping URLs to views.
- **myproject/asgi.py and myproject/wsgi.py:** Two Python modules that are used to run your Django project as a web application. These files are used by servers like Gunicorn or uWSGI to start your application and handle incoming requests.

Assuming you have also created a Django app named todo, the directory structure for the app would look like this:

```markdown
myproject/
    manage.py
    myproject/
        __init__.py
        settings.py
        urls.py
        asgi.py
        wsgi.py
    todo/
        __init__.py
        admin.py
        apps.py
        models.py
        views.py
        urls.py
        migrations/
        templates/
            todo/
                todo_list.html
```

Here's what each of these files and directories represents:

- **todo/:** This is the directory that contains the app code. It's located inside the project directory and is treated as a Python package.
- **todo/__init__.py:** An empty file that tells Python to treat the todo directory as a Python package.
- **todo/admin.py:** A Python module that defines the admin interface for the app. This file is responsible for registering models with the Django admin site.
- **todo/apps.py:** A Python module that defines the configuration for the app. This file is responsible for telling Django about the app and its configuration.
- **todo/models.py:** A Python module that defines the data models for the app. This file is responsible for defining the fields and relationships that are used to store and retrieve data from the database.
- **todo/views.py:** A Python module that defines the views for the app. This file is responsible for defining the functions or classes that handle incoming requests and generate responses.
- **todo/urls.py:** A Python module that defines the URL patterns for the app. This file is responsible for mapping URLs to views.
- **todo/migrations/:** A directory that contains the database migration files for the app. These files are used to create and modify database tables and fields.
- **todo/templates/:** A directory that contains the HTML templates for the app.
- **todo/templates/todo/todo_list.html:** A specific HTML template file that defines how to display a to-do list.

Finally, the directory where commands are issued is typically the project directory (`myproject/)

---
<a id="questions"></a> 
### Questions

Here are some multiple choice questions to test knowledge of Django:

1. What is the purpose of an app in a Django project?
- a) To define the overall structure of the project
- b) To provide specific functionality
- c) To manage the project's database
- d) To handle user authentication

Answer: b) To provide specific functionality

2. What is the purpose of models in a Django project?
- a) To define the structure of data and how it is stored in the database
- b) To define how data is displayed to the user
- c) To define how requests are routed to views
- d) To define the configuration options for the project

Answer: a) To define the structure of data and how it is stored in the database

3. What is the purpose of views in a Django project?
- a) To define the structure of data and how it is stored in the database
- b) To define how data is displayed to the user
- c) To define how requests are routed to views
- d) To define the configuration options for the project

Answer: b) To define how data is displayed to the user

4. What is the purpose of templates in a Django project?
- a) To define the structure of data and how it is stored in the database
- b) To define how data is displayed to the user
- c) To define how requests are routed to views
- d) To define the configuration options for the project

Answer: b) To define how data is displayed to the user

5. What is the purpose of URLs in a Django project?
- a) To define the structure of data and how it is stored in the database
- b) To define how data is displayed to the user
- c) To define how requests are routed to views
- d) To define the configuration options for the project

Answer: c) To define how requests are routed to views









