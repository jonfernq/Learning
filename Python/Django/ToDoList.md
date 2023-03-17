## Todo List App (Django) 

Table of Contents: 
- [Code](#code)
- [Directory Structure](#directory-structure)
- [Questions](#questions)

---
<h3 id="code">Code</h3>

> For a simple to-do list app in Django, please provide all the files and code needed. 

Here's an example of a simple to-do list app in Django with all the necessary files and code:

First, create a new Django project by running the following command in your terminal:

```
django-admin startproject myproject
```

Next, create a new Django app within your project by running the following command:

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
### Directory Structure
<h3 id="directory-structure">Directory Structure</h3>

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
<h3 id="questions">Questions</h3>

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









