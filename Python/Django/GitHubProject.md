## Django Projects 

### Running a Project from GitHub Repository 

> If one has a Django project folder from a GitHub repository what is the smallest and quickest set of commands to get it working? 

Assuming that you have already installed Django and its dependencies, here are the steps to get a Django project from a GitHub repository working on your local machine:

Clone the repository to your local machine using the following command:

```php
bash
git clone <repository URL>
```

Replace <repository URL> with the URL of the GitHub repository you want to clone.

Change your current directory to the project folder:

```php
bash
cd <project folder>
```


Replace <project folder> with the name of the folder you just cloned from the repository.

Create a virtual environment for the project:

```php
python -m venv <venv>
```

Replace <venv> with the name you want to give to your virtual environment.

Activate the virtual environment:

```bash
source <venv>/bin/activate
```
Replace <venv> with the name of the virtual environment you just created.

Install the project's dependencies:

```
pip install -r requirements.txt
```

This assumes that the project has a requirements.txt file with all its dependencies listed.

Run the migrations to create the database:

```
python manage.py migrate
```

Create a superuser account (optional):

```
python manage.py createsuperuser
```

Start the development server:

```
python manage.py runserver
```

That's it! You should now be able to access the project by visiting http://localhost:8000 in your web browser.

---
### Deactivate Pipenv Environment

> How do you deavctivate a pipenv environment? 

To deactivate a pipenv environment, follow these steps:

1. Open your terminal or command prompt.

2. Navigate to the directory that contains the pipenv environment you want to deactivate.

3. Run the following command to deactivate the pipenv environment:

```
pipenv shell deactivate
```

Alternatively, you can simply use the exit command to leave the shell session:

```bash
exit
``` 

4. You should now see that you have returned to your original shell environment and the pipenv environment is no longer active.

Note: It's important to deactivate the pipenv environment when you're done using it to prevent any conflicts with other projects or environments on your system.



