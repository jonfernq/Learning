## Sphinx Deployment to GitHub

- [Deploy Sphinx documentation to GitHub Pages](https://coderefinery.github.io/documentation/gh_workflow/#exercise-deploy-sphinx-documentation-to-github-pages): Detailed Instructions

"In this exercise we will create an example repository on GitHub and deploy it to GitHub Pages. 
The example project contains a script for counting the frequency distribution of words 
in a given file and some documentation generated using Sphinx. For bigger projects, we can have more source files."

---
- **Step 1:** Go to the documentation-example project template on GitHub and create a copy to your namespace (“Generate”, since this is a template repository):

Either by downloading or by: Use this template/Create a New respository

---
- **Step 2:** Clone the repository

 "Cloning a repository" means creating a local copy of a remote repository, which is usually hosted on a code sharing platform like GitHub, GitLab, or Bitbucket. 
 This allows you to work on the codebase and make changes locally before pushing those changes back to the remote repository.

To clone a remote repository using Git bash, you can use the git clone command followed by the URL of the repository you want to clone. Here's an example:

 ```bash
git clone https://github.com/username/repo.git
 ```
 
Replace username with the username of the repository owner and repo with the name of the repository you want to clone.

This will create a local copy of the remote repository in a directory with the same name as the repository. 
You can then navigate to this directory using the cd command and start working with the files in the repository.

The repository contains following two folders, among few other files:

- source folder contains the source code

- doc folder contains the Sphinx documentation

The doc folder contains the Sphinx configuration file (conf.py) and the index file (index.rst) and some contents (Markdown files). The conf.py file has been adjusted to be able to autogenerate documentation from sources.

---
-- **Step 3:** Build HTML pages locally

Inside the cloned repository, build the documentation and verify the result in your browser:

```bash
sphinx-build doc _build
```

**Error Message:** no theme named 'sphinx_rtd_theme' found (missing theme.conf?)

This error message usually indicates that Sphinx is unable to locate the specified theme, in this case, sphinx_rtd_theme.

sphinx_rtd_theme is a popular theme for Sphinx documentation, and it needs to be installed separately. To install it, you can use pip, the package manager for Python.

Open a terminal or Git Bash and type the following command:

Copy code
pip install sphinx-rtd-theme
This will install the theme, and once it's installed, you can specify it in your conf.py file. Add the following line to the conf.py file:

```python
html_theme = 'sphinx_rtd_theme'
```

Save the file and try running make html again. The error message should be resolved.

---
- **Step 4:** Test HTML pages links

Inside the cloned repository, check the integrity of all internal and external links:

```
sphinx-build doc -W -b linkcheck -d _build/doctrees _build/html
```

---
- **Step 5:** Add the GitHub Action

Create a new file at .github/workflows/documentation.yaml with the contents

```
name: Docs
on: [push, pull_request, workflow_dispatch]
permissions:
    contents: write
jobs:
  docs:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions/setup-python@v3
      - name: Install dependencies
        run: |
          pip install sphinx sphinx_rtd_theme
      - name: Sphinx build
        run: |
          sphinx-build doc _build
      - name: Deploy
        uses: peaceiris/actions-gh-pages@v3
        if: ${{ github.event_name == 'push' && github.ref == 'refs/heads/main' }}
        with:
          publish_branch: gh-pages
          github_token: ${{ secrets.GITHUB_TOKEN }}
          publish_dir: _build/
          force_orphan: true
```

You don’t need to understand all of the above, but you might spot familiar commands in the run: sections.

Add, commit and push to GitHub

Check the action at https://github.com/<myuser>/documentation-example/actions. Replace <myuser> with your GitHub username.

 ---
- **Step 6:** Step 2: Enable GitHub Pages

Go to https://github.com/<myuser>/documentation-example/settings/pages

In the “Source” section, choose “gh-pages” in the dropdown menu and click save

(You should be able to verify the pages deployment in the Actions list)

- **Verify the result**

That’s it! Your site should now be live on https://<myuser>.github.io/documentation-example/ (replace username).

Verify refreshing the documentation

- **Finally, make some changes to your documentation**

Add documentation related to other functions

Prerequisites and how to use the program

Rules for contribution

Some example results (figures, tables, …)

Commit and push them, and verify that the documentation website refreshes after your changes (can take few seconds or a minute)


