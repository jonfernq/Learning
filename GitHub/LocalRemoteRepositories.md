## Local & Remote Repositories

### Local Repositories

> How does one set up a local repository? What are the steps?  

Here are the basic steps to set up a local repository:

- 1. Create a new directory on your local machine where you want to store the repository.

- 2. Navigate to the directory using the command line or terminal.

- 3. Initialize the directory as a Git repository by running the command git init.

- 4. Add files to the repository by copying them into the directory or creating new files within the directory.

- 5. Stage the changes you want to commit by running the command git add followed by the name of the file(s) or the . character to stage all changes.

- 6. Commit the changes by running the command git commit -m "Commit message here". Be sure to include a descriptive commit message that summarizes the changes you made.

At this point, you have set up a local repository and committed your changes. You can continue working on the project and making changes, committing them as needed. To share your changes with others or back them up to a remote repository like GitHub, you can push your changes to the remote repository using the git push command.

---
### Add all files to local repository 

> What if there is one directory of files and you are adding all of them to the local repository? What the actual commands to be issued?  

If you have a directory of files that you want to add to a local Git repository, you can use the following commands:

Navigate to the directory that contains the files you want to add.

```bash
cd /path/to/directory
```

Initialize a new Git repository in the current directory.

```csharp
git init
```

Add all the files in the directory to the Git index.

```csharp
git add .
```

This command stages all the changes in the directory, including new files, modifications to existing files, and deletions.

Commit the changes to the local repository.

```sql
git commit -m "Initial commit"
```

This command creates a new commit with a message describing the changes you made.

After following these steps, your directory of files should now be added to the local Git repository. You can use git status to check the status of your repository and git log to view the commit history.

---
### Remote Repositories

> What are the steps taken to get the local repository on one's machine into the remote repository on github ? 

To push the local repository on your machine to the remote repository on GitHub, you can follow these steps:

- 1. Open Git Bash or a terminal window and navigate to the local repository directory using the cd command.

- 2. Use the git remote -v command to verify that you have a remote repository set up for the local repository.

- 3. If you haven't already done so, set up the remote repository on GitHub by creating a new repository or forking an existing repository.

- 4. Use the git add . command to add all the changes you want to push to the remote repository.

- 5. Use the git commit -m "Commit message" command to commit the changes you added.

- 6. Use the git push command to push the committed changes to the remote repository on GitHub. You may need to specify the remote repository URL and the branch name if you haven't set them up already.

- 7. Enter your GitHub username and password (or personal access token) when prompted to authenticate the push to the remote repository.

After following these steps, the changes you made in the local repository should be reflected in the remote repository on GitHub.








