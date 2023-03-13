## Markdown Help

> How does one play audio in a github markdown page? Where does one store this audio? 

GitHub markdown pages do not support embedding audio files directly. You will need to upload the audio file to a hosting service that supports direct linking, such as SoundCloud or Dropbox. Once the audio file is uploaded, you can get the direct link to the file and use it to create an audio player in your markdown page.

Here's an example of how to create an audio player in Markdown using the HTML5 audio tag and a direct link to an audio file:

```bash
<audio controls>
  <source src="https://example.com/audiofile.mp3" type="audio/mpeg">
  Your browser does not support the audio element.
</audio>
```  

In this example, replace the src attribute with the direct link to your audio file.

You can also use services like GitHub Pages or Netlify to host your audio files and create a webpage with the audio player embedded.

---
> Is there an online markdown editor at GitHub that saves directly to the repository or do i have to cut and paste from another editor? 

Yes, GitHub provides an online Markdown editor that allows you to edit Markdown files directly in your repository without the need to copy and paste from another editor. To access the editor, you can follow these steps:

Go to your GitHub repository and navigate to the file you want to edit.

Click on the pencil icon on the right side of the file to open the file editor.

The file editor should automatically detect that the file is written in Markdown, and display the file contents in the Markdown editor mode. You can edit the file using the editor, which provides formatting options and a preview window to preview the rendered Markdown.

Once you have made your changes, click the "Commit changes" button at the bottom of the editor to save your changes to the repository.

Note that the online Markdown editor provided by GitHub may not have all the features and functionality of a dedicated Markdown editor, but it can be a convenient and easy way to make quick edits to Markdown files in your repository.

---
> The backtick key on my computer seems to be broken and i need to insert three backticks in markdown all the time, is there an easy way to do this ? 

Yes, there are a few ways to insert backticks into Markdown without using the backtick key:

Use the on-screen keyboard: You can use the on-screen keyboard on your computer to insert backticks. In Windows, go to Start menu > Windows Accessories > On-Screen Keyboard, and then click the backtick key on the keyboard.

Copy and paste: You can also copy and paste a backtick character from another source, such as a web page or document, and paste it into your Markdown document.

Use the code formatting button: Many Markdown editors have a button or shortcut for inserting code formatting, which typically adds backticks around the selected text. For example, in the Markdown editor used on Stack Exchange sites, you can select the text you want to format as code and click the "code" button or use the keyboard shortcut Ctrl+K.

Use a text expansion tool: You can also use a text expansion tool, such as TextExpander or AutoHotkey, to automatically replace a certain phrase or key combination with three backticks. For example, you could set up a text expansion rule that automatically replaces the phrase "`" with "```" whenever you type it.

These are just a few options, and there may be other methods that work better for your specific situation.
