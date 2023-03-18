## GitHub Flavored Markdown (GFM)

Notes on using GFM.

- [Transform ChatGPT text to Markdown](https://github.com/jonfernq/Learning/blob/main/Markdown/GitHubFlavoredmarkdown/CleanUpChatGPTText.md) 

---
### HTML in Markdown Files

Using HTML can make text look nicer. 

For example, anchor tags for navigation within a page apparently look nicer with inline HTML. 

---
> But the Markdown "# Section 1" looks better than "#section-1" , what can be done?  

If you prefer to use a heading with spaces in the Markdown document, 
you can add an id attribute to the heading in your HTML code that matches the link anchor in your Markdown. The id attribute can contain spaces, and it will be used as the anchor target for your link.

For example, if you have the following Markdown link:

```markdown
[Link to Section 1](#section-1)
```

You can add an id attribute to the corresponding heading in your HTML code like this:

```html
<h2 id="section-1">Section 1</h2>
```

This will create a heading that displays as "Section 1" in the rendered page, but can still be linked to with the #section-1 anchor tag in your Markdown link.

---
> Put HTML code in a Markdown file? 

Yes, you can include HTML code in a Markdown file by simply writing the HTML code directly in the Markdown document. When the Markdown file is rendered, the HTML code will be treated as regular HTML and will be displayed as such.

For example, if you want to include an HTML code snippet for a button in a Markdown file, you can write the HTML code directly in the Markdown file like this:

```markdown
Here's an example button:

<button class="btn btn-primary">Click me!</button>
```

When this Markdown file is rendered, the button HTML code will be displayed and rendered as a button element.

It's important to note that when including HTML code in a Markdown file, you should ensure that the code is well-formed and valid HTML, otherwise it may not be displayed correctly. Additionally, some Markdown renderers may restrict or sanitize the HTML code that can be included for security reasons, so it's a good idea to check the documentation for your specific Markdown renderer to understand any limitations or restrictions.

---
> So this button would activate some JavaScript code, where would this be? In the Markdown file or an external file pointed to from the Markdown file? 
If you want to include JavaScript code that is triggered by the button in your Markdown file, it's generally a better practice to put the JavaScript code in an external file rather than including it directly in the Markdown file.

To do this, you can create a separate .js file that contains your JavaScript code, and then include it in the HTML code of your Markdown file using a <script> tag. Here's an example:

```markdown
Here's an example button:

<button class="btn btn-primary" id="myButton">Click me!</button>

<script src="path/to/my/script.js"></script>
```

In this example, the button HTML code is included in the Markdown file as before, but the JavaScript code is stored in an external script.js file, which is referenced in the Markdown file using a <script> tag. When the Markdown file is rendered, the JavaScript code will be loaded from the external file and will be triggered when the button is clicked.

By separating your JavaScript code into a separate file, you can keep your Markdown file clean and easy to read, and it also makes it easier to reuse your JavaScript code across multiple pages if needed.


