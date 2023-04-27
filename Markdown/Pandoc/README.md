## Pandoc

> Using Pandoc can I format GitHub markdown in to a pdf or word file ?

Yes, you can use Pandoc to convert GitHub markdown to a PDF or Word document. Pandoc is a versatile command-line tool that can convert files from one format to another, including converting GitHub-flavored markdown to PDF or Word.

Here's an example command to convert a GitHub markdown file to a PDF using Pandoc:

```lua

pandoc input.md -o output.pdf
```



This will convert the file `input.md` to a PDF file named `output.pdf`.

Similarly, you can convert a GitHub markdown file to a Word document using Pandoc with this command:

```lua

pandoc input.md -o output.docx
```



This will convert the file `input.md` to a Word document named `output.docx`.

Note that you may need to install additional software or libraries to convert to certain formats, such as LaTeX for PDF conversion.
