# wwwtex

By the way, if anyone can come up with a better name, do let me know.

This is a markup language specifically for writing portable,
Unicode-friendly math documents.

Basically, if you want to write a document with math, you have two
choices:

* Something like [Markdown][1], combined with something like
  [MathJaX][2]. This allows easy conversion to HTML, and not much else.

* [LaTeX][3], which supports macros and specialized environments, so you
  have a lot less boilerplate. Unfortunately, the only easy output
  format for this is a PDF or DVI.

There's also the problem where LaTeX was written eight years before the
Unicode standard was published, so it's very hard to write LaTeX in
non-Latin alphabets.

WWWTeX aims to fix all of this. It's a markup language that has simple,
intuitive syntax like Markdown, but allows for more complicated
constructs, such as mixins and macros.

The WWWTeX standard and/or parser have not been written yet, so this is
purely fantasy at the moment.

This project will hopefully become a usable, portable markup language.

## Inspiration

I am writing a math textbook, called
[*Learn You Some Algebras for Glorious Good!*][4]. I currently have to
publish the book as a PDF, because I can't convert my macro-heavy code
to anything else.

I'm writing this for myself.

[1]: http://pandoc.org/demo/example9/pandocs-markdown.html
[2]: http://www.mathjax.org/
[3]: http://www.latex-project.org/
[4]: http://www.learnyou.org/
