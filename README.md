# comarkdown

This is a markup language specifically for writing portable,
Unicode-friendly math documents.

Basically, if you want to write a document with math, you have two
choices:

* Something like [Markdown][1], combined with something like
  [MathJaX][2]. This allows easy conversion to HTML, and not much
  else. 

  While this combination would allow simple math formatting, it's not
  powerful enough to support macros, mixins, or environments. This means
  that the author has to write a lot more boilerplate work, and
  therefore less content.

* [LaTeX][3], which supports macros and specialized environments, so you
  have a lot less boilerplate. Unfortunately, the only easy output
  formats for this are PDF and DVI, which are a bit painful to
  distribute.

There's also the problem where LaTeX was written eight years before the
Unicode standard was published, so it's very hard to write LaTeX in
non-Latin alphabets.

Comarkdown aims to fix all of this. It's a markup language that has simple,
intuitive syntax like Markdown, but allows for more complicated
constructs, such as environments, mixins, and macros.

The Comarkdown standard and/or parser have not been written yet, so this is
purely fantasy at the moment.

This project will hopefully become a usable, portable markup language,
along with a thorough standard.

## Inspiration

I am writing a math textbook, called
[*Learn You Some Algebras for Glorious Good!*][4]. I currently have to
publish the book as a PDF, because I can't convert my macro-heavy code
to anything else.

## Contact

* Bugs should be reported in the [GitHub issue tracker][5].
* Email: `peter@harpending.org`
* IRC: `pharpend` on FreeNode.

[1]: http://pandoc.org/demo/example9/pandocs-markdown.html
[2]: http://www.mathjax.org/
[3]: http://www.latex-project.org/
[4]: http://www.learnyou.org/
[5]: https://github.com/pharpend/comarkdown/issues
