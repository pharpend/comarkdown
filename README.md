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

## Installation and Usage

### Installation

I haven't published a version yet, but you can build the development
version if you want. You need [Git][6], and [Stack][8].

    $ git clone git://github.com/pharpend/comarkdown.git
    $ cd comarkdown
    $ stack install

Using the old `cabal install` probably works, although I haven't tested
it.

You can run the test suite with `stack test`.

### Usage

The `comd` executable doesn't actually do anything yet, but here you go
nonetheless:

```
Usage: comd --version
  Compiler for Comarkdown

Available options:
  -h,--help                Show this help text
  --version                Show the version
```

### Using the Haskell library

I haven't put a version on Hackage, so there's no Haddock documentation
anywhere. **But**, you can read the source files found in
[`lib/Text/Comarkdown/*.hs`](lib/Text/Comarkdown/).

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
[6]: https://git-scm.com/book/en/v2/Getting-Started-Installing-Git
[7]: https://github.com/bitemyapp/learnhaskell/blob/master/install.md
[8]: https://github.com/commercialhaskell/stack/wiki/Downloads
