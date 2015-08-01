\xtb{Comarkdown}

Comarkdown is a preprocessor for plain-text documents of any format,
although it was originally intended for [Pandoc's Markdown][1]. At the moment,
it's very simple, and not even functional.

\bold-face{txt=Installation}

I recommend you use [Stack][2] to install this. I haven't published a
version on Hackage yet, so you'll also need [Git][3] to download the
source code.

\ignore

foo bar

\moreshit
\unignore
    git clone git://github.com/pharpend/comarkdown.git
    cd comarkdown
    stack build

There isn't an executable yet, but you can run the test suite with
`stack test`.

## Contact

* Bug reports: <https://github.com/pharpend/comarkdown/issues/new>
* Email: `peter@harpending.org`
* IRC: `pharpend` on FreeNode

[1]: http://www.pandoc.org/README.html#pandocs-markdown
[2]: https://github.com/commercialhaskell/stack/
[3]: https://git-scm.com/book/en/v2/Getting-Started-Installing-Git
