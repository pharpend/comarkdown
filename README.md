# Comarkdown

Comarkdown is a preprocessor for plain-text documents of any format,
although it was originally intended for [Pandoc's Markdown][1]. At the moment,
it's very simple, and not even functional.

## Installation

### With Stack

I recommend you use [Stack][2] to install this. I haven't published a
version on Hackage yet, so you'll also need [Git][3] to download the
source code.

    git clone git://github.com/pharpend/comarkdown.git
    cd comarkdown

There isn't an executable yet, so `stack install` won't do you any
good. You can however run the test suite with `stack test`.

### Sans Stack

This requires GHC 7.10 and `cabal-install >=1.22`

    git clone git://github.com/pharpend/comarkdown.git
    cd comarkdown

Again, there isn't an ex

## Contact

Bug reports: <https://github.com/pharpend/comarkdown/issues/new>
Email: `peter@harpending.org`
IRC: `pharpend` on FreeNode

[1]: http://www.pandoc.org/README.html#pandocs-markdown
[2]: https://github.com/commercialhaskell/stack/
[3]: https://git-scm.com/book/en/v2/Getting-Started-Installing-Git
