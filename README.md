# arXiv-citation

Generate citation data for PDF files from the [arXiv] or from [zbmath].
Additionally, download preprints to a specified directory and open them.
Includes [elfeed] support.

[arXiv]: https://arxiv.org/
[elfeed]: https://github.com/skeeto/elfeed
[zbmath]: https://zbmath.org/

## Features

The high-level overview is:

 + `arxiv-citation-gui`: Slurp an arXiv (or zbmath) link from the
   primary selection or the clipboard and insert the corresponding
   citation into every file specified in `arxiv-citation-bibtex-files`
   (NOTE: this is `nil` by default!).  This uses `gui-get-selection` and
   is thus dependent on X11.  Below is a showcase of invoking this
   function via a global keybinding, instead of inside Emacs itself.

   ![](https://user-images.githubusercontent.com/50166980/165585713-b798bbba-c5d9-4611-8a7c-b89fec898cf2.gif)

 + `arxiv-citation-elfeed`: Invoking this function when viewing a paper
   in elfeed downloads it to `arxiv-citation-library` with name
   "author1-author2-...authorn_title-sep-by-dashes.pdf" and opens it
   with `arxiv-citation-open-pdf-function`.

   ![](https://user-images.githubusercontent.com/50166980/165453050-3b8eb116-2a38-43fd-8a76-4d5226e75438.gif)

 + `arxiv-citation-download-and-open`: This works much like
   `arxiv-citation-elfeed`, but relies on X selections for getting the
   url to the paper.

## Installation

### MELPA

The package is on MELPA, so you can install it like any other package:

        M-x package-install RET arxiv-citation RET

### Manual

Copy `arxiv-citation.el` into a directory within your `load-path` and
require it.  For example, assuming that this file was placed within the
`~/.config/emacs/elisp` directory:

``` emacs-lisp
(add-to-list 'load-path "~/.config/emacs/elisp/")
(require 'arxiv-citation)
```

If you use [use-package], you can express the above as

``` emacs-lisp
(use-package arxiv-citation
  :load-path "~/.config/emacs/elisp/")
```

[use-package]: https://github.com/jwiegley/use-package

## Configuration

You will need to customise at least `arxiv-citation-bibtex-files` if you
want to use any function that adds citations and
`arxiv-citation-library` to download files to an appropriate directory.
For example:

``` emacs-lisp
(use-package arxiv-citation
  :commands (arxiv-citation-elfeed arxiv-citation-gui)
  :custom
  (arxiv-citation-library "~/library")
  (arxiv-citation-bibtex-files
   '("~/.tex/bibliography.bib"
     "~/projects/super-secret-project/main.bib")))
```
