# arXiv-citation

Generate citation data for PDF files from the arXiv.  Additionally,
download preprints to a specified directory and open them.  Includes
elfeed[1] support.

The high-level overview is:

 + `arXiv-citation-gui`: Slurp an arXiv link from the primary selection
   or the clipboard and insert the corresponding citation into every
   file specified in `arXiv-citation-bibtex-files` (NOTE: this is `nil`
   by default!).  This uses `gui-get-selection` and is thus dependent on
   X11.

 + `arXiv-citation-download-and-open`: Invoking this function with an
   arXiv url downloads it to `arXiv-citation-library` with name
   "author1-author2-...authorn_title-sep-by-dashes.pdf" and opens it
   with `arXiv-citation-open-pdf-function`.

 + `arXiv-citation-elfeed`: Elfeed integration.  This works much like
   `arXiv-citation-download-and-open`, but uses the currently viewed
   elfeed item instead of any X selections.

[1]: https://github.com/skeeto/elfeed
