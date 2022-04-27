# arXiv-citation

Generate citation data for PDF files from the arXiv.  Additionally,
download preprints to a specified directory and open them.  Includes
[elfeed] support.

The high-level overview is:

 + `arXiv-citation-gui`: Slurp an arXiv link from the primary selection
   or the clipboard and insert the corresponding citation into every
   file specified in `arXiv-citation-bibtex-files` (NOTE: this is `nil`
   by default!).  This uses `gui-get-selection` and is thus dependent on
   X11.

   ![](https://user-images.githubusercontent.com/50166980/165585713-b798bbba-c5d9-4611-8a7c-b89fec898cf2.gif)

 + `arXiv-citation-elfeed`: Invoking this function when viewing a paper
   in elfeed downloads it to `arXiv-citation-library` with name
   "author1-author2-...authorn_title-sep-by-dashes.pdf" and opens it
   with `arXiv-citation-open-pdf-function`.

   ![](https://user-images.githubusercontent.com/50166980/165453050-3b8eb116-2a38-43fd-8a76-4d5226e75438.gif)

 + `arXiv-citation-download-and-open`: This works much like
   `arXiv-citation-elfeed`, but relies on X selections for getting the
   url to the paper.

[1]: https://github.com/skeeto/elfeed
