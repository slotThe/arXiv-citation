;;; arXiv-citation.el --- Utility functions for dealing with arXiv papers -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Tony Zorman
;;
;; Author: Tony Zorman <soliditsallgood@mailbox.org>
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Homepage: https://gitlab.com/slotThe/dotfiles

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Generate citation data for PDF files from the arXiv.  Additionally,
;; download and open the current elfeed[1] entry.
;;
;; The high-level overview is:
;;
;;  + `arXiv-citation-gui': Slurp an arXiv link from the primary
;;    selection or the clipboard and insert the corresponding citation
;;    into every file specified in `arXiv-citation-bibtex-files' (NOTE:
;;    this is `nil' by default!).  This uses `gui-get-selection' and is
;;    thus dependent on X11.
;;
;;  + `arXiv-citation-download-and-open': Invoking this function with an
;;    arXiv url downloads it to `arXiv-citation-library' with name
;;    "author1-author2-...authorn_title-sep-by-dashes.pdf" and opens it
;;    with `arXiv-citation-open-pdf-function'.
;;
;;  + `arXiv-citation-elfeed': Elfeed integration.  This works much like
;;    `arXiv-citation-download-and-open', but uses the currently viewed
;;    elfeed item instead of any X selections.
;;
;; [1]: https://github.com/skeeto/elfeed

;;; Code:

(require 'dash)
(require 's)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Variables

(defcustom arXiv-citation-bibtex-files nil
  "List of files to insert bibtex information into.")

(defcustom arXiv-citation-library user-emacs-directory
  "Path to the library.
I.e., the place where all files should be downloaded to.")

(defcustom arXiv-citation-open-pdf-function 'browse-url-xdg-open
  "Function with which to open PDF files.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers

(defun arXiv-citation-arXiv-id (url)
  "Get the arXiv id of URL."
  (caddr (s-match "arxiv.org/\\(pdf\\|abs\\)/\\([0-9.]*\\)" url)))

(defun arXiv-citation-pdf-link (url)
  "Construct the PDF URL from an ordinary arXiv one."
  (concat (s-replace "/abs/" "/pdf/" url) ".pdf"))

(defun arXiv-citation-parse (method)
  "Parse the current buffer as either html or xml.
METHOD is a keyword; either `:html' or `:xml'."
  (pcase-let ((`(,parse-fun . ,start)
               (pcase method
                 (:html (cons #'libxml-parse-html-region "<html "))
                 (:xml  (cons #'libxml-parse-xml-region  "<?xml ")))))
    (funcall parse-fun
             (progn (goto-char 0)
                    (search-forward start)
                    (match-beginning 0))
             (point-max))))

(defun arXiv-citation-pdf-name (info)
  "Produce a standardised PDF name.
INFO is information as given by `arXiv-citation-get-details'.
The output name is of the following form:

    author1-author2-...authorn_title-sep-by-dashes.pdf."
  (cl-flet ((take-lastnames (n)
              (seq-take-while (lambda (c) (not (equal c ?,)))
                              n)))
    (format "%s/%s_%s.pdf"
            arXiv-citation-library
            (mapconcat (-compose #'downcase #'take-lastnames)
                       (plist-get info :authors)
                       "-")
            (->> (plist-get info :title)
                 downcase
                 (s-replace-all '(("_" . "-") (" " . "-")
                                  ("$" . "") ("," . "")
                                  ("\\" . "")))))))

(defun arXiv-citation-generate-autokey ()
  "Generate a key for a bibtex entry in the current buffer.
Defers to `bibtex-generate-autokey' for the actual generation
work—thankfully other people have already solved this much better
than I ever could."
  (bibtex-mode)
  (setq-local bibtex-autokey-year-title-separator ":"
              bibtex-autokey-titleword-separator "-")
  (bibtex-generate-autokey))

(defun arXiv-citation-get-details (link)
  "Get some important details of an arXiv PDF.
LINK is a normal arXiv link of the form

    https://arxiv.org/abs/<arXiv-id>

Returns a plist of with keywords `:id', `:authors', `:title',
`:year', and `:categories'."
  (let* ((arXiv-id (arXiv-citation-arXiv-id link))
         (url (format "http://export.arxiv.org/api/query?id_list=%s" arXiv-id)))
    (with-current-buffer (url-retrieve-synchronously url t t)
      (let* ((xml (arXiv-citation-parse :xml))
             (entry (alist-get 'entry xml))
             (title (s-replace "\n" "" (cadr (alist-get 'title entry))))
             (authors (->> entry
                           (--filter (string= (car it) 'author))
                           (--map (nth 2 (nth 2 it)))
                           (--map (s-split " " it))
                           (--map (apply 'concat (-last-item it) ", " (-butlast it)))))
             (year (seq-take (cadr (alist-get 'published entry)) 4))
             (categories (->> entry
                              (--filter (string= (car it) 'category))
                              (--map (alist-get 'term (nth 1 it))))))
        (unless (string= title "Error")
          (list :id arXiv-id
                :authors authors
                :title title
                :year year
                :categories categories))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Citations

(defun arXiv-citation-get-citation (url)
  "Return the citation corresponding to URL.
URL can be either an arXiv or a zbmath url.  Try zbmath first; if
the paper is not published yet, generate a citation from arXiv
data if applicable (i.e., an arXiv url)."
  (let* ((zbmath "https://zbmath.org")
         (zbmath-url
          (cond ((arXiv-citation-arXiv-id url)
                 (format "%s/?q=arXiv:%s" zbmath
                         (arXiv-citation-arXiv-id (arXiv-citation-pdf-link url))))
                ((s-prefix? zbmath url)
                 url))))
    (with-current-buffer (url-retrieve-synchronously zbmath-url t t)
      (let* ((html (arXiv-citation-parse :html))
             (id (cadr (s-match "Document Zbl \\([0-9.]*\\)"
                                ;; Hahahahahahahahaha
                                (caddr (cadddr (caddr html)))))))
        (if id
            (arXiv-citation-get-zbmath-citation (concat zbmath "/bibtex/" id ".bib"))
          (arXiv-citation-get-arXiv-citation url))))))

(defun arXiv-citation-get-zbmath-citation (url)
  "Obtain a zbmath citation from URL."
  (with-current-buffer (url-retrieve-synchronously url t t)
    (goto-char 0)
    (search-forward "\n\n")             ; skip header
    ;; Format citation
    (let ((citation (buffer-substring (point) (point-max))))
      (erase-buffer)
      (insert citation)
      ;; Insert readable name.
      (goto-char 0)
      (search-forward "{")
      (zap-up-to-char 1 ?,)             ; @Article{,
      (insert (arXiv-citation-generate-autokey))    ; @Article{name,
      ;; Align.
      (align-regexp (point-min) (point-max) "\\(\\s-*\\) =")
      (buffer-string))))

(defun arXiv-citation-get-arXiv-citation (url)
  "Extract an arXiv citation from URL."
  (let* ((info         (arXiv-citation-get-details url))
         (first-author (car (plist-get info :lastnames)))
         (authors      (mapconcat #'identity (plist-get info :authors) ", "))
         (year         (plist-get info :year))
         (id           (plist-get info :id))
         (title        (plist-get info :title))
         (cats         (plist-get info :categories)))
    (cl-flet ((mk-citation (key)
                (concat "@Article{" (or key "") ",\n"
                        " author        = {" authors "},\n"
                        " journal       = {arXiv e-prints},\n"
                        " title         = {" title "},\n"
                        " year          = {" year "},\n"
                        " eprint        = {" id "},\n"
                        " eprintclass   = {" (car cats) "},\n"
                        " eprinttype    = {arXiv},\n"
                        " keywords      = {" (mapconcat #'identity cats ", ") "},\n"
                        "}")))
      (mk-citation (with-temp-buffer
                     (insert (mk-citation nil))
                     (arXiv-citation-generate-autokey))))))

;;;###autoload
(defun arXiv-citation (url)
  "Create a citation from the given arXiv or zbmath URL.
Insert the new entry into all files listed in the variable
`arXiv-citation-bibtex-files'."
  (interactive)
  (let ((citation (arXiv-citation-get-citation url)))
    (dolist (file arXiv-citation-bibtex-files)
      (append-to-file (concat "\n" citation "\n") nil file))))

;;;###autoload
(defun arXiv-citation-gui ()
  "Create a citation from the current arXiv or zbmath link.
\"Current\" means \"in the primary selection or the clipboard\"
\(in that order\).  First, try to pull down citation information
from zbmath—in case the paper is already published. If not,
gather the necessary details from the arXiv API if applicable.

Insert the new entry into all files listed in the variable
`arXiv-citation-bibtex-files'."
  (interactive)
  (let ((primary (gui-get-primary-selection))
        (clipboard (gui-get-selection 'CLIPBOARD)))
    (cond ((s-prefix? "http" primary)   (arXiv-citation primary))
          ((s-prefix? "http" clipboard) (arXiv-citation clipboard)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Downloading papers

;;;###autoload
(defun arXiv-citation-download-and-open (url)
  "Download and open an arXiv PDF from URL."
  (let* ((link (arXiv-citation-pdf-link url))
         (file (arXiv-citation-pdf-name (arXiv-citation-get-details link))))
    ;; Integer as third arg: ask for confirmation before overwriting; lol.
    (url-copy-file link file 42)
    (funcall arXiv-citation-open-pdf-function file)))

;;;###autoload
(defun arXiv-citation-elfeed ()
  "When viewing a paper in elfeed, fetch and open it.
Fetch a paper from the arXiv and open it in zathura."
  (interactive)
  (require 'elfeed)
  (arXiv-citation-download-and-open (elfeed-entry-link elfeed-show-entry)))

(provide 'arXiv-citation)
;;; arXiv-citation.el ends here
