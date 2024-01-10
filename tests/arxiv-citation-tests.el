;;; arxiv-citation-tests.el --- Tests for arxiv-citation.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Tony Zorman

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

;;; Code:

(require 'ert)
(require 'arxiv-citation)

(ert-deftest arxiv-citation-test/arxiv-citation ()
  (let ((comod-paper "@Article{halbig23:diagr-comod-monad,
 author        = {Halbig, Sebastian and Zorman, Tony},
 journal       = {arXiv e-prints},
 title         = {{D}iagrammatics for {C}omodule {M}onads},
 year          = {2023},
 eprint        = {2312.13074},
 eprintclass   = {math.CT},
 eprinttype    = {arXiv},
 keywords      = {math.CT, math.QA, 18M30 (Primary) 18M05, 16T05, 18C15 (Secondary)},
}"))
    (should (string= (arxiv-citation-get-arxiv-citation "https://arxiv.org/abs/2312.13074")
                     comod-paper))))

(ert-deftest arxiv-citation-test/zbmath-citation ()
  (let ((formal-theory-of-monads-2
         "@Article{lack02,
 Author        = {Lack, Stephen and Street, Ross},
 Title         = {The formal theory of monads. {II}},
 FJournal      = {Journal of Pure and Applied Algebra},
 Journal       = {J. Pure Appl. Algebra},
 ISSN          = {0022-4049},
 Volume        = {175},
 Number        = {1-3},
 Pages         = {243--265},
 Year          = {2002},
 Language      = {English},
 DOI           = {10.1016/S0022-4049(02)00137-8},
 Keywords      = {18C20,18D05,18C15,18A35,18G15,16W30},
 zbMATH        = {1839070},
 Zbl           = {1019.18002}
}"))
    (should (string= (arxiv-citation-get-citation "https://zbmath.org/1019.18002")
                     formal-theory-of-monads-2))))

(ert-deftest arxiv-citation-test/prefers-zbmath-citation ()
  "`arxiv-citation-get-citation' should prefer a zbMath citation to
an arXiv one, even if an arXiv URL is given."
  (let ((hopf-monads
         "@Article{bruguieres07:hopf,
 Author        = {Brugui{\\`e}res, Alain and Virelizier, Alexis},
 Title         = {Hopf monads},
 FJournal      = {Advances in Mathematics},
 Journal       = {Adv. Math.},
 ISSN          = {0001-8708},
 Volume        = {215},
 Number        = {2},
 Pages         = {679--733},
 Year          = {2007},
 Language      = {English},
 DOI           = {10.1016/j.aim.2007.04.011},
 Keywords      = {18C20,16W30,18D10},
 zbMATH        = {5197740},
 Zbl           = {1168.18002}
}"))
    (should (string= (arxiv-citation-get-citation "https://arxiv.org/abs/math/0604180")
                     hopf-monads))))

(provide 'arxiv-citation-tests)
;;; arxiv-citation-tests.el ends here
