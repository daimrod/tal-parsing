;;; cky.el ---

;; Copyright (C) 2013 Grégoire Jadi

;; Author: Grégoire Jadi <gregoire.jadi@gmail.com>

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'cl)

(defvar cky/grammar
  nil)

(setf cky/grammar
      '((S   -> NP VP)
        (VP  -> VP PP)
        (VP  -> V NP)
        (VP  -> eats)
        (PP  -> P NP)
        (NP  -> Det N)
        (NP  -> she)
        (V   -> eats)
        (P   -> with)
        (N   -> fish)
        (N   -> fork)
        (Det -> a)))

(defvar cky/sentence nil)
(setf cky/sentence '(she eats a fish with a fork))

(defun cky/find-rules (produce grammar)
  "Utility function to retrive rules in GRAMMAR by what they can PRODUCE"
  (loop for rule in grammar
        if (equal produce (cddr rule))
        collect rule))

(defun cky-algorithm (grammar sentence)
  "Source: http://en.wikipedia.org/wiki/CYK_algorithm"
  (let* ((n (length sentence))
         (table (loop repeat n
                      collect (loop repeat n
                                    collect (list)))))
    ;; Initialize the first line
    (loop for i from 0 to (1- n)
          for rules = (cky/find-rules (list (elt sentence i)) grammar)
          if rules
          do (loop for rule in rules
                   do (pushnew (first rule) (elt (elt table i) 0))))
    

    ;; Fill the rest of the table
    (loop for i from 2 to n
          do
          (loop for j from 1 to (1+ (- n i))
                do
                (loop for k from 1 to (1- i)
                      do
                      (loop for B in (elt (elt table (1- j))
                                          (1- k))
                            do
                            (loop for C in (elt (elt table (1- (+ j k)))
                                                (1- (- i k)))
                                  for rules = (cky/find-rules (list B C) grammar)
                                  if rules
                                  do (loop for rule in rules
                                           do (pushnew (first rule) (elt (elt table (1- j))
                                                                         (1- i)))))))))
    table))

(provide 'cky)

;;; cky.el ends here
