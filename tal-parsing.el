;;; tal-parsing.el ---

;; Copyright (C) 2014 Grégoire Jadi

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

(defstruct tal/tag
  initial-trees
  auxiliary-trees)

(defun tal/initialize-array (size)
  "Initialize the array used by the algorithm.

It's a four dimentional array."
  (let ((ret (make-list size nil)))
    (dotimes (i size)
      (setf (elt ret i) (make-list size nil))
      (dotimes (j size)
        (setf (elt (elt ret i) j) (make-list size nil))
        (dotimes (k size)
          (setf (elt (elt (elt ret i) j) k) (make-list size nil))
          (dotimes (l size)
            (setf (elt (elt (elt (elt ret i) j) k) l) (list))))))
    ret))

(defun tal/parsing (tag input)
  "Main entry point to parse the INPUT and determine whether it
can be generated by TAG."
  (let* ((n (length input))
         (A (tal/initialize-array (1+ n))))
    (tal/init-frontier A tag input)
    (tal/init-foot-nodes A tag input)
    (loop for l from 0 to n
          do
          (loop for i downfrom l to 0
                do
                (loop for j from i to l
                      do
                      (loop for k downfrom l to j
                            do (tal/case-1 A tag input i j k l)
                            do (tal/case-2 A tag input i j k l)
                            do (tal/case-3 A tag input i j k l)
                            do (tal/case-5 A tag input i j k l)
                            do (tal/case-4 A tag input i j k l)))))
    (loop for j from 0 to n
          for intersection = (intersection (tal/tag-initial-trees tag)
                                           (elt (elt (elt (elt A 0)
                                                          j)
                                                     j)
                                                n))
          if intersection
          do (return intersection))))

(defun tal/add-node-frontier (node input A i j k l)
  "Determine whether NODE ∈ A[i, j, k, l] for the given INPUT and
add it if it's the case.

We say a node X of a derived tree γ belongs to A[i, j, k, l] if X
dominates a sub-tree of γ whose frontier is given by either

 a_i+1 ... a_j Y a_k+1 ... l

 (where the foot node of γ is labelled by Y) or

 a_i+1 ... l

 (i.e j = k. This corresponds to the case when γ is
a sentential tree).

The indices (i, j, k, l) refer to the positions between the input
symbols and range over 0 through n. If i = 5 say, then it refers
to the gap beteen a_6 and a_l (or a_n or something I can't read
it)."
  (let ((frontier (mapcar #'bt/node-symbol (bt/leaves node)))
        (n (length input))
        bool)
    (setf bool (bt/node-p node))

    ;; check the first part (i -> j)
    (when bool
      (loop for x from i to (min (1- j) (1- n))
            for el = (pop frontier) 
            
            if (not (eq el (elt input x)))
            do (setf bool nil)
            
            while bool))
    
    ;; check the second part (k -> l)
    (when bool
      (loop for x from k to (min (1- l) (1- n))
            for el = (pop frontier)
            
            if (not (eq el (elt input x)))
            do (setf bool nil)
            
            while bool))

    ;; if it's okay, add the node to A[i, j, k, l]
    (when bool
      (pushnew node (elt (elt (elt (elt A i) j) k) l)))))

(defun tal/case-1 (A tag input i j k l)
  "Case 1 corresponds to situation where the left sibling is the
ancestor of the foot node. The parent is put in A[i, j, k, l] if
the left sibling is in A[i, j, k, m] and the right sibling is in
A[m, p, p, l], where k <= m < l, m <= p, p <= l."
  (loop for m from k to (1- l)
        do
        (loop for p from m to l
              ;; if left siblings in A[i, j, k, m] are an ancestor of
              ;; a foot node
              for left = (remove-if-not #'bt/node-foot-node-ancestor
                                        (elt (elt (elt (elt A i)
                                                       j)
                                                  k)
                                             m))
              ;; and their right siblings in A[m, p, p, l]
              for right = (remove-if (lambda (node)
                                       (or (bt/node-foot-node-ancestor node)
                                           (not (find (bt/node-left node)
                                                      left))))
                                     (elt (elt (elt (elt A m)
                                                    p)
                                               p)
                                          l))
              do
              ;; satisfying appropriate restrictions
              (loop for node in left
                    if (and (<= k m)
                            (< m l)
                            (<= m p)
                            (<= p l))
                    ;; then put their parent in A[i, j, k, l]
                    do (tal/add-node-frontier (bt/node-parent node)
                                              input
                                              A i j k l)))))

(defun tal/case-2 (A tag input i j k l)
  "Case 2 corresponds to the case where the right sibling is the
ancestor of the foot node. If the left sibling is in A[i, m, m,
p] and the right sibling is in A[p, j, k, l], i <= m < p and p <=
j, then we put their parent in A[i, j, k, l]."
  (loop for m from i to (1- j)
        do
        (loop for p from (1+ m) to j
              ;; if right siblings in A[m, p, p, l] are an ancestor of
              ;; a foot node
              for right = (remove-if-not #'bt/node-foot-node-ancestor
                                         (elt (elt (elt (elt A m)
                                                        p)
                                                   p)
                                              l))

              ;; and their left siblings in A[i, j, k, m]
              for left = (remove-if (lambda (node)
                                      (or (bt/node-foot-node-ancestor node)
                                          (not (find (bt/node-right node)))))
                                    (elt (elt (elt (elt A i)
                                                   j)
                                              k)
                                         m))


              do
              ;; satisfying appropriate restrictions
              (loop for node in right
                    if (and (<= i m)
                            (< m p)
                            (<= p j))
                    ;; then put their parent in A[i, j, k, l]
                    do (tal/add-node-frontier (bt/node-parent node) input
                                              A i j k l)))))

(defun tal/case-3 (A tag input i j k l)
  "Case 3 corresponds to the case where neither children are
ancestors of the foot node. If the left sibling ∈ A[i, j, j, m]
and the right sibling ∈ A[m, p, p, l] then we can put the parent
in A[i, j, j, l] if it is the case that (i < j <= m or i <= j <
m) and (m < p <= l or m <= p < l)"
  (loop for m from j to (1- l)
        do
        (loop for p from j to l
              ;; for all left siblings in A[i, j, j, m]
              for left = (remove-if #'bt/node-foot-node-ancestor
                                    (elt (elt (elt (elt A i)
                                                   j)
                                              j)
                                         m))
              ;; and right siblings in A[m ,p, p, l]
              for right = (remove-if #'bt/node-foot-node-ancestor
                                     (elt (elt (elt (elt A m)
                                                    p)
                                               p)
                                          l))
              ;; satisfying the appropriate restrictions
              do
              (loop for node in left
                    if (and (find node right)
                            (and
                             (or (and (< i j)
                                      (<= j m))
                                 (and (<= i j)
                                      (< j m)))
                             (or
                              (and (< m p)
                                   (<= p l))
                              (and (<= m p)
                                   (< p l)))))
                    do
                    ;; put their parent in A[i, j, j, l]
                    (tal/add-node-frontier (bt/node-parent node) input
                                           A i j j l)))))

(defun tal/case-4 (A tag input i j k l)
  "Case 4 corresponds to the case where a node Y has only one
child X. If X ∈ A[i, j, k, l] then put Y in A[i, j, k, l]. Repeat
Case 4 again if Y has no siblings."
  (loop for X in (elt (elt (elt (elt A i)
                                j)
                           k)
                      l)
        for Y = (bt/node-parent X)
        if (and Y
                (or (null (bt/node-left Y))
                    (null (bt/node-right Y))))
        do (tal/add-node-frontier Y input
                                  A i j k l)))

(defun tal/case-5 (A tag input i j k l)
  "Case 5 corresponds to adjoining. If X is a node in A[m, j, k,
p] and Y is the root of an auxiliary tree with the same symbol as
that of X, such that Y is in A[i, m, p, l] ((i <= m <= p < l or i
< m <= p <= l) and (m < j <= k <= p or m <= j <= k < p))."
  (loop for m from i to j
        do
        (loop for p from m to l
              do
              ;; if a node X ∈ A[m, j, k, p]
              (loop for X in (elt (elt (elt (elt A m)
                                            j)
                                       k)
                                  p)
                    do
                    ;; find the auxiliary trees in A[i, m, p, l] that
                    ;; can be adjoined at the node X
                    (loop for node in (elt (elt (elt (elt A i)
                                                     m)
                                                p)
                                           l)
                          if (and (tal/is-auxiliary-tree node)
                                  (eq (bt/node-symbol node)
                                      (bt/node-symbol X)))
                          ;; then put X in A[i, j, k, l]
                          do (tal/add-node-frontier X input
                                                    A i j k l))))))

(defun tal/init-frontier (A tag input)
  "First step of initialization of the array A.

During this step we fill A with all leaves in TAG whose label are
in the INPUT."
  (let ((terminal-leaves (append
                          (mapcan (lambda (node)
                                    (bt/leaves node 'terminal))
                                  (tal/tag-initial-trees tag))
                          (mapcan (lambda (node)
                                    (bt/leaves node 'terminal))
                                  (tal/tag-auxiliary-trees tag)))))
    (loop for i from 0 to (1- (length input))
          do
          ;; put all nodes in the frontier of elementary trees whose
          ;; label is a_i+1 in A[i, i+1, i+1, i+1]
          (mapc (lambda (node)
                  (when (eq (elt input i)
                        (bt/node-symbol node))
                    (tal/add-node-frontier node input
                                           A i (1+ i) (1+ i) (1+ i))))
                terminal-leaves)))
  A)                                    ; REPL sanity

(defun tal/init-foot-nodes (A tag input)
  "Second step of initialization of the array A.

During this step we fill A with all foot nodes from all auxiliary
trees of TAG."
  (let ((n (length input))
        ;; the foot nodes are the non-terminal leaves
        (foot-nodes (mapcan (lambda (node)
                              (bt/leaves node 'non-terminal))
                            (tal/tag-auxiliary-trees tag))))
    (loop for i from 0 to (1- n)
          do
          (loop for j from i to (1- n)
                do
                ;; put foot nodes of all auxiliary trees in A[i, i, j,
                ;; j]
                (mapc (lambda (node)
                        (tal/add-node-frontier node input
                                               A i i j j))
                      foot-nodes))))
  A)                                    ; REPL sanity

;; Utils

(defun tal/is-terminal (symbol)
  "Determine whether SYMBOL is terminal or not.

A symbol is terminal if it is in downcase, otherwise it is a
non-terminal symbol."
  (string= (symbol-name symbol)
           (downcase (symbol-name symbol))))

(defun tal/is-auxiliary-tree (node)
  "Determine whether NODE is an auxliary tree or not.

An auxiliary tree is a tree whose root X is also its foot node."
  (and (null (bt/node-parent node))
       (bt/node-foot-node-ancestor node)))

(defun tal/aref-m (x &rest index)
  (if (null index)
      x
    `(elt ,(tal/aref-m x (rest index))
          ,(first index))))

;; Bin Tree

(defstruct bt/node
  symbol left right parent foot-node-ancestor)

(defun bt/make-tree (list)
  "Make a binary tree made of `bt/node' from the lispy
representation in list."
  (labels ((make-tree (thing &optional parent root)
                      (unless (or (null parent)
                                  (bt/node-p parent))
                        (error "invalid PARENT (%S)" parent))
                      (unless (or (null root)
                                  (symbolp root))
                        (error "invalid ROOT (%S)" root))
                      (when thing
                        (cond ((consp thing)
                               (let ((node (make-bt/node
                                            :symbol (first thing)
                                            :parent parent)))
                                 (setf (bt/node-left node) (make-tree (second thing) node (or root (first thing)))
                                       (bt/node-right node) (make-tree (third thing) node (or root (first thing))))
                                 node))
                              ((symbolp thing)
                               (let ((node (make-bt/node :symbol thing
                                                         :parent parent)))
                                 (when (eq thing root)
                                   (mark-foot-node-ancestor parent node))
                                 node)))))
           (mark-foot-node-ancestor (node foot-node)
                                    (when node
                                      (setf (bt/node-foot-node-ancestor node) foot-node)
                                      (mark-foot-node-ancestor (bt/node-parent node) foot-node))))
    (make-tree list)))

(defun bt/make-trees (lists)
  "Make trees from the list of trees represented as lisp structures.

See `bt/make-tree' for more information."
  (mapcar #'bt/make-tree lists))

(defun bt/node-sibling (node &optional side)
  "Return the sibling of NODE. SIDE can be either LEFT, RIGHT or
nil to specify the wanted sibling.

Return nil if NODE has no sibling."
  (case side
    (left (bt/node-left (bt/node-parent node)))
    (right (bt/node-right (bt/node-parent node)))
    (otherwise (or (bt/node-left (bt/node-parent node))
                   (bt/node-right (bt/node-parent node))))))

(defun bt/is-leaf (node)
  "Determine whether NODE is a leaf.

A node is a leaf if it has no children."
  (and (not (bt/node-left node))
       (not (bt/node-right node))))

(defun bt/leaves (node &optional type)
  "Return all leaves in NODE. NODE is scanned recursively to find
all leaves.

TYPE can be TERMINAL, NON-TERMINAL or nil and is used to filter
the kind of leaves we are looking for."
  (cond ((not (bt/node-p node))
         nil)
        ((bt/is-leaf node)
         (case type
           (terminal (and (tal/is-terminal (bt/node-symbol node))
                          (list node)))
           (non-terminal (and (not (tal/is-terminal (bt/node-symbol node)))
                              (list node)))
           (otherwise (list node))))
        (t (append (bt/leaves (bt/node-left node) type)
                   (bt/leaves (bt/node-right node) type)))))

;; Initialization
(defvar tal/tag nil)
(setf tal/tag (make-tal/tag
                 :initial-trees (bt/make-trees '((S e)))
                 :auxiliary-trees (bt/make-trees '((S a (T S b))
                                                   (T a (S T b))))))

(defvar tal/input nil)
(setf tal/input '(a a e b b))

(provide 'tal-parsing)

;;; tal-parsing.el ends here
