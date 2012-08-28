#|----------------------------------------------------------------------------|
 | Copyright 1992, 2002, 2006, 2011 John Carroll, Ted Briscoe, Rebecca Watson |
 |                                                                            |
 | This file is part of RASP.                                                 |
 |                                                                            |
 | RASP is free software: you can redistribute it and/or modify it            |
 | under the terms of the GNU Lesser General Public License as published      |
 | by the Free Software Foundation, either version 3 of the License, or       |
 | (at your option) any later version.                                        |
 |                                                                            |
 | RASP is distributed in the hope that it will be useful,                    |
 | but WITHOUT ANY WARRANTY; without even the implied warranty of             |
 | MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              |
 | GNU Lesser General Public License for more details.                        |
 |                                                                            |
 | You should have received a copy of the GNU Lesser General Public License   |
 | along with RASP.  If not, see <http://www.gnu.org/licenses/>.              |
 |----------------------------------------------------------------------------|#

;;; GRAMMAR DEVELOPMENT ENVIRONMENT - GRAMMAR CACHE INVALIDATION
;;;
;;; Author: John Carroll
;;;
;;; Functions for invalidating cached data relating to ID rules
;;; and words, and for working out which particular ID rule
;;; definitions need to be invalidated when a metarule,
;;; propagation rule or default rule has been added, changed or
;;; deleted from the grammar.
;;;
;;; Entry points:
;;;
;;;  * (defun Input-idrule-invalidations (rules type) ...
;;;  * (defun Input-word-invalidations (words type) ...
;;;  * (defun Metarule-invalidated-idrules
;;;       (metarule metarule-name) ...
;;;  * (defun Proprule-invalidated-idrules
;;;       (proprule proprule-name) ...
;;;  * (defun Defrule-invalidated-idrules
;;;       (defrule defrule-name) ...
;;;  * (defun Catrule-invalidated-idrules (catrule catrule-name)
;;; ...
;;;
;;; A general point about invalidation is that doing too much
;;; when not strictly required is safe, but less efficient for
;;; subsequent recompilation. It must be this way round though.
;;;
;;; Infelicities:
;;;
;;; - Invalidation acts on all cached expanded or compiled data
;;; on an ID rule, rather than only that beyond the relevant
;;; point.
;;;
;;; - Rules of different construct types but with the same name
;;; can cause too much invalidation to be performed.
;;;
;;; - All possible data is invalidated for changes to LP rules.

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


;;; Type may be one of 'expanded' or 'compiled' for ID rules,
;;; 'normalised' or 'compiled' for words, - the type depending
;;; on the declaration that was input, changed or deleted that
;;; affects the given rules.

(defun input-idrule-invalidations (rules type)
   (dolist (name rules)
      (progn
         (when (member type '(normalised expanded) :test #'eq)
            (remprop name 'expanded-idrules)
            (remprop name 'lr1-psrule))
         (when
            (member type
               '(normalised expanded compiled) :test #'eq)
            (remprop name 'compiled-idrules))))
   (g-init-parse)
   (setf *generator-rules nil))


(defun input-word-invalidations (words type)
   (dolist (name words)
      (progn
         (when (eq type 'normalised)
            (remprop name 'normalised-word))
         (when (member type '(normalised compiled) :test #'eq)
            (remprop name 'compiled-word))))
   (setf *generator-words nil))


;;; Given a metarule definition and its name, return names of
;;; root ID rules that may have invalid cached data now that the
;;; grammar has changed. ID rules are those which were
;;; previously expanded with a metarule of the same name, and
;;; those to which the new metarule is applicable (at the
;;; relevant point in the metarule application sequence). Null
;;; metarule argument indicates metarule is being deleted.
;;;
;;; N.B. The metarule will be non-linear - linear metarule
;;; invalidation works on the cached compiled ID rule
;;; definition.
;;;
;;; Even if the new metarule is not applicable, ID rules derived
;;; using a previous version of the metarule are now invalid.

(defun metarule-invalidated-idrules
   (metarule metarule-name)
   (let
      ((after-names
          (member metarule-name *meta-rules :test #'eq)))
      (mapcan
         #'(lambda (idrule)
              (when
                 (member-if
                    #'(lambda (expanded-idrule)
                         (let
                            ((meta-names
                                (top-rule-name-meta-names
                                   (id-rule-name
                                      expanded-idrule))))
                            (or
                               (f-find metarule-name meta-names
                                  :key #'sub-rule-name-base
                                  :test #'eq)
                               (and metarule
                                  (dolist
                                     (sub-name meta-names t)
                                     (if
                                        (member
                                           (sub-rule-name-base
                                              sub-name)
                                           after-names)
                                        (return nil)))
                                  (match-invalid-metarule
                                     expanded-idrule
(normalise-metarule-definition
                                        metarule-name))))))
                    (or (get idrule 'compiled-idrules)
                       (get idrule 'expanded-idrules)))
                 (ncons idrule)))
         *id-rules)))


(defun match-invalid-metarule (idrule metarule)
   (match-rule-pattern
      (id-rule-binding-list idrule)
      (id-rule-binding-nos idrule)
      (meta-rule-cat-bindings metarule)
      (meta-rule-lhs-binding-nos metarule)))


;;; The algorithm for propagation, default and category rule
;;; invalidation of cached ID rule data is based on the
;;; following observation:
;;;
;;; That if an ID rule or category pattern (in which no category
;;; contains ~feat and all specified features have non-variable
;;; values) belonging to e.g. a default rule fails to match an
;;; ID rule, then the pattern could not have matched it at any
;;; previous point. This is because subsequent processing (other
;;; than metarule application) can only further instantiate the
;;; ID rule, and cannot delete already existing features or
;;; change non-variable feature values. (Matching is by
;;; extension of the pattern rule categories).
;;;
;;; So if an attempted match between a pattern and an ID rule
;;; that has previously undergone default, propagation and
;;; category processing succeeds, it is possible that the match
;;; would succeed if it were performed at the point at which it
;;; normally would be attempted, so the cached ID rule
;;; definition should be removed. Conversely, if the match
;;; fails, then there could no possibly be a match at the normal
;;; point, so the ID rule definition is still valid.
;;;
;;; If one of the default / propagation / category rule pattern
;;; categories contains ~feat this sort of analysis is not
;;; possible since the pattern might match in the normal course
;;; of processing, followed at some point by a rule which adds
;;; the feature which is the subject of the ~. Then the rule
;;; would not match after, and so the cached ID rule definition
;;; would erroneously not be removed. Similarly for the pattern
;;; value @.
;;;
;;; As with metarules, all ID rule definitions produced with an
;;; old version of the rule in question must be invalidated. If
;;; there is no cached data, then normalisation (possibly
;;; resulting in an error) of the rule can be avoided.

(defun proprule-invalidated-idrules
   (proprule proprule-name)
   (cond
      ((and proprule (exists-cached-idrule-data)
          (bindings-contain-negation
             (prop-rule-cat-bindings
                (normalise-proprule-definition
                   proprule-name))))
         *id-rules)
      (t
         (mapcan
            #'(lambda (idrule)
                 (when
                    (member-if
                       #'(lambda (expanded-idrule)
                            (or
                               (member proprule-name
                                  (id-rule-rules-applied
                                     expanded-idrule) :test #'eq)
                               (and proprule
                                  (let
                                     ((normalised-proprule
(normalise-proprule-definition
                                            proprule-name)))
                                     (match-rule-pattern
                                        (id-rule-binding-list
                                           expanded-idrule)
                                        (id-rule-binding-nos
                                           expanded-idrule)
                                        (prop-rule-cat-bindings
                                           normalised-proprule)
                                        (prop-rule-binding-nos
normalised-proprule))))))
                       (or (get idrule 'compiled-idrules)
                          (get idrule 'expanded-idrules)))
                    (ncons idrule)))
            *id-rules))))


(defun defrule-invalidated-idrules
   (defrule defrule-name)
   (cond
      ((and defrule (exists-cached-idrule-data)
          (bindings-contain-negation
             (default-rule-cat-bindings
                (normalise-defrule-definition
                   defrule-name))))
         *id-rules)
      (t
         (mapcan
            #'(lambda (idrule)
                 (when
                    (member-if
                       #'(lambda (expanded-idrule)
                            (or
                               (member defrule-name
                                  (id-rule-rules-applied
                                     expanded-idrule) :test #'eq)
                               (and defrule
                                  (let
                                     ((normalised-defrule
(normalise-defrule-definition
                                            defrule-name)))
                                     (match-rule-pattern
                                        (id-rule-binding-list
                                           expanded-idrule)
                                        (id-rule-binding-nos
                                           expanded-idrule)
(default-rule-cat-bindings
                                           normalised-defrule)
(default-rule-binding-nos
normalised-defrule))))))
                       (or (get idrule 'compiled-idrules)
                          (get idrule 'expanded-idrules)))
                    (ncons idrule)))
            *id-rules))))


(defun catrule-invalidated-idrules
   (catrule catrule-name)
   (cond
      ((and catrule (exists-cached-idrule-data)
          (bindings-contain-negation
             (category-declaration-cat-bindings
                (normalise-category-definition
                   catrule-name))))
         *id-rules)
      (t
         (mapcan
            #'(lambda (idrule)
                 (when
                    (member-if
                       #'(lambda (expanded-idrule)
                            (or
                               (member catrule-name
                                  (id-rule-rules-applied
                                     expanded-idrule) :test #'eq)
                               (and catrule
                                  (let
                                     ((catrule-bindings
(category-declaration-cat-bindings
(normalise-category-definition
                                               catrule-name))))
                                     (member-if
                                        #'(lambda
                                             (top-binding-no)
                                             (match-category 0
                                                catrule-bindings
                                                top-binding-no
(id-rule-binding-list
expanded-idrule)))
                                        (id-rule-binding-nos
expanded-idrule))))))
                       (or (get idrule 'compiled-idrules)
                          (get idrule 'expanded-idrules)))
                    (ncons idrule)))
            *id-rules))))


(defun exists-cached-idrule-data nil
   (member-if
      #'(lambda (idrule)
           (or (get idrule 'expanded-idrules)
              (get idrule 'compiled-idrules)))
      *id-rules))


(defun bindings-contain-negation
   (cat-bindings)
   (member-if
      #'(lambda (binding)
           (member-if
              #'(lambda (fvpair)
                   (let
                      ((value (fv-pair-value fvpair)))
                      (if (consp value)
                         (member '*novalue* value :test #'eq)
                         (or (eq value '*novalue*)
                            (eq value '*absent*)))))
              (category-binding-category binding)))
      cat-bindings))


;;; End of file

