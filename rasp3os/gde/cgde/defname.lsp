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

;;; GRAMMAR DEVELOPMENT ENVIRONMENT - NAMES OF FEATURES ETC
;;;
;;; Author: John Carroll
;;;
;;; The functions in this file return the conventional names for
;;; certain important features, sets, feature values, and
;;; categories. These particular names are thus in a sense
;;; hard-wired into the environment.
;;;
;;; Entry points:
;;;
;;;  * (defmacro Bar-feature-name NIL ...
;;;  * (defmacro Head-feature-name NIL ...
;;;  * (defmacro Takes-feature-name NIL ...
;;;  * (defmacro Subcat-feature-name NIL ...
;;;  * (defmacro Null-feature-name NIL ...
;;;  * (defmacro Cat-feature-value NIL ...
;;;  * (defmacro W-category-name NIL ...
;;;  * (defmacro U-category-name NIL ...
;;;  * (defmacro F-variable-name NIL ...
;;;  * (defmacro Element-variable-name NIL ...
;;;  * (defmacro Alt-semantics-marker NIL
;;;  * (defmacro Unnamed-variable NIL ...
;;;  * (defmacro Rule-feature-name NIL ...
;;;  * (defmacro Kleene-feature-name NIL ...
;;;  * (defmacro Null-index-name NIL ...
;;;  * (defmacro Word-index-name NIL ...
;;;  * (defmacro Whead-set-name NIL ...
;;;  * (defmacro Wdaughter-set-name NIL ...
;;;  * (defmacro Morphologyonly-set-name NIL ...

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


(defmacro bar-feature-name nil ''|BAR|)


(defmacro head-feature-name nil ''|H|)


(defmacro takes-feature-name nil ''|TAKES|)


(defmacro subcat-feature-name nil ''|SUBCAT|)


(defmacro null-feature-name nil ''|NULL|)


(defmacro cat-feature-value nil ''|CAT|)


(defmacro w-category-name nil ''|W|)


(defmacro u-category-name nil ''|U|)


(defmacro f-variable-name nil ''|F|)


(defmacro element-variable-name nil ''|in|)


(defmacro alt-semantics-marker nil ''|alts|)


(defmacro unnamed-variable nil ''\@)


;;; Category index names needed when printing grammar to file
;;; for later use by parser.

(defmacro null-index-name nil ''|null|)


(defmacro word-index-name nil ''|word|)


;;; Set names used by morph package.

(defmacro whead-set-name nil ''|WHEAD|)


(defmacro wdaughter-set-name nil ''|WDAUGHTER|)


(defmacro morphologyonly-set-name nil ''|MORPHOLOGYONLY|)


;;; End of file

