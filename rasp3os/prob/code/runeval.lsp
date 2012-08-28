#|----------------------------------------------------------------------------|
 | Copyright 2002, 2006, 2011 John Carroll, Ted Briscoe, Rebecca Watson       |
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

;; +prob-data-pathname+ +prob-greval-pathname+ defined in load.lsp

(load (#-(or procyon :coral poplog) compile-file #+(or procyon :coral poplog) identity
         (merge-pathnames "gramreleval.lsp" +prob-greval-pathname+)))


(greval-relation-summary t
   (gramreleval
      (merge-pathnames "suste-text" +prob-greval-pathname+)
      (merge-pathnames "suste-gr" +prob-greval-pathname+)
      (merge-pathnames "sustetsg12.parses-ti-lex-ph-gr" +prob-data-pathname+)
      #+(or unix sunos darwin) "output" #-(or unix sunos darwin) "internal:output")
   )


;;; End of file
