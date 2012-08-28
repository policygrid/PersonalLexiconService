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

(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  #+linuxppc-target
  (require "LINUX-SYSCALLS")
  #+darwinppc-target
  (require "DARWIN-SYSCALLS"))

(let ((*warn-if-redefine-kernel* nil))
(defun timed-wait-on-semaphore (s duration)
  "Wait until the given semaphore has a postive count which can be
atomically decremented, or until a timeout expires."
  (multiple-value-bind (secs nanos) (nanoseconds duration)
    (let* ((now (get-internal-real-time))
           (stop (+ now
                    (* secs 1000)
                    (ceiling nanos 1000000))))
      (loop
        (multiple-value-bind (success err)
            (%wait-on-semaphore-ptr (semaphore-value s) secs nanos)
          (when success
            (return t))
          (when (or (not (eql err #$EINTR))
                    (>= (setq now (get-internal-real-time)) stop))
            (return nil))
          (unless (zerop duration)
            (let* ((diff (- stop now)))
              (multiple-value-bind (remaining-seconds remaining-millis)
                  (floor diff 1000)
                (setq secs remaining-seconds
                      nanos (* remaining-millis 1000000))))))))))
)

