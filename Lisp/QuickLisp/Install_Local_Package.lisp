;;Installing local Package in SBCL
;;=================================

;To Install Local package into SBCL we can use Quicklisp's help


(push #P"/path/to/projectasdf/" asdf:*central-registry*)
(ql:quickload "project")
