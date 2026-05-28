#lang racket
(provide (all-defined-out))
(require scribble/core scribble/html-properties scribble/manual) 

;(define prof1 (link "https://jmct.cc" "José Manuel Calderón Trilla"))
;(define prof1-pronouns "he/him")
;(define prof1-email "jmct@cs.umd.edu")
;(define prof1-initials "JMCT")

(define prof1 (link "https://www.cs.umd.edu/~dvanhorn/" "David Van Horn"))
(define prof1-pronouns "he/him")
(define prof1-email "dvanhorn@cs.umd.edu")
(define prof1-initials "DVH")

(define semester "summer")
(define year "2026")
(define courseno "CMSC 430")

(define lecture-dates "June 1 -- July 10, 2026")

(define IRB "IRB") 
(define AVW "AVW")
(define KEY "KEY")

(define office-hour-location "TBD")

(define start-date "June 1")
(define exam1-date "June 16")
(define exam2-date "July 1")
(define exam3-date "July 10")
(define exam-hours "24")
(define final-date "July 10")
(define exam-end-time "11:59pm")
(define elms-url "https://umd.instructure.com/courses/1406982")

(define online? #t)

(define racket-version "8.18")

(define staff
  (list (list "Pierce Darragh" "pdarragh@umd.edu")))

(define lecture-schedule1 "Weekdays, 10:00-11:15am")

(define classroom1
  (link "https://umd.zoom.us/j/96210969737?pwd=K6Iz8IZNchyWEcV3UERCi6KPkawRDw.1" "Online"))

;(define discord "TBD")
(define piazza "https://piazza.com/umd/summer2026/cmsc430/home")
(define gradescope "https://www.gradescope.com/courses/1319905")

(define feedback "https://forms.gle/99yTz7HVfopCaDMz9")

(define (assign-deadline i)
  (list-ref '("Thursday, June 4, 11:59PM"
              "Friday, June 5, 11:59PM"
              "Friday, June 12, 11:59PM"
              "Monday, June 22, 11:59PM"
              "Monday, June 29, 11:59PM"
              "Monday, July 6, 11:59PM"
              "Wednesday, July 8, 11:59PM"
              "Thursday, July 9, 11:59PM")
            (sub1 i)))

(define office-hours
  "TBD")
