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

(define semester "fall")
(define year "2025")
(define courseno "CMSC 430")

(define lecture-dates "" #;"May 30 -- July 7, 2023")

(define IRB "IRB") 
(define AVW "AVW")
(define KEY "KEY")

(define office-hour-location (elem AVW " " "4122"))


(define m1-date "October 9")
(define m2-date "November 6")
(define midterm-hours "24")
(define final-date "TBD")
(define final-end-time "TBD")
(define elms-url "https://umd.instructure.com/courses/1388468")


(define racket-version "8.18")

(define staff
  (list (list "Pierce Darragh" "pdarragh@umd.edu")
        (list "Benjamin Quiring" "bquiring@umd.edu")
        (list "Kalyan Bhetwal" "bhetwal@umd.edu")
        (list "Zhongqi Wang" "zqwang@umd.edu")))

(define lecture-schedule1 "TTh, 2:00-3:15pm")

(define classroom1 "LEF 2205")

;(define discord "TBD")
(define piazza "https://piazza.com/umd/fall2025/cmsc430/home")
(define gradescope "https://www.gradescope.com/courses/1098215/")

(define feedback "https://forms.gle/99yTz7HVfopCaDMz9")

(define (assign-deadline i)
  (list-ref '("Tuesday, September 10, 11:59PM"
              "Thursday, September 12, 11:59PM"
              "Thursday, October 3, 11:59PM"
              "Thursday, October 31, 11:59PM"
              "Tuesday, November 26, 11:59PM")            
            (sub1 i)))
