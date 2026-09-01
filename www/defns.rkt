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
(define year "2026")
(define courseno "CMSC 430")

(define lecture-dates "September 1 -- December 10, 2026")

(define IRB "IRB") 
(define AVW "AVW")
(define KEY "KEY")

(define office-hour-location (link "https://discord.gg/Qa2x6akMy" "Discord"))

(define start-date "September 1")
(define exam1-date "October 1")
(define exam2-date "November 5")
;(define exam3-date "December 10")
;(define exam-hours "24")
(define final-date "December 17")
(define exam-end-time "11:59pm")
(define final-time "10:30am--12:30pm")
(define elms-url "https://umd.instructure.com/courses/1416023")

(define online? #f)

(define racket-version "9.3")

(define staff
  (list (list "Pierce Darragh" "pdarragh@umd.edu")
        (list "Le Chang" "lchang21@umd.edu")
        (list "Deena Postol" "dpostol@umd.edu")
        (list "Zhongqi Wang" "zqwang@umd.edu")))

(define lecture-schedule1 "TTh, 2-3:15pm")

(define classroom1 (link "https://25live.collegenet.com/pro/umd#!/home/location/1792/details" "LEF 2205"))

;(define discord "TBD")
(define piazza "https://piazza.com/umd/fall2026/cmsc430/home")
(define gradescope "https://www.gradescope.com/courses/1381335")

(define feedback "https://forms.gle/99yTz7HVfopCaDMz9")

(define (practice-deadline i)
  (list-ref '("Thursday, September 3, 11:59PM"
              "Thursday, September 10, 11:59PM")
            (sub1 i)))

(define (assign-deadline i)
  (list-ref '("Thursday, September 10, 11:59PM"
              "Thursday, September 17, 11:59PM"
              "Thursday, September 24, 11:59PM"
              "Thursday, October 8, 11:59PM"
              "Thursday, October 8, 11:59PM"
              "Thursday, October 22, 11:59PM"
              "Thursday, October 29, 11:59PM"
              "Thursday, November 12, 11:59PM"
              "Thursday, December 3, 11:59PM")
            (sub1 i)))

(define office-hours
  "TBD")

(define prof-office-hours
  "TBD")
