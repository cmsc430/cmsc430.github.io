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

(define start-date "September 2")
(define m1-date "October 16")
(define m2-date "November 13")
(define midterm-hours "24")
(define final-date "December 18")
(define final-end-time "12:30pm")
(define elms-url "https://umd.instructure.com/courses/1388468")


(define racket-version "8.18")

(define staff
  (list (list "Pierce Darragh" "pdarragh@umd.edu")
        (list "Benjamin Quiring" "bquiring@umd.edu")
        (list "Kalyan Bhetwal" "bhetwal@umd.edu")
        (list "Abhi Senthilkumar" "asenthil@terpmail.umd.edu")
        (list "Zhongqi Wang" "zqwang@umd.edu")
        (list "Kazi Tasnim Zinat" "kzintas@umd.edu")))

(define lecture-schedule1 "TTh, 2:00-3:15pm")

(define classroom1 "LEF 2205")

;(define discord "TBD")
(define piazza "https://piazza.com/umd/fall2025/cmsc430/home")
(define gradescope "https://www.gradescope.com/courses/1098215/")

(define feedback "https://forms.gle/99yTz7HVfopCaDMz9")

(define (assign-deadline i)
  (list-ref '("Thursday, September 11, 11:59PM"
              "Thursday, September 18, 11:59PM"
              "Thursday, September 25, 11:59PM"
              "Thursday, October 2, 11:59PM"
              "Thursday, October 9, 11:59PM"
              "Thursday, October 23, 11:59PM"
              "Thursday, October 30, 11:59PM"
              "Thursday, November 6, 11:59PM"
              "Thursday, November 20, 11:59PM"
              "Thursday, December 4, 11:59PM")
            (sub1 i)))

(define office-hours
  (itemlist
   #:style 'compact
   (item "Monday"
         (itemlist
          (item "9:00am–12:00pm — Zhonqi")
          (item "12:00pm–3:00pm — Ben")
          (item "3:00pm–6:00pm — Abhi")))
   (item "Tuesday"
         (itemlist
          (item "11:00am–2:00pm — Abhi")))
   (item "Wednesday"
         (itemlist
          (item "9:00am–12:00pm — Zhonqi")
          (item "12:00pm–3:00pm — Ben")
          (item "3:00pm–6:00pm — Kalyan")))
   (item "Thursday"
         (itemlist
          (item "10:00am–1:00pm — Zinat")))
   (item "Friday"
         (itemlist
          (item "11:00am–2:00pm — Zinat")
          (item "4:00pm–7:00pm — Kalyan")))))
