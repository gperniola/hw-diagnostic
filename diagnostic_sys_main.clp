
(defglobal ?*highest-priority* = 1000)
(defglobal ?*high-priority* = 100)
(defglobal ?*low-priority* = -100)
(defglobal ?*lowest-priority* = -1000)

(defmodule MAIN (export ?ALL))


;;****************
;;* DEFFUNCTIONS *
;;****************

(deffunction MAIN::ask-question (?question ?allowed-values)
  (printout t ?question crlf "Risposte accettate: " ?allowed-values " : " )
  (bind ?answer (read))
  (if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))
  (while (not (member ?answer ?allowed-values)) do
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) then (bind ?answer (lowcase ?answer))))
  ?answer)

  (deffunction yes-or-no-p (?question)
  (bind ?question (sym-cat ?question " (yes/y/no/n): "))
     (bind ?response (ask-question ?question yes no y n))
     (if (or (eq ?response yes) (eq ?response y))
         then TRUE
         else FALSE))


;;******************
;;* NODE TEMPLATES *
;;******************

  (deftemplate symptom
    (slot name  (type SYMBOL))
    (slot value (type SYMBOL))
    (slot level (type SYMBOL))
  )

  (deftemplate diagnosis
    (slot name        (type SYMBOL))
    (slot description (type STRING))
  )
