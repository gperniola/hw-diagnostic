
(defglobal ?*highest-priority* = 1000)
(defglobal ?*high-priority* = 100)
(defglobal ?*low-priority* = -100)
(defglobal ?*lowest-priority* = -1000)

;(defmodule MAIN (export ?ALL))


;;****************
;;* DEFFUNCTIONS *
;;****************

(deffunction ask-question (?question ?allowed-values)
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

  (deftemplate sintomo
    (slot nome    (type SYMBOL))
    (slot valore  (type SYMBOL))
    (slot livello (type SYMBOL))
  )

  (deftemplate diagnosi
    (slot nome        (type SYMBOL))
    (slot descrizione (type STRING))
  )

  (deftemplate info
    (slot nome    (type SYMBOL))
    (slot valore  (type SYMBOL))
  )

;;******************
;;* CONTROL RULES  *
;;******************

(defrule inizializzazione
  (declare (salience ?*highest-priority*))
  =>
  (assert (info (nome tipo-dispositivo) (valore sconosciuto)))
  (printout t crlf crlf)
  (printout t "*** SISTEMA DIAGNOSTICO PER DISPOSITIVI ELETTRONICI ***" crlf crlf))

(defrule diagnosi-trovata
  (declare (salience ?*highest-priority*))
  (diagnosi ?d)
  =>
  (printout t crlf ">>>> Diagnosi del guasto: " ?d crlf crlf)
  (halt))
  ; (if (yes-or-no-p "Would you like to revise the diagnosis?")
  ;     then (assert (revise-diagnosis))
  ;     else (halt)))
