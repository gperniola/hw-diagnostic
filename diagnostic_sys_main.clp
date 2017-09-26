
(defglobal ?*highest-priority* = 1000)
(defglobal ?*high-priority* = 100)
(defglobal ?*low-priority* = -100)
(defglobal ?*lowest-priority* = -1000)

;(defmodule MAIN (export ?ALL))


;;****************
;;* DEFFUNCTIONS *
;;****************

; (deffunction ask-question (?question ?allowed-values)
;   (printout t ?question crlf "Risposte accettate: " ?allowed-values " : " )
;   (bind ?answer (read))
;   (if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))
;   (while (not (member ?answer ?allowed-values)) do
;       (printout t ?question)
;       (bind ?answer (read))
;       (if (lexemep ?answer) then (bind ?answer (lowcase ?answer))))
;   ?answer)


(deffunction ask-question (?question $?allowed-values)
  (printout t ?question crlf)
  (loop-for-count (?cnt1 1 (length ?allowed-values)) do
      (printout t ?cnt1 ". " (nth$ ?cnt1 ?allowed-values) crlf)
  )
  (bind ?answer (read))
  (if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))
  (while (not (member (nth$ ?answer ?allowed-values) ?allowed-values)) do
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) then (bind ?answer (lowcase ?answer))))
   ?answer)



  ; (deffunction yes-or-no-p (?question)
  ; (bind ?question (sym-cat ?question " (yes/y/no/n): "))
  ;    (bind ?response (ask-question ?question yes no y n))
  ;    (if (or (eq ?response yes) (eq ?response y))
  ;        then TRUE
  ;        else FALSE))


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

  (deftemplate domanda
    (slot attributo     (type SYMBOL) (default ?NONE))
    (slot testo-domanda (type STRING) (default ?NONE))
    (multislot risposte-valide (type SYMBOL) (default ?NONE))
    (multislot descrizione-risposte (type STRING) (default ?NONE))
    (slot gia-chiesta   (default  FALSE))
  )


;;******************
;;* CONTROL RULES  *
;;******************

(defrule inizializzazione
  (declare (salience ?*highest-priority*))
  =>
  (assert (info (nome tipo-dispositivo) (valore sconosciuto)))
  (printout t crlf crlf)
  (printout t   "***                                                 ***" crlf
                "**  SISTEMA DIAGNOSTICO PER DISPOSITIVI ELETTRONICI  **" crlf
                "*                                                     *" crlf
                "*     Rispondere alle domande inserendo il numero     *" crlf
                "**       corrispondente alla risposta corretta.      **" crlf
                "***                                                 ***" crlf crlf))

(defrule diagnosi-trovata
  (declare (salience ?*highest-priority*))
  (diagnosi (nome ?nome) (descrizione ?desc))
  =>
  (printout t crlf ">>>> Diagnosi del guasto: " ?nome " - " ?desc crlf crlf)
  (halt))
  ; (if (yes-or-no-p "Would you like to revise the diagnosis?")
  ;     then (assert (revise-diagnosis))
  ;     else (halt)))


;;********************
;;* INFERENCE RULES  *
;;********************




;;********************
;;* QUESTIONS RULES  *
;;********************

(defrule chiedi-tipo-dispositivo
  (declare (salience ?*low-priority*))
  ?d <- (info (nome tipo-dispositivo) (valore sconosciuto))
  ?f <- (domanda (attributo tipo-dispositivo) (testo-domanda ?domanda) (risposte-valide $?risposte) (descrizione-risposte $?descrizioni))
  =>
  (bind ?risposta (ask-question ?domanda ?descrizioni))
  (assert (info (nome tipo-dispositivo) (valore (nth$ ?risposta ?risposte))))
  (retract ?d)
  (modify ?f (gia-chiesta TRUE))
)

;;********************
;;* QUESTIONS FACTS  *
;;********************

(deffacts domande
  (domanda  (attributo tipo-dispositivo)
            (testo-domanda "A quale tipologia appartiene il dispositivo?")
            (risposte-valide pc-fisso pc-portatile tablet smartphone)
            (descrizione-risposte "Pc fisso" "Pc portatile/Netbook" "Tablet" "Smartphone"))

)


;;********************
;;* DIAGNOSIS RULES  *
;;********************

(defrule diagnosi-pc
  (info (nome tipo-dispositivo) (valore pc))
  =>
  (assert (diagnosi (nome diagnosi-pc) (descrizione "Pc guasto.")))
)

(defrule diagnosi-tablet
  (info (nome tipo-dispositivo) (valore tablet))
  =>
  (assert (diagnosi (nome diagnosi-tablet) (descrizione "Tablet guasto.")))
)

(defrule diagnosi-smartphone
  (info (nome tipo-dispositivo) (valore smartphone))
  =>
  (assert (diagnosi (nome diagnosi-smartphone) (descrizione "Smartphone guasto.")))
)
