(defmodule MODULO-SPIEGAZIONE(import MAIN ?ALL)(export ?ALL))

; SPIEGAZIONE DOMANDA
;*******************************************************************************

(deffunction MODULO-SPIEGAZIONE::leggi-nodi($?p)
  (if (> (length ?p) 0) then
    (loop-for-count (?cnt1 1 (length ?p)) do
      (bind ?v (fact-slot-value (nth$ ?cnt1 ?p) nome))
      (assert (spiega ?v))
    )
  else
    (printout t  "- Questa e' una domanda generica posta per identificare il tipo di dispositivo su cui si sta operando." crlf crlf)
  )
)

(deffunction MODULO-SPIEGAZIONE::stampa-spiegazione-domanda(?n-sp ?n ?dom ?risp)
  (printout t ?n-sp ". Alla domanda n." ?n ": " crlf ?dom crlf "l'utente ha risposto: "  ?risp crlf crlf)
)

(deffunction MODULO-SPIEGAZIONE::stampa-spiegazione-attributo(?n-sp ?spiegazione)
  (printout t  ?n-sp ". Il sistema ha dedotto che:" crlf ?spiegazione crlf crlf)
)

; (defrule MODULO-SPIEGAZIONE::debug
;   (declare (salience ?*highest-priority*))
;   =>
;   (printout t "DEBUG >> Modulo spiegazioni" crlf crlf)
; )

(defrule MODULO-SPIEGAZIONE::init-spiegazione-domanda
  ?f <- (fase spiegazione)
  ?target <- (nodo (nome spiegazione) (valore ?attr ))
  ?domanda <- (nodo (nome chiedi) (valore ?attr) (nodo-padre $?p))
  (domanda (attributo ?attr))
  =>
  (printout t "***** MOTIVAZIONI DOMANDA *****" crlf)
  (retract ?target)
  (leggi-nodi ?p)
  ;(bind ?answer (read))
  (assert (contatore-spiegazione 0))
  (retract ?f)
)

; (defrule SPIEGAZIONE::init-spiegazione-domanda-generica
;   ?target <- (nodo (nome spiegazione) (valore ?attr ))
;   ?domanda <- (nodo (nome chiedi) (valore ?attr) (nodo-padre $?p))
;   (domanda (attributo ?attr) (domanda-generica TRUE))
;   =>
;   (printout t "***** MOTIVAZIONI DOMANDA *****" crlf crlf)
;   (printout t "Questa e' una domanda generica necessaria per introdurre al sistema delle informazioni basilari sul dispositivo." crlf)
;   (retract ?target)
; )

(defrule MODULO-SPIEGAZIONE::spiega-domanda
  ?s <- (spiega ?attr)
  ?d <- (domanda (attributo ?attr) (gia-chiesta TRUE) (num-domanda ?n-domanda) (risposta-selezionata ?n-risposta) (testo-domanda ?domanda) (descrizione-risposte $?risposte))
  ?c <- (contatore-spiegazione ?cont)
  =>
  (retract ?c)
  (assert (contatore-spiegazione (+ ?cont 1)))
  (bind ?risposta (nth$ ?n-risposta ?risposte))
  (bind ?n-spieg (+ ?cont 1))
  (stampa-spiegazione-domanda ?n-spieg ?n-domanda ?domanda ?risposta)
  (retract ?s)
)

(defrule MODULO-SPIEGAZIONE::spiega-attributo
  ?s <- (spiega ?attr)
  ?a <- (nodo (nome ?attr) (nodo-padre $?p) (descrizione ?descr))
  ?c <- (contatore-spiegazione ?cont)
  (not (domanda (attributo ?attr) (gia-chiesta TRUE)))
  =>
  (retract ?c)
  (assert (contatore-spiegazione (+ ?cont 1)))
  (retract ?s)
  (bind ?n-spieg (+ ?cont 1))
  (stampa-spiegazione-attributo ?n-spieg ?descr)
  (leggi-nodi ?p)
)

(defrule MODULO-SPIEGAZIONE::end-spiegazione
  (not (nodo (nome spiegazione)))
  (not (spiega ?s))
  =>
  (printout t "Premere 0 e INVIO per tornare alla normale esecuzione del programma." crlf)
  (bind ?answer (read))
  (focus MAIN)
)
