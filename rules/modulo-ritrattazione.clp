(defmodule MODULO-RITRATTAZIONE(import MAIN ?ALL)(export ?ALL))

; STAMPA ELENCO E REVISIONE DOMANDE
;*******************************************************************************

(deffunction stampa-header-revisione()
  ;(clear-window)
  (printout t crlf "******************** REVISIONE DOMANDE ********************" crlf crlf)
)

(deffunction stampa-footer-revisione()
  (printout t crlf "***********************************************************" crlf crlf crlf)
)

(defrule iniz-ritrattazione
    =>
    (assert (avvia-ritrattazione))
  )

(defrule STAMPA-ELENCO-header
    ?a <- (avvia-ritrattazione)
    =>
    (retract ?a)
    (stampa-header-revisione)
    (assert (revisiona-domande))
)

(defrule LOOP-STAMPA-ELENCO-domande
  (revisiona-domande)
  ?d <- (domanda (testo-domanda ?testo) (attributo ?attr) (num-domanda ?n) (gia-chiesta TRUE) (stampata FALSE) (descrizione-risposte $?descr) (risposta-selezionata ?r-selezionata))
  (not (domanda (num-domanda ?m&:(< ?m ?n))  (gia-chiesta TRUE)(stampata FALSE)))
  =>
  (modify ?d (stampata TRUE))
  (printout t "Domanda " ?n ": " ?testo crlf "Risposta: " (nth ?r-selezionata ?descr) crlf crlf)
)

(defrule END-STAMPA-ELENCO
  ?r <-(revisiona-domande)
  (not (domanda (gia-chiesta TRUE) (stampata FALSE)))
  (contatore-domande ?n)
  =>
  (printout t crlf crlf)
  (bind ?risposta (ask-question-revision ?n))
  (stampa-footer-revisione)
  (retract ?r)
  (assert (annulla-stampa-domande))
  (if (<> ?risposta 0) then (assert (revisiona-da ?risposta))
    else (focus MAIN) ;;*****EXIT POINT
  )
)

(defrule LOOP-STAMPA-ELENCO-reset
  (annulla-stampa-domande)
  ?d <- (domanda (stampata TRUE))
  =>
  (modify ?d (stampata FALSE))
)

(defrule END-STAMPA-ELENCO-reset
  ?a <- (annulla-stampa-domande)
  (not (domanda (stampata TRUE)))
  =>
  (retract ?a)
)


; REVISIONA DOMANDA (RITRATTAZIONE)
;****************************************************************************


(defrule revisiona-da
  (not(annulla-stampa-domande))
  ?r <- (revisiona-da ?n)
  ?d <- (domanda (attributo ?attr)(testo-domanda ?domanda) (risposte-valide $?risposte) (descrizione-risposte $?descr) (num-domanda ?n) (gia-chiesta TRUE))
  ?nodo-partenza <- (nodo (nome chiedi) (valore ?attr) (nodo-padre $?padri))
  =>
  (assert (elimina-nodi-da ?nodo-partenza))
  (bind ?risposta (ask-question-direct ?n ?domanda ?descr))
  (modify ?d (risposta-selezionata ?risposta))
  (retract ?r)
  (assert (fine-revisione))
)

(defrule LOOP-elimina-nodi-da
  ?p1 <- (elimina-nodi-da ?n)
  ?p2 <- (nodo (nodo-padre $?x ?n $?y))
  =>
  (assert (elimina-nodi-da ?p2))
  (retract ?p2)
)

(defrule END-elimina-nodi-da
  ?p1 <- (elimina-nodi-da ?n)
  (not (nodo (nodo-padre $?x ?n $?y)))
  =>
  (retract ?p1)
)

(defrule END-revisiona-domande
  ?r <- (fine-revisione)
  (not (elimina-nodi da ?e))
  =>
  (retract ?r)
  (assert (avvia-ritrattazione))
  ;(assert (attiva-nodi))
  ;(assert (init-revisiona-domande))
)

; (defrule attiva-nodi-diagnosi-terminali
;   ;(declare (salience ?*highest-priority*))
;   (attiva-nodi)
;   ?n1 <- (nodo (nome diagnosi) (valore ?v) (stato inattivo))
;   (not (nodo (nome diagnosi) (valore ?v) (nodo-padre $?x ?n1 $?y)))
;   =>
;   (modify ?n1 (stato attivo))
; )

; (defrule attiva-nodi-diagnosi-terminali
;   ?r <- (attiva-nodi)
;   ?n1 <- (nodo (nome diagnosi) (valore ?v) (stato inattivo))
;   (not (nodo (nome diagnosi) (valore ?v) (nodo-padre $?x ?n1 $?y)))
;   =>
;   (modify ?n1 (stato attivo))
; )

; (defrule END-attiva-nodi-diagnosi-terminali
;   ?r <- (attiva-nodi)
;   (not (and (nodo (nome diagnosi) (valore ?v) (stato inattivo))
;             (not(nodo (nome diagnosi) (valore ?v) (nodo-padre $?x ?n1 $?y)))))
;   =>
;   (retract ?r)
;   (assert (avvia-ritrattazione))
; )
