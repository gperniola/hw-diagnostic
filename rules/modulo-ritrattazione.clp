(defmodule MODULO-RITRATTAZIONE(import MAIN ?ALL)(export ?ALL))

; STAMPA ELENCO E REVISIONE DOMANDE
;*******************************************************************************

(deffunction stampa-header-revisione()
  (clear-window)
  (printout t crlf "******************** REVISIONE DOMANDE ********************" crlf crlf)
)

(deffunction stampa-footer-revisione()
  (printout t crlf "***********************************************************" crlf crlf crlf)
)

(deffunction ask-question-revision(?n-domande-chieste)
  (printout t "Inserire il numero di domanda da modificare oppure" crlf "premere 0 per tornare alla normale esecuzione del programma: ")
  (bind ?answer (read))
  (if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))
  (while (and (> ?answer ?n-domande-chieste) (< ?answer 0)) do
    (printout t crlf "Valore inserito non valido, riprovare: ")
    (bind ?answer (read))
  )
?answer)

; (defrule iniz-ritrattazione
;     ?f <- (fase ritrattazione)
;     =>
;     (retract ?f)
;     (assert (avvia-ritrattazione))
;   )

(defrule STAMPA-ELENCO-header
    ;?a <- (avvia-ritrattazione)
    ?f <- (fase ritrattazione)
    =>
    ;(retract ?a)
    (retract ?f)
    (stampa-header-revisione)
    (assert (revisiona-domande))
)

(defrule LOOP-STAMPA-ELENCO-domande
  (revisiona-domande)
  ?d <- (domanda (testo-domanda ?testo) (attributo ?attr) (num-domanda ?n) (gia-chiesta TRUE) (stampata FALSE) (descrizione-risposte $?descr) (risposta-selezionata ?r-selezionata))
  (nodo (nome chiedi) (valore ?attr))
  (not (and (domanda (attributo ?attr2) (num-domanda ?m&:(< ?m ?n))  (gia-chiesta TRUE)(stampata FALSE))
            (nodo (nome chiedi) (valore ?attr2))))
  =>
  (modify ?d (stampata TRUE))
  (printout t "Domanda " ?n ": " ?testo crlf "Risposta: " (nth ?r-selezionata ?descr) crlf crlf)
)

(defrule END-STAMPA-ELENCO
  ?r <-(revisiona-domande)
  (not (and (domanda (attributo ?attr) (gia-chiesta TRUE) (stampata FALSE))
            (nodo (nome chiedi) (valore ?attr))))
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
  ?nodo-partenza <- (nodo (nome chiedi) (valore ?attr) (nodo-padre $?padri) (id-nodo ?id-partenza))
  =>
  (assert (elimina-nodi-da ?id-partenza))
  (bind ?risposta (ask-question-direct ?n ?domanda ?descr))
  (modify ?d (risposta-selezionata ?risposta))
  (retract ?r)


  ; (printout t "- RITRATTAZIONE DA " ?nodo-partenza crlf)
  ; (bind ?h (read))
)

(defrule LOOP-elimina-nodi-da
  (declare (salience ?*highest-priority*))
  ?p1 <- (elimina-nodi-da ?n)
  ?p2 <- (nodo (nodo-padre $?x ?n $?y) (id-nodo ?id-p2))
  =>
  (assert (elimina-nodi-da ?id-p2))
  ;
  ; (printout t "- eliminazione di " ?p2 crlf)
  ; (bind ?h (read))
  (retract ?p2)
)

(defrule END-elimina-nodi-da

  ?p1 <- (elimina-nodi-da ?n)
  (not (nodo (nodo-padre $?x ?n $?y)))
  =>
  ; (bind ?x (modify ?n (stato attivo)) )
  ; (printout t "- attivazione da " ?n " a " ?x crlf)
  ; (bind ?h (read))
  (retract ?p1)
  (assert (fine-revisione))
)

(defrule END-revisiona-domande
  (declare (salience ?*lowest-priority*))
  ?r <- (fine-revisione)
  (not (elimina-nodi-da ?e))
  =>

  ; (printout t "- In end-revisiona-domande" crlf)
  ; (bind ?h (read))
  (retract ?r)
  (focus MAIN) ;;EXIT POINT
  ;(assert (avvia-ritrattazione))
  ;(assert (attiva-nodi))
  ;(assert (init-revisiona-domande))
)

(defrule attiva-nodi-diagnosi-terminali
  (declare (salience ?*highest-priority*))
  ;(attiva-nodi)
  ?p <- (nodo (nome ?n) (valore ?v) (attivo FALSE) (id-nodo ?id))
  (not (nodo (nome ?n) (valore ?v) (nodo-padre $?x ?id $?y)))
  =>
  (modify ?p (attivo TRUE))
  (printout t "Reactivated node " ?id crlf)
)

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
