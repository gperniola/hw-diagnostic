(defmodule MODULO-RITRATTAZIONE(import MAIN ?ALL)(export ?ALL))



; STAMPA ELENCO E REVISIONE DOMANDE
;*******************************************************************************

; (deffunction stampa-header-revisione()
;   (clear-window)
;   (printout t crlf "******************** REVISIONE DOMANDE ********************" crlf crlf)
; )
;
; (deffunction stampa-footer-revisione()
;   (printout t crlf "***********************************************************" crlf crlf crlf)
; )
;
; (deffunction ask-question-revision(?n-domande-chieste)
;   (printout t "Inserire il numero di domanda da modificare oppure" crlf "premere 0 per tornare alla normale esecuzione del programma: ")
;   (bind ?answer (read))
;   (if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))
;   (while (and (> ?answer ?n-domande-chieste) (< ?answer 0)) do
;     (printout t crlf "Valore inserito non valido, riprovare: ")
;     (bind ?answer (read))
;   )
; ?answer)
;
; ; (defrule iniz-ritrattazione
; ;     ?f <- (fase ritrattazione)
; ;     =>
; ;     (retract ?f)
; ;     (assert (avvia-ritrattazione))
; ;   )
;
; (defrule STAMPA-ELENCO-header
;     ;?a <- (avvia-ritrattazione)
;     ?f <- (fase ritrattazione)
;     =>
;     ;(retract ?a)
;     (retract ?f)
;     (stampa-header-revisione)
;     (assert (revisiona-domande))
; )
;
; (defrule LOOP-STAMPA-ELENCO-domande
;   (revisiona-domande)
;   ?d <- (domanda (testo-domanda ?testo) (attributo ?attr) (num-domanda ?n) (gia-chiesta TRUE) (stampata FALSE) (descrizione-risposte $?descr) (risposta-selezionata ?r-selezionata))
;   (nodo (nome chiedi) (valore ?attr))
;   (not (and (domanda (attributo ?attr2) (num-domanda ?m&:(< ?m ?n))  (gia-chiesta TRUE)(stampata FALSE))
;             (nodo (nome chiedi) (valore ?attr2))))
;   =>
;   (modify ?d (stampata TRUE))
;   (printout t "Domanda " ?n ": " ?testo crlf "Risposta: " (nth ?r-selezionata ?descr) crlf crlf)
; )
;
; (defrule END-STAMPA-ELENCO
;   ?r <-(revisiona-domande)
;   (not (and (domanda (attributo ?attr) (gia-chiesta TRUE) (stampata FALSE))
;             (nodo (nome chiedi) (valore ?attr))))
;   (contatore-domande ?n)
;   =>
;   (printout t crlf crlf)
;   (bind ?risposta (ask-question-revision ?n))
;   (stampa-footer-revisione)
;   (retract ?r)
;   (assert (annulla-stampa-domande))
;   (if (<> ?risposta 0) then (assert (revisiona-da ?risposta))
;     else (focus MAIN) ;;*****EXIT POINT
;   )
; )
;
; (defrule LOOP-STAMPA-ELENCO-reset
;   (annulla-stampa-domande)
;   ?d <- (domanda (stampata TRUE))
;   =>
;   (modify ?d (stampata FALSE))
; )
;
; (defrule END-STAMPA-ELENCO-reset
;   ?a <- (annulla-stampa-domande)
;   (not (domanda (stampata TRUE)))
;   =>
;   (retract ?a)
; )



(deffunction ordina-per-n-domanda(?f1 ?f2)
   (> (fact-slot-value ?f1 num-domanda) (fact-slot-value ?f2 num-domanda)))

; (deffunction resetta-domande-da (?n)
;     (bind ?domande (find-all-facts ((?d domanda))(>= ?d:num-domanda ?n)))
;     (progn$ (?x ?domande)
;       (modify ?x (num-domanda 0) (gia-chiesta FALSE) (risposta-selezionata 0))
;     )
; )
;
; (deffunction reimposta-nodo-domanda-attuale (?n)
;   (if (= ?n 1) then (return 0)
;   else
;     (bind ?domanda (find-fact ((?d domanda))(eq ?d:num-domanda (- ?n 1))))
;     (bind ?attr (fact-slot-value (nth$ 1 ?domanda) attributo))
;     (bind ?nodo-chiedi (find-fact ((?c nodo))(and(eq ?c:nome chiedi) (eq ?c:valore ?attr))))
;     (return (fact-slot-value (nth$ 1 ?nodo-chiedi) id-nodo))
;   )
; )





(deffunction MODULO-RITRATTAZIONE::elimina-nodi-da (?n)
    (bind ?nodi-figli (find-all-facts ((?d nodo))(member$ ?n ?d:nodo-padre)))
    (progn$ (?x ?nodi-figli)
        (bind ?id-nodo (fact-slot-value ?x id-nodo))
        (if (neq ?id-nodo FALSE) then
            ;(printout t "- deleting inner " ?id-nodo " - " (fact-slot-value ?x nome) " " (fact-slot-value ?x valore) crlf )
            (retract ?x)
            (elimina-nodi-da ?id-nodo)
        )
    )
)



(deffunction MODULO-RITRATTAZIONE::ritratta-da-domanda (?n)
    (bind ?domanda-ritratt (find-fact ((?d domanda))(eq ?d:num-domanda ?n)))
    (bind ?dom1 (nth$ 1 ?domanda-ritratt))
    (bind ?attr (fact-slot-value ?dom1 attributo))
    (bind ?risposte-valide (fact-slot-value ?dom1 risposte-valide))
    (bind ?descrizioni-valide (fact-slot-value ?dom1 descrizione-risposte))

    ;(modify ?dom1 (num-domanda 0) (gia-chiesta FALSE) (risposta-selezionata 0))
    (bind ?nodo-chiedi1 (find-fact ((?c nodo)) (and (eq ?c:nome chiedi) (eq ?c:valore ?attr) (eq ?c:attivo TRUE))))
    (bind ?id-nodo-chiedi1 (fact-slot-value (nth$ 1 ?nodo-chiedi1) id-nodo))
    (elimina-nodi-da ?id-nodo-chiedi1)

    (bind ?domande (find-all-facts ((?d domanda))(> ?d:num-domanda ?n)))
    (progn$ (?x ?domande)
      (bind ?attr2 (fact-slot-value ?x attributo))
      (modify ?x (num-domanda 0) (gia-chiesta FALSE) (risposta-selezionata 0))
      (bind ?nodo-chiedi (find-fact ((?c nodo)) (and (eq ?c:nome chiedi) (eq ?c:valore ?attr2) (eq ?c:attivo TRUE))))
      (if (> (length$ ?nodo-chiedi) 0) then
          (bind ?id-nodo-chiedi (fact-slot-value (nth$ 1 ?nodo-chiedi) id-nodo))
          (retract (nth$ 1 ?nodo-chiedi))
          (elimina-nodi-da ?id-nodo-chiedi)
      )
    )

    (bind ?nuova-risposta (ask-question ?n ?dom1))
    (modify ?dom1 (risposta-selezionata ?nuova-risposta))
    (assert (nodo (nome ?attr) (valore (nth$ ?nuova-risposta ?risposte-valide)) (descrizione (nth$ ?nuova-risposta ?descrizioni-valide)) (sorgente-info utente) (nodo-padre ?id-nodo-chiedi1)))
)



(deffunction MODULO-RITRATTAZIONE::stampa-tutte-le-domande (?n-domande-chieste)
    ;(clear-window)
    (printout t crlf "              *********** DOMANDE CHIESTE ***********" crlf)

    (bind ?domande (find-all-facts ((?d domanda))(eq ?d:gia-chiesta TRUE)))
    (bind ?domande (sort ordina-per-n-domanda ?domande))
    (progn$ (?x ?domande)
            (format t "%n%n%2d. %s%n" (fact-slot-value ?x num-domanda)(upcase(fact-slot-value ?x testo-domanda)))
            (format t "     Risposta utente: %s%n" (nth$ (fact-slot-value ?x risposta-selezionata)(fact-slot-value ?x descrizione-risposte)))
            (format t "     Altre risposte valide:%n")
            (loop-for-count (?cnt1 1 (length (fact-slot-value ?x descrizione-risposte))) do
                  (if (neq ?cnt1 (fact-slot-value ?x risposta-selezionata)) then
                      (format t "     - %s%n" (nth$ ?cnt1 (fact-slot-value ?x descrizione-risposte)))
                  )
            )
    )
    (printout t crlf crlf "Inserire il numero della domanda da modificare: ")
    (bind ?answer (read))
    (if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))
    (while (or (> ?answer ?n-domande-chieste) (< ?answer 0)) do
      (printout t crlf "Valore inserito non valido, riprovare: ")
      (bind ?answer (read))
    )
    (return ?answer)
)




(defrule MODULO-RITRATTAZIONE::avvia-ritrattazione
  ?c <- (contatore-domande ?n-dom-chieste)
  ?fase <- (fase-ritrattazione)
  =>
  (bind ?punto-di-riavvio (stampa-tutte-le-domande ?n-dom-chieste))
  (retract ?c)
  (assert (contatore-domande ?punto-di-riavvio))
  (ritratta-da-domanda ?punto-di-riavvio)
  (retract ?fase)
)


; RITRATTAZIONE
;****************************************************************************


;
; (defrule MODULO-RITRATTAZIONE::print-ritratta
;   (declare (salience 1200))
;   (or
;     ?x <- (fase-ritrattazione)
;     ?x <- (fase-ricerca-diagnosi)
;     ?x <- (contatore-domande ?i)
;     ?x <- (nodo-domanda-attuale ?a)
;   )
;   =>
;   (printout t "CONDITION: " ?x crlf)
; )
;
; (defrule MODULO-RITRATTAZIONE::print-ritratta2
;   (declare (salience 1200))
;   ?r <- (ritratta-da ?n)
;   ?d <- (domanda (attributo ?attr)(testo-domanda ?domanda) (risposte-valide $?risposte) (descrizione-risposte $?descr) (num-domanda ?n) (gia-chiesta TRUE))
;   ?nodo-partenza <- (nodo (nome chiedi) (valore ?attr) (nodo-padre $?padri) (id-nodo ?id-partenza))
;   =>
;   (printout t "CONDITION RITRATTA OK " crlf)
; )
;
;
; (defrule MODULO-RITRATTAZIONE::ritratta-domande-da
;   ?fase <- (fase-ritrattazione)
;   ?r <- (ritratta-da ?n)
;   ?cnt-dom <- (contatore-domande ?i)
;   ;;;;;;?nodo-dom-attuale <- (nodo-domanda-attuale ?a)
;   ?d <- (domanda (attributo ?attr)(testo-domanda ?domanda) (risposte-valide $?risposte) (descrizione-risposte $?descr) (num-domanda ?n) (gia-chiesta TRUE))
;   ?nodo-partenza <- (nodo (nome chiedi) (valore ?attr) (nodo-padre $?padri) (id-nodo ?id-partenza))
;   =>
;   (printout t "BREAKPOINT HERE" crlf)
;   (assert (elimina-nodi-da ?id-partenza))
;   (resetta-domande-da ?n)
;   ;;;; (retract ?nodo-dom-attuale)
;   ;;;;; (assert (nodo-domanda-attuale (reimposta-nodo-domanda-attuale ?n)))
;   (retract ?cnt-dom)
;   (assert (contatore-domande (- ?n 1)))
;   (retract ?r)
;   (retract ?fase)
;   (printout t "BREAKPOINT END "  crlf)
; )
;
; (defrule MODULO-RITRATTAZIONE::LOOP-elimina-nodi-da
;   (declare (salience ?*highest-priority*))
;   ?p1 <- (elimina-nodi-da ?n)
;   ?p2 <- (nodo (nodo-padre $?x ?n $?y) (id-nodo ?id-p2))
;   =>
;   (assert (elimina-nodi-da ?id-p2))
;   (retract ?p2)
; )
;
; (defrule MODULO-RITRATTAZIONE::END-elimina-nodi-da
;
;   ?p1 <- (elimina-nodi-da ?n)
;   (not (nodo (nodo-padre $?x ?n $?y)))
;   =>
;   (retract ?p1)
;   (assert (fine-revisione))
; )
;
; (defrule MODULO-RITRATTAZIONE::END-revisiona-domande
;   (declare (salience ?*lowest-priority*))
;   ?r <- (fine-revisione)
;   ?r2 <- (fase-ritrattazione)
;   (not (elimina-nodi-da ?e))
;   =>
;   (retract ?r)
;   (retract ?r2)
; )

(defrule MODULO-RITRATTAZIONE::riattiva-nodi-senza-figli-uguali
  (declare (salience ?*highest-priority*))
  ;(attiva-nodi)
  ?p <- (nodo (nome ?n) (valore ?v) (attivo FALSE) (id-nodo ?id))
  (not (nodo (nome ?n) (valore ?v) (nodo-padre $?x ?id $?y)))
  =>
  (modify ?p (attivo TRUE))
  ;(printout t "Reactivated node " ?id crlf)
)
