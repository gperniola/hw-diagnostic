(defmodule MODULO-RITRATTAZIONE(import MAIN ?ALL)(export ?ALL))

(deffunction ordina-per-n-domanda(?f1 ?f2)
   (> (fact-slot-value ?f1 num-domanda) (fact-slot-value ?f2 num-domanda)))


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


(defrule MODULO-RITRATTAZIONE::riattiva-nodi-senza-figli-uguali
  (declare (salience ?*highest-priority*))
  ;(attiva-nodi)
  ?p <- (nodo (nome ?n) (valore ?v) (attivo FALSE) (id-nodo ?id))
  (not (nodo (nome ?n) (valore ?v) (nodo-padre $?x ?id $?y)))
  =>
  (modify ?p (attivo TRUE))
  ;(printout t "Reactivated node " ?id crlf)
)
