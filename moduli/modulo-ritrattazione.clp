(defmodule MODULO-RITRATTAZIONE(import MAIN ?ALL)(export ?ALL))

;** Questa funzione indica come ordinare i valori ?f1 e ?f2 utilizzando lo slot num-domanda **
(deffunction ordina-per-n-domanda(?f1 ?f2)
   (> (fact-slot-value ?f1 num-domanda) (fact-slot-value ?f2 num-domanda)))


;** Questa funzione elimina i nodi figli del nodo ?n **
(deffunction MODULO-RITRATTAZIONE::elimina-nodi-da (?n)
    (bind ?nodi-figli (find-all-facts ((?d nodo))(member$ ?n ?d:nodo-padre)))
    (progn$ (?x ?nodi-figli)
        (bind ?id-nodo (fact-slot-value ?x id-nodo))
        (if (neq ?id-nodo FALSE) then
            (retract ?x)
            (elimina-nodi-da ?id-nodo)
        )
    )
)


;** Questa funzione si occupa di eliminare i fatti in memoria generati a partire dalla domanda numero ?n **
;** vengono eliminati anche i nodi generati dalle domandi successive ad ?n                               **
(deffunction MODULO-RITRATTAZIONE::ritratta-da-domanda (?n)
    (bind ?domanda-ritratt (find-fact ((?d domanda))(eq ?d:num-domanda ?n)))
    (bind ?dom1 (nth$ 1 ?domanda-ritratt))
    (bind ?attr (fact-slot-value ?dom1 attributo))
    (bind ?risposte-valide (fact-slot-value ?dom1 risposte-valide))
    (bind ?risposta-utente (nth (fact-slot-value ?dom1 risposta-selezionata) ?risposte-valide))

    (bind ?descrizioni-valide (fact-slot-value ?dom1 descrizione-risposte))
    (bind ?spiegazioni-valide (fact-slot-value ?dom1 spiegazione-risposte))
    (bind ?nodo-domanda-da-ritrattare (find-fact ((?c nodo)) (and (eq ?c:nome ?attr) (eq ?c:valore ?risposta-utente) (eq ?c:sorgente-info utente))))
    (bind ?id-nodo-domanda-da-ritrattare (fact-slot-value (nth$ 1 ?nodo-domanda-da-ritrattare) id-nodo))

    (elimina-nodi-da ?id-nodo-domanda-da-ritrattare) ;** elimino i nodi generati dalla domanda ?n

    ;** recupero tutte le domande successive ad ?n
    (bind ?domande (find-all-facts ((?d domanda))(> ?d:num-domanda ?n)))
    (progn$ (?x ?domande) ;** Per ogni domanda..
      (bind ?attr2 (fact-slot-value ?x attributo))
      (bind ?risposte-valide2 (fact-slot-value ?x risposte-valide))
      (bind ?risposta-utente2 (nth (fact-slot-value ?x risposta-selezionata) ?risposte-valide2))

      (modify ?x (num-domanda 0) (gia-chiesta FALSE) (risposta-selezionata 0)) ;** imposto la domanda come non chiesta
      (bind ?nodo-domanda-da-eliminare (find-fact ((?c nodo)) (and (eq ?c:nome ?attr2) (eq ?c:valore ?risposta-utente2) (eq ?c:sorgente-info utente))))
      (if (> (length$ ?nodo-domanda-da-eliminare) 0) then
          (bind ?id-nodo-domanda-da-eliminare (fact-slot-value (nth$ 1 ?nodo-domanda-da-eliminare) id-nodo))
          (retract (nth$ 1 ?nodo-domanda-da-eliminare))
          (elimina-nodi-da ?id-nodo-domanda-da-eliminare) ;** elimino i nodi generati dalla domanda
      )
    )

    (bind ?*num-domande-chieste* (- ?n 1))
    (bind ?nuovo-val (chiedi-domanda-fnz ?attr)) ;** chiedo nuovamente la domanda ?n all'utente
    (bind ?desc (get-descrizione-risposta ?attr ?nuovo-val))
    (retract (nth$ 1 ?nodo-domanda-da-ritrattare))
    (assert (nodo (nome ?attr) (valore ?nuovo-val) (descrizione ?desc) (sorgente-info utente))) ;** asserisco un nuovo fatto in base alla risposta dell'utente
)


;** Qyesta funzione stampa tutte le domande a cui l'utente ha già risposto, stampa tutte le possibili risposte e quelle selezionate **
;** dall'utente, quindi chiede di selezionare il numero della domanda da modificare e lo restituisce                                **
(deffunction MODULO-RITRATTAZIONE::stampa-tutte-le-domande (?n-domande-chieste)
    (printout t crlf "              *********** DOMANDE CHIESTE ***********" crlf)

    (bind ?domande (find-all-facts ((?d domanda))(eq ?d:gia-chiesta TRUE)))
    (bind ?domande (sort ordina-per-n-domanda ?domande))
    (progn$ (?x ?domande)
            (format t "%n%n%n DOMANDA%2d: %s%n" (fact-slot-value ?x num-domanda)(fact-slot-value ?x testo-domanda))
            (format t " POSSIBILI RISPOSTE:%n")
            (loop-for-count (?cnt1 1 (length (fact-slot-value ?x descrizione-risposte))) do
                  (format t "          - %s%n" (nth$ ?cnt1 (fact-slot-value ?x descrizione-risposte)))
            )
            (format t "%n LA TUA RISPOSTA: %s%n" (nth$ (fact-slot-value ?x risposta-selezionata)(fact-slot-value ?x descrizione-risposte)))
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


;** Questa funzione serve per chiedere all'utente se vuole avviare la ritrattazione, attivandola in caso di risposta positiva **
(deffunction MODULO-RITRATTAZIONE::chiedi-soddisfazione-utente ()
  (printout t crlf crlf "L'utente vuole rivedere le domande a cui ha gia' risposto?")
  (printout t crlf "1. Si")
  (printout t crlf "2. No" crlf crlf)
  (printout t "Inserire risposta: ")
  (bind ?answer (read))
  (if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))

  (while (not (and(>= ?answer 1) (<= ?answer 2)))
      (printout t "Valore non riconosciuto, riprovare: ")
      (bind ?answer (read))
      (if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))
  )
  (if (= ?answer 2) then
      (printout t crlf crlf "Premere INVIO per riavviare il programma..." crlf)
      (readline)
      (reset)
      (run)
  else
    (bind ?punto-di-riavvio (stampa-tutte-le-domande ?*num-domande-chieste*))
    (ritratta-da-domanda ?punto-di-riavvio)
    )
)


;** Questa regola viene attivata quando il modulo passa in focus, chiede all'utente se vuole visualizzare e modificare le domande **
(defrule MODULO-RITRATTAZIONE::avvia-ritrattazione
  ?f <- (in-esecuzione)
  =>
  (chiedi-soddisfazione-utente)
  (retract ?f)
)


;** Questa regola viene attivata quando un nodo non attivo non ha figli identici e quindi dev'essere reimpostato ad attivo, **
;** questo è necessario perché durante la ritrattazione alcuni nodi vengono eliminati                                       **
(defrule MODULO-RITRATTAZIONE::riattiva-nodi-senza-figli-uguali
  (declare (salience ?*highest-priority*))
  ?p <- (nodo (nome ?n) (valore ?v) (attivo FALSE) (id-nodo ?id))
  (not (nodo (nome ?n) (valore ?v) (nodo-padre $?x ?id $?y)))
  =>
  (modify ?p (attivo TRUE))
)
