(defmodule MODULO-RITRATTAZIONE(import MAIN ?ALL)(export ?ALL))


(deffunction count$ (?list ?value)
   (bind ?count 0)
   (foreach ?l ?list
      (if (eq ?l ?value)
         then
         (bind ?count (+ ?count 1))))
   (return ?count))

   (deffunction delete-duplicates$ (?list ?value)
      (bind ?count (count$ ?list ?value))
      (if (<= ?count 1)
         then
         (return ?list))
      (loop-for-count (- ?count 1)
         (bind ?pos (member$ ?value ?list))
         (bind ?list (delete$ ?list ?pos ?pos)))
      (return ?list))


    (deffunction delete-duplicates-val$ (?list)
       (foreach ?v ?list
         (bind ?list (delete-duplicates$ ?list ?v))
       )
       (bind ?list (sort > ?list))
       (bind ?list (delete$ ?list 1 1))
      (return ?list)
    )

  (deffunction ordinamento-per-certezza (?f1 ?f2)
    (< (fact-slot-value ?f1 certezza) (fact-slot-value ?f2 certezza)))


    (deffunction spiega(?n)
      (bind ?diagnosi-da-spiegare (find-fact ((?d nodo))(eq ?d:id-nodo ?n)))
      (bind ?diag (nth$ 1 ?diagnosi-da-spiegare))
      (bind ?nome-nodo (fact-slot-value ?diag nome))
      (bind ?valore-nodo (fact-slot-value ?diag valore))
      (bind ?nodi-padre (fact-slot-value ?diag nodo-padre))
      (bind ?list 0)
      (loop-for-count (?cnt1 1 (length$ ?nodi-padre)) do
        (bind ?id-nodo-padre (nth$ ?cnt1 ?nodi-padre))
        (bind ?n-padre (find-fact ((?d nodo))(eq ?d:id-nodo ?id-nodo-padre)))
        (bind ?nodo-padre (nth$ 1 ?n-padre))
        (bind ?nome-nodo-padre (fact-slot-value ?nodo-padre nome))
        (bind ?valore-nodo-padre (fact-slot-value ?nodo-padre valore))
        (if (and (eq ?nome-nodo ?nome-nodo-padre) (eq ?valore-nodo ?valore-nodo-padre)) then
            (bind ?list  ?list (spiega ?id-nodo-padre))
            else
              (bind ?list ?list ?id-nodo-padre)
        )
      )
      (return ?list)
    )

    (deffunction stampa-spiega(?titolo-diagnosi ?lista-id)
      (printout t crlf crlf "    DIAGNOSI:  " ?titolo-diagnosi crlf " MOTIVAZIONI: " crlf)
      (progn$ (?id ?lista-id)
        (bind ?nodo-da-spiegare (find-fact ((?d nodo))(eq ?d:id-nodo ?id)))
        (bind ?nodo (nth$ 1 ?nodo-da-spiegare))
        (bind ?descrizione (fact-slot-value ?nodo descrizione))
        (printout t "            -  " ?descrizione crlf)
      )
    )


    (deffunction stampa-spiegaz-diagnosi ()
      (printout t crlf "              *********** SPIEGAZIONE DELLE DIAGNOSI RISCONTRATE ***********" crlf)
      (bind ?diagnosi (find-all-facts ((?n nodo))(and (eq ?n:nome diagnosi)(>= ?n:certezza 0.70)(eq ?n:attivo TRUE))))
      (bind ?diagnosi (sort ordinamento-per-certezza ?diagnosi))
      (progn$ (?x ?diagnosi)
          (bind ?data-diagnosi (find-all-facts ((?d diagnosi))(eq (fact-slot-value ?x valore) ?d:attributo)))
          (progn$ (?y ?data-diagnosi)
              (bind ?lista (spiega (fact-slot-value ?x id-nodo)))
              (bind ?lista (delete-duplicates-val$ ?lista))
              (stampa-spiega (fact-slot-value ?y titolo) ?lista)
          )
      )
    )




(deffunction ordina-per-n-domanda(?f1 ?f2)
   (> (fact-slot-value ?f1 num-domanda) (fact-slot-value ?f2 num-domanda)))


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


(deffunction MODULO-RITRATTAZIONE::ritratta-da-domanda (?n)
    (bind ?domanda-ritratt (find-fact ((?d domanda))(eq ?d:num-domanda ?n)))
    (bind ?dom1 (nth$ 1 ?domanda-ritratt))
    (bind ?attr (fact-slot-value ?dom1 attributo))
    (bind ?risposte-valide (fact-slot-value ?dom1 risposte-valide))
    (bind ?descrizioni-valide (fact-slot-value ?dom1 descrizione-risposte))
    (bind ?spiegazioni-valide (fact-slot-value ?dom1 spiegazione-risposte))
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
    (assert (nodo (nome ?attr) (valore (nth$ ?nuova-risposta ?risposte-valide)) (descrizione (nth$ ?nuova-risposta ?spiegazioni-valide)) (sorgente-info utente) (nodo-padre ?id-nodo-chiedi1)))
)



(deffunction MODULO-RITRATTAZIONE::stampa-tutte-le-domande (?n-domande-chieste)
    ;(clear-window)
    (printout t crlf "              *********** DOMANDE CHIESTE ***********" crlf)

    (bind ?domande (find-all-facts ((?d domanda))(eq ?d:gia-chiesta TRUE)))
    (bind ?domande (sort ordina-per-n-domanda ?domande))
    (progn$ (?x ?domande)
            (format t "%n%n%n DOMANDA%2d: %s%n" (fact-slot-value ?x num-domanda)(fact-slot-value ?x testo-domanda))
            (format t "  RISPOSTA: %s%n" (nth$ (fact-slot-value ?x risposta-selezionata)(fact-slot-value ?x descrizione-risposte)))
            (format t "     ALTRE:%n")
            (loop-for-count (?cnt1 1 (length (fact-slot-value ?x descrizione-risposte))) do
                  (if (neq ?cnt1 (fact-slot-value ?x risposta-selezionata)) then
            (format t "          - %s%n" (nth$ ?cnt1 (fact-slot-value ?x descrizione-risposte)))
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
  (clear-window)
  (stampa-spiegaz-diagnosi)
  (printout t crlf crlf "Premere INVIO per modificare le domande a cui l'utente ha risposto..." crlf)
  (readline)
  (bind ?punto-di-riavvio (stampa-tutte-le-domande ?n-dom-chieste))
  (retract ?c)
  (assert (contatore-domande ?punto-di-riavvio))
  (ritratta-da-domanda ?punto-di-riavvio)
  (retract ?fase)
)


(defrule MODULO-RITRATTAZIONE::riattiva-nodi-senza-figli-uguali
  (declare (salience ?*highest-priority*))
  ?p <- (nodo (nome ?n) (valore ?v) (attivo FALSE) (id-nodo ?id))
  (not (nodo (nome ?n) (valore ?v) (nodo-padre $?x ?id $?y)))
  =>
  (modify ?p (attivo TRUE))
)