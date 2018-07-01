(defmodule MODULO-STAMPA(import MAIN ?ALL)(export ?ALL))


;** Questa funzione conta e restituisce il numero di occorrenze di ?value all'interno di ?list **
  (deffunction count$ (?list ?value)
     (bind ?count 0)
     (foreach ?l ?list
        (if (eq ?l ?value)
           then
           (bind ?count (+ ?count 1))))
     (return ?count))


;** Questa funzione elimina tutti i duplicati dell'elemento ?value presenti in ?list e restituisce la lista **
   (deffunction delete-duplicates$ (?list ?value)
      (bind ?count (count$ ?list ?value))
      (if (<= ?count 1)
         then
         (return ?list))
      (loop-for-count (- ?count 1)
         (bind ?pos (member$ ?value ?list))
         (bind ?list (delete$ ?list ?pos ?pos)))
      (return ?list))


;** Questa funzione elimina tutti i valori duplicati presenti in ?list **
    (deffunction delete-duplicates-val$ (?list)
       (foreach ?v ?list
         (bind ?list (delete-duplicates$ ?list ?v))
       )
       (bind ?list (sort > ?list))
       (bind ?list (delete$ ?list 1 1))
      (return ?list)
    )


;** Questa funzione indica come ordinare i valori ?f1 e ?f2 utilizzando lo slot certezza **
    (deffunction ordinamento-per-certezza (?f1 ?f2)
      (< (fact-slot-value ?f1 certezza) (fact-slot-value ?f2 certezza)))



;** Questa funzione si occupa di trovare i nodi padre di un nodo ?n e li restituisce in una lista.         **
;** Viene utilizzata per recuperare i nodi che compongono le motivazioni che hanno portato ad una diagnosi **
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


;** Questa funzione si occupa di stampare le motivazioni per una diagnosi, stampando le descrizioni dei **
;** nodi passati come id in ?lista-id                                                                   **
    (deffunction stampa-spiega(?titolo-diagnosi ?lista-id)
      (printout t crlf "             MOTIVAZIONI: " crlf)
      (progn$ (?id ?lista-id)
        (bind ?nodo-da-spiegare (find-fact ((?d nodo))(eq ?d:id-nodo ?id)))
        (bind ?nodo (nth$ 1 ?nodo-da-spiegare))
        (bind ?descrizione (fact-slot-value ?nodo descrizione))
        (printout t "              -  " ?descrizione crlf)
      )
    )


;** Questa funzione stampa tutte le diagnosi con un fattore di certezza >= 0.70 **
   (deffunction stampa-tutte-le-diagnosi ()
     (printout t crlf "              *********** DIAGNOSI E SOLUZIONI TROVATE ***********" crlf crlf)
     (printout t "  CERTEZZA         DIAGNOSI                                         " crlf)
     (printout t " -------------------------------------------------------------------" crlf)

     (bind ?diagnosi (find-all-facts ((?n nodo))(and (eq ?n:nome diagnosi)(>= ?n:certezza 0.70)(eq ?n:attivo TRUE))))
     (bind ?diagnosi (sort ordinamento-per-certezza ?diagnosi))
     (progn$ (?x ?diagnosi)
             (bind ?data-diagnosi (find-all-facts ((?d diagnosi))(eq (fact-slot-value ?x valore) ?d:attributo)))
             (progn$ (?y ?data-diagnosi)
                 (format t "   %2d/10  -  %-60s %n" (integer (* (fact-slot-value ?x certezza) 10)) (fact-slot-value ?y titolo) )
                 (format t "             %-60s %n" (fact-slot-value ?y descrizione))

                 (bind ?lista (spiega (fact-slot-value ?x id-nodo)))
                 (bind ?lista (delete-duplicates-val$ ?lista))
                 (stampa-spiega (fact-slot-value ?y titolo) ?lista)
                 (printout t crlf crlf)
             )
     )
   )


;** Questa funzione stampa tutte le soluzioni con un fattore di certezza >= 0.70 **
   (deffunction stampa-tutte-le-soluzioni ()
     (printout t crlf crlf)
     (printout t "  CERTEZZA         SOLUZIONE                                        " crlf)
     (printout t " -------------------------------------------------------------------" crlf)

     (bind ?soluzione (find-all-facts ((?n nodo))(and (eq ?n:nome soluzione)(>= ?n:certezza 0.70)(eq ?n:attivo TRUE))))
     (bind ?soluzione (sort ordinamento-per-certezza ?soluzione))
     (progn$ (?x ?soluzione)
             (bind ?data-soluzione (find-all-facts ((?d soluzione))(eq (fact-slot-value ?x valore) ?d:attributo)))
             (progn$ (?y ?data-soluzione)
                 (format t "   %2d/10  -  %-60s %n" (integer (* (fact-slot-value ?x certezza) 10)) (fact-slot-value ?y titolo) )
                 (format t "             %-60s %n%n" (fact-slot-value ?y descrizione))
             )
     )
   )


;** Questa regola viene attivata quando è presente almeno una diagnosi con fattore di certezza >= 0.70, avviando la stampa **
   (defrule MODULO-STAMPA::diagnosi-trovata
    ?f <- (in-esecuzione)
    (nodo (nome diagnosi)(certezza ?cer&:(>= ?cer 0.70)) (attivo TRUE))
    =>
    (clear-window)
    (stampa-tutte-le-diagnosi)
    (stampa-tutte-le-soluzioni)
   )


;** Questa regola viene attivata quando non sono presenti diagnosi con fattore di certezza >= 0.70, stampa un messaggio per l'utente **
   (defrule MODULO-STAMPA::diagnosi-non-trovata
    (not(nodo (nome diagnosi)(certezza ?cer&:(>= ?cer 0.70)) (attivo TRUE)))
    ?f <- (in-esecuzione)
    =>
    (clear-window)
    (printout t crlf "              *********** DIAGNOSI E SOLUZIONI TROVATE ***********" crlf crlf)
    (printout t "Purtroppo il sistema non e' stato in grado di trovare alcuna diagnosi valida." crlf)
    (printout t "E' possibile che il problema dipenda da un problema software (non diagnosticati da questo programma)." crlf)
   )
