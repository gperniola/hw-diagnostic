(defmodule MODULO-STAMPA-DIAGNOSI-SOLUZIONI(import MAIN ?ALL)(export ?ALL))



(deffunction ordinamento-per-certezza (?f1 ?f2)
   (< (fact-slot-value ?f1 certezza) (fact-slot-value ?f2 certezza)))



(defrule MODULO-STAMPA-DIAGNOSI-SOLUZIONI::stampa-tutte-le-diagnosi
  (declare (salience 500))
  =>
  (clear-window)
  (printout t crlf "              *********** DIAGNOSI E SOLUZIONI TROVATE ***********" crlf crlf)
  (printout t "  CERTEZZA         DIAGNOSI                                         " crlf)
  (printout t " -------------------------------------------------------------------" crlf)

  (bind ?diagnosi (find-all-facts ((?n nodo))(and (eq ?n:nome diagnosi)(>= ?n:certezza 0.70)(eq ?n:attivo TRUE))))
  (bind ?diagnosi (sort ordinamento-per-certezza ?diagnosi))
  (progn$ (?x ?diagnosi)
          (bind ?data-diagnosi (find-all-facts ((?d diagnosi))(eq (fact-slot-value ?x valore) ?d:attributo)))
          (progn$ (?y ?data-diagnosi)
              (format t "   %2d/10  -  %-60s %n" (integer (* (fact-slot-value ?x certezza) 10)) (fact-slot-value ?y titolo) )
              (format t "             %-60s %n%n" (fact-slot-value ?y descrizione))
          )
  )
)


(defrule MODULO-STAMPA-DIAGNOSI-SOLUZIONI::stampa-tutte-le-soluzioni
  (declare (salience 490))
  =>
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

(defrule MODULO-STAMPA-DIAGNOSI-SOLUZIONI::chiedi-soddisfazione-utente
  (declare (salience 480))
  =>
  (printout t crlf crlf "L'utente e' soddisfatto del risultato ottenuto?")
  (printout t crlf "1. Si.")
  (printout t crlf "2. No." crlf crlf)
  (printout t "Inserire risposta: ")
  (bind ?answer (read))
  (if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))

  (while (not (and(>= ?answer 1) (<= ?answer 2)))
      (printout t "Valore non riconosciuto, riprovare: ")
      (bind ?answer (read))
      (if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))
  )
  (printout t crlf "Entrypoint per ritrattazione" crlf crlf)
)
