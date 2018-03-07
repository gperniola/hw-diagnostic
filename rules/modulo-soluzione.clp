(defmodule MODULO-SOLUZIONE(import MAIN ?ALL)(export ?ALL))

(defrule MODULO-SOLUZIONE::stampa-header
  (declare (salience ?*highest-priority*))
  (fase 5-stampa-soluzioni)
  (not (resetta-soluzione))
  =>
  (printout t "             *********** POSSIBILI SOLUZIONI ***********" crlf crlf)
  (printout t "  CERTEZZA         SOLUZIONE                                        " crlf)
  (printout t " -------------------------------------------------------------------" crlf)
)

(defrule MODULO-SOLUZIONE::stampa-soluzione
  (declare (salience ?*high-priority*))
  (fase 5-stampa-soluzioni)
  (not (resetta-soluzione))
  ?n <- (nodo (nome soluzione) (valore ?attr-soluzione) (certezza ?cer&:(> ?cer 0.10)) (attivo TRUE))
  ?d <- (soluzione (attributo ?attr-soluzione) (titolo ?titolo) (descrizione ?desc) (stampata FALSE))
  (not (and
            (nodo (nome soluzione) (valore ?attr-soluzione2) (certezza ?cer2&:(> ?cer2 ?cer)) (attivo TRUE))
            (soluzione (attributo ?attr-soluzione2) (titolo ?titolo2) (descrizione ?desc2) (stampata FALSE))
  ))
  =>
  ;(printout t "[" (integer (* ?cer 100)) "%] - " ?titolo ": " ?desc crlf)
  (format t "   %2d/10  -  %s %n" (integer (* ?cer 10)) ?titolo )
  (format t "             %s %n%n" ?desc)
  ;(format t " %-60s %2d%%%n %-60s %n %n" ?titolo (integer (* ?cer 100)) ?desc)
  ;(format t " %-60s %2d/10%n %-60s %n %n" ?titolo (integer (/(* ?cer 100) 10)) ?desc)
  (modify ?d (stampata TRUE))
)

(defrule MODULO-SOLUZIONE::fine-stampa-soluzione
  (not (resetta-soluzione))
  (not (and (nodo (nome soluzione) (valore ?attr-soluzione) (certezza ?cer&:(> ?cer 0.10)) (attivo TRUE))
            (soluzione (attributo ?attr-soluzione) (stampata FALSE))))
  =>
  (assert (resetta-soluzione))
)

(defrule MODULO-SOLUZIONE::resetta-soluzione
  (resetta-soluzione)
  ?d <- (soluzione (attributo ?attr-soluzione) (stampata TRUE))
  =>
  (modify ?d (stampata FALSE))
)

(defrule MODULO-SOLUZIONE::fine-resetta-soluzione
  ?r <- (resetta-soluzione)
  ?f <- (fase 5-stampa-soluzioni)
  (not (soluzione (attributo ?attr-soluzione) (stampata TRUE)))
  =>
  (retract ?r)
  (retract ?f)
  ;(assert (ferma-programma-2))
  (ask-stop-program-2)
  (focus MAIN)
)
