(defmodule MODULO-SOLUZIONE(import MAIN ?ALL)(export ?ALL))

(defrule MODULO-SOLUZIONE::stampa-header
  (declare (salience ?*highest-priority*))
  (fase 5-stampa-soluzioni)
  (not (resetta-soluzione))
  =>
  (printout t "             *********** POSSIBILI SOLUZIONI ***********" crlf crlf)
  (printout t "  SOLUZIONE                                                CERTEZZA" crlf)
  (printout t " -------------------------------------------------------------------" crlf)
)

(defrule MODULO-SOLUZIONE::stampa-soluzione
  (declare (salience ?*high-priority*))
  (fase 5-stampa-soluzioni)
  (not (resetta-soluzione))
  ?n <- (nodo (nome soluzione) (valore ?attr-soluzione) (certezza ?cer&:(> ?cer 0.10)))
  ?d <- (soluzione (attributo ?attr-soluzione) (titolo ?titolo) (descrizione ?desc) (stampata FALSE))
  =>
  ;(printout t "[" (integer (* ?cer 100)) "%] - " ?titolo ": " ?desc crlf)

  (format t " %-60s %2d%%%n %-60s %n %n" ?titolo (integer (* ?cer 100)) ?desc)
  (modify ?d (stampata TRUE))
)

(defrule MODULO-SOLUZIONE::fine-stampa-soluzione
  (not (resetta-soluzione))
  (not (and (nodo (nome soluzione) (valore ?attr-soluzione) (certezza ?cer&:(> ?cer 0.10)))
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
