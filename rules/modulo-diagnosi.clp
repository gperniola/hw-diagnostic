(defmodule MODULO-DIAGNOSI(import MAIN ?ALL)(export ?ALL))

(defrule MODULO-DIAGNOSI::stampa-header
  (declare (salience ?*highest-priority*))
  (fase 3-stampa-diagnosi)
  (not (resetta-diagnosi))
  =>
  (printout t "              *********** POSSIBILI DIAGNOSI ***********" crlf crlf)
  (printout t "  DIAGNOSI                                                 CERTEZZA" crlf)
  (printout t " -------------------------------------------------------------------" crlf)
)


(defrule MODULO-DIAGNOSI::stampa-diagnosi
  (declare (salience ?*high-priority*))
  (fase 3-stampa-diagnosi)
  (not (resetta-diagnosi))
  ?n <- (nodo (nome diagnosi) (valore ?attr-diagnosi) (certezza ?cer&:(> ?cer 0.10)))
  (not (nodo (nome diagnosi) (valore ?attr-diagnosi) (nodo-padre $?pdr1 ?n $?pdr2)))
  ?d <- (diagnosi (attributo ?attr-diagnosi) (titolo ?titolo) (descrizione ?desc) (stampata FALSE))

  ; (not
  ;   (and
  ;      ?n2 <- (nodo (nome diagnosi) (valore ?attr-diagnosi2) (certezza ?cer2&:(> ?cer2 ?cer)))
  ;      (not (nodo (nome diagnosi) (valore ?attr-diagnosi2) (nodo-padre $?pdrb1 ?n2 $?pdrb2)))
  ;      (diagnosi (attributo ?attr-diagnosi2) (titolo ?titolo2) (descrizione ?desc2) (stampata FALSE))
  ;   )
  ; )
  ;; ORDINAMENTO NON FUNZIONANTE PERCHE ?N2 NON DICHIARABILE IN NOT
  =>
  ;(printout t "[" (integer (* ?cer 100)) "%] - " ?titolo ": " ?desc crlf)
  (format t " %-60s %2d%%%n" ?titolo (integer (* ?cer 100)))
  (modify ?d (stampata TRUE))
)


(defrule MODULO-DIAGNOSI::stampa-diagnosi-2
  (declare (salience ?*high-priority*))
  (fase 3-stampa-diagnosi)
  (not (resetta-diagnosi))
  ?n <- (nodo (nome diagnosi) (valore ?attr-diagnosi) (certezza ?cer&:(> ?cer 0.10)))
  (not (nodo (nome diagnosi) (valore ?attr-diagnosi) (nodo-padre $?pdr1 ?n $?pdr2)))
  ?d <- (diagnosi (attributo ?attr-diagnosi) (titolo ?titolo) (descrizione ?desc) (stampata FALSE))


       ?n2 <- (nodo (nome diagnosi) (valore ?attr-diagnosi2) (certezza ?cer2&:(> ?cer2 ?cer)))
       (not (nodo (nome diagnosi) (valore ?attr-diagnosi2) (nodo-padre $?pdrb1 ?n2 $?pdrb2)))
       ?d2 <- (diagnosi (attributo ?attr-diagnosi2) (titolo ?titolo2) (descrizione ?desc2) (stampata FALSE))
       ;(neq ?n ?n2)

  =>
  ;(printout t "[" (integer (* ?cer 100)) "%] - " ?titolo ": " ?desc crlf)
  (printout t ?attr-diagnosi2 " - " ?cer2 " > " ?attr-diagnosi " - " ?cer crlf )
  (modify ?d2 (stampata TRUE))
)

(defrule MODULO-DIAGNOSI::fine-stampa-diagnosi
  (not (resetta-diagnosi))
  (not (and (nodo (nome diagnosi) (valore ?attr-diagnosi) (certezza ?cer&:(> ?cer 0.10)))
            (not (nodo (nome diagnosi) (valore ?attr-diagnosi) (nodo-padre $?pdr1 ?n $?pdr2)))
            (diagnosi (attributo ?attr-diagnosi) (stampata FALSE))))
  =>
  (assert (resetta-diagnosi))
)

(defrule MODULO-DIAGNOSI::resetta-diagnosi
  (resetta-diagnosi)
  ?d <- (diagnosi (attributo ?attr-diagnosi) (stampata TRUE))
  =>
  (modify ?d (stampata FALSE))
)

(defrule MODULO-DIAGNOSI::fine-resetta-diagnosi
  ?r <- (resetta-diagnosi)
  ?f <- (fase 3-stampa-diagnosi)
  (not (diagnosi (attributo ?attr-diagnosi) (stampata TRUE)))
  =>
  (retract ?r)
  (retract ?f)
  ;(assert (ferma-programma))
  (ask-stop-program)
  (focus MAIN)
)
