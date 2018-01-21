(defmodule MODULO-DIAGNOSI(import MAIN ?ALL)(export ?ALL))

(defrule MODULO-DIAGNOSI::stampa-diagnosi
  (not (resetta-diagnosi))
  (nodo (nome diagnosi) (valore ?attr-diagnosi) (certezza ?cer&:(> ?cer 0.10)) (stato attivo))
  ?d <- (diagnosi (attributo ?attr-diagnosi) (titolo ?titolo) (descrizione ?desc) (stampata FALSE))
  =>
  (printout t "[" (integer (* ?cer 100)) "%] - " ?titolo ": " ?desc crlf)
  (modify ?d (stampata TRUE))
)

(defrule MODULO-DIAGNOSI::fine-stampa-diagnosi
  (not (resetta-diagnosi))
  (not (and (nodo (nome diagnosi) (valore ?attr-diagnosi) (certezza ?cer&:(> ?cer 0.10)) (stato attivo))
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
  (not (diagnosi (attributo ?attr-diagnosi) (stampata TRUE)))
  =>
  (retract ?r)
  (assert (ferma-programma))
  (focus MAIN)
)