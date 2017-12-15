


(defmodule TEMPLATES (export ?ALL))

(deftemplate TEMPLATES::diagnosi
  (slot attributo   (type SYMBOL))
  (slot titolo      (type STRING))
  (slot descrizione (type STRING))
)

(deftemplate TEMPLATES::nodo
  (slot nome    (type SYMBOL))
  (multislot valore  (type SYMBOL))
  (slot tipo (type SYMBOL))
  (slot descrizione (type STRING))
  (multislot nodo-padre (type FACT-ADDRESS))
)


(deftemplate TEMPLATES::domanda
  (slot attributo     (type SYMBOL) (default ?NONE))
  (slot testo-domanda (type STRING) (default ?NONE))
  (multislot risposte-valide (type SYMBOL) (default ?NONE))
  (multislot descrizione-risposte (type STRING) (default ?NONE))
  (slot risposta-selezionata (type INTEGER))
  (slot gia-chiesta   (default  FALSE))
  (slot num-domanda (type INTEGER))
  (slot stampata (default FALSE))
)



(defmodule MAIN (import TEMPLATES ?ALL)(export ?ALL))

; (defglobal ?*highest-priority* = 1000)
; (defglobal ?*high-priority* = 100)
; (defglobal ?*low-priority* = -100)
; (defglobal ?*lowest-priority* = -1000)

;;****************
;;* DEFFUNCTIONS *
;;****************


(deffunction MAIN::stampa-header()
  (clear-window)
  (printout t crlf crlf)
  (printout t   "***                                                 ***" crlf
                "**  SISTEMA DIAGNOSTICO PER DISPOSITIVI ELETTRONICI  **" crlf
                "*                                                     *" crlf
                "*     Rispondere alle domande inserendo il numero     *" crlf
                "**       corrispondente alla risposta corretta.      **" crlf
                "***                                                 ***" crlf crlf)
)




;;******************
;;*    TEMPLATES   *
;;******************



;;**********************
;;*    INITIAL FACTS   *
;;**********************
(deffacts MAIN::fatti-iniziali

  (contatore-domande 0)
)




;;******************
;;* CONTROL RULES  *
;;******************

(defrule MAIN::inizializzazione
  ;(declare (salience ?*highest-priority*))
  =>
  (clear-window)
  (set-fact-duplication TRUE)
  (focus DOMANDE-GENERICHE MAIN)
)
;   (printout t crlf crlf)
;   (printout t   "***                                                 ***" crlf
;                 "**  SISTEMA DIAGNOSTICO PER DISPOSITIVI ELETTRONICI  **" crlf
;                 "*                                                     *" crlf
;                 "*     Rispondere alle domande inserendo il numero     *" crlf
;                 "**       corrispondente alla risposta corretta.      **" crlf
;                 "***                                                 ***" crlf crlf))


; (defrule MAIN::diagnosi-trovata
;   ;(declare (salience ?*highest-priority*))
;   (nodo (nome diagnosi) (valore ?attr-diagnosi))
;   (diagnosi (attributo ?attr-diagnosi) (titolo ?titolo) (descrizione ?desc))
;   =>
;   (printout t crlf "***** DIAGNOSI *****" crlf " - " ?titolo ":" crlf ?desc crlf crlf)
;   (assert(ferma-programma))
; )




(defmodule ELENCO-DIAGNOSI (import TEMPLATES ?ALL)(export ?ALL))




  (deffacts ELENCO-DIAGNOSI::elenco-diagnosi

    (diagnosi (attributo cavi-display-non-connessi)
              (titolo "Cavi del display non connessi correttamente")
              (descrizione "A volte un cavo video.")
    )


  )

;;********************
;;* QUESTIONS FACTS  *
;;********************
(defmodule ELENCO-DOMANDE(import MAIN deftemplate domanda))

  (deffacts ELENCO-DOMANDE::domande

    (domanda  (attributo tipo-dispositivo)
              (testo-domanda "A quale tipologia appartiene il dispositivo?")
              (risposte-valide pc-desktop pc-portatile)
              (descrizione-risposte "PC Desktop" "PC Portatile/Netbook")
    )

  )


  ;******************* MODULO DOMANDE GENERICHE **********************************

  (defmodule DOMANDE-GENERICHE (import MAIN deftemplate nodo) )

      (defrule DOMANDE-GENERICHE::init
        ;(declare (salience ?*highest-priority*))
        =>
        (set-strategy random)
        (printout t "DEBUG >> DOMANDE-GENERICHE >> strategy set to random." crlf crlf)
      )

      (defrule DOMANDE-GENERICHE::end
        ;(declare (salience ?*lowest-priority*))
        =>
        (set-strategy depth)
        (printout t "DEBUG >> DOMANDE-GENERICHE >> strategy set to depth." crlf crlf)
        ;(focus MAIN)
      )

      (defrule DOMANDE-GENERICHE::chiedi-tipo-dispositivo
        =>
        (assert (nodo (nome chiedi) (valore tipo-dispositivo)))
      )



  ;*******************************************************************************
