
(defglobal ?*highest-priority* = 1000)
(defglobal ?*high-priority* = 100)
(defglobal ?*low-priority* = -100)
(defglobal ?*lowest-priority* = -1000)


;;****************
;;* DEFFUNCTIONS *
;;****************

(deffunction ask-question-revision(?n-domande-chieste)
  (printout t "Inserire il numero di domanda da modificare oppure" crlf "premere 0 per tornare alla normale esecuzione del programma: ")
  (bind ?answer (read))
  (if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))
  (while (and (> ?answer ?n-domande-chieste) (< ?answer 0)) do
    (printout t crlf "Valore inserito non valido, riprovare: ")
    (bind ?answer (read))
  )
?answer)

(deffunction ask-question-direct (?j ?question $?allowed-values)
  (printout t "***** REVISIONE DOMANDA N." ?j " *****" crlf ?question crlf)
  (loop-for-count (?cnt1 1 (length ?allowed-values)) do
      (printout t ?cnt1 ". " (nth$ ?cnt1 ?allowed-values) crlf)
  )
  (printout t "Inserire risposta: ")
  (bind ?answer (read))
  (if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))
  (while  (not (member (nth$ ?answer ?allowed-values) ?allowed-values)) do
      (printout t crlf "Valore inserito non valido, riprovare: ")
      (bind ?answer (read))
      (if (lexemep ?answer) then (bind ?answer (lowcase ?answer))))
   ?answer)


(deffunction ask-question (?j ?question $?allowed-values)
  (printout t "***** DOMANDA N." ?j " *****" crlf ?question crlf)
  (loop-for-count (?cnt1 1 (length ?allowed-values)) do
      (printout t ?cnt1 ". " (nth$ ?cnt1 ?allowed-values) crlf)
  )
  (printout t "0. Revisiona domande precedenti." crlf crlf)
  (printout t "Inserire risposta: ")
  (bind ?answer (read))
  (if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))
  (while (and (not (member (nth$ ?answer ?allowed-values) ?allowed-values)) (not (= ?answer 0))) do
      (printout t crlf "Valore inserito non valido, riprovare: ")
      (bind ?answer (read))
      (if (lexemep ?answer) then (bind ?answer (lowcase ?answer))))
  (printout t crlf crlf)
   ?answer)


(deffunction ask-stop-program ()
  (printout t crlf crlf "Continuare l'esecuzione del programma?" crlf "1. Si" crlf "2. No" crlf)
  (bind ?answer (read))
  (while (not (member ?answer (create$ 1 2)))
      (printout t "Continuare l'esecuzione del programma?" crlf "1. Si" crlf "2. No" crlf)
      (bind ?answer (read))
  )
  (if (eq ?answer 2) then
      (halt)
  )
)

(deffunction stampa-header-revisione()
  (clear-window)
  (printout t crlf "******************** REVISIONE DOMANDE ********************" crlf crlf)
)

(deffunction stampa-footer-revisione()
  (printout t crlf "***********************************************************" crlf crlf crlf)
)

(defmodule MAIN (export ?ALL))

;;******************
;;*    TEMPLATES   *
;;******************

  (deftemplate nodo
    (slot nome    (type SYMBOL))
    (multislot valore  (type SYMBOL))
    (slot tipo (type SYMBOL))
    (slot descrizione (type STRING))
    (multislot nodo-padre (type FACT-ADDRESS))
  )

  (deftemplate diagnosi
    (slot nome        (type SYMBOL))
    (slot descrizione (type STRING))
  )

  (deftemplate domanda
    (slot attributo     (type SYMBOL) (default ?NONE))
    (slot testo-domanda (type STRING) (default ?NONE))
    (multislot risposte-valide (type SYMBOL) (default ?NONE))
    (multislot descrizione-risposte (type STRING) (default ?NONE))
    (slot risposta-selezionata (type INTEGER))
    (slot gia-chiesta   (default  FALSE))
    (slot num-domanda (type INTEGER))
    (slot stampata (default FALSE))
  )

;;**********************
;;*    INITIAL FACTS   *
;;**********************
(deffacts fatti-iniziali

  (contatore-domande 0)
)




;;******************
;;* CONTROL RULES  *
;;******************

(defrule inizializzazione
  (declare (salience ?*highest-priority*))
  =>
  (clear-window)
  (focus DOMANDE-GENERICHE)
  (printout t crlf crlf)
  (printout t   "***                                                 ***" crlf
                "**  SISTEMA DIAGNOSTICO PER DISPOSITIVI ELETTRONICI  **" crlf
                "*                                                     *" crlf
                "*     Rispondere alle domande inserendo il numero     *" crlf
                "**       corrispondente alla risposta corretta.      **" crlf
                "***                                                 ***" crlf crlf))


(defrule diagnosi-trovata
  (declare (salience ?*highest-priority*))
  (diagnosi (nome ?nome) (descrizione ?desc))
  =>
  (printout t crlf ">>>> Diagnosi del guasto: " ?nome " - " ?desc crlf crlf)
  (halt))


(defrule diagnosi-parziale-trovata
  (declare (salience ?*highest-priority*))
  (nodo (nome diagnosi-parziale) (valore ?val) (descrizione ?desc))
  =>
  (printout t crlf "DIAGNOSI PARZIALE TROVATA: " ?desc)
  (assert(ferma-programma))
)


(defrule ferma-esecuzione
  (declare (salience ?*low-priority*))
  (ferma-programma)
  =>
  (ask-stop-program)
)


; STAMPA ELENCO E REVISIONE DOMANDE
;****************************************************************************

(defrule INIT-STAMPA-ELENCO-header
  (declare (salience ?*highest-priority*))
  ?p <- (init-revisiona-domande)
  =>
  (stampa-header-revisione)
  (retract ?p)
  (assert (revisiona-domande))
)

(defrule LOOP-STAMPA-ELENCO-domande
  (declare (salience ?*highest-priority*))
  (revisiona-domande)
  ?d <- (domanda (testo-domanda ?testo) (attributo ?attr) (num-domanda ?n) (gia-chiesta TRUE) (stampata FALSE) (descrizione-risposte $?descr) (risposta-selezionata ?r-selezionata))
  (not (domanda (num-domanda ?m&:(< ?m ?n))  (gia-chiesta TRUE)(stampata FALSE)))
  =>
  (modify ?d (stampata TRUE))
  (printout t "Domanda " ?n ": " ?testo crlf "Risposta: " (nth ?r-selezionata ?descr) crlf crlf)
)

(defrule END-STAMPA-ELENCO
  (declare (salience ?*highest-priority*))
  ?r <-(revisiona-domande)
  (not (domanda (gia-chiesta TRUE) (stampata FALSE)))
  (contatore-domande ?n)
  =>
  (printout t crlf crlf)
  (bind ?risposta (ask-question-revision ?n))
  (stampa-footer-revisione)
  (retract ?r)
  (assert (annulla-stampa-domande))
  (if (<> ?risposta 0) then
    (assert (revisiona-da ?risposta))
  )
)

(defrule LOOP-STAMPA-ELENCO-reset
  (declare (salience ?*highest-priority*))
  (annulla-stampa-domande)
  ?d <- (domanda (stampata TRUE))
  =>
  (modify ?d (stampata FALSE))
)

(defrule END-STAMPA-ELENCO-reset
  (declare (salience ?*highest-priority*))
  ?a <- (annulla-stampa-domande)
  (not (domanda (stampata TRUE)))
  =>
  (retract ?a)
)


; REVISIONA DOMANDA
;****************************************************************************


(defrule revisiona-da
  (declare (salience ?*highest-priority*))
  (not(annulla-stampa-domande))
  ?r <- (revisiona-da ?n)
  ?d <- (domanda (attributo ?attr)(testo-domanda ?domanda) (risposte-valide $?risposte) (descrizione-risposte $?descr) (num-domanda ?n) (gia-chiesta TRUE))
  ?nodo-partenza <- (nodo (nome chiedi) (valore ?attr) (nodo-padre $?padri))
  =>
  (assert (elimina-nodi-da ?nodo-partenza))
  (bind ?risposta (ask-question-direct ?n ?domanda ?descr))
  (modify ?d (risposta-selezionata ?risposta))
  (retract ?r)
  (assert (fine-revisione))
)

(defrule LOOP-elimina-nodi-da
  (declare (salience ?*highest-priority*))
  ?p1 <- (elimina-nodi-da ?n)
  ?p2 <- (nodo (nodo-padre $?x ?n $?y))
  =>
  (assert (elimina-nodi-da ?p2))
  (retract ?p2)
)

(defrule END-elimina-nodi-da
  (declare (salience ?*highest-priority*))
  ?p1 <- (elimina-nodi-da ?n)
  (not (nodo (nodo-padre $?x ?n $?y)))
  =>
  (retract ?p1)
)

(defrule END-revisiona-domande
  (declare (salience ?*high-priority*))
  ?r <- (fine-revisione)
  (not (elimina-nodi da ?e))
  =>
  (retract ?r)
  (assert (init-revisiona-domande))
)


; CHIEDI DOMANDA
;****************************************************************************


(defrule chiedi-domanda
  (declare (salience ?*low-priority*))
  ?ask <- (nodo (nome chiedi)(valore ?attr)(nodo-padre $?p))
  ?f <- (domanda (attributo ?attr) (testo-domanda ?domanda) (risposte-valide $?risposte) (descrizione-risposte $?descrizioni) (gia-chiesta FALSE))
  (not (nodo (nome ?attr)))
  ?cont-dom <- (contatore-domande ?i)
  =>
  (bind ?j (+ ?i 1))
  (bind ?risposta (ask-question ?j ?domanda ?descrizioni))
  (if (= ?risposta 0) then
    (retract ?ask)
    (assert (nodo (nome chiedi)(valore ?attr)(nodo-padre ?p))) ;;NECESSARIO PER RIPROPORRE LA STESSA DOMANDA NEL CASO DI ANNULLAMENTO REVISIONE
    (assert (init-revisiona-domande))
  else
    ;;(assert (nodo (nome ?attr) (valore (nth$ ?risposta ?risposte)) (descrizione (nth$ ?risposta ?descrizioni)) (tipo info-utente) (nodo-padre ?ask)))
    (modify ?f (gia-chiesta TRUE)(num-domanda ?j)(risposta-selezionata ?risposta))
    ;;(retract ?ask)
    (retract ?cont-dom)
    (assert (contatore-domande ?j))
  )
)

(defrule usa-risposta-utente
  ?ask <- (nodo (nome chiedi)(valore ?attr)(nodo-padre $?p))
  ?f <- (domanda (attributo ?attr) (testo-domanda ?domanda) (risposte-valide $?risposte) (descrizione-risposte $?descrizioni) (gia-chiesta TRUE) (risposta-selezionata ?risp))
  (not (nodo (nome ?attr)))
  =>
  (assert (nodo (nome ?attr) (valore (nth$ ?risp ?risposte)) (descrizione (nth$ ?risp ?descrizioni)) (tipo info-utente) (nodo-padre ?ask)))
)

;;********************
;;*    ASK RULES     *
;;********************

;******************* MODULO DOMANDE GENERICHE **********************************

(defmodule DOMANDE-GENERICHE (import MAIN ?ALL)(export ?ALL))

    (defrule DOMANDE-GENERICHE::init
      (declare (salience ?*highest-priority*))
      =>
      (set-strategy random)
      (printout t "DEBUG >> DOMANDE-GENERICHE >> strategy set to random." crlf crlf)
    )

    (defrule DOMANDE-GENERICHE::end
      (declare (salience ?*lowest-priority*))
      =>
      (set-strategy depth)
      (printout t "DEBUG >> DOMANDE-GENERICHE >> strategy set to depth." crlf crlf)
      (focus MAIN)
    )




    (defrule DOMANDE-GENERICHE::chiedi-tipo-dispositivo
      =>
      (assert (nodo (nome chiedi) (valore tipo-dispositivo)))
    )

    (defrule DOMANDE-GENERICHE::chiedi-accensione
      =>
      (assert (nodo (nome chiedi) (valore stato-accensione)))
    )

    (defrule DOMANDE-GENERICHE::chiedi-problema-principale
      =>
      (assert (nodo (nome chiedi) (valore problema-principale)))
    )

    (defrule DOMANDE-GENERICHE::chiedi-anni-dispositivo
      =>
      (assert (nodo (nome chiedi) (valore anni-dispositivo)))
    )

    (defrule DOMANDE-GENERICHE::chiedi-installazione-nuovo-hw
      =>
      (assert (nodo (nome chiedi) (valore installazione-nuovo-hw))
    )

;*******************************************************************************

(defrule chiedi-garanzia
  ?p1 <- (nodo (nome anni-dispositivo) (valore ?val&meno-2-anni|meno-5-anni|sconosciuto))
  =>
  (assert (nodo (nome chiedi) (valore anni-dispositivo) (nodo-padre ?p1)))
)

(defrule chiedi-problema-boot-o-SO
  ?p1 <- (nodo (nome stato-accensione) (valore ok))
  ?p2 <- (nodo (nome problema-principale) (valore accensione-SO))
  =>
  (assert (nodo (nome chiedi) (valore problema-boot-o-SO) (nodo-padre ?p1 ?p2)))
)

(defrule chiedi-riavvio-forzato
  ?p1 <- (nodo (nome stato-accensione) (valore ok))
  =>
  (assert (nodo (nome chiedi) (valore riavvio-forzato) (nodo-padre ?p1)))
)

(defrule chiedi-bluescreen ;;TODO
  ?p1 <- (nodo (nome problema-boot-o-SO) (valore SO))
  ?p2 <- (nodo (nome sistema-operativo) (valore windows))
  =>
  (assert (nodo (nome chiedi) (valore bluescreen) (nodo-padre ?p1 ?p2)))
)



(defrule chiedi-segnali-bios
  ?p1 <- (nodo (nome problema-boot-o-SO) (valore boot))
  =>
  (assert (nodo (nome chiedi) (valore segnali-bios) (nodo-padre ?p1)))
)

(defrule chiedi-display-rotto
  ?p1 <- (nodo (nome problema-principale) (valore video))
  =>
  (assert (nodo (nome chiedi) (valore display-rotto) (nodo-padre ?p1)))
)

(defrule chiedi-disturbo-video
  ?p1 <- (nodo (nome display-rotto) (valore no))
  ?p2 <- (nodo (nome problema-principale) (valore video))
  ;?p2 <- (nodo (nome stato-video) (valore fallito))
  =>
  (assert (nodo (nome chiedi) (valore disturbo-video) (nodo-padre ?p1 ?p2 )))
)



;;********************
;;*     DIAGNOSI     *
;;********************

(defrule diagnosi-parziale-connettori-video
  ?p1 <- (nodo (nome disturbo-video) (valore si))
  =>
  (assert (nodo (nome diagnosi-parziale) (valore problema-connettori-video) (descrizione "Problema ai connettori video") (nodo-padre ?p1)))
)

(defrule diagnosi-parziale-problema-pixels
  ?p1 <- (nodo (nome disturbo-video) (valore si))
  =>
  (assert (nodo (nome diagnosi-parziale) (valore problema-pixels) (descrizione "Possibile guasto ai pixels del display") (nodo-padre ?p1)))
)

(defrule diagnosi-parziale-sostituzione-display
  (or
    ?p1 <- (nodo (nome display-rotto) (valore si))
    ?p1 <- (nodo (nome diagnosi-parziale) (valore problema-pixels))
  )
  =>
  (assert (nodo (nome diagnosi-parziale) (valore sostituzione-display) (descrizione "Necessaria sostituzione del display") (nodo-padre ?p1)))
)

(defrule diagnosi-parziale-stop-error
  ?p1 <- (nodo (nome bluescreen) (valore si))
  =>
  (assert (nodo (nome diagnosi-parziale) (valore stop-error) (nodo-padre ?p1)))
)

(defrule diagnosi-parziale-conflitto-hw
  ?p1 <- (nodo (nome diagnosi-parziale) (valore stop-error))
  ?p2 <- (nodo (nome installazione-nuovo-hw) (valore si))
  =>
  (assert (nodo (nome diagnosi-parziale) (valore conflitto-hw) (descrizione "Conflitto tra diversi componenti hardware") (nodo-padre ?p1 ?p2)))
)

(defrule diagnosi-parziale-alimentazione
  (or
    (and
      ?p1 <- (nodo (nome bluescreen) (valore no))
      ?p2 <- (nodo (nome riavvio-forzato) (valore si))
    )
    (and
      ?p1 <- (nodo (nome segnali-bios) (valore no))
      ?p2 <- (nodo (nome riavvio-forzato) (valore si))
    )
  )
  =>
  (assert (nodo(nome diagnosi-parziale) (valore problema-alimentazione) (descrizione "Problema all'alimentazione del dispositivo") (nodo-padre ?p1 ?p2)))
)

(defrule diagnosi-parziale-alimentazione-2
  ?p1 <- (nodo (nome stato-accensione) (valore fallito))
  =>
  (assert (nodo(nome diagnosi-parziale) (valore problema-alimentazione) (descrizione "Problema all'alimentazione del dispositivo") (nodo-padre ?p1)))
)

(defrule diagnosi-parziale-scheda-madre
  (or
      (and
        ?p1 <- (nodo (nome bluescreen) (valore no))
        ?p2 <- (nodo (nome riavvio-forzato) (valore si))
      )
      (and
        ?p1 <- (nodo (nome segnali-bios) (valore no))
        ?p2 <- (nodo (nome riavvio-forzato) (valore si))
      )
  )
  =>
  (assert (nodo(nome diagnosi-parziale) (valore problema-scheda-madre) (descrizione "Possibile guasto della scheda madre") (nodo-padre ?p1 ?p2)))
)

(defrule diagnosi-parziale-scheda-madre-2
  ?p1 <- (nodo (nome stato-accensione) (valore fallito))
  =>
  (assert (nodo(nome diagnosi-parziale) (valore problema-scheda-madre) (descrizione "Possibile guasto della scheda madre") (nodo-padre ?p1)))
)

(defrule diagnosi-parziale-bios
  ?p1 <- (nodo (nome segnali-bios) (valore si))
  =>
  (assert (nodo (nome diagnosi-parziale) (valore problema-bios) (descrizione "Problema impostazioni BIOS") (nodo-padre ?p1)))
)

(defrule diagnosi-parziale-ram
  (or
    ?p1 <- (nodo (nome segnali-bios) (valore si))
    ?p1 <- (nodo (nome diagnosi-parziale) (valore stop-error))
  )
  =>
  (assert (nodo (nome diagnosi-parziale) (valore problema-ram) (descrizione "Possibile guasto memoria RAM") (nodo-padre ?p1)))
)

(defrule diagnosi-parziale-hard-disk
  (or
    ?p1 <- (nodo (nome segnali-bios) (valore si))
    ?p1 <- (nodo (nome diagnosi-parziale) (valore stop-error))
  )
  =>
  (assert (nodo (nome diagnosi-parziale) (valore problema-hard-disk) (descrizione "Possibile guasto del disco fisso") (nodo-padre ?p1)))
)




(defrule deduci-SO-windows
  ?p1 <- (nodo (nome tipo-dispositivo) (valore ?dispositivo&pc-fisso|pc-portatile))
  =>
  (assert (nodo (nome sistema-operativo) (valore windows) (tipo inferenza) (nodo-padre ?p1)))
)

(defrule deduci-SO-android
  ?p1 <- (nodo (nome tipo-dispositivo) (valore ?dispositivo&tablet|smarthpone))
  =>
  (assert (nodo (nome SO) (valore android) (tipo inferenza) (nodo-padre ?p1)))
)




;;********************
;;* QUESTIONS FACTS  *
;;********************
(defmodule ELENCO-DOMANDE(import MAIN ?ALL)(export ?ALL))



  (deffacts ELENCO-DOMANDE::domande


    (domanda  (attributo tipo-dispositivo)
              (testo-domanda "A quale tipologia appartiene il dispositivo?")
              (risposte-valide pc-fisso pc-portatile)
              (descrizione-risposte "PC Desktop" "PC Portatile/Netbook")
    )

    (domanda  (attributo stato-accensione)
              (testo-domanda "E' possibile avviare il dispositivo premendo il pulsante di accensione?")
              (risposte-valide ok fallito)
              (descrizione-risposte "Si" "No, il dispositivo non si accende")
    )

    (domanda  (attributo problema-principale)
              (testo-domanda "Qual'e' la categoria che sembra più appropriata per il problema da analizzare?")
              (risposte-valide accensione-SO video surriscaldamento altro)
              (descrizione-risposte "Problema relativo all'accensione del dispositivo, caricamento e funzionamento del sistema operativo"
                                    "Disturbo del segnale video, problema del display"
                                    "Surriscaldamento eccessivo del dispositivo"
                                    "Altro")
    )

    (domanda  (attributo anni-dispositivo)
              (testo-domanda "Quanti anni ha il dispositivo?")
              (risposte-valide meno-2-anni meno-5-anni meno-10-anni piu-10-anni sconosciuto)
              (descrizione-risposte "Meno di due anni" "Meno di cinque anni" "Meno di dieci anni" "Piu' di dieci anni" "Non so")
    )

    (domanda  (attributo garanzia)
              (testo-domanda "Il dispositivo è ancora in garanzia?")
              (risposte-valide si no sconosciuto)
              (descrizione-risposte "Si" "No" "Non so")
    )





    (domanda  (attributo problema-boot-o-SO)
              (testo-domanda "Il problema si verifica nella fase di boot o dopo aver caricato il sistema operativo?")
              (risposte-valide boot SO nessuno)
              (descrizione-risposte "Nella fase di boot" "Dopo il caricamento del sistema operativo" "Il sistema funziona correttamente")
    )

    (domanda  (attributo riavvio-forzato)
              (testo-domanda "Il dispositivo si riavvia forzatamente piu' volte?")
              (risposte-valide si no)
              (descrizione-risposte "Si" "No")
    )

    (domanda  (attributo bluescreen)
              (testo-domanda "Il sistema termina con un errore bluescreen?")
              (risposte-valide si no non-so)
              (descrizione-risposte "Si" "No" "Non so")
    )

    (domanda  (attributo installazione-nuovo-hw)
              (testo-domanda "E' stato installato del nuovo hardware prima che il problema cominciasse a verificarsi?")
              (risposte-valide si no non-so)
              (descrizione-risposte "Si" "No" "Non so")
    )

    (domanda  (attributo segnali-bios)
              (testo-domanda "Il dispositivo emana dei segnali sonori al momento del boot?")
              (risposte-valide si no )
              (descrizione-risposte "Si" "No")
    )

    (domanda  (attributo stato-video)
              (testo-domanda "Il problema ha a che fare con il display o il segnale video?")
              (risposte-valide fallito ok )
              (descrizione-risposte "Si" "No")
    )

    (domanda  (attributo display-rotto)
              (testo-domanda "Il display del dispositivo è rotto o incrinato?")
              (risposte-valide si no )
              (descrizione-risposte "Si" "No")
    )

    (domanda  (attributo disturbo-video)
              (testo-domanda "E' presente un disturbo del segnale video? [schermo nero oppure fasce colorate sul display]")
              (risposte-valide si no )
              (descrizione-risposte "Si" "No")
    )


  )
