
(defglobal ?*highest-priority* = 1000)
(defglobal ?*high-priority* = 100)
(defglobal ?*low-priority* = -100)
(defglobal ?*lowest-priority* = -1000)

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

  (deftemplate diagnosi
    (slot attributo   (type SYMBOL))
    (slot titolo      (type STRING))
    (slot descrizione (type STRING))
  )




(defmodule MAIN(export ?ALL))

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

(deffunction MAIN::ask-question-revision(?n-domande-chieste)
  (printout t "Inserire il numero di domanda da modificare oppure" crlf "premere 0 per tornare alla normale esecuzione del programma: ")
  (bind ?answer (read))
  (if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))
  (while (and (> ?answer ?n-domande-chieste) (< ?answer 0)) do
    (printout t crlf "Valore inserito non valido, riprovare: ")
    (bind ?answer (read))
  )
?answer)

(deffunction MAIN::ask-question-direct (?j ?question $?allowed-values)
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


(deffunction MAIN::ask-question (?j ?question $?allowed-values)
  (stampa-header)
  (printout t "***** DOMANDA N." ?j " *****" crlf ?question crlf crlf)
  (loop-for-count (?cnt1 1 (length ?allowed-values)) do
      (printout t ?cnt1 ". " (nth$ ?cnt1 ?allowed-values) crlf)
  )
  (printout t crlf "9. Perche' questa domanda?")
  (printout t crlf "0. Revisiona domande precedenti." crlf crlf)
  (printout t "Inserire risposta: ")
  (bind ?answer (read))
  (if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))
  (while (and (not (member (nth$ ?answer ?allowed-values) ?allowed-values)) (not (= ?answer 0)) (not (= ?answer 9))) do
      (printout t crlf "Valore inserito non valido, riprovare: ")
      (bind ?answer (read))
      (if (lexemep ?answer) then (bind ?answer (lowcase ?answer))))
  (printout t crlf crlf)
   ?answer)


(deffunction MAIN::ask-stop-program ()
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

(deffunction MAIN::stampa-header-revisione()
  (clear-window)
  (printout t crlf "******************** REVISIONE DOMANDE ********************" crlf crlf)
)

(deffunction MAIN::stampa-footer-revisione()
  (printout t crlf "***********************************************************" crlf crlf crlf)
)





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
  (declare (salience ?*highest-priority*))
  =>
  (load-facts "data/DOMANDE.DAT")
  (load-facts "data/DIAGNOSI.DAT")
  ;(assert (facts-loaded))
  (clear-window)
  (focus DOMANDE-GENERICHE)
)
;   (printout t crlf crlf)
;   (printout t   "***                                                 ***" crlf
;                 "**  SISTEMA DIAGNOSTICO PER DISPOSITIVI ELETTRONICI  **" crlf
;                 "*                                                     *" crlf
;                 "*     Rispondere alle domande inserendo il numero     *" crlf
;                 "**       corrispondente alla risposta corretta.      **" crlf
;                 "***                                                 ***" crlf crlf))


(defrule MAIN::diagnosi-trovata
  (declare (salience ?*highest-priority*))
  (nodo (nome diagnosi) (valore ?attr-diagnosi))
  (diagnosi (attributo ?attr-diagnosi) (titolo ?titolo) (descrizione ?desc))
  =>
  (printout t crlf "***** DIAGNOSI *****" crlf " - " ?titolo ":" crlf ?desc crlf crlf)
  (assert(ferma-programma))
)


; (defrule diagnosi-parziale-trovata
;   (declare (salience ?*highest-priority*))
;   (nodo (nome diagnosi) (valore ?val) (descrizione ?desc))
;   =>
;   (printout t crlf "DIAGNOSI PARZIALE TROVATA: " ?desc)
;   (assert(ferma-programma))
; )


(defrule MAIN::ferma-esecuzione
  (declare (salience ?*low-priority*))
  (ferma-programma)
  =>
  (ask-stop-program)
)








; STAMPA ELENCO E REVISIONE DOMANDE
;*******************************************************************************

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
    (if (= ?risposta 9) then
      (retract ?ask)
      (assert (nodo (nome chiedi)(valore ?attr)(nodo-padre ?p))) ;;NECESSARIO PER RIPROPORRE LA STESSA DOMANDA NEL CASO DI ANNULLAMENTO REVISIONE
      (assert (nodo (nome spiegazione) (valore ?attr)))
      (focus SPIEGAZIONE)
    else
      ;;(assert (nodo (nome ?attr) (valore (nth$ ?risposta ?risposte)) (descrizione (nth$ ?risposta ?descrizioni)) (tipo info-utente) (nodo-padre ?ask)))
      (modify ?f (gia-chiesta TRUE)(num-domanda ?j)(risposta-selezionata ?risposta))
      ;;(retract ?ask)
      (retract ?cont-dom)
      (assert (contatore-domande ?j))
    )
  )
)

(defrule usa-risposta-utente
  ?ask <- (nodo (nome chiedi)(valore ?attr)(nodo-padre $?p))
  ?f <- (domanda (attributo ?attr) (testo-domanda ?domanda) (risposte-valide $?risposte) (descrizione-risposte $?descrizioni) (gia-chiesta TRUE) (risposta-selezionata ?risp))
  (not (nodo (nome ?attr)))
  =>
  (assert (nodo (nome ?attr) (valore (nth$ ?risp ?risposte)) (descrizione (nth$ ?risp ?descrizioni)) (tipo info-utente) (nodo-padre ?ask)))
)





;;******************************************************************************
;;*    REGOLE PER CHIDERE DOMANDE ALL'UTENTE                                   *
;;******************************************************************************


;; DOMANDE ACCENSIONE E SO *****************************************************

(defrule chiedi-riavvio-forzato
  ?p1 <- (nodo (nome stato-accensione) (valore ok))
  =>
  (assert (nodo (nome chiedi) (valore riavvio-forzato) (nodo-padre ?p1)))
)

(defrule chiedi-alimentazione
  ?p1 <- (nodo (nome stato-accensione) (valore fallito))
  =>
  (assert (nodo (nome chiedi) (valore alimentazione-collegata) (nodo-padre ?p1)))
)

;; Per dispositivi desktop ...

(defrule chiedi-spia-alimentatore-pcdesktop
  ?p1 <- (nodo (nome stato-accensione) (valore fallito))
  ?p2 <- (nodo (nome tipo-dispositivo) (valore pc-desktop))
  ?p3 <- (nodo (nome alimentazione-collegata) (valore si))
  ?p4 <- (nodo (nome interruttore-alimentatore) (valore acceso))
  =>
  (assert (nodo (nome chiedi) (valore spia-alimentatore-pcdesktop) (nodo-padre ?p1 ?p2 ?p3 ?p4)))
)

(defrule chiedi-interruttore-alimentatore
  ?p1 <- (nodo (nome stato-accensione) (valore fallito))
  ?p2 <- (nodo (nome tipo-dispositivo) (valore pc-desktop))
  ?p3 <- (nodo (nome alimentazione-collegata) (valore si))
  =>
  (assert (nodo (nome chiedi) (valore interruttore-alimentatore) (nodo-padre ?p1 ?p2 ?p3)))
)

;; Per dispositivi laptop ...

(defrule chiedi-spia-alimentatore-pcportatile
  ?p1 <- (nodo (nome stato-accensione) (valore fallito))
  ?p2 <- (nodo (nome tipo-dispositivo) (valore pc-portatile))
  ?p3 <- (nodo (nome alimentazione-collegata) (valore si))
  =>
  (assert (nodo (nome chiedi) (valore spia-alimentatore-pcportatile) (nodo-padre ?p1 ?p2 ?p3)))
)

(defrule chiedi-batteria-difettosa
  ?p1 <- (nodo (nome stato-accensione) (valore fallito))
  ?p2 <- (nodo (nome tipo-dispositivo) (valore pc-portatile))
  ?p3 <- (nodo (nome alimentazione-collegata) (valore si))
  =>
  (assert (nodo (nome chiedi) (valore batteria-difettosa) (nodo-padre ?p1 ?p2 ?p3)))
)

;;******************************************************************************


;; DOMANDE VIDEO ***************************************************************

(defrule chiedi-disturbo-video
  ?p1 <- (nodo (nome problema-principale) (valore video))
  =>
  (assert (nodo (nome chiedi) (valore disturbo-video) (nodo-padre ?p1)))
)

(defrule chiedi-monitor-esterno
  ?p1 <- (nodo (nome disturbo-video) (valore ?v&fasce|schermo-nero|linee-oriz))
  ;?p2 <- (nodo (nome riavvio-forzato) (valore no))
  =>
  (assert (nodo (nome chiedi) (valore monitor-esterno) (nodo-padre ?p1)))
)

(defrule chiedi-fasce-bios
  ?p1 <- (nodo (nome disturbo-video) (valore ?v&fasce|linee-oriz))
  =>
  (assert (nodo (nome chiedi) (valore fasce-bios) (nodo-padre ?p1)))
)

(defrule chiedi-blocco-cursore
  ?p1 <- (nodo (nome disturbo-video) (valore schermo-nero))
  =>
  (assert (nodo (nome chiedi) (valore blocco-cursore) (nodo-padre ?p1)))
)

(defrule chiedi-cavi-display
  ?p1 <- (nodo (nome disturbo-video) (valore schermo-nero))
  ?p2 <- (nodo (nome tipo-dispositivo) (valore pc-desktop))
  =>
  (assert (nodo (nome chiedi) (valore cavi-display) (nodo-padre ?p1 ?p2 )))
)

(defrule chiedi-muovere-cavi-display
  ?p1 <- (nodo (nome disturbo-video) (valore ?v&fasce|linee-oriz))
  ?p2 <- (nodo (nome tipo-dispositivo) (valore pc-desktop))
  =>
  (assert (nodo (nome chiedi) (valore muovere-cavi-display) (nodo-padre ?p1 ?p2 )))
)

;;******************************************************************************




;;******************************************************************************
;;*     DIAGNOSI                                                               *
;;******************************************************************************

;; DIAGNOSI ACCENSIONE E SO ****************************************************

(defrule diagnosi-alimentazione-disconnessa
  ?p1 <- (nodo (nome stato-accensione) (valore fallito))
  ?p2 <- (nodo (nome alimentazione-collegata) (valore no))
  =>
  (assert (nodo (nome diagnosi) (valore alimentazione-disconnessa)(nodo-padre ?p1 ?p2)))
)

(defrule diagnosi-batteria-difettosa
  ?p1 <- (nodo (nome batteria-difettosa) (valore si))
  =>
  (assert (nodo (nome diagnosi) (valore batteria-difettosa)(nodo-padre ?p1)))
)

(defrule diagnosi-alimentatore-spento
  ?p1 <- (nodo (nome interruttore-alimentatore) (valore spento))
  ?p2 <- (nodo (nome stato-accensione) (valore fallito))
  ?p3 <- (nodo (nome alimentazione-collegata) (valore si))
  =>
  (assert (nodo (nome diagnosi) (valore alimentatore-spento)(nodo-padre ?p1 ?p2 ?p3)))
)

(defrule diagnosi-alimentatore-guasto
?p1 <- (nodo (nome stato-accensione) (valore fallito))
?p2 <- (nodo (nome alimentazione-collegata) (valore si))
(or
  ?p3 <- (nodo (nome ronzio-alimentatore) (valore si))
  ?p3 <- (nodo (nome spia-alimentatore-pcportatile) (valore ?v&no|sconosciuto))
  ?p3 <- (nodo (nome spia-alimentatore-pcdesktop) (valore ?v&no|sconosciuto))
)
=>
(assert (nodo (nome diagnosi) (valore alimentatore-guasto)(nodo-padre ?p1 ?p2 ?p3)))
)

(defrule diagnosi-scheda-madre-guasta
?p1 <- (nodo (nome stato-accensione) (valore fallito))
?p2 <- (nodo (nome alimentazione-collegata) (valore si))
(or
  ?p3 <- (nodo (nome spia-alimentatore-pcportatile) (valore sconosciuto))
  ?p3 <- (nodo (nome spia-alimentatore-pcdesktop) (valore sconosciuto))
)
=>
(assert (nodo (nome diagnosi) (valore scheda-madre-guasta)(nodo-padre ?p1 ?p2 ?p3)))
)

;;******************************************************************************


;; DIAGNOSI VIDEO **************************************************************

(defrule diagnosi-cavi-display-non-connessi
  ?p1 <- (nodo (nome muovere-cavi-display) (valore risolto))
  =>
  (assert (nodo (nome diagnosi) (valore cavi-display-non-connessi) (nodo-padre ?p1)))
)
;;Se il problema persiste e' possibile che il cavo sia danneggiato.

(defrule diagnosi-display-guasto
  ?p1 <- (nodo (nome disturbo-video) (valore ?v1&fasce|linee-oriz))
  ?p2 <- (nodo (nome riavvio-forzato) (valore no))
  ?p3 <- (nodo (nome monitor-esterno) (valore ?v2&funzionante|no))
  ?p4 <- (nodo (nome fasce-bios) (valore si))
  ?p5 <- (nodo (nome muovere-cavi-display) (valore ?v3&non-risolto|interni))
  =>
  (assert (nodo (nome diagnosi) (valore guasto-display) (nodo-padre ?p1 ?p2 ?p3 ?p4 ?p5)))
)

(defrule diagnosi-display-guasto-2
  ?p1 <- (nodo (nome disturbo-video) (valore schermo-nero))
  ?p2 <- (nodo (nome cavi-display) (valore ?v1&ok|interni))
  ?p3 <- (nodo (nome monitor-esterno) (valore ?v2&funzionante|no))
  ?p4 <- (nodo (nome blocco-cursore) (valore no))
  =>
  (assert (nodo (nome diagnosi) (valore guasto-display) (nodo-padre ?p1 ?p2 ?p3 ?p4)))
)

(defrule diagnosi-cavi-display-portatile-guasti
  ?p1 <- (nodo (nome diagnosi) (valore guasto-display))
  (or
    ?p2 <- (nodo (nome cavi-display) (valore interni))
    ?p2 <- (nodo (nome muovere-cavi-display) (valore interni))
  )
  (not (nodo (nome diagnosi) (valore cavi-display-portatile-guasti)))
  =>
  (assert (nodo (nome diagnosi) (valore cavi-display-portatile-guasti) (nodo-padre ?p1 ?p2)))
)

(defrule diagnosi-display-guasto-3
  ?p1 <- (nodo (nome disturbo-video) (valore macchie))
  =>
  (assert (nodo (nome diagnosi) (valore guasto-display) (nodo-padre ?p1)))
)

(defrule diagnosi-guasto-vga
  ?p1 <- (nodo (nome disturbo-video) (valore fasce))
  ?p2 <- (nodo (nome monitor-esterno) (valore ?v1&errore|no))
  ?p3 <- (nodo (nome muovere-cavi-display) (valore ?v2&non-risolto|interni))
  =>
  (assert (nodo (nome diagnosi) (valore guasto-vga) (nodo-padre ?p1 ?p2 ?p3)))
)

(defrule diagnosi-guasto-vga-2
  ?p1 <- (nodo (nome disturbo-video) (valore schermo-nero))
  ?p2 <- (nodo (nome cavi-display) (valore ?v2&ok|interni))
  ?p3 <- (nodo (nome monitor-esterno) (valore ?v1&errore|no))
  ?p4 <- (nodo (nome blocco-cursore) (valore no))
  =>
  (assert (nodo (nome diagnosi) (valore guasto-vga) (nodo-padre ?p1 ?p2 ?p3 ?p4)))
)

(defrule diagnosi-problema-driver-video
  ?p1 <- (nodo (nome disturbo-video) (valore fasce))
  ?p2 <- (nodo (nome monitor-esterno) (valore ?v1&errore|no))
  ?p3 <- (nodo (nome fasce-bios) (valore no))
  ?p4 <- (nodo (nome muovere-cavi-display) (valore ?v2&non-risolto|interni))
  =>
  (assert (nodo (nome diagnosi) (valore problema-driver-video) (nodo-padre ?p1 ?p2 ?p3 ?p4)))
)
;;provare ad aggiornare o ripristinare i driver.

(defrule diagnosi-problema-driver-video-2
  ?p1 <- (nodo (nome disturbo-video) (valore schermo-nero))
  ?p2 <- (nodo (nome cavi-display) (valore ?v2&ok|interni))
  ?p3 <- (nodo (nome monitor-esterno) (valore ?v1&errore|no))
  ?p4 <- (nodo (nome blocco-cursore) (valore no))
  =>
  (assert (nodo (nome diagnosi) (valore problema-driver-video) (nodo-padre ?p1 ?p2 ?p3 ?p4)))
)

(defrule diagnosi-problema-caricamento-SO
  ?p1 <- (nodo (nome disturbo-video) (valore schermo-nero))
  ?p2 <- (nodo (nome cavi-display) (valore ?v2&ok|interni))
  ?p3 <- (nodo (nome monitor-esterno) (valore ?v1&errore|no))
  ?p4 <- (nodo (nome blocco-cursore) (valore si))
  =>
  (assert (nodo (nome diagnosi) (valore problema-caricamento-SO) (nodo-padre ?p1 ?p2 ?p3 ?p4)))
)

(defrule diagnosi-cavi-display-disconnessi ;;NON INSERITA NELLE DIAGNOSI
  ?p1 <- (nodo (nome cavi-display) (valore errore))
  =>
  (assert (nodo (nome diagnosi) (valore cavi-display-disconnessi)(nodo-padre ?p1)))
)

;;******************************************************************************




;; REGOLE PER INFERENZA ********************************************************

(defrule cavi-display-portatile
  ?p1 <- (nodo (nome tipo-dispositivo) (valore pc-portatile))
  =>
  (assert (nodo (nome muovere-cavi-display) (valore interni) (nodo-padre ?p1 )))
  (assert (nodo (nome cavi-display) (valore interni) (nodo-padre ?p1 )))
)

(defrule deduci-SO-windows
  ?p1 <- (nodo (nome tipo-dispositivo) (valore ?dispositivo&pc-desktop|pc-portatile))
  =>
  (assert (nodo (nome sistema-operativo) (valore windows) (tipo inferenza) (nodo-padre ?p1)))
)

(defrule deduci-SO-android
  ?p1 <- (nodo (nome tipo-dispositivo) (valore ?dispositivo&tablet|smartphone))
  =>
  (assert (nodo (nome SO) (valore android) (tipo inferenza) (nodo-padre ?p1)))
)








(defmodule ELENCO-DIAGNOSI (import MAIN ?ALL)(export ?ALL))

(defmodule ELENCO-DOMANDE(import MAIN ?ALL)(export ?ALL))



(defmodule SPIEGAZIONE(import MAIN ?ALL)(export ?ALL))

; SPIEGAZIONE DOMANDA
;*******************************************************************************

(deffunction SPIEGAZIONE::leggi-nodi($?p)
  (printout t "Motivazione alla domanda: " crlf)
  (loop-for-count (?cnt1 1 (length ?p)) do
    (bind ?v (fact-slot-value (nth$ ?cnt1 ?p) nome))
    (assert (spiega ?v))
  )
)

(deffunction SPIEGAZIONE::stampa-spiegazione(?n ?dom ?risp)
  (printout t "- Alla domanda n." ?n " l'utente ha risposto: " crlf ?risp crlf crlf)
)

(defrule SPIEGAZIONE::debug
  (declare (salience ?*highest-priority*))
  =>
  (printout t "DEBUG >> Modulo spiegazioni" crlf crlf)
)

(defrule SPIEGAZIONE::init-spiegazione
  ?target <- (nodo (nome spiegazione) (valore ?attr ))
  ?domanda <- (nodo (nome chiedi) (valore ?attr) (nodo-padre $?p))
  =>
  (retract ?target)
  (leggi-nodi ?p)
  ;(bind ?answer (read))
)

(defrule SPIEGAZIONE::spiega-attributo
  ?s <- (spiega ?attr)
  ?d <- (domanda (attributo ?attr) (gia-chiesta TRUE) (num-domanda ?n-domanda) (risposta-selezionata ?n-risposta) (testo-domanda ?domanda) (descrizione-risposte $?risposte))
  =>
  (bind ?risposta (nth$ ?n-risposta ?risposte))
  (stampa-spiegazione ?n-domanda ?domanda ?risposta)
  (retract ?s)
)

(defrule SPIEGAZIONE::end-spiegazione
  (not (nodo (nome spiegazione)))
  (not (spiega ?s))
  =>
  (printout t "end spiegazione" crlf)
  (bind ?answer (read))
)






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
        (assert (nodo (nome chiedi) (valore installazione-nuovo-hw)))
      )

      (defrule DOMANDE-GENERICHE::chiedi-garanzia
        ?p1 <- (nodo (nome anni-dispositivo) (valore ?val&meno-2-anni|meno-5-anni|sconosciuto))
        =>
        (assert (nodo (nome chiedi) (valore garanzia) (nodo-padre ?p1)))
      )


  ;*******************************************************************************
