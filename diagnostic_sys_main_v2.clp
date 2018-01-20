
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
    (slot certezza (type FLOAT) (default 1.0))
    (slot stato (type SYMBOL) (default attivo))
    (slot tipo (type SYMBOL))
    (multislot nodo-padre (type FACT-ADDRESS))
    (slot descrizione (type STRING))
  )

  (deftemplate domanda
    (slot attributo     (type SYMBOL) (default ?NONE))
    (slot gia-chiesta   (default  FALSE))
    (slot num-domanda (type INTEGER))
    (multislot risposte-valide (type SYMBOL) (default ?NONE))
    (slot risposta-selezionata (type INTEGER))
    (multislot testo-domanda (default ?NONE))
    ;(slot testo-domanda (type STRING) (default ?NONE))
    (multislot descrizione-risposte (type STRING) (default ?NONE))
    (slot stampata (default FALSE))
    (slot domanda-generica (default FALSE))
  )

  (deftemplate diagnosi
    (slot attributo   (type SYMBOL))
    (slot titolo      (type STRING))
    (slot descrizione (type STRING))
    (slot stampata (default FALSE))
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
  (printout t "***** DOMANDA N." ?j " *****" crlf)
  (format t ?question)
  (printout t crlf crlf)
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
  (bind ?answer 0)
  (while (not (member ?answer (create$ 1 2 3 4)))
      (printout t crlf "Selezionare un opzione per continuare:" crlf "1. Continua esecuzione" crlf "2. Revisiona le risposte date" crlf "3. Riavvia programma" crlf "4. Termina programma" crlf crlf)
      (bind ?answer (read))
  )
  (if (eq ?answer 2) then (assert (init-revisiona-domande)))
  (if (eq ?answer 3) then (reset) (run))
  (if (eq ?answer 4) then (halt))
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


;********************************
;* CF RULES AND FUNCTIONS       *
;********************************

(deffunction MAIN::AND-multifield($?list)
  (bind ?min-v (nth$ 1 ?list))
  (loop-for-count (?cnt 2 (length ?list)) do
      (bind ?temp (nth$ ?cnt ?list))
      (if (< ?temp ?min-v) then (bind ?min-v ?temp))
  )
  (return ?min-v)
)

(deffunction MAIN::OR-multifield($?list)
  (bind ?max-v (nth$ 1 ?list))
  (loop-for-count (?cnt 2 (length ?list)) do
      (bind ?temp (nth$ ?cnt ?list))
      (if (> ?temp ?max-v) then (bind ?max-v ?temp))
  )
  (return ?max-v)
)

(deffunction MAIN::calcola-certezza(?cert-RHS $?cert-LHS)
  (bind ?min-cert (AND-multifield $?cert-LHS))
  (return (* ?cert-RHS ?min-cert))
)

(deffunction MAIN::combina-CF(?cf1 ?cf2)
  (if (and (> ?cf1 0) (> ?cf2 0)) then (bind ?CF (- (+ ?cf1 ?cf2) (* ?cf1 ?cf2))))
  (if (and (< ?cf1 0) (< ?cf2 0)) then (bind ?CF (+ (+ ?cf1 ?cf2) (* ?cf1 ?cf2))))
  (if (< (* ?cf1 ?cf2) 0) then (bind ?CF (/ (+ ?cf1 ?cf2) (- 1 (min (abs ?cf1)(abs ?cf2))))))
  (return ?CF)
)

(defrule MAIN::combina-certezza-nodi
  (declare (salience ?*highest-priority*))
  ?nodo1 <- (nodo (nome ?n1) (valore ?v1) (certezza ?c1) (nodo-padre $?padre1) (stato attivo))
  ?nodo2 <- (nodo (nome ?n1) (valore ?v1) (certezza ?c2) (nodo-padre $?padre2) (stato attivo))
  (test (neq ?nodo1 ?nodo2))
  =>
  ;(printout t "Combine: " ?n1 " - " ?v1 " - " ?c1 "/" ?c2 crlf)
  ;(retract ?nodo1)
  (bind ?x1  (modify ?nodo1 (stato inattivo)))
  (bind ?x2  (modify ?nodo2 (stato inattivo)))
  (assert (nodo (nome ?n1) (valore ?v1) (certezza (combina-CF ?c1 ?c2)) (nodo-padre ?x1 ?x2)))
)

(defrule MAIN::rimuovi-padri-duplicati
  (declare (salience ?*highest-priority*))
  ?n <- (nodo (nodo-padre $?nodi1 ?elem $?nodi2 ?elem $?nodi3))
  =>
  (modify ?n (nodo-padre ?nodi1 ?elem ?nodi2 ?nodi3))
)

;***********EXAMPLE CF RULES ***********************
(deffacts exfacts
  (nodo (nome statox) (valore x) (certezza 0.6))
  (nodo (nome statoy) (valore y) (certezza 0.8))
  (nodo (nome statoz) (valore z) (certezza -0.6))
)
(defrule chiedi-x
  ?p1 <- (nodo (nome statox) (valore x) (certezza ?crt1))
  ?p2 <- (nodo (nome statoy) (valore y) (certezza ?crt2))
  =>
  (bind ?crt (calcola-certezza 0.7 ?crt1 ?crt2))
  (assert (nodo (nome statoz) (valore z) (certezza ?crt) (nodo-padre ?p1 ?p2)))
)
;***************************************************


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
  ;(declare (salience ?*high-priority*))
  (nodo (nome diagnosi) (valore ?attr-diagnosi) (certezza ?cer&:(> ?cer 0.80)) (stato attivo))
  (diagnosi (attributo ?attr-diagnosi))
  (not (stampa-diagnosi))
  (not (ferma-programma))
  =>
  (printout t crlf "***** DIAGNOSI *****" crlf crlf)
  (assert (stampa-diagnosi))
  ;(assert(ferma-programma))
)

(defrule MAIN::stampa-diagnosi
  ;(declare (salience ?*high-priority*))
  (stampa-diagnosi)
  (not (resetta-diagnosi))
  (nodo (nome diagnosi) (valore ?attr-diagnosi) (certezza ?cer&:(> ?cer 0.10)) (stato attivo))
  ?d <- (diagnosi (attributo ?attr-diagnosi) (titolo ?titolo) (descrizione ?desc) (stampata FALSE))
  =>
  (printout t "[" (integer (* ?cer 100)) "%] - " ?titolo ": " ?desc crlf)
  (modify ?d (stampata TRUE))
)

(defrule MAIN::fine-stampa-diagnosi
  ;(declare (salience ?*high-priority*))
  ?s <- (stampa-diagnosi)
  (nodo (nome diagnosi) (valore ?attr-diagnosi) (certezza ?cer&:(> ?cer 0.10)) (stato attivo))
  (not (diagnosi (attributo ?attr-diagnosi) (stampata FALSE)))
  =>
  ;(retract ?s)
  (assert (resetta-diagnosi))
)

(defrule MAIN::resetta-diagnosi
  (stampa-diagnosi)
  (resetta-diagnosi)
  ?d <- (diagnosi (attributo ?attr-diagnosi) (stampata TRUE))
  =>
  (modify ?d (stampata FALSE))
)

(defrule MAIN::fine-resetta-diagnosi
  ?s <- (stampa-diagnosi)
  ?r <- (resetta-diagnosi)
  (not (diagnosi (attributo ?attr-diagnosi) (stampata TRUE)))
  =>
  (retract ?s)
  (retract ?r)
  (assert (ferma-programma))
)

(defrule MAIN::fine-domande
(declare (salience ?*lowest-priority*))
(nodo (nome chiedi) (valore ?dom))
(not (domanda (attributo ?dom) (gia-chiesta FALSE)))
(not (stampa-diagnosi))
(not (ferma-programma))
=>
(assert (stampa-diagnosi))
)


; (defrule diagnosi-parziale-trovata
;   (declare (salience ?*highest-priority*))
;   (nodo (nome diagnosi) (valore ?val) (descrizione ?desc))
;   =>
;   (printout t crlf "DIAGNOSI PARZIALE TROVATA: " ?desc)
;   (assert(ferma-programma))
; )


(defrule MAIN::ferma-esecuzione
  (declare (salience ?*highest-priority*))
  ?x <- (ferma-programma)
  =>
  (retract ?x)
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


; REVISIONA DOMANDA (RITRATTAZIONE)
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
  (assert (attiva-nodi))
)

(defrule attiva-nodi-diagnosi-terminali
  ?r <- (attiva-nodi)
  ?n1 <- (nodo (nome diagnosi) (valore ?v) (stato inattivo))
  (not (nodo (nome diagnosi) (valore ?v) (nodo-padre $?x ?n1 $?y)))
  =>
  (modify ?n1 (stato attivo))
)

(defrule END-attiva-nodi-diagnosi-terminali
  ?r <- (attiva-nodi)
  (not (nodo (nome diagnosi) (valore ?v) (stato inattivo)))
  (not(nodo (nome diagnosi) (valore ?v) (nodo-padre $?x ?n1 $?y)))
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




;; DOMANDE VIDEO ***************************************************************

(defrule chiedi-disturbo-video
  ?p1 <- (nodo (nome problema-principale) (valore video))
  =>
  (assert (nodo (nome chiedi) (valore disturbo-video) (nodo-padre ?p1)))
)

(defrule chiedi-monitor-esterno
  ;?p1 <- (nodo (nome disturbo-video) (valore ?v&fasce|schermo-nero|linee-oriz))
  ?p1 <- (nodo (nome tipo-disturbo-video) (valore display-rotto))
  ;?p2 <- (nodo (nome riavvio-forzato) (valore no))
  =>
  (assert (nodo (nome chiedi) (valore monitor-esterno) (nodo-padre ?p1)))
)

(defrule chiedi-fasce-bios
  ;?p1 <- (nodo (nome disturbo-video) (valore ?v&fasce|linee-oriz))
  ?p1 <- (nodo (nome tipo-disturbo-video) (valore interferenza))
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
  ?p2 <- (nodo (nome cavi-display-accessibili) (valore si))
  ;?p2 <- (nodo (nome tipo-dispositivo) (valore pc-desktop))
  =>
  (assert (nodo (nome chiedi) (valore cavi-display) (nodo-padre ?p1 ?p2 )))
)

(defrule chiedi-muovere-cavi-display
  ?p1 <- (nodo (nome tipo-disturbo-video) (valore interferenza))
  ;?p1 <- (nodo (nome disturbo-video) (valore ?v&fasce|linee-oriz))
  ?p2 <- (nodo (nome cavi-display-accessibili) (valore si))
  ;?p2 <- (nodo (nome tipo-dispositivo) (valore pc-desktop))
  ?p3 <- (nodo (nome momento-manifestazione-problema) (valore avvio))
  =>
  (assert (nodo (nome chiedi) (valore muovere-cavi-display) (nodo-padre ?p1 ?p2 ?p3 )))
)

;;******************************************************************************




;;******************************************************************************
;;*     DIAGNOSI                                                               *
;;******************************************************************************

;; DIAGNOSI ACCENSIONE E SO ****************************************************

; (defrule diagnosi-alimentazione-disconnessa
;   ?p1 <- (nodo (nome stato-accensione) (valore fallito) (certezza ?crt1))
;   ?p2 <- (nodo (nome alimentazione-collegata) (valore no) (certezza ?crt2))
;   ?p3 <- (nodo (nome alimentatore-funzionante) (valore no) (certezza ?crt3))
;   =>
;   (bind ?crt (calcola-certezza 0.9 ?crt1 ?crt2 ?crt3))
;   (assert (nodo (nome diagnosi) (valore alimentazione-disconnessa)(nodo-padre ?p1 ?p2 ?p3) (certezza ?crt)))
; )

; (defrule diagnosi-batteria-difettosa
;   ?p1 <- (nodo (nome batteria-difettosa) (valore si) (certezza ?crt1))
;   =>
;   (assert (nodo (nome diagnosi) (valore batteria-difettosa) (certezza (* 1.0 ?crt1)) (nodo-padre ?p1)))
; )

; (defrule diagnosi-alimentatore-spento
;   ?p1 <- (nodo (nome interruttore-alimentatore) (valore spento))
;   ?p2 <- (nodo (nome stato-accensione) (valore fallito))
;   ?p3 <- (nodo (nome alimentazione-collegata) (valore si))
;   =>
;   (assert (nodo (nome diagnosi) (valore alimentatore-spento)(nodo-padre ?p1 ?p2 ?p3)))
; )

; (defrule diagnosi-alimentatore-guasto
; ?p1 <- (nodo (nome stato-accensione) (valore fallito))
; ?p2 <- (nodo (nome alimentazione-collegata) (valore si))
; (or
;   ?p3 <- (nodo (nome ronzio-alimentatore) (valore si))
;   ?p3 <- (nodo (nome spia-alimentatore-pcportatile) (valore ?v&no|sconosciuto))
;   ?p3 <- (nodo (nome spia-alimentatore-pcdesktop) (valore ?v&no|sconosciuto))
; )
; =>
; (assert (nodo (nome diagnosi) (valore alimentatore-guasto)(nodo-padre ?p1 ?p2 ?p3)))
; )
;
; (defrule diagnosi-scheda-madre-guasta
; ?p1 <- (nodo (nome stato-accensione) (valore fallito))
; ?p2 <- (nodo (nome alimentazione-collegata) (valore si))
; (or
;   ?p3 <- (nodo (nome spia-alimentatore-pcportatile) (valore sconosciuto))
;   ?p3 <- (nodo (nome spia-alimentatore-pcdesktop) (valore sconosciuto))
; )
; =>
; (assert (nodo (nome diagnosi) (valore scheda-madre-guasta)(nodo-padre ?p1 ?p2 ?p3)))
; )

;;******************************************************************************


;; DIAGNOSI VIDEO **************************************************************

(defrule diagnosi-cavi-display-non-connessi
  ?p1 <- (nodo (nome muovere-cavi-display) (valore risolto))
  =>
  (assert (nodo (nome diagnosi) (valore cavi-display-non-connessi) (nodo-padre ?p1)))
)
;;Se il problema persiste e' possibile che il cavo sia danneggiato.

(defrule diagnosi-display-guasto
  ?p1 <- (nodo (nome tipo-disturbo-video) (valore interferenza))
  ?p2 <- (nodo (nome riavvio-forzato) (valore no))
  ?p3 <- (nodo (nome monitor-esterno) (valore ?v2&funzionante|no))
  ?p4 <- (nodo (nome momento-manifestazione-problema) (valore avvio))
  (or
    ?p5 <- (nodo (nome muovere-cavi-display) (valore non-risolto))
    ?p5 <- (nodo (nome cavi-display-accessibili) (valore no))
  )
  =>
  (assert (nodo (nome diagnosi) (valore guasto-display) (nodo-padre ?p1 ?p2 ?p3 ?p4 ?p5)))
)

(defrule diagnosi-display-guasto-2
  ?p1 <- (nodo (nome disturbo-video) (valore schermo-nero))
  (or
    ?p2 <- (nodo (nome cavi-display) (valore ok))
    ?p2 <- (nodo (nome cavi-display-accessibili) (valore no))
  )
  ?p3 <- (nodo (nome monitor-esterno) (valore ?v2&funzionante|no))
  ?p4 <- (nodo (nome blocco-cursore) (valore no))
  =>
  (assert (nodo (nome diagnosi) (valore guasto-display) (nodo-padre ?p1 ?p2 ?p3 ?p4)))
)

(defrule diagnosi-cavi-display-portatile-guasti
  ?p1 <- (nodo (nome diagnosi) (valore guasto-display))
  ?p2 <- (nodo (nome cavi-display-accessibili) (valore no))
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
  (or
    ?p3 <- (nodo (nome muovere-cavi-display) (valore non-risolto))
    ?p3 <- (nodo (nome cavi-display-accessibili) (valore no))
  )
  =>
  (assert (nodo (nome diagnosi) (valore guasto-vga) (nodo-padre ?p1 ?p2 ?p3)))
)

(defrule diagnosi-guasto-vga-2
  ?p1 <- (nodo (nome disturbo-video) (valore schermo-nero))
  (or
    ?p2 <- (nodo (nome cavi-display) (valore ok))
    ?p2 <- (nodo (nome cavi-display-accessibili) (valore no))
  )
  ?p3 <- (nodo (nome monitor-esterno) (valore ?v1&errore|no))
  ?p4 <- (nodo (nome blocco-cursore) (valore no))
  =>
  (assert (nodo (nome diagnosi) (valore guasto-vga) (nodo-padre ?p1 ?p2 ?p3 ?p4)))
)

(defrule diagnosi-problema-driver-video
  ?p1 <- (nodo (nome tipo-disturbo-video) (valore fasce))
  ?p2 <- (nodo (nome monitor-esterno) (valore ?v1&errore|no))
  ?p3 <- (nodo (nome fasce-bios) (valore no))
  (or
    ?p4 <- (nodo (nome muovere-cavi-display) (valore non-risolto))
    ?p4 <- (nodo (nome cavi-display-accessibili) (valore no))
  )
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
  (or
    ?p2 <- (nodo (nome cavi-display) (valore ok))
    ?p2 <- (nodo (nome cavi-display-accessibili) (valore no))
  )
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

; (defrule cavi-display-portatile
;   ?p1 <- (nodo (nome tipo-dispositivo) (valore pc-portatile))
;   =>
;   (assert (nodo (nome muovere-cavi-display) (valore interni) (nodo-padre ?p1 )))
;   (assert (nodo (nome cavi-display) (valore interni) (nodo-padre ?p1 )))
; )



(defrule interferenza-video
  ?p1 <- (nodo (nome disturbo-video) (valore ?v&fasce|linee-oriz))
  =>
  (assert (nodo (nome tipo-disturbo-video) (valore interferenza) (nodo-padre ?p1) (descrizione "E' possibile che il problema sia causato da un interferenza.")))
)

(defrule display-rotto
  ?p1 <- (nodo (nome disturbo-video) (valore  ?v&fasce|schermo-nero|linee-oriz))
  =>
  (assert (nodo (nome tipo-disturbo-video) (valore display-rotto) (nodo-padre ?p1) (descrizione "E' possibile che qualche componente del display sia guasta.")))
)

(defrule momento-problema-avvio
  ?p1 <- (nodo (nome fasce-bios) (valore si))
  =>
  (assert (nodo (nome momento-manifestazione-problema) (valore avvio) (nodo-padre ?p1) (descrizione "Il problema si manifesta sin dall'avvio del dispositivo")))
)

;; ****** REGOLE PER CREAZIONE PROFILO UTENTE E DISPOSITIVO ********

(defrule dispositivo-portatile
  ?p1 <- (nodo (nome tipo-dispositivo) (valore pc-portatile) (certezza ?c1))
  =>
  ;;(assert (nodo (nome possiede-batteria) (valore si) (nodo-padre ?p1) (descrizione "Il dispositivo possiede una batteria.")))
  (assert (nodo (nome cavi-display-accessibili) (valore no) (certezza (* 1.0 ?c1)) (nodo-padre ?p1) (descrizione "I cavi che collegano il dispositivo al display non sono accessibili.")))
  (assert (nodo (nome alimentatore-caricabatterie) (valore si) (certezza (* 1.0 ?c1)) (nodo-padre ?p1) (descrizione "Il dispositivo possiede un alimentatore caricabatterie esterno.")))
  (assert (nodo (nome interruttore-alimentatore) (valore acceso) (certezza (* 1.0 ?c1)) (nodo-padre ?p1) (descrizione "Essendo un dispositivo portatile, l'alimentatore non possiede un tasto di accensione esterno.")))
)

(defrule dispositivo-fisso
  ?p1 <- (nodo (nome tipo-dispositivo) (valore pc-desktop) (certezza ?c1))
  =>
  (assert (nodo (nome ha-batteria) (valore no) (certezza (* 1.0 ?c1)) (nodo-padre ?p1) (descrizione "Il dispositivo, essendo un pc fisso, non possiede una batteria di alimentazione.")))
  (assert (nodo (nome cavi-display-accessibili) (certezza (* 1.0 ?c1)) (valore si) (nodo-padre ?p1) (descrizione "I cavi che collegano il dispositivo al display sono accessibili.")))
  (assert (nodo (nome alimentatore-caricabatterie) (valore no) (certezza (* 1.0 ?c1)) (nodo-padre ?p1) (descrizione "Il dispositivo non possiede un alimentatore caricabatterie esterno.")))
)

(defrule portatile-ha-batteria
  ?p1 <- (nodo (nome tipo-dispositivo) (valore pc-portatile) (certezza ?crt1))
  ?p2 <- (nodo (nome ha-batteria) (valore sconosciuto) (certezza ?crt2))
  =>
  (bind ?crt-ha-batteria (calcola-certezza 0.8 ?crt1 ?crt2))
  (bind ?crt-non-ha-batteria (calcola-certezza 0.3 ?crt1 ?crt2))
  (assert (nodo (nome ha-batteria) (valore si) (certezza ?crt-ha-batteria) (nodo-padre ?p1 ?p2) (descrizione "Essendo il dispositivo un portatile, e' probabile che la batteria sia inserita.")))
  (assert (nodo (nome ha-batteria) (valore no) (certezza ?crt-non-ha-batteria) (nodo-padre ?p1 ?p2) (descrizione "Essendo il dispositivo un portatile, c'e' una piccola probabilita' che la batteria non sia inserita.")))
)

(defrule utente-inesperto
?p1 <- (nodo (nome esperienza-utente) (valore utente-inesperto))
=>
(assert (nodo (nome problema-principale) (valore analisi-guidata) (nodo-padre ?p1)))
)

(defrule garanzia-2-anni
  ?p1 <- (nodo (nome anni-dispositivo) (valore meno-2-anni) (certezza ?crt1))
  ?p2 <- (nodo (nome garanzia) (valore sconosciuto) (certezza ?crt2))
  =>
  (bind ?crt (calcola-certezza 0.8 ?crt1 ?crt2))
  (assert (nodo (nome garanzia) (valore si) (certezza ?crt) (nodo-padre ?p1 ?p2)))
)

(defrule garanzia-5-anni
  ?p1 <- (nodo (nome anni-dispositivo) (valore meno-5-anni) (certezza ?crt1))
  ?p2 <- (nodo (nome garanzia) (valore sconosciuto) (certezza ?crt2))
  =>
  (bind ?crt (calcola-certezza 0.4 ?crt1 ?crt2))
  (assert (nodo (nome garanzia) (valore si) (certezza ?crt) (nodo-padre ?p1 ?p2)))
)

(defrule garanzia-anni-sconosciuti
  ?p1 <- (nodo (nome anni-dispositivo) (valore sconosciuto) (certezza ?crt1))
  ?p2 <- (nodo (nome garanzia) (valore sconosciuto) (certezza ?crt2))
  =>
  (bind ?crt (calcola-certezza 0.6 ?crt1 ?crt2))
  (assert (nodo (nome garanzia) (valore si) (certezza ?crt) (nodo-padre ?p1 ?p2)))
)

(defrule garanzia-10-anni-piu
  ?p1 <- (nodo (nome anni-dispositivo) (valore  ?val&meno-10-anni|piu-10-anni) (certezza ?crt1))
  ?p2 <- (nodo (nome garanzia) (valore sconosciuto) (certezza ?crt2))
  =>
  (bind ?crt (calcola-certezza 1.0 ?crt1 ?crt2))
  (assert (nodo (nome garanzia) (valore no) (certezza ?crt) (nodo-padre ?p1 ?p2)))
)

;;*********************************************************

;;REGOLE FASE 2 *******************************************

(defrule chiedi-accensione
  =>
  (assert (nodo (nome chiedi) (valore stato-accensione)))
)


(defrule chiedi-alim-funzionante-ut-esperto
  ?p1 <- (nodo (nome esperienza-utente) (valore utente-esperto))
  ?p2 <- (nodo (nome stato-accensione) (valore fallito))
  =>
  (assert (nodo (nome chiedi) (valore alimentatore-funzionante-ut-esperto) (nodo-padre ?p1 ?p2)))
)

(defrule chiedi-alim-funzionante-ut-inesperto
  ?p1 <- (nodo (nome esperienza-utente) (valore utente-inesperto))
  ?p2 <- (nodo (nome stato-accensione) (valore fallito))
  =>
  (assert (nodo (nome chiedi) (valore alimentatore-funzionante-ut-inesperto) (nodo-padre ?p1 ?p2)))
)

(defrule alim-funzionante
  ;;generalizza fatto in base al profilo utente
  (or
    ?p1 <- (nodo (nome alimentatore-funzionante-ut-inesperto) (valore ?val) (certezza ?c))
    ?p1 <- (nodo (nome alimentatore-funzionante-ut-esperto) (valore ?val) (certezza ?c))
  )
  =>
  (assert (nodo (nome alimentatore-funzionante) (valore ?val) (certezza ?c) (nodo-padre ?p1)))
  )

  (defrule alim-funzionante-si
    ?p1 <- (nodo (nome alimentatore-funzionante) (valore si) (certezza ?crt1))
    ?p2 <- (nodo (nome stato-accensione) (valore fallito) (certezza ?crt2) (nodo-padre $?pdr))
    =>
    (bind ?crt-schermo-nero (calcola-certezza 0.9 ?crt1 ?crt2))
    (modify ?p2 (valore ok) (nodo-padre ?pdr ?p1) (certezza ?crt-schermo-nero))
    (assert (nodo (nome schermo-nero) (valore si) (certezza ?crt-schermo-nero) (nodo-padre ?p1 ?p2)))

    (bind ?crt-tipo-problema (calcola-certezza 0.5 ?crt1 ?crt2))
    ;(assert (nodo (nome tipologia-problema) (valore scheda-madre) (certezza ?crt-tipo-problema) (descrizione "Il problema potrebbe essere causato da un corto circuito sulla scheda madre.") (nodo-padre ?p1 ?p2)))
    ;(assert (nodo (nome tipologia-problema) (valore scheda-video) (certezza ?crt-tipo-problema) (descrizione "Il problema potrebbe essere causato da un guasto della scheda video.") (nodo-padre ?p1 ?p2)))
    ;(assert (nodo (nome tipologia-problema) (valore cavi-video) (certezza ?crt-tipo-problema) (descrizione "Il problema potrebbe essere causato da un guasto dei cavi che collegano la scheda video al display.") (nodo-padre ?p1 ?p2)))
  )

(defrule alim-funzionante-no
  ;?p1 <- (nodo (nome tipologia-problema) (valore alimentazione) (certezza ?crt1))
  ?p1 <- (nodo (nome alimentatore-funzionante) (valore no) (certezza ?crt1))
  ?p2 <- (nodo (nome stato-accensione) (valore fallito) (certezza ?crt2))
  =>
  ;; calcola valori di certezza delle diagnosi...
  (bind ?crt-guasto-alim (calcola-certezza 0.4 ?crt1 ?crt2))
  (assert (nodo (nome diagnosi) (valore alimentatore-guasto) (certezza ?crt-guasto-alim) (nodo-padre ?p1 ?p2)))
  (bind ?crt-alim-non-collegata (calcola-certezza 0.2 ?crt1 ?crt2))
  (assert (nodo (nome diagnosi) (valore alimentazione-disconnessa) (certezza ?crt-alim-non-collegata) (nodo-padre ?p1 ?p2)))
  (bind ?crt-guasto-scheda-madre (calcola-certezza 0.1 ?crt1 ?crt2))
  (assert (nodo (nome diagnosi) (valore scheda-madre-guasta) (certezza ?crt-guasto-scheda-madre) (nodo-padre ?p1 ?p2)))
  ;; chiedi la prossima domanda...
  (assert (nodo (nome chiedi) (valore alimentazione-collegata) (nodo-padre ?p1)))
)

(defrule alim-collegata-si
  ?p1 <- (nodo (nome alimentazione-collegata) (valore si) (certezza ?crt1))
  =>
  (assert (nodo (nome diagnosi) (valore alimentatore-guasto) (certezza (* 0.6 ?crt1)) (nodo-padre ?p1)))
  (assert (nodo (nome diagnosi) (valore scheda-madre-guasta) (certezza (* 0.3 ?crt1)) (nodo-padre ?p1)))
  (assert (nodo (nome diagnosi) (valore alimentazione-disconnessa) (certezza (* -1.0 ?crt1)) (nodo-padre ?p1)))
)

(defrule DIAGNOSI-alim-collegata-no
  ?p1 <- (nodo (nome stato-accensione) (valore fallito) (certezza ?crt1))
  ?p2 <- (nodo (nome alimentazione-collegata) (valore no) (certezza ?crt2))
  ?p3 <- (nodo (nome alimentatore-funzionante) (valore no) (certezza ?crt3))
  =>
  (bind ?crt (calcola-certezza 0.9 ?crt1 ?crt2 ?crt3))
  (assert (nodo (nome diagnosi) (valore alimentazione-disconnessa)(nodo-padre ?p1 ?p2 ?p3) (certezza ?crt)))
)

(defrule spia-alimentatore
    ?p1 <- (nodo (nome ?att&spia-alimentatore-pcportatile|spia-alimentatore-pcdesktop) (valore ?v) (certezza ?c1))
    =>
    (assert (nodo (nome spia-alimentatore) (valore ?v) (nodo-padre ?p1) (certezza ?c1)))
)

(defrule spia-alimentatore-sconosciuta
  ?p1 <- (nodo (nome spia-alimentatore) (valore sconosciuto) (certezza ?crt1))
  =>
  (assert (nodo (nome spia-alimentatore) (valore accesa) (certezza (* 0.5 ?crt1)) (nodo-padre ?p1)))
  (assert (nodo (nome spia-alimentatore) (valore spenta) (certezza (* 0.5 ?crt1)) (nodo-padre ?p1)))
)

;; Per dispositivi laptop ...

(defrule chiedi-batteria-difettosa
  ;?p1 <- (nodo (nome tipologia-problema) (valore alimentazione) (certezza ?crt1))
  ?p1 <- (nodo (nome alimentatore-funzionante) (valore no) (certezza ?crt1))
  ;; (ha-batteria si) dev'essere una risposta fornita dall'utente
  ?p2 <- (nodo (nome ha-batteria) (valore si) (certezza ?crt2) (tipo info-utente))
  ?p3 <- (nodo (nome alimentazione-collegata) (valore si) (certezza ?crt3))
  ?p4 <- (nodo (nome stato-accensione) (valore fallito) (certezza ?crt4))
  =>
  (assert (nodo (nome chiedi) (valore batteria-difettosa) (nodo-padre ?p1 ?p2 ?p3)))
)

(defrule inferenza-batteria-difettosa
  ;?p1 <- (nodo (nome tipologia-problema) (valore alimentazione) (certezza ?crt1))
  ?p1 <- (nodo (nome alimentatore-funzionante) (valore no) (certezza ?crt1))
  ;; (ha-batteria si) dev'essere una risposta dedotta dal sistema
  ?p2 <- (nodo (nome ha-batteria) (valore si) (certezza ?crt2) (tipo ?t&~info-utente))
  ?p3 <- (nodo (nome alimentazione-collegata) (valore si) (certezza ?crt3))
  ?p4 <- (nodo (nome stato-accensione) (valore fallito) (certezza ?crt4))
  =>
  (bind ?crt-batteria (calcola-certezza 0.6 ?crt1 ?crt2 ?crt3 ?crt4))
  (assert (nodo (nome batteria-difettosa) (valore si)(certezza ?crt-batteria)(nodo-padre ?p1 ?p2 ?p3 ?p4)))
)

(defrule DIAGNOSI-batteria-difettosa-si
  ?p1 <- (nodo (nome batteria-difettosa) (valore si) (certezza ?crt1))
  =>
  (assert (nodo (nome diagnosi) (valore batteria-difettosa) (certezza (* 1.0 ?crt1)) (nodo-padre ?p1)))
)

(defrule chiedi-spia-alimentatore-pcportatile
  ?p1 <- (nodo (nome alimentatore-funzionante) (valore no) (certezza ?crt1))
  ?p2 <- (nodo (nome tipo-dispositivo) (valore pc-portatile) (certezza ?crt2))
  ?p3 <- (nodo (nome alimentazione-collegata) (valore si) (certezza ?crt3))
  ?p4 <- (nodo (nome stato-accensione) (valore fallito) (certezza ?crt4))
  ?p5 <- (nodo (nome batteria-difettosa) (valore no) (certezza ?crt5))
  =>
  (assert (nodo (nome chiedi) (valore spia-alimentatore-pcportatile) (nodo-padre ?p1 ?p2 ?p3 ?p4 ?p5)))
)



(defrule batteria-non-presente
?p1 <- (nodo (nome ha-batteria) (valore no) (certezza ?c1))
=>
(assert (nodo (nome batteria-difettosa) (valore no) (certezza (* 1.0 ?c1)) (nodo-padre ?p1) (descrizione "Il dispositivo non ha una batteria.")))
)

(defrule DIAGNOSI-spia-alimentatore-accesa
  ?p1 <- (nodo (nome alimentatore-funzionante) (valore no) (certezza ?crt1))
  ;?p2 <- (nodo (nome tipo-dispositivo) (valore pc-portatile) (certezza ?crt2))
  ?p2 <- (nodo (nome interruttore-alimentatore) (valore acceso) (certezza ?crt2))
  ?p3 <- (nodo (nome alimentazione-collegata) (valore si) (certezza ?crt3))
  ?p4 <- (nodo (nome stato-accensione) (valore fallito) (certezza ?crt4))
  ?p5 <- (nodo (nome batteria-difettosa) (valore no) (certezza ?crt5))
  ?p6 <- (nodo (nome spia-alimentatore) (valore accesa) (certezza ?crt6))
  =>
  (bind ?crt-alim-guasto (calcola-certezza -0.6 ?crt1 ?crt2 ?crt3 ?crt4 ?crt5 ?crt6))
  (bind ?crt-scheda-madre-guasta (calcola-certezza 0.7 ?crt1 ?crt2 ?crt3 ?crt4 ?crt5 ?crt6))
  (assert (nodo (nome diagnosi) (valore alimentatore-guasto)(nodo-padre ?p1 ?p2 ?p3 ?p4 ?p5 ?p6) (certezza ?crt-alim-guasto)))
  (assert (nodo (nome diagnosi) (valore scheda-madre-guasta)(nodo-padre ?p1 ?p2 ?p3 ?p4 ?p5 ?p6) (certezza ?crt-scheda-madre-guasta)))
)

(defrule DIAGNOSI-spia-alimentatore-spenta
  ?p1 <- (nodo (nome alimentatore-funzionante) (valore no) (certezza ?crt1))
  ?p2 <- (nodo (nome interruttore-alimentatore) (valore acceso) (certezza ?crt2))
  ?p3 <- (nodo (nome alimentazione-collegata) (valore si) (certezza ?crt3))
  ?p4 <- (nodo (nome stato-accensione) (valore fallito) (certezza ?crt4))
  ?p5 <- (nodo (nome batteria-difettosa) (valore no) (certezza ?crt5))
  ?p6 <- (nodo (nome spia-alimentatore) (valore spenta) (certezza ?crt6))
  =>
  (bind ?crt-alim-guasto (calcola-certezza 0.8 ?crt1 ?crt2 ?crt3 ?crt4 ?crt5 ?crt6))
  (bind ?crt-scheda-madre-guasta (calcola-certezza 0.2 ?crt1 ?crt2 ?crt3 ?crt4 ?crt5 ?crt6))
  (assert (nodo (nome diagnosi) (valore alimentatore-guasto)(nodo-padre ?p1 ?p2 ?p3 ?p4 ?p5 ?p6) (certezza ?crt-alim-guasto)))
  (assert (nodo (nome diagnosi) (valore scheda-madre-guasta)(nodo-padre ?p1 ?p2 ?p3 ?p4 ?p5 ?p6) (certezza ?crt-scheda-madre-guasta)))
)



;; Per dispositivi desktop ...

(defrule chiedi-spia-alimentatore-pcdesktop
  ?p1 <- (nodo (nome tipo-dispositivo) (valore pc-desktop) (certezza ?crt1))
  ?p2 <- (nodo (nome alimentazione-collegata) (valore si) (certezza ?crt2))
  ?p3 <- (nodo (nome alimentatore-funzionante) (valore no) (certezza ?crt3))
  ?p4 <- (nodo (nome stato-accensione) (valore fallito) (certezza ?crt4))
  ?p5 <- (nodo (nome interruttore-alimentatore) (valore acceso) (certezza ?crt5))
  =>
  (assert (nodo (nome chiedi) (valore spia-alimentatore-pcdesktop) (nodo-padre ?p1 ?p2 ?p3 ?p4 ?p5)))
)

(defrule chiedi-interruttore-alimentatore
  ?p1 <- (nodo (nome tipo-dispositivo) (valore pc-desktop) (certezza ?crt1))
  ?p2 <- (nodo (nome alimentazione-collegata) (valore si) (certezza ?crt2))
  ?p3 <- (nodo (nome alimentatore-funzionante) (valore no) (certezza ?crt3))
  ?p4 <- (nodo (nome stato-accensione) (valore fallito) (certezza ?crt4))
  =>
  (assert (nodo (nome chiedi) (valore interruttore-alimentatore) (nodo-padre ?p1 ?p2 ?p3)))
)

(defrule DIAGNOSI-interruttore-alimentatore-spento
  ?p1 <- (nodo (nome tipo-dispositivo) (valore pc-desktop) (certezza ?crt1))
  ?p2 <- (nodo (nome alimentazione-collegata) (valore si) (certezza ?crt2))
  ?p3 <- (nodo (nome alimentatore-funzionante) (valore no) (certezza ?crt3))
  ?p4 <- (nodo (nome stato-accensione) (valore fallito) (certezza ?crt4))
  ?p5 <- (nodo (nome interruttore-alimentatore) (valore spento) (certezza ?crt5))
  =>
  (bind ?crt-alim-spento (calcola-certezza 0.9 ?crt1 ?crt2 ?crt3 ?crt4 ?crt5))
  (assert (nodo (nome diagnosi) (valore alimentatore-spento)(nodo-padre ?p1 ?p2 ?p3 ?p4 ?p5) (certezza ?crt-alim-spento)))
)



;; DOMANDE ACCENSIONE E SO *****************************************************

(defrule chiedi-riavvio-forzato
  ?p1 <- (nodo (nome stato-accensione) (valore ok))
  =>
  (assert (nodo (nome chiedi) (valore riavvio-forzato) (nodo-padre ?p1)))
)





;;******************************************************************************




; (defrule deduci-SO-windows
;   ?p1 <- (nodo (nome tipo-dispositivo) (valore ?dispositivo&pc-desktop|pc-portatile))
;   =>
;   (assert (nodo (nome sistema-operativo) (valore windows) (tipo inferenza) (nodo-padre ?p1)))
; )
;
; (defrule deduci-SO-android
;   ?p1 <- (nodo (nome tipo-dispositivo) (valore ?dispositivo&tablet|smartphone))
;   =>
;   (assert (nodo (nome SO) (valore android) (tipo inferenza) (nodo-padre ?p1)))
; )








(defmodule ELENCO-DIAGNOSI (import MAIN ?ALL)(export ?ALL))

(defmodule ELENCO-DOMANDE(import MAIN ?ALL)(export ?ALL))



(defmodule SPIEGAZIONE(import MAIN ?ALL)(export ?ALL))

; SPIEGAZIONE DOMANDA
;*******************************************************************************

(deffunction SPIEGAZIONE::leggi-nodi($?p)
  (loop-for-count (?cnt1 1 (length ?p)) do
    (bind ?v (fact-slot-value (nth$ ?cnt1 ?p) nome))
    (assert (spiega ?v))
  )
)

(deffunction SPIEGAZIONE::stampa-spiegazione-domanda(?n-sp ?n ?dom ?risp)
  (printout t ?n-sp ". Alla domanda n." ?n ": " crlf ?dom crlf "l'utente ha risposto: "  ?risp crlf crlf)
)

(deffunction SPIEGAZIONE::stampa-spiegazione-attributo(?n-sp ?spiegazione)
  (printout t  ?n-sp ". Il sistema ha dedotto che:" crlf ?spiegazione crlf crlf)
)

; (defrule SPIEGAZIONE::debug
;   (declare (salience ?*highest-priority*))
;   =>
;   (printout t "DEBUG >> Modulo spiegazioni" crlf crlf)
; )

(defrule SPIEGAZIONE::init-spiegazione-domanda-non-generica
  ?target <- (nodo (nome spiegazione) (valore ?attr ))
  ?domanda <- (nodo (nome chiedi) (valore ?attr) (nodo-padre $?p))
  (domanda (attributo ?attr) (domanda-generica FALSE))
  =>
  (printout t "***** MOTIVAZIONI DOMANDA *****" crlf)
  (retract ?target)
  (leggi-nodi ?p)
  ;(bind ?answer (read))
  (assert (contatore-spiegazione 0))
)

(defrule SPIEGAZIONE::init-spiegazione-domanda-generica
  ?target <- (nodo (nome spiegazione) (valore ?attr ))
  ?domanda <- (nodo (nome chiedi) (valore ?attr) (nodo-padre $?p))
  (domanda (attributo ?attr) (domanda-generica TRUE))
  =>
  (printout t "***** MOTIVAZIONI DOMANDA *****" crlf crlf)
  (printout t "Questa e' una domanda generica necessaria per introdurre al sistema delle informazioni basilari sul dispositivo." crlf)
  (retract ?target)
)

(defrule SPIEGAZIONE::spiega-domanda
  ?s <- (spiega ?attr)
  ?d <- (domanda (attributo ?attr) (gia-chiesta TRUE) (num-domanda ?n-domanda) (risposta-selezionata ?n-risposta) (testo-domanda ?domanda) (descrizione-risposte $?risposte))
  ?c <- (contatore-spiegazione ?cont)
  =>
  (retract ?c)
  (assert (contatore-spiegazione (+ ?cont 1)))
  (bind ?risposta (nth$ ?n-risposta ?risposte))
  (bind ?n-spieg (+ ?cont 1))
  (stampa-spiegazione-domanda ?n-spieg ?n-domanda ?domanda ?risposta)
  (retract ?s)
)

(defrule SPIEGAZIONE::spiega-attributo
  ?s <- (spiega ?attr)
  ?a <- (nodo (nome ?attr) (nodo-padre $?p) (descrizione ?descr))
  ?c <- (contatore-spiegazione ?cont)
  (not (domanda (attributo ?attr) (gia-chiesta TRUE)))
  =>
  (retract ?c)
  (assert (contatore-spiegazione (+ ?cont 1)))
  (retract ?s)
  (bind ?n-spieg (+ ?cont 1))
  (stampa-spiegazione-attributo ?n-spieg ?descr)
  (leggi-nodi ?p)
)

(defrule SPIEGAZIONE::end-spiegazione
  (not (nodo (nome spiegazione)))
  (not (spiega ?s))
  =>
  (printout t "Premere 0 e INVIO per tornare alla normale esecuzione del programma." crlf)
  (bind ?answer (read))
)






  ;******************* MODULO DOMANDE GENERICHE **********************************

  (defmodule DOMANDE-GENERICHE (import MAIN deftemplate ?ALL)(import MAIN defglobal ?ALL) (import MAIN deffunction ?ALL)(export ?ALL))


      (defrule DOMANDE-GENERICHE::init
        (declare (salience ?*highest-priority*))
        =>
        (set-strategy random)
        (printout t "DEBUG >> DOMANDE-GENERICHE >> strategy set to random." crlf crlf)
      )

      (defrule DOMANDE-GENERICHE::end
        (declare (salience ?*lowest-priority*))
        (not (domanda (domanda-generica TRUE) (gia-chiesta FALSE)))
        =>
        (set-strategy depth)
        (printout t "DEBUG >> DOMANDE-GENERICHE >> strategy set to depth." crlf crlf)
      )


      (defrule DOMANDE-GENERICHE::chiedi-domanda-generica
        ;(declare (salience ?*low-priority*))
        ?ask <- (nodo (nome chiedi)(valore ?attr)(nodo-padre $?p))
        ?f <- (domanda (attributo ?attr) (testo-domanda ?domanda) (risposte-valide $?risposte) (descrizione-risposte $?descrizioni) (gia-chiesta FALSE)(domanda-generica TRUE))
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

      (defrule DOMANDE-GENERICHE::usa-risposta-utente-gen
        ?ask <- (nodo (nome chiedi)(valore ?attr)(nodo-padre $?p))
        ?f <- (domanda (attributo ?attr) (testo-domanda ?domanda) (risposte-valide $?risposte) (descrizione-risposte $?descrizioni) (gia-chiesta TRUE) (risposta-selezionata ?risp) (domanda-generica TRUE))
        (not (nodo (nome ?attr)))
        =>
        (assert (nodo (nome ?attr) (valore (nth$ ?risp ?risposte)) (descrizione (nth$ ?risp ?descrizioni)) (tipo info-utente) (nodo-padre ?ask)))
      )



      (defrule DOMANDE-GENERICHE::chiedi-tipo-dispositivo
        =>
        (assert (nodo (nome chiedi) (valore tipo-dispositivo)))
      )

      (defrule DOMANDE-GENERICHE::chiedi-esperienza-utente
        =>
        (assert (nodo (nome chiedi) (valore esperienza-utente)))
      )

      (defrule DOMANDE-GENERICHE::chiedi-problema-principale
        ?p1 <- (nodo (nome esperienza-utente) (valore utente-esperto))
        =>
        (assert (nodo (nome chiedi) (valore problema-principale) (nodo-padre ?p1)))
      )

      (defrule DOMANDE-GENERICHE::chiedi-anni-dispositivo
        =>
        (assert (nodo (nome chiedi) (valore anni-dispositivo)))
      )

      (defrule DOMANDE-GENERICHE::chiedi-garanzia
        ?p1 <- (nodo (nome anni-dispositivo) (valore ?val&meno-2-anni|meno-5-anni|sconosciuto))
        =>
        (assert (nodo (nome chiedi) (valore garanzia) (nodo-padre ?p1)))
      )

      (defrule DOMANDE-GENERICHE::chiedi-ha-batteria
        ?p1 <- (nodo (nome tipo-dispositivo) (valore pc-portatile))
        =>
        (assert (nodo (nome chiedi) (valore ha-batteria) (nodo-padre ?p1)))
      )



  ;*******************************************************************************
