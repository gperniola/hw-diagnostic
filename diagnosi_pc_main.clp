

(defglobal ?*highest-priority* = 1000)
(defglobal ?*high-priority* = 100)
(defglobal ?*low-priority* = -100)
(defglobal ?*lowest-priority* = -1000)

(defglobal ?*id* = 0)

(deffunction next-id ()
   (bind ?*id* (+ ?*id* 1))
   (return ?*id*)
)

;;******************
;;*    TEMPLATES   *
;;******************

  (deftemplate nodo
    (slot id-nodo (default-dynamic (next-id)))
    (slot attivo  (default TRUE))
    (slot nome    (type SYMBOL))
    (multislot valore  (type SYMBOL))
    (slot certezza (type FLOAT) (default 1.0))
    (slot sorgente-info (type SYMBOL) (default sistema))
    (multislot nodo-padre (type FACT-ADDRESS))
    (slot descrizione (type STRING))
  )

(deffacts examples
  (nodo (nome diagnosi) (valore A) (certezza 0.2))
  (nodo (nome diagnosi) (valore B) (certezza 0.8))
  (nodo (nome diagnosi) (valore C) (certezza 0.5))
  (nodo (nome diagnosi) (valore D) (certezza -0.3))
  (nodo (nome diagnosi) (valore E) (certezza 0.5))
  (nodo (nome diagnosi) (valore F) (certezza 0.8))
  )



  (deftemplate domanda
    (slot attributo     (type SYMBOL) (default ?NONE))
    (slot gia-chiesta   (default  FALSE))
    (slot stampata (default FALSE))
    (slot num-domanda (type INTEGER))
    (multislot risposte-valide (type SYMBOL) (default ?NONE))
    (slot risposta-selezionata (type INTEGER))
    (multislot testo-domanda (default ?NONE))
    ;(slot testo-domanda (type STRING) (default ?NONE))
    (multislot descrizione-risposte (type STRING) (default ?NONE))
    ;(slot domanda-generica (default FALSE))
  )

  (deftemplate diagnosi
    (slot attributo   (type SYMBOL))
    (slot titolo      (type STRING))
    (slot descrizione (type STRING))
    (slot stampata (default FALSE))
  )

  (deftemplate soluzione
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
      (printout t crlf "Selezionare un opzione per continuare:" crlf "1. Trova soluzioni al problema" crlf "2. Ritratta le risposte date" crlf "3. Riavvia programma" crlf "4. Termina programma" crlf crlf)
      (bind ?answer (read))
  )
  (if (eq ?answer 1) then (assert (fase 4-trova-soluzioni)))
  (if (eq ?answer 2) then (assert (fase ritrattazione)))
  (if (eq ?answer 3) then (reset) (run))
  (if (eq ?answer 4) then (halt))
)

(deffunction MAIN::ask-stop-program-2 ()
  (bind ?answer 0)
  (while (not (member ?answer (create$ 1 2)))
      (printout t crlf "Selezionare un opzione per continuare:" crlf "1. Riavvia programma" crlf "2. Termina programma" crlf crlf)
      (bind ?answer (read))
  )
  (if (eq ?answer 1) then (reset) (run))
  (if (eq ?answer 2) then (halt))
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
  (if (and (> ?cf1 0) (> ?cf2 0)) then (bind ?CF (- (+ ?cf1 ?cf2) (* ?cf1 ?cf2)))
      ;(printout t "[" ?cf1 " + " ?cf2 "] - [" ?cf1 " * " ?cf2 " ] = " ?CF crlf)
  )
  (if (and (< ?cf1 0) (< ?cf2 0)) then (bind ?CF (+ (+ ?cf1 ?cf2) (* ?cf1 ?cf2)))
      ;(printout t "[" ?cf1 " + " ?cf2 "] + [" ?cf1 " * " ?cf2 " ] = " ?CF crlf)
  )
  (if (< (* ?cf1 ?cf2) 0) then (bind ?CF (/ (+ ?cf1 ?cf2) (- 1 (min (abs ?cf1)(abs ?cf2)))))
      ;(printout t "[" ?cf1 " + " ?cf2 "] / [ 1 - minabs [" ?cf1 ", " ?cf2 " ] = " ?CF crlf)
  )
  (return ?CF)
)

(defrule MAIN::combina-certezza-nodi
  (declare (salience ?*highest-priority*))
  ?nodo1 <- (nodo (nome ?n) (valore ?v) (certezza ?c1) (attivo TRUE) (id-nodo ?i1))
  ?nodo2 <- (nodo (nome ?n) (valore ?v) (certezza ?c2) (attivo TRUE) (id-nodo ?i2))
  ;(not (nodo (nome ?n) (valore ?v) (nodo-padre $?pdr1 ?nodo&?nodo1|?nodo2 $?pdr2)))
  (test (neq ?nodo1 ?nodo2))
  =>
  ;(printout t "Combine: " ?nodo1 " - " ?nodo2  crlf)
  ;(bind ?h (read))
  ;(retract ?nodo1)
  (modify ?nodo1 (attivo FALSE))
  (modify ?nodo2 (attivo FALSE))
  (assert (nodo (nome ?n) (valore ?v) (certezza (combina-CF ?c1 ?c2)) (nodo-padre ?i1 ?i2)))
  ;(printout t "Combined in: " ?x1 ", " ?x2 " --> " ?y  crlf)
  ;(assert (nodo (nome ?n) (valore ?v) (certezza (combina-CF ?c1 ?c2)) (nodo-padre ?nodo1 ?nodo2)))
)

(defrule MAIN::rimuovi-padri-duplicati
  (declare (salience ?*highest-priority*))
  ?n <- (nodo (nodo-padre $?nodi1 ?elem $?nodi2 ?elem $?nodi3))
  =>
  (modify ?n (nodo-padre ?nodi1 ?elem ?nodi2 ?nodi3))
)

; (defrule MAIN::attiva-nodi-terminali
;   (declare (salience ?*superhigh-priority*))
;   (fase 2-analisi)
;   ?nodo <- (nodo (nome ?n) (valore ?v) (stato inattivo))
;   (not (nodo (nome ?n) (valore ?v) (nodo-padre $?x ?nodo $?y)))
;   =>
;   ;(printout t "Attiva: " ?nodo  crlf)
;   ;(bind ?h (read))
;   (bind ?y (modify ?nodo (stato attivo)))
;   ;(printout t "Attivato in: " ?y crlf)
; )

; (defrule MAIN::disattiva-nodi-diagnosi-terminali
;   (declare (salience ?*superhigh-priority*))
;   (fase 2-analisi)
;   ?nodo <- (nodo (nome ?n) (valore ?v) (stato attivo))
;   ?child <- (nodo (nome ?n) (valore ?v) (nodo-padre $?x ?nodo $?y))
;   =>
;   (printout t "Disattiva: " ?nodo " - child: " ?child crlf)
;   (bind ?h (read))
;   (modify ?nodo (stato inattivo))
; )


;***********EXAMPLE CF RULES ***********************
; (deffacts exfacts
;   (nodo (nome statox) (valore x) (certezza 0.6))
;   (nodo (nome statoy) (valore y) (certezza 0.8))
;   (nodo (nome statoz) (valore z) (certezza -0.6))
; )
; (defrule chiedi-x
;   ?p1 <- (nodo (nome statox) (valore x) (certezza ?crt1))
;   ?p2 <- (nodo (nome statoy) (valore y) (certezza ?crt2))
;   =>
;   (bind ?crt (calcola-certezza 0.7 ?crt1 ?crt2))
;   (assert (nodo (nome statoz) (valore z) (certezza ?crt) (nodo-padre ?p1 ?p2)))
; )
;***************************************************


;;******************
;;* CONTROL RULES  *
;;******************

(defrule MAIN::inizializzazione
  (declare (salience ?*highest-priority*))
  =>
  (load-facts "data/DOMANDE.DAT")
  (load-facts "data/DIAGNOSI.DAT")
  (load-facts "data/SOLUZIONI.DAT")
  (load "rules/modulo-diagnosi.clp")
  (load "rules/modulo-soluzione.clp")
  (load "rules/modulo-spiegazione.clp")
  (load "rules/modulo-ritrattazione.clp")
  (clear-window)
  (assert (contatore-domande 0))
  (assert (fase 1-profilazione))
)

(defrule fase-1-profilazione
  (declare (salience ?*highest-priority*))
  (fase 1-profilazione)
  =>
  (set-strategy random)
)

(defrule fase-2-analisi
  (declare (salience ?*highest-priority*))
  (fase 2-analisi)
  =>
  (set-strategy depth)
)

(defrule fase-3-stampa-diagnosi
  (declare (salience ?*highest-priority*))
  (fase 3-stampa-diagnosi)
  =>
  ;(printout t "***** DIAGNOSI DEL PROBLEMA *****" crlf crlf)
  (focus MODULO-DIAGNOSI)
)


(defrule fase-4-trova-soluzioni
  (declare (salience ?*highest-priority*))
  (fase 4-trova-soluzioni)
  ;?f <- (fase 3-stampa-diagnosi)
  =>
  ;(printout t "FASE SOLUZIONI" crlf)
  ;(retract ?f)
)

;; DA IMPLEMENTARE
(defrule fase-5-stampa-soluzioni
  (declare (salience ?*highest-priority*))
  (fase 5-stampa-soluzioni)
  =>
  ;(printout t "***** LISTA SOLUZIONI *****" crlf crlf)
  (focus MODULO-SOLUZIONE)
)
;;;;;;;;;;;;;;;;;;;

(defrule fase-ritrattazione
  (declare (salience ?*highest-priority*))
  ?f1 <- (fase ritrattazione)
;  ?f2 <- (fase ?v&~ritrattazione)
  =>
  ;(retract ?f1)
  ; (retract ?f2)
  ; (assert (fase 2-analisi))
  (focus MODULO-RITRATTAZIONE)
)

(defrule fase-spiegazione
  (declare (salience ?*highest-priority*))
  (fase spiegazione)
  =>
  (focus MODULO-SPIEGAZIONE)
)


(defrule MAIN::diagnosi-trovata
  (declare (salience ?*low-priority*))
  ?f <- (fase 2-analisi)
  (nodo (nome diagnosi) (valore ?attr-diagnosi) (certezza ?cer&:(> ?cer 0.95)))
  (diagnosi (attributo ?attr-diagnosi))
  (not (fase 3-stampa-diagnosi))
  (not (fase 4-trova-soluzioni))
  (not (fase 5-stampa-soluzioni))
  ;(not (ferma-programma))
  =>
  ;(printout t crlf "***** DIAGNOSI 0.95 *****" crlf crlf)
  ;(retract ?f)
  (assert (fase 3-stampa-diagnosi))
  ;(assert(ferma-programma))
)

(defrule MAIN::fine-domande
  (declare (salience ?*lowest-priority*))
  ?f <- (fase 2-analisi)
  (nodo (nome chiedi) (valore ?dom))
  (not (domanda (attributo ?dom) (gia-chiesta FALSE)))
  (not (fase 3-stampa-diagnosi))
  (not (fase 4-trova-soluzioni))
  (not (fase 5-stampa-soluzioni))
  ;(not (ferma-programma))
  =>
  ;(printout t crlf "***** FINE DOMANDE *****" crlf crlf)
  ;(retract ?f)
  (assert (fase 3-stampa-diagnosi))
)

; (defrule MAIN::ferma-esecuzione
;   (declare (salience ?*highest-priority*))
;   ?x <- (ferma-programma)
;   =>
;   (retract ?x)
;   (ask-stop-program)
; )
;
; (defrule MAIN::ferma-esecuzione-2
;   (declare (salience ?*highest-priority*))
;   ?x <- (ferma-programma-2)
;   =>
;   (retract ?x)
;   (ask-stop-program-2)
; )


; CHIEDI DOMANDA
;****************************************************************************


(defrule chiedi-domanda
  (declare (salience ?*low-priority*))
  (not (fase ?fase&3-stampa-diagnosi|4-trova-soluzioni|5-stampa-soluzioni))
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
    (assert (fase ritrattazione))
  else
    (if (= ?risposta 9) then
      (retract ?ask)
      (assert (nodo (nome chiedi)(valore ?attr)(nodo-padre ?p))) ;;NECESSARIO PER RIPROPORRE LA STESSA DOMANDA NEL CASO DI ANNULLAMENTO REVISIONE
      (assert (nodo (nome spiegazione) (valore ?attr)))
      (assert (fase spiegazione))
    else
      ;;(assert (nodo (nome ?attr) (valore (nth$ ?risposta ?risposte)) (descrizione (nth$ ?risposta ?descrizioni)) (tipo info-utente) (nodo-padre ?ask)))
      (modify ?f (gia-chiesta TRUE)(num-domanda ?j)(risposta-selezionata ?risposta))
      ;;(retract ?ask)
      (retract ?cont-dom)
      (assert (contatore-domande ?j))
    )
  )
)

(defrule usa-risposta-utente-memorizzata
  ?ask <- (nodo (nome chiedi)(valore ?attr)(nodo-padre $?p) (id-nodo ?id-ask))
  ?f <- (domanda (attributo ?attr) (testo-domanda ?domanda) (risposte-valide $?risposte) (descrizione-risposte $?descrizioni) (gia-chiesta TRUE) (risposta-selezionata ?risp))
  (not (nodo (nome ?attr)))
  =>
  (assert (nodo (nome ?attr) (valore (nth$ ?risp ?risposte)) (descrizione (nth$ ?risp ?descrizioni)) (sorgente-info utente) (nodo-padre ?id-ask)))
)





;;******************************************************************************
;;*    REGOLE PER CHIDERE DOMANDE ALL'UTENTE                                   *
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






;; REGOLE PER INFERENZA ********************************************************

; (defrule cavi-display-portatile
;   ?p1 <- (nodo (nome tipo-dispositivo) (valore pc-portatile))
;   =>
;   (assert (nodo (nome muovere-cavi-display) (valore interni) (nodo-padre ?p1 )))
;   (assert (nodo (nome cavi-display) (valore interni) (nodo-padre ?p1 )))
; )





;; *****************************************************************************
;; ************ REGOLE PER FASE 1: PROFILAZIONE UTENTE E DISPOSITIVO ***********
;; *****************************************************************************

;; ***** FASE 1: DOMANDE DA PORRE ALL'UTENTE

(defrule chiedi-tipo-dispositivo
  (or (fase 1-profilazione) (fase 2-analisi))
  (not (nodo (nome chiedi) (valore tipo-dispositivo)))
  =>
  (assert (nodo (nome chiedi) (valore tipo-dispositivo)))
)

(defrule chiedi-esperienza-utente
  (or (fase 1-profilazione) (fase 2-analisi))
  (not (nodo (nome chiedi) (valore esperienza-utente)))
  =>
  (assert (nodo (nome chiedi) (valore esperienza-utente)))
)

(defrule chiedi-problema-principale
  (or (fase 1-profilazione) (fase 2-analisi))
  (not (nodo (nome chiedi) (valore problema-principale)))
  ?p1 <- (nodo (nome esperienza-utente) (valore utente-esperto) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome chiedi) (valore problema-principale) (nodo-padre ?id-p1)))
)

(defrule chiedi-anni-dispositivo
  (or (fase 1-profilazione) (fase 2-analisi))
  (not (nodo (nome chiedi) (valore anni-dispositivo)))
  =>
  (assert (nodo (nome chiedi) (valore anni-dispositivo)))
)

(defrule chiedi-garanzia
  (or (fase 1-profilazione) (fase 2-analisi))
  (not (nodo (nome chiedi) (valore garanzia)))
  ?p1 <- (nodo (nome anni-dispositivo) (valore ?val&meno-2-anni|2-5-anni|sconosciuto) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome chiedi) (valore garanzia) (nodo-padre ?id-p1)))
)

(defrule chiedi-ha-batteria
  (or (fase 1-profilazione) (fase 2-analisi))
  (not  (nodo (nome chiedi) (valore ha-batteria)))
  ?p1 <- (nodo (nome tipo-dispositivo) (valore pc-portatile) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome chiedi) (valore ha-batteria) (nodo-padre ?id-p1)))
)

;; ***** FASE 1: REGOLA PER PASSAGGIO ALLA PROSSIMA FASE

(defrule passa-alla-fase-2
  (declare (salience ?*lowest-priority*))
  ?f <- (fase 1-profilazione)
  =>
  ;(retract ?f)
  (assert (fase 2-analisi))
)

;;*********************************************************

;; ***** FASE 2: DEDUZIONI DEL SISTEMA

(defrule dispositivo-portatile
  (fase 2-analisi)
  ?p1 <- (nodo (nome tipo-dispositivo) (valore pc-portatile) (certezza ?c1) (id-nodo ?id-p1))
  =>
  ;;(assert (nodo (nome possiede-batteria) (valore si) (nodo-padre ?p1) (descrizione "Il dispositivo possiede una batteria.")))
  (assert (nodo (nome cavi-display-accessibili) (valore no) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1) (descrizione "I cavi che collegano il dispositivo al display non sono accessibili.")))
  (assert (nodo (nome alimentatore-caricabatterie) (valore si) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1) (descrizione "Il dispositivo possiede un alimentatore caricabatterie esterno.")))
  (assert (nodo (nome interruttore-alimentatore) (valore acceso) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1) (descrizione "Essendo un dispositivo portatile, l'alimentatore non possiede un tasto di accensione esterno.")))
)

(defrule dispositivo-fisso
  (fase 2-analisi)
  ?p1 <- (nodo (nome tipo-dispositivo) (valore pc-desktop) (certezza ?c1) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome ha-batteria) (valore no) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1) (descrizione "Il dispositivo, essendo un pc fisso, non possiede una batteria di alimentazione.")))
  (assert (nodo (nome cavi-display-accessibili) (valore si) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1) (descrizione "I cavi che collegano il dispositivo al display sono accessibili.")))
  (assert (nodo (nome alimentatore-caricabatterie) (valore no) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1) (descrizione "Il dispositivo non possiede un alimentatore caricabatterie esterno.")))
)

(defrule portatile-ha-batteria
  (fase 2-analisi)
  ?p1 <- (nodo (nome tipo-dispositivo) (valore pc-portatile) (certezza ?crt1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome ha-batteria) (valore sconosciuto) (certezza ?crt2) (id-nodo ?id-p2))
  =>
  (bind ?crt-ha-batteria (calcola-certezza 0.8 ?crt1 ?crt2))
  (bind ?crt-non-ha-batteria (calcola-certezza 0.3 ?crt1 ?crt2))
  (assert (nodo (nome ha-batteria) (valore si) (certezza ?crt-ha-batteria) (nodo-padre ?id-p1 ?id-p2) (descrizione "Essendo il dispositivo un portatile, e' probabile che la batteria sia inserita.")))
  (assert (nodo (nome ha-batteria) (valore no) (certezza ?crt-non-ha-batteria) (nodo-padre ?id-p1 ?id-p2) (descrizione "Essendo il dispositivo un portatile, c'e' una piccola probabilita' che la batteria non sia inserita.")))
)

(defrule utente-inesperto
  (fase 2-analisi)
  ?p1 <- (nodo (nome esperienza-utente) (valore utente-inesperto) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome problema-principale) (valore analisi-guidata) (nodo-padre ?id-p1)))
)

(defrule eta-dispositivo-sconosciuta
  (fase 2-analisi)
  ?p1 <- (nodo (nome anni-dispositivo) (valore sconosciuto) (sorgente-info utente) (certezza ?crt1) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome anni-dispositivo) (valore 2-5-anni)    (certezza (* 0.75 ?crt1)) (nodo-padre ?id-p1)))
  (assert (nodo (nome anni-dispositivo) (valore meno-2-anni) (certezza (* 0.25 ?crt1)) (nodo-padre ?id-p1)))
  (assert (nodo (nome anni-dispositivo) (valore 5-10-anni)   (certezza (* 0.35 ?crt1)) (nodo-padre ?id-p1)))
  (assert (nodo (nome anni-dispositivo) (valore piu-10-anni) (certezza (* 0.10 ?crt1)) (nodo-padre ?id-p1)))
)

(defrule garanzia-2-anni
  (fase 2-analisi)
  ?p1 <- (nodo (nome anni-dispositivo) (valore meno-2-anni) (sorgente-info utente) (certezza ?crt1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome garanzia) (valore sconosciuto) (certezza ?crt2) (id-nodo ?id-p2))
  =>
  (bind ?crt (calcola-certezza 0.8 ?crt1 ?crt2))
  (assert (nodo (nome garanzia) (valore si) (certezza ?crt) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule garanzia-5-anni
  (fase 2-analisi)
  ?p1 <- (nodo (nome anni-dispositivo) (valore 2-5-anni) (sorgente-info utente) (certezza ?crt1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome garanzia) (valore sconosciuto) (certezza ?crt2) (id-nodo ?id-p2))
  =>
  (bind ?crt (calcola-certezza 0.4 ?crt1 ?crt2))
  (assert (nodo (nome garanzia) (valore si) (certezza ?crt) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule garanzia-anni-sconosciuti
  (fase 2-analisi)
  ?p1 <- (nodo (nome anni-dispositivo) (valore sconosciuto) (sorgente-info utente) (certezza ?crt1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome garanzia) (valore sconosciuto) (certezza ?crt2) (id-nodo ?id-p2))
  =>
  (bind ?crt-garanzia (calcola-certezza 0.5 ?crt1 ?crt2))
  (assert (nodo (nome garanzia) (valore si) (certezza ?crt-garanzia) (nodo-padre ?id-p1 ?id-p2)))

)

(defrule garanzia-10-anni-piu
  (fase 2-analisi)
  ?p1 <- (nodo (nome anni-dispositivo) (valore  ?val&5-10-anni|piu-10-anni) (sorgente-info utente) (certezza ?crt1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome garanzia) (valore sconosciuto) (certezza ?crt2) (id-nodo ?id-p2))
  =>
  (bind ?crt (calcola-certezza 1.0 ?crt1 ?crt2))
  (assert (nodo (nome garanzia) (valore no) (certezza ?crt) (nodo-padre ?id-p1 ?id-p2)))
)



;;REGOLE FASE 2 *******************************************

(defrule controllo-accensione
  (fase 2-analisi)
  ?p1 <- (nodo (nome problema-principale) (valore ?v&analisi-guidata|accensione-SO) (certezza ?crt1) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome chiedi) (valore controllo-accensione)))
)

(defrule problema-video-accensione-funzionante
  (fase 2-analisi)
  ?p1 <- (nodo (nome problema-principale) (valore video) (certezza ?crt1) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome stato-accensione) (valore funzionante) (certezza (* 1.0 ?crt1)) (nodo-padre ?id-p1 ) (descrizione "Il dispositivo ha un problema video, quindi il dispositivo dovrebbe accendersi.")))
)

(defrule controllo-accensione-ut-inesperto
  (fase 2-analisi)
  ?p1 <- (nodo (nome esperienza-utente) (valore utente-inesperto) (certezza ?crt1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome controllo-accensione) (valore possibile-non-funzionante) (certezza ?crt2) (id-nodo ?id-p2))
  =>
  (assert (nodo (nome chiedi) (valore controllo-accensione-ut-inesperto) (nodo-padre ?id-p1 ?id-p2)))
)


(defrule stato-accensione-ut-esperto
  (fase 2-analisi)
  ?p1 <- (nodo (nome esperienza-utente) (valore utente-esperto) (certezza ?crt1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome controllo-accensione) (valore possibile-non-funzionante) (certezza ?crt2) (id-nodo ?id-p2))
  =>
  (bind ?crt-accensione (calcola-certezza 1 ?crt1 ?crt2)) ;;livello CRT utente-esperto settato a 1
  (assert (nodo (nome stato-accensione) (valore non-funzionante) (certezza ?crt-accensione) (nodo-padre ?id-p1 ?id-p2) (descrizione "Il dispositivo non si accende, c'e' un problema con il circuito di alimentazione.")))
)

(defrule stato-accensione-funzionante
  (fase 2-analisi)
  ?p1 <- (nodo (nome controllo-accensione) (valore funzionante) (certezza ?crt1) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome stato-accensione) (valore funzionante) (certezza (* 1.0 ?crt1)) (nodo-padre ?id-p1 ) (descrizione "Il dispositivo si accende, il circuito di alimentazione sembra funzionare.")))
)

(defrule stato-accensione-ut-inesperto
  (fase 2-analisi)
  ?p1 <- (nodo (nome controllo-accensione) (valore possibile-non-funzionante) (certezza ?crt1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome controllo-accensione-ut-inesperto) (valore non-funzionante) (certezza ?crt2) (id-nodo ?id-p2))
  =>
  (bind ?crt-accensione (calcola-certezza 1 ?crt1 ?crt2)) ;;livello CRT utente-inesperto settato a 1
  (assert (nodo (nome stato-accensione) (valore non-funzionante) (certezza ?crt-accensione) (nodo-padre ?id-p1 ?id-p2) (descrizione "Il dispositivo non si accende, c'e' un problema con il circuito di alimentazione.")))
  )

(defrule stato-accensione-funzionante-con-schermo-nero
  (fase 2-analisi)
  ?p1 <- (nodo (nome controllo-accensione) (valore possibile-non-funzionante) (certezza ?crt1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome controllo-accensione-ut-inesperto) (valore funzionante) (certezza ?crt2) (id-nodo ?id-p2))
  =>
  (bind ?crt-schermo-nero (calcola-certezza 0.9 ?crt1 ?crt2))
  (assert (nodo (nome disturbo-video) (valore schermo-nero) (certezza ?crt-schermo-nero) (nodo-padre ?id-p1 ?id-p2) (descrizione "Il dispositivo si accende ma lo schermo e nero e non sembra dare segni di vita.")))
  (bind ?crt-accensione (calcola-certezza 1 ?crt1 ?crt2)) ;;livello CRT utente-inesperto settato a 1
  (assert (nodo (nome stato-accensione) (valore funzionante) (certezza ?crt-accensione) (nodo-padre ?id-p1 ?id-p2) (descrizione "Il dispositivo si accende, il circuito di alimentazione sembra funzionare.")))

  ;(bind ?crt-tipo-problema (calcola-certezza 0.5 ?crt1 ?crt2))
    ;(assert (nodo (nome tipologia-problema) (valore scheda-madre) (certezza ?crt-tipo-problema) (descrizione "Il problema potrebbe essere causato da un corto circuito sulla scheda madre.") (nodo-padre ?p1 ?p2)))
    ;(assert (nodo (nome tipologia-problema) (valore scheda-video) (certezza ?crt-tipo-problema) (descrizione "Il problema potrebbe essere causato da un guasto della scheda video.") (nodo-padre ?p1 ?p2)))
    ;(assert (nodo (nome tipologia-problema) (valore cavi-video) (certezza ?crt-tipo-problema) (descrizione "Il problema potrebbe essere causato da un guasto dei cavi che collegano la scheda video al display.") (nodo-padre ?p1 ?p2)))
  )

(defrule alim-funzionante-no
  (fase 2-analisi)
  ;?p1 <- (nodo (nome tipologia-problema) (valore alimentazione) (certezza ?crt1))
  ;?p1 <- (nodo (nome alimentatore-funzionante) (valore no) (certezza ?crt1))
  ?p1 <- (nodo (nome stato-accensione) (valore non-funzionante) (certezza ?crt1) (id-nodo ?id-p1))
  =>
  (printout t "correct rule" crlf)
  ;; calcola valori di certezza delle diagnosi...
  (bind ?crt-guasto-alim (calcola-certezza 0.4 ?crt1))
  (assert (nodo (nome diagnosi) (valore alimentatore-guasto) (certezza ?crt-guasto-alim) (nodo-padre ?id-p1)))
  (bind ?crt-alim-non-collegata (calcola-certezza 0.2 ?crt1))
  (assert (nodo (nome diagnosi) (valore alimentazione-disconnessa) (certezza ?crt-alim-non-collegata) (nodo-padre ?id-p1)))
  (bind ?crt-guasto-scheda-madre (calcola-certezza 0.1 ?crt1))
  (assert (nodo (nome diagnosi) (valore scheda-madre-guasta) (certezza ?crt-guasto-scheda-madre) (nodo-padre ?id-p1)))
  ;; chiedi la prossima domanda...
  (assert (nodo (nome chiedi) (valore alimentazione-collegata) (nodo-padre ?id-p1)))
)

(defrule alim-collegata-si
  (fase 2-analisi)
  ?p1 <- (nodo (nome alimentazione-collegata) (valore si) (certezza ?crt1) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome diagnosi) (valore alimentatore-guasto) (certezza (* 0.6 ?crt1)) (nodo-padre ?id-p1)))
  (assert (nodo (nome diagnosi) (valore scheda-madre-guasta) (certezza (* 0.3 ?crt1)) (nodo-padre ?id-p1)))
  (assert (nodo (nome diagnosi) (valore alimentazione-disconnessa) (certezza (* -1.0 ?crt1)) (nodo-padre ?id-p1)))
)

(defrule DIAGNOSI-alim-collegata-no
  (fase 2-analisi)
  ?p1 <- (nodo (nome stato-accensione) (valore non-funzionante) (certezza ?crt1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome alimentazione-collegata) (valore no) (certezza ?crt2) (id-nodo ?id-p2))
  ;?p3 <- (nodo (nome alimentatore-funzionante) (valore no) (certezza ?crt3))
  =>
  (bind ?crt (calcola-certezza 1.0 ?crt1 ?crt2))
  (assert (nodo (nome diagnosi) (valore alimentazione-disconnessa)(nodo-padre ?id-p1 ?id-p2) (certezza ?crt)))
  (bind ?crt-diagnosi-escluse (calcola-certezza -1 ?crt1 ?crt2))
  (assert (nodo (nome diagnosi) (valore alimentatore-guasto) (certezza ?crt-diagnosi-escluse ) (nodo-padre ?id-p1 ?id-p2)))
  (assert (nodo (nome diagnosi) (valore scheda-madre-guasta) (certezza ?crt-diagnosi-escluse ) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule spia-alimentatore
  ; unifica la risposta da pc-desktop o laptop sulla spia di accensione
  (fase 2-analisi)
  ?p1 <- (nodo (nome ?att&spia-alimentatore-pcportatile|spia-alimentatore-pcdesktop) (valore ?v) (certezza ?c1) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome spia-alimentatore) (valore ?v) (nodo-padre ?id-p1) (certezza ?c1)))
)

(defrule spia-alimentatore-sconosciuta
  (fase 2-analisi)
  ?p1 <- (nodo (nome spia-alimentatore) (valore sconosciuto) (certezza ?crt1) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome spia-alimentatore) (valore accesa) (certezza (* 0.5 ?crt1)) (nodo-padre ?id-p1)))
  (assert (nodo (nome spia-alimentatore) (valore spenta) (certezza (* 0.5 ?crt1)) (nodo-padre ?id-p1)))
)

;; Per dispositivi laptop ...

(defrule chiedi-batteria-difettosa
  (fase 2-analisi)
  ;?p1 <- (nodo (nome tipologia-problema) (valore alimentazione) (certezza ?crt1))
  ;?p1 <- (nodo (nome alimentatore-funzionante) (valore no) (certezza ?crt1))
  ;; (ha-batteria si) dev'essere una risposta fornita dall'utente
  ?p1 <- (nodo (nome ha-batteria) (valore si) (certezza ?crt1) (sorgente-info utente) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome alimentazione-collegata) (valore si) (certezza ?crt2) (id-nodo ?id-p2))
  ?p3 <- (nodo (nome stato-accensione) (valore  non-funzionante) (certezza ?crt3) (id-nodo ?id-p3))
  =>
  (assert (nodo (nome chiedi) (valore batteria-difettosa) (nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule inferenza-batteria-difettosa
  (fase 2-analisi)
  ;?p1 <- (nodo (nome tipologia-problema) (valore alimentazione) (certezza ?crt1))
  ;?p1 <- (nodo (nome alimentatore-funzionante) (valore no) (certezza ?crt1))
  ;; (ha-batteria si) dev'essere una risposta dedotta dal sistema
  ?p1 <- (nodo (nome ha-batteria) (valore si) (certezza ?crt1) (sorgente-info sistema) (id-nodo ?id-p1)) ;; l'utente non sa se il disp. ha una batteria o meno
  ?p2 <- (nodo (nome alimentazione-collegata) (valore si) (certezza ?crt2) (id-nodo ?id-p2))
  ?p3 <- (nodo (nome stato-accensione) (valore non-funzionante) (certezza ?crt3) (id-nodo ?id-p3))
  =>
  (bind ?crt-batteria (calcola-certezza 0.6 ?crt1 ?crt2 ?crt3))
  (assert (nodo (nome batteria-difettosa) (valore si)(certezza ?crt-batteria)(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule DIAGNOSI-batteria-difettosa-si
  (fase 2-analisi)
  ?p1 <- (nodo (nome batteria-difettosa) (valore si) (certezza ?crt1) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome diagnosi) (valore batteria-difettosa) (certezza (* 1.0 ?crt1)) (nodo-padre ?id-p1)))
  (assert (nodo (nome diagnosi) (valore alimentatore-guasto) (certezza (* -0.5 ?crt1)) (nodo-padre ?id-p1)))
  (assert (nodo (nome diagnosi) (valore scheda-madre-guasta) (certezza (* -0.5 ?crt1)) (nodo-padre ?id-p1)))
)

(defrule chiedi-spia-alimentatore-pcportatile
  (fase 2-analisi)
  ;?p1 <- (nodo (nome alimentatore-funzionante) (valore no) (certezza ?crt1))
  ?p1 <- (nodo (nome tipo-dispositivo) (valore pc-portatile) (certezza ?crt1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome alimentazione-collegata) (valore si) (certezza ?crt2) (id-nodo ?id-p2))
  ?p3 <- (nodo (nome stato-accensione) (valore non-funzionante) (certezza ?crt3) (id-nodo ?id-p3))
  ;?p5 <- (nodo (nome batteria-difettosa) (valore no) (certezza ?crt5))
  =>
  (assert (nodo (nome chiedi) (valore spia-alimentatore-pcportatile) (nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)



(defrule batteria-non-presente
  (fase 2-analisi)
  ?p1 <- (nodo (nome ha-batteria) (valore no) (certezza ?c1) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome batteria-difettosa) (valore no) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1) (descrizione "Il dispositivo non ha una batteria.")))
  (assert (nodo (nome diagnosi) (valore batteria-difettosa) (certezza (* -1.0 ?c1)) (nodo-padre ?id-p1)))
)

(defrule DIAGNOSI-spia-alimentatore-accesa
  (fase 2-analisi)
  ;?p1 <- (nodo (nome alimentatore-funzionante) (valore no) (certezza ?crt1))
  ;?p2 <- (nodo (nome tipo-dispositivo) (valore pc-portatile) (certezza ?crt2))
  ?p1 <- (nodo (nome interruttore-alimentatore) (valore acceso) (certezza ?crt1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome alimentazione-collegata) (valore si) (certezza ?crt2) (id-nodo ?id-p2))
  ?p3 <- (nodo (nome stato-accensione) (valore  non-funzionante) (certezza ?crt3) (id-nodo ?id-p3))
  ;?p5 <- (nodo (nome batteria-difettosa) (valore no) (certezza ?crt5))
  ?p4 <- (nodo (nome spia-alimentatore) (valore accesa) (certezza ?crt4) (id-nodo ?id-p4))
  =>
  (bind ?crt-alim-guasto (calcola-certezza -0.3 ?crt1 ?crt2 ?crt3 ?crt4 ))
  (bind ?crt-scheda-madre-guasta (calcola-certezza 0.8 ?crt1 ?crt2 ?crt3 ?crt4 ))
  (assert (nodo (nome diagnosi) (valore alimentatore-guasto)(nodo-padre ?id-p1 ?id-p2 ?id-p3 ?id-p4) (certezza ?crt-alim-guasto)))
  (assert (nodo (nome diagnosi) (valore scheda-madre-guasta)(nodo-padre ?id-p1 ?id-p2 ?id-p3 ?id-p4) (certezza ?crt-scheda-madre-guasta)))
)

(defrule DIAGNOSI-spia-alimentatore-spenta
  (fase 2-analisi)
  ;?p1 <- (nodo (nome alimentatore-funzionante) (valore no) (certezza ?crt1))
  ?p1 <- (nodo (nome interruttore-alimentatore) (valore acceso) (certezza ?crt1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome alimentazione-collegata) (valore si) (certezza ?crt2) (id-nodo ?id-p2))
  ?p3 <- (nodo (nome stato-accensione) (valore  non-funzionante) (certezza ?crt3) (id-nodo ?id-p3))
  ;?p5 <- (nodo (nome batteria-difettosa) (valore no) (certezza ?crt5))
  ?p4 <- (nodo (nome spia-alimentatore) (valore spenta) (certezza ?crt4) (id-nodo ?id-p4))
  =>
  (bind ?crt-alim-guasto (calcola-certezza 0.8 ?crt1 ?crt2 ?crt3 ?crt4))
  (bind ?crt-scheda-madre-guasta (calcola-certezza 0.3 ?crt1 ?crt2 ?crt3 ?crt4))
  (assert (nodo (nome diagnosi) (valore alimentatore-guasto)(nodo-padre ?id-p1 ?id-p2 ?id-p3 ?id-p4) (certezza ?crt-alim-guasto)))
  (assert (nodo (nome diagnosi) (valore scheda-madre-guasta)(nodo-padre ?id-p1 ?id-p2 ?id-p3 ?id-p4) (certezza ?crt-scheda-madre-guasta)))
)



;; Per dispositivi desktop ...

(defrule chiedi-spia-alimentatore-pcdesktop
  (fase 2-analisi)
  ?p1 <- (nodo (nome tipo-dispositivo) (valore pc-desktop) (certezza ?crt1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome alimentazione-collegata) (valore si) (certezza ?crt2) (id-nodo ?id-p2))
  ;?p3 <- (nodo (nome alimentatore-funzionante) (valore no) (certezza ?crt3))
  ?p3 <- (nodo (nome stato-accensione) (valore  non-funzionante) (certezza ?crt3) (id-nodo ?id-p3))
  ?p4 <- (nodo (nome interruttore-alimentatore) (valore acceso) (certezza ?crt4) (id-nodo ?id-p4))
  =>
  (assert (nodo (nome chiedi) (valore spia-alimentatore-pcdesktop) (nodo-padre ?id-p1 ?id-p2 ?id-p3 ?id-p4)))
)

(defrule chiedi-interruttore-alimentatore
  (fase 2-analisi)
  ?p1 <- (nodo (nome tipo-dispositivo) (valore pc-desktop) (certezza ?crt1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome alimentazione-collegata) (valore si) (certezza ?crt2) (id-nodo ?id-p2))
  ;?p3 <- (nodo (nome alimentatore-funzionante) (valore no) (certezza ?crt3))
  ?p3 <- (nodo (nome stato-accensione) (valore  non-funzionante) (certezza ?crt3) (id-nodo ?id-p3))
  =>
  (assert (nodo (nome chiedi) (valore interruttore-alimentatore) (nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule DIAGNOSI-interruttore-alimentatore-spento
  (fase 2-analisi)
  ?p1 <- (nodo (nome tipo-dispositivo) (valore pc-desktop) (certezza ?crt1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome alimentazione-collegata) (valore si) (certezza ?crt2) (id-nodo ?id-p2))
  ;?p3 <- (nodo (nome alimentatore-funzionante) (valore no) (certezza ?crt3))
  ?p3 <- (nodo (nome stato-accensione) (valore  non-funzionante) (certezza ?crt3) (id-nodo ?id-p3))
  ?p4 <- (nodo (nome interruttore-alimentatore) (valore spento) (certezza ?crt4) (id-nodo ?id-p4))
  =>
  (bind ?crt-alim-spento (calcola-certezza 1.0 ?crt1 ?crt2 ?crt3 ?crt4))
  (assert (nodo (nome diagnosi) (valore alimentatore-spento)(nodo-padre ?id-p1 ?id-p2 ?id-p3 ?id-p4) (certezza ?crt-alim-spento)))
  (bind ?crt-diagnosi-escluse (calcola-certezza -1.0 ?crt1 ?crt2 ?crt3 ?crt4))
  (assert (nodo (nome diagnosi) (valore alimentatore-guasto) (certezza ?crt-diagnosi-escluse ) (nodo-padre ?id-p1 ?id-p2 ?id-p3 ?id-p4)))
  (assert (nodo (nome diagnosi) (valore scheda-madre-guasta) (certezza ?crt-diagnosi-escluse ) (nodo-padre ?id-p1 ?id-p2 ?id-p3 ?id-p4)))
)



;; DOMANDE VIDEO ***************************************************************

(defrule chiedi-problema-video-avvio
  (fase 2-analisi)
  ?p1 <- (nodo (nome stato-accensione)(valore funzionante)(certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome problema-principale) (valore analisi-guidata) (certezza ?crt2)(attivo TRUE)(id-nodo ?id-p2))
  (not (nodo (nome disturbo-video)(valore ?v)(certezza ?crt3&:(> ?crt3 0))(attivo TRUE)(id-nodo ?id-p3)))
  =>
  (assert (nodo (nome chiedi) (valore problema-video-avvio) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule chiedi-disturbo-video
  (fase 2-analisi)
  (or
      ?p1 <- (nodo (nome problema-video-avvio)(valore si)(certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))
      ?p1 <- (nodo (nome problema-principale) (valore video)(certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))
  )
  (not (nodo (nome disturbo-video) (valore ?v)(certezza ?crt2&:(> ?crt2 0))(attivo TRUE)(id-nodo ?id-p2)))
  =>
  (assert (nodo (nome chiedi) (valore disturbo-video) (nodo-padre ?id-p1)))
)

(defrule disturbo-fasce-verticali
  (fase 2-analisi)
  ?p1 <- (nodo(nome disturbo-video)(valore fasce-verticali)(certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))
  =>
  (assert (nodo (nome tipo-disturbo-video)(valore interferenza) (certezza (* 0.8 ?crt1))(nodo-padre ?id-p1)
          (descrizione "E' possibile che il problema sia causato da un interferenza.")))
  (assert (nodo (nome tipo-disturbo-video)(valore display-rotto)(certezza (* 0.5 ?crt1))(nodo-padre ?id-p1)
          (descrizione "E' possibile che qualche componente del display sia guasta.")))
)

(defrule disturbo-linee-orizzontali
  (fase 2-analisi)
  ?p1 <- (nodo(nome disturbo-video)(valore linee-orizzontali)(certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))
  =>
  (assert (nodo (nome tipo-disturbo-video)(valore interferenza) (certezza (* 0.9 ?crt1))(nodo-padre ?id-p1)
          (descrizione "E' possibile che il problema sia causato da un interferenza.")))
  (assert (nodo (nome tipo-disturbo-video)(valore display-rotto)(certezza (* 0.3 ?crt1))(nodo-padre ?id-p1)
          (descrizione "E' possibile che qualche componente del display sia guasta.")))
)

(defrule chiedi-cavi-display
  (fase 2-analisi)
  ?p1 <- (nodo (nome disturbo-video)(valore schermo-nero)(certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome cavi-display-accessibili)(valore si)(certezza ?crt2)(attivo TRUE)(id-nodo ?id-p2))
  ;?p2 <- (nodo (nome tipo-dispositivo) (valore pc-desktop))
  =>
  (assert (nodo (nome chiedi) (valore cavi-display) (nodo-padre ?id-p1 ?id-p2)))
)



(defrule disturbo-schermo-nero
  (fase 2-analisi)
  ?p1 <- (nodo(nome disturbo-video)(valore schermo-nero)(certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome cavi-display)(valore ok)(certezza ?crt2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (bind ?crt (calcola-certezza 1.0 ?crt1 ?crt2))
  (assert (nodo (nome tipo-disturbo-video)(valore display-rotto)(certezza ?crt)(nodo-padre ?id-p1 ?id-p2)
          (descrizione "E' possibile che qualche componente del display sia guasta.")))
)



; (defrule interferenza-video
;   (fase 2-analisi)
;   ?p1 <- (nodo(nome disturbo-video)(valore ?v&fasce-verticali|linee-orizzontali)(certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))
;   =>
;   (assert (nodo (nome tipo-disturbo-video)(valore interferenza) (certezza (* 0.8 ?crt1))(nodo-padre ?id-p1)
;           (descrizione "E' possibile che il problema sia causato da un interferenza.")))
; )
;
; (defrule display-rotto
;   (fase 2-analisi)
;   ?p1 <- (nodo (nome disturbo-video)(valore ?v&fasce-verticali|schermo-nero|linee-orizzontali)(certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))
;   =>
;   (assert (nodo (nome tipo-disturbo-video)(valore display-rotto)(certezza (* 0.7 ?crt1))(nodo-padre ?id-p1)
;           (descrizione "E' possibile che qualche componente del display sia guasta.")))
; )

; (defrule momento-problema-avvio
;   (fase 2-analisi)
;   ?p1 <- (nodo (nome problema-video-all-avvio)(valore si)(certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))
;   =>
;   (assert (nodo (nome momento-manifestazione-problema)(valore avvio)(certezza (* 1 ?crt1))(nodo-padre ?id-p1)
;           (descrizione "Il problema si manifesta sin dall'avvio del dispositivo")))
; )




(defrule chiedi-monitor-esterno
  (fase 2-analisi)
  ?p1 <- (nodo (nome tipo-disturbo-video)(valore ?v1&display-rotto|interferenza)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
  (not (nodo (nome chiedi) (valore monitor-esterno)))
  =>
  (assert (nodo (nome chiedi) (valore monitor-esterno) (nodo-padre ?id-p1)))
)

(defrule no-monitor-secondario
  (fase 2-analisi)
  ?p1 <- (nodo (nome monitor-esterno)(valore no)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
  =>
  (assert (nodo (nome monitor-esterno) (valore funzionante)(certezza (* 0.5 ?c1))(nodo-padre ?id-p1)))
  (assert (nodo (nome monitor-esterno) (valore errore)(certezza (* 0.5 ?c1))(nodo-padre ?id-p1)))
)

(defrule chiedi-problema-video-all-avvio
  (fase 2-analisi)
  ;?p1 <- (nodo (nome disturbo-video) (valore ?v&fasce|linee-oriz))
  ?p1 <- (nodo (nome tipo-disturbo-video)(valore ?v1&display-rotto|interferenza)(certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))
  (not (nodo (nome chiedi) (valore problema-video-all-avvio)))
  =>
  (assert (nodo (nome chiedi) (valore problema-video-all-avvio) (nodo-padre ?id-p1)))
)

; (defrule chiedi-blocco-cursore
;   (fase 2-analisi)
;   ?p1 <- (nodo (nome disturbo-video)(valore schermo-nero)(certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))
;   ?p2 <- (nodo (nome problema-video-all-avvio)(valore no)(certezza ?crt2)(attivo TRUE)(id-nodo ?id-p2))
;   ?p3 <- (nodo (nome cavi-display)(valore ok)(certezza ?crt3)(attivo TRUE)(id-nodo ?id-p3))
;   =>
;   (assert (nodo (nome chiedi) (valore blocco-cursore) (nodo-padre ?id-p1)))
; )

; (defrule cursore-sconosciuto
;   (fase 2-analisi)
;   ?p1 <- (nodo (nome blocco-cursore)(valore non-so)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
;   =>
;   (assert (nodo (nome blocco-cursore) (valore si)(certezza (* 0.5 ?c1))(nodo-padre ?id-p1)))
;   (assert (nodo (nome blocco-cursore) (valore no)(certezza (* 0.5 ?c1))(nodo-padre ?id-p1)))
; )

; (defrule chiedi-muovere-cavi-display
;   (fase 2-analisi)
;   ?p1 <- (nodo (nome tipo-disturbo-video) (valore interferenza) (certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))
;   ;?p1 <- (nodo (nome disturbo-video) (valore ?v&fasce|linee-oriz))
;   ?p2 <- (nodo (nome cavi-display-accessibili) (valore si)(certezza ?crt2)(attivo TRUE) (id-nodo ?id-p2))
;   ;?p2 <- (nodo (nome tipo-dispositivo) (valore pc-desktop))
;   ?p3 <- (nodo (nome problema-video-all-avvio)(valore si)(certezza ?crt3)(attivo TRUE)(id-nodo ?id-p3))
;   =>
;   (assert (nodo (nome chiedi) (valore muovere-cavi-display) (nodo-padre ?id-p1 ?id-p2 ?id-p3)))
; ) SPOSTARE IN DIAGNOSI

;;******************************************************************************
;; DIAGNOSI VIDEO **************************************************************

; (defrule diagnosi-cavi-display-non-connessi
;   ?p1 <- (nodo (nome muovere-cavi-display) (valore risolto) (id-nodo ?id-p1))
;   =>
;   (assert (nodo (nome diagnosi) (valore cavi-display-non-connessi) (nodo-padre ?id-p1)))
; )
;;Se il problema persiste e' possibile che il cavo sia danneggiato.

(defrule diagnosi-problema-SW-video
  ?p1 <- (nodo (nome tipo-disturbo-video) (valore display-rotto)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
  ; (or
  ;   ?p2 <- (nodo (nome cavi-display) (valore ok) (id-nodo ?id-p2))
  ;   ?p2 <- (nodo (nome cavi-display-accessibili) (valore no) (id-nodo ?id-p2))
  ; )
  ; ?p3 <- (nodo (nome monitor-esterno) (valore ?v1&errore|no) (id-nodo ?id-p3))
  ;?p2 <- (nodo (nome blocco-cursore) (valore si)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
  ?p2 <- (nodo (nome problema-video-all-avvio) (valore no)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (bind ?crt-SW (calcola-certezza 0.8 ?c1 ?c2))
  ;(bind ?crt-driver (calcola-certezza 0.8 ?c1 ?c2))
  (bind ?crt-neg (calcola-certezza -0.8 ?c1 ?c2))
  (assert (nodo (nome diagnosi)(valore problema-SW-video)(certezza ?crt-SW)(nodo-padre ?id-p1 ?id-p2)))
  ;(assert (nodo (nome diagnosi)(valore problema-driver-video)(certezza ?crt-driver)(nodo-padre ?id-p1 ?id-p2)))
  (assert (nodo (nome diagnosi)(valore guasto-display)(certezza ?crt-neg)(nodo-padre ?id-p1 ?id-p2)))
)

(defrule diagnosi-display-guasto
  ?p1 <- (nodo (nome tipo-disturbo-video) (valore display-rotto)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
  ;?p2 <- (nodo (nome riavvio-forzato) (valore no) (id-nodo ?id-p2))
  ?p2 <- (nodo (nome monitor-esterno) (valore funzionante)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo (nome problema-video-all-avvio) (valore si)(certezza ?c3)(attivo TRUE)(id-nodo ?id-p3))
  ; (or
  ;   ?p5 <- (nodo (nome muovere-cavi-display) (valore non-risolto) (id-nodo ?id-p5))
  ;   ?p5 <- (nodo (nome cavi-display-accessibili) (valore no) (id-nodo ?id-p5))
  ; )
  =>
  (bind ?crt (calcola-certezza 0.8 ?c1 ?c2 ?c3))
  (assert (nodo (nome diagnosi) (valore guasto-display)(certezza ?crt)(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule diagnosi-inverter-guasto
  ?p1 <- (nodo (nome tipo-disturbo-video) (valore ?v1&display-rotto|interferenza)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
    (not (nodo (nome tipo-disturbo-video) (valore ?v2&display-rotto|interferenza)(certezza ?c1b&:(> ?c1b ?c1))(attivo TRUE)(id-nodo ?id2)))
    (not (nodo (nome tipo-disturbo-video) (valore ?v3&display-rotto|interferenza)(certezza ?c1c&:(eq ?c1c ?c1))(attivo TRUE)(id-nodo ?id3&:(> ?id3 ?id-p1))))

  ?p2 <- (nodo (nome monitor-esterno) (valore funzionante)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo (nome problema-video-all-avvio) (valore si)(certezza ?c3)(attivo TRUE)(id-nodo ?id-p3))
  ?p4 <- (nodo (nome tipo-dispositivo) (valore pc-portatile)(certezza ?c4)(attivo TRUE)(id-nodo ?id-p4))
  ; (or
  ;   ?p5 <- (nodo (nome muovere-cavi-display) (valore non-risolto) (id-nodo ?id-p5))
  ;   ?p5 <- (nodo (nome cavi-display-accessibili) (valore no) (id-nodo ?id-p5))
  ; )
  =>
  (bind ?crt (calcola-certezza 0.9 ?c1 ?c2 ?c3 ?c4))
  (assert (nodo (nome diagnosi) (valore guasto-inverter)(certezza ?crt)(nodo-padre ?id-p1 ?id-p2 ?id-p3 ?id-p4)))
)

; (defrule diagnosi-display-guasto-2
;   ?p1 <- (nodo (nome disturbo-video) (valore schermo-nero) (id-nodo ?id-p1))
;   (or
;     ?p2 <- (nodo (nome cavi-display) (valore ok) (id-nodo ?id-p2))
;     ?p2 <- (nodo (nome cavi-display-accessibili) (valore no) (id-nodo ?id-p2))
;   )
;   ?p3 <- (nodo (nome monitor-esterno) (valore ?v2&funzionante|no) (id-nodo ?id-p3))
;   ?p4 <- (nodo (nome blocco-cursore) (valore no) (id-nodo ?id-p4))
;   =>
;   (assert (nodo (nome diagnosi) (valore guasto-display) (nodo-padre ?id-p1 ?id-p2 ?id-p3 ?id-p4)))
; )

; (defrule diagnosi-cavi-display-portatile-guasti
;   ?p1 <- (nodo (nome diagnosi) (valore guasto-display) (id-nodo ?id-p1))
;   ?p2 <- (nodo (nome cavi-display-accessibili) (valore no) (id-nodo ?id-p2))
;   =>
;   (assert (nodo (nome diagnosi) (valore cavi-display-portatile-guasti) (nodo-padre ?id-p1 ?id-p2)))
; )

(defrule diagnosi-interferenze-cavo
  ?p1 <- (nodo (nome tipo-disturbo-video)(valore interferenza)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome problema-video-all-avvio)(valore si)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (bind ?crt (calcola-certezza 0.9 ?c1 ?c2))
  (assert (nodo (nome diagnosi) (valore interferenze-cavo)(certezza ?crt)(nodo-padre ?id-p1 ?id-p2)))
)

; (defrule diagnosi-display-guasto-3
;   ?p1 <- (nodo (nome disturbo-video) (valore macchie) (id-nodo ?id-p1))
;   =>
;   (assert (nodo (nome diagnosi) (valore guasto-display) (nodo-padre ?id-p1)))
; )

(defrule diagnosi-guasto-vga
  ?p1 <- (nodo (nome tipo-disturbo-video) (valore display-rotto)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome monitor-esterno) (valore errore)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo (nome problema-video-all-avvio)(valore si)(certezza ?c3)(attivo TRUE)(id-nodo ?id-p3))
  ; (or
  ;   ?p3 <- (nodo (nome muovere-cavi-display) (valore non-risolto) (id-nodo ?id-p3))
  ;   ?p3 <- (nodo (nome cavi-display-accessibili) (valore no) (id-nodo ?id-p3))
  ; )
  =>
  (bind ?crt (calcola-certezza 0.9 ?c1 ?c2))
  (assert (nodo (nome diagnosi) (valore guasto-vga)(certezza ?crt)(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule disturbo-macchie-con-DIAGNOSI
  (fase 2-analisi)
  ?p1 <- (nodo (nome disturbo-video)(valore macchie)(certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))
  =>
  (assert (nodo (nome diagnosi) (valore guasto-display)(certezza (* 0.95 ?crt1))(nodo-padre ?id-p1)))
)

(defrule cavi-display-portatile
  (fase 2-analisi)
  ?p1 <- (nodo (nome disturbo-video)(valore schermo-nero)(certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome cavi-display-accessibili)(valore no)(certezza ?crt2)(attivo TRUE)(id-nodo ?id-p2))
  ;?p2 <- (nodo (nome tipo-dispositivo) (valore pc-desktop))
  =>
  (bind ?crt-cavi-collegati (calcola-certezza 1.0 ?crt1 ?crt2))
  ;(bind ?crt-cavi-rovinati (calcola-certezza 0.4 ?crt1 ?crt2))
  (assert (nodo(nome cavi-display)(valore ok)(certezza ?crt-cavi-collegati)(nodo-padre ?id-p1 ?id-p2)))
  ;(assert (nodo(nome cavi-display)(valore errore)(certezza ?crt-cavi-rovinati)(nodo-padre ?id-p1 ?id-p2)))
)

(defrule DIAGNOSI-cavi-video-scollegati
  (fase 2-analisi)
  ?p1 <- (nodo(nome disturbo-video)(valore schermo-nero)(certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome cavi-display)(valore errore)(certezza ?crt2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (bind ?crt (calcola-certezza 1.0 ?crt1 ?crt2))
  (assert (nodo (nome diagnosi) (valore cavi-display-scollegati)(certezza ?crt)(nodo-padre ?id-p1 ?id-p2)))
)


; (defrule diagnosi-guasto-vga-2
;   ?p1 <- (nodo (nome disturbo-video) (valore schermo-nero) (id-nodo ?id-p1))
;   (or
;     ?p2 <- (nodo (nome cavi-display) (valore ok) (id-nodo ?id-p2))
;     ?p2 <- (nodo (nome cavi-display-accessibili) (valore no) (id-nodo ?id-p2))
;   )
;   ?p3 <- (nodo (nome monitor-esterno) (valore ?v1&errore|no) (id-nodo ?id-p3))
;   ?p4 <- (nodo (nome blocco-cursore) (valore no) (id-nodo ?id-p4))
;   =>
;   (assert (nodo (nome diagnosi) (valore guasto-vga) (nodo-padre ?id-p1 ?id-p2 ?id-p3 ?id-p4)))
; )
;
; (defrule diagnosi-problema-driver-video
;   ?p1 <- (nodo (nome tipo-disturbo-video) (valore fasce) (id-nodo ?id-p1))
;   ?p2 <- (nodo (nome monitor-esterno) (valore ?v1&errore|no) (id-nodo ?id-p2))
;   ?p3 <- (nodo (nome problema-video-all-avvio) (valore no) (id-nodo ?id-p3))
;   (or
;     ?p4 <- (nodo (nome muovere-cavi-display) (valore non-risolto) (id-nodo ?id-p4))
;     ?p4 <- (nodo (nome cavi-display-accessibili) (valore no) (id-nodo ?id-p4))
;   )
;   =>
;   (assert (nodo (nome diagnosi) (valore problema-driver-video) (nodo-padre ?id-p1 ?id-p2 ?id-p3 ?id-p4)))
; )
; ;;provare ad aggiornare o ripristinare i driver.
;
; (defrule diagnosi-problema-driver-video-2
;   ?p1 <- (nodo (nome disturbo-video) (valore schermo-nero) (id-nodo ?id-p1))
;   ?p2 <- (nodo (nome cavi-display) (valore ?v2&ok|interni) (id-nodo ?id-p2))
;   ?p3 <- (nodo (nome monitor-esterno) (valore ?v1&errore|no) (id-nodo ?id-p3))
;   ?p4 <- (nodo (nome blocco-cursore) (valore no) (id-nodo ?id-p4))
;   =>
;   (assert (nodo (nome diagnosi) (valore problema-driver-video) (nodo-padre ?id-p1 ?id-p2 ?id-p3 ?id-p4)))
; )
;
;
;
; (defrule diagnosi-cavi-display-disconnessi ;;NON INSERITA NELLE DIAGNOSI
;   ?p1 <- (nodo (nome cavi-display) (valore errore) (id-nodo ?id-p1))
;   =>
;   (assert (nodo (nome diagnosi) (valore cavi-display-disconnessi)(nodo-padre ?id-p1)))
; )

;;******************************************************************************













;; DOMANDE ACCENSIONE E SO *****************************************************

; (defrule chiedi-riavvio-forzato
;   (fase 2-analisi)
;   ?p1 <- (nodo (nome stato-accensione) (valore ok) (id-nodo ?id-p1))
;   =>
;   (assert (nodo (nome chiedi) (valore riavvio-forzato) (nodo-padre ?id-p1)))
; )





;;******************************************************************************





;; FASE 4 SOLUZIONI ********************

; (defrule or-solution
;   (fase 4-trova-soluzioni)
;   (or
;         (nodo (nome ?n&diagnosi) (valore ?v&alimentatore-guasto) (certezza ?c1))
;         (nodo (nome ?n2&diagnosi) (valore ?v2&scheda-madre-guasta) (certezza ?c2))
;   )
;   =>
;     (bind ?x (max ?c1 ?c2))
;     (printout t "FOUND " ?x crlf)
;   )


(defrule ex-sol
  (fase 4-trova-soluzioni)

      ?p1 <- (nodo (id-nodo ?id1) (attivo TRUE) (nome diagnosi) (valore ?v1&A|B|C|D|E|F) (certezza ?c1))
      ;; assicura che p1 sia il nodo con certezza piu' alta
      (not (nodo (id-nodo ?id2) (attivo TRUE) (nome diagnosi) (valore ?v2&A|B|C|D|E|F) (certezza ?c2&:(> ?c2 ?c1))))
      ;; se abbiamo due o piu' nodi con la stessa certezza massima, prendiamo sempre il nodo con id piu' alto per evitare di attivare la stessa regola piu' volte
      (not (nodo (id-nodo ?id3&:(> ?id3 ?id1)) (attivo TRUE) (nome diagnosi) (valore ?v3&A|B|C|D|E|F) (certezza ?c3&:(eq ?c3 ?c1))))
  =>
  (printout t "MAX IS: " ?v1 " WITH: " ?c1 crlf)
)

; (defrule modify-sol
;   (fase 4-trova-soluzioni)
;   ?p1 <-  (nodo (id-nodo ?i) (nome ?n&diagnosi) (valore ?v&alimentatore-spento) (certezza ?c1))
;   (not (nodo (nome ?n) (valore ?v) (nodo-padre $?pdr1 ?p1 $?pdr2)))
;   =>
;   (printout  t "FACT: " ?p1 " ID: " ?i crlf)
;   (bind ?x (modify ?p1 (valore WOWOWOW)))
;   (printout  t "MODDED: " ?x crlf)
;
;   (assert (nodo (nome soluzione) (valore accendi-alimentatore) (certezza (* 0.95 ?c1)) (nodo-padre ?p1)))
;
; )

(defrule guasto-hardware-si
  (fase 4-trova-soluzioni)
  ?p1 <- (nodo (id-nodo ?id-p1) (attivo TRUE) (nome diagnosi) (valore ?v1&batteria-difettosa|alimentatore-guasto|scheda-madre-guasta|guasto-display|guasto-vga|guasto-inverter) (certezza ?c1))
      ;; assicura che p1 sia il nodo con certezza piu' alta
  (not (nodo (id-nodo ?id-p2) (attivo TRUE) (nome diagnosi) (valore ?v2&batteria-difettosa|alimentatore-guasto|scheda-madre-guasta|guasto-display|guasto-vga|guasto-inverter) (certezza ?c2&:(> ?c2 ?c1))))
      ;; se abbiamo due o piu' nodi con la stessa certezza massima, prendiamo sempre il nodo con id piu' alto per evitare di attivare la stessa regola piu' volte
  (not (nodo (id-nodo ?id-p3&:(> ?id-p3 ?id-p1))(attivo TRUE) (nome diagnosi) (valore ?v3&batteria-difettosa|alimentatore-guasto|scheda-madre-guasta|guasto-display|guasto-vga|guasto-inverter)(certezza ?c3&:(eq ?c3 ?c1))))
  =>
  (assert (nodo (nome guasto-hardware) (valore si) (certezza (* 1 ?c1)) (nodo-padre ?id-p1) (descrizione "Almeno una componente hardware del dispositivo e' guasta.")))
)

(defrule soluzione-dispositivo-in-assistenza
    (fase 4-trova-soluzioni)
    ?p1 <- (nodo (nome guasto-hardware) (valore si) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
    ?p2 <- (nodo (nome garanzia) (valore si) (certezza ?c2) (attivo TRUE) (id-nodo ?id-p2))
    =>
    (bind ?crt (calcola-certezza 1 ?c1 ?c2))
    (assert (nodo (nome soluzione) (valore dispositivo-in-assistenza) (certezza ?crt) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule soluzione-sostituzione-dispositivo-obsoleto
    (fase 4-trova-soluzioni)
    ?p1 <- (nodo (nome guasto-hardware) (valore si) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
    ?p2 <- (nodo (nome anni-dispositivo) (valore piu-10-anni) (certezza ?c2) (attivo TRUE) (id-nodo ?id-p2))
    =>
    (bind ?crt (calcola-certezza 0.8 ?c1 ?c2))
    (assert (nodo (nome soluzione) (valore sostituzione-dispositivo) (certezza ?crt) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule soluzione-sostituisci-alimentatore
    (fase 4-trova-soluzioni)
    ?p1 <- (nodo (nome diagnosi) (valore alimentatore-guasto) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
    =>
    (assert (nodo (nome soluzione) (valore sostituisci-alimentatore) (certezza (* 0.95 ?c1)) (nodo-padre ?id-p1)))
)

(defrule soluzione-sostituisci-scheda-madre
  (fase 4-trova-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore scheda-madre-guasta) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome tipo-dispositivo) (valore pc-desktop) (certezza ?c2) (attivo TRUE) (id-nodo ?id-p2))
  =>
  (bind ?crt (calcola-certezza 0.9 ?c1 ?c2))
  (assert (nodo (nome soluzione) (valore sostituisci-scheda-madre) (certezza ?crt) (nodo-padre ?id-p1)))
)

(defrule soluzione-sostituisci-portatile
  (fase 4-trova-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore scheda-madre-guasta) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome tipo-dispositivo) (valore pc-portatile) (certezza ?c2) (attivo TRUE) (id-nodo ?id-p2))
  =>
  (bind ?crt (calcola-certezza 0.75 ?c1 ?c2))
  (assert (nodo (nome soluzione) (valore sostituzione-dispositivo) (certezza ?crt) (nodo-padre ?id-p1)))
)

(defrule soluzione-riparazione-scheda-madre
  (fase 4-trova-soluzioni)
  ?p1 <- (nodo (id-nodo ?id-p1) (attivo TRUE) (nome anni-dispositivo) (valore ?v1&meno-2-anni|2-5-anni) (certezza ?c1))
  (not (nodo (id-nodo ?id-p2) (attivo TRUE) (nome anni-dispositivo) (valore ?v2&meno-2-anni|2-5-anni) (certezza ?c2&:(> ?c2 ?c1))))
  (not (nodo (id-nodo ?id-p3&:(> ?id-p3 ?id-p1))(attivo TRUE) (nome anni-dispositivo) (valore ?v3&meno-2-anni|2-5-anni)(certezza ?c3&:(eq ?c3 ?c1))))
  ?p4 <- (nodo (nome diagnosi) (valore scheda-madre-guasta) (certezza ?c4) (attivo TRUE) (id-nodo ?id-p4))
  =>
  (bind ?crt (calcola-certezza 0.95 ?c1 ?c4))
  (assert (nodo (nome soluzione) (valore riparazione-scheda-madre) (certezza ?crt) (nodo-padre ?id-p1 ?id-p4)))
)

(defrule soluzione-accendi-alimentatore
  (fase 4-trova-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore alimentatore-spento) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome soluzione) (valore accendi-alimentatore) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1)))
)

(defrule soluzione-connetti-alimentazione
  (fase 4-trova-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore alimentazione-disconnessa) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome soluzione) (valore connetti-alimentazione) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1)))
)

;;SOLUZIONI VIDEO

(defrule soluzione-connetti-cavi-video
  (fase 4-trova-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore cavi-display-scollegati) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome soluzione) (valore connetti-cavi-video) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1)))
)

(defrule soluzione-sostituisci-display
  (fase 4-trova-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore guasto-display) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome soluzione) (valore sostituisci-display) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1)))
)

(defrule soluzione-sostituisci-vga
  (fase 4-trova-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore guasto-vga) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome soluzione) (valore sostituisci-vga) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1)))
)

(defrule soluzione-controllo-cavi-video
  (fase 4-trova-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore interferenze-cavo) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome cavi-display-accessibili)(valore si)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (bind ?crt (calcola-certezza 0.9 ?c1 ?c2))
  (assert (nodo (nome soluzione) (valore controllo-cavi-video) (certezza ?crt) (nodo-padre ?id-p1)))
)

(defrule soluzione-controllo-cavi-video-portatile
  (fase 4-trova-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore interferenze-cavo) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome cavi-display-accessibili)(valore no)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (bind ?crt (calcola-certezza 0.7 ?c1 ?c2))
  (assert (nodo (nome soluzione) (valore controllo-cavi-video-portatile) (certezza ?crt) (nodo-padre ?id-p1)))
)

(defrule soluzione-sostituisci-inverter
  (fase 4-trova-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore guasto-inverter) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome soluzione) (valore sostituisci-inverter) (certezza (* 0.9 ?c1)) (nodo-padre ?id-p1)))
)

(defrule soluzione-problema-SW-video
  (fase 4-trova-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore problema-SW-video) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome soluzione) (valore soluzione-SW-video) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1)))
)




;; ***** FASE 4: REGOLA PER PASSAGGIO ALLA PROSSIMA FASE

(defrule passa-alla-fase-5
  (declare (salience ?*lowest-priority*))
  ?f <- (fase 4-trova-soluzioni)
  =>
  (retract ?f)
  (assert (fase 5-stampa-soluzioni))
)












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








; (defmodule ELENCO-DIAGNOSI (import MAIN ?ALL)(export ?ALL))
;
; (defmodule ELENCO-DOMANDE(import MAIN ?ALL)(export ?ALL))










  ;******************* MODULO DOMANDE GENERICHE **********************************

  ; (defmodule DOMANDE-GENERICHE (import MAIN deftemplate ?ALL)(import MAIN defglobal ?ALL) (import MAIN deffunction ?ALL)(export ?ALL))
  ;
  ;
  ;     (defrule DOMANDE-GENERICHE::init
  ;       (declare (salience ?*highest-priority*))
  ;       =>
  ;       (set-strategy random)
  ;       (printout t "DEBUG >> DOMANDE-GENERICHE >> strategy set to random." crlf crlf)
  ;     )
  ;
  ;
  ;
  ;
  ;     (defrule DOMANDE-GENERICHE::chiedi-domanda-generica
  ;       ;(declare (salience ?*low-priority*))
  ;       ?ask <- (nodo (nome chiedi)(valore ?attr)(nodo-padre $?p))
  ;       ?f <- (domanda (attributo ?attr) (testo-domanda ?domanda) (risposte-valide $?risposte) (descrizione-risposte $?descrizioni) (gia-chiesta FALSE)(domanda-generica TRUE))
  ;       (not (nodo (nome ?attr)))
  ;       ?cont-dom <- (contatore-domande ?i)
  ;       =>
  ;       (bind ?j (+ ?i 1))
  ;       (bind ?risposta (ask-question ?j ?domanda ?descrizioni))
  ;       (if (= ?risposta 0) then
  ;         (retract ?ask)
  ;         (assert (nodo (nome chiedi)(valore ?attr)(nodo-padre ?p))) ;;NECESSARIO PER RIPROPORRE LA STESSA DOMANDA NEL CASO DI ANNULLAMENTO REVISIONE
  ;         (assert (init-revisiona-domande))
  ;       else
  ;         (if (= ?risposta 9) then
  ;           (retract ?ask)
  ;           (assert (nodo (nome chiedi)(valore ?attr)(nodo-padre ?p))) ;;NECESSARIO PER RIPROPORRE LA STESSA DOMANDA NEL CASO DI ANNULLAMENTO REVISIONE
  ;           (assert (nodo (nome spiegazione) (valore ?attr)))
  ;           (focus SPIEGAZIONE)
  ;         else
  ;           ;;(assert (nodo (nome ?attr) (valore (nth$ ?risposta ?risposte)) (descrizione (nth$ ?risposta ?descrizioni)) (tipo info-utente) (nodo-padre ?ask)))
  ;           (modify ?f (gia-chiesta TRUE)(num-domanda ?j)(risposta-selezionata ?risposta))
  ;           ;;(retract ?ask)
  ;           (retract ?cont-dom)
  ;           (assert (contatore-domande ?j))
  ;         )
  ;       )
  ;     )
  ;
  ;     (defrule DOMANDE-GENERICHE::usa-risposta-utente-gen
  ;       ?ask <- (nodo (nome chiedi)(valore ?attr)(nodo-padre $?p))
  ;       ?f <- (domanda (attributo ?attr) (testo-domanda ?domanda) (risposte-valide $?risposte) (descrizione-risposte $?descrizioni) (gia-chiesta TRUE) (risposta-selezionata ?risp) (domanda-generica TRUE))
  ;       (not (nodo (nome ?attr)))
  ;       =>
  ;       (assert (nodo (nome ?attr) (valore (nth$ ?risp ?risposte)) (descrizione (nth$ ?risp ?descrizioni)) (tipo info-utente) (nodo-padre ?ask)))
  ;     )







  ;*******************************************************************************
