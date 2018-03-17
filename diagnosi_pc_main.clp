

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
    (slot valore  (type SYMBOL))
    (slot certezza (type FLOAT) (default 1.0))
    (slot sorgente-info (type SYMBOL) (default sistema))
    (multislot nodo-padre (type FACT-ADDRESS))
    (slot descrizione (type STRING))
  )


  (deftemplate domanda
    (slot attributo     (type SYMBOL) (default ?NONE))
    (slot gia-chiesta   (default  FALSE))
    (slot stampata (default FALSE))
    (slot num-domanda (type INTEGER))
    (multislot risposte-valide (type SYMBOL) (default ?NONE))
    (slot risposta-selezionata (type INTEGER))
    (slot testo-domanda (type STRING) (default ?NONE))
    (multislot descrizione-risposte (type STRING) (default ?NONE))

    (slot spiegazione (type STRING) (default "placeholder spiegazione"))
    (slot help (type STRING) (default "placeholder help"))
  )

  (deftemplate diagnosi
    (slot attributo   (type SYMBOL))
    (slot titolo      (type STRING))
    (slot descrizione (type STRING))
    ;(slot stampata (default FALSE))
  )

  (deftemplate soluzione
    (slot attributo   (type SYMBOL))
    (slot titolo      (type STRING))
    (slot descrizione (type STRING))
    ;(slot stampata (default FALSE))

  )


(defmodule MAIN(export ?ALL))

;;****************
;;* DEFFUNCTIONS *
;;****************

(deffunction MAIN::ask-question (?num-domanda ?nodo-domanda)
  (bind ?testo-domanda (fact-slot-value ?nodo-domanda testo-domanda))
  (bind ?spiegazione (fact-slot-value ?nodo-domanda spiegazione))
  (bind ?aiuto (fact-slot-value ?nodo-domanda help))
  (bind ?descrizioni (fact-slot-value ?nodo-domanda descrizione-risposte))

  ;(clear-window)
  (printout t crlf crlf)
  (printout t   "***                                                 ***" crlf
                "**  SISTEMA DIAGNOSTICO PER DISPOSITIVI ELETTRONICI  **" crlf
                "*                                                     *" crlf
                "*     Rispondere alle domande inserendo il numero     *" crlf
                "**       corrispondente alla risposta corretta.      **" crlf
                "***                                                 ***" crlf crlf)

  (printout t "***** DOMANDA N." ?num-domanda " *****" crlf)
  (format t "%s%n%n" ?testo-domanda)
  ;(printout t crlf crlf)
  (loop-for-count (?cnt1 1 (length ?descrizioni)) do
      (printout t ?cnt1 ". " (nth$ ?cnt1 ?descrizioni) crlf)
  )
  (printout t crlf "9. Perche' questa domanda?")
  (printout t crlf "0. Aiutami a rispondere a questa domanda." crlf crlf)
  (printout t "Inserire risposta: ")
  (bind ?answer (read))
  (if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))

  (while (not (and (>= ?answer 1) (<= ?answer (length ?descrizioni))))
      (if (= ?answer 0) then (printout t ?aiuto crlf crlf))
      (if (= ?answer 9) then (printout t ?spiegazione crlf crlf))
      (printout t "Inserire risposta: ")
      (bind ?answer (read))
      (if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))
  )
  (printout t crlf crlf)

  (return ?answer)
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
  (test (neq ?nodo1 ?nodo2))
  =>
  (modify ?nodo1 (attivo FALSE))
  (modify ?nodo2 (attivo FALSE))
  (assert (nodo (nome ?n) (valore ?v) (certezza (combina-CF ?c1 ?c2)) (nodo-padre ?i1 ?i2)))
)

(defrule MAIN::rimuovi-padri-duplicati
  (declare (salience ?*highest-priority*))
  ?n <- (nodo (nodo-padre $?nodi1 ?elem $?nodi2 ?elem $?nodi3))
  =>
  (modify ?n (nodo-padre ?nodi1 ?elem ?nodi2 ?nodi3))
)

;;******************
;;* CONTROL RULES  *
;;******************

(defrule MAIN::inizializzazione
  (declare (salience ?*highest-priority*))
  =>
  (load-facts "data/DOMANDE.DAT")
  (load-facts "data/DIAGNOSI.DAT")
  (load-facts "data/SOLUZIONI.DAT")
  (load "rules/modulo-stampa-diagnosi-soluzioni.clp")
  (load "rules/modulo-spiegazione.clp")
  (load "rules/modulo-ritrattazione.clp")
  ;(clear-window)
  (assert (contatore-domande 0))
  (set-strategy random)
)

(defrule MAIN::profilazione-completata
  (declare (salience ?*highest-priority*))
  (nodo (nome esperienza-utente) (id-nodo ?id-p1) (attivo TRUE))
  ;(nodo (nome problema-principale) (id-nodo ?id-p2) (attivo TRUE))
  (nodo (nome anni-dispositivo) (id-nodo ?id-p3) (attivo TRUE))
  (nodo (nome garanzia) (id-nodo ?id-p4) (attivo TRUE))
  (nodo (nome tipo-dispositivo) (id-nodo ?id-p5) (attivo TRUE))
  (nodo (nome ha-batteria) (id-nodo ?id-p6) (attivo TRUE))
  =>
  (set-strategy depth)
  (assert (nodo (nome nodo-di-collegamento) (valore avvia-ricerca-diagnosi) (nodo-padre ?id-p1 ?id-p3 ?id-p4 ?id-p5 ?id-p6)))
  )


(defrule MAIN::fine-domande-passa-a-ricerca-soluzioni
  (declare (salience ?*lowest-priority*))
  (nodo (nome chiedi) (valore ?dom))
  (not (domanda (attributo ?dom) (gia-chiesta FALSE)))
  (not (fase-cerca-soluzioni))
  =>
  (assert (fase-cerca-soluzioni))
)

(defrule MAIN::fine-ricerca-soluzioni-passa-a-stampa
  (declare (salience ?*lowest-priority*))
  (nodo (nome chiedi) (valore ?dom))
  (not (domanda (attributo ?dom) (gia-chiesta FALSE)))
  ?f <- (fase-cerca-soluzioni)
  =>
  (retract ?f)
  (focus  MODULO-STAMPA-DIAGNOSI-SOLUZIONI)
)

(defrule MAIN::passa-a-modulo-ritrattazione
  (declare (salience ?*highest-priority*))
  (fase-ritrattazione)
  =>
  (focus MODULO-RITRATTAZIONE)
)

; CHIEDI DOMANDA
;****************************************************************************


(defrule chiedi-domanda
  (declare (salience ?*low-priority*))

  ?ask <- (nodo (nome chiedi)(valore ?attr)(nodo-padre $?p)(id-nodo ?id-ask))
  ?f <- (domanda (attributo ?attr) (testo-domanda ?domanda) (spiegazione ?spieg) (help ?aiuto)
          (risposte-valide $?risposte) (descrizione-risposte $?descrizioni) (gia-chiesta FALSE))
  (not (nodo (nome ?attr))) ; ATTENZIONE: controllare se eliminabile
  ?cont-dom <- (contatore-domande ?i)
  =>
  (bind ?j (+ ?i 1))
  (bind ?risposta (ask-question ?j ?f))
  (modify ?f (gia-chiesta TRUE)(num-domanda ?j)(risposta-selezionata ?risposta))
  (retract ?cont-dom)
  (assert (contatore-domande ?j))
  (assert (nodo (nome ?attr) (valore (nth$ ?risposta ?risposte)) (descrizione (nth$ ?risposta ?descrizioni)) (sorgente-info utente) (nodo-padre ?id-ask)))
)

;;******************************************************************************
;;*    REGOLE PER CHIDERE DOMANDE ALL'UTENTE                                   *
;;******************************************************************************

;; *****************************************************************************
;; ************ REGOLE PER FASE 1: PROFILAZIONE UTENTE E DISPOSITIVO ***********
;; *****************************************************************************

;; ***** FASE 1: DOMANDE DA PORRE ALL'UTENTE

(defrule chiedi-tipo-dispositivo
  ; (or (fase 1-profilazione) ;(fase 2-analisi))
  (not (nodo (nome chiedi) (valore tipo-dispositivo)))
  (domanda (attributo tipo-dispositivo)(gia-chiesta FALSE))
  =>
  (assert (nodo (nome chiedi) (valore tipo-dispositivo)))
)

(defrule chiedi-esperienza-utente
  ; (or (fase 1-profilazione) ;(fase 2-analisi))
  (not (nodo (nome chiedi) (valore esperienza-utente)))
  (domanda (attributo esperienza-utente)(gia-chiesta FALSE))
  =>
  (assert (nodo (nome chiedi) (valore esperienza-utente)))
)

; (defrule chiedi-problema-principale
;   ; (or (fase 1-profilazione) ;(fase 2-analisi))
;   (not (nodo (nome chiedi) (valore problema-principale)))
;   (domanda (attributo problema-principale)(gia-chiesta FALSE))
;   ?p1 <- (nodo (nome esperienza-utente) (valore utente-esperto) (id-nodo ?id-p1))
;   =>
;   (assert (nodo (nome chiedi) (valore problema-principale) (nodo-padre ?id-p1)))
; )

(defrule chiedi-anni-dispositivo
  ; (or (fase 1-profilazione) ;(fase 2-analisi))
  (not (nodo (nome chiedi) (valore anni-dispositivo)))
  (domanda (attributo anni-dispositivo)(gia-chiesta FALSE))
  =>
  (assert (nodo (nome chiedi) (valore anni-dispositivo)))
)

(defrule chiedi-garanzia
  ; (or (fase 1-profilazione) ;(fase 2-analisi))
  (not (nodo (nome chiedi) (valore garanzia)))
  (domanda (attributo garanzia)(gia-chiesta FALSE))
  ?p1 <- (nodo (nome anni-dispositivo) (valore ?val&meno-2-anni|2-5-anni|sconosciuto) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome chiedi) (valore garanzia) (nodo-padre ?id-p1)))
)

(defrule chiedi-ha-batteria
  ; (or (fase 1-profilazione) ;(fase 2-analisi))
  (not  (nodo (nome chiedi) (valore ha-batteria)))
  (domanda (attributo ha-batteria)(gia-chiesta FALSE))
  ?p1 <- (nodo (nome tipo-dispositivo) (valore pc-portatile) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome chiedi) (valore ha-batteria) (nodo-padre ?id-p1)))
)

;; ***** FASE 2: DEDUZIONI DEL SISTEMA

(defrule dispositivo-portatile
  ; ;(fase 2-analisi)
  ?p1 <- (nodo (nome tipo-dispositivo) (valore pc-portatile) (certezza ?c1) (id-nodo ?id-p1))
  =>
  ;;(assert (nodo (nome possiede-batteria) (valore si) (nodo-padre ?p1) (descrizione "Il dispositivo possiede una batteria.")))
  (assert (nodo (nome cavi-display-accessibili) (valore no) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1) (descrizione "I cavi che collegano il dispositivo al display non sono accessibili.")))
  (assert (nodo (nome alimentatore-caricabatterie) (valore si) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1) (descrizione "Il dispositivo possiede un alimentatore caricabatterie esterno.")))
  (assert (nodo (nome interruttore-alimentatore) (valore acceso) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1) (descrizione "Essendo un dispositivo portatile, l'alimentatore non possiede un tasto di accensione esterno.")))
)

(defrule dispositivo-fisso
  ; ;(fase 2-analisi)
  ?p1 <- (nodo (nome tipo-dispositivo) (valore pc-desktop) (certezza ?c1) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome ha-batteria) (valore no) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1) (descrizione "Il dispositivo, essendo un pc fisso, non possiede una batteria di alimentazione.")))
  (assert (nodo (nome cavi-display-accessibili) (valore si) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1) (descrizione "I cavi che collegano il dispositivo al display sono accessibili.")))
  (assert (nodo (nome alimentatore-caricabatterie) (valore no) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1) (descrizione "Il dispositivo non possiede un alimentatore caricabatterie esterno.")))
)

(defrule portatile-ha-batteria
  ; ;(fase 2-analisi)
  ?p1 <- (nodo (nome tipo-dispositivo) (valore pc-portatile) (certezza ?crt1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome ha-batteria) (valore sconosciuto) (certezza ?crt2) (id-nodo ?id-p2))
  =>
  (bind ?crt-ha-batteria (calcola-certezza 0.8 ?crt1 ?crt2))
  (bind ?crt-non-ha-batteria (calcola-certezza 0.3 ?crt1 ?crt2))
  (assert (nodo (nome ha-batteria) (valore si) (certezza ?crt-ha-batteria) (nodo-padre ?id-p1 ?id-p2) (descrizione "Essendo il dispositivo un portatile, e' probabile che la batteria sia inserita.")))
  (assert (nodo (nome ha-batteria) (valore no) (certezza ?crt-non-ha-batteria) (nodo-padre ?id-p1 ?id-p2) (descrizione "Essendo il dispositivo un portatile, c'e' una piccola probabilita' che la batteria non sia inserita.")))
)

; (defrule utente-inesperto
;   ; ;(fase 2-analisi)
;   ?p1 <- (nodo (nome esperienza-utente) (valore utente-inesperto) (id-nodo ?id-p1))
;   =>
;   (assert (nodo (nome problema-principale) (valore analisi-guidata) (nodo-padre ?id-p1)))
; )

(defrule eta-dispositivo-sconosciuta
  ; ;(fase 2-analisi)
  ?p1 <- (nodo (nome anni-dispositivo) (valore sconosciuto) (sorgente-info utente) (certezza ?crt1) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome anni-dispositivo) (valore 2-5-anni)    (certezza (* 0.75 ?crt1)) (nodo-padre ?id-p1)))
  (assert (nodo (nome anni-dispositivo) (valore meno-2-anni) (certezza (* 0.25 ?crt1)) (nodo-padre ?id-p1)))
  (assert (nodo (nome anni-dispositivo) (valore 5-10-anni)   (certezza (* 0.35 ?crt1)) (nodo-padre ?id-p1)))
  (assert (nodo (nome anni-dispositivo) (valore piu-10-anni) (certezza (* 0.10 ?crt1)) (nodo-padre ?id-p1)))
)

(defrule garanzia-2-anni
  ; ;(fase 2-analisi)
  ?p1 <- (nodo (nome anni-dispositivo) (valore meno-2-anni) (sorgente-info utente) (certezza ?crt1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome garanzia) (valore sconosciuto) (certezza ?crt2) (id-nodo ?id-p2))
  =>
  (bind ?crt (calcola-certezza 0.8 ?crt1 ?crt2))
  (assert (nodo (nome garanzia) (valore si) (certezza ?crt) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule garanzia-5-anni
  ; ;(fase 2-analisi)
  ?p1 <- (nodo (nome anni-dispositivo) (valore 2-5-anni) (sorgente-info utente) (certezza ?crt1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome garanzia) (valore sconosciuto) (certezza ?crt2) (id-nodo ?id-p2))
  =>
  (bind ?crt (calcola-certezza 0.4 ?crt1 ?crt2))
  (assert (nodo (nome garanzia) (valore si) (certezza ?crt) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule garanzia-anni-sconosciuti
  ; ;(fase 2-analisi)
  ?p1 <- (nodo (nome anni-dispositivo) (valore sconosciuto) (sorgente-info utente) (certezza ?crt1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome garanzia) (valore sconosciuto) (certezza ?crt2) (id-nodo ?id-p2))
  =>
  (bind ?crt-garanzia (calcola-certezza 0.5 ?crt1 ?crt2))
  (assert (nodo (nome garanzia) (valore si) (certezza ?crt-garanzia) (nodo-padre ?id-p1 ?id-p2)))

)

(defrule garanzia-10-anni-piu
  ; ;(fase 2-analisi)
  ?p1 <- (nodo (nome anni-dispositivo) (valore  ?val&5-10-anni|piu-10-anni) (sorgente-info utente) (certezza ?crt1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome garanzia) (valore sconosciuto) (certezza ?crt2) (id-nodo ?id-p2))
  =>
  (bind ?crt (calcola-certezza 1.0 ?crt1 ?crt2))
  (assert (nodo (nome garanzia) (valore no) (certezza ?crt) (nodo-padre ?id-p1 ?id-p2)))
)



;;REGOLE FASE 2 *******************************************

(defrule controllo-accensione
  ;(fase 2-analisi)
  ?px <- (nodo (nome nodo-di-collegamento) (valore avvia-ricerca-diagnosi) (id-nodo ?id-px) (attivo TRUE))

  ;?p1 <- (nodo (nome problema-principale) (valore ?v&analisi-guidata|accensione) (certezza ?crt1) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome chiedi) (valore controllo-accensione) (nodo-padre ?id-px)))
)

; (defrule problema-video-accensione-funzionante
;   ;(fase 2-analisi)
;   ?p1 <- (nodo (nome problema-principale) (valore video) (certezza ?crt1) (id-nodo ?id-p1))
;   =>
;   (assert (nodo (nome stato-accensione) (valore funzionante) (certezza (* 1.0 ?crt1)) (nodo-padre ?id-p1 ) (descrizione "Il dispositivo ha un problema video, quindi il dispositivo dovrebbe accendersi.")))
; )

(defrule controllo-accensione-ut-inesperto
  ;(fase 2-analisi)
  ?p1 <- (nodo (nome esperienza-utente) (valore utente-inesperto) (certezza ?crt1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome controllo-accensione) (valore possibile-non-funzionante) (certezza ?crt2) (id-nodo ?id-p2))
  =>
  (assert (nodo (nome chiedi) (valore controllo-accensione-ut-inesperto) (nodo-padre ?id-p1 ?id-p2)))
)


(defrule stato-accensione-ut-esperto
  ;(fase 2-analisi)
  ?p1 <- (nodo (nome esperienza-utente) (valore utente-esperto) (certezza ?crt1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome controllo-accensione) (valore possibile-non-funzionante) (certezza ?crt2) (id-nodo ?id-p2))
  =>
  (bind ?crt-accensione (calcola-certezza 1 ?crt1 ?crt2)) ;;livello CRT utente-esperto settato a 1
  (assert (nodo (nome stato-accensione) (valore non-funzionante) (certezza ?crt-accensione) (nodo-padre ?id-p1 ?id-p2) (descrizione "Il dispositivo non si accende, c'e' un problema con il circuito di alimentazione.")))
)

(defrule stato-accensione-funzionante
  ;(fase 2-analisi)
  ?p1 <- (nodo (nome controllo-accensione) (valore funzionante) (certezza ?crt1) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome stato-accensione) (valore funzionante) (certezza (* 1.0 ?crt1)) (nodo-padre ?id-p1 ) (descrizione "Il dispositivo si accende, il circuito di alimentazione sembra funzionare.")))
)

(defrule stato-accensione-ut-inesperto
  ;(fase 2-analisi)
  ?p1 <- (nodo (nome controllo-accensione) (valore possibile-non-funzionante) (certezza ?crt1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome controllo-accensione-ut-inesperto) (valore non-funzionante) (certezza ?crt2) (id-nodo ?id-p2))
  =>
  (bind ?crt-accensione (calcola-certezza 1 ?crt1 ?crt2)) ;;livello CRT utente-inesperto settato a 1
  (assert (nodo (nome stato-accensione) (valore non-funzionante) (certezza ?crt-accensione) (nodo-padre ?id-p1 ?id-p2) (descrizione "Il dispositivo non si accende, c'e' un problema con il circuito di alimentazione.")))
  )

(defrule stato-accensione-funzionante-con-schermo-nero
  ;(fase 2-analisi)
  ?p1 <- (nodo (nome controllo-accensione) (valore possibile-non-funzionante) (certezza ?crt1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome controllo-accensione-ut-inesperto) (valore funzionante) (certezza ?crt2) (id-nodo ?id-p2))
  =>
  (bind ?crt-schermo-nero (calcola-certezza 0.9 ?crt1 ?crt2))
  (assert (nodo (nome disturbo-video) (valore schermo-nero) (certezza ?crt-schermo-nero) (nodo-padre ?id-p1 ?id-p2) (descrizione "Il dispositivo si accende ma lo schermo e nero e non sembra dare segni di vita.")))
  (bind ?crt-accensione (calcola-certezza 1 ?crt1 ?crt2)) ;;livello CRT utente-inesperto settato a 1
  (assert (nodo (nome stato-accensione) (valore funzionante) (certezza ?crt-accensione) (nodo-padre ?id-p1 ?id-p2) (descrizione "Il dispositivo si accende, il circuito di alimentazione sembra funzionare.")))
  (bind ?crt-prob-video (calcola-certezza 1 ?crt1 ?crt2)) ;;livello CRT utente-inesperto settato a 1
  (assert (nodo (nome problema-video-dispositivo)(valore si)(certezza ?crt-prob-video)(nodo-padre ?id-p1 ?id-p2) (descrizione "Il dispositivo ha un problema al display.")))
  ;(bind ?crt-tipo-problema (calcola-certezza 0.5 ?crt1 ?crt2))
    ;(assert (nodo (nome tipologia-problema) (valore scheda-madre) (certezza ?crt-tipo-problema) (descrizione "Il problema potrebbe essere causato da un corto circuito sulla scheda madre.") (nodo-padre ?p1 ?p2)))
    ;(assert (nodo (nome tipologia-problema) (valore scheda-video) (certezza ?crt-tipo-problema) (descrizione "Il problema potrebbe essere causato da un guasto della scheda video.") (nodo-padre ?p1 ?p2)))
    ;(assert (nodo (nome tipologia-problema) (valore cavi-video) (certezza ?crt-tipo-problema) (descrizione "Il problema potrebbe essere causato da un guasto dei cavi che collegano la scheda video al display.") (nodo-padre ?p1 ?p2)))
  )

(defrule alim-funzionante-no
  ;(fase 2-analisi)
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
  ;(fase 2-analisi)
  ?p1 <- (nodo (nome alimentazione-collegata) (valore si) (certezza ?crt1) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome diagnosi) (valore alimentatore-guasto) (certezza (* 0.6 ?crt1)) (nodo-padre ?id-p1)))
  (assert (nodo (nome diagnosi) (valore scheda-madre-guasta) (certezza (* 0.3 ?crt1)) (nodo-padre ?id-p1)))
  (assert (nodo (nome diagnosi) (valore alimentazione-disconnessa) (certezza (* -1.0 ?crt1)) (nodo-padre ?id-p1)))
)

(defrule DIAGNOSI-alim-collegata-no
  ;(fase 2-analisi)
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
  ;(fase 2-analisi)
  ?p1 <- (nodo (nome ?att&spia-alimentatore-pcportatile|spia-alimentatore-pcdesktop) (valore ?v) (certezza ?c1) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome spia-alimentatore) (valore ?v) (nodo-padre ?id-p1) (certezza ?c1)))
)

(defrule spia-alimentatore-sconosciuta
  ;(fase 2-analisi)
  ?p1 <- (nodo (nome spia-alimentatore) (valore sconosciuto) (certezza ?crt1) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome spia-alimentatore) (valore accesa) (certezza (* 0.5 ?crt1)) (nodo-padre ?id-p1)))
  (assert (nodo (nome spia-alimentatore) (valore spenta) (certezza (* 0.5 ?crt1)) (nodo-padre ?id-p1)))
)

;; Per dispositivi laptop ...

(defrule chiedi-batteria-difettosa
  ;(fase 2-analisi)
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
  ;(fase 2-analisi)
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
  ;(fase 2-analisi)
  ?p1 <- (nodo (nome batteria-difettosa) (valore si) (certezza ?crt1) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome diagnosi) (valore batteria-difettosa) (certezza (* 1.0 ?crt1)) (nodo-padre ?id-p1)))
  (assert (nodo (nome diagnosi) (valore alimentatore-guasto) (certezza (* -0.5 ?crt1)) (nodo-padre ?id-p1)))
  (assert (nodo (nome diagnosi) (valore scheda-madre-guasta) (certezza (* -0.5 ?crt1)) (nodo-padre ?id-p1)))
)

(defrule chiedi-spia-alimentatore-pcportatile
  ;(fase 2-analisi)
  ;?p1 <- (nodo (nome alimentatore-funzionante) (valore no) (certezza ?crt1))
  ?p1 <- (nodo (nome tipo-dispositivo) (valore pc-portatile) (certezza ?crt1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome alimentazione-collegata) (valore si) (certezza ?crt2) (id-nodo ?id-p2))
  ?p3 <- (nodo (nome stato-accensione) (valore non-funzionante) (certezza ?crt3) (id-nodo ?id-p3))
  ;?p5 <- (nodo (nome batteria-difettosa) (valore no) (certezza ?crt5))
  =>
  (assert (nodo (nome chiedi) (valore spia-alimentatore-pcportatile) (nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)



(defrule batteria-non-presente
  ;(fase 2-analisi)
  ?p1 <- (nodo (nome ha-batteria) (valore no) (certezza ?c1) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome batteria-difettosa) (valore no) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1) (descrizione "Il dispositivo non ha una batteria.")))
  (assert (nodo (nome diagnosi) (valore batteria-difettosa) (certezza (* -1.0 ?c1)) (nodo-padre ?id-p1)))
)

(defrule DIAGNOSI-spia-alimentatore-accesa
  ;(fase 2-analisi)
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
  ;(fase 2-analisi)
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
  ;(fase 2-analisi)
  ?p1 <- (nodo (nome tipo-dispositivo) (valore pc-desktop) (certezza ?crt1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome alimentazione-collegata) (valore si) (certezza ?crt2) (id-nodo ?id-p2))
  ;?p3 <- (nodo (nome alimentatore-funzionante) (valore no) (certezza ?crt3))
  ?p3 <- (nodo (nome stato-accensione) (valore  non-funzionante) (certezza ?crt3) (id-nodo ?id-p3))
  ?p4 <- (nodo (nome interruttore-alimentatore) (valore acceso) (certezza ?crt4) (id-nodo ?id-p4))
  =>
  (assert (nodo (nome chiedi) (valore spia-alimentatore-pcdesktop) (nodo-padre ?id-p1 ?id-p2 ?id-p3 ?id-p4)))
)

(defrule chiedi-interruttore-alimentatore
  ;(fase 2-analisi)
  ?p1 <- (nodo (nome tipo-dispositivo) (valore pc-desktop) (certezza ?crt1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome alimentazione-collegata) (valore si) (certezza ?crt2) (id-nodo ?id-p2))
  ;?p3 <- (nodo (nome alimentatore-funzionante) (valore no) (certezza ?crt3))
  ?p3 <- (nodo (nome stato-accensione) (valore  non-funzionante) (certezza ?crt3) (id-nodo ?id-p3))
  =>
  (assert (nodo (nome chiedi) (valore interruttore-alimentatore) (nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule DIAGNOSI-interruttore-alimentatore-spento
  ;(fase 2-analisi)
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

(defrule chiedi-problema-video-dispositivo
  ;(fase 2-analisi)
  ;?px <- (nodo (nome nodo-di-collegamento) (valore avvia-ricerca-diagnosi) (id-nodo ?id-px) (attivo TRUE))

  ?p1 <- (nodo (nome stato-accensione)(valore funzionante)(certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))
  ;(not (nodo (nome problema-principale) (valore video) (certezza ?crt2)(attivo TRUE)(id-nodo ?id-p2)))
  (not (nodo (nome disturbo-video)(valore ?v)(certezza ?crt3&:(> ?crt3 0))(attivo TRUE)(id-nodo ?id-p3)))
  =>
  (assert (nodo (nome chiedi) (valore problema-video-dispositivo) (nodo-padre ?id-p1)))
)

(defrule chiedi-disturbo-video
  ;(fase 2-analisi)
  ; ?px <- (nodo (nome nodo-di-collegamento) (valore avvia-ricerca-diagnosi) (id-nodo ?id-px) (attivo TRUE))
  ?p1 <- (nodo (nome problema-video-dispositivo)(valore si)(certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))

  (not (nodo (nome disturbo-video) (valore ?v)(certezza ?crt2&:(> ?crt2 0))(attivo TRUE)(id-nodo ?id-p2)))
  =>
  (assert (nodo (nome chiedi) (valore disturbo-video) (nodo-padre ?id-p1)))
)

(defrule disturbo-fasce-verticali
  ;(fase 2-analisi)
  ?p1 <- (nodo(nome disturbo-video)(valore fasce-verticali)(certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))
  =>
  (assert (nodo (nome tipo-disturbo-video)(valore interferenza) (certezza (* 0.8 ?crt1))(nodo-padre ?id-p1)
          (descrizione "E' possibile ci siano interferenze sul segnale video.")))
  (assert (nodo (nome tipo-disturbo-video)(valore guasto-circuito-video)(certezza (* 0.8 ?crt1))(nodo-padre ?id-p1)
          (descrizione "E' possibile che qualche componente video come display o scheda video, sia guasta.")))
)

(defrule disturbo-linee-orizzontali
  ;(fase 2-analisi)
  ?p1 <- (nodo(nome disturbo-video)(valore linee-orizzontali)(certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))
  =>
  (assert (nodo (nome tipo-disturbo-video)(valore interferenza) (certezza (* 0.9 ?crt1))(nodo-padre ?id-p1)
          (descrizione "E' possibile ci siano interferenze sul segnale video.")))
  (assert (nodo (nome tipo-disturbo-video)(valore guasto-circuito-video)(certezza (* 0.3 ?crt1))(nodo-padre ?id-p1)
          (descrizione "E' possibile che qualche componente video come display o scheda video, sia guasta.")))
)

(defrule chiedi-cavi-display
  ;(fase 2-analisi)
  ?p1 <- (nodo (nome disturbo-video)(valore schermo-nero)(certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome cavi-display-accessibili)(valore si)(certezza ?crt2)(attivo TRUE)(id-nodo ?id-p2))
  ;?p2 <- (nodo (nome tipo-dispositivo) (valore pc-desktop))
  =>
  (assert (nodo (nome chiedi) (valore cavi-display) (nodo-padre ?id-p1 ?id-p2)))
)



(defrule disturbo-schermo-nero
  ;(fase 2-analisi)
  ?p1 <- (nodo(nome disturbo-video)(valore schermo-nero)(certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome cavi-display)(valore ok)(certezza ?crt2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (bind ?crt (calcola-certezza 0.9 ?crt1 ?crt2))
  (assert (nodo (nome tipo-disturbo-video)(valore guasto-circuito-video)(certezza ?crt)(nodo-padre ?id-p1 ?id-p2)
          (descrizione "E' possibile che qualche componente video come display o scheda video, sia guasta.")))
)

(defrule chiedi-monitor-esterno
  ;(fase 2-analisi)
  ?p1 <- (nodo (nome disturbo-video)(valore ?v1&fasce-verticali|linee-orizzontali|schermo-nero)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
  (not (nodo (nome chiedi) (valore monitor-esterno)))
  =>
  (assert (nodo (nome chiedi) (valore monitor-esterno) (nodo-padre ?id-p1)))
)

(defrule no-monitor-secondario
  ;(fase 2-analisi)
  ?p1 <- (nodo (nome monitor-esterno)(valore no)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
  =>
  (assert (nodo (nome monitor-esterno) (valore funzionante)(certezza (* 0.5 ?c1))(nodo-padre ?id-p1)))
  (assert (nodo (nome monitor-esterno) (valore errore)(certezza (* 0.5 ?c1))(nodo-padre ?id-p1)))
)

(defrule chiedi-problema-video-all-avvio
  ;(fase 2-analisi)
  ;?p1 <- (nodo (nome disturbo-video) (valore ?v&fasce|linee-oriz))
  ?p1 <- (nodo (nome disturbo-video)(valore ?v1&fasce-verticali|linee-orizzontali|schermo-nero)(certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))
  (not (nodo (nome chiedi) (valore problema-video-all-avvio)))
  =>
  (assert (nodo (nome chiedi) (valore problema-video-all-avvio) (nodo-padre ?id-p1)))
)




(defrule diagnosi-problema-SW-video-1
  ?p1 <- (nodo (nome disturbo-video)(valore fasce-verticali)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome problema-video-all-avvio) (valore no)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (bind ?crt-SW (calcola-certezza 0.75 ?c1 ?c2))
  (assert (nodo (nome diagnosi)(valore problema-SW-video)(certezza ?crt-SW)(nodo-padre ?id-p1 ?id-p2)))
)
(defrule diagnosi-problema-SW-video-2
  ?p1 <- (nodo (nome disturbo-video)(valore linee-orizzontali)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome problema-video-all-avvio) (valore no)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (bind ?crt-SW (calcola-certezza 0.5 ?c1 ?c2))
  (assert (nodo (nome diagnosi)(valore problema-SW-video)(certezza ?crt-SW)(nodo-padre ?id-p1 ?id-p2)))
)
(defrule diagnosi-problema-SW-video-3
  ?p1 <- (nodo (nome disturbo-video)(valore schermo-nero)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome problema-video-all-avvio) (valore no)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (bind ?crt-SW (calcola-certezza 0.9 ?c1 ?c2))
  (assert (nodo (nome diagnosi)(valore problema-SW-video)(certezza ?crt-SW)(nodo-padre ?id-p1 ?id-p2)))
)

(defrule diagnosi-display-guasto
  ?p1 <- (nodo (nome tipo-disturbo-video) (valore guasto-circuito-video)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
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
  ?p1 <- (nodo (nome tipo-disturbo-video) (valore ?v1&guasto-circuito-video|interferenza)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
    (not (nodo (nome tipo-disturbo-video) (valore ?v2&guasto-circuito-video|interferenza)(certezza ?c1b&:(> ?c1b ?c1))(attivo TRUE)(id-nodo ?id2)))
    (not (nodo (nome tipo-disturbo-video) (valore ?v3&guasto-circuito-video|interferenza)(certezza ?c1c&:(eq ?c1c ?c1))(attivo TRUE)(id-nodo ?id3&:(> ?id3 ?id-p1))))

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


(defrule diagnosi-interferenze-cavo
  ?p1 <- (nodo (nome tipo-disturbo-video)(valore interferenza)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome problema-video-all-avvio)(valore si)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (bind ?crt (calcola-certezza 0.9 ?c1 ?c2))
  (assert (nodo (nome diagnosi) (valore interferenze-cavo)(certezza ?crt)(nodo-padre ?id-p1 ?id-p2)))
)


(defrule diagnosi-guasto-vga
  ?p1 <- (nodo (nome tipo-disturbo-video) (valore guasto-circuito-video)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
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
  ;(fase 2-analisi)
  ?p1 <- (nodo (nome disturbo-video)(valore macchie)(certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))
  =>
  (assert (nodo (nome diagnosi) (valore guasto-display)(certezza (* 0.95 ?crt1))(nodo-padre ?id-p1)))
)

(defrule cavi-display-portatile
  ;(fase 2-analisi)
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
  ;(fase 2-analisi)
  ?p1 <- (nodo(nome disturbo-video)(valore schermo-nero)(certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome cavi-display)(valore errore)(certezza ?crt2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (bind ?crt (calcola-certezza 1.0 ?crt1 ?crt2))
  (assert (nodo (nome diagnosi) (valore cavi-display-scollegati)(certezza ?crt)(nodo-padre ?id-p1 ?id-p2)))
)

(defrule diagnosi-CMOS-corrotta
  ;(fase 2-analisi)
  ?p1 <- (nodo(nome disturbo-video)(valore schermo-nero)(certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome cavi-display)(valore ok)(certezza ?crt2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (bind ?crt (calcola-certezza 0.5 ?crt1 ?crt2))
  (assert (nodo (nome diagnosi)(valore CMOS-corrotta)(certezza ?crt)(nodo-padre ?id-p1 ?id-p2)))
)

;; REGOLE POST BIOS********************



(defrule chiedi-problema-POST-ut-esperto
  ;(fase 2-analisi)
  ;?px <- (nodo (nome nodo-di-collegamento) (valore avvia-ricerca-diagnosi) (id-nodo ?id-px) (attivo TRUE))

  ?p1 <- (nodo (nome stato-accensione)(valore funzionante)(certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))
  ;?p2 <- (nodo (nome problema-principale) (valore analisi-guidata) (certezza ?crt2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo (nome problema-video-dispositivo)(valore no)(certezza ?crt3)(attivo TRUE)(id-nodo ?id-p3))
  ?p4 <- (nodo (nome esperienza-utente) (valore utente-esperto)(certezza ?crt4)(attivo TRUE)(id-nodo ?id-p4))
  =>
  (assert (nodo (nome chiedi) (valore fase-POST-ut-esperto) (nodo-padre ?id-p1 ?id-p3 ?id-p4)))
)

(defrule chiedi-problema-POST-ut-inesperto
  ;(fase 2-analisi)
  ;?px <- (nodo (nome nodo-di-collegamento) (valore avvia-ricerca-diagnosi) (id-nodo ?id-px) (attivo TRUE))

  ?p1 <- (nodo (nome stato-accensione)(valore funzionante)(certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))
  ;?p2 <- (nodo (nome problema-principale) (valore analisi-guidata) (certezza ?crt2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo (nome problema-video-dispositivo)(valore no)(certezza ?crt3)(attivo TRUE)(id-nodo ?id-p3))
  ?p4 <- (nodo (nome esperienza-utente) (valore utente-inesperto)(certezza ?crt4)(attivo TRUE)(id-nodo ?id-p4))
  =>
  (assert (nodo (nome chiedi) (valore fase-POST-ut-inesperto) (nodo-padre ?id-p1 ?id-p3 ?id-p4)))
)

; (defrule chiedi-problema-POST-ut-esperto2
;   ;(fase 2-analisi)
;   ;?px <- (nodo (nome nodo-di-collegamento) (valore avvia-ricerca-diagnosi) (id-nodo ?id-px) (attivo TRUE))
;
;   ;?p1 <- (nodo (nome problema-principale)(valore caricamento-SO)(certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))
;   ?p2 <- (nodo (nome esperienza-utente) (valore utente-esperto)(certezza ?crt2)(attivo TRUE)(id-nodo ?id-p2))
;   =>
;   (assert (nodo (nome chiedi) (valore fase-POST-ut-esperto) (nodo-padre ?id-px ?id-p1 ?id-p2)))
; )



(defrule combina-chiedi-problema-POST
  ;(fase 2-analisi)
  ?p1 <- (nodo(nome ?n&fase-POST-ut-esperto|fase-POST-ut-inesperto)(valore ?v)(certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))
  =>
  (assert (nodo (nome fase-POST) (valore ?v)(certezza ?crt1)(nodo-padre ?id-p1)))
)


(defrule diagnosi-errore-messaggio-POST
  ;(fase 2-analisi)
  ?p1 <- (nodo(nome fase-POST)(valore errore-messaggio)(certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))
  =>
  (assert (nodo (nome diagnosi) (valore errore-POST-hardware)(certezza (* 0.85 ?crt1))(nodo-padre ?id-p1)))
  (assert (nodo (nome diagnosi) (valore CMOS-corrotta)(certezza (* 0.5 ?crt1))(nodo-padre ?id-p1)))
  (assert (nodo (nome diagnosi) (valore errore-POST-boot)(certezza (* 0.5 ?crt1))(nodo-padre ?id-p1)))
)

(defrule diagnosi-errore-beep-code
  ;(fase 2-analisi)
  ?p1 <- (nodo(nome fase-POST)(valore errore-beep-code)(certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))
  =>
  (assert (nodo (nome diagnosi) (valore errore-POST-hardware)(certezza (* 0.85 ?crt1))(nodo-padre ?id-p1)))
  (assert (nodo (nome diagnosi) (valore CMOS-corrotta)(certezza (* 0.5 ?crt1))(nodo-padre ?id-p1)))
)

(defrule chiedi-installazione-nuovo-HW-SW
  ;(fase 2-analisi)
  ?p1 <- (nodo(nome fase-POST)(valore errore-BSOD)(certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))
  =>
  (assert (nodo (nome chiedi) (valore conflitto-HW-SW) (nodo-padre ?id-p1)))
)

(defrule diagnosi-errore-BSOD
  ;(fase 2-analisi)
  ?p1 <- (nodo(nome fase-POST)(valore errore-BSOD)(certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))
  =>
  (assert (nodo (nome diagnosi) (valore infetto-da-virus)(certezza (* 0.3 ?crt1))(nodo-padre ?id-p1)))
  (assert (nodo (nome diagnosi) (valore CMOS-corrotta)(certezza (* 0.5 ?crt1))(nodo-padre ?id-p1)))
  (assert (nodo (nome diagnosi) (valore problema-caricamento-SO)(certezza (* 0.9 ?crt1))(nodo-padre ?id-p1)))
)

(defrule diagnosi-errore-BSOD-conflitto-HW
  ;(fase 2-analisi)
  ?p1 <- (nodo(nome fase-POST)(valore errore-BSOD)(certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome conflitto-HW-SW)(valore si-HW)(certezza ?crt2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (bind ?crt (calcola-certezza 0.95 ?crt1 ?crt2))
  (assert (nodo (nome diagnosi) (valore conflitto-HW)(certezza ?crt)(nodo-padre ?id-p1 ?id-p2)))
)

(defrule diagnosi-errore-BSOD-conflitto-SW
  ;(fase 2-analisi)
  ?p1 <- (nodo(nome fase-POST)(valore errore-BSOD)(certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome conflitto-HW-SW)(valore ?v&si-SW|si-aggiornamenti)(certezza ?crt2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (bind ?crt (calcola-certezza 0.8 ?crt1 ?crt2))
  (assert (nodo (nome diagnosi) (valore conflitto-SW)(certezza ?crt)(nodo-padre ?id-p1 ?id-p2)))
)

(defrule diagnosi-errore-BSOD-conflitto-sconosciuto
  ;(fase 2-analisi)
  ?p1 <- (nodo(nome fase-POST)(valore errore-BSOD)(certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome conflitto-HW-SW)(valore non-so)(certezza ?crt2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (bind ?crt (calcola-certezza 0.5 ?crt1 ?crt2))
  (assert (nodo (nome diagnosi) (valore conflitto-SW)(certezza ?crt)(nodo-padre ?id-p1 ?id-p2)))
  (assert (nodo (nome diagnosi) (valore conflitto-HW)(certezza ?crt)(nodo-padre ?id-p1 ?id-p2)))
)


(defrule diagnosi-errore-riavvio
  ;(fase 2-analisi)
  ?p1 <- (nodo(nome fase-POST)(valore errore-riavvio)(certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))
  =>
  (assert (nodo (nome diagnosi) (valore errore-POST-hardware)(certezza (* 0.75 ?crt1))(nodo-padre ?id-p1)))
  (assert (nodo (nome diagnosi) (valore CMOS-corrotta)(certezza (* 0.5 ?crt1))(nodo-padre ?id-p1)))
  (assert (nodo (nome diagnosi) (valore errore-POST-boot)(certezza (* 0.5 ?crt1))(nodo-padre ?id-p1)))
  (assert (nodo (nome diagnosi) (valore guasto-alimentatore)(certezza (* 0.5 ?crt1))(nodo-padre ?id-p1)))
  (assert (nodo (nome diagnosi) (valore guasto-scheda-madre)(certezza (* 0.2 ?crt1))(nodo-padre ?id-p1)))
)

(defrule chiedi-inattivita-dispositivo
  ;(fase 2-analisi)
  ?p1 <- (nodo(nome fase-POST)(valore ?v&errore-messaggio|errore-beep-code|errore-riavvio)(certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))
  =>
  (assert (nodo (nome chiedi) (valore inattivita-dispositivo) (nodo-padre ?id-p1)))
)

(defrule diagnosi-batteria-CMOS-esausta-1
  ;(fase 2-analisi)
  ?p1 <- (nodo(nome fase-POST)(valore ?v&errore-messaggio|errore-beep-code|errore-riavvio)(certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome inattivita-dispositivo)(valore si)(certezza ?crt2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (bind ?crt (calcola-certezza 0.5 ?crt1 ?crt2))
  (assert (nodo (nome diagnosi)(valore batteria-CMOS-esausta)(certezza ?crt)(nodo-padre ?id-p1 ?id-p2)))
)

(defrule diagnosi-batteria-CMOS-esausta-2
  ;(fase 2-analisi)
  ?p1 <- (nodo(nome fase-POST)(valore ?v&errore-messaggio|errore-beep-code|errore-riavvio)(certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome anni-dispositivo)(valore piu-10-anni)(certezza ?crt2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (bind ?crt (calcola-certezza 0.9 ?crt1 ?crt2))
  (assert (nodo (nome diagnosi)(valore batteria-CMOS-esausta)(certezza ?crt)(nodo-padre ?id-p1 ?id-p2)))
)

(defrule diagnosi-batteria-CMOS-esausta-3
  ;(fase 2-analisi)
  ?p1 <- (nodo(nome fase-POST)(valore ?v&errore-messaggio|errore-beep-code|errore-riavvio)(certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome anni-dispositivo)(valore 5-10-anni)(certezza ?crt2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (bind ?crt (calcola-certezza 0.7 ?crt1 ?crt2))
  (assert (nodo (nome diagnosi)(valore batteria-CMOS-esausta)(certezza ?crt)(nodo-padre ?id-p1 ?id-p2)))
)

; (defrule POST-nessun-errore-ut-esperto
;   ;(fase 2-analisi)
;   ?p1 <- (nodo(nome fase-POST)(valore nessun-errore)(certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))
;   ?p2 <- (nodo (nome esperienza-utente) (valore utente-esperto)(certezza ?crt2)(attivo TRUE)(id-nodo ?id-p2))
;   =>
;   (assert (nodo (nome chiedi) (valore fase-caricamento-SO-ut-esperto) (nodo-padre ?id-p1)))
; )
;
; (defrule POST-nessun-errore-ut-inesperto
;   ;(fase 2-analisi)
;   ?p1 <- (nodo(nome fase-POST)(valore nessun-errore)(certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))
;   ?p2 <- (nodo (nome esperienza-utente) (valore utente-inesperto)(certezza ?crt2)(attivo TRUE)(id-nodo ?id-p2))
;   =>
;   (assert (nodo (nome chiedi) (valore fase-caricamento-SO-ut-inesperto) (nodo-padre ?id-p1)))
; )
;
; (defrule combina-chiedi-problema-caricamento-SO
;   ;(fase 2-analisi)
;   ?p1 <- (nodo(nome ?n&fase-caricamento-SO-ut-esperto|fase-caricamento-SO-ut-inesperto)(valore ?v)(certezza ?crt1)(attivo TRUE)(id-nodo ?id-p1))
;   =>
;   (assert (nodo (nome fase-caricamento-SO) (valore ?v)(certezza ?crt1)(nodo-padre ?id-p1)))
; )





;; FASE 4 SOLUZIONI ********************

(defrule guasto-hardware-si
  (fase-cerca-soluzioni)
  ?p1 <- (nodo (id-nodo ?id-p1) (attivo TRUE) (nome diagnosi) (valore ?v1&batteria-difettosa|alimentatore-guasto|scheda-madre-guasta|guasto-display|guasto-vga|guasto-inverter) (certezza ?c1))
      ;; assicura che p1 sia il nodo con certezza piu' alta
  (not (nodo (id-nodo ?id-p2) (attivo TRUE) (nome diagnosi) (valore ?v2&batteria-difettosa|alimentatore-guasto|scheda-madre-guasta|guasto-display|guasto-vga|guasto-inverter) (certezza ?c2&:(> ?c2 ?c1))))
      ;; se abbiamo due o piu' nodi con la stessa certezza massima, prendiamo sempre il nodo con id piu' alto per evitare di attivare la stessa regola piu' volte
  (not (nodo (id-nodo ?id-p3&:(> ?id-p3 ?id-p1))(attivo TRUE) (nome diagnosi) (valore ?v3&batteria-difettosa|alimentatore-guasto|scheda-madre-guasta|guasto-display|guasto-vga|guasto-inverter)(certezza ?c3&:(eq ?c3 ?c1))))
  =>
  (assert (nodo (nome guasto-hardware) (valore si) (certezza (* 1 ?c1)) (nodo-padre ?id-p1) (descrizione "Almeno una componente hardware del dispositivo e' guasta.")))
)

(defrule soluzione-dispositivo-in-assistenza
    (fase-cerca-soluzioni)
    ?p1 <- (nodo (nome guasto-hardware) (valore si) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
    ?p2 <- (nodo (nome garanzia) (valore si) (certezza ?c2) (attivo TRUE) (id-nodo ?id-p2))
    =>
    (bind ?crt (calcola-certezza 1 ?c1 ?c2))
    (assert (nodo (nome soluzione) (valore dispositivo-in-assistenza) (certezza ?crt) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule soluzione-sostituzione-dispositivo-obsoleto
    (fase-cerca-soluzioni)
    ?p1 <- (nodo (nome guasto-hardware) (valore si) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
    ?p2 <- (nodo (nome anni-dispositivo) (valore piu-10-anni) (certezza ?c2) (attivo TRUE) (id-nodo ?id-p2))
    =>
    (bind ?crt (calcola-certezza 0.8 ?c1 ?c2))
    (assert (nodo (nome soluzione) (valore sostituzione-dispositivo) (certezza ?crt) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule soluzione-sostituisci-alimentatore
    (fase-cerca-soluzioni)
    ?p1 <- (nodo (nome diagnosi) (valore alimentatore-guasto) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
    =>
    (assert (nodo (nome soluzione) (valore sostituisci-alimentatore) (certezza (* 0.95 ?c1)) (nodo-padre ?id-p1)))
)

(defrule soluzione-sostituisci-scheda-madre
  (fase-cerca-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore scheda-madre-guasta) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome tipo-dispositivo) (valore pc-desktop) (certezza ?c2) (attivo TRUE) (id-nodo ?id-p2))
  =>
  (bind ?crt (calcola-certezza 0.9 ?c1 ?c2))
  (assert (nodo (nome soluzione) (valore sostituisci-scheda-madre) (certezza ?crt) (nodo-padre ?id-p1)))
)

(defrule soluzione-sostituisci-portatile
  (fase-cerca-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore scheda-madre-guasta) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome tipo-dispositivo) (valore pc-portatile) (certezza ?c2) (attivo TRUE) (id-nodo ?id-p2))
  =>
  (bind ?crt (calcola-certezza 0.75 ?c1 ?c2))
  (assert (nodo (nome soluzione) (valore sostituzione-dispositivo) (certezza ?crt) (nodo-padre ?id-p1)))
)

(defrule soluzione-riparazione-scheda-madre
  (fase-cerca-soluzioni)
  ?p1 <- (nodo (id-nodo ?id-p1) (attivo TRUE) (nome anni-dispositivo) (valore ?v1&meno-2-anni|2-5-anni) (certezza ?c1))
  (not (nodo (id-nodo ?id-p2) (attivo TRUE) (nome anni-dispositivo) (valore ?v2&meno-2-anni|2-5-anni) (certezza ?c2&:(> ?c2 ?c1))))
  (not (nodo (id-nodo ?id-p3&:(> ?id-p3 ?id-p1))(attivo TRUE) (nome anni-dispositivo) (valore ?v3&meno-2-anni|2-5-anni)(certezza ?c3&:(eq ?c3 ?c1))))
  ?p4 <- (nodo (nome diagnosi) (valore scheda-madre-guasta) (certezza ?c4) (attivo TRUE) (id-nodo ?id-p4))
  =>
  (bind ?crt (calcola-certezza 0.95 ?c1 ?c4))
  (assert (nodo (nome soluzione) (valore riparazione-scheda-madre) (certezza ?crt) (nodo-padre ?id-p1 ?id-p4)))
)

(defrule soluzione-accendi-alimentatore
  (fase-cerca-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore alimentatore-spento) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome soluzione) (valore accendi-alimentatore) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1)))
)

(defrule soluzione-connetti-alimentazione
  (fase-cerca-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore alimentazione-disconnessa) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome soluzione) (valore connetti-alimentazione) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1)))
)

;;SOLUZIONI VIDEO

(defrule soluzione-connetti-cavi-video
  (fase-cerca-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore cavi-display-scollegati) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome soluzione) (valore connetti-cavi-video) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1)))
)

(defrule soluzione-sostituisci-display
  (fase-cerca-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore guasto-display) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome soluzione) (valore sostituisci-display) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1)))
)

(defrule soluzione-sostituisci-vga
  (fase-cerca-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore guasto-vga) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome soluzione) (valore sostituisci-vga) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1)))
)

(defrule soluzione-controllo-cavi-video
  (fase-cerca-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore interferenze-cavo) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome cavi-display-accessibili)(valore si)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (bind ?crt (calcola-certezza 0.9 ?c1 ?c2))
  (assert (nodo (nome soluzione) (valore controllo-cavi-video) (certezza ?crt) (nodo-padre ?id-p1)))
)

(defrule soluzione-controllo-cavi-video-portatile
  (fase-cerca-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore interferenze-cavo) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome cavi-display-accessibili)(valore no)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (bind ?crt (calcola-certezza 0.7 ?c1 ?c2))
  (assert (nodo (nome soluzione) (valore controllo-cavi-video-portatile) (certezza ?crt) (nodo-padre ?id-p1)))
)

(defrule soluzione-sostituisci-inverter
  (fase-cerca-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore guasto-inverter) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome soluzione) (valore sostituisci-inverter) (certezza (* 0.9 ?c1)) (nodo-padre ?id-p1)))
)

(defrule soluzione-problema-SW-linee-orizzontali
  (fase-cerca-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore problema-SW-video) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome disturbo-video)(valore linee-orizzontali) (certezza ?c2) (attivo TRUE) (id-nodo ?id-p2))
  =>
  (bind ?crt (calcola-certezza 0.95 ?c1 ?c2))
  (assert (nodo (nome soluzione) (valore aggiorna-driver-video) (certezza ?crt) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule soluzione-problema-SW-fasce-verticali
  (fase-cerca-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore problema-SW-video) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome disturbo-video)(valore fasce-verticali) (certezza ?c2) (attivo TRUE) (id-nodo ?id-p2))
  =>
  (bind ?crt (calcola-certezza 0.95 ?c1 ?c2))
  (assert (nodo (nome soluzione) (valore aggiorna-driver-video) (certezza ?crt) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule soluzione-problema-SW-schermo-nero
  (fase-cerca-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore problema-SW-video) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome disturbo-video)(valore schermo-nero) (certezza ?c2) (attivo TRUE) (id-nodo ?id-p2))
  =>
  (bind ?crt-driver (calcola-certezza 0.75 ?c1 ?c2))
  (bind ?crt-files  (calcola-certezza 0.5 ?c1 ?c2))
  (bind ?crt-SO     (calcola-certezza 0.75 ?c1 ?c2))
  (assert (nodo (nome soluzione) (valore aggiorna-driver-video) (certezza ?crt-driver) (nodo-padre ?id-p1 ?id-p2)))
  (assert (nodo (nome soluzione) (valore controllo-integrita-files) (certezza ?crt-files) (nodo-padre ?id-p1 ?id-p2)))
  (assert (nodo (nome soluzione) (valore ripara-file-SO) (certezza ?crt-SO) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule soluzione-resetta-CMOS
  (fase-cerca-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore CMOS-corrotta) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome soluzione) (valore resetta-CMOS) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1)))
)


;; SOLUZIONI BOOT

(defrule soluzione-sostituisci-batteria-CMOS
  (fase-cerca-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore batteria-CMOS-esausta) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome soluzione) (valore sostituisci-batteria-CMOS) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1)))
)

(defrule soluzione-controlla-scheda-POST
  (fase-cerca-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore errore-POST-hardware) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome fase-POST)(valore errore-riavvio) (certezza ?c2) (attivo TRUE) (id-nodo ?id-p2))
  =>
  (bind ?crt (calcola-certezza 0.9 ?c1 ?c2))
  (assert (nodo (nome soluzione) (valore controlla-scheda-POST) (certezza ?crt) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule soluzione-controlla-beep-code
  (fase-cerca-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore errore-POST-hardware) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome fase-POST)(valore errore-beep-code) (certezza ?c2) (attivo TRUE) (id-nodo ?id-p2))
  =>
  (bind ?crt (calcola-certezza 1 ?c1 ?c2))
  (assert (nodo (nome soluzione) (valore controlla-beep-code) (certezza ?crt) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule soluzione-controlla-messaggio-POST
  (fase-cerca-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore errore-POST-hardware) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome fase-POST)(valore errore-messaggio) (certezza ?c2) (attivo TRUE) (id-nodo ?id-p2))
  =>
  (bind ?crt (calcola-certezza 1 ?c1 ?c2))
  (assert (nodo (nome soluzione) (valore controlla-messaggio-post) (certezza ?crt) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule soluzione-rimuovi-memorie-esterne
  (fase-cerca-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore errore-POST-boot) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome soluzione) (valore rimuovi-memorie-esterne) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1)))
)


(defrule soluzione-scansione-antivirus
  (fase-cerca-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore infetto-da-virus) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome soluzione) (valore scansione-antivirus) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1)))
)

(defrule soluzione-problema-caricamento-SO-BSOD
  (fase-cerca-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore problema-caricamento-SO) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome fase-POST)(valore errore-BSOD) (certezza ?c2) (attivo TRUE) (id-nodo ?id-p2))
  =>
  (bind ?crt-ripara-file (calcola-certezza 0.7 ?c1 ?c2))
  (bind ?crt-stop-error (calcola-certezza 1.0 ?c1 ?c2))
  (bind ?crt-ripristina-SO (calcola-certezza 0.7 ?c1))
  (assert (nodo (nome soluzione) (valore ripara-file-SO) (certezza ?crt-ripara-file) (nodo-padre ?id-p1 ?id-p2)))
  (assert (nodo (nome soluzione) (valore controlla-stop-error) (certezza ?crt-stop-error) (nodo-padre ?id-p1 ?id-p2)))
  (assert (nodo (nome soluzione) (valore ripristina-configurazione-sistema) (certezza ?crt-ripristina-SO) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule soluzione-conflitto-HW
  (fase-cerca-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore conflitto-HW) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (bind ?crt-ripristina-SO (calcola-certezza 0.7 ?c1))
  (bind ?crt-rimuovi-HW (calcola-certezza 1.0 ?c1))
  (assert (nodo (nome soluzione) (valore ripristina-configurazione-sistema) (certezza ?crt-ripristina-SO) (nodo-padre ?id-p1)))
  (assert (nodo (nome soluzione) (valore rimuovi-HW-installato) (certezza ?crt-rimuovi-HW) (nodo-padre ?id-p1)))
)

(defrule soluzione-conflitto-SW
  (fase-cerca-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore conflitto-SW) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (bind ?crt-ripristina-SO (calcola-certezza 0.7 ?c1))
  (bind ?crt-rimuovi-SW (calcola-certezza 0.9 ?c1))
  (assert (nodo (nome soluzione) (valore ripristina-configurazione-sistema) (certezza ?crt-ripristina-SO) (nodo-padre ?id-p1)))
  (assert (nodo (nome soluzione) (valore rimuovi-SW-installato) (certezza ?crt-rimuovi-SW) (nodo-padre ?id-p1)))
)
