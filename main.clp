

(defglobal ?*highest-priority* = 1000)
(defglobal ?*high-priority* = 100)
(defglobal ?*low-priority* = -100)
(defglobal ?*lowest-priority* = -1000)

(defglobal ?*id* = 0)
(defglobal ?*num-domande-chieste* = 0)

(deffunction next-id ()
   (bind ?*id* (+ ?*id* 1))
   (return ?*id*)
)

(deffunction next-num-domanda ()
   (bind ?*num-domande-chieste* (+ ?*num-domande-chieste* 1))
   (return ?*num-domande-chieste*)
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
    (slot descrizione (type STRING) (default "desc placeholder"))
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
    (multislot spiegazione-risposte (type STRING) (default ?NONE))
    (slot spiegazione (type STRING) (default "placeholder spiegazione"))
    (slot help (type STRING) (default "placeholder help"))
  )

  (deftemplate diagnosi
    (slot attributo   (type SYMBOL))
    (slot titolo      (type STRING))
    (slot descrizione (type STRING))
  )

  (deftemplate soluzione
    (slot attributo   (type SYMBOL))
    (slot titolo      (type STRING))
    (slot descrizione (type STRING))
  )


(defmodule MAIN(export ?ALL))

;;****************
;;* DEFFUNCTIONS *
;;****************

; (deffunction MAIN::ask-question (?num-domanda ?nodo-domanda)
;   (bind ?testo-domanda (fact-slot-value ?nodo-domanda testo-domanda))
;   (bind ?spiegazione (fact-slot-value ?nodo-domanda spiegazione))
;   (bind ?aiuto (fact-slot-value ?nodo-domanda help))
;   (bind ?descrizioni (fact-slot-value ?nodo-domanda descrizione-risposte))
;
;   (clear-window)
;   (printout t crlf crlf)
;   (printout t   "***                                                 ***" crlf
;                 "**  SISTEMA DIAGNOSTICO PER DISPOSITIVI ELETTRONICI  **" crlf
;                 "*                                                     *" crlf
;                 "*     Rispondere alle domande inserendo il numero     *" crlf
;                 "**       corrispondente alla risposta corretta.      **" crlf
;                 "***                                                 ***" crlf crlf)
;
;   (printout t "***** DOMANDA N." ?num-domanda " *****" crlf)
;   (format t "%s%n%n" ?testo-domanda)
;   (loop-for-count (?cnt1 1 (length ?descrizioni)) do
;       (printout t ?cnt1 ". " (nth$ ?cnt1 ?descrizioni) crlf)
;   )
;   (printout t crlf "9. Perche' questa domanda?")
;   (printout t crlf "0. Aiutami a rispondere a questa domanda." crlf crlf)
;   (printout t "Inserire risposta: ")
;   (bind ?answer (read))
;   (if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))
;
;   (while (not (and (>= ?answer 1) (<= ?answer (length ?descrizioni))))
;       (if (= ?answer 0) then (printout t ?aiuto crlf crlf))
;       (if (= ?answer 9) then (printout t ?spiegazione crlf crlf))
;       (printout t "Inserire risposta: ")
;       (bind ?answer (read))
;       (if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))
;   )
;   (printout t crlf crlf)
;   (return ?answer)
; )


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
  )
  (if (and (< ?cf1 0) (< ?cf2 0)) then (bind ?CF (+ (+ ?cf1 ?cf2) (* ?cf1 ?cf2)))
  )
  (if (< (* ?cf1 ?cf2) 0) then (bind ?CF (/ (+ ?cf1 ?cf2) (- 1 (min (abs ?cf1)(abs ?cf2)))))
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
  (load "moduli/modulo-stampa.clp")
  (load "moduli/modulo-soluzione.clp")
  (load "moduli/modulo-ritrattazione.clp")
  (clear-window)
  ;(assert (contatore-domande 0))
  (set-strategy random)
  (assert (in-esecuzione))
)





(defrule MAIN::profilazione-non-completata
  (declare (salience ?*highest-priority*))
  (or
    (not (nodo (nome esperienza-utente) (sorgente-info utente)))
    (not (nodo (nome anni-dispositivo)  (sorgente-info utente)))
    (not (nodo (nome garanzia)  (sorgente-info utente)))
    (not (nodo (nome tipo-dispositivo)  (sorgente-info utente)))
    ;(not (nodo (nome ha-batteria) (id-nodo ?id-p6) (attivo TRUE)))
    (not (nodo (nome utente-proprietario)  (sorgente-info utente)))
  )
  ;(not (nodo (nome ?v&utente-proprietario|tipo-dispositivo|garanzia|anni-dispositivo|esperienza-utente)  (sorgente-info utente)))
  =>
  (set-strategy random)
  (printout t "SET STRATEGY RANDOM" crlf)
  )


(defrule MAIN::profilazione-completata
  (declare (salience ?*highest-priority*))
  (nodo (nome esperienza-utente) (id-nodo ?id-p1) (attivo TRUE))
  (nodo (nome anni-dispositivo) (id-nodo ?id-p3) (attivo TRUE))
  (nodo (nome garanzia) (id-nodo ?id-p4) (attivo TRUE))
  (nodo (nome tipo-dispositivo) (id-nodo ?id-p5) (attivo TRUE))
  ;(nodo (nome ha-batteria) (id-nodo ?id-p6) (attivo TRUE))
  (nodo (nome utente-proprietario) (id-nodo ?id-p7) (attivo TRUE))
  =>
  (set-strategy complexity)
  ;(assert (nodo (nome nodo-di-collegamento) (valore avvia-ricerca-diagnosi) (nodo-padre ?id-p1 ?id-p3 ?id-p4 ?id-p5 ?id-p7)))

  (printout t "SET STRATEGY COMPLEXITY" crlf)
  )


; (defrule MAIN::fine-domande-passa-a-ricerca-soluzioni
;   (declare (salience ?*lowest-priority*))
;   ;(nodo (nome chiedi) (valore ?dom))
;   ;(not (domanda (attributo ?dom) (gia-chiesta FALSE)))
;   (not (fase-cerca-soluzioni))
;   =>
;   (assert (fase-cerca-soluzioni))
;   (focus MODULO-SOLUZIONE)
; )
;
; (defrule MAIN::fine-ricerca-soluzioni-passa-a-stampa
;   (declare (salience ?*lowest-priority*))
;   ;(nodo (nome chiedi) (valore ?dom))
;   ;(not (domanda (attributo ?dom) (gia-chiesta FALSE)))
;   ?f <- (fase-cerca-soluzioni)
;   =>
;   (retract ?f)
;   (assert (fase-stampa))
;   (focus  MODULO-STAMPA)
; )
;
; (defrule MAIN::passa-a-modulo-ritrattazione
;   (declare (salience ?*highest-priority*))
;   (fase-ritrattazione)
;   =>
;   (focus MODULO-RITRATTAZIONE)
; )

(defrule MAIN::riattiva-esecuzione
  (declare (salience ?*highest-priority*))
  (not(in-esecuzione))
  =>
  (assert (in-esecuzione))
  )

(defrule MAIN::gestione-moduli
  (declare (salience ?*lowest-priority*))
  ;(nodo (nome chiedi) (valore ?dom))
  ;(not (domanda (attributo ?dom) (gia-chiesta FALSE)))
  ;(not (fase-cerca-soluzioni))
  ?f <- (in-esecuzione)
  =>
  (printout t "IN GESTIONE" crlf)
  ;(assert (fase-cerca-soluzioni))
  ;(retract ?f)
  (focus MODULO-SOLUZIONE MODULO-STAMPA MODULO-RITRATTAZIONE)
  ;(focus  MODULO-STAMPA)
  ;(focus MODULO-RITRATTAZIONE)
)

; CHIEDI DOMANDA
;****************************************************************************


; (defrule chiedi-domanda
;   (declare (salience ?*low-priority*))
;
;   ?ask <- (nodo (nome chiedi)(valore ?attr)(nodo-padre $?p)(id-nodo ?id-ask))
;   ?f <- (domanda (attributo ?attr) (testo-domanda ?domanda) (spiegazione ?spieg) (help ?aiuto)
;           (risposte-valide $?risposte) (descrizione-risposte $?descrizioni) (spiegazione-risposte $?sp-risposte) (gia-chiesta FALSE))
;   (not (nodo (nome ?attr))) ; ATTENZIONE: controllare se eliminabile
;   ?cont-dom <- (contatore-domande ?i)
;   =>
;   (bind ?j (+ ?i 1))
;   (bind ?risposta (ask-question ?j ?f))
;   (modify ?f (gia-chiesta TRUE)(num-domanda ?j)(risposta-selezionata ?risposta))
;   (retract ?cont-dom)
;   (assert (contatore-domande ?j))
;   (assert (nodo (nome ?attr) (valore (nth$ ?risposta ?risposte)) (descrizione (nth$ ?risposta ?sp-risposte)) (sorgente-info utente) (nodo-padre ?id-ask)))
; )

(deffunction get-descrizione-risposta(?attributo ?risposta)
    (bind ?dom (find-fact ((?d domanda))(eq ?d:attributo ?attributo)))
    (bind ?domanda (nth$ 1 ?dom))

    (bind ?risposte-valide (fact-slot-value ?domanda risposte-valide))
    (bind ?spiegazioni-valide (fact-slot-value ?domanda spiegazione-risposte))

    (loop-for-count (?cnt1 1 (length ?risposte-valide)) do
        (if (eq ?risposta (nth$ ?cnt1 ?risposte-valide)) then (return (nth$ ?cnt1 ?spiegazioni-valide)))
    )
)

(deffunction chiedi-domanda-fnz(?attributo)
    (bind ?dom (find-fact ((?d domanda))(eq ?d:attributo ?attributo)))
    (bind ?domanda (nth$ 1 ?dom))

    ;(bind ?num-domanda (fact-slot-value ?domanda num-domanda))
    (bind ?testo-domanda (fact-slot-value ?domanda testo-domanda))
    (bind ?risposte-valide (fact-slot-value ?domanda risposte-valide))
    (bind ?descrizioni (fact-slot-value ?domanda descrizione-risposte))
    (bind ?spiegazioni-valide (fact-slot-value ?domanda spiegazione-risposte))
    (bind ?spiegazione (fact-slot-value ?domanda spiegazione))
    (bind ?aiuto (fact-slot-value ?domanda help))

    (bind ?num-domanda (next-num-domanda))

    ;(clear-window)
    (printout t crlf crlf)
    (printout t   "***                                                 ***" crlf
                  "**  SISTEMA DIAGNOSTICO PER DISPOSITIVI ELETTRONICI  **" crlf
                  "*                         ***                         *" crlf
                  "*     Rispondere alle domande inserendo il numero     *" crlf
                  "**       corrispondente alla risposta corretta.      **" crlf
                  "***                                                 ***" crlf crlf)

    (printout t "***** DOMANDA N." ?num-domanda " *****" crlf)
    (format t "%s%n%n" ?testo-domanda)
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

    (modify ?domanda (num-domanda ?*num-domande-chieste*) (gia-chiesta TRUE) (risposta-selezionata ?answer))

    (return (nth$ ?answer ?risposte-valide))


)

;;******************************************************************************
;;*    REGOLE PER CHIDERE DOMANDE ALL'UTENTE                                   *
;;******************************************************************************

;; *****************************************************************************
;; ************ REGOLE PER FASE 1: PROFILAZIONE UTENTE E DISPOSITIVO ***********
;; *****************************************************************************

;; ***** FASE 1: DOMANDE DA PORRE ALL'UTENTE

(defrule chiedi-tipo-dispositivo
  (declare (salience ?*high-priority*))
  ;WRITTEN DOWN
  ;(not (nodo (nome chiedi) (valore tipo-dispositivo)))
  (domanda (attributo ?attr&tipo-dispositivo)(gia-chiesta FALSE))
  =>
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
  ;(assert (nodo (nome chiedi) (valore tipo-dispositivo)))
)

(defrule chiedi-utente-proprietario
  (declare (salience ?*high-priority*))
  ;WRITTEN DOWN
  ;(not (nodo (nome chiedi) (valore utente-proprietario)))
  (domanda (attributo ?attr&utente-proprietario)(gia-chiesta FALSE))
  =>
  ;(assert (nodo (nome chiedi) (valore utente-proprietario)))
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-esperienza-utente
  (declare (salience ?*high-priority*))
  ;WRITTEN DOWN
  ;(not (nodo (nome chiedi) (valore esperienza-utente)))
  (domanda (attributo ?attr&esperienza-utente)(gia-chiesta FALSE))
  =>
  ;(assert (nodo (nome chiedi) (valore esperienza-utente)))
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-anni-dispositivo
  (declare (salience ?*high-priority*))
  ;WRITTEN DOWN
  ;(not (nodo (nome chiedi) (valore anni-dispositivo)))
  (domanda (attributo ?attr&anni-dispositivo)(gia-chiesta FALSE))
  =>
  ;(assert (nodo (nome chiedi) (valore anni-dispositivo)))
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-garanzia
  (declare (salience ?*high-priority*))
  ;WRITTEN DOWN
  ;(not (nodo (nome chiedi) (valore garanzia)))
  (domanda (attributo ?attr&garanzia)(gia-chiesta FALSE))
  ;?p1 <- (nodo (nome anni-dispositivo) (valore ?v&0-3-anni|sconosciuto) (id-nodo ?id-p1))
  =>
  ;(assert (nodo (nome chiedi) (valore garanzia) (nodo-padre ?id-p1)))
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)



;; ***** FASE 2: DEDUZIONI DEL SISTEMA

(defrule dispositivo-portatile
  ;WRITTEN DOWN
  ?p1 <- (nodo (nome tipo-dispositivo) (valore pc-portatile) (certezza ?c1) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome cavi-display-accessibili) (valore no) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1) (descrizione "I cavi che collegano il dispositivo al display non sono accessibili all'utente.")))
  (assert (nodo (nome alimentatore-caricabatterie) (valore si) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1) (descrizione "Il dispositivo possiede un alimentatore caricabatterie esterno.")))
  (assert (nodo (nome interruttore-alimentatore) (valore acceso) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1) (descrizione "Essendo un dispositivo portatile, l'alimentatore non possiede un tasto di accensione esterno.")))
)

(defrule dispositivo-fisso
  ;WRITTEN DOWN
  ?p1 <- (nodo (nome tipo-dispositivo) (valore pc-desktop) (certezza ?c1) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome ha-batteria) (valore no) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1) (descrizione "Il dispositivo, essendo un pc fisso, non possiede una batteria di alimentazione.")))
  (assert (nodo (nome cavi-display-accessibili) (valore si) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1) (descrizione "I cavi che collegano il dispositivo al display sono accessibili all'utente.")))
  (assert (nodo (nome alimentatore-caricabatterie) (valore no) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1) (descrizione "Il dispositivo non possiede un alimentatore caricabatterie esterno.")))
)

(defrule eta-dispositivo-sconosciuta-1
  ;WRITTEN DOWN
  ?p1 <- (nodo (nome anni-dispositivo) (valore sconosciuto) (sorgente-info utente) (certezza ?CF1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome garanzia) (valore sconosciuto) (sorgente-info utente) (certezza ?CF2) (id-nodo ?id-p2))
  =>
  (assert (nodo (nome anni-dispositivo) (valore 0-3-anni) (certezza (* 0.5 ?CF1)) (descrizione "Il dispositivo potrebbe avere fino a tre anni di eta'") (nodo-padre ?id-p1)))
  (assert (nodo (nome anni-dispositivo) (valore 4-6-anni) (certezza (* 0.5 ?CF1)) (descrizione "Il dispositivo potrebbe avere tra i quattro e i sei anni di eta'") (nodo-padre ?id-p1)))
  (assert (nodo (nome anni-dispositivo) (valore 7-anni)   (certezza (* 0.3 ?CF1)) (descrizione "Il dispositivo potrebbe avere piu' di sei anni di eta'") (nodo-padre ?id-p1)))
)

(defrule eta-dispositivo-sconosciuta-2
  ;WRITTEN DOWN
  ?p1 <- (nodo (nome anni-dispositivo) (valore sconosciuto) (sorgente-info utente) (certezza ?CF1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome garanzia) (valore si) (sorgente-info utente) (certezza ?CF2) (id-nodo ?id-p2))
  =>
  (assert (nodo (nome anni-dispositivo) (valore 0-3-anni) (certezza (* 0.9 ?CF1)) (descrizione "Il dispositivo potrebbe avere fino a tre anni di eta'") (nodo-padre ?id-p1)))
)

(defrule eta-dispositivo-sconosciuta-3
  ;WRITTEN DOWN
  ?p1 <- (nodo (nome anni-dispositivo) (valore sconosciuto) (sorgente-info utente) (certezza ?CF1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome garanzia) (valore no) (sorgente-info utente) (certezza ?CF2) (id-nodo ?id-p2))
  =>
  (assert (nodo (nome anni-dispositivo) (valore 0-3-anni) (certezza (* 0.2 ?CF1)) (descrizione "Il dispositivo potrebbe avere fino a tre anni di eta'") (nodo-padre ?id-p1)))
  (assert (nodo (nome anni-dispositivo) (valore 4-6-anni) (certezza (* 0.8 ?CF1)) (descrizione "Il dispositivo potrebbe avere tra i quattro e i sei anni di eta'") (nodo-padre ?id-p1)))
  (assert (nodo (nome anni-dispositivo) (valore 7-anni)   (certezza (* 0.5 ?CF1)) (descrizione "Il dispositivo potrebbe avere piu' di sei anni di eta'") (nodo-padre ?id-p1)))
)

(defrule garanzia-3-anni
  ;WRITTEN DOWN
  ?p1 <- (nodo (nome anni-dispositivo) (valore 0-3-anni) (sorgente-info utente) (certezza ?CF1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome garanzia) (valore sconosciuto) (sorgente-info utente) (certezza ?CF2) (id-nodo ?id-p2))
  =>
  (bind ?CF (calcola-certezza 0.9 ?CF1 ?CF2))
  (assert (nodo (nome garanzia) (valore si) (certezza ?CF) (descrizione "E' possibile che il dispositivo sia coperto da garanzia") (nodo-padre ?id-p1 ?id-p2)))
)

(defrule garanzia-4-anni-piu
  ;WRITTEN DOWN
  ?p1 <- (nodo (nome anni-dispositivo) (valore  ?val&4-6-anni|7-anni) (sorgente-info utente) (certezza ?CF1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome garanzia) (valore sconosciuto) (sorgente-info utente) (certezza ?CF2) (id-nodo ?id-p2))
  =>
  (bind ?CF (calcola-certezza 1.0 ?CF1 ?CF2))
  (assert (nodo (nome garanzia) (valore no) (certezza ?CF) (descrizione "Il dispositivo non e' coperto da garanzia'") (nodo-padre ?id-p1 ?id-p2)))
)


(defrule garanzia-anni-sconosciuti
  ;WRITTEN DOWN
  ?p1 <- (nodo (nome anni-dispositivo) (valore sconosciuto) (sorgente-info utente) (certezza ?CF1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome garanzia) (valore sconosciuto) (sorgente-info utente) (certezza ?CF2) (id-nodo ?id-p2))
  =>
  (bind ?CF-garanzia (calcola-certezza 0.7 ?CF1 ?CF2))
  (assert (nodo (nome garanzia) (valore si) (certezza ?CF-garanzia) (descrizione "E' possibile che il dispositivo sia coperto da garanzia") (nodo-padre ?id-p1 ?id-p2)))

)

;;REGOLE FASE 2 *******************************************

(defrule chiedi-ha-batteria
  ;ASK
  ;WRITTEN DOWN
  ;(not  (nodo (nome chiedi) (valore ha-batteria)))
  (domanda (attributo ?attr&ha-batteria)(gia-chiesta FALSE))
  ?p1 <- (nodo (nome tipo-dispositivo) (valore pc-portatile) (id-nodo ?id-p1))
  =>
  ;(assert (nodo (nome chiedi) (valore ha-batteria) (nodo-padre ?id-p1)))
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc) (nodo-padre ?id-p1)))
)

(defrule controllo-accensione
  ;ASK
  ;WRITTEN DOWN
  ;(not (nodo (nome chiedi) (valore controllo-accensione)))
  (domanda (attributo ?attr&controllo-accensione)(gia-chiesta FALSE))
  ;?px <- (nodo (nome nodo-di-collegamento) (valore avvia-ricerca-diagnosi) (id-nodo ?id-px) (attivo TRUE))
  =>
  ;(assert (nodo (nome chiedi) (valore controllo-accensione)))
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-rimuovere-batteria
  ;ASK
  ;WRITTEN DOWN
  ;(not (nodo (nome chiedi) (valore batteria-difettosa)))
  (domanda (attributo ?attr&batteria-difettosa)(gia-chiesta FALSE))
  ?p1 <- (nodo (nome ha-batteria) (valore si) (certezza ?CF1) (id-nodo ?id-p1))
  ?p3 <- (nodo (nome controllo-accensione) (valore ?v&possibile-non-funzionante|riavvio) (certezza ?CF3) (id-nodo ?id-p3))
  =>
  ;(assert (nodo (nome chiedi) (valore batteria-difettosa) (nodo-padre ?id-p1 ?id-p3)))
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-alim-collegata
  ;ASK
  ;WRITTEN DOWN
  ;(not (nodo (nome chiedi) (valore alimentazione-collegata)))
  (domanda (attributo ?attr&alimentazione-collegata)(gia-chiesta FALSE))
  ?p1 <- (nodo (nome controllo-accensione) (valore possibile-non-funzionante) (certezza ?CF1) (id-nodo ?id-p1))
  (or
    ?p2 <- (nodo (nome ha-batteria) (valore no) (certezza ?CF2) (id-nodo ?id-p2))
    ?p2 <- (nodo (nome batteria-difettosa) (valore no) (certezza ?CF2) (id-nodo ?id-p2))
  )
  =>
  ;(assert (nodo (nome chiedi) (valore alimentazione-collegata) (nodo-padre ?id-p1 ?id-p2)))
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule controllo-accensione-ut-inesperto
  ;ASK
  ;WRITTEN DOWN
  ;(not (nodo (nome chiedi) (valore controllo-accensione-ut-inesperto)))
  (domanda (attributo ?attr&controllo-accensione-ut-inesperto)(gia-chiesta FALSE))
  ?p1 <- (nodo (nome esperienza-utente) (valore utente-inesperto) (certezza ?CF1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome controllo-accensione) (valore possibile-non-funzionante) (certezza ?CF2) (id-nodo ?id-p2))
  ?p3 <- (nodo (nome alimentazione-collegata) (valore si) (certezza ?CF3) (id-nodo ?id-p3))
  =>
  ;(assert (nodo (nome chiedi) (valore controllo-accensione-ut-inesperto) (nodo-padre ?id-p1 ?id-p2 ?id-p3)))
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-spia-alimentatore-pcportatile
  ;ASK
  ;WRITTEN DOWN
  ;(not (nodo (nome chiedi) (valore spia-alimentatore-pcportatile)))
  (domanda (attributo ?attr&spia-alimentatore-pcportatile)(gia-chiesta FALSE))
  ?p1 <- (nodo (nome tipo-dispositivo) (valore pc-portatile) (certezza ?CF1) (id-nodo ?id-p1))
  ?p3 <- (nodo (nome stato-accensione) (valore non-funzionante) (certezza ?CF3) (id-nodo ?id-p3))
  =>
  ;(assert (nodo (nome chiedi) (valore spia-alimentatore-pcportatile) (nodo-padre ?id-p1 ?id-p3)))
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-spia-alimentatore-pcdesktop
  ;ASK
  ;WRITTEN DOWN
  ;(not (nodo (nome chiedi) (valore spia-alimentatore-pcdesktop)))
  (domanda (attributo ?attr&spia-alimentatore-pcdesktop)(gia-chiesta FALSE))
  ?p1 <- (nodo (nome tipo-dispositivo) (valore pc-desktop) (certezza ?CF1) (id-nodo ?id-p1))
  ?p3 <- (nodo (nome stato-accensione) (valore  non-funzionante) (certezza ?CF3) (id-nodo ?id-p3))
  ?p4 <- (nodo (nome interruttore-alimentatore) (valore acceso) (certezza ?CF4) (id-nodo ?id-p4))
  =>
  ;(assert (nodo (nome chiedi) (valore spia-alimentatore-pcdesktop) (nodo-padre ?id-p1 ?id-p3 ?id-p4)))
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-interruttore-alimentatore
  ;WRITTEN DOWN
  ;ASK
  ;(not (nodo (nome chiedi) (valore interruttore-alimentatore)))
    (domanda (attributo ?attr&interruttore-alimentatore)(gia-chiesta FALSE))
  ?p1 <- (nodo (nome tipo-dispositivo) (valore pc-desktop) (certezza ?CF1) (id-nodo ?id-p1))
  ?p3 <- (nodo (nome stato-accensione) (valore  non-funzionante) (certezza ?CF3) (id-nodo ?id-p3))
  =>
  ;(assert (nodo (nome chiedi) (valore interruttore-alimentatore) (nodo-padre ?id-p1 ?id-p3)))
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-problema-video-dispositivo
  ;WRITTEN DOWN
  ;ASK
  (domanda (attributo ?attr&problema-video-dispositivo)(gia-chiesta FALSE))
  ?p1 <- (nodo (nome stato-accensione)(valore funzionante)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  (not (nodo (nome disturbo-video)(valore ?v)(certezza ?CF3&:(> ?CF3 0))(attivo TRUE)(id-nodo ?id-p3)))
  =>
  ;(assert (nodo (nome chiedi) (valore problema-video-dispositivo) (nodo-padre ?id-p1)))
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-disturbo-video
  ;WRITTEN DOWN
  ;ASK
  (domanda (attributo ?attr&disturbo-video)(gia-chiesta FALSE))
  ?p1 <- (nodo (nome problema-video-dispositivo)(valore si)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  (not (nodo (nome disturbo-video) (valore ?v)(certezza ?CF2&:(> ?CF2 0))(attivo TRUE)(id-nodo ?id-p2)))
  =>
  ;(assert (nodo (nome chiedi) (valore disturbo-video) (nodo-padre ?id-p1)))
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-cavo-alimentazione-display
  ;WRITTEN DOWN
  ;ASK
  (domanda (attributo ?attr&cavo-alimentazione-display)(gia-chiesta FALSE))
  ?p1 <- (nodo (nome disturbo-video)(valore schermo-nero)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome cavi-display-accessibili)(valore si)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  ;(assert (nodo (nome chiedi) (valore cavo-alimentazione-display) (nodo-padre ?id-p1 ?id-p2)))
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-monitor-esterno
  ;WRITTEN DOWN
  ;ASK
  (domanda (attributo ?attr&monitor-esterno)(gia-chiesta FALSE))
  ?p1 <- (nodo (nome disturbo-video)(valore ?v1&fasce-verticali|linee-orizzontali|schermo-nero)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome cavo-alimentazione-display)(valore ok)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo (nome problema-video-all-avvio) (valore si)(certezza ?c3)(attivo TRUE)(id-nodo ?id-p3))
  ;(not (nodo (nome chiedi) (valore monitor-esterno)))
  =>
  ;(assert (nodo (nome chiedi) (valore monitor-esterno) (nodo-padre ?id-p1 ?id-p2 ?id-p3)))
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-problema-video-all-avvio
  ;WRITTEN DOWN
  ;ASK
  (domanda (attributo ?attr&problema-video-all-avvio)(gia-chiesta FALSE))
  ?p1 <- (nodo (nome disturbo-video)(valore ?v1&fasce-verticali|linee-orizzontali|schermo-nero)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome cavo-alimentazione-display)(valore ok)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
  ;(not (nodo (nome chiedi) (valore problema-video-all-avvio)))
  =>
  ;(assert (nodo (nome chiedi) (valore problema-video-all-avvio) (nodo-padre ?id-p1 ?id-p2)))
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-problema-POST-ut-esperto
  ;WRITTEN DOWN
  ;ASK
  (domanda (attributo ?attr&fase-POST-ut-esperto)(gia-chiesta FALSE))
  ?p1 <- (nodo (nome stato-accensione)(valore ?v&funzionante|riavvio)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p3 <- (nodo (nome problema-video-dispositivo)(valore no)(certezza ?CF3)(attivo TRUE)(id-nodo ?id-p3))
  ?p4 <- (nodo (nome esperienza-utente) (valore utente-esperto)(certezza ?CF4)(attivo TRUE)(id-nodo ?id-p4))
  =>
  ;(assert (nodo (nome chiedi) (valore fase-POST-ut-esperto) (nodo-padre ?id-p1 ?id-p3 ?id-p4)))
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-problema-POST-ut-inesperto
  ;WRITTEN DOWN
  ;ASK
  (domanda (attributo ?attr&fase-POST-ut-inesperto)(gia-chiesta FALSE))
  ?p1 <- (nodo (nome stato-accensione)(valore ?v&funzionante|riavvio)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p3 <- (nodo (nome problema-video-dispositivo)(valore no)(certezza ?CF3)(attivo TRUE)(id-nodo ?id-p3))
  ?p4 <- (nodo (nome esperienza-utente) (valore utente-inesperto)(certezza ?CF4)(attivo TRUE)(id-nodo ?id-p4))
  =>
  ;(assert (nodo (nome chiedi) (valore fase-POST-ut-inesperto) (nodo-padre ?id-p1 ?id-p3 ?id-p4)))
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-surriscaldamento
  ;WRITTEN DOWN
  ;ASK
  (domanda (attributo ?attr&surriscaldamento)(gia-chiesta FALSE))
  ?p1 <- (nodo(nome fase-POST)(valore ?v&errore-messaggio|errore-beep-code|errore-riavvio)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome utente-proprietario)(valore si)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  ;(assert (nodo (nome chiedi) (valore surriscaldamento)(nodo-padre ?id-p1 ?id-p2)))
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-versione-bios
  ;ASK
  (domanda (attributo ?attr&versione-bios)(gia-chiesta FALSE))
  ?p1 <- (nodo(nome fase-POST)(valore errore-beep-code)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  =>
  ;(assert (nodo (nome chiedi) (valore versione-bios) (nodo-padre ?id-p1)))
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-ami-beep-codes
  ;ASK
  (domanda (attributo ?attr&ami-beep-codes)(gia-chiesta FALSE))
  ?p1 <- (nodo(nome fase-POST)(valore errore-beep-code)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome versione-bios)(valore ami-bios)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  ;(assert (nodo (nome chiedi) (valore ami-beep-codes) (nodo-padre ?id-p1 ?id-p2)))
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-award-beep-codes
  ;ASK
  (domanda (attributo ?attr&award-beep-codes)(gia-chiesta FALSE))
  ?p1 <- (nodo(nome fase-POST)(valore errore-beep-code)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome versione-bios)(valore award-bios)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  ;(assert (nodo (nome chiedi) (valore award-beep-codes) (nodo-padre ?id-p1 ?id-p2)))
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-asus-beep-codes
  ;ASK
  (domanda (attributo ?attr&asus-beep-codes)(gia-chiesta FALSE))
  ?p1 <- (nodo(nome fase-POST)(valore errore-beep-code)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome versione-bios)(valore asus)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  ;(assert (nodo (nome chiedi) (valore asus-beep-codes) (nodo-padre ?id-p1 ?id-p2)))
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-intel-beep-codes
  ;ASK
  (domanda (attributo ?attr&intel-beep-codes)(gia-chiesta FALSE))
  ?p1 <- (nodo(nome fase-POST)(valore errore-beep-code)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome versione-bios)(valore intel)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  ;(assert (nodo (nome chiedi) (valore intel-beep-codes) (nodo-padre ?id-p1 ?id-p2)))
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-asrock-beep-codes
  ;ASK
  (domanda (attributo ?attr&ami-beep-codes)(gia-chiesta FALSE))
  ?p1 <- (nodo(nome fase-POST)(valore errore-beep-code)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome versione-bios)(valore asrock)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  ;(assert (nodo (nome chiedi) (valore ami-beep-codes) (nodo-padre ?id-p1 ?id-p2)))
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-installazione-nuovo-HW-SW
  ;WRITTEN DOWN
  ;ASK
  (domanda (attributo ?attr&conflitto-HW-SW)(gia-chiesta FALSE))
  ?p1 <- (nodo(nome fase-POST)(valore errore-BSOD)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  =>
  ;(assert (nodo (nome chiedi) (valore conflitto-HW-SW) (nodo-padre ?id-p1)))
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-inattivita-dispositivo
  ;WRITTEN DOWN
  ;ASK
  (domanda (attributo ?attr&inattivita-dispositivo)(gia-chiesta FALSE))
  ?p1 <- (nodo(nome fase-POST)(valore ?v&errore-messaggio|errore-riavvio)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome utente-proprietario)(valore si)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  ;(assert (nodo (nome chiedi) (valore inattivita-dispositivo) (nodo-padre ?id-p1 ?id-p2)))
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)















(defrule riavvio-non-controllato
  ?p1 <- (nodo (nome controllo-accensione) (valore riavvio) (certezza ?CF1) (id-nodo ?id-p1))
  (or
    ?p2 <- (nodo (nome ha-batteria) (valore no) (certezza ?CF2) (id-nodo ?id-p2))
    ?p2 <- (nodo (nome batteria-difettosa) (valore no) (certezza ?CF2) (id-nodo ?id-p2))
  )
  =>
  (assert (nodo (nome stato-accensione) (valore riavvio) (certezza (calcola-certezza 1 ?CF1 ?CF2)) (nodo-padre ?id-p1 ?id-p2 ) (descrizione "Il dispositivo si riavvia da solo in continuazione")))
  (assert (nodo (nome problema-video-dispositivo) (valore no) (certezza (calcola-certezza 1 ?CF1 ?CF2)) (nodo-padre ?id-p1 ?id-p2 ) (descrizione "Il dispositivo non soffre di problemi al display")))
)


(defrule DIAGNOSI-alimentazione-non-collegata
  ;WRITTEN DOWN
  ?p1 <- (nodo (nome alimentazione-collegata) (valore no) (certezza ?CF1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome controllo-accensione) (valore possibile-non-funzionante) (certezza ?CF2) (id-nodo ?id-p2))
  =>
  (bind ?CF (calcola-certezza 1.0 ?CF1 ?CF2))
  (assert (nodo (nome diagnosi) (valore alimentazione-disconnessa)(nodo-padre ?id-p1 ?id-p2) (certezza ?CF)))
)





(defrule stato-accensione-ut-esperto
  ;WRITTEN DOWN
  ?p1 <- (nodo (nome esperienza-utente) (valore utente-esperto) (certezza ?CF1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome controllo-accensione) (valore possibile-non-funzionante) (certezza ?CF2) (id-nodo ?id-p2))
  ?p3 <- (nodo (nome alimentazione-collegata) (valore si) (certezza ?CF3) (id-nodo ?id-p3))
  =>
  (bind ?CF-accensione (calcola-certezza 1 ?CF1 ?CF2 ?CF3)) ;;livello CRT utente-esperto settato a 1
  (assert (nodo (nome stato-accensione) (valore non-funzionante) (certezza ?CF-accensione) (nodo-padre ?id-p1 ?id-p2 ?id-p3) (descrizione "Il dispositivo non si accende, c'e' un problema con il circuito di alimentazione.")))
)

(defrule stato-accensione-funzionante
  ;WRITTEN DOWN
  ?p1 <- (nodo (nome controllo-accensione) (valore funzionante) (certezza ?CF1) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome stato-accensione) (valore funzionante) (certezza (* 1.0 ?CF1)) (nodo-padre ?id-p1 ) (descrizione "Il dispositivo si accende, il circuito di alimentazione sembra funzionare.")))
)

(defrule stato-accensione-ut-inesperto
  ;WRITTEN DOWN
  ?p1 <- (nodo (nome controllo-accensione) (valore possibile-non-funzionante) (certezza ?CF1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome controllo-accensione-ut-inesperto) (valore non-funzionante) (certezza ?CF2) (id-nodo ?id-p2))
  ?p3 <- (nodo (nome alimentazione-collegata) (valore si) (certezza ?CF3) (id-nodo ?id-p3))
  =>
  (bind ?CF-accensione (calcola-certezza 1 ?CF1 ?CF2 ?CF3))
  (assert (nodo (nome stato-accensione) (valore non-funzionante) (certezza ?CF-accensione) (nodo-padre ?id-p1 ?id-p2 ?id-p3) (descrizione "Il dispositivo non si accende, c'e' un problema con il circuito di alimentazione.")))
  )

(defrule stato-accensione-funzionante-con-schermo-nero
  ;WRITTEN DOWN
  ?p1 <- (nodo (nome controllo-accensione) (valore possibile-non-funzionante) (certezza ?CF1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome controllo-accensione-ut-inesperto) (valore funzionante) (certezza ?CF2) (id-nodo ?id-p2))
  ?p3 <- (nodo (nome alimentazione-collegata) (valore si) (certezza ?CF3) (id-nodo ?id-p3))
  =>
  (bind ?CF-schermo-nero (calcola-certezza 1 ?CF1 ?CF2 ?CF3))
  (assert (nodo (nome disturbo-video) (valore schermo-nero) (certezza ?CF-schermo-nero) (nodo-padre ?id-p1 ?id-p2 ?id-p3) (descrizione "Il dispositivo si accende ma lo schermo e' nero e non sembra dare segni di vita.")))
  (bind ?CF-accensione (calcola-certezza 1 ?CF1 ?CF2 ?CF3))
  (assert (nodo (nome stato-accensione) (valore funzionante) (certezza ?CF-accensione) (nodo-padre ?id-p1 ?id-p2 ?id-p3) (descrizione "Il dispositivo si accende, il circuito di alimentazione sembra funzionare.")))
  (bind ?CF-prob-video (calcola-certezza 1 ?CF1 ?CF2 ?CF3))
  (assert (nodo (nome problema-video-dispositivo)(valore si)(certezza ?CF-prob-video)(nodo-padre ?id-p1 ?id-p2 ?id-p3) (descrizione "Il dispositivo sembra avere un problema al display.")))
  )


(defrule alimentazione-collegata
  ;WRITTEN DOWN
  ?p1 <- (nodo (nome alimentazione-collegata) (valore si) (certezza ?CF1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome stato-accensione) (valore non-funzionante) (certezza ?CF2) (id-nodo ?id-p2))
  =>
  (bind ?CF-guasto-alim (calcola-certezza 0.6 ?CF1 ?CF2))
  (bind ?CF-guasto-scheda-madre (calcola-certezza 0.4 ?CF1 ?CF2))
  (assert (nodo (nome diagnosi) (valore alimentatore-guasto) (certezza ?CF-guasto-alim) (nodo-padre ?id-p1 ?id-p2)))
  (assert (nodo (nome diagnosi) (valore scheda-madre-guasta) (certezza ?CF-guasto-scheda-madre) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule spia-alimentatore-accesa
  ; unifica la risposta da pc-desktop o laptop sulla spia di accensione
  ?p1 <- (nodo (nome ?att&spia-alimentatore-pcportatile|spia-alimentatore-pcdesktop) (valore accesa) (certezza ?c1) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome spia-alimentatore) (valore accesa) (nodo-padre ?id-p1) (certezza ?c1) (descrizione "La spia dell'alimentatore e' accesa.")))
)

(defrule spia-alimentatore-spenta
  ; unifica la risposta da pc-desktop o laptop sulla spia di accensione
  ?p1 <- (nodo (nome ?att&spia-alimentatore-pcportatile|spia-alimentatore-pcdesktop) (valore spenta) (certezza ?c1) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome spia-alimentatore) (valore spenta) (nodo-padre ?id-p1) (certezza ?c1) (descrizione "La spia dell'alimentatore e' spenta.")))
)

(defrule spia-alimentatore-sconosciuta
  ;WRITTEN DOWN
  ?p1 <- (nodo (nome ?att&spia-alimentatore-pcportatile|spia-alimentatore-pcdesktop) (valore sconosciuto) (certezza ?CF1) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome spia-alimentatore) (valore accesa) (certezza (* 0.5 ?CF1)) (nodo-padre ?id-p1) (descrizione "La spia dell'alimentatore potrebbe essere accesa.")))
  (assert (nodo (nome spia-alimentatore) (valore spenta) (certezza (* 0.5 ?CF1)) (nodo-padre ?id-p1) (descrizione "La spia dell'alimentatore potrebbe essere spenta.")))
)

;; Per dispositivi laptop ...


(defrule DIAGNOSI-batteria-difettosa
  ;WRITTEN DOWN
  ?p1 <- (nodo (nome batteria-difettosa) (valore si) (certezza ?CF1) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome diagnosi) (valore batteria-difettosa) (certezza (* 1.0 ?CF1)) (nodo-padre ?id-p1)))
)



(defrule DIAGNOSI-spia-alimentatore-accesa
  ;WRITTEN DOWN
  ?p1 <- (nodo (nome interruttore-alimentatore) (valore acceso) (certezza ?CF1) (id-nodo ?id-p1))
  ?p3 <- (nodo (nome stato-accensione) (valore  non-funzionante) (certezza ?CF3) (id-nodo ?id-p3))
  ?p4 <- (nodo (nome spia-alimentatore) (valore accesa) (certezza ?CF4) (id-nodo ?id-p4))
  =>
  (bind ?CF-alim-guasto (calcola-certezza -0.3 ?CF1 ?CF3 ?CF4 ))
  (bind ?CF-scheda-madre-guasta (calcola-certezza 0.8 ?CF1 ?CF3 ?CF4 ))
  (assert (nodo (nome diagnosi) (valore alimentatore-guasto)(nodo-padre ?id-p1 ?id-p3 ?id-p4) (certezza ?CF-alim-guasto)))
  (assert (nodo (nome diagnosi) (valore scheda-madre-guasta)(nodo-padre ?id-p1 ?id-p3 ?id-p4) (certezza ?CF-scheda-madre-guasta)))
)

(defrule DIAGNOSI-spia-alimentatore-spenta
  ;WRITTEN DOWN
  ?p1 <- (nodo (nome interruttore-alimentatore) (valore acceso) (certezza ?CF1) (id-nodo ?id-p1))
  ?p3 <- (nodo (nome stato-accensione) (valore  non-funzionante) (certezza ?CF3) (id-nodo ?id-p3))
  ?p4 <- (nodo (nome spia-alimentatore) (valore spenta) (certezza ?CF4) (id-nodo ?id-p4))
  =>
  (bind ?CF-alim-guasto (calcola-certezza 0.8 ?CF1 ?CF3 ?CF4))
  (bind ?CF-scheda-madre-guasta (calcola-certezza 0.4 ?CF1 ?CF3 ?CF4))
  (assert (nodo (nome diagnosi) (valore alimentatore-guasto)(nodo-padre ?id-p1 ?id-p3 ?id-p4) (certezza ?CF-alim-guasto)))
  (assert (nodo (nome diagnosi) (valore scheda-madre-guasta)(nodo-padre ?id-p1 ?id-p3 ?id-p4) (certezza ?CF-scheda-madre-guasta)))
)

;; Per dispositivi desktop ...





(defrule DIAGNOSI-interruttore-alimentatore-spento
  ;WRITTEN DOWN
  ?p1 <- (nodo (nome tipo-dispositivo) (valore pc-desktop) (certezza ?CF1) (id-nodo ?id-p1))
  ?p3 <- (nodo (nome stato-accensione) (valore  non-funzionante) (certezza ?CF3) (id-nodo ?id-p3))
  ?p4 <- (nodo (nome interruttore-alimentatore) (valore spento) (certezza ?CF4) (id-nodo ?id-p4))
  =>
  (bind ?CF-alim-spento (calcola-certezza 1.0 ?CF1 ?CF3 ?CF4))
  (assert (nodo (nome diagnosi) (valore alimentatore-spento)(nodo-padre ?id-p1 ?id-p3 ?id-p4) (certezza ?CF-alim-spento)))
  (bind ?CF-diagnosi-escluse (calcola-certezza -1.0 ?CF1 ?CF3 ?CF4))
  (assert (nodo (nome diagnosi) (valore alimentatore-guasto) (certezza ?CF-diagnosi-escluse ) (nodo-padre ?id-p1 ?id-p3 ?id-p4)))
  (assert (nodo (nome diagnosi) (valore scheda-madre-guasta) (certezza ?CF-diagnosi-escluse ) (nodo-padre ?id-p1 ?id-p3 ?id-p4)))
)


;; DOMANDE VIDEO ***************************************************************



(defrule cavo-alimentazione-display-collegato
  ;WRITTEN DOWN
  (or
    ?p1 <- (nodo (nome disturbo-video)(valore ?v&fasce-verticali|linee-orizzontali|macchie)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
    ?p1 <- (nodo (nome cavi-display-accessibili)(valore no)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
    )
  (not (nodo (nome cavo-alimentazione-display)(attivo TRUE)))
  =>
  (bind ?CF-cavi-collegati (calcola-certezza 1.0 ?CF1))
  (assert (nodo(nome cavo-alimentazione-display)(valore ok)(certezza ?CF-cavi-collegati)(nodo-padre ?id-p1) (descrizione "Il monitor si accende, quindi l'alimentazione sembra essere collegata.")))
)



(defrule diagnosi-problema-SW-video-1
  ;WRITTEN DOWN
  ?p1 <- (nodo (nome disturbo-video)(valore fasce-verticali)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome problema-video-all-avvio) (valore no)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (bind ?CF-SW (calcola-certezza 0.8 ?c1 ?c2))
  (assert (nodo (nome diagnosi)(valore problema-SW-video)(certezza ?CF-SW)(nodo-padre ?id-p1 ?id-p2)))
)
(defrule diagnosi-problema-SW-video-2
  ;WRITTEN DOWN
  ?p1 <- (nodo (nome disturbo-video)(valore linee-orizzontali)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome problema-video-all-avvio) (valore no)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (bind ?CF-SW (calcola-certezza 0.8 ?c1 ?c2))
  (assert (nodo (nome diagnosi)(valore problema-SW-video)(certezza ?CF-SW)(nodo-padre ?id-p1 ?id-p2)))
)
(defrule diagnosi-problema-SW-video-3
  ;WRITTEN DOWN
  ?p1 <- (nodo (nome disturbo-video)(valore schermo-nero)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome problema-video-all-avvio) (valore no)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (bind ?CF-SW (calcola-certezza 0.9 ?c1 ?c2))
  (assert (nodo (nome diagnosi)(valore problema-SW-video)(certezza ?CF-SW)(nodo-padre ?id-p1 ?id-p2)))
)

(defrule diagnosi-display-guasto
  ;WRITTEN DOWN
  ?p1 <- (nodo (nome disturbo-video)(valore schermo-nero|fasce-verticali|linee-orizzontali)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
  ?p3 <- (nodo (nome problema-video-all-avvio) (valore si)(certezza ?c3)(attivo TRUE)(id-nodo ?id-p3))
  =>
  (bind ?CF (calcola-certezza 0.7 ?c1 ?c3))
  (assert (nodo (nome diagnosi) (valore guasto-display)(certezza ?CF)(nodo-padre ?id-p1 ?id-p3)))
)

(defrule diagnosi-inverter-guasto
  ;WRITTEN DOWN
  ?p1 <- (nodo (nome disturbo-video)(valore ?v&linee-orizzontali|fasce-verticali|schermo-nero)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
  ?p3 <- (nodo (nome problema-video-all-avvio) (valore si)(certezza ?c3)(attivo TRUE)(id-nodo ?id-p3))
  ?p4 <- (nodo (nome tipo-dispositivo) (valore pc-portatile)(certezza ?c4)(attivo TRUE)(id-nodo ?id-p4))
  =>
  (bind ?CF (calcola-certezza 0.8 ?c1 ?c3 ?c4))
  (assert (nodo (nome diagnosi) (valore guasto-inverter)(certezza ?CF)(nodo-padre ?id-p1 ?id-p3 ?id-p4)))
)

(defrule diagnosi-scheda-video-guasta
  ;WRITTEN DOWN
  ?p1 <- (nodo (nome disturbo-video)(valore ?v&schermo-nero|fasce-verticali)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
  ?p3 <- (nodo (nome problema-video-all-avvio)(valore si)(certezza ?c3)(attivo TRUE)(id-nodo ?id-p3))
  =>
  (bind ?CF (calcola-certezza 0.7 ?c1 ?c3))
  (assert (nodo (nome diagnosi) (valore guasto-vga)(certezza ?CF)(nodo-padre ?id-p1 ?id-p3)))
)


(defrule diagnosi-interferenze-cavo-1
  ;WRITTEN DOWN
  ?p1 <- (nodo (nome disturbo-video)(valore ?v&linee-orizzontali|fasce-verticali)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome problema-video-all-avvio)(valore si)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo (nome cavi-display-accessibili)(valore si)(certezza ?c3)(attivo TRUE)(id-nodo ?id-p3))
  =>
  (bind ?CF (calcola-certezza 0.9 ?c1 ?c2 ?c3))
  (assert (nodo (nome diagnosi) (valore interferenze-cavo)(certezza ?CF)(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule diagnosi-interferenze-cavo-2
  ;WRITTEN DOWN
  ?p1 <- (nodo (nome disturbo-video)(valore ?v&linee-orizzontali|fasce-verticali)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome problema-video-all-avvio)(valore si)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo (nome cavi-display-accessibili)(valore no)(certezza ?c3)(attivo TRUE)(id-nodo ?id-p3))
  =>
  (bind ?CF (calcola-certezza 0.7 ?c1 ?c2 ?c3))
  (assert (nodo (nome diagnosi) (valore interferenze-cavo)(certezza ?CF)(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule monitor-esterno-funzionante-pc-desktop
  ;WRITTEN DOWN
  ?p1 <- (nodo (nome disturbo-video)(valore ?v&linee-orizzontali|fasce-verticali|schermo-nero)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome monitor-esterno) (valore funzionante)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo (nome tipo-dispositivo) (valore pc-desktop)(certezza ?c3)(attivo TRUE)(id-nodo ?id-p3))
  =>
  (bind ?CF-display-guasto (calcola-certezza 0.9 ?c1 ?c2 ?c3))
  (bind ?CF-vga-guasta (calcola-certezza -1.0 ?c1 ?c2 ?c3))
  (bind ?CF-problema-sw (calcola-certezza -0.5 ?c1 ?c2 ?c3))
  (assert (nodo (nome diagnosi) (valore guasto-display)(certezza ?CF-display-guasto)(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
  (assert (nodo (nome diagnosi) (valore guasto-vga)(certezza ?CF-vga-guasta)(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
  (assert (nodo (nome diagnosi)(valore problema-SW-video)(certezza ?CF-problema-sw)(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule monitor-esterno-non-funzionante-pc-desktop
  ;WRITTEN DOWN
  ?p1 <- (nodo (nome disturbo-video)(valore ?v&linee-orizzontali|fasce-verticali|schermo-nero)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome monitor-esterno) (valore errore)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo (nome tipo-dispositivo) (valore pc-desktop)(certezza ?c3)(attivo TRUE)(id-nodo ?id-p3))
  =>
  (bind ?CF-display-guasto (calcola-certezza -1.0 ?c1 ?c2 ?c3))
  (bind ?CF-vga-guasta (calcola-certezza 0.8 ?c1 ?c2 ?c3))
  (bind ?CF-problema-sw (calcola-certezza 0.5 ?c1 ?c2 ?c3))
  (assert (nodo (nome diagnosi) (valore guasto-display)(certezza ?CF-display-guasto)(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
  (assert (nodo (nome diagnosi) (valore guasto-vga)(certezza ?CF-vga-guasta)(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
  (assert (nodo (nome diagnosi)(valore problema-SW-video)(certezza ?CF-problema-sw)(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule monitor-esterno-funzionante-pc-portatile
  ;WRITTEN DOWN
  ?p1 <- (nodo (nome disturbo-video)(valore ?v&linee-orizzontali|fasce-verticali|schermo-nero)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome monitor-esterno) (valore funzionante)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo (nome tipo-dispositivo) (valore pc-portatile)(certezza ?c3)(attivo TRUE)(id-nodo ?id-p3))
  =>
  (bind ?CF-display-guasto (calcola-certezza 0.8 ?c1 ?c2 ?c3))
  (bind ?CF-inverter-guasto (calcola-certezza 0.8 ?c1 ?c2 ?c3))
  (bind ?CF-vga-guasta (calcola-certezza -1.0 ?c1 ?c2 ?c3))
  (bind ?CF-problema-sw (calcola-certezza -0.5 ?c1 ?c2 ?c3))
  (assert (nodo (nome diagnosi) (valore guasto-display)(certezza ?CF-display-guasto)(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
  (assert (nodo (nome diagnosi) (valore guasto-inverter)(certezza ?CF-inverter-guasto)(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
  (assert (nodo (nome diagnosi) (valore guasto-vga)(certezza ?CF-vga-guasta)(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
  (assert (nodo (nome diagnosi)(valore problema-SW-video)(certezza ?CF-problema-sw)(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule monitor-esterno-non-funzionante-pc-portatile
  ;WRITTEN DOWN
  ?p1 <- (nodo (nome disturbo-video)(valore ?v&linee-orizzontali|fasce-verticali|schermo-nero)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome monitor-esterno) (valore errore)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo (nome tipo-dispositivo) (valore pc-portatile)(certezza ?c3)(attivo TRUE)(id-nodo ?id-p3))
  =>
  (bind ?CF-display-guasto (calcola-certezza -1.0 ?c1 ?c2 ?c3))
  (bind ?CF-inverter-guasto (calcola-certezza -1.0 ?c1 ?c2 ?c3))
  (bind ?CF-vga-guasta (calcola-certezza 0.8 ?c1 ?c2 ?c3))
  (bind ?CF-problema-sw (calcola-certezza 0.5 ?c1 ?c2 ?c3))
  (assert (nodo (nome diagnosi) (valore guasto-display)(certezza ?CF-display-guasto)(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
  (assert (nodo (nome diagnosi) (valore guasto-inverter)(certezza ?CF-inverter-guasto)(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
  (assert (nodo (nome diagnosi) (valore guasto-vga)(certezza ?CF-vga-guasta)(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
  (assert (nodo (nome diagnosi)(valore problema-SW-video)(certezza ?CF-problema-sw)(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)


(defrule diagnosi-disturbo-macchie
  ;WRITTEN DOWN
  ?p1 <- (nodo (nome disturbo-video)(valore macchie)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  =>
  (assert (nodo (nome diagnosi) (valore guasto-display)(certezza (* 0.95 ?CF1))(nodo-padre ?id-p1)))
)

(defrule diagnosi-cavi-video-scollegati
  ;WRITTEN DOWN
  ?p1 <- (nodo(nome disturbo-video)(valore schermo-nero)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome cavo-alimentazione-display)(valore errore)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (bind ?CF (calcola-certezza 1.0 ?CF1 ?CF2))
  (assert (nodo (nome diagnosi) (valore cavo-alimentazione-display-scollegato)(certezza ?CF)(nodo-padre ?id-p1 ?id-p2)))
)

(defrule diagnosi-CMOS-corrotta
  ;WRITTEN DOWN
  ?p1 <- (nodo(nome disturbo-video)(valore schermo-nero)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome cavo-alimentazione-display)(valore ok)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (bind ?CF (calcola-certezza 0.5 ?CF1 ?CF2))
  (assert (nodo (nome diagnosi)(valore CMOS-corrotta)(certezza ?CF)(nodo-padre ?id-p1 ?id-p2)))
)

;; REGOLE POST BIOS********************




(defrule combina-chiedi-problema-POST
  ?p1 <- (nodo(nome ?n&fase-POST-ut-esperto|fase-POST-ut-inesperto)(valore ?v) (descrizione ?d)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  =>
  (assert (nodo (nome fase-POST) (valore ?v)(certezza ?CF1)(nodo-padre ?id-p1) (descrizione ?d)))
  (assert (nodo (nome problema-fase-boot) (valore si)(certezza ?CF1)(nodo-padre ?id-p1) (descrizione "Il dispositivo si accende e il problema si verifica durante la fase di boot.")))
)

(defrule diagnosi-errore-messaggio-POST
  ;WRITTEN DOWN
  ?p1 <- (nodo(nome fase-POST)(valore errore-messaggio)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome problema-fase-boot)(valore si)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (assert (nodo (nome diagnosi) (valore errore-POST-hardware)(certezza (calcola-certezza 0.5 ?CF1 ?CF2))(nodo-padre ?id-p1 ?id-p2)))
  (assert (nodo (nome diagnosi) (valore CMOS-corrotta)(certezza (calcola-certezza 0.7 ?CF1 ?CF2))(nodo-padre ?id-p1 ?id-p2)))
  (assert (nodo (nome diagnosi) (valore errore-POST-boot)(certezza (calcola-certezza 0.8 ?CF1 ?CF2))(nodo-padre ?id-p1 ?id-p2)))
)



(defrule diagnosi-guasto-hw-1
  ;WRITTEN DOWN
  ?p1 <- (nodo(nome fase-POST)(valore ?v&errore-messaggio|errore-beep-code|errore-riavvio)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome anni-dispositivo)(valore 4-6-anni)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (bind ?CF-diagnosi (calcola-certezza 0.3 ?CF1 ?CF2))
  (assert (nodo (nome diagnosi) (valore errore-POST-hardware)(certezza ?CF-diagnosi)(nodo-padre ?id-p1 ?id-p2)))
)

(defrule diagnosi-guasto-hw-2
  ;WRITTEN DOWN
  ?p1 <- (nodo(nome fase-POST)(valore ?v&errore-messaggio|errore-beep-code|errore-riavvio)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome anni-dispositivo)(valore 7-anni)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (bind ?CF-diagnosi (calcola-certezza 0.8 ?CF1 ?CF2))
  (assert (nodo (nome diagnosi) (valore errore-POST-hardware)(certezza ?CF-diagnosi)(nodo-padre ?id-p1 ?id-p2)))
)

(defrule diagnosi-guasto-hw-3
  ;WRITTEN DOWN
  ?p1 <- (nodo(nome fase-POST)(valore ?v&errore-messaggio|errore-beep-code|errore-riavvio)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome surriscaldamento)(valore si)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (bind ?CF-diagnosi (calcola-certezza 0.5 ?CF1 ?CF2))
  (assert (nodo (nome diagnosi) (valore errore-POST-hardware)(certezza ?CF-diagnosi)(nodo-padre ?id-p1 ?id-p2)))
)





(defrule unisci-beep-codes
  ?p1 <- (nodo(nome ?n&ami-beep-codes|award-beep-codes|asus-beep-codes|intel-beep-codes)(valore ?v)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1)(descrizione ?d))
  =>
  (assert (nodo (nome beep-code) (valore ?v) (certezza ?CF1) (attivo TRUE) (nodo-padre ?id-p1) (descrizione ?d)))
)

(defrule diagnosi-guasto-RAM-beepcode
  ?p1 <- (nodo(nome beep-code)(valore ?v&ami-1-4S|ami-1L3S|award-beep-ripetuto|asus-1L2S|intel-3S)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome versione-bios)(valore ?x&~sconosciuto)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo(nome fase-POST)(valore errore-beep-code)(certezza ?CF3)(attivo TRUE)(id-nodo ?id-p3))
  =>
  (bind ?CF (calcola-certezza 0.9 ?CF1 ?CF2 ?CF3))
  (assert (nodo (nome diagnosi) (valore guasto-RAM)(certezza ?CF)(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule diagnosi-guasto-CPU-beepcode
  ?p1 <- (nodo(nome beep-code)(valore ?v&ami-5S|award-hi-lo)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome versione-bios)(valore ?x&~sconosciuto)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo(nome fase-POST)(valore errore-beep-code)(certezza ?CF3)(attivo TRUE)(id-nodo ?id-p3))
  =>
  (bind ?CF (calcola-certezza 0.9 ?CF1 ?CF2 ?CF3))
  (assert (nodo (nome diagnosi) (valore guasto-CPU)(certezza ?CF)(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule diagnosi-guasto-VGA-beepcode
  ?p1 <- (nodo(nome beep-code)(valore ?v&ami-8S|ami-1L2S|award-1L2S|award-1L3S|asus-1L3S|intel-2S)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome versione-bios)(valore ?x&~sconosciuto)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo(nome fase-POST)(valore errore-beep-code)(certezza ?CF3)(attivo TRUE)(id-nodo ?id-p3))
  =>
  (bind ?CF (calcola-certezza 0.9 ?CF1 ?CF2 ?CF3))
  (assert (nodo (nome diagnosi) (valore guasto-vga)(certezza ?CF)(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule diagnosi-surriscaldamento-CPU-beepcode
  ?p1 <- (nodo(nome beep-code)(valore ?v&award-beep-costante|intel-hi-lo)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome versione-bios)(valore ?x&~sconosciuto)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo(nome fase-POST)(valore errore-beep-code)(certezza ?CF3)(attivo TRUE)(id-nodo ?id-p3))
  =>
  (bind ?CF (calcola-certezza 0.9 ?CF1 ?CF2 ?CF3))
  (assert (nodo (nome diagnosi) (valore surriscaldamento-CPU)(certezza ?CF)(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule diagnosi-guasto-scheda-madre-beepcode
  ?p1 <- (nodo(nome beep-code)(valore ?v&ami-9S)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome versione-bios)(valore ?x&~sconosciuto)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo(nome fase-POST)(valore errore-beep-code)(certezza ?CF3)(attivo TRUE)(id-nodo ?id-p3))
  =>
  (bind ?CF (calcola-certezza 0.9 ?CF1 ?CF2 ?CF3))
  (assert (nodo (nome diagnosi) (valore scheda-madre-guasta)(certezza ?CF)(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule diagnosi-errore-generico-beepcode
  ?p1 <- (nodo(nome beep-code)(valore ?v&ami-6-7S|asus-1L4S)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome versione-bios)(valore ?x&~sconosciuto)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo(nome fase-POST)(valore errore-beep-code)(certezza ?CF3)(attivo TRUE)(id-nodo ?id-p3))
  =>
  (bind ?CF (calcola-certezza 0.9 ?CF1 ?CF2 ?CF3))
  (assert (nodo (nome diagnosi) (valore errore-POST-hardware)(certezza ?CF)(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule diagnosi-errore-beepcode
  ;****MODDED
  ?p1 <- (nodo(nome fase-POST)(valore errore-beep-code)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  (or ?p2 <- (nodo(nome versione-bios)(valore sconosciuto)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
      ?p2 <- (nodo(nome beep-code)(valore altro)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  )
  =>
  (assert (nodo (nome diagnosi) (valore errore-POST-hardware)(certezza (calcola-certezza 0.9 ?CF1 ?CF2))(nodo-padre ?id-p1 ?id-p2)))
  (assert (nodo (nome diagnosi) (valore CMOS-corrotta)(certezza (calcola-certezza 0.7 ?CF1 ?CF2))(nodo-padre ?id-p1 ?id-p2)))
)



(defrule diagnosi-errore-BSOD
  ;WRITTEN DOWN
  ?p1 <- (nodo(nome fase-POST)(valore errore-BSOD)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome problema-fase-boot)(valore si)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (assert (nodo (nome diagnosi) (valore infetto-da-virus)(certezza (calcola-certezza 0.3 ?CF1 ?CF2))(nodo-padre ?id-p1 ?id-p2)))
  (assert (nodo (nome diagnosi) (valore CMOS-corrotta)(certezza (calcola-certezza 0.7 ?CF1 ?CF2))(nodo-padre ?id-p1 ?id-p2)))
  (assert (nodo (nome diagnosi) (valore problema-caricamento-SO)(certezza (calcola-certezza 0.9 ?CF1 ?CF2))(nodo-padre ?id-p1 ?id-p2)))
)

(defrule diagnosi-errore-BSOD-conflitto-HW
  ;WRITTEN DOWN
  ?p1 <- (nodo(nome fase-POST)(valore errore-BSOD)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome conflitto-HW-SW)(valore si-HW)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo(nome problema-fase-boot)(valore si)(certezza ?CF3)(attivo TRUE)(id-nodo ?id-p3))
  =>
  (bind ?CF (calcola-certezza 0.9 ?CF1 ?CF2 ?CF3))
  (assert (nodo (nome diagnosi) (valore conflitto-HW)(certezza ?CF)(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule diagnosi-errore-BSOD-conflitto-SW
  ;WRITTEN DOWN
  ?p1 <- (nodo(nome fase-POST)(valore errore-BSOD)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome conflitto-HW-SW)(valore ?v&si-SW|si-aggiornamenti)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo(nome problema-fase-boot)(valore si)(certezza ?CF3)(attivo TRUE)(id-nodo ?id-p3))
  =>
  (bind ?CF (calcola-certezza 0.8 ?CF1 ?CF2 ?CF3))
  (assert (nodo (nome diagnosi) (valore conflitto-SW)(certezza ?CF)(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule diagnosi-errore-BSOD-conflitto-sconosciuto
  ;WRITTEN DOWN
  ?p1 <- (nodo(nome fase-POST)(valore errore-BSOD)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome conflitto-HW-SW)(valore non-so)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo(nome problema-fase-boot)(valore si)(certezza ?CF3)(attivo TRUE)(id-nodo ?id-p3))
  =>
  (bind ?CF-SW (calcola-certezza 0.5 ?CF1 ?CF2 ?CF3))
  (assert (nodo (nome diagnosi) (valore conflitto-SW)(certezza ?CF-SW)(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule diagnosi-errore-riavvio
  ;WRITTEN DOWN
  ?p1 <- (nodo(nome fase-POST)(valore errore-riavvio)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome problema-fase-boot)(valore si)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (assert (nodo (nome diagnosi) (valore errore-POST-hardware)(certezza (calcola-certezza 0.3 ?CF1 ?CF2))(nodo-padre ?id-p1 ?id-p2)))
  (assert (nodo (nome diagnosi) (valore CMOS-corrotta)(certezza (calcola-certezza 0.7 ?CF1 ?CF2))(nodo-padre ?id-p1 ?id-p2)))
  (assert (nodo (nome diagnosi) (valore errore-POST-boot)(certezza (calcola-certezza 0.8 ?CF1 ?CF2))(nodo-padre ?id-p1 ?id-p2)))
  (assert (nodo (nome diagnosi) (valore alimentatore-guasto)(certezza (calcola-certezza 0.8 ?CF1 ?CF2))(nodo-padre ?id-p1 ?id-p2)))
  (assert (nodo (nome diagnosi) (valore scheda-madre-guasta)(certezza (calcola-certezza 0.2 ?CF1 ?CF2))(nodo-padre ?id-p1 ?id-p2)))
)



(defrule diagnosi-batteria-CMOS-esausta-1
  ;WRITTEN DOWN
  ?p1 <- (nodo(nome fase-POST)(valore ?v&errore-messaggio|errore-riavvio)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome inattivita-dispositivo)(valore si)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo(nome problema-fase-boot)(valore si)(certezza ?CF3)(attivo TRUE)(id-nodo ?id-p3))
  =>
  (bind ?CF (calcola-certezza 0.5 ?CF1 ?CF2 ?CF3))
  (assert (nodo (nome diagnosi)(valore batteria-CMOS-esausta)(certezza ?CF)(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule diagnosi-batteria-CMOS-esausta-2
  ;WRITTEN DOWN
  ?p1 <- (nodo(nome fase-POST)(valore ?v&errore-messaggio|errore-riavvio)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome anni-dispositivo)(valore 7-anni)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo(nome problema-fase-boot)(valore si)(certezza ?CF3)(attivo TRUE)(id-nodo ?id-p3))
  =>
  (bind ?CF (calcola-certezza 0.8 ?CF1 ?CF2 ?CF3))
  (assert (nodo (nome diagnosi)(valore batteria-CMOS-esausta)(certezza ?CF)(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule diagnosi-batteria-CMOS-esausta-3
  ;WRITTEN DOWN
  ?p1 <- (nodo(nome fase-POST)(valore ?v&errore-messaggio|errore-riavvio)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome anni-dispositivo)(valore 4-6-anni)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo(nome problema-fase-boot)(valore si)(certezza ?CF3)(attivo TRUE)(id-nodo ?id-p3))
  =>
  (bind ?CF (calcola-certezza 0.5 ?CF1 ?CF2 ?CF3))
  (assert (nodo (nome diagnosi)(valore batteria-CMOS-esausta)(certezza ?CF)(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)
