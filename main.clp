

(defglobal ?*highest-priority* = 1000)
(defglobal ?*domande-random* = 100)
(defglobal ?*domande-specifiche* = -100)
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
    (multislot spiegazione-risposte (type STRING) (default ?NONE))
    (slot spiegazione (type STRING))
    (slot help (type STRING))
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



;**********************************************
;* FUNZIONI E REGOLE PER FATTORI DI CERTEZZA  *
;**********************************************

;** Seleziona il minimo nella lista **
(deffunction MAIN::AND-multifield($?list)
  (bind ?min-v (nth$ 1 ?list))
  (loop-for-count (?cnt 2 (length ?list)) do
      (bind ?temp (nth$ ?cnt ?list))
      (if (< ?temp ?min-v) then (bind ?min-v ?temp))
  )
  (return ?min-v)
)

;** Seleziona il massimo nella lista **
(deffunction MAIN::OR-multifield($?list)
  (bind ?max-v (nth$ 1 ?list))
  (loop-for-count (?cnt 2 (length ?list)) do
      (bind ?temp (nth$ ?cnt ?list))
      (if (> ?temp ?max-v) then (bind ?max-v ?temp))
  )
  (return ?max-v)
)

;** Combina la certezza componente di una regola con il minimo delle ipotesi della parte sinistra **
(deffunction MAIN::calcola-certezza(?cert-RHS $?cert-LHS)
  (bind ?min-cert (AND-multifield $?cert-LHS))
  (return (* ?cert-RHS ?min-cert))
)

;** Combina la certezza di più conclusioni uguali **
(deffunction MAIN::combina-CF(?cf1 ?cf2)
  (if (and (> ?cf1 0) (> ?cf2 0)) then (bind ?CF (- (+ ?cf1 ?cf2) (* ?cf1 ?cf2)))
  )
  (if (and (< ?cf1 0) (< ?cf2 0)) then (bind ?CF (+ (+ ?cf1 ?cf2) (* ?cf1 ?cf2)))
  )
  (if (< (* ?cf1 ?cf2) 0) then (bind ?CF (/ (+ ?cf1 ?cf2) (- 1 (min (abs ?cf1)(abs ?cf2)))))
  )
  (return ?CF)
)

;** Cerca tra i fatti in memoria se sono presenti conclusioni identiche la cui certezza dev'essere combinata **
(defrule MAIN::combina-certezza-nodi
  (declare (salience ?*highest-priority*))
  ?nodo1 <- (nodo (nome ?n) (valore ?v) (certezza ?c1) (attivo TRUE) (id-nodo ?i1) (descrizione ?d))
  ?nodo2 <- (nodo (nome ?n) (valore ?v) (certezza ?c2) (attivo TRUE) (id-nodo ?i2) (descrizione ?d))
  (test (neq ?nodo1 ?nodo2))
  =>
  (modify ?nodo1 (attivo FALSE))
  (modify ?nodo2 (attivo FALSE))
  (assert (nodo (nome ?n) (valore ?v) (certezza (combina-CF ?c1 ?c2)) (nodo-padre ?i1 ?i2) (descrizione ?d)))
)

;** Rimuove dalla lista dei padri di un nodo possibili id duplicati **
(defrule MAIN::rimuovi-padri-duplicati
  (declare (salience ?*highest-priority*))
  ?n <- (nodo (nodo-padre $?nodi1 ?elem $?nodi2 ?elem $?nodi3))
  =>
  (modify ?n (nodo-padre ?nodi1 ?elem ?nodi2 ?nodi3))
)

;;**************************************
;;* REGOLO DI CONTROLLO DEL PROGRAMMA  *
;;**************************************


;** Regola che viene eseguita all'avvio del programma: carica gli altri moduli e setta la strategia come random **
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
  (set-strategy random)
  (assert (in-esecuzione))
)

;** Se non sono state poste tutte le domande random, allora setta la strategia a random (utilizzata dopo la ritrattazione) **
(defrule MAIN::poni-domande-random
  (declare (salience ?*highest-priority*))
  (or
    (not (nodo (nome esperienza-utente) (sorgente-info utente)))
    (not (nodo (nome anni-dispositivo)  (sorgente-info utente)))
    (not (nodo (nome garanzia)  (sorgente-info utente)))
    (not (nodo (nome tipo-dispositivo)  (sorgente-info utente)))
    (not (nodo (nome utente-proprietario)  (sorgente-info utente)))
  )
  =>
  (set-strategy random)
)

;** Se tutte le domande random sono state poste, cambia la strategia a complexity **
(defrule MAIN::fine-domande-random
  (declare (salience ?*highest-priority*))
  (nodo (nome esperienza-utente) (id-nodo ?id-p1) (attivo TRUE))
  (nodo (nome anni-dispositivo) (id-nodo ?id-p2) (attivo TRUE))
  (nodo (nome garanzia) (id-nodo ?id-p3) (attivo TRUE))
  (nodo (nome tipo-dispositivo) (id-nodo ?id-p4) (attivo TRUE))
  (nodo (nome utente-proprietario) (id-nodo ?id-p5) (attivo TRUE))
  =>
  (set-strategy complexity)
)

;** Permette di riattivare le regole di controllo dopo la ritrattazione **
(defrule MAIN::riattiva-esecuzione
  (declare (salience ?*highest-priority*))
  (not(in-esecuzione))
  =>
  (assert (in-esecuzione))
)

;** Se il sistema ha trovato una diagnosi certa, allora passa a cercare soluzioni e quindi alla stampa delle diagnosi **
(defrule MAIN::stop-diagnosi-certa
  (declare (salience ?*highest-priority*))
  (nodo (nome diagnosi) (valore ?v) (certezza 1.0))
  ?f <- (in-esecuzione)
  =>
  (focus MODULO-SOLUZIONE MODULO-STAMPA MODULO-RITRATTAZIONE)
)

;** Se il sistema ha esaurito le regole attivabili, allora passa a cercare soluzioni e quindi alla stampa delle diagnosi  **
(defrule MAIN::stop-ultima-regola-in-agenda
  (declare (salience ?*lowest-priority*))
  ?f <- (in-esecuzione)
  =>
  (focus MODULO-SOLUZIONE MODULO-STAMPA MODULO-RITRATTAZIONE)
)

;** Restituisce la descrizione delle risposte possibili di una domanda **
(deffunction get-descrizione-risposta(?attributo ?risposta)
    (bind ?dom (find-fact ((?d domanda))(eq ?d:attributo ?attributo)))
    (bind ?domanda (nth$ 1 ?dom))
    (bind ?risposte-valide (fact-slot-value ?domanda risposte-valide))
    (bind ?spiegazioni-valide (fact-slot-value ?domanda spiegazione-risposte))

    (loop-for-count (?cnt1 1 (length ?risposte-valide)) do
        (if (eq ?risposta (nth$ ?cnt1 ?risposte-valide)) then (return (nth$ ?cnt1 ?spiegazioni-valide)))
    )
)


;** Chiede la domanda all'utente e ne restituisce la risposta **
(deffunction chiedi-domanda-fnz(?attributo)
    (bind ?dom (find-fact ((?d domanda))(eq ?d:attributo ?attributo)))
    (bind ?domanda (nth$ 1 ?dom))
    (bind ?testo-domanda (fact-slot-value ?domanda testo-domanda))
    (bind ?risposte-valide (fact-slot-value ?domanda risposte-valide))
    (bind ?descrizioni (fact-slot-value ?domanda descrizione-risposte))
    (bind ?spiegazioni-valide (fact-slot-value ?domanda spiegazione-risposte))
    (bind ?spiegazione (fact-slot-value ?domanda spiegazione))
    (bind ?aiuto (fact-slot-value ?domanda help))
    (bind ?num-domanda (next-num-domanda))

    (clear-window)
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


;; *****************************************************************************
;; ************                   REGOLE DOMANDE RANDOM              ***********
;; *****************************************************************************

(defrule chiedi-tipo-dispositivo
  (declare (salience ?*domande-random*))
  (domanda (attributo ?attr&tipo-dispositivo)(gia-chiesta FALSE))
  =>
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-utente-proprietario
  (declare (salience ?*domande-random*))
  (domanda (attributo ?attr&utente-proprietario)(gia-chiesta FALSE))
  =>
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-esperienza-utente
  (declare (salience ?*domande-random*))
  (domanda (attributo ?attr&esperienza-utente)(gia-chiesta FALSE))
  =>
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-anni-dispositivo
  (declare (salience ?*domande-random*))
  (domanda (attributo ?attr&anni-dispositivo)(gia-chiesta FALSE))
  =>
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-garanzia
  (declare (salience ?*domande-random*))
  (domanda (attributo ?attr&garanzia)(gia-chiesta FALSE))
  =>
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)


;; *****************************************************************************
;; ************                   REGOLE ALTRE DOMANDE               ***********
;; *****************************************************************************

(defrule chiedi-surriscaldamento
  (declare (salience ?*domande-specifiche*))
  (domanda (attributo ?attr&surriscaldamento)(gia-chiesta FALSE))
  ?p2 <- (nodo(nome utente-proprietario)(valore si)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-inattivita-dispositivo
  (declare (salience ?*domande-specifiche*))
  (domanda (attributo ?attr&inattivita-dispositivo)(gia-chiesta FALSE))
  ?p2 <- (nodo(nome utente-proprietario)(valore si)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-installazione-hw
  (declare (salience ?*domande-specifiche*))
  (domanda (attributo ?attr&installazione-hw)(gia-chiesta FALSE))
  ?p1 <- (nodo (nome stato-accensione) (valore ?v&funzionante|riavvio) (certezza ?CF1) (id-nodo ?id-p1))
  =>
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-installazione-sw
  (declare (salience ?*domande-specifiche*))
  (domanda (attributo ?attr&installazione-sw)(gia-chiesta FALSE))
  ?p1 <- (nodo (nome stato-accensione) (valore ?v&funzionante|riavvio) (certezza ?CF1) (id-nodo ?id-p1))
  =>
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-alimentatore-di-riserva
  (declare (salience ?*domande-specifiche*))
  (domanda (attributo ?attr&alimentatore-di-riserva)(gia-chiesta FALSE))
  ?p1 <- (nodo (nome stato-accensione) (valore ?v&non-funzionante|riavvio) (certezza ?CF1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome tipo-dispositivo) (valore pc-portatile) (certezza ?CF2) (id-nodo ?id-p2))
  =>
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-cavo-alimentazione-di-riserva
  (declare (salience ?*domande-specifiche*))
  (domanda (attributo ?attr&cavo-alimentazione-di-riserva)(gia-chiesta FALSE))
  ?p1 <- (nodo (nome stato-accensione) (valore ?v&non-funzionante|riavvio) (certezza ?CF1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome tipo-dispositivo) (valore pc-desktop) (certezza ?CF2) (id-nodo ?id-p2))
  =>
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-ha-batteria
  (declare (salience ?*domande-specifiche*))
  (domanda (attributo ?attr&ha-batteria)(gia-chiesta FALSE))
  ?p1 <- (nodo (nome tipo-dispositivo) (valore pc-portatile) (id-nodo ?id-p1))
  =>
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc) (nodo-padre ?id-p1)))
)

(defrule chiedi-corrente-su-presa-elettrica
  (declare (salience ?*domande-specifiche*))
  (domanda (attributo ?attr&corrente-su-presa-elettrica)(gia-chiesta FALSE))
  ?p1 <- (nodo (nome stato-accensione) (valore non-funzionante) (certezza ?CF1) (id-nodo ?id-p1))
  =>
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-alimentazione-collegata
  (declare (salience ?*domande-specifiche*))
  (domanda (attributo ?attr&alimentazione-collegata)(gia-chiesta FALSE))
  ?p1 <- (nodo (nome stato-accensione) (valore non-funzionante) (certezza ?CF1) (id-nodo ?id-p1))
  =>
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-problema-video-dispositivo
  (declare (salience ?*domande-specifiche*))
  (domanda (attributo ?attr&problema-video-dispositivo)(gia-chiesta FALSE))
  ?p1 <- (nodo (nome stato-accensione)(valore ?v1&funzionante|riavvio)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  (not (nodo (nome problema-principale)(valore ?v2&schermo-nero|macchie-schermo|fasce-schermo)(certezza ?CF2&:(> ?CF2 0))(attivo TRUE)(id-nodo ?id-p2)))
  =>
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-blue-screen
  (declare (salience ?*domande-specifiche*))
  (domanda (attributo ?attr&blue-screen)(gia-chiesta FALSE))
  ?p1 <- (nodo (nome stato-accensione) (valore ?v&funzionante|riavvio) (certezza ?CF1) (id-nodo ?id-p1))
  (not(nodo(nome problema-principale)(valore ?v2)))
  =>
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-messaggio-di-errore
  (declare (salience ?*domande-specifiche*))
  (domanda (attributo ?attr&messaggio-di-errore)(gia-chiesta FALSE))
  ?p1 <- (nodo (nome stato-accensione) (valore ?v&funzionante|riavvio)  (certezza ?CF1) (id-nodo ?id-p1))
  (not(nodo(nome problema-principale)(valore ?v2)))
  =>
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-codice-acustico
  (declare (salience ?*domande-specifiche*))
  (domanda (attributo ?attr&codice-acustico)(gia-chiesta FALSE))
  ?p1 <- (nodo (nome stato-accensione) (valore ?v&funzionante|riavvio)  (certezza ?CF1) (id-nodo ?id-p1))
  (not(nodo(nome problema-principale)(valore ?v2)))
  =>
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-domanda-accensione
  (declare (salience ?*domande-specifiche*))
  (domanda (attributo ?attr&domanda-accensione)(gia-chiesta FALSE))
  =>
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-domanda-accensione-ut-inesperto
  (declare (salience ?*domande-specifiche*))
  (domanda (attributo ?attr&domanda-accensione-ut-inesperto)(gia-chiesta FALSE))
  ?p1 <- (nodo (nome esperienza-utente) (valore utente-inesperto) (certezza ?CF1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome domanda-accensione) (valore non-si-accende) (certezza ?CF2) (id-nodo ?id-p2))
  =>
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-rimuovere-batteria
  (declare (salience ?*domande-specifiche*))
  (domanda (attributo ?attr&batteria-difettosa)(gia-chiesta FALSE))
  ?p1 <- (nodo (nome ha-batteria) (valore si) (certezza ?CF1) (id-nodo ?id-p1))
  ?p3 <- (nodo (nome stato-accensione) (valore ?v&non-funzionante|riavvio) (certezza ?CF3) (id-nodo ?id-p3))
  =>
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-spia-alimentatore-pcportatile
  (declare (salience ?*domande-specifiche*))
  (domanda (attributo ?attr&spia-alimentatore-pcportatile)(gia-chiesta FALSE))
  ?p1 <- (nodo (nome tipo-dispositivo) (valore pc-portatile) (certezza ?CF1) (id-nodo ?id-p1))
  ?p3 <- (nodo (nome stato-accensione) (valore non-funzionante) (certezza ?CF3) (id-nodo ?id-p3))
  =>
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-spia-alimentatore-pcdesktop
  (declare (salience ?*domande-specifiche*))
  (domanda (attributo ?attr&spia-alimentatore-pcdesktop)(gia-chiesta FALSE))
  ?p1 <- (nodo (nome tipo-dispositivo) (valore pc-desktop) (certezza ?CF1) (id-nodo ?id-p1))
  ?p3 <- (nodo (nome stato-accensione) (valore  non-funzionante) (certezza ?CF3) (id-nodo ?id-p3))
  =>
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-interruttore-alimentatore
  (declare (salience ?*domande-specifiche*))
  (domanda (attributo ?attr&interruttore-alimentatore)(gia-chiesta FALSE))
  ?p1 <- (nodo (nome tipo-dispositivo) (valore pc-desktop) (certezza ?CF1) (id-nodo ?id-p1))
  ?p3 <- (nodo (nome stato-accensione) (valore  non-funzionante) (certezza ?CF3) (id-nodo ?id-p3))
  =>
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-schermo-nero
  (declare (salience ?*domande-specifiche*))
  (domanda (attributo ?attr&schermo-nero)(gia-chiesta FALSE))
  ?p1 <- (nodo (nome problema-video-dispositivo)(valore si)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  (not (nodo (nome problema-principale)(valore ?v&schermo-nero|macchie-schermo|fasce-schermo)))
  =>
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-macchie-schermo
  (declare (salience ?*domande-specifiche*))
  (domanda (attributo ?attr&macchie-schermo)(gia-chiesta FALSE))
  ?p1 <- (nodo (nome problema-video-dispositivo)(valore si)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  (not (nodo (nome problema-principale)(valore ?v&schermo-nero|macchie-schermo|fasce-schermo)))
  =>
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-fasce-schermo
  (declare (salience ?*domande-specifiche*))
  (domanda (attributo ?attr&fasce-schermo)(gia-chiesta FALSE))
  ?p1 <- (nodo (nome problema-video-dispositivo)(valore si)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  (not (nodo (nome problema-principale)(valore ?v&schermo-nero|macchie-schermo|fasce-schermo)))
  =>
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-cavi-monitor
  (declare (salience ?*domande-specifiche*))
  (domanda (attributo ?attr&cavi-monitor)(gia-chiesta FALSE))
  ?p1 <- (nodo (nome problema-principale)(valore ?v&schermo-nero|fasce-schermo|macchie-schermo)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome cavi-display-accessibili)(valore si)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-monitor-acceso

  (declare (salience ?*domande-specifiche*))
  (domanda (attributo ?attr&monitor-acceso)(gia-chiesta FALSE))
  ?p1 <- (nodo (nome problema-principale)(valore schermo-nero)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome tipo-dispositivo)(valore pc-desktop)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-monitor-secondario
  (declare (salience ?*domande-specifiche*))
  (domanda (attributo ?attr&monitor-secondario)(gia-chiesta FALSE))
  ?p1 <- (nodo (nome problema-principale)(valore ?v1&fasce-schermo|schermo-nero)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
  =>
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-problema-video-all-avvio

  (declare (salience ?*domande-specifiche*))
  (domanda (attributo ?attr&problema-video-all-avvio)(gia-chiesta FALSE))
  ?p1 <- (nodo (nome problema-principale)(valore ?v1&fasce-schermo|schermo-nero)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  =>
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-versione-bios
  (declare (salience ?*domande-specifiche*))
  (domanda (attributo ?attr&versione-bios)(gia-chiesta FALSE))
  ?p1 <- (nodo(nome problema-principale)(valore codice-acustico)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome esperienza-utente) (valore utente-esperto) (certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-ami-beep-codes
  (declare (salience ?*domande-specifiche*))
  (domanda (attributo ?attr&ami-beep-codes)(gia-chiesta FALSE))
  ?p1 <- (nodo(nome problema-principale)(valore codice-acustico)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome versione-bios)(valore ami-bios)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-award-beep-codes
  (declare (salience ?*domande-specifiche*))
  (domanda (attributo ?attr&award-beep-codes)(gia-chiesta FALSE))
  ?p1 <- (nodo(nome problema-principale)(valore codice-acustico)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome versione-bios)(valore award-bios)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-asus-beep-codes
  (declare (salience ?*domande-specifiche*))
  (domanda (attributo ?attr&asus-beep-codes)(gia-chiesta FALSE))
  ?p1 <- (nodo(nome problema-principale)(valore codice-acustico)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome versione-bios)(valore asus)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-intel-beep-codes
  (declare (salience ?*domande-specifiche*))
  (domanda (attributo ?attr&intel-beep-codes)(gia-chiesta FALSE))
  ?p1 <- (nodo(nome problema-principale)(valore codice-acustico)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome versione-bios)(valore intel)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)

(defrule chiedi-asrock-beep-codes
  (declare (salience ?*domande-specifiche*))
  (domanda (attributo ?attr&ami-beep-codes)(gia-chiesta FALSE))
  ?p1 <- (nodo(nome problema-principale)(valore codice-acustico)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome versione-bios)(valore asrock)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (bind ?val (chiedi-domanda-fnz ?attr))
  (bind ?desc (get-descrizione-risposta ?attr ?val))
  (assert (nodo (nome ?attr) (valore ?val) (sorgente-info utente) (descrizione ?desc)))
)




;; ************************************************************************************************
;; ************                   REGOLE PER INFERENZA DI NUOVA CONOSCENZA              ***********
;; ************************************************************************************************

(defrule portatile-cavi-accessibili
  ?p1 <- (nodo (nome tipo-dispositivo) (valore pc-portatile) (certezza ?c1) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome cavi-display-accessibili) (valore no) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1) (descrizione "I cavi che collegano il dispositivo al display non sono accessibili all'utente.")))
)

(defrule portatile-alimentatore-caricabatterie
  ?p1 <- (nodo (nome tipo-dispositivo) (valore pc-portatile) (certezza ?c1) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome alimentatore-caricabatterie) (valore si) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1) (descrizione "Il dispositivo possiede un alimentatore caricabatterie esterno.")))
)

(defrule portatile-alimentatore-acceso
  ?p1 <- (nodo (nome tipo-dispositivo) (valore pc-portatile) (certezza ?c1) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome interruttore-alimentatore) (valore acceso) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1) (descrizione "Essendo un dispositivo portatile, l'alimentatore non possiede un tasto di accensione esterno.")))
)

(defrule desktop-no-batteria
  ?p1 <- (nodo (nome tipo-dispositivo) (valore pc-desktop) (certezza ?c1) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome ha-batteria) (valore no) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1) (descrizione "Il dispositivo, essendo un pc fisso, non possiede una batteria di alimentazione.")))
)

(defrule desktop-cavi-accessibili
  ?p1 <- (nodo (nome tipo-dispositivo) (valore pc-desktop) (certezza ?c1) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome cavi-display-accessibili) (valore si) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1) (descrizione "I cavi che collegano il dispositivo al display sono accessibili all'utente.")))
)

(defrule eta-dispositivo-sconosciuta-1
  ?p1 <- (nodo (nome anni-dispositivo) (valore sconosciuto) (sorgente-info utente) (certezza ?CF1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome garanzia) (valore ?v&sconosciuto|no) (sorgente-info utente) (certezza ?CF2) (id-nodo ?id-p2))
  =>
  (assert (nodo (nome anni-dispositivo) (valore 0-3-anni) (certezza (calcola-certezza 0.5 ?CF1 ?CF2)) (descrizione "Il dispositivo potrebbe avere fino a tre anni di eta'") (nodo-padre ?id-p1 ?id-p2)))
)

(defrule eta-dispositivo-sconosciuta-2
  ?p1 <- (nodo (nome anni-dispositivo) (valore sconosciuto) (sorgente-info utente) (certezza ?CF1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome garanzia) (valore ?v&sconosciuto|no) (sorgente-info utente) (certezza ?CF2) (id-nodo ?id-p2))
  =>
  (assert (nodo (nome anni-dispositivo) (valore 4-6-anni) (certezza (calcola-certezza 0.5 ?CF1 ?CF2)) (descrizione "Il dispositivo potrebbe avere tra i quattro e i sei anni di eta'") (nodo-padre ?id-p1 ?id-p2)))
)

(defrule eta-dispositivo-sconosciuta-3
  ?p1 <- (nodo (nome anni-dispositivo) (valore sconosciuto) (sorgente-info utente) (certezza ?CF1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome garanzia) (valore ?v&sconosciuto|no) (sorgente-info utente) (certezza ?CF2) (id-nodo ?id-p2))
  =>
  (assert (nodo (nome anni-dispositivo) (valore 7-anni)   (certezza (calcola-certezza 0.5 ?CF1 ?CF2)) (descrizione "Il dispositivo potrebbe avere piu' di sei anni di eta'") (nodo-padre ?id-p1 ?id-p2)))
)

(defrule eta-dispositivo-sconosciuta-4
  ?p1 <- (nodo (nome anni-dispositivo) (valore sconosciuto) (sorgente-info utente) (certezza ?CF1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome garanzia) (valore si) (sorgente-info utente) (certezza ?CF2) (id-nodo ?id-p2))
  =>
  (assert (nodo (nome anni-dispositivo) (valore 0-3-anni) (certezza (* 0.9 ?CF1)) (descrizione "Il dispositivo potrebbe avere fino a tre anni di eta'") (nodo-padre ?id-p1)))
)

(defrule garanzia-3-anni
  ?p1 <- (nodo (nome anni-dispositivo) (valore 0-3-anni) (sorgente-info utente) (certezza ?CF1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome garanzia) (valore sconosciuto) (sorgente-info utente) (certezza ?CF2) (id-nodo ?id-p2))
  =>
  (assert (nodo (nome garanzia) (valore si) (certezza (calcola-certezza 0.8 ?CF1 ?CF2)) (descrizione "E' possibile che il dispositivo sia coperto da garanzia") (nodo-padre ?id-p1 ?id-p2)))
)

(defrule garanzia-4-anni-piu
  ?p1 <- (nodo (nome anni-dispositivo) (valore  ?val&4-6-anni|7-anni) (sorgente-info utente) (certezza ?CF1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome garanzia) (valore sconosciuto) (sorgente-info utente) (certezza ?CF2) (id-nodo ?id-p2))
  =>
  (assert (nodo (nome garanzia) (valore no) (certezza (calcola-certezza 1.0 ?CF1 ?CF2)) (descrizione "Il dispositivo non e' coperto da garanzia'") (nodo-padre ?id-p1 ?id-p2)))
)


(defrule garanzia-anni-sconosciuti
  ?p1 <- (nodo (nome anni-dispositivo) (valore sconosciuto) (sorgente-info utente) (certezza ?CF1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome garanzia) (valore sconosciuto) (sorgente-info utente) (certezza ?CF2) (id-nodo ?id-p2))
  =>
  (assert (nodo (nome garanzia) (valore si) (certezza (calcola-certezza 0.7 ?CF1 ?CF2)) (descrizione "E' possibile che il dispositivo sia coperto da garanzia") (nodo-padre ?id-p1 ?id-p2)))

)

(defrule utente-inesperto
  ?p1 <- (nodo (nome esperienza-utente) (valore utente-inesperto) (certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  =>
  (assert (nodo (nome versione-bios) (valore sconosciuto) (certezza (calcola-certezza 1 ?CF1)) (nodo-padre ?id-p1)))
)



(defrule cavo-alimentatore-danneggiato-1
  ?p1 <- (nodo (nome cavo-alimentazione-di-riserva) (valore funzionante) (certezza ?CF1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome stato-accensione) (valore ?v&non-funzionante|riavvio) (certezza ?CF2) (id-nodo ?id-p2))
  ?p3 <- (nodo (nome tipo-dispositivo) (valore pc-desktop) (certezza ?CF3) (id-nodo ?id-p3))
  =>
  (assert (nodo (nome cavo-alimentazione-danneggiato) (valore si) (certezza (calcola-certezza 1 ?CF1 ?CF2 ?CF3)) (nodo-padre ?id-p1 ?id-p2 ?id-p3) (descrizione "Il cavo di riserva funziona, quindi il cavo di alimentazione potrebbe essere danneggiato")))
)

(defrule cavo-alimentatore-danneggiato-2
  ?p1 <- (nodo (nome alimentatore-di-riserva) (valore funzionante) (certezza ?CF1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome stato-accensione) (valore ?v&non-funzionante|riavvio) (certezza ?CF2) (id-nodo ?id-p2))
  ?p3 <- (nodo (nome tipo-dispositivo) (valore pc-portatile) (certezza ?CF3) (id-nodo ?id-p3))
  =>
  (assert (nodo (nome cavo-alimentazione-danneggiato) (valore si) (certezza (calcola-certezza 0.8 ?CF1 ?CF2 ?CF3)) (nodo-padre ?id-p1 ?id-p2 ?id-p3)(descrizione "Il cavo di riserva funziona, quindi il cavo di alimentazione o l'alimentatore potrebbe essere danneggiato")))
)

(defrule alimentatore-guasto-1
  ?p1 <- (nodo (nome alimentatore-di-riserva) (valore funzionante) (certezza ?CF1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome stato-accensione) (valore ?v&non-funzionante|riavvio) (certezza ?CF2) (id-nodo ?id-p2))
  ?p3 <- (nodo (nome tipo-dispositivo) (valore pc-portatile) (certezza ?CF3) (id-nodo ?id-p3))
  =>
  (assert (nodo (nome diagnosi) (valore alimentatore-guasto) (certezza (calcola-certezza 0.8 ?CF1 ?CF2 ?CF3)) (nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule corrente-assente
  ?p1 <- (nodo (nome corrente-su-presa-elettrica) (valore no) (certezza ?CF1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome stato-accensione) (valore non-funzionante) (certezza ?CF2) (id-nodo ?id-p2))
  =>
  (assert (nodo (nome diagnosi) (valore corrente-assente) (certezza (calcola-certezza 1 ?CF1 ?CF2)) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule cavo-alimentatore-guasto-desktop
  ?p1 <- (nodo (nome cavo-alimentazione-danneggiato) (valore si) (certezza ?CF1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome stato-accensione) (valore ?v&non-funzionante|riavvio) (certezza ?CF2) (id-nodo ?id-p2))
  ?p3 <- (nodo (nome tipo-dispositivo) (valore pc-desktop) (certezza ?CF3) (id-nodo ?id-p3))
  =>
  (assert (nodo (nome diagnosi) (valore cavo-alimentatore-pcdesktop-guasto) (certezza (calcola-certezza 1 ?CF1 ?CF2 ?CF3)) (nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule cavo-alimentatore-guasto-portatile
  ?p1 <- (nodo (nome cavo-alimentazione-danneggiato) (valore si) (certezza ?CF1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome stato-accensione) (valore ?v&non-funzionante|riavvio) (certezza ?CF2) (id-nodo ?id-p2))
  ?p3 <- (nodo (nome tipo-dispositivo) (valore pc-portatile) (certezza ?CF3) (id-nodo ?id-p3))
  =>
  (assert (nodo (nome diagnosi) (valore cavo-alimentatore-pcportatile-guasto) (certezza (calcola-certezza 1 ?CF1 ?CF2 ?CF3)) (nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule riavvio-non-controllato
  ?p1 <- (nodo (nome domanda-accensione) (valore riavvio) (certezza ?CF1) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome stato-accensione) (valore riavvio) (certezza (calcola-certezza 1 ?CF1 )) (nodo-padre ?id-p1 ) (descrizione "Il dispositivo si riavvia da solo in continuazione")))
)

(defrule alimentazione-non-collegata
  ?p1 <- (nodo (nome alimentazione-collegata) (valore no) (certezza ?CF1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome stato-accensione) (valore non-funzionante) (certezza ?CF2) (id-nodo ?id-p2))
  =>
  (assert (nodo (nome diagnosi) (valore alimentazione-disconnessa)(nodo-padre ?id-p1 ?id-p2) (certezza (calcola-certezza 1.0 ?CF1 ?CF2))))
)

(defrule stato-accensione-ut-esperto
  ?p1 <- (nodo (nome esperienza-utente) (valore utente-esperto) (certezza ?CF1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome domanda-accensione) (valore non-si-accende) (certezza ?CF2) (id-nodo ?id-p2))
  =>
  (assert (nodo (nome stato-accensione) (valore non-funzionante) (certezza (calcola-certezza 1 ?CF1 ?CF2 )) (nodo-padre ?id-p1 ?id-p2) (descrizione "Il dispositivo non si accende, c'e' un problema con il circuito di alimentazione.")))
)

(defrule stato-accensione-funzionante
  ?p1 <- (nodo (nome domanda-accensione) (valore si-accende) (certezza ?CF1) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome stato-accensione) (valore funzionante) (certezza (calcola-certezza 1.0 ?CF1)) (nodo-padre ?id-p1 ) (descrizione "Il dispositivo si accende, il circuito di alimentazione sembra funzionare.")))
)

(defrule stato-accensione-ut-inesperto

  ?p1 <- (nodo (nome domanda-accensione) (valore non-si-accende) (certezza ?CF1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome domanda-accensione-ut-inesperto) (valore non-funzionante) (certezza ?CF2) (id-nodo ?id-p2))
  ?p3 <- (nodo (nome esperienza-utente) (valore utente-inesperto) (certezza ?CF1) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome stato-accensione) (valore non-funzionante) (certezza (calcola-certezza 1 ?CF1 ?CF2)) (nodo-padre ?id-p1 ?id-p2) (descrizione "Il dispositivo non si accende, c'e' un problema con il circuito di alimentazione.")))
  )

(defrule problema-schermo-nero-2
  ?p1 <- (nodo (nome domanda-accensione) (valore non-si-accende) (certezza ?CF1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome domanda-accensione-ut-inesperto) (valore funzionante) (certezza ?CF2) (id-nodo ?id-p2))
  =>
  (assert (nodo (nome problema-principale) (valore schermo-nero) (certezza (calcola-certezza 1 ?CF1 ?CF2)) (nodo-padre ?id-p1 ?id-p2) (descrizione "Il dispositivo si accende ma lo schermo e' nero e non sembra dare segni di vita.")))
  )

(defrule problema-schermo-nero-1
  ?p1 <- (nodo (nome domanda-accensione) (valore non-si-accende) (certezza ?CF1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome domanda-accensione-ut-inesperto) (valore funzionante) (certezza ?CF2) (id-nodo ?id-p2))
  =>
  (assert (nodo (nome stato-accensione) (valore funzionante) (certezza (calcola-certezza 1 ?CF1 ?CF2)) (nodo-padre ?id-p1 ?id-p2) (descrizione "Il dispositivo si accende, il circuito di alimentazione sembra funzionare.")))
)

(defrule problema-schermo-nero-3
  ?p1 <- (nodo (nome domanda-accensione) (valore non-si-accende) (certezza ?CF1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome domanda-accensione-ut-inesperto) (valore funzionante) (certezza ?CF2) (id-nodo ?id-p2))
  =>
  (assert (nodo (nome problema-video-dispositivo)(valore si)(certezza (calcola-certezza 1 ?CF1 ?CF2))(nodo-padre ?id-p1 ?id-p2) (descrizione "Il dispositivo sembra avere un problema al display.")))
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

(defrule spia-alimentatore-sconosciuta-1
  ?p1 <- (nodo (nome ?att&spia-alimentatore-pcportatile|spia-alimentatore-pcdesktop) (valore sconosciuto) (certezza ?CF1) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome spia-alimentatore) (valore accesa) (certezza (* 0.5 ?CF1)) (nodo-padre ?id-p1) (descrizione "La spia dell'alimentatore potrebbe essere accesa.")))
)

(defrule spia-alimentatore-sconosciuta-2
  ?p1 <- (nodo (nome ?att&spia-alimentatore-pcportatile|spia-alimentatore-pcdesktop) (valore sconosciuto) (certezza ?CF1) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome spia-alimentatore) (valore spenta) (certezza (* 0.5 ?CF1)) (nodo-padre ?id-p1) (descrizione "La spia dell'alimentatore potrebbe essere spenta.")))
)

(defrule corrente-su-scheda-madre-si
  ?p1 <- (nodo (nome spia-alimentatore) (valore accesa) (certezza ?CF1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome corrente-su-alimentatore) (valore si) (certezza ?CF2) (id-nodo ?id-p2))
  =>
  (assert (nodo (nome corrente-su-scheda-madre) (valore si) (nodo-padre ?id-p1 ?id-p2) (certezza (calcola-certezza 0.9 ?CF1 ?CF2)) (descrizione "La corrente elettrica raggiunge la scheda madre.")))
)

(defrule corrente-su-scheda-madre-no
  ?p1 <- (nodo (nome spia-alimentatore) (valore spenta) (certezza ?CF1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome corrente-su-alimentatore) (valore si) (certezza ?CF2) (id-nodo ?id-p2))
  =>
  (assert (nodo (nome corrente-su-scheda-madre) (valore no) (nodo-padre ?id-p1 ?id-p2) (certezza (calcola-certezza 0.8 ?CF1 ?CF2)) (descrizione "La corrente elettrica non raggiunge la scheda madre.")))
)

(defrule batteria-difettosa
  ?p1 <- (nodo (nome batteria-difettosa) (valore si) (certezza ?CF1) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome diagnosi) (valore batteria-difettosa) (certezza (calcola-certezza 1.0 ?CF1)) (nodo-padre ?id-p1)))
)

(defrule corrente-su-alimentatore-1
  ?p1 <- (nodo (nome corrente-su-presa-elettrica) (valore si) (certezza ?CF1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome interruttore-alimentatore) (valore acceso) (certezza ?CF2) (id-nodo ?id-p2))
  ?p3 <- (nodo (nome alimentazione-collegata) (valore si) (certezza ?CF3) (id-nodo ?id-p3))
  =>
  (assert (nodo (nome corrente-su-alimentatore) (valore si) (certezza (calcola-certezza 1 ?CF1 ?CF2 ?CF3)) (nodo-padre ?id-p1 ?id-p2 ?id-p3 ) (descrizione "L'alimentatore e' connesso alla rete elettrica e sta ricevendo corrente")))
)

(defrule corrente-su-alimentatore-2
  ?p1 <- (nodo (nome stato-accensione) (valore riavvio) (certezza ?CF1) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome corrente-su-alimentatore) (valore si) (certezza (calcola-certezza 1 ?CF1)) (nodo-padre ?id-p1) (descrizione "Il dispositivo si riavvia, pertanto l'alimentatore e' connesso alla rete elettrica e sta ricevendo corrente")))
)

(defrule alimentatore-guasto-2
  ?p1 <- (nodo  (nome corrente-su-alimentatore) (valore si) (certezza ?CF1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome stato-accensione) (valore non-funzionante) (certezza ?CF2) (id-nodo ?id-p2))
  =>
  (assert (nodo (nome diagnosi) (valore alimentatore-guasto) (certezza (calcola-certezza 0.7 ?CF1 ?CF2)) (nodo-padre ?id-p1 ?id-p2))) ;**
)

(defrule scheda-madre-guasta-1
  ?p1 <- (nodo  (nome corrente-su-alimentatore) (valore si) (certezza ?CF1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome stato-accensione) (valore non-funzionante) (certezza ?CF2) (id-nodo ?id-p2))
  =>
  (assert (nodo (nome diagnosi) (valore scheda-madre-guasta) (certezza (calcola-certezza 0.7 ?CF1 ?CF2)) (nodo-padre ?id-p1 ?id-p2))) ;**
)

(defrule alimentatore-riserva-non-disponibile
  ?p1 <- (nodo (nome alimentatore-di-riserva) (valore non-disponibile) (certezza ?CF1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome stato-accensione) (valore ?v&non-funzionante|riavvio) (certezza ?CF2) (id-nodo ?id-p2))
  ?p3 <- (nodo  (nome corrente-su-alimentatore) (valore si) (certezza ?CF3) (id-nodo ?id-p3))
  ?p4 <- (nodo (nome tipo-dispositivo) (valore pc-portatile) (certezza ?CF4) (id-nodo ?id-p4))
  =>
  (assert (nodo (nome cavo-alimentazione-danneggiato) (valore si) (certezza (calcola-certezza 0.8 ?CF1 ?CF2 ?CF3 ?CF4)) (nodo-padre ?id-p1 ?id-p2 ?id-p3 ?id-p4) (descrizione "E' possibile che il cavo di alimentazione sia danneggiato")))
)

(defrule scheda-madre-guasta-2
  ?p1 <- (nodo  (nome corrente-su-alimentatore) (valore si) (certezza ?CF1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome stato-accensione) (valore  non-funzionante) (certezza ?CF2) (id-nodo ?id-p2))
  ?p3 <- (nodo (nome corrente-su-scheda-madre) (valore si) (certezza ?CF3) (id-nodo ?id-p3))
  =>
  (assert (nodo (nome diagnosi) (valore scheda-madre-guasta)(nodo-padre ?id-p1 ?id-p2 ?id-p3) (certezza (calcola-certezza 0.8 ?CF1 ?CF2 ?CF3 ))))
)

(defrule scheda-madre-guasta-3
  ?p1 <- (nodo  (nome corrente-su-alimentatore) (valore si) (certezza ?CF1) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome stato-accensione) (valore  non-funzionante) (certezza ?CF2) (id-nodo ?id-p2))
  ?p3 <- (nodo (nome alimentatore-di-riserva) (valore non-funzionante) (certezza ?CF3) (id-nodo ?id-p3))
  =>
  (assert (nodo (nome diagnosi) (valore scheda-madre-guasta)(nodo-padre ?id-p1 ?id-p2 ?id-p3) (certezza (calcola-certezza 0.9 ?CF1 ?CF2 ?CF3))))
)

(defrule alimentatore-guasto-3
  ?p1 <- (nodo  (nome corrente-su-alimentatore) (valore si) (certezza ?CF1) (id-nodo ?id-p1))
  ?p3 <- (nodo (nome stato-accensione) (valore  non-funzionante) (certezza ?CF3) (id-nodo ?id-p3))
  ?p4 <- (nodo (nome corrente-su-scheda-madre) (valore no) (certezza ?CF4) (id-nodo ?id-p4))
  =>
  (assert (nodo (nome diagnosi) (valore alimentatore-guasto)(nodo-padre ?id-p1 ?id-p3 ?id-p4) (certezza (calcola-certezza 0.8 ?CF1 ?CF3 ?CF4))))
)

(defrule alimentatore-spento-1
  ?p1 <- (nodo (nome tipo-dispositivo) (valore pc-desktop) (certezza ?CF1) (id-nodo ?id-p1))
  ?p3 <- (nodo (nome stato-accensione) (valore  non-funzionante) (certezza ?CF3) (id-nodo ?id-p3))
  ?p4 <- (nodo (nome interruttore-alimentatore) (valore spento) (certezza ?CF4) (id-nodo ?id-p4))
  =>
  (assert (nodo (nome diagnosi) (valore alimentatore-spento)(nodo-padre ?id-p1 ?id-p3 ?id-p4) (certezza (calcola-certezza 1.0 ?CF1 ?CF3 ?CF4))))
)

; (defrule alimentatore-funzionante
;   ?p1 <- (nodo (nome tipo-dispositivo) (valore pc-desktop) (certezza ?CF1) (id-nodo ?id-p1))
;   ?p3 <- (nodo (nome stato-accensione) (valore  non-funzionante) (certezza ?CF3) (id-nodo ?id-p3))
;   ?p4 <- (nodo (nome interruttore-alimentatore) (valore spento) (certezza ?CF4) (id-nodo ?id-p4))
;   =>
;   (assert (nodo (nome diagnosi) (valore alimentatore-guasto) (certezza (calcola-certezza -1.0 ?CF1 ?CF3 ?CF4)) (nodo-padre ?id-p1 ?id-p3 ?id-p4)))
; )
;
; (defrule scheda-madre-funzionante
;   ?p1 <- (nodo (nome tipo-dispositivo) (valore pc-desktop) (certezza ?CF1) (id-nodo ?id-p1))
;   ?p3 <- (nodo (nome stato-accensione) (valore  non-funzionante) (certezza ?CF3) (id-nodo ?id-p3))
;   ?p4 <- (nodo (nome interruttore-alimentatore) (valore spento) (certezza ?CF4) (id-nodo ?id-p4))
;   =>
;   (assert (nodo (nome diagnosi) (valore scheda-madre-guasta) (certezza (calcola-certezza -1.0 ?CF1 ?CF3 ?CF4)) (nodo-padre ?id-p1 ?id-p3 ?id-p4)))
; )

(defrule problema-principale-trovato
  ?p1 <- (nodo (nome ?att&schermo-nero|macchie-schermo|fasce-schermo) (valore si) (certezza ?CF1) (attivo TRUE) (id-nodo ?id-p1) (descrizione ?d))
  =>
  (assert (nodo (nome problema-principale) (valore ?att) (certezza (calcola-certezza 1 ?CF1)) (nodo-padre ?id-p1) (descrizione ?d)))
  )


(defrule cavi-monitor-collegati
  (or
    ?p1 <- (nodo (nome problema-principale)(valore ?v&fasce-schermo|macchie-schermo)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
    ?p1 <- (nodo (nome cavi-display-accessibili)(valore no)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
    )
  (not (nodo (nome cavi-monitor)(attivo TRUE)))
  =>
  (assert (nodo(nome cavi-monitor)(valore ok)(certezza (calcola-certezza 1.0 ?CF1))(nodo-padre ?id-p1) (descrizione "Il monitor si accende, quindi l'alimentazione sembra essere collegata.")))
)

(defrule monitor-acceso
  (or
    ?p1 <- (nodo (nome problema-principale)(valore ?v&fasce-schermo|macchie-schermo)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
    ?p1 <- (nodo (nome cavi-display-accessibili)(valore no)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
    )
  ;(not (nodo (nome cavi-monitor)(attivo TRUE)))
  =>
  (assert (nodo(nome monitor-acceso)(valore acceso)(certezza (calcola-certezza 1.0 ?CF1))(nodo-padre ?id-p1) (descrizione "Il display mostra un problema al segnale video, quindi il monitor e' acceso.")))
)

(defrule problema-SW-video-1
  ?p1 <- (nodo (nome problema-principale)(valore fasce-schermo)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome problema-video-all-avvio) (valore no)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (assert (nodo (nome diagnosi)(valore problema-SW-video)(certezza (calcola-certezza 0.8 ?c1 ?c2))(nodo-padre ?id-p1 ?id-p2)))
)

(defrule problema-SW-video-2
  ?p1 <- (nodo (nome problema-principale)(valore schermo-nero)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome problema-video-all-avvio) (valore no)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (assert (nodo (nome diagnosi)(valore problema-SW-video)(certezza (calcola-certezza 0.9 ?c1 ?c2))(nodo-padre ?id-p1 ?id-p2)))
)

(defrule display-guasto-1
  ?p1 <- (nodo (nome problema-principale)(valore schermo-nero|fasce-schermo)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
  ?p3 <- (nodo (nome problema-video-all-avvio) (valore si)(certezza ?c3)(attivo TRUE)(id-nodo ?id-p3))
  =>
  (assert (nodo (nome diagnosi) (valore guasto-display)(certezza (calcola-certezza 0.5 ?c1 ?c3))(nodo-padre ?id-p1 ?id-p3))) ;**
)

(defrule monitor-spento-1
  ?p1 <- (nodo (nome problema-principale)(valore schermo-nero)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome monitor-acceso)(valore spento)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo (nome cavi-monitor) (valore ok)(certezza ?c3)(attivo TRUE)(id-nodo ?id-p3))
  ?p4 <- (nodo (nome tipo-dispositivo) (valore pc-desktop)(certezza ?c4)(attivo TRUE)(id-nodo ?id-p4))
  =>
  (assert (nodo (nome diagnosi) (valore monitor-spento)(certezza (calcola-certezza 1 ?c1 ?c2 ?c3 ?c4))(nodo-padre ?id-p1 ?id-p2 ?id-p3 ?id-p4)))
)

(defrule inverter-guasto-1
  ?p1 <- (nodo (nome problema-principale)(valore ?v&fasce-schermo|schermo-nero)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
  ?p3 <- (nodo (nome problema-video-all-avvio) (valore si)(certezza ?c3)(attivo TRUE)(id-nodo ?id-p3))
  ?p4 <- (nodo (nome tipo-dispositivo) (valore pc-portatile)(certezza ?c4)(attivo TRUE)(id-nodo ?id-p4))
  =>
  (assert (nodo (nome diagnosi) (valore guasto-inverter)(certezza (calcola-certezza 0.8 ?c1 ?c3 ?c4))(nodo-padre ?id-p1 ?id-p3 ?id-p4)))
)

(defrule scheda-video-guasta-1
  ?p1 <- (nodo (nome problema-principale)(valore ?v&schermo-nero|fasce-schermo)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
  ?p3 <- (nodo (nome problema-video-all-avvio)(valore si)(certezza ?c3)(attivo TRUE)(id-nodo ?id-p3))
  =>
  (assert (nodo (nome diagnosi) (valore guasto-vga)(certezza (calcola-certezza 0.5 ?c1 ?c3))(nodo-padre ?id-p1 ?id-p3))) ;**
)

(defrule interferenze-cavo-1
  ?p1 <- (nodo (nome problema-principale)(valore ?v&fasce-schermo|schermo-nero)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome problema-video-all-avvio)(valore si)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo (nome cavi-display-accessibili)(valore no)(certezza ?c3)(attivo TRUE)(id-nodo ?id-p3))
  =>
  (assert (nodo (nome diagnosi) (valore interferenze-cavo)(certezza (calcola-certezza 0.8 ?c1 ?c2 ?c3))(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule problema-circuito-interno-video
    ?p1 <- (nodo (nome problema-principale)(valore ?v&fasce-schermo|schermo-nero)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
    ?p2 <- (nodo (nome monitor-secondario) (valore errore)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
    =>
    (assert (nodo (nome guasto-circuito-interno-video) (valore si)(certezza (calcola-certezza 1 ?CF1 ?CF2))(nodo-padre ?id-p1 ?id-p2) (descrizione "Il monitor secondario non funziona, pertanto potrebbe esserci un guasto sul circuito del segnale video.")))
)

(defrule problema-monitor-principale
    ?p1 <- (nodo (nome problema-principale)(valore ?v&fasce-schermo|schermo-nero)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
    ?p2 <- (nodo (nome monitor-secondario) (valore funzionante)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
    =>
    (assert (nodo (nome guasto-monitor-principale) (valore si)(certezza (calcola-certezza 1 ?CF1 ?CF2))(nodo-padre ?id-p1 ?id-p2) (descrizione "Il monitor secondario funziona, pertanto potrebbe esserci un problema legato al display del monitor principale.")))
)

(defrule problema-monitor-sconosciuto-1
    ?p1 <- (nodo (nome problema-principale)(valore ?v&fasce-schermo|schermo-nero)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
    ?p2 <- (nodo (nome monitor-secondario) (valore no)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
    =>
    (assert (nodo (nome guasto-monitor-principale) (valore si)(certezza (calcola-certezza 0.5 ?CF1 ?CF2))(nodo-padre ?id-p1 ?id-p2) (descrizione "Potrebbe esserci un problema legato al display del monitor principale.")))
)

(defrule problema-monitor-sconosciuto-2
    ?p1 <- (nodo (nome problema-principale)(valore ?v&fasce-schermo|schermo-nero)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
    ?p2 <- (nodo (nome monitor-secondario) (valore no)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
    =>
    (assert (nodo (nome guasto-circuito-interno-video) (valore si)(certezza (calcola-certezza 0.5 ?CF1 ?CF2))(nodo-padre ?id-p1 ?id-p2) (descrizione "Potrebbe esserci un guasto sul circuito del segnale video.")))
)

(defrule guasto-display-1
  ?p1 <- (nodo (nome problema-principale)(valore ?v&fasce-schermo|schermo-nero)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome guasto-monitor-principale) (valore si)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo (nome tipo-dispositivo) (valore pc-desktop)(certezza ?c3)(attivo TRUE)(id-nodo ?id-p3))
  =>
  ; (bind ?CF-display-guasto (calcola-certezza 0.9 ?c1 ?c2 ?c3))
  ; (bind ?CF-vga-guasta (calcola-certezza -1.0 ?c1 ?c2 ?c3))
  ; (bind ?CF-problema-sw (calcola-certezza -0.5 ?c1 ?c2 ?c3))
  (assert (nodo (nome diagnosi) (valore guasto-display)(certezza (calcola-certezza 0.9 ?c1 ?c2 ?c3))(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
  ;(assert (nodo (nome diagnosi) (valore guasto-vga)(certezza ?CF-vga-guasta)(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
  ;(assert (nodo (nome diagnosi)(valore problema-SW-video)(certezza ?CF-problema-sw)(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule guasto-vga-1
  ?p1 <- (nodo (nome problema-principale)(valore ?v&fasce-schermo|schermo-nero)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome guasto-circuito-interno-video) (valore si)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo (nome tipo-dispositivo) (valore pc-desktop)(certezza ?c3)(attivo TRUE)(id-nodo ?id-p3))
  =>
  ; (bind ?CF-display-guasto (calcola-certezza -1.0 ?c1 ?c2 ?c3))
  ; (bind ?CF-vga-guasta (calcola-certezza 0.8 ?c1 ?c2 ?c3))
  ; (bind ?CF-problema-sw (calcola-certezza 0.5 ?c1 ?c2 ?c3))
  ;(assert (nodo (nome diagnosi) (valore guasto-display)(certezza ?CF-display-guasto)(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
  (assert (nodo (nome diagnosi) (valore guasto-vga)(certezza (calcola-certezza 0.8 ?c1 ?c2 ?c3))(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule problema-sw-1
  ?p1 <- (nodo (nome problema-principale)(valore ?v&fasce-schermo|schermo-nero)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome guasto-circuito-interno-video) (valore si)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo (nome tipo-dispositivo) (valore pc-desktop)(certezza ?c3)(attivo TRUE)(id-nodo ?id-p3))
  =>
  (assert (nodo (nome diagnosi)(valore problema-SW-video)(certezza (calcola-certezza 0.5 ?c1 ?c2 ?c3))(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule guasto-display-2
  ?p1 <- (nodo (nome problema-principale)(valore ?v&fasce-schermo|schermo-nero)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome guasto-monitor-principale) (valore si)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo (nome tipo-dispositivo) (valore pc-portatile)(certezza ?c3)(attivo TRUE)(id-nodo ?id-p3))
  =>
  ; (bind ?CF-display-guasto (calcola-certezza 0.8 ?c1 ?c2 ?c3))
  ; (bind ?CF-inverter-guasto (calcola-certezza 0.8 ?c1 ?c2 ?c3))
  ; (bind ?CF-vga-guasta (calcola-certezza -1.0 ?c1 ?c2 ?c3))
  ; (bind ?CF-problema-sw (calcola-certezza -0.5 ?c1 ?c2 ?c3))
  (assert (nodo (nome diagnosi) (valore guasto-display)(certezza (calcola-certezza 0.8 ?c1 ?c2 ?c3))(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
  ;(assert (nodo (nome diagnosi) (valore guasto-vga)(certezza ?CF-vga-guasta)(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
  ;(assert (nodo (nome diagnosi)(valore problema-SW-video)(certezza ?CF-problema-sw)(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule inverter-guasto-2
  ?p1 <- (nodo (nome problema-principale)(valore ?v&fasce-schermo|schermo-nero)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome guasto-monitor-principale) (valore si)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo (nome tipo-dispositivo) (valore pc-portatile)(certezza ?c3)(attivo TRUE)(id-nodo ?id-p3))
  =>
  (assert (nodo (nome diagnosi) (valore guasto-inverter)(certezza (calcola-certezza 0.8 ?c1 ?c2 ?c3))(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule guasto-vga-2
  ?p1 <- (nodo (nome problema-principale)(valore ?v&fasce-schermo|schermo-nero)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome guasto-circuito-interno-video) (valore si)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo (nome tipo-dispositivo) (valore pc-portatile)(certezza ?c3)(attivo TRUE)(id-nodo ?id-p3))
  =>
  ; (bind ?CF-display-guasto (calcola-certezza -1.0 ?c1 ?c2 ?c3))
  ; (bind ?CF-inverter-guasto (calcola-certezza -1.0 ?c1 ?c2 ?c3))
  ; (bind ?CF-vga-guasta (calcola-certezza 0.8 ?c1 ?c2 ?c3))
  ; (bind ?CF-problema-sw (calcola-certezza 0.5 ?c1 ?c2 ?c3))
  ;(assert (nodo (nome diagnosi) (valore guasto-display)(certezza ?CF-display-guasto)(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
  ;(assert (nodo (nome diagnosi) (valore guasto-inverter)(certezza ?CF-inverter-guasto)(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
  (assert (nodo (nome diagnosi) (valore guasto-vga)(certezza (calcola-certezza 0.8 ?c1 ?c2 ?c3))(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule problema-sw-2
  ?p1 <- (nodo (nome problema-principale)(valore ?v&fasce-schermo|schermo-nero)(certezza ?c1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome guasto-circuito-interno-video) (valore si)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo (nome tipo-dispositivo) (valore pc-portatile)(certezza ?c3)(attivo TRUE)(id-nodo ?id-p3))
  =>
  (assert (nodo (nome diagnosi)(valore problema-SW-video)(certezza (calcola-certezza 0.5 ?c1 ?c2 ?c3))(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)


(defrule disturbo-macchie-schermo
  ?p1 <- (nodo (nome problema-principale)(valore macchie-schermo)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  =>
  (assert (nodo (nome diagnosi) (valore guasto-display)(certezza (calcola-certezza 1 ?CF1))(nodo-padre ?id-p1)))
)

(defrule diagnosi-cavi-video-scollegati
  ?p1 <- (nodo(nome problema-principale)(valore ?v&schermo-nero|fasce-schermo|macchie-schermo)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome cavi-monitor)(valore errore)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (assert (nodo (nome diagnosi) (valore cavi-monitor-scollegati)(certezza (calcola-certezza 1.0 ?CF1 ?CF2))(nodo-padre ?id-p1 ?id-p2)))
)

(defrule CMOS-corrotta-1
  ?p1 <- (nodo(nome problema-principale)(valore schermo-nero)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo (nome cavi-monitor)(valore ok)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (assert (nodo (nome diagnosi)(valore CMOS-corrotta)(certezza (calcola-certezza 0.5 ?CF1 ?CF2))(nodo-padre ?id-p1 ?id-p2)))
)



(defrule problema-principale-trovato-2
  ?p1 <- (nodo(nome ?n&messaggio-di-errore|codice-acustico|blue-screen)(valore si) (descrizione ?d)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  =>
  (assert (nodo (nome problema-principale) (valore ?n)(certezza ?CF1)(nodo-padre ?id-p1) (descrizione ?d)))
)

(defrule riavvio-senza-errori
  ?p1 <- (nodo(nome messaggio-di-errore)(valore no)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome codice-acustico)(valore no)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo(nome blue-screen)(valore no)(certezza ?CF3)(attivo TRUE)(id-nodo ?id-p3))
  ?p4 <- (nodo(nome stato-accensione)(valore riavvio) (descrizione ?d)(certezza ?CF4)(attivo TRUE)(id-nodo ?id-p4))
  =>
  (assert (nodo (nome problema-principale) (valore errore-riavvio)(certezza (calcola-certezza 1 ?CF1 ?CF2 ?CF3 ?CF4))(nodo-padre ?id-p1 ?id-p2 ?id-p3 ?id-p4) (descrizione "Il dispositivo si riavvia in continuazione, senza mostrare errori.")))
)

(defrule problema-in-fase-boot
  ?p1 <- (nodo(nome problema-principale)(valore ?v&messaggio-di-errore|codice-acustico|blue-screen|errore-riavvio) (descrizione ?d)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  (not (nodo (nome problema-fase-boot) (valore si)))
  =>
  (assert (nodo (nome problema-fase-boot) (valore si)(certezza (calcola-certezza 1 ?CF1))(nodo-padre ?id-p1) (descrizione "Il dispositivo si accende e il problema si verifica durante la fase di boot.")))
  )

(defrule diagnosi-errore-messaggio-POST-1
  ?p1 <- (nodo(nome problema-principale)(valore messaggio-di-errore)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome problema-fase-boot)(valore si)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (assert (nodo (nome diagnosi) (valore errore-POST-hardware)(certezza (calcola-certezza 0.5 ?CF1 ?CF2))(nodo-padre ?id-p1 ?id-p2)))
)

(defrule diagnosi-errore-messaggio-POST-2
  ?p1 <- (nodo(nome problema-principale)(valore messaggio-di-errore)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome problema-fase-boot)(valore si)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (assert (nodo (nome diagnosi) (valore CMOS-corrotta)(certezza (calcola-certezza 0.7 ?CF1 ?CF2))(nodo-padre ?id-p1 ?id-p2)))
)

(defrule diagnosi-errore-messaggio-POST-3
  ?p1 <- (nodo(nome problema-principale)(valore messaggio-di-errore)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome problema-fase-boot)(valore si)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (assert (nodo (nome diagnosi) (valore errore-POST-boot)(certezza (calcola-certezza 0.8 ?CF1 ?CF2))(nodo-padre ?id-p1 ?id-p2)))
)



(defrule guasto-hw-1
  ?p1 <- (nodo(nome problema-principale)(valore ?v&messaggio-di-errore|codice-acustico|errore-riavvio)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome anni-dispositivo)(valore 4-6-anni)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (assert (nodo (nome diagnosi) (valore errore-POST-hardware)(certezza (calcola-certezza 0.3 ?CF1 ?CF2))(nodo-padre ?id-p1 ?id-p2)))
)

(defrule guasto-hw-2
  ?p1 <- (nodo(nome problema-principale)(valore ?v&messaggio-di-errore|codice-acustico|errore-riavvio)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome anni-dispositivo)(valore 7-anni)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (assert (nodo (nome diagnosi) (valore errore-POST-hardware)(certezza (calcola-certezza 0.8 ?CF1 ?CF2))(nodo-padre ?id-p1 ?id-p2)))
)

(defrule guasto-hw-3
  ?p1 <- (nodo(nome problema-principale)(valore ?v&messaggio-di-errore|codice-acustico|errore-riavvio)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome surriscaldamento)(valore si)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (assert (nodo (nome diagnosi) (valore errore-POST-hardware)(certezza (calcola-certezza 0.5 ?CF1 ?CF2))(nodo-padre ?id-p1 ?id-p2)))
)

(defrule unisci-beep-codes
  ?p1 <- (nodo(nome ?n&ami-beep-codes|award-beep-codes|asus-beep-codes|intel-beep-codes)(valore ?v)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1)(descrizione ?d))
  =>
  (assert (nodo (nome beep-code) (valore ?v) (certezza ?CF1) (attivo TRUE) (nodo-padre ?id-p1) (descrizione ?d)))
)

(defrule diagnosi-guasto-RAM-beepcode
  ?p1 <- (nodo(nome beep-code)(valore ?v&ami-1-4S|ami-1L3S|award-beep-ripetuto|asus-1L2S|intel-3S)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome versione-bios)(valore ?x&~sconosciuto)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo(nome problema-principale)(valore codice-acustico)(certezza ?CF3)(attivo TRUE)(id-nodo ?id-p3))
  =>
  (assert (nodo (nome diagnosi) (valore guasto-RAM)(certezza (calcola-certezza 1.0 ?CF1 ?CF2 ?CF3))(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule diagnosi-guasto-CPU-beepcode
  ?p1 <- (nodo(nome beep-code)(valore ?v&ami-5S|award-hi-lo)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome versione-bios)(valore ?x&~sconosciuto)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo(nome problema-principale)(valore codice-acustico)(certezza ?CF3)(attivo TRUE)(id-nodo ?id-p3))
  =>
  (assert (nodo (nome diagnosi) (valore guasto-CPU)(certezza (calcola-certezza 1.0 ?CF1 ?CF2 ?CF3))(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule diagnosi-guasto-VGA-beepcode
  ?p1 <- (nodo(nome beep-code)(valore ?v&ami-8S|ami-1L2S|award-1L2S|award-1L3S|asus-1L3S|intel-2S)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome versione-bios)(valore ?x&~sconosciuto)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo(nome problema-principale)(valore codice-acustico)(certezza ?CF3)(attivo TRUE)(id-nodo ?id-p3))
  =>
  (assert (nodo (nome diagnosi) (valore guasto-vga)(certezza (calcola-certezza 1.0 ?CF1 ?CF2 ?CF3))(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule diagnosi-surriscaldamento-CPU-beepcode
  ?p1 <- (nodo(nome beep-code)(valore ?v&award-beep-costante|intel-hi-lo)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome versione-bios)(valore ?x&~sconosciuto)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo(nome problema-principale)(valore codice-acustico)(certezza ?CF3)(attivo TRUE)(id-nodo ?id-p3))
  =>
  (assert (nodo (nome diagnosi) (valore surriscaldamento-CPU)(certezza (calcola-certezza 1.0 ?CF1 ?CF2 ?CF3))(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule diagnosi-guasto-scheda-madre-beepcode
  ?p1 <- (nodo(nome beep-code)(valore ?v&ami-9S)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome versione-bios)(valore ?x&~sconosciuto)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo(nome problema-principale)(valore codice-acustico)(certezza ?CF3)(attivo TRUE)(id-nodo ?id-p3))
  =>
  (assert (nodo (nome diagnosi) (valore scheda-madre-guasta)(certezza (calcola-certezza 1.0 ?CF1 ?CF2 ?CF3))(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule diagnosi-errore-generico-beepcode
  ?p1 <- (nodo(nome beep-code)(valore ?v&ami-6-7S|asus-1L4S)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome versione-bios)(valore ?x&~sconosciuto)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo(nome problema-principale)(valore codice-acustico)(certezza ?CF3)(attivo TRUE)(id-nodo ?id-p3))
  =>
  (assert (nodo (nome diagnosi) (valore errore-POST-hardware)(certezza (calcola-certezza 1.0 ?CF1 ?CF2 ?CF3))(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule diagnosi-errore-beepcode-1
  ?p1 <- (nodo(nome problema-principale)(valore codice-acustico)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  (or ?p2 <- (nodo(nome versione-bios)(valore sconosciuto)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
      ?p2 <- (nodo(nome beep-code)(valore altro)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  )
  =>
  (assert (nodo (nome diagnosi) (valore errore-POST-hardware)(certezza (calcola-certezza 0.9 ?CF1 ?CF2))(nodo-padre ?id-p1 ?id-p2)))
)

(defrule diagnosi-errore-beepcode-2
  ?p1 <- (nodo(nome problema-principale)(valore codice-acustico)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  (or ?p2 <- (nodo(nome versione-bios)(valore sconosciuto)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
      ?p2 <- (nodo(nome beep-code)(valore altro)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  )
  =>
  (assert (nodo (nome diagnosi) (valore CMOS-corrotta)(certezza (calcola-certezza 0.7 ?CF1 ?CF2))(nodo-padre ?id-p1 ?id-p2)))
)

(defrule diagnosi-errore-BSOD-1
  ?p1 <- (nodo(nome problema-principale)(valore blue-screen)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome problema-fase-boot)(valore si)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (assert (nodo (nome diagnosi) (valore infetto-da-virus)(certezza (calcola-certezza 0.3 ?CF1 ?CF2))(nodo-padre ?id-p1 ?id-p2)))
)

(defrule diagnosi-errore-BSOD-2
  ?p1 <- (nodo(nome problema-principale)(valore blue-screen)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome problema-fase-boot)(valore si)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (assert (nodo (nome diagnosi) (valore CMOS-corrotta)(certezza (calcola-certezza 0.7 ?CF1 ?CF2))(nodo-padre ?id-p1 ?id-p2)))
)

(defrule diagnosi-errore-BSOD-3
  ?p1 <- (nodo(nome problema-principale)(valore blue-screen)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome problema-fase-boot)(valore si)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (assert (nodo (nome diagnosi) (valore problema-caricamento-SO)(certezza (calcola-certezza 0.9 ?CF1 ?CF2))(nodo-padre ?id-p1 ?id-p2)))
)

(defrule conflitto-HW-1
  ?p1 <- (nodo(nome problema-principale)(valore blue-screen)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome installazione-hw)(valore si)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo(nome problema-fase-boot)(valore si)(certezza ?CF3)(attivo TRUE)(id-nodo ?id-p3))
  =>
  (assert (nodo (nome diagnosi) (valore conflitto-HW)(certezza (calcola-certezza 0.9 ?CF1 ?CF2 ?CF3))(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule conflitto-HW-2
  ?p1 <- (nodo(nome problema-principale)(valore ?v&messaggio-di-errore|errore-riavvio)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome installazione-hw)(valore si)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo(nome problema-fase-boot)(valore si)(certezza ?CF3)(attivo TRUE)(id-nodo ?id-p3))
  =>
  (assert (nodo (nome diagnosi) (valore conflitto-HW)(certezza (calcola-certezza 0.8 ?CF1 ?CF2 ?CF3))(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule conflitto-SW-1
  ?p1 <- (nodo(nome problema-principale)(valore blue-screen)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome installazione-sw)(valore si)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo(nome problema-fase-boot)(valore si)(certezza ?CF3)(attivo TRUE)(id-nodo ?id-p3))
  =>
  (assert (nodo (nome diagnosi) (valore conflitto-SW)(certezza (calcola-certezza 0.8 ?CF1 ?CF2 ?CF3))(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule conflitto-SW-2
  ?p1 <- (nodo(nome problema-principale)(valore errore-riavvio)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome installazione-sw)(valore si)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo(nome problema-fase-boot)(valore si)(certezza ?CF3)(attivo TRUE)(id-nodo ?id-p3))
  =>
  (assert (nodo (nome diagnosi) (valore conflitto-SW)(certezza (calcola-certezza 0.7 ?CF1 ?CF2 ?CF3))(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule errore-BSOD-conflitto-sconosciuto
  ?p1 <- (nodo(nome problema-principale)(valore blue-screen)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome installazione-sw)(valore sconosciuto)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo(nome problema-fase-boot)(valore si)(certezza ?CF3)(attivo TRUE)(id-nodo ?id-p3))
  =>
  (assert (nodo (nome diagnosi) (valore conflitto-SW)(certezza (calcola-certezza 0.5 ?CF1 ?CF2 ?CF3))(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule errore-riavvio-1
  ?p1 <- (nodo(nome problema-principale)(valore errore-riavvio)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome problema-fase-boot)(valore si)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (assert (nodo (nome diagnosi) (valore errore-POST-hardware)(certezza (calcola-certezza 0.3 ?CF1 ?CF2))(nodo-padre ?id-p1 ?id-p2)))
)

(defrule errore-riavvio-2
  ?p1 <- (nodo(nome problema-principale)(valore errore-riavvio)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome problema-fase-boot)(valore si)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (assert (nodo (nome diagnosi) (valore CMOS-corrotta)(certezza (calcola-certezza 0.7 ?CF1 ?CF2))(nodo-padre ?id-p1 ?id-p2)))
)

(defrule errore-riavvio-3
  ?p1 <- (nodo(nome problema-principale)(valore errore-riavvio)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome problema-fase-boot)(valore si)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (assert (nodo (nome diagnosi) (valore errore-POST-boot)(certezza (calcola-certezza 0.8 ?CF1 ?CF2))(nodo-padre ?id-p1 ?id-p2)))
)

(defrule errore-riavvio-4
  ?p1 <- (nodo(nome problema-principale)(valore errore-riavvio)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome problema-fase-boot)(valore si)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (assert (nodo (nome diagnosi) (valore alimentatore-guasto)(certezza (calcola-certezza 0.7 ?CF1 ?CF2))(nodo-padre ?id-p1 ?id-p2)))
)

(defrule errore-riavvio-5
  ?p1 <- (nodo(nome problema-principale)(valore errore-riavvio)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome problema-fase-boot)(valore si)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (assert (nodo (nome diagnosi) (valore scheda-madre-guasta)(certezza (calcola-certezza 0.2 ?CF1 ?CF2))(nodo-padre ?id-p1 ?id-p2)))
)

(defrule batteria-CMOS-esausta-1
  ?p1 <- (nodo(nome problema-principale)(valore ?v&messaggio-di-errore|errore-riavvio)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome inattivita-dispositivo)(valore si)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo(nome problema-fase-boot)(valore si)(certezza ?CF3)(attivo TRUE)(id-nodo ?id-p3))
  =>
  (assert (nodo (nome diagnosi)(valore batteria-CMOS-esausta)(certezza (calcola-certezza 0.5 ?CF1 ?CF2 ?CF3))(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule batteria-CMOS-esausta-2
  ?p1 <- (nodo(nome problema-principale)(valore ?v&messaggio-di-errore|errore-riavvio)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome anni-dispositivo)(valore 7-anni)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo(nome problema-fase-boot)(valore si)(certezza ?CF3)(attivo TRUE)(id-nodo ?id-p3))
  =>
  (assert (nodo (nome diagnosi)(valore batteria-CMOS-esausta)(certezza (calcola-certezza 0.8 ?CF1 ?CF2 ?CF3))(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)

(defrule batteria-CMOS-esausta-3
  ?p1 <- (nodo(nome problema-principale)(valore ?v&messaggio-di-errore|errore-riavvio)(certezza ?CF1)(attivo TRUE)(id-nodo ?id-p1))
  ?p2 <- (nodo(nome anni-dispositivo)(valore 4-6-anni)(certezza ?CF2)(attivo TRUE)(id-nodo ?id-p2))
  ?p3 <- (nodo(nome problema-fase-boot)(valore si)(certezza ?CF3)(attivo TRUE)(id-nodo ?id-p3))
  =>
  (assert (nodo (nome diagnosi)(valore batteria-CMOS-esausta)(certezza (calcola-certezza 0.5 ?CF1 ?CF2 ?CF3))(nodo-padre ?id-p1 ?id-p2 ?id-p3)))
)
