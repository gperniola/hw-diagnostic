
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


(deffunction ask-question (?j ?question $?allowed-values)
  (printout t "DOMANDA N. " ?j ":" crlf ?question crlf)
  (loop-for-count (?cnt1 1 (length ?allowed-values)) do
      (printout t ?cnt1 ". " (nth$ ?cnt1 ?allowed-values) crlf)
  )
  (printout t "0. Revisiona domande precedenti." crlf)
  (bind ?answer (read))
  (if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))
  (while (and (not (member (nth$ ?answer ?allowed-values) ?allowed-values)) (not (= ?answer 0))) do
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) then (bind ?answer (lowcase ?answer))))
   ?answer)


(deffunction ask-stop-program ()
  (printout t "Continuare l'esecuzione del programma?" crlf "1. Si" crlf "2. No" crlf)
  (bind ?answer (read))
  (while (not (member ?answer (create$ 1 2)))
      (printout t "Continuare l'esecuzione del programma?" crlf "1. Si" crlf "2. No" crlf)
      (bind ?answer (read))
  )
  (if (eq ?answer 2) then
      (assert (ferma-programma))
  )
)


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
  (nodo (nome diagnosi-parziale) (valore ?val))
  =>
  (printout t crlf ">>>> DIAGNOSI PARZIALE TROVATA: " ?val crlf)
  (ask-stop-program)
)


(defrule ferma-esecuzione
  (declare (salience ?*highest-priority*))
  (ferma-programma)
  =>
  (halt)
)

(defrule revisiona-domande-stampa-elenco
  (declare (salience ?*highest-priority*))
  (revisiona-domande)
  ?d <- (domanda (testo-domanda ?testo) (attributo ?attr) (num-domanda ?n) (gia-chiesta TRUE) (stampata FALSE))
  (not (domanda (num-domanda ?m&:(< ?m ?n))  (gia-chiesta TRUE)(stampata FALSE)))
  (nodo (nome ?attr) (valore ?val) (descrizione ?descr))
  =>
  (modify ?d (stampata TRUE))
  (printout t "Domanda " ?n ": " ?testo crlf "Risposta: " ?descr crlf crlf)
)

(defrule revisiona-domande
  (declare (salience ?*highest-priority*))
  ?r <-(revisiona-domande)
  (not (domanda (gia-chiesta TRUE) (stampata FALSE)))
  (contatore-domande ?n)
  =>
  (printout t crlf crlf)
  (bind ?risposta (ask-question-revision ?n))
  (printout t crlf "**************************************" crlf crlf crlf)
  (retract ?r)
  (assert (annulla-stampa-domande))
  (if (<> ?risposta 0) then
    (assert (revisiona-da ?risposta))
  )
)

(defrule annulla-stampa-domande
  (declare (salience ?*highest-priority*))
  (annulla-stampa-domande)
  ?d <- (domanda (stampata TRUE))
  =>
  (modify ?d (stampata FALSE))
)

(defrule annulla-stampa-domande-fine
  (declare (salience ?*highest-priority*))
  ?a <- (annulla-stampa-domande)
  (not (domanda (stampata TRUE)))
  =>
  (retract ?a)
)

(defrule resetta-domande
  (declare (salience ?*highest-priority*))
  (revisiona-da ?dom)
  ?d <- (domanda (attributo ?attr) (num-domanda ?n&:(> ?n ?dom)) (gia-chiesta TRUE))
  =>
  (modify ?d (num-domanda 0) (gia-chiesta FALSE))
)

(defrule revisiona-da
  (declare (salience ?*highest-priority*))
  (revisiona-da ?dom)
  ?d <- (domanda (attributo ?attr) (num-domanda ?dom) (gia-chiesta TRUE))
  ?nodo-partenza <- (nodo (nome chiedi) (valore ?attr) (nodo-padre $?padri))
  ?cont <- (contatore-domande ?c)
  =>
  (assert (elimina-nodi-da ?nodo-partenza))
  ;;(retract ?nodo-partenza)
  ;;(assert (nodo (nome chiedi) (valore ?attr) (nodo-padre ?padri)))
  (modify ?d (num-domanda 0) (gia-chiesta FALSE))
  (retract ?cont)
  (assert (contatore-domande (- ?dom 1)))
)

(defrule elimina-nodi-da
  (declare (salience ?*highest-priority*))
  ?p1 <- (elimina-nodi-da ?n)
  ?p2 <- (nodo (nodo-padre $?x ?n $?y))
  =>
  (assert (elimina-nodi-da ?p2))
  (retract ?p2)
)

(defrule ferma-elimina-nodi-da
  ?p1 <- (elimina-nodi-da ?n)
  (not (nodo (nodo-padre $?x ?n $?y)))
  =>
  (retract ?p1)
)

(defrule fine-revisiona-domande
  (declare (salience ?*highest-priority*))
  ?r <- (revisiona-da ?dom)
  (not (elimina-nodi da ?e))
  (not (domanda (attributo ?attr) (num-domanda ?n&:(>= ?n ?dom)) (gia-chiesta TRUE)))
  =>
  (retract ?r)
)







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
    (printout t crlf "***** REVISIONE DOMANDE *****" crlf)
    (assert (revisiona-domande))
  else
    (assert (nodo (nome ?attr) (valore (nth$ ?risposta ?risposte)) (descrizione (nth$ ?risposta ?descrizioni)) (tipo info-utente) (nodo-padre ?ask)))
    (modify ?f (gia-chiesta TRUE)(num-domanda ?j))
    ;;(retract ?ask)
    (retract ?cont-dom)
    (assert (contatore-domande ?j))
  )
)

;;********************
;;*    ASK RULES     *
;;********************

(defrule chiedi-tipo-dispositivo
  =>
  (assert (nodo (nome chiedi) (valore tipo-dispositivo)))
)

(defrule chiedi-accensione
  =>
  (assert (nodo (nome chiedi) (valore stato-accensione)))
)

(defrule chiedi-video
  =>
  (assert (nodo (nome chiedi) (valore stato-video)))
)

(defrule chiedi-problema-boot-o-SO
  ?p1 <- (nodo (nome stato-accensione) (valore ok))
  ?p2 <- (nodo (nome sistema-operativo) (valore windows))
  =>
  (assert (nodo (nome chiedi) (valore problema-boot-o-SO) (nodo-padre ?p1 ?p2)))
)

(defrule chiedi-riavvio-forzato
  ?p1 <- (nodo (nome stato-accensione) (valore ok))
  =>
  (assert (nodo (nome chiedi) (valore riavvio-forzato) (nodo-padre ?p1)))
)

(defrule chiedi-bluescreen
  ?p1 <- (nodo (nome problema-boot-o-SO) (valore SO))
  ?p2 <- (nodo (nome sistema-operativo) (valore windows))
  =>
  (assert (nodo (nome chiedi) (valore bluescreen) (nodo-padre ?p1 ?p2)))
)

(defrule chiedi-installazione-nuovo-hw
  ?p1 <- (nodo (nome tipo-dispositivo) (valore ?val&pc-fisso|pc-portatile))
  =>
  (assert (nodo (nome chiedi) (valore installazione-nuovo-hw) (nodo-padre ?p1)))
)

(defrule chiedi-segnali-bios
  ?p1 <- (nodo (nome problema-boot-o-SO) (valore boot))
  =>
  (assert (nodo (nome chiedi) (valore segnali-bios) (nodo-padre ?p1)))
)

(defrule chiedi-display-rotto
  ?p1 <- (nodo (nome stato-video) (valore fallito))
  =>
  (assert (nodo (nome chiedi) (valore display-rotto) (nodo-padre ?p1)))
)

(defrule chiedi-disturbo-video
  ?p1 <- (nodo (nome display-rotto) (valore no))
  =>
  (assert (nodo (nome chiedi) (valore disturbo-video) (nodo-padre ?p1)))
)



;;********************
;;*     DIAGNOSI     *
;;********************

(defrule diagnosi-parziale-connettori-video
  ?p1 <- (nodo (nome disturbo-video) (valore si))
  =>
  (assert (nodo (nome diagnosi-parziale) (valore problema-connettori-video) (nodo-padre ?p1)))
)

(defrule diagnosi-parziale-problema-pixels
  ?p1 <- (nodo (nome disturbo-video) (valore si))
  =>
  (assert (nodo (nome diagnosi-parziale) (valore problema-pixels) (nodo-padre ?p1)))
)

(defrule diagnosi-parziale-sostituzione-display
  (or
    ?p1 <- (nodo (nome display-rotto) (valore si))
    ?p1 <- (nodo (nome diagnosi-parziale) (valore problema-pixels))
  )
  =>
  (assert (nodo (nome diagnosi-parziale) (valore sostituzione-display) (nodo-padre ?p1)))
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
  (assert (nodo (nome diagnosi-parziale) (valore conflitto-hw) (nodo-padre ?p1 ?p2)))
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
  (assert (nodo(nome diagnosi-parziale) (valore problema-alimentazione) (nodo-padre ?p1 ?p2)))
)

(defrule diagnosi-parziale-alimentazione-2
  ?p1 <- (nodo (nome stato-accensione) (valore fallito))
  =>
  (assert (nodo(nome diagnosi-parziale) (valore problema-alimentazione) (nodo-padre ?p1)))
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
  (assert (nodo(nome diagnosi-parziale) (valore problema-scheda-madre) (nodo-padre ?p1 ?p2)))
)

(defrule diagnosi-parziale-scheda-madre-2
  ?p1 <- (nodo (nome stato-accensione) (valore fallito))
  =>
  (assert (nodo(nome diagnosi-parziale) (valore problema-scheda-madre) (nodo-padre ?p1)))
)

(defrule diagnosi-parziale-bios
  ?p1 <- (nodo (nome segnali-bios) (valore si))
  =>
  (assert (nodo (nome diagnosi-parziale) (valore problema-bios) (nodo-padre ?p1)))
)

(defrule diagnosi-parziale-ram
  (or
    ?p1 <- (nodo (nome segnali-bios) (valore si))
    ?p1 <- (nodo (nome diagnosi-parziale) (valore stop-error))
  )
  =>
  (assert (nodo (nome diagnosi-parziale) (valore problema-ram) (nodo-padre ?p1)))
)

(defrule diagnosi-parziale-hard-disk
  (or
    ?p1 <- (nodo (nome segnali-bios) (valore si))
    ?p1 <- (nodo (nome diagnosi-parziale) (valore stop-error))
  )
  =>
  (assert (nodo (nome diagnosi-parziale) (valore problema-hard-disk) (nodo-padre ?p1)))
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
(deffacts domande


  (domanda  (attributo tipo-dispositivo)
            (testo-domanda "A quale tipologia appartiene il dispositivo?")
            (risposte-valide pc-fisso pc-portatile tablet smartphone)
            (descrizione-risposte "Pc fisso Windows" "Pc portatile/Netbook Windows" "Tablet Android" "Smartphone Android")
  )

  (domanda  (attributo stato-accensione)
            (testo-domanda "E' possibile accendere il dispositivo?")
            (risposte-valide ok fallito)
            (descrizione-risposte "Si" "No")
  )

  (domanda  (attributo problema-boot-o-SO)
            (testo-domanda "Il problema si verifica nella fase di boot o dopo aver caricato il sistema operativo?")
            (risposte-valide boot SO nessuno)
            (descrizione-risposte "Nella fase di boot" "Dopo il caricamento del sistema operativo" "Il sistema funziona correttamente")
  )

  (domanda  (attributo riavvio-forzato)
            (testo-domanda "Il dispositivo si riavvia forzatamente più volte?")
            (risposte-valide si no)
            (descrizione-risposte "Si" "No")
  )

  (domanda  (attributo bluescreen)
            (testo-domanda "Il sistema termina con un errore bluescreen?")
            (risposte-valide si no non-so)
            (descrizione-risposte "Si" "No" "Non so")
  )

  (domanda  (attributo installazione-nuovo-hw)
            (testo-domanda "E' stato installato del nuovo hardware subito prima che il problema si verificasse?")
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
