
(defglobal ?*highest-priority* = 1000)
(defglobal ?*high-priority* = 100)
(defglobal ?*low-priority* = -100)
(defglobal ?*lowest-priority* = -1000)


;;****************
;;* DEFFUNCTIONS *
;;****************


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

(defrule revisiona-domande
  (declare (salience ?*highest-priority*))
  (revisiona-domande)
  ?d <- (domanda (testo-domanda ?testo) (attributo ?attr) (num-domanda ?n&:(> ?n 0)))
  (nodo (nome ?attr) (valore ?val) (descrizione ?descr))
  =>
  (printout t "Domanda " ?n ": " ?testo crlf "Risposta: " ?descr crlf crlf)
)


;;********************
;;*    ASK RULES     *
;;********************
(defrule chiedi-domanda
  (declare (salience ?*low-priority*))
  ?ask <- (chiedi ?attr)
  ?f <- (domanda (attributo ?attr) (testo-domanda ?domanda) (risposte-valide $?risposte) (descrizione-risposte $?descrizioni) (gia-chiesta FALSE))
  (not (nodo (nome ?attr)))
  ?cont-dom <- (contatore-domande ?i)
  =>
  (bind ?j (+ ?i 1))
  (bind ?risposta (ask-question ?j ?domanda ?descrizioni))
  (if (= ?risposta 0) then
    (printout t crlf "***** REVISIONE DOMANDE *****" crlf)
    (assert (revisiona-domande))
  else
    (assert (nodo (nome ?attr) (valore (nth$ ?risposta ?risposte)) (descrizione (nth$ ?risposta ?descrizioni)) (tipo info-utente)))
    (modify ?f (gia-chiesta TRUE)(num-domanda ?j))
    (retract ?ask)
    (retract ?cont-dom)
    (assert (contatore-domande ?j))
  )
)


(defrule chiedi-tipo-dispositivo
  =>
  (assert (chiedi tipo-dispositivo))
)

(defrule chiedi-accensione
  =>
  (assert (chiedi stato-accensione))
)

(defrule chiedi-video
  =>
  (assert (chiedi stato-video))
)

(defrule chiedi-problema-boot-o-SO
  (nodo (nome stato-accensione) (valore ok))
  (nodo (nome sistema-operativo) (valore windows))
  =>
  (assert (chiedi problema-boot-o-SO))
)

(defrule chiedi-riavvio-forzato
  (nodo (nome stato-accensione) (valore ok))
  =>
  (assert (chiedi riavvio-forzato))
)

(defrule chiedi-bluescreen
  (nodo (nome problema-boot-o-SO) (valore SO))
  (nodo (nome sistema-operativo) (valore windows))
  =>
  (assert (chiedi bluescreen))
)

(defrule chiedi-installazione-nuovo-hw
  (nodo (nome tipo-dispositivo) (valore ?val&pc-fisso|pc-portatile))
  =>
  (assert(chiedi installazione-nuovo-hw))
)

(defrule chiedi-segnali-bios
  (nodo (nome problema-boot-o-SO) (valore boot))
  =>
  (assert (chiedi segnali-bios))
)

(defrule chiedi-display-rotto
  (nodo (nome stato-video) (valore fallito))
  =>
  (assert (chiedi display-rotto))
)

(defrule chiedi-disturbo-video
  (nodo (nome display-rotto) (valore no))
  =>
  (assert (chiedi disturbo-video))
)



;;********************
;;*     DIAGNOSI     *
;;********************

(defrule diagnosi-parziale-connettori-video
  (nodo (nome disturbo-video) (valore si))
  =>
  (assert (nodo (nome diagnosi-parziale) (valore problema-connettori-video)))
)

(defrule diagnosi-parziale-problema-pixels
  (nodo (nome disturbo-video) (valore si))
  =>
  (assert (nodo (nome diagnosi-parziale) (valore problema-pixels)))
)

(defrule diagnosi-parziale-sostituzione-display
  (or
    (nodo (nome display-rotto) (valore si))
    (nodo (nome diagnosi-parziale) (valore problema-pixels))
  )
  =>
  (assert (nodo (nome diagnosi-parziale) (valore sostituzione-display)))
)

(defrule diagnosi-parziale-stop-error
  (nodo (nome bluescreen) (valore si))
  =>
  (assert (nodo (nome diagnosi-parziale) (valore stop-error)))
)

(defrule diagnosi-parziale-conflitto-hw
  (nodo (nome diagnosi-parziale) (valore stop-error))
  (nodo (nome installazione-nuovo-hw) (valore si))
  =>
  (assert (nodo (nome diagnosi-parziale) (valore conflitto-hw)))
)

(defrule diagnosi-parziale-alimentazione
  (or
    (and
      (nodo (nome bluescreen) (valore no))
      (nodo (nome riavvio-forzato) (valore si))
    )
    (and
      (nodo (nome segnali-bios) (valore no))
      (nodo (nome riavvio-forzato) (valore si))
    )
    (nodo (nome stato-accensione) (valore fallito))
  )
  =>
  (assert (nodo(nome diagnosi-parziale) (valore problema-alimentazione)))
)

(defrule diagnosi-parziale-scheda-madre
  (or
      (and
        (nodo (nome bluescreen) (valore no))
        (nodo (nome riavvio-forzato) (valore si))
      )
      (and
        (nodo (nome segnali-bios) (valore no))
        (nodo (nome riavvio-forzato) (valore si))
      )
      (nodo (nome stato-accensione) (valore fallito))
  )
  =>
  (assert (nodo(nome diagnosi-parziale) (valore problema-scheda-madre)))
)

(defrule diagnosi-parziale-bios
  (nodo (nome segnali-bios) (valore si))
  =>
  (assert (nodo (nome diagnosi-parziale) (valore problema-bios)))
)

(defrule diagnosi-parziale-ram
  (or
    (nodo (nome segnali-bios) (valore si))
    (nodo (nome diagnosi-parziale) (valore stop-error))
  )
  =>
  (assert (nodo (nome diagnosi-parziale) (valore problema-ram)))
)

(defrule diagnosi-parziale-hard-disk
  (or
    (nodo (nome segnali-bios) (valore si))
    (nodo (nome diagnosi-parziale) (valore stop-error))
  )
  =>
  (assert (nodo (nome diagnosi-parziale) (valore problema-hard-disk)))
)




(defrule deduci-SO-windows
  (nodo (nome tipo-dispositivo) (valore ?dispositivo&pc-fisso|pc-portatile))
  =>
  (assert (nodo (nome sistema-operativo) (valore windows) (tipo inferenza)))
)

(defrule deduci-SO-android
  (nodo (nome tipo-dispositivo) (valore ?dispositivo&tablet|smarthpone))
  =>
  (assert (nodo (nome SO) (valore android) (tipo inferenza)))
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
