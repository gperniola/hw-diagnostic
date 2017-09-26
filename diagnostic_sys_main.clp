
(defglobal ?*highest-priority* = 1000)
(defglobal ?*high-priority* = 100)
(defglobal ?*low-priority* = -100)
(defglobal ?*lowest-priority* = -1000)

;(defmodule MAIN (export ?ALL))


;;****************
;;* DEFFUNCTIONS *
;;****************

; (deffunction ask-question (?question ?allowed-values)
;   (printout t ?question crlf "Risposte accettate: " ?allowed-values " : " )
;   (bind ?answer (read))
;   (if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))
;   (while (not (member ?answer ?allowed-values)) do
;       (printout t ?question)
;       (bind ?answer (read))
;       (if (lexemep ?answer) then (bind ?answer (lowcase ?answer))))
;   ?answer)


(deffunction ask-question (?question $?allowed-values)
  (printout t ?question crlf)
  (loop-for-count (?cnt1 1 (length ?allowed-values)) do
      (printout t ?cnt1 ". " (nth$ ?cnt1 ?allowed-values) crlf)
  )
  (bind ?answer (read))
  (if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))
  (while (not (member (nth$ ?answer ?allowed-values) ?allowed-values)) do
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) then (bind ?answer (lowcase ?answer))))
   ?answer)



  ; (deffunction yes-or-no-p (?question)
  ; (bind ?question (sym-cat ?question " (yes/y/no/n): "))
  ;    (bind ?response (ask-question ?question yes no y n))
  ;    (if (or (eq ?response yes) (eq ?response y))
  ;        then TRUE
  ;        else FALSE))


;;******************
;;* NODE TEMPLATES *
;;******************

  (deftemplate sintomo
    (slot nome    (type SYMBOL))
    (slot valore  (type SYMBOL))
    (slot livello (type SYMBOL))
  )

  (deftemplate diagnosi
    (slot nome        (type SYMBOL))
    (slot descrizione (type STRING))
  )

  (deftemplate info
    (slot nome    (type SYMBOL))
    (slot valore  (type SYMBOL))
  )

  (deftemplate domanda
    (slot attributo     (type SYMBOL) (default ?NONE))
    (slot testo-domanda (type STRING) (default ?NONE))
    (multislot risposte-valide (type SYMBOL) (default ?NONE))
    (multislot descrizione-risposte (type STRING) (default ?NONE))
    (slot gia-chiesta   (default  FALSE))
  )


;;******************
;;* CONTROL RULES  *
;;******************

(defrule inizializzazione
  (declare (salience ?*highest-priority*))
  =>
  (assert (info (nome tipo-dispositivo) (valore sconosciuto)))
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
  ; (if (yes-or-no-p "Would you like to revise the diagnosis?")
  ;     then (assert (revise-diagnosis))
  ;     else (halt)))


;;********************
;;* INFERENCE RULES  *
;;********************

(defrule bios-ok
  (sintomo (nome accensione) (valore ok))
  (sintomo (nome caricamento-SO) (valore ok))
  =>
  (assert (sintomo (nome caricamento-bios) (valore ok)))
  )



;;********************
;;* QUESTIONS RULES  *
;;********************

(defrule chiedi-tipo-dispositivo
  (declare (salience ?*low-priority*))
  ?d <- (info (nome tipo-dispositivo) (valore sconosciuto))
  ?f <- (domanda (attributo tipo-dispositivo) (testo-domanda ?domanda) (risposte-valide $?risposte) (descrizione-risposte $?descrizioni) (gia-chiesta FALSE))
  =>
  (bind ?risposta (ask-question ?domanda ?descrizioni))
  (assert (info (nome tipo-dispositivo) (valore (nth$ ?risposta ?risposte))))
  (retract ?d)
  (modify ?f (gia-chiesta TRUE))
)

(defrule chiedi-problema-pc
  (declare (salience ?*low-priority*))
  (info (nome tipo-dispositivo) (valore pc-fisso))
  ?f <- (domanda (attributo problemi-pc-fisso) (testo-domanda ?domanda) (risposte-valide $?risposte) (descrizione-risposte $?descrizioni) (gia-chiesta FALSE))
  =>
  (bind ?risposta (ask-question ?domanda ?descrizioni))
  (assert (info (nome problema-principale) (valore (nth$ ?risposta ?risposte))))
  (modify ?f (gia-chiesta TRUE))
)

(defrule chiedi-problema-portatile
  (declare (salience ?*low-priority*))
  (info (nome tipo-dispositivo) (valore pc-portatile))
  ?f <- (domanda (attributo problemi-pc-portatile) (testo-domanda ?domanda) (risposte-valide $?risposte) (descrizione-risposte $?descrizioni) (gia-chiesta FALSE))
  =>
  (bind ?risposta (ask-question ?domanda ?descrizioni))
  (assert (info (nome problema-principale) (valore (nth$ ?risposta ?risposte))))
  (modify ?f (gia-chiesta TRUE))
)

(defrule chiedi-problema-smartphone
  (declare (salience ?*low-priority*))
  (info (nome tipo-dispositivo) (valore smartphone))
  ?f <- (domanda (attributo problemi-smartphone) (testo-domanda ?domanda) (risposte-valide $?risposte) (descrizione-risposte $?descrizioni) (gia-chiesta FALSE))
  =>
  (bind ?risposta (ask-question ?domanda ?descrizioni))
  (assert (info (nome problema-principale) (valore (nth$ ?risposta ?risposte))))
  (modify ?f (gia-chiesta TRUE))
)

(defrule chiedi-problema-tablet
  (declare (salience ?*low-priority*))
  (info (nome tipo-dispositivo) (valore tablet))
  ?f <- (domanda (attributo problemi-tablet) (testo-domanda ?domanda) (risposte-valide $?risposte) (descrizione-risposte $?descrizioni) (gia-chiesta FALSE))
  =>
  (bind ?risposta (ask-question ?domanda ?descrizioni))
  (assert (info (nome problema-principale) (valore (nth$ ?risposta ?risposte))))
  (modify ?f (gia-chiesta TRUE))
)

;;********************
;;* QUESTIONS FACTS  *
;;********************

(deffacts domande
  (domanda  (attributo tipo-dispositivo)
            (testo-domanda "A quale tipologia appartiene il dispositivo?")
            (risposte-valide pc-fisso pc-portatile tablet smartphone)
            (descrizione-risposte "Pc fisso" "Pc portatile/Netbook" "Tablet" "Smartphone")
            )

  (domanda  (attributo problemi-pc-fisso)
            (testo-domanda "A quale categoria appartiene il problema principale da analizzare?")
            (risposte-valide accensione video rumori altro)
            (descrizione-risposte   "Accensione del dispositivo e caricamento del sistema operativo"
                                    "Problemi con il segnale video"
                                    "Rumori provenienti dall'interno del case"
                                    "Altro"
            ))

  (domanda  (attributo problemi-pc-portatile)
            (testo-domanda "A quale categoria appartiene il problema principale da analizzare?")
            (risposte-valide accensione video rumori surriscaldamento altro)
            (descrizione-risposte   "Accensione del dispositivo e caricamento del sistema operativo"
                                    "Problemi con il segnale video, display rotto o incrinato"
                                    "Rumori provenienti dall'interno del dispositivo"
                                    "Surriscaldamento eccessivo del dispositivo"
                                    "Altro"
            ))

  (domanda  (attributo problemi-smartphone)
            (testo-domanda "Seleziona l'opzione più appropriata per il problema da analizzare")
            (risposte-valide accensione video surriscaldamento altro)
            (descrizione-risposte   "Accensione del dispositivo e caricamento del sistema operativo"
                                    "Problemi con il segnale video, display rotto o incrinato"
                                    "Surriscaldamento eccessivo del dispositivo"
                                    "Altro"
            ))

  (domanda  (attributo problemi-tablet)
            (testo-domanda "Seleziona l'opzione più appropriata per il problema da analizzare")
            (risposte-valide accensione video surriscaldamento altro)
            (descrizione-risposte   "Accensione del dispositivo e caricamento del sistema operativo"
                                    "Problemi con il segnale video, display rotto o incrinato"
                                    "Surriscaldamento eccessivo del dispositivo"
                                    "Altro"
            ))

  (domanda  (attributo accensione)
            (testo-domanda "E' possibile accendere il dispositivo?")
            (risposte-valide ok fallito)
            (descrizione-risposte "Si" "No")
  )

  (domanda  (attributo caricamento-SO)
            (testo-domanda "Il sistema operativo viene caricato correttamente? L'utente riesce ad accedere alla schermata iniziale?")
            (risposte-valide ok fallito)
            (descrizione-risposte "Si" "No, il problema si verifica tra l'accensione del dispositivo e l'accesso alla schermata iniziale")
  )

)


;;********************
;;* DIAGNOSIS RULES  *
;;********************

(defrule diagnosi-pc
  (info (nome tipo-dispositivo) (valore pc-fisso))
  =>
  (assert (diagnosi (nome diagnosi-pc) (descrizione "Pc guasto.")))
)

(defrule diagnosi-tablet
  (info (nome tipo-dispositivo) (valore tablet))
  =>
  (assert (diagnosi (nome diagnosi-tablet) (descrizione "Tablet guasto.")))
)

(defrule diagnosi-smartphone
  (info (nome tipo-dispositivo) (valore smartphone))
  =>
  (assert (diagnosi (nome diagnosi-smartphone) (descrizione "Smartphone guasto.")))
)
