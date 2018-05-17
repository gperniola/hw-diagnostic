(defmodule MODULO-SOLUZIONE(import MAIN ?ALL)(export ?ALL))

;; FASE 4 SOLUZIONI ********************

(defrule MODULO-SOLUZIONE::guasto-hardware-si
  ;(fase-cerca-soluzioni)
  ?p1 <- (nodo (id-nodo ?id-p1) (attivo TRUE) (nome diagnosi) (valore ?v1&batteria-difettosa|alimentatore-guasto|scheda-madre-guasta|guasto-display|guasto-vga|guasto-inverter|guasto-RAM|guasto-CPU|errore-POST-hardware) (certezza ?c1))
      ;; assicura che p1 sia il nodo con certezza piu' alta
  (not (nodo (id-nodo ?id-p2) (attivo TRUE) (nome diagnosi) (valore ?v2&batteria-difettosa|alimentatore-guasto|scheda-madre-guasta|guasto-display|guasto-vga|guasto-inverter|guasto-RAM|guasto-CPU|errore-POST-hardware) (certezza ?c2&:(> ?c2 ?c1))))
      ;; se abbiamo due o piu' nodi con la stessa certezza massima, prendiamo sempre il nodo con id piu' alto per evitare di attivare la stessa regola piu' volte
  (not (nodo (id-nodo ?id-p3&:(> ?id-p3 ?id-p1))(attivo TRUE) (nome diagnosi) (valore ?v3&batteria-difettosa|alimentatore-guasto|scheda-madre-guasta|guasto-display|guasto-vga|guasto-inverter|guasto-RAM|guasto-CPU|errore-POST-hardware)(certezza ?c3&:(eq ?c3 ?c1))))
  =>
  (assert (nodo (nome guasto-hardware) (valore si) (certezza (* 1 ?c1)) (nodo-padre ?id-p1) (descrizione "Almeno una componente hardware del dispositivo e' guasta.")))
)

(defrule MODULO-SOLUZIONE::soluzione-dispositivo-in-assistenza
    ;(fase-cerca-soluzioni)
    ?p1 <- (nodo (nome guasto-hardware) (valore si) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
    ?p2 <- (nodo (nome garanzia) (valore si) (certezza ?c2) (attivo TRUE) (id-nodo ?id-p2))
    =>
    (bind ?CF (calcola-certezza 1 ?c1 ?c2))
    (assert (nodo (nome soluzione) (valore dispositivo-in-assistenza) (certezza ?CF) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule MODULO-SOLUZIONE::soluzione-sostituzione-dispositivo-obsoleto
    ;(fase-cerca-soluzioni)
    ?p1 <- (nodo (nome guasto-hardware) (valore si) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
    ?p2 <- (nodo (nome anni-dispositivo) (valore 7-anni) (certezza ?c2) (attivo TRUE) (id-nodo ?id-p2))
    =>
    (bind ?CF (calcola-certezza 0.9 ?c1 ?c2))
    (assert (nodo (nome soluzione) (valore sostituzione-dispositivo) (certezza ?CF) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule MODULO-SOLUZIONE::soluzione-sostituisci-batteria
    ;(fase-cerca-soluzioni)
    ?p1 <- (nodo (nome diagnosi) (valore batteria-difettosa) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
    =>
    (assert (nodo (nome soluzione) (valore sostituisci-batteria) (certezza (* 1 ?c1)) (nodo-padre ?id-p1)))
)

(defrule MODULO-SOLUZIONE::soluzione-sostituisci-alimentatore
    ;(fase-cerca-soluzioni)
    ?p1 <- (nodo (nome diagnosi) (valore alimentatore-guasto) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
    =>
    (assert (nodo (nome soluzione) (valore sostituisci-alimentatore) (certezza (* 1 ?c1)) (nodo-padre ?id-p1)))
)

(defrule MODULO-SOLUZIONE::soluzione-sostituisci-RAM
    ;(fase-cerca-soluzioni)
    ?p1 <- (nodo (nome diagnosi) (valore guasto-RAM) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
    =>
    (assert (nodo (nome soluzione) (valore sostituisci-RAM) (certezza (* 1 ?c1)) (nodo-padre ?id-p1)))
)

(defrule MODULO-SOLUZIONE::soluzione-riaggancia-RAM
    ;(fase-cerca-soluzioni)
    ?p1 <- (nodo (nome diagnosi) (valore guasto-RAM) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
    ?p2 <- (nodo(nome fase-POST)(valore errore-beep-code)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
    =>
    (assert (nodo (nome soluzione) (valore riaggancia-RAM) (certezza (calcola-certezza 0.8 ?c1 ?c2)) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule MODULO-SOLUZIONE::soluzione-sostituisci-CPU
    ;(fase-cerca-soluzioni)
    ?p1 <- (nodo (nome diagnosi) (valore guasto-CPU) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
    =>
    (assert (nodo (nome soluzione) (valore sostituisci-CPU) (certezza (* 1 ?c1)) (nodo-padre ?id-p1)))
)

(defrule MODULO-SOLUZIONE::soluzione-riaggancia-CPU
    ;(fase-cerca-soluzioni)
    ?p1 <- (nodo (nome diagnosi) (valore guasto-CPU) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
    ?p2 <- (nodo(nome fase-POST)(valore errore-beep-code)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
    =>
    (assert (nodo (nome soluzione) (valore riaggancia-CPU) (certezza (calcola-certezza 0.8 ?c1 ?c2)) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule MODULO-SOLUZIONE::soluzione-riaggancia-VGA
    ;(fase-cerca-soluzioni)
    ?p1 <- (nodo (nome diagnosi) (valore guasto-vga) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
    ?p2 <- (nodo(nome fase-POST)(valore errore-beep-code)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
    =>
    (assert (nodo (nome soluzione) (valore riaggancia-VGA) (certezza (calcola-certezza 0.8 ?c1 ?c2)) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule MODULO-SOLUZIONE::soluzione-manutenzione-CPU
    ;(fase-cerca-soluzioni)
    ?p1 <- (nodo (nome diagnosi) (valore surriscaldamento-CPU) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
    =>
    (assert (nodo (nome soluzione) (valore manutenzione-CPU) (certezza (* 1 ?c1)) (nodo-padre ?id-p1)))
)

(defrule MODULO-SOLUZIONE::soluzione-sostituisci-scheda-madre
  ;(fase-cerca-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore scheda-madre-guasta) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome tipo-dispositivo) (valore pc-desktop) (certezza ?c2) (attivo TRUE) (id-nodo ?id-p2))
  =>
  (bind ?CF (calcola-certezza 1 ?c1 ?c2))
  (assert (nodo (nome soluzione) (valore sostituisci-scheda-madre) (certezza ?CF) (nodo-padre ?id-p1)))
)

(defrule MODULO-SOLUZIONE::soluzione-sostituisci-portatile
  ;(fase-cerca-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore scheda-madre-guasta) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome tipo-dispositivo) (valore pc-portatile) (certezza ?c2) (attivo TRUE) (id-nodo ?id-p2))
  =>
  (bind ?CF (calcola-certezza 1 ?c1 ?c2))
  (assert (nodo (nome soluzione) (valore sostituzione-dispositivo) (certezza ?CF) (nodo-padre ?id-p1)))
)

(defrule MODULO-SOLUZIONE::soluzione-accendi-alimentatore
  ;(fase-cerca-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore alimentatore-spento) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome soluzione) (valore accendi-alimentatore) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1)))
)

(defrule MODULO-SOLUZIONE::soluzione-connetti-alimentazione
  ;(fase-cerca-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore alimentazione-disconnessa) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome soluzione) (valore connetti-alimentazione) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1)))
)

;;SOLUZIONI VIDEO

(defrule MODULO-SOLUZIONE::soluzione-connetti-cavi-video
  ;(fase-cerca-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore cavo-alimentazione-display-scollegato) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome soluzione) (valore connetti-cavi-video) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1)))
)

(defrule MODULO-SOLUZIONE::soluzione-sostituisci-display-1
  ;(fase-cerca-soluzioni)
  ;;******MODIFIED
  ?p1 <- (nodo (nome diagnosi) (valore guasto-display) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome tipo-dispositivo) (valore pc-portatile) (certezza ?c2) (attivo TRUE) (id-nodo ?id-p2))
  =>
  (assert (nodo (nome soluzione) (valore sostituisci-display-portatile) (certezza (calcola-certezza 1.0 ?c1 ?c2)) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule MODULO-SOLUZIONE::soluzione-sostituisci-display-2
  ;(fase-cerca-soluzioni)
    ;;******ADDED
  ?p1 <- (nodo (nome diagnosi) (valore guasto-display) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome tipo-dispositivo) (valore pc-desktop) (certezza ?c2) (attivo TRUE) (id-nodo ?id-p2))
  =>
  (assert (nodo (nome soluzione) (valore sostituisci-display-desktop) (certezza (calcola-certezza 1.0 ?c1 ?c2)) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule MODULO-SOLUZIONE::soluzione-sostituisci-vga
  ;(fase-cerca-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore guasto-vga) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome soluzione) (valore sostituisci-vga) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1)))
)

(defrule MODULO-SOLUZIONE::soluzione-controllo-cavi-video
  ;(fase-cerca-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore interferenze-cavo) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome cavi-display-accessibili)(valore si)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (bind ?CF (calcola-certezza 0.9 ?c1 ?c2))
  (assert (nodo (nome soluzione) (valore controllo-cavi-video) (certezza ?CF) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule MODULO-SOLUZIONE::soluzione-controllo-cavi-video-portatile
  ;(fase-cerca-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore interferenze-cavo) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome cavi-display-accessibili)(valore no)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (bind ?CF (calcola-certezza 0.8 ?c1 ?c2))
  (assert (nodo (nome soluzione) (valore controllo-cavi-video-portatile) (certezza ?CF) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule MODULO-SOLUZIONE::soluzione-sostituisci-inverter
  ;(fase-cerca-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore guasto-inverter) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome soluzione) (valore sostituisci-inverter) (certezza (* 1 ?c1)) (nodo-padre ?id-p1)))
)

(defrule MODULO-SOLUZIONE::soluzione-problema-SW-1
  ;(fase-cerca-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore problema-SW-video) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome disturbo-video)(valore ?v&linee-orizzontali|fasce-verticali) (certezza ?c2) (attivo TRUE) (id-nodo ?id-p2))
  =>
  (bind ?CF (calcola-certezza 0.9 ?c1 ?c2))
  (assert (nodo (nome soluzione) (valore aggiorna-driver-video) (certezza ?CF) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule MODULO-SOLUZIONE::soluzione-problema-SW-2
  ;(fase-cerca-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore problema-SW-video) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome disturbo-video)(valore schermo-nero) (certezza ?c2) (attivo TRUE) (id-nodo ?id-p2))
  =>
  (bind ?CF-driver (calcola-certezza 0.8 ?c1 ?c2))
  (bind ?CF-files  (calcola-certezza 0.9 ?c1 ?c2))
  (bind ?CF-SO     (calcola-certezza 0.9 ?c1 ?c2))
  (assert (nodo (nome soluzione) (valore aggiorna-driver-video) (certezza ?CF-driver) (nodo-padre ?id-p1 ?id-p2)))
  (assert (nodo (nome soluzione) (valore controllo-integrita-files) (certezza ?CF-files) (nodo-padre ?id-p1 ?id-p2)))
  (assert (nodo (nome soluzione) (valore ripara-file-SO) (certezza ?CF-SO) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule MODULO-SOLUZIONE::soluzione-resetta-CMOS
  ;(fase-cerca-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore CMOS-corrotta) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome soluzione) (valore resetta-CMOS) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1)))
)


;; SOLUZIONI BOOT

(defrule MODULO-SOLUZIONE::soluzione-sostituisci-batteria-CMOS
  ;(fase-cerca-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore batteria-CMOS-esausta) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome soluzione) (valore sostituisci-batteria-CMOS) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1)))
)

(defrule MODULO-SOLUZIONE::soluzione-controlla-scheda-POST
  ;(fase-cerca-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore errore-POST-hardware) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome fase-POST)(valore errore-riavvio) (certezza ?c2) (attivo TRUE) (id-nodo ?id-p2))
  =>
  (bind ?CF (calcola-certezza 1 ?c1 ?c2))
  (assert (nodo (nome soluzione) (valore controlla-scheda-POST) (certezza ?CF) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule MODULO-SOLUZIONE::soluzione-controlla-beep-code
  ;(fase-cerca-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore errore-POST-hardware) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome fase-POST)(valore errore-beep-code) (certezza ?c2) (attivo TRUE) (id-nodo ?id-p2))
  =>
  (bind ?CF (calcola-certezza 1 ?c1 ?c2))
  (assert (nodo (nome soluzione) (valore controlla-beep-code) (certezza ?CF) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule MODULO-SOLUZIONE::soluzione-controlla-messaggio-POST
  ;(fase-cerca-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore errore-POST-hardware) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome fase-POST)(valore errore-messaggio) (certezza ?c2) (attivo TRUE) (id-nodo ?id-p2))
  =>
  (bind ?CF (calcola-certezza 1 ?c1 ?c2))
  (assert (nodo (nome soluzione) (valore controlla-messaggio-post) (certezza ?CF) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule MODULO-SOLUZIONE::soluzione-rimuovi-memorie-esterne
  ;(fase-cerca-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore errore-POST-boot) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome soluzione) (valore rimuovi-memorie-esterne) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1)))
)


(defrule MODULO-SOLUZIONE::soluzione-scansione-antivirus
  ;(fase-cerca-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore infetto-da-virus) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome soluzione) (valore scansione-antivirus) (certezza (* 1.0 ?c1)) (nodo-padre ?id-p1)))
)

(defrule MODULO-SOLUZIONE::soluzione-problema-caricamento-SO-BSOD
  ;(fase-cerca-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore problema-caricamento-SO) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome fase-POST)(valore errore-BSOD) (certezza ?c2) (attivo TRUE) (id-nodo ?id-p2))
  =>
  (bind ?CF-ripara-file (calcola-certezza 0.8 ?c1 ?c2))
  (bind ?CF-stop-error (calcola-certezza 1.0 ?c1 ?c2))
  (bind ?CF-ripristina-SO (calcola-certezza 0.8 ?c1))
  (assert (nodo (nome soluzione) (valore ripara-file-SO) (certezza ?CF-ripara-file) (nodo-padre ?id-p1 ?id-p2)))
  (assert (nodo (nome soluzione) (valore controlla-stop-error) (certezza ?CF-stop-error) (nodo-padre ?id-p1 ?id-p2)))
  (assert (nodo (nome soluzione) (valore ripristina-configurazione-sistema) (certezza ?CF-ripristina-SO) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule MODULO-SOLUZIONE::soluzione-conflitto-HW
  ;(fase-cerca-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore conflitto-HW) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (bind ?CF-ripristina-SO (calcola-certezza 0.7 ?c1))
  (bind ?CF-rimuovi-HW (calcola-certezza 1.0 ?c1))
  (assert (nodo (nome soluzione) (valore ripristina-configurazione-sistema) (certezza ?CF-ripristina-SO) (nodo-padre ?id-p1)))
  (assert (nodo (nome soluzione) (valore rimuovi-HW-installato) (certezza ?CF-rimuovi-HW) (nodo-padre ?id-p1)))
)

(defrule MODULO-SOLUZIONE::soluzione-conflitto-SW
  ;(fase-cerca-soluzioni)
  ?p1 <- (nodo (nome diagnosi) (valore conflitto-SW) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (bind ?CF-ripristina-SO (calcola-certezza 0.8 ?c1))
  (bind ?CF-rimuovi-SW (calcola-certezza 0.9 ?c1))
  (assert (nodo (nome soluzione) (valore ripristina-configurazione-sistema) (certezza ?CF-ripristina-SO) (nodo-padre ?id-p1)))
  (assert (nodo (nome soluzione) (valore rimuovi-SW-installato) (certezza ?CF-rimuovi-SW) (nodo-padre ?id-p1)))
)
