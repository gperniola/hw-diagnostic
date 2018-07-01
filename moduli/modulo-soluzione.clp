(defmodule MODULO-SOLUZIONE(import MAIN ?ALL)(export ?ALL))


(defrule MODULO-SOLUZIONE::guasto-hardware-si

  ?p1 <- (nodo (id-nodo ?id-p1) (attivo TRUE) (nome diagnosi) (valore ?v1&batteria-difettosa|alimentatore-guasto|scheda-madre-guasta|guasto-display|guasto-vga|guasto-inverter|guasto-RAM|guasto-CPU|errore-POST-hardware) (certezza ?c1))
      ;; assicura che p1 sia il nodo con certezza piu' alta
  (not (nodo (id-nodo ?id-p2) (attivo TRUE) (nome diagnosi) (valore ?v2&batteria-difettosa|alimentatore-guasto|scheda-madre-guasta|guasto-display|guasto-vga|guasto-inverter|guasto-RAM|guasto-CPU|errore-POST-hardware) (certezza ?c2&:(> ?c2 ?c1))))
      ;; se abbiamo due o piu' nodi con la stessa certezza massima, prendiamo sempre il nodo con id piu' alto per evitare di attivare la stessa regola piu' volte
  (not (nodo (id-nodo ?id-p3&:(> ?id-p3 ?id-p1))(attivo TRUE) (nome diagnosi) (valore ?v3&batteria-difettosa|alimentatore-guasto|scheda-madre-guasta|guasto-display|guasto-vga|guasto-inverter|guasto-RAM|guasto-CPU|errore-POST-hardware)(certezza ?c3&:(eq ?c3 ?c1))))
  =>
  (assert (nodo (nome guasto-hardware) (valore si) (certezza (calcola-certezza 1 ?c1)) (nodo-padre ?id-p1) (descrizione "Almeno una componente hardware del dispositivo e' guasta.")))
)

(defrule MODULO-SOLUZIONE::soluzione-dispositivo-in-assistenza

    ?p1 <- (nodo (nome guasto-hardware) (valore si) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
    ?p2 <- (nodo (nome garanzia) (valore si) (certezza ?c2) (attivo TRUE) (id-nodo ?id-p2))
    =>
    (assert (nodo (nome soluzione) (valore dispositivo-in-assistenza) (certezza (calcola-certezza 1 ?c1 ?c2)) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule MODULO-SOLUZIONE::soluzione-sostituzione-dispositivo-obsoleto

    ?p1 <- (nodo (nome guasto-hardware) (valore si) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
    ?p2 <- (nodo (nome anni-dispositivo) (valore 7-anni) (certezza ?c2) (attivo TRUE) (id-nodo ?id-p2))
    =>
    (assert (nodo (nome soluzione) (valore sostituzione-dispositivo) (certezza (calcola-certezza 0.9 ?c1 ?c2)) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule MODULO-SOLUZIONE::soluzione-sostituisci-batteria

    ?p1 <- (nodo (nome diagnosi) (valore batteria-difettosa) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
    =>
    (assert (nodo (nome soluzione) (valore sostituisci-batteria) (certezza (calcola-certezza 1 ?c1)) (nodo-padre ?id-p1)))
)

(defrule MODULO-SOLUZIONE::soluzione-sostituisci-alimentatore

    ?p1 <- (nodo (nome diagnosi) (valore alimentatore-guasto) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
    =>
    (assert (nodo (nome soluzione) (valore sostituisci-alimentatore) (certezza (calcola-certezza 1 ?c1)) (nodo-padre ?id-p1)))
)

(defrule MODULO-SOLUZIONE::soluzione-sostituisci-RAM

    ?p1 <- (nodo (nome diagnosi) (valore guasto-RAM) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
    =>
    (assert (nodo (nome soluzione) (valore sostituisci-RAM) (certezza (calcola-certezza 1 ?c1)) (nodo-padre ?id-p1)))
)

(defrule MODULO-SOLUZIONE::soluzione-riaggancia-RAM

    ?p1 <- (nodo (nome diagnosi) (valore guasto-RAM) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
    ?p2 <- (nodo(nome problema-principale)(valore codice-acustico)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
    =>
    (assert (nodo (nome soluzione) (valore riaggancia-RAM) (certezza (calcola-certezza 0.8 ?c1 ?c2)) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule MODULO-SOLUZIONE::soluzione-sostituisci-CPU

    ?p1 <- (nodo (nome diagnosi) (valore guasto-CPU) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
    =>
    (assert (nodo (nome soluzione) (valore sostituisci-CPU) (certezza (calcola-certezza 1 ?c1)) (nodo-padre ?id-p1)))
)

(defrule MODULO-SOLUZIONE::soluzione-riaggancia-CPU

    ?p1 <- (nodo (nome diagnosi) (valore guasto-CPU) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
    ?p2 <- (nodo(nome problema-principale)(valore codice-acustico)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
    =>
    (assert (nodo (nome soluzione) (valore riaggancia-CPU) (certezza (calcola-certezza 0.8 ?c1 ?c2)) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule MODULO-SOLUZIONE::soluzione-riaggancia-VGA

    ?p1 <- (nodo (nome diagnosi) (valore guasto-vga) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
    ?p2 <- (nodo(nome problema-principale)(valore codice-acustico)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
    =>
    (assert (nodo (nome soluzione) (valore riaggancia-VGA) (certezza (calcola-certezza 0.8 ?c1 ?c2)) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule MODULO-SOLUZIONE::soluzione-manutenzione-CPU

    ?p1 <- (nodo (nome diagnosi) (valore surriscaldamento-CPU) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
    =>
    (assert (nodo (nome soluzione) (valore manutenzione-CPU) (certezza (calcola-certezza 1 ?c1)) (nodo-padre ?id-p1)))
)

(defrule MODULO-SOLUZIONE::soluzione-sostituisci-scheda-madre

  ?p1 <- (nodo (nome diagnosi) (valore scheda-madre-guasta) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome tipo-dispositivo) (valore pc-desktop) (certezza ?c2) (attivo TRUE) (id-nodo ?id-p2))
  =>
  (assert (nodo (nome soluzione) (valore sostituisci-scheda-madre) (certezza (calcola-certezza 1 ?c1 ?c2)) (nodo-padre ?id-p1)))
)

(defrule MODULO-SOLUZIONE::soluzione-sostituisci-portatile

  ?p1 <- (nodo (nome diagnosi) (valore scheda-madre-guasta) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome tipo-dispositivo) (valore pc-portatile) (certezza ?c2) (attivo TRUE) (id-nodo ?id-p2))
  =>
  (assert (nodo (nome soluzione) (valore sostituzione-dispositivo) (certezza (calcola-certezza 1 ?c1 ?c2)) (nodo-padre ?id-p1)))
)

(defrule MODULO-SOLUZIONE::soluzione-accendi-alimentatore

  ?p1 <- (nodo (nome diagnosi) (valore alimentatore-spento) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome soluzione) (valore accendi-alimentatore) (certezza (calcola-certezza 1.0 ?c1)) (nodo-padre ?id-p1)))
)

(defrule MODULO-SOLUZIONE::soluzione-connetti-alimentazione

  ?p1 <- (nodo (nome diagnosi) (valore alimentazione-disconnessa) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome soluzione) (valore connetti-alimentazione) (certezza (calcola-certezza 1.0 ?c1)) (nodo-padre ?id-p1)))
)

(defrule MODULO-SOLUZIONE::soluzione-controllare-impianto-elettrico

  ?p1 <- (nodo (nome diagnosi) (valore corrente-assente) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome soluzione) (valore controllare-impianto-elettrico) (certezza (calcola-certezza 1.0 ?c1)) (nodo-padre ?id-p1)))
)

(defrule MODULO-SOLUZIONE::soluzione-sostituzione-cavo-alimentatore

  ?p1 <- (nodo (nome diagnosi) (valore cavo-alimentatore-pcdesktop-guasto) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome soluzione) (valore sostituzione-cavo-alimentatore) (certezza (calcola-certezza 1.0 ?c1)) (nodo-padre ?id-p1)))
)

(defrule MODULO-SOLUZIONE::soluzione-sostituzione-cavo-alimentatore-2

  ?p1 <- (nodo (nome diagnosi) (valore cavo-alimentatore-pcportatile-guasto) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome soluzione) (valore sostituzione-cavo-alimentatore) (certezza (calcola-certezza 1.0 ?c1)) (nodo-padre ?id-p1)))
)

(defrule MODULO-SOLUZIONE::soluzione-accendi-monitor

  ?p1 <- (nodo (nome diagnosi) (valore monitor-spento) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome soluzione) (valore accendi-monitor) (certezza (calcola-certezza 1 ?c1)) (nodo-padre ?id-p1)))
)

(defrule MODULO-SOLUZIONE::soluzione-connetti-cavi-video

  ?p1 <- (nodo (nome diagnosi) (valore cavi-monitor-scollegati) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome soluzione) (valore connetti-cavi-video) (certezza (calcola-certezza 1.0 ?c1)) (nodo-padre ?id-p1)))
)

(defrule MODULO-SOLUZIONE::soluzione-sostituisci-display-1

  ?p1 <- (nodo (nome diagnosi) (valore guasto-display) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome tipo-dispositivo) (valore pc-portatile) (certezza ?c2) (attivo TRUE) (id-nodo ?id-p2))
  =>
  (assert (nodo (nome soluzione) (valore sostituisci-display-portatile) (certezza (calcola-certezza 1.0 ?c1 ?c2)) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule MODULO-SOLUZIONE::soluzione-sostituisci-display-2

  ?p1 <- (nodo (nome diagnosi) (valore guasto-display) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome tipo-dispositivo) (valore pc-desktop) (certezza ?c2) (attivo TRUE) (id-nodo ?id-p2))
  =>
  (assert (nodo (nome soluzione) (valore sostituisci-display-desktop) (certezza (calcola-certezza 1.0 ?c1 ?c2)) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule MODULO-SOLUZIONE::soluzione-sostituisci-vga

  ?p1 <- (nodo (nome diagnosi) (valore guasto-vga) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome soluzione) (valore sostituisci-vga) (certezza (calcola-certezza 1.0 ?c1)) (nodo-padre ?id-p1)))
)

(defrule MODULO-SOLUZIONE::soluzione-controllo-cavi-video-portatile

  ?p1 <- (nodo (nome diagnosi) (valore interferenze-cavo) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome cavi-display-accessibili)(valore no)(certezza ?c2)(attivo TRUE)(id-nodo ?id-p2))
  =>
  (assert (nodo (nome soluzione) (valore controllo-cavi-video-portatile) (certezza (calcola-certezza 0.9 ?c1 ?c2)) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule MODULO-SOLUZIONE::soluzione-sostituisci-inverter

  ?p1 <- (nodo (nome diagnosi) (valore guasto-inverter) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome soluzione) (valore sostituisci-inverter) (certezza (calcola-certezza 1 ?c1)) (nodo-padre ?id-p1)))
)

(defrule MODULO-SOLUZIONE::soluzione-problema-SW-1

  ?p1 <- (nodo (nome diagnosi) (valore problema-SW-video) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome problema-principale)(valore fasce-schermo) (certezza ?c2) (attivo TRUE) (id-nodo ?id-p2))
  =>
  (assert (nodo (nome soluzione) (valore aggiorna-driver-video) (certezza (calcola-certezza 0.9 ?c1 ?c2)) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule MODULO-SOLUZIONE::soluzione-problema-SW-2

  ?p1 <- (nodo (nome diagnosi) (valore problema-SW-video) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome problema-principale)(valore schermo-nero) (certezza ?c2) (attivo TRUE) (id-nodo ?id-p2))
  =>
  (assert (nodo (nome soluzione) (valore aggiorna-driver-video) (certezza (calcola-certezza 0.8 ?c1 ?c2)) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule MODULO-SOLUZIONE::soluzione-problema-SW-3

  ?p1 <- (nodo (nome diagnosi) (valore problema-SW-video) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome problema-principale)(valore schermo-nero) (certezza ?c2) (attivo TRUE) (id-nodo ?id-p2))
  =>
  (assert (nodo (nome soluzione) (valore controllo-integrita-files) (certezza (calcola-certezza 0.9 ?c1 ?c2)) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule MODULO-SOLUZIONE::soluzione-problema-SW-4

  ?p1 <- (nodo (nome diagnosi) (valore problema-SW-video) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome problema-principale)(valore schermo-nero) (certezza ?c2) (attivo TRUE) (id-nodo ?id-p2))
  =>
  (assert (nodo (nome soluzione) (valore ripara-file-SO) (certezza (calcola-certezza 0.9 ?c1 ?c2)) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule MODULO-SOLUZIONE::soluzione-resetta-CMOS

  ?p1 <- (nodo (nome diagnosi) (valore CMOS-corrotta) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome soluzione) (valore resetta-CMOS) (certezza (calcola-certezza 1.0 ?c1)) (nodo-padre ?id-p1)))
)

(defrule MODULO-SOLUZIONE::soluzione-sostituisci-batteria-CMOS

  ?p1 <- (nodo (nome diagnosi) (valore batteria-CMOS-esausta) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome soluzione) (valore sostituisci-batteria-CMOS) (certezza (calcola-certezza 1.0 ?c1)) (nodo-padre ?id-p1)))
)

(defrule MODULO-SOLUZIONE::soluzione-controlla-scheda-POST

  ?p1 <- (nodo (nome diagnosi) (valore errore-POST-hardware) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome problema-principale)(valore errore-riavvio) (certezza ?c2) (attivo TRUE) (id-nodo ?id-p2))
  =>
  (assert (nodo (nome soluzione) (valore controlla-scheda-POST) (certezza (calcola-certezza 1 ?c1 ?c2)) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule MODULO-SOLUZIONE::soluzione-controlla-beep-code

  ?p1 <- (nodo (nome diagnosi) (valore errore-POST-hardware) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome problema-principale)(valore codice-acustico) (certezza ?c2) (attivo TRUE) (id-nodo ?id-p2))
  =>
  (assert (nodo (nome soluzione) (valore controlla-beep-code) (certezza (calcola-certezza 1 ?c1 ?c2)) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule MODULO-SOLUZIONE::soluzione-controlla-messaggio-POST

  ?p1 <- (nodo (nome diagnosi) (valore errore-POST-hardware) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome problema-principale)(valore messaggio-di-errore) (certezza ?c2) (attivo TRUE) (id-nodo ?id-p2))
  =>
  (assert (nodo (nome soluzione) (valore controlla-messaggio-post) (certezza (calcola-certezza 1 ?c1 ?c2)) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule MODULO-SOLUZIONE::soluzione-rimuovi-memorie-esterne

  ?p1 <- (nodo (nome diagnosi) (valore errore-POST-boot) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome soluzione) (valore rimuovi-memorie-esterne) (certezza (calcola-certezza 1.0 ?c1)) (nodo-padre ?id-p1)))
)

(defrule MODULO-SOLUZIONE::soluzione-scansione-antivirus

  ?p1 <- (nodo (nome diagnosi) (valore infetto-da-virus) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome soluzione) (valore scansione-antivirus) (certezza (calcola-certezza 1.0 ?c1)) (nodo-padre ?id-p1)))
)

(defrule MODULO-SOLUZIONE::soluzione-problema-caricamento-SO-BSOD-1

  ?p1 <- (nodo (nome diagnosi) (valore problema-caricamento-SO) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome problema-principale)(valore blue-screen) (certezza ?c2) (attivo TRUE) (id-nodo ?id-p2))
  =>
  (assert (nodo (nome soluzione) (valore ripara-file-SO) (certezza (calcola-certezza 0.8 ?c1 ?c2)) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule MODULO-SOLUZIONE::soluzione-problema-caricamento-SO-BSOD-2

  ?p1 <- (nodo (nome diagnosi) (valore problema-caricamento-SO) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome problema-principale)(valore blue-screen) (certezza ?c2) (attivo TRUE) (id-nodo ?id-p2))
  =>
  (assert (nodo (nome soluzione) (valore controlla-stop-error) (certezza (calcola-certezza 1.0 ?c1 ?c2)) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule MODULO-SOLUZIONE::soluzione-problema-caricamento-SO-BSOD-3

  ?p1 <- (nodo (nome diagnosi) (valore problema-caricamento-SO) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  ?p2 <- (nodo (nome problema-principale)(valore blue-screen) (certezza ?c2) (attivo TRUE) (id-nodo ?id-p2))
  =>
  (assert (nodo (nome soluzione) (valore ripristina-configurazione-sistema) (certezza (calcola-certezza 0.8 ?c1)) (nodo-padre ?id-p1 ?id-p2)))
)

(defrule MODULO-SOLUZIONE::soluzione-conflitto-HW-1

  ?p1 <- (nodo (nome diagnosi) (valore conflitto-HW) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome soluzione) (valore ripristina-configurazione-sistema) (certezza (calcola-certezza 0.7 ?c1)) (nodo-padre ?id-p1)))
)

(defrule MODULO-SOLUZIONE::soluzione-conflitto-HW-2

  ?p1 <- (nodo (nome diagnosi) (valore conflitto-HW) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome soluzione) (valore rimuovi-HW-installato) (certezza (calcola-certezza 1.0 ?c1)) (nodo-padre ?id-p1)))
)

(defrule MODULO-SOLUZIONE::soluzione-conflitto-SW-1

  ?p1 <- (nodo (nome diagnosi) (valore conflitto-SW) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome soluzione) (valore ripristina-configurazione-sistema) (certezza (calcola-certezza 0.8 ?c1)) (nodo-padre ?id-p1)))
)

(defrule MODULO-SOLUZIONE::soluzione-conflitto-SW-2

  ?p1 <- (nodo (nome diagnosi) (valore conflitto-SW) (certezza ?c1) (attivo TRUE) (id-nodo ?id-p1))
  =>
  (assert (nodo (nome soluzione) (valore rimuovi-SW-installato) (certezza (calcola-certezza 0.9 ?c1)) (nodo-padre ?id-p1)))
)
