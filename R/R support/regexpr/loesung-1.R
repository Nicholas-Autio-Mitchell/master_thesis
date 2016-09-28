## Bsp 1: siehe Folien

## ACHTUNG: R_LIBS ist eine Umgebungsvariable, d.h., muß im
## Betriebssystem (Windows, Linux, ...) gesetzt werden, nicht in R
## selber.

###**********************************************************

## Bsp 2: siehe Bsp aus den Folien, Loesung haengt vom eingegebenen
## Datensatz ab

###**********************************************************

## Bsp 3:

x <- c("Anna Huber", "Franz Mayer", "Hans Rieger")
sub("(.*) + (.*)", "\\2, \\1", x)


## Gleicher Code loest sogar etwas komplexere Aufgabe (solange
## Nachname keine Leerzeichen enthaelt):

x <- c("Anna Huber", "Franz Mayer-Doppler", "Hans Josef Rieger")
sub("(.*) +(.*)", "\\2, \\1", x)

###**********************************************************
## Bsp 4:
###**********************************************************

Umlaute = c("Ä", "Ö", "Ü", "ä", "ö", "ü", "ß")

Transskription = c("Ae", "Oe", "Ue", "ae", "oe", "ue", "sz")

x <- c("Anna Hübner", "Hans Jörg Wußter")


for(k in 1:length(Umlaute))
    x <- gsub(Umlaute[k], Transskription[k], x)
x

###**********************************************************
## Bsp 5: siehe Folien
###**********************************************************

## Bsp 6:

x <- readLines("tcpdump-output2.txt")

## ein paar zeilen enthalten fehlermeldungen -> rauswerfen
notok <- grep("unknown version", x)
x <- x[-notok]



### extrahiere sender fields und loesche protokoll
sender <- sub(".* ([^ ]*) > .*", "\\1", x)
sender <- sub("(.*)\\.[^.]*$", "\\1", sender)

### extrahiere empfaenger fields und loesche protokoll
recp <- sub(".* > ([^ ]*).*", "\\1", x)
recp <- sub("(.*)\\.[^.]*:$", "\\1", recp)


### CRAN ist immer entweder sender oder empfaenger -> rauswerfen
sender <- sender[-grep("cran.wu-wien", sender)]
recp <- recp[-grep("cran.wu-wien", recp)]

### Wer sendet bzw. empfaengt wieviele Pakete? Jeweils nur Top 10.
send10 <- sort(table(sender), decreasing=TRUE)[1:10]
recp10 <- sort(table(recp), decreasing=TRUE)[1:10]

all10 <- sort(table(c(sender, recp)), decreasing=TRUE)[1:10]

send10
recp10
all10

###**********************************************************

## Bsp 7:

## Extrahiere alle Verbindungen mit Domain "ser.de"
ser.de <- grep("ser\\.de", x, value=TRUE)

## extrahiere zeit
z <- sub("^(..:..:..).*", "\\1", ser.de)

ztab <- table(z)
ztab

## einfach, aber falsch (nicht in allen Sekunden kamen Pakete zustande):
par(mfrow=c(2,1), las=2)
plot(ztab)

## fuer korrekte Darstellung der Zeit kann man z.b. die funktion
## times() aus dem Paket chron verwenden
library("chron")
par(las=1)
plot(times(names(ztab)), ztab, type="h", lwd=2)

## ANALOG FUER ANDERE DOMAINS
