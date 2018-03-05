################################################################################
### Rsti: Textbasierte Lernspiele Für Erstklässler
###
### Copyright (C) 2018 Sebastian Meyer <seb.meyer@fau.de>
###
### This file is part of the R package "Rsti",
### free software under the terms of the GNU General Public License, version 2,
### or (at your option) any later version, a copy of which is available at
### https://www.R-project.org/Licenses/.
################################################################################

##' Hauptfunktion zum Starten des Lernspiels
##'
##' Wie der Name schon sagt, startet diese Funktion das Lernspiel. Es ist rein
##' textbasiert, d.h. es läuft in der Konsole ab und erwartet immer wieder
##' Tastatureingaben, die mit der Eingabetaste abzuschließen sind.
##' 
##' @param hausis An welchen Wochentagen soll gefragt werden, ob die
##'     Hausaufgaben schon erledigt wurden?
##' @importFrom tools toTitleCase
##' @export

los <- function (hausis = 7)
{
    NAME <- toTitleCase(tolower(ask("Wie ist dein Name?")))
    hallo(NAME)
    cat("Ich bin ein Computerprogramm.",
        "Sebastian Meyer hat mich programmiert.", sep = "\n")
    ## SPITZ <- toTitleCase(tolower(ask("Und wie ist dein Spitzname?")))
    ## if (ask_options("Darf ich dich mit deinem Spitznamen anreden?"))
    ##     NAME <- SPITZ
    
    cat(NAME, ", ich habe ein paar Aufgaben für dich!\n", sep = "")
    spielen <- ask_options("Magst du spielen?")
    if (!spielen) {
        tschuess(name=NAME)
        return(invisible())
    }

    cat("Sehr schön! Ich auch!\n")
    if (strftime(Sys.Date(),"%u") %in% hausis) {
        cat("\nABER ... ")
        Sys.sleep(5)
        hausis <- ask_options("Hast du denn deine Hausaufgaben schon gemacht?")
        if (hausis) hausis <- ask_options("Wirklich?")
        if (!hausis) {
            cat("Oh. Dann schlage ich vor, wir spielen später.",
                "Bis bald!\n", sep = "\n")
            return(invisible())
        }
    }

    cat("Dann legen wir mal los!",
        "Ich bin schon gespannt wie das wird!\n", sep = "\n")

    spiele <- sub("^spiel_", "",
                  ls(pattern="^spiel_", envir = getNamespace("Rsti")))

    weiter <- TRUE
    while(weiter) {
        spiel <- ask_options(
            "Welches Spiel möchtest du als nächstes spielen?",
            options = spiele)
        do.call(paste0("spiel_", spiel), list(name=NAME))
        cat("--- Das", paste0(spiel,"-Spiel"),
            "ist jetzt zu Ende ---\n\n")
        weiter <- ask_options("Magst du noch etwas anderes spielen?")
    }
    
    tschuess(name=NAME)
    cat("\n--- PROGRAMM ENDE ---\n")
    return(invisible())
}

hallo <- function (name) {
    stunde <- as.integer(strftime(Sys.time(), "%H"))
    hallo <- if (stunde < 10) {
        "Guten Morgen"
    } else if (stunde > 17) {
        "Guten Abend"
    } else "Hallo"
    cat(hallo, " ", name, "!\n", sep = "")
}

tschuess <- function (name) {
    cat("Du magst nicht mehr spielen? Schade.",
        "\nNaja, vielleicht ein anderes Mal wieder. :-)",
        "\nTschüss ", name, "!\n", sep = "")
}
