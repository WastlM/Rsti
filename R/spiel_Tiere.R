################################################################################
### Fehlende Buchstaben in Tiernamen ergänzen
###
### Copyright (C) 2018 Sebastian Meyer <seb.meyer@fau.de>
###
### This file is part of the R package "Rsti",
### free software under the terms of the GNU General Public License, version 2,
### or (at your option) any later version, a copy of which is available at
### https://www.R-project.org/Licenses/.
################################################################################

TIERE <- c(
    "Bär", "Ameise", "Esel", "Wal", "Kamel", "Wolf", "Tiger", "Löwe",
    "Nashorn", "Nilpferd", "Pferd", "Spinne", "Elefant", "Giraffe",
    "Fuchs", "Gorilla", "Eisbär", "Schnecke", "Maus", "Dinosaurier",
    "Schmetterling", "Papagei", "Eule", "Adler", "Hund", "Regenwurm",
    "Katze", "Hase", "Zebra", "Schlange", "Kojote"
)


##' Fehlende Buchstaben ergänzen
##'
##' In diesem Spiel sollen fehlende Buchstaben in Wörtern ergänzt werden.
##' Derzeit sind alle Wörter Tierarten.
##' 
##' @param name Der Name des Spielers.
##' @export

spiel_Tiere <- function (name)
{
    cat("TIERE", "------\n", sep = "\n")
    cat("Ich kenne viele Tiere!",
        "Aber mir sind Buchstaben verloren gegangen.",
        "Kannst du mir helfen?", sep = "\n")

    ntask <- ask09("Wieviele Tiernamen möchtest du korrigieren?")
    cat("\nAlso", ntask, "Tiere. Auf geht's!\n",
        "Sag mir bitte immer welche Buchstaben fehlen.\n")
    replicate(ntask, {
        tieraufgabe(name)
        cat("\n")
    })
    cat("Jetzt hast du die", ntask, "Tiere geschafft. Prima!\n\n")
    return(invisible())
}

tieraufgabe <- function (name)
{
    tier <- sample(TIERE, 1)
    buchstaben <- strsplit(tier, "")[[1]]
    fehlt <- sample(2:length(buchstaben), 1)
    task <- buchstaben
    task[fehlt] <- "_"
    ask(paste0(task, collapse=""),
        expected = buchstaben[fehlt], name = name)
    return(invisible())
}
