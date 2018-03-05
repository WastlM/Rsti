################################################################################
### Fragen zu Datum und Uhrzeit
###
### Copyright (C) 2018 Sebastian Meyer <seb.meyer@fau.de>
###
### This file is part of the R package "Rsti",
### free software under the terms of the GNU General Public License, version 2,
### or (at your option) any later version, a copy of which is available at
### https://www.R-project.org/Licenses/.
################################################################################

##' Fragen zu Datum und Uhrzeit
##'
##' Welcher Wochentag ist heute und dann übermorgen? Welchen Monat haben wir und
##' welchen Tag im Monat? Wieviel Uhr ist es?
##' 
##' @param name Der Name des Spielers.
##' @export

spiel_Datum <- function (name)
{
    cat("DATUM", "-----\n", sep = "\n")
    cat("Ich kann meinen Kalender leider nicht finden.\n")

    ask("Weißt du welcher Wochentag heute ist?",
        expected = weekdays(Sys.Date()), name = name)

    ask("Und welcher Wochentag ist dann übermorgen?",
        expected = weekdays(Sys.Date()+2), name = name)

    ask("Und welchen Monat haben wir?",
        expected = months(Sys.Date()), name = name)

    ask(paste("Weißt du auch den wievielten Tag im",
              months(Sys.Date()), "wir heute haben?"),
        expected = as.integer(strftime(Sys.Date(), "%d")), name = name)

    ask(paste("Meine Uhr ist kaputt. :-(",
              "Es werden leider nur die Minuten angezeigt.",
              paste0("Schau, ", name, ", bei mir zeigt die Uhr nur:"),
              paste0("??:", strftime(Sys.time(), "%M")),
              "Welche Stunde haben wir denn (siehe Backofen)?",
              sep = "\n"),
        expected = as.integer(strftime(Sys.time(), "%H")), name = name)
    
    return(invisible())
}
