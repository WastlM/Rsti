################################################################################
### Rechenaufgaben mit Plus und Minus
###
### Copyright (C) 2018-2019 Sebastian Meyer <seb.meyer@fau.de>
###
### This file is part of the R package "Rsti",
### free software under the terms of the GNU General Public License, version 2,
### or (at your option) any later version, a copy of which is available at
### https://www.R-project.org/Licenses/.
################################################################################

##' Rechenaufgaben mit Plus und Minus
##'
##' In diesem Spiel wird eine selbst zu wählende Anzahl von Rechenaufgaben
##' gestellt. Diese enthalten jeweils einzelne oder verkettete Additionen oder
##' Subtraktionen der natürlichen Zahlen von 0 bis 9. Der mögliche
##' Ergebnisbereich wird vorher festgelegt (standardmäßig von 0 bis 20).
##' 
##' @param name Der Name des Spielers.
##' @param minimum,maximum Erlaubter Ergebnisbereich.
##' @param zahlengrenze Größte Zahl mit der gerechnet werden soll.
##' @export

spiel_Rechnen <- function (name, minimum = 0, maximum = 20, zahlengrenze = 9)
{
    cat("RECHNEN", "-------\n", sep = "\n")
    cat("Ich habe mir ein paar Rechenaufgaben überlegt.",
        "Mal schauen ob du die lösen kannst! :-)", sep = "\n")

    ntask <- ask09("Wieviele Aufgaben möchtest du haben?")
    cat("\nAlso", ntask, "Aufgaben. Auf geht's!\n")
    replicate(ntask, {
        rechenaufgabe(name, minimum, maximum, zahlengrenze)
        cat("\n")
    })
    cat("Jetzt hast du die", ntask, "Aufgaben geschafft. Prima!\n\n")
    return(invisible())
}

rechenaufgabe <- function (name, minimum = 0, maximum = 20, zahlengrenze = 9)
{
    result <- -Inf
    while(result < minimum || result > maximum) {
        signs <- sample(list("+", "-", c("+", "-")), 1)[[1]]
        numbers <- sample(0:zahlengrenze, length(signs)+1, replace = TRUE)
        task <- paste(numbers, c(signs, ""), collapse = " ")
        result <- eval(parse(text = task))
    }
    ask(paste0(task, "= ?"), expected = result, name = name)
    return(invisible())
}
