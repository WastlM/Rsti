################################################################################
### Detektivspiel mit verschlüsselten Sätzen
###
### Copyright (C) 2018 Sebastian Meyer <seb.meyer@fau.de>
###
### This file is part of the R package "Rsti",
### free software under the terms of the GNU General Public License, version 2,
### or (at your option) any later version, a copy of which is available at
### https://www.R-project.org/Licenses/.
################################################################################

##' @include spiel_Tiere.R
BOTSCHAFTEN <- c(
    paste(c("Papa", "Mami", "Moritz", "Jonathan"), "=", sample(TIERE))
)

##' Detektivspiel mit verschlüsselten Sätzen
##'
##' In diesem Spiel sollen verschlüsselte Sätze entziffert werden. Die
##' Buchstaben eines Satzes sind jeweils durch Zahlen ersetzt, entsprechend des
##' Index im Alphabet.
##' 
##' @param name Der Name des Spielers.
##' @importFrom stats setNames
##' @export

spiel_Detektiv <- function (name)
{
    cat("DETEKTIV", "--------\n", sep = "\n")
    cat("Ich habe geheime Nachrichten empfangen.",
        "\nVielleicht kann Detektiv", name,
        "sie für mich entschlüsseln?\n")

    CODE <- setNames(seq_along(LETTERS), LETTERS)
    
    cat("Dazu brauchst du diesen geheimen Code:\n\n")
    print_code(CODE)
    cat("Zum Beispiel:", CODE["A"], "bedeutet A und",
        CODE["E"], "bedeutet E und so weiter.\n\n")
    
    ntask <- ask09("Wieviele Botschaften möchtest du entschlüsseln?")
    cat("\nAlso", ntask, "geheime Botschaften. Auf geht's!\n")
    replicate(ntask, {
        cat("Hier siehst du nochmal den Code:\n")
        print_code(CODE); cat("\n")
        detektivaufgabe(name, code=CODE)
        cat("\n")
    })
    cat("Jetzt hast du die", ntask, "Botschaften geschafft. Prima!\n\n")
    return(invisible())
}

detektivaufgabe <- function (name, code)
{
    botschaft <- toupper(sample(BOTSCHAFTEN, 1))
    zeichen <- strsplit(botschaft, "")[[1]]
    encrypted <- ifelse(zeichen %in% names(code), code[zeichen], zeichen)
    ask(cat("Geheime Botschaft:\n\n", encrypted, "\n\nWas bedeutet das?"),
        expected = botschaft, name = name, ignore = c(" ", "="))
    return(invisible())
}

print_code <- function (code, columns = 13, print.gap = 2, ...)
{
    l <- length(code)
    nl <- ceiling(l/columns)
    hline <- paste0(rep("-", columns*(2+print.gap)), collapse = "")
    cat(hline, "\n")
    for(i in seq_len(nl)) {
        idx <- (i-1)*columns + seq_len(columns)
        print(code[idx[idx <= l]], print.gap = print.gap, ...)
        if (i < nl) cat("\n")
    }
    cat(hline, "\n")
}
