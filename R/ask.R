################################################################################
### Offene und geschlossene Fragen stellen und ggf. Antwort prüfen
###
### Copyright (C) 2018 Sebastian Meyer <seb.meyer@fau.de>
###
### This file is part of the R package "Rsti",
### free software under the terms of the GNU General Public License, version 2,
### or (at your option) any later version, a copy of which is available at
### https://www.R-project.org/Licenses/.
################################################################################

SURE <- c(
    "Bist du dir sicher?",
    "Das kann doch nicht sein, oder?",
    "Wirklich?",
    "Hoppla! Das überrascht mich.",
    "Ja gibt es denn sowas? Das glaube ich nicht.",
    "Nein, nein, nein, das kann nicht sein."
)

PRAISE <- c(
    "Sehr gut", "Toll gemacht", "Prima", "Ja, weiter so", "Spitze",
    "Das hast du gut gemacht"
)

praise <- function (name = NULL)
{
    cat(sample(PRAISE, 1),
        if (!is.null(name)) paste(",", name),
        "!\n", sep = "")
}

ask <- function (question, expected = NULL, name = NULL,
                 max_trials = 3, ignore = NULL)
{
    cat(question, "\n--> ")
    answer <- readLines(n = 1)
    if (is.null(expected))
        return(answer)

    simplify <- function (x) {
        x <- tolower(x)
        if (!is.null(ignore)) {
            for (i in ignore)
                x <- gsub(i, "", x, fixed = TRUE)
        }
        x
    }
        
    trials <- 1
    while(!identical(simplify(answer), simplify(expected))) {
        if (trials == max_trials) {
            cat("OK, ich verrate dir, was ich erwartet habe: ",
                expected, "!\n")
            return(expected)
        }
        cat(sample(SURE, 1),
            "Ich habe etwas anderes erwartet. Hast du dich verschrieben?",
            "Versuche es bitte nochmal.", sep = "\n")
        cat("--> ")
        answer <- readLines(n = 1)
        trials <- trials + 1
    }
    praise(name)
    return(expected)
}

ask_options <- function (question, options = c("Ja", "Nein"))
{
    qo <- paste0(question, "\n",
                paste0(options, collapse = " oder "), "?")
    while (is.na(sel <- match(trimws(tolower(ask(qo))),
                              trimws(tolower(options))))) {
        cat("Das habe ich nicht verstanden. Hast du dich verschrieben?\n")
        cat("Ich frage dich am besten nochmal:\n")
    }
    if (missing(options)) sel == 1 else options[sel]
}

ask09 <- function (question)
{
    res <- ask(paste(question, "\nSchreibe eine Zahl zwischen 0 und 9."))
    res <- suppressWarnings(as.integer(res))
    if (is.na(res) || res < 0) {
        res <- sample(0:9, 1)
        cat("Diese Zahl kenne ich nicht. Nehmen wir einfach mal",
            res, ".\n")
    } else {
        if (res > 9) {
            cat("Mehr als neun war aber nicht erlaubt. Nehmen wir 9.\n")
            res <- 9
        }
    }
    res
}
