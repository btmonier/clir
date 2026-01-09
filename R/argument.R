## ----
#' Argument class for defining command-line arguments
#'
#' @description
#' Represents a single command-line argument with its properties.
#' This is an internal class used by ArgumentParser.
#'
#' @keywords internal
Argument <- S7::new_class(
    name = "Argument",
    package = "clir",
    properties = list(
        name       = S7::class_character,
        short_name = S7::class_character,
        long_name  = S7::class_character,
        dest       = S7::class_character,
        help       = S7::class_character,
        default    = S7::class_any,
        type       = S7::class_character,
        action     = S7::class_character,
        nargs      = S7::class_character,
        required   = S7::class_logical,
        choices    = S7::class_any,
        metavar    = S7::class_character
    ),
    constructor = function(
        name       = character(),
        short_name = character(),
        long_name  = character(),
        dest       = character(),
        help       = character(),
        default    = NULL,
        type       = "character",
        action     = "store",
        nargs      = "?",
        required   = FALSE,
        choices    = NULL,
        metavar    = NULL
    ) {
        # Set dest to name if not provided
        if (length(dest) == 0) {
            dest <- name
        }

        # Set metavar to uppercase name if not provided
        if (is.null(metavar)) {
            metavar <- toupper(name)
        }

        S7::new_object(
            S7::S7_object(),
            name       = name,
            short_name = short_name,
            long_name  = long_name,
            dest       = dest,
            help       = help,
            default    = default,
            type       = type,
            action     = action,
            nargs      = nargs,
            required   = required,
            choices    = choices,
            metavar    = metavar
        )
    }
)
