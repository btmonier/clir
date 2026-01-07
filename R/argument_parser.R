#' @import S7
#' @keywords internal
"_PACKAGE"

## ----
#' ArgumentParser class for parsing command-line arguments
#'
#' @description
#' Main class for creating and managing command-line argument parsers,
#' similar to Python's argparse module.
#'
#' @export
ArgumentParser <- S7::new_class(
    name = "ArgumentParser",
    package = "clir",
    properties = list(
        prog            = S7::class_character,
        description     = S7::class_character,
        epilog          = S7::class_character,
        arguments       = S7::class_list,
        formatter_class = S7::class_character
    ),
    constructor = function(
        prog            = NULL,
        description     = character(),
        epilog          = character(),
        formatter_class = "help"
    ) {
        # Get program name from command line if not provided
        if (is.null(prog)) {
            prog <- basename(commandArgs()[1])
            if (prog == "R" || prog == "Rscript") {
                prog <- "Rscript"
            }
        }

        parser_obj <- S7::new_object(
            S7::S7_object(),
            prog            = prog,
            description     = description,
            epilog          = epilog,
            arguments       = list(),
            formatter_class = formatter_class
        )

        # Add default --help and -h arguments
        help_arg <- Argument(
            name   = "--help",
            dest   = "help",
            help   = "show this help message and exit",
            action = "store_true"
        )
        h_arg <- Argument(
            name   = "-h",
            dest   = "help",
            help   = "show this help message and exit",
            action = "store_true"
        )
        parser_obj@arguments <- list(help_arg, h_arg)

        return(parser_obj)
    }
)

## ----
#' Add an argument to the parser
#'
#' @param parser An ArgumentParser object
#' @param ... Arguments passed to Argument constructor
#' @param name Argument name (e.g., "--foo" or "-f")
#' @param dest Destination variable name
#' @param help Help text for the argument
#' @param default Default value
#' @param type Type of the argument (character, integer, numeric, logical)
#' @param action Action to take (store, store_true, store_false, count)
#' @param nargs Number of arguments expected (? for optional, + for one or more, * for zero or more)
#' @param required Whether the argument is required
#' @param choices Valid choices for the argument
#' @param metavar Metavariable name for help text
#'
#' @return The ArgumentParser object (for chaining)
#' @export
add_argument <- function(
    parser,
    ...,
    name,
    dest     = NULL,
    help     = character(),
    default  = NULL,
    type     = "character",
    action   = "store",
    nargs    = "?",
    required = FALSE,
    choices  = NULL,
    metavar  = NULL
) {
    if (!S7::S7_inherits(parser, ArgumentParser)) {
        stop("parser must be an ArgumentParser object", call. = FALSE)
    }

    # Set dest from name if not provided
    if (is.null(dest)) {
        dest <- gsub("^-+", "", name)
        dest <- gsub("-", "_", dest)
    }

    # Create new argument
    arg <- Argument(
        name     = name,
        dest     = dest,
        help     = help,
        default  = default,
        type     = type,
        action   = action,
        nargs    = nargs,
        required = required,
        choices  = choices,
        metavar  = metavar
    )

    # Add to parser's arguments list
    parser@arguments <- append(parser@arguments, list(arg))

    return(invisible(parser))
}
