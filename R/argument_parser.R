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
#' @param prog Program name (defaults to script name from command line)
#' @param description Description text shown at the top of help output
#' @param epilog Text shown at the bottom of help output
#' @param formatter_class Formatter class for help output (default: "help")
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

        # Add default --help and -h argument (combined)
        # Create basic argument then set short/long names
        help_arg <- Argument(
            name   = "--help",
            dest   = "help",
            help   = "show this help message and exit",
            action = "store_true"
        )
        help_arg@short_name <- "-h"
        help_arg@long_name <- "--help"
        parser_obj@arguments <- list(help_arg)

        return(parser_obj)
    }
)

## ----
#' Add an argument to the parser
#'
#' @param parser An ArgumentParser object
#' @param ... Arguments passed to Argument constructor
#' @param name Argument name(s). Can be a character vector with both short (e.g., "-f")
#'   and long (e.g., "--foo") forms. At most one short form and one long form allowed.
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

    # Parse name(s) into short and long forms
    parsed <- parse_argument_names(name)
    short_name <- parsed$short_name
    long_name <- parsed$long_name

    # Primary name is long form if available, otherwise short form
    primary_name <- if (length(long_name) > 0) long_name else short_name

    # Set dest from primary name if not provided
    if (is.null(dest)) {
        dest <- gsub("^-+", "", primary_name)
        dest <- gsub("-", "_", dest)
    }

    # Create new argument with base properties, then set short/long names
    arg <- Argument(
        name     = primary_name,
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
    # Set short and long name properties directly
    arg@short_name <- short_name
    arg@long_name <- long_name

    # Add to parser's arguments list
    parser@arguments <- append(parser@arguments, list(arg))

    return(invisible(parser))
}

## ----
#' Parse argument names into short and long forms
#'
#' @param names Character vector of argument names
#' @return List with short_name and long_name
#' @keywords internal
parse_argument_names <- function(names) {
    short_names <- character()
    long_names <- character()

    for (n in names) {
        if (startsWith(n, "--")) {
            long_names <- c(long_names, n)
        } else if (startsWith(n, "-")) {
            short_names <- c(short_names, n)
        } else {
            stop(sprintf("Argument name '%s' must start with '-' or '--'", n), call. = FALSE)
        }
    }

    # Validate: at most one short and one long
    if (length(short_names) > 1) {
        stop(sprintf(
            "Multiple short argument names provided: %s. Only one short form allowed.",
            paste(short_names, collapse = ", ")
        ), call. = FALSE)
    }

    if (length(long_names) > 1) {
        stop(sprintf(
            "Multiple long argument names provided: %s. Only one long form allowed.",
            paste(long_names, collapse = ", ")
        ), call. = FALSE)
    }

    list(
        short_name = if (length(short_names) > 0) short_names[1] else character(),
        long_name = if (length(long_names) > 0) long_names[1] else character()
    )
}
