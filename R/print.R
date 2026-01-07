# Print/help functions for clir package

## ----
#' Print help message
#'
#' @param parser An ArgumentParser object
#'
#' @return Character string with help message (for compatibility), but also prints to console
#' @export
print_help <- function(parser) {
    if (!S7::S7_inherits(parser, ArgumentParser)) {
        stop("parser must be an ArgumentParser object", call. = FALSE)
    }

    # Program name and description header
    cat("\n")
    if (length(parser@description) > 0 && nchar(parser@description) > 0) {
        cat(.white(parser@description), "\n\n", sep = "")
    }

    # Usage section
    cat(.bold_magenta("Usage:"), "\n", sep = "")

    # Build usage line
    prog_styled <- .bold_cyan(parser@prog)

    usage_parts <- c()
    for (arg in parser@arguments) {
        if (arg@dest == "help") next
        if (arg@required) {
            part <- paste0(.yellow(arg@name), " ", .cyan(toupper(arg@dest)))
            usage_parts <- c(usage_parts, part)
        } else if (arg@action %in% c("store_true", "store_false", "count")) {
            part <- paste0("[", .green(arg@name), "]")
            usage_parts <- c(usage_parts, part)
        } else {
            part <- paste0("[", .green(arg@name), " ", .cyan(toupper(arg@dest)), "]")
            usage_parts <- c(usage_parts, part)
        }
    }

    cat("  ", prog_styled, " ", paste(usage_parts, collapse = " "), "\n\n", sep = "")

    # Options section
    cat(.bold_magenta("Options:"), "\n", sep = "")

    # Group arguments
    required_args <- list()
    optional_args <- list()

    for (arg in parser@arguments) {
        if (arg@required) {
            required_args <- append(required_args, list(arg))
        } else {
            optional_args <- append(optional_args, list(arg))
        }
    }

    # Print required arguments first
    if (length(required_args) > 0) {
        for (arg in required_args) {
            print_argument_help(arg, required = TRUE)
        }
    }

    # Print optional arguments
    if (length(optional_args) > 0) {
        for (arg in optional_args) {
            print_argument_help(arg, required = FALSE)
        }
    }

    # Epilog/Examples
    if (length(parser@epilog) > 0 && nchar(parser@epilog) > 0) {
        cat("\n", .bold_magenta("Examples:"), "\n", sep = "")
        examples <- strsplit(parser@epilog, "\n")[[1]]
        for (ex in examples) {
            ex_trimmed <- trimws(ex)
            # Skip lines that are just "Examples:" header (we already printed it)
            if (nchar(ex_trimmed) > 0 && !grepl("^Examples:?$", ex_trimmed, ignore.case = TRUE)) {
                # Check if it looks like a command (starts with Rscript or similar)
                if (grepl("^(Rscript|R |\\$|\\./)", ex_trimmed)) {
                    cat("  ", .cyan(ex_trimmed), "\n", sep = "")
                } else {
                    cat("  ", .gray(ex_trimmed), "\n", sep = "")
                }
            }
        }
    }

    cat("\n")

    # Return formatted text for compatibility (build it without cli formatting)
    lines <- character()
    usage <- sprintf("usage: %s", parser@prog)
    for (arg in parser@arguments) {
        if (arg@required) {
            usage <- paste(usage, arg@name, sep = " ")
        } else {
            usage <- paste(usage, sprintf("[%s]", arg@name), sep = " ")
        }
    }
    lines <- c(lines, usage)
    lines <- c(lines, "")

    if (length(parser@description) > 0 && nchar(parser@description) > 0) {
        lines <- c(lines, parser@description)
        lines <- c(lines, "")
    }

    if (length(parser@arguments) > 0) {
        lines <- c(lines, "arguments:")
        for (arg in parser@arguments) {
            help_text <- if (length(arg@help) > 0 && nchar(arg@help) > 0) {
                arg@help
            } else {
                "(no description)"
            }
            arg_line <- sprintf("  %-20s %s", arg@name, help_text)
            lines <- c(lines, arg_line)
        }
        lines <- c(lines, "")
    }

    if (length(parser@epilog) > 0 && nchar(parser@epilog) > 0) {
        lines <- c(lines, parser@epilog)
    }

    return(invisible(paste(lines, collapse = "\n")))
}

## ----
#' Print formatted help for a single argument
#' @param arg An Argument object
#' @param required Whether the argument is required
#' @keywords internal
print_argument_help <- function(arg, required = FALSE) {
    # Build the option name with metavar
    opt_name <- arg@name

    # Determine if we need metavar
    needs_value <- !arg@action %in% c("store_true", "store_false", "count")

    if (needs_value) {
        meta <- toupper(arg@dest)
        if (required) {
            opt_styled <- paste0(.bold_yellow(opt_name), " ", .cyan(meta))
        } else {
            opt_styled <- paste0(.green(opt_name), " ", .cyan(meta))
        }
    } else {
        if (required) {
            opt_styled <- .bold_yellow(opt_name)
        } else {
            opt_styled <- .green(opt_name)
        }
    }

    # Get help text
    help_text <- if (length(arg@help) > 0 && nchar(arg@help) > 0) arg@help else ""

    # Build extra info
    extra_parts <- c()

    # Add choices
    if (!is.null(arg@choices)) {
        choices_str <- paste0("[", paste(arg@choices, collapse = "|"), "]")
        extra_parts <- c(extra_parts, .cyan(choices_str))
    }

    # Add default
    if (!is.null(arg@default) && !arg@action %in% c("store_true", "store_false")) {
        if (is.character(arg@default) && nchar(arg@default) > 0) {
            extra_parts <- c(extra_parts, .gray(paste0("(default: ", arg@default, ")")))
        } else if (is.numeric(arg@default)) {
            extra_parts <- c(extra_parts, .gray(paste0("(default: ", arg@default, ")")))
        }
    }

    # Add required marker
    if (required) {
        extra_parts <- c(extra_parts, .bold_yellow("*required"))
    }

    extra_str <- if (length(extra_parts) > 0) {
        paste0(" ", paste(extra_parts, collapse = " "))
    } else {
        ""
    }

    # Calculate padding for alignment
    # We need to get the "display width" without ANSI codes for alignment
    opt_plain  <- if (needs_value) paste0(opt_name, " ", toupper(arg@dest)) else opt_name
    pad_width  <- max(0, 24 - nchar(opt_plain))
    padding    <- paste(rep(" ", pad_width), collapse = "")

    cat("  ", opt_styled, padding, help_text, extra_str, "\n", sep = "")
}
