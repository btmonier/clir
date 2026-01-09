# Argument parsing functions for clir package

## ----
#' Parse command-line arguments
#'
#' @param parser An ArgumentParser object
#' @param args Optional character vector of arguments to parse. If NULL, uses commandArgs()
#'
#' @return A named list of parsed arguments
#' @export
parse_args <- function(parser, args = NULL) {
    if (!S7::S7_inherits(parser, ArgumentParser)) {
        stop("parser must be an ArgumentParser object", call. = FALSE)
    }

    # Get command line arguments if not provided
    if (is.null(args)) {
        args <- commandArgs(trailingOnly = TRUE)
    }

    # Check for --help flag early
    if ("--help" %in% args || "-h" %in% args) {
        print_help(parser)
        if (interactive()) {
            return(invisible(NULL))
        } else {
            quit(status = 0)
        }
    }

    # Initialize result with defaults
    result <- init_defaults(parser)

    # Track which arguments were provided
    provided <- logical(length(parser@arguments))
    names(provided) <- sapply(parser@arguments, function(x) x@dest)

    # Parse arguments
    i <- 1
    while (i <= length(args)) {
        current_arg <- args[i]

        # Find matching argument
        matched_arg <- find_matching_arg(parser, current_arg)

        if (is.null(matched_arg)) {
            # Skip unknown arguments for now
            i <- i + 1
            next
        }

        # Mark argument as provided
        provided[[matched_arg@dest]] <- TRUE

        # Process the argument based on its action
        parsed <- process_argument(matched_arg, args, i, result)
        result <- parsed$result
        i <- parsed$next_index
    }

    # Check required arguments
    check_required_args(parser, provided)

    return(result)
}

## ----
#' Initialize result with default values
#' @param parser An ArgumentParser object
#' @return A named list with default values
#' @keywords internal
init_defaults <- function(parser) {
    result <- list()

    for (arg in parser@arguments) {
        if (arg@action == "store_true") {
            result[[arg@dest]] <- FALSE
        } else if (arg@action == "store_false") {
            result[[arg@dest]] <- TRUE
        } else if (arg@action == "count") {
            result[[arg@dest]] <- 0L
        } else if (!is.null(arg@default)) {
            result[[arg@dest]] <- arg@default
        }
    }

    return(result)
}

## ----
#' Find matching argument in parser
#' @param parser An ArgumentParser object
#' @param arg_name The argument name to match
#' @return The matching Argument object or NULL
#' @keywords internal
find_matching_arg <- function(parser, arg_name) {
    for (arg in parser@arguments) {
        # Match against primary name, short_name, or long_name
        if (arg@name == arg_name) {
            return(arg)
        }
        if (length(arg@short_name) > 0 && arg@short_name == arg_name) {
            return(arg)
        }
        if (length(arg@long_name) > 0 && arg@long_name == arg_name) {
            return(arg)
        }
    }
    return(NULL)
}

## ----
#' Process a single argument and return updated result
#' @param arg The Argument object
#' @param args The full argument vector
#' @param i Current index in args
#' @param result Current result list
#' @return List with updated result and next index
#' @keywords internal
process_argument <- function(arg, args, i, result) {
    if (arg@action == "store_true") {
        result[[arg@dest]] <- TRUE
        return(list(result = result, next_index = i + 1))
    }

    if (arg@action == "store_false") {
        result[[arg@dest]] <- FALSE
        return(list(result = result, next_index = i + 1))
    }

    if (arg@action == "count") {
        result[[arg@dest]] <- result[[arg@dest]] + 1L
        return(list(result = result, next_index = i + 1))
    }

    # Store action - need to get value(s)
    parsed <- parse_value(arg, args, i)
    value  <- convert_type(parsed$value, arg@type)
    validate_choices(value, arg)

    result[[arg@dest]] <- value
    return(list(result = result, next_index = parsed$next_index))
}

## ----
#' Parse value(s) for an argument based on nargs
#' @param arg The Argument object
#' @param args The full argument vector
#' @param i Current index in args
#' @return List with value and next index
#' @keywords internal
parse_value <- function(arg, args, i) {
    if (arg@nargs == "?") {
        # Optional: get one value if available
        if (i + 1 <= length(args) && !startsWith(args[i + 1], "-")) {
            return(list(value = args[i + 1], next_index = i + 2))
        } else {
            return(list(value = arg@default, next_index = i + 1))
        }
    }

    if (arg@nargs == "+") {
        # One or more values
        values <- character()
        j <- i + 1
        while (j <= length(args) && !startsWith(args[j], "-")) {
            values <- c(values, args[j])
            j <- j + 1
        }
        if (length(values) == 0) {
            stop(sprintf("Argument %s requires at least one value", arg@name))
        }
        return(list(value = values, next_index = j))
    }

    if (arg@nargs == "*") {
        # Zero or more values
        values <- character()
        j <- i + 1
        while (j <= length(args) && !startsWith(args[j], "-")) {
            values <- c(values, args[j])
            j <- j + 1
        }
        return(list(
            value      = if (length(values) == 0) character(0) else values,
            next_index = j
        ))
    }

    # Single value required (default case)
    if (i + 1 > length(args) || startsWith(args[i + 1], "-")) {
        stop(sprintf("Argument %s requires a value", arg@name))
    }
    return(list(value = args[i + 1], next_index = i + 2))
}

## ----
#' Convert value to specified type
#' @param value The value to convert
#' @param type The target type
#' @return The converted value
#' @keywords internal
convert_type <- function(value, type) {
    if (is.null(value)) return(value)

    switch(
        type,
        integer = as.integer(value),
        numeric = as.numeric(value),
        logical = as.logical(value),
        value
    )
}

## ----
#' Validate value against choices
#' @param value The value to validate
#' @param arg The Argument object with choices
#' @keywords internal
validate_choices <- function(value, arg) {
    if (!is.null(arg@choices) && !all(value %in% arg@choices)) {
        stop(sprintf(
            "Invalid choice for %s. Must be one of: %s",
            arg@name,
            paste(arg@choices, collapse = ", ")
        ))
    }
}

## ----
#' Check that all required arguments were provided
#' @param parser An ArgumentParser object
#' @param provided Named logical vector of provided arguments
#' @keywords internal
check_required_args <- function(parser, provided) {
    for (arg in parser@arguments) {
        if (arg@required && !provided[[arg@dest]]) {
            stop(sprintf("Required argument %s not provided", arg@name))
        }
    }
}
