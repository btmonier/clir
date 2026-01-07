# Utility functions for clir package

# CLI styling helpers (wrappers around cli package)
# These are internal functions used for formatted output

## ----
#' @keywords internal
.green <- function(text) cli::col_green(text)

## ----
#' @keywords internal
.yellow <- function(text) cli::col_yellow(text)

## ----
#' @keywords internal
.cyan <- function(text) cli::col_cyan(text)

## ----
#' @keywords internal
.magenta <- function(text) cli::col_magenta(text)

## ----
#' @keywords internal
.white <- function(text) cli::col_white(text)

## ----
#' @keywords internal
.gray <- function(text) cli::col_br_black(text)

## ----
#' @keywords internal
.bold_yellow <- function(text) cli::style_bold(cli::col_yellow(text))

## ----
#' @keywords internal
.bold_cyan <- function(text) cli::style_bold(cli::col_cyan(text))

## ----
#' @keywords internal
.bold_magenta <- function(text) cli::style_bold(cli::col_magenta(text))
