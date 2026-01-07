# Tests for print_help function

test_that("print_help fails with non-parser object", {
    expect_error(
        print_help(list()),
        "parser must be an ArgumentParser object"
    )
})

test_that("print_help returns character string invisibly", {
    parser <- ArgumentParser(prog = "test")
    
    result <- capture.output(help_text <- print_help(parser))
    
    expect_type(help_text, "character")
})

test_that("print_help includes program name", {
    parser <- ArgumentParser(prog = "my_script.R")
    
    result <- capture.output(help_text <- print_help(parser))
    
    expect_true(any(grepl("my_script.R", result)))
})

test_that("print_help includes description", {
    parser <- ArgumentParser(
        prog = "test",
        description = "A test application for processing data"
    )
    
    result <- capture.output(print_help(parser))
    result_text <- paste(result, collapse = "\n")
    
    expect_true(grepl("A test application for processing data", result_text))
})

test_that("print_help includes epilog examples", {
    parser <- ArgumentParser(
        prog = "test",
        epilog = "Rscript test.R --input data.csv"
    )
    
    result <- capture.output(print_help(parser))
    result_text <- paste(result, collapse = "\n")
    
    expect_true(grepl("Rscript test.R --input data.csv", result_text))
})

test_that("print_help includes argument names", {
    parser <- ArgumentParser(prog = "test")
    parser <- add_argument(parser, name = "--input", help = "Input file path")
    parser <- add_argument(parser, name = "--output", help = "Output file path")
    
    result <- capture.output(print_help(parser))
    result_text <- paste(result, collapse = "\n")
    
    expect_true(grepl("--input", result_text))
    expect_true(grepl("--output", result_text))
})

test_that("print_help includes argument help text", {
    parser <- ArgumentParser(prog = "test")
    parser <- add_argument(parser, name = "--verbose", help = "Enable verbose output")
    
    result <- capture.output(print_help(parser))
    result_text <- paste(result, collapse = "\n")
    
    expect_true(grepl("Enable verbose output", result_text))
})

test_that("print_help includes default help argument", {
    parser <- ArgumentParser(prog = "test")
    
    result <- capture.output(print_help(parser))
    result_text <- paste(result, collapse = "\n")
    
    expect_true(grepl("--help", result_text))
    expect_true(grepl("-h", result_text))
})

test_that("print_help output contains Usage section", {
    parser <- ArgumentParser(prog = "test")
    
    result <- capture.output(print_help(parser))
    result_text <- paste(result, collapse = "\n")
    
    expect_true(grepl("Usage", result_text, ignore.case = TRUE))
})

test_that("print_help output contains Options section", {
    parser <- ArgumentParser(prog = "test")
    parser <- add_argument(parser, name = "--test")
    
    result <- capture.output(print_help(parser))
    result_text <- paste(result, collapse = "\n")
    
    expect_true(grepl("Options", result_text, ignore.case = TRUE))
})

test_that("print_help shows choices for arguments", {
    parser <- ArgumentParser(prog = "test")
    parser <- add_argument(parser, name = "--format", choices = c("csv", "json", "xml"))
    
    result <- capture.output(print_help(parser))
    result_text <- paste(result, collapse = "\n")
    
    expect_true(grepl("csv", result_text))
    expect_true(grepl("json", result_text))
})

test_that("print_help shows default values", {
    parser <- ArgumentParser(prog = "test")
    parser <- add_argument(parser, name = "--threads", default = "4")
    
    result <- capture.output(print_help(parser))
    result_text <- paste(result, collapse = "\n")
    
    expect_true(grepl("default.*4", result_text, ignore.case = TRUE))
})

test_that("print_help indicates required arguments", {
    parser <- ArgumentParser(prog = "test")
    parser <- add_argument(parser, name = "--input", required = TRUE, help = "Input file")
    
    result <- capture.output(print_help(parser))
    result_text <- paste(result, collapse = "\n")
    
    expect_true(grepl("required", result_text, ignore.case = TRUE))
})

test_that("print_help handles complex parser configuration", {
    parser <- ArgumentParser(
        prog = "data_processor",
        description = "Process data files with various options",
        epilog = "Examples:\n  Rscript data_processor.R --input data.csv --format json"
    )
    parser <- add_argument(parser, name = "--input", required = TRUE, help = "Input file")
    parser <- add_argument(parser, name = "--output", default = "output.txt", help = "Output file")
    parser <- add_argument(parser, name = "--format", choices = c("csv", "json"), default = "csv")
    parser <- add_argument(parser, name = "--verbose", action = "store_true", help = "Verbose mode")
    
    result <- capture.output(print_help(parser))
    result_text <- paste(result, collapse = "\n")
    
    # Check all key elements are present
    expect_true(grepl("data_processor", result_text))
    expect_true(grepl("Process data files", result_text))
    expect_true(grepl("--input", result_text))
    expect_true(grepl("--output", result_text))
    expect_true(grepl("--format", result_text))
    expect_true(grepl("--verbose", result_text))
})

