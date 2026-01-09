# Tests for ArgumentParser class and add_argument function

test_that("ArgumentParser can be created with defaults", {
    parser <- ArgumentParser()
    
    expect_s3_class(parser, "clir::ArgumentParser")
    expect_equal(parser@description, character())
    expect_equal(parser@epilog, character())
    expect_equal(parser@formatter_class, "help")
})

test_that("ArgumentParser uses provided prog name", {
    parser <- ArgumentParser(prog = "my_script.R")
    expect_equal(parser@prog, "my_script.R")
})

test_that("ArgumentParser stores description", {
    parser <- ArgumentParser(description = "A test CLI application")
    expect_equal(parser@description, "A test CLI application")
})

test_that("ArgumentParser stores epilog", {
    parser <- ArgumentParser(epilog = "Example: Rscript test.R --help")
    expect_equal(parser@epilog, "Example: Rscript test.R --help")
})

test_that("ArgumentParser has default help arguments", {
    parser <- ArgumentParser(prog = "test")
    
    # Should have help argument with both --help and -h forms
    expect_equal(length(parser@arguments), 1)
    help_arg <- parser@arguments[[1]]
    expect_equal(help_arg@name, "--help")
    expect_equal(help_arg@short_name, "-h")
    expect_equal(help_arg@long_name, "--help")
    expect_equal(help_arg@dest, "help")
})

test_that("add_argument adds argument to parser", {
    parser <- ArgumentParser(prog = "test")
    initial_count <- length(parser@arguments)
    
    parser <- add_argument(parser, name = "--verbose", help = "Be verbose")
    
    expect_equal(length(parser@arguments), initial_count + 1)
})

test_that("add_argument returns parser invisibly for chaining", {
    parser <- ArgumentParser(prog = "test")
    
    # The result should be the parser
    result <- add_argument(parser, name = "--test")
    expect_s3_class(result, "clir::ArgumentParser")
})

test_that("add_argument auto-generates dest from name", {
    parser <- ArgumentParser(prog = "test")
    parser <- add_argument(parser, name = "--output-file")
    
    # Get the last added argument
    last_arg <- parser@arguments[[length(parser@arguments)]]
    expect_equal(last_arg@dest, "output_file")
})

test_that("add_argument strips leading dashes from dest", {
    parser <- ArgumentParser(prog = "test")
    parser <- add_argument(parser, name = "--verbose")
    
    last_arg <- parser@arguments[[length(parser@arguments)]]
    expect_equal(last_arg@dest, "verbose")
})

test_that("add_argument preserves explicit dest", {
    parser <- ArgumentParser(prog = "test")
    parser <- add_argument(parser, name = "--output-file", dest = "outfile")
    
    last_arg <- parser@arguments[[length(parser@arguments)]]
    expect_equal(last_arg@dest, "outfile")
})

test_that("add_argument passes all properties to Argument", {
    parser <- ArgumentParser(prog = "test")
    parser <- add_argument(
        parser,
        name = "--format",
        dest = "fmt",
        help = "Output format",
        default = "csv",
        type = "character",
        action = "store",
        nargs = "?",
        required = TRUE,
        choices = c("csv", "json"),
        metavar = "FMT"
    )
    
    last_arg <- parser@arguments[[length(parser@arguments)]]
    
    expect_equal(last_arg@name, "--format")
    expect_equal(last_arg@dest, "fmt")
    expect_equal(last_arg@help, "Output format")
    expect_equal(last_arg@default, "csv")
    expect_equal(last_arg@type, "character")
    expect_equal(last_arg@action, "store")
    expect_true(last_arg@required)
    expect_equal(last_arg@choices, c("csv", "json"))
    expect_equal(last_arg@metavar, "FMT")
})

test_that("add_argument fails with non-parser object", {
    expect_error(
        add_argument(list(), name = "--test"),
        "parser must be an ArgumentParser object"
    )
})

test_that("Multiple arguments can be added to parser", {
    parser <- ArgumentParser(prog = "test")
    initial_count <- length(parser@arguments)
    
    parser <- add_argument(parser, name = "--input", help = "Input file")
    parser <- add_argument(parser, name = "--output", help = "Output file")
    parser <- add_argument(parser, name = "--verbose", action = "store_true")
    
    expect_equal(length(parser@arguments), initial_count + 3)
})

test_that("add_argument with store_true action", {
    parser <- ArgumentParser(prog = "test")
    parser <- add_argument(parser, name = "--verbose", action = "store_true")
    
    last_arg <- parser@arguments[[length(parser@arguments)]]
    expect_equal(last_arg@action, "store_true")
})

test_that("add_argument with store_false action", {
    parser <- ArgumentParser(prog = "test")
    parser <- add_argument(parser, name = "--no-cache", action = "store_false")
    
    last_arg <- parser@arguments[[length(parser@arguments)]]
    expect_equal(last_arg@action, "store_false")
})

test_that("add_argument with count action", {
    parser <- ArgumentParser(prog = "test")
    parser <- add_argument(parser, name = "-v", action = "count")
    
    last_arg <- parser@arguments[[length(parser@arguments)]]
    expect_equal(last_arg@action, "count")
})

test_that("add_argument accepts character vector for short and long forms", {
    parser <- ArgumentParser(prog = "test")
    parser <- add_argument(parser, name = c("-v", "--verbose"), action = "store_true")
    
    last_arg <- parser@arguments[[length(parser@arguments)]]
    expect_equal(last_arg@name, "--verbose")
    expect_equal(last_arg@short_name, "-v")
    expect_equal(last_arg@long_name, "--verbose")
})

test_that("add_argument throws error for multiple short forms", {
    parser <- ArgumentParser(prog = "test")
    
    expect_error(
        add_argument(parser, name = c("-v", "-V", "--verbose")),
        "Multiple short argument names provided"
    )
})

test_that("add_argument throws error for multiple long forms", {
    parser <- ArgumentParser(prog = "test")
    
    expect_error(
        add_argument(parser, name = c("-v", "--verbose", "--verb")),
        "Multiple long argument names provided"
    )
})
