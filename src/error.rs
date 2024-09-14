use std::num::ParseIntError;

use miette::{miette, LabeledSpan, Report, Severity};

use crate::{lexer::Token, symbol::Span};

// Lexer errors

pub fn lex_invalid_dir(span: Span, src: &'static str) -> Report {
    miette!(
        severity = Severity::Error,
        code = "parse::dir",
        help = "check the list of available directives in the documentation.",
        labels = vec![LabeledSpan::at(span, "incorrect directive")],
        "Encountered an invalid directive.",
    )
    .with_source_code(src)
}

pub fn lex_unclosed_str(span: Span, src: &'static str) -> Report {
    miette!(
        severity = Severity::Error,
        code = "parse::str_lit",
        help = "make sure to close string literals with a \" character.",
        labels = vec![LabeledSpan::at(span, "incorrect literal")],
        "Encountered an unterminated string literal.",
    )
    .with_source_code(src)
}

pub fn lex_invalid_lit(span: Span, src: &'static str, e: ParseIntError) -> Report {
    miette!(
        severity = Severity::Error,
        code = "parse::bad_lit",
        help = "ranges from -32,768 to 32,767 or 0 to 65,535 are allowed",
        labels = vec![LabeledSpan::at(span, "incorrect literal")],
        "Encountered an invalid literal: {e}",
    )
    .with_source_code(src)
}

pub fn lex_unknown(span: Span, src: &'static str) -> Report {
    miette!(
        severity = Severity::Error,
        code = "lex::unknown",
        help = "make sure that your int literals start with #",
        labels = vec![LabeledSpan::at(span, "unknown token")],
        "Encounetered an unknown token",
    )
    .with_source_code(src)
}

// Preprocessor errors

pub fn preproc_bad_lit(span: Span, src: &'static str, is_present: bool) -> Report {
    let (help, label) = if is_present {
        (
            "this directive expects a positive literal",
            "negative literal",
        )
    } else {
        (
            "this directive requires an integer or hex literal to follow",
            "not a numeric literal",
        )
    };
    miette!(
        severity = Severity::Error,
        code = "preproc::bad_lit",
        help = help,
        labels = vec![LabeledSpan::at(span, label)],
        "Expected valid integer or hex literal",
    )
    .with_source_code(src)
}

pub fn preproc_no_str(span: Span, src: &'static str) -> Report {
    miette!(
        severity = Severity::Error,
        code = "preproc::stringz",
        help = ".stringz requires a valid string literal like \"hello\n\"",
        labels = vec![LabeledSpan::at(span, "not a string literal")],
        "Expected a valid string literal.",
    )
    .with_source_code(src)
}

// Parser errors

pub fn parse_duplicate_label(span: Span, src: &'static str) -> Report {
    miette!(
        severity = Severity::Error,
        code = "parse::duplicate_label",
        help = "prefix labels are only allowed once per file",
        labels = vec![LabeledSpan::at(span, "duplicate label")],
        "Duplicate prefix label"
    )
    .with_source_code(src)
}

pub fn parse_generic_unexpected(src: &'static str, expected: &str, found: Token) -> Report {
    miette!(
        severity = Severity::Error,
        code = "parse::unexpected_token",
        help = "check the operands for this instruction",
        labels = vec![LabeledSpan::at(found.span, "unexpected token")],
        "Expected token of type {expected}, found {}",
        found.kind
    )
    .with_source_code(src)
}

pub fn parse_eof(src: &'static str) -> Report {
    miette!(
        severity = Severity::Error,
        code = "parse::unexpected_eof",
        help = "you may be missing operands in your last statement",
        labels = vec![LabeledSpan::at_offset(src.len() - 1, "unexpected token")],
        "Unexpected end of file",
    )
    .with_source_code(src)
}
