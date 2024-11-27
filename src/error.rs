use std::num::ParseIntError;

use miette::{miette, LabeledSpan, Report, Severity};

use crate::{
    lexer::{Token, TokenKind},
    parser::Bits,
    symbol::Span,
};

// Lexer errors

pub fn lex_invalid_dir(span: Span, src: &'static str) -> Report {
    miette!(
        severity = Severity::Error,
        code = "lex::dir",
        help = "check the list of available directives in the documentation.",
        labels = vec![LabeledSpan::at(span, "incorrect directive")],
        "Encountered an invalid directive.",
    )
    .with_source_code(src)
}

pub fn lex_unclosed_str(span: Span, src: &'static str) -> Report {
    miette!(
        severity = Severity::Error,
        code = "lex::str_lit",
        help = "make sure to close string literals with a \" character.",
        labels = vec![LabeledSpan::at(span, "incorrect literal")],
        "Encountered an unterminated string literal.",
    )
    .with_source_code(src)
}

pub fn lex_invalid_lit(span: Span, src: &'static str, e: ParseIntError) -> Report {
    miette!(
        severity = Severity::Error,
        code = "lex::bad_lit",
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
        "Encountered an unknown token",
    )
    .with_source_code(src)
}

// Preprocessor errors

pub fn preproc_bad_lit(span: Span, src: &'static str, is_present: bool) -> Report {
    let (help, label, severity) = if is_present {
        (
            "you may have meant to use a positive literal",
            "negative literal",
            Severity::Warning,
        )
    } else {
        (
            "this directive requires an integer or hex literal to follow",
            "not a numeric literal",
            Severity::Error,
        )
    };
    miette!(
        severity = severity,
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
        help = ".stringz requires a valid string literal like \"hello\\n\"",
        labels = vec![LabeledSpan::at(span, "not a string literal")],
        "Expected a valid string literal",
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
    let mut help = "check the operands for this instruction".to_string();
    if found.kind == TokenKind::Label {
        let label = &src[found.span.offs()..found.span.offs() + found.span.len()];
        if label.chars().all(|c| char::is_ascii_digit(&c)) {
            help = format!("you may have meant to write a decimal literal: #{}", label)
        }
    }
    miette!(
        severity = Severity::Error,
        code = "parse::unexpected_token",
        help = help,
        labels = vec![LabeledSpan::at(
            found.span,
            format!("unexpected {}", found.kind)
        )],
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
        labels = vec![LabeledSpan::at_offset(src.len() - 1, "here")],
        "Unexpected end of file",
    )
    .with_source_code(src)
}

pub fn parse_lit_range(span: Span, src: &'static str, bits: Bits) -> Report {
    miette!(
        severity = Severity::Error,
        code = "parse::unexpected_token",
        help = format!("this instruction expects literals that can be contained in {bits} bits",),
        labels = vec![LabeledSpan::at(span, "out-of-range literal")],
        "Found numeric literal of incorrect size"
    )
    .with_source_code(src)
}

pub fn parse_stack_extension_not_enabled(instr: &str, span: Span, src: &'static str) -> Report {
    miette!(
        severity = Severity::Error,
        code = "parse::stack_extension_not_enabled",
        help = "\
        this instruction requires the non-standard 'stack' extension\n\
        run with `LACE_STACK=1` to enable\n\
        note: this identifier cannot be used as a label\
        ",
        labels = vec![LabeledSpan::at(span, "non-standard instruction")],
        "Non-standard '{}' instruction used without 'stack' extension enabled",
        instr
    )
    .with_source_code(src)
}
