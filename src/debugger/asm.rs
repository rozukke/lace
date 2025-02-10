use std::ops::Range;

use crate::air::AsmLine;
use crate::{dprintln, DIAGNOSTIC_CONTEXT_LINES};

/// Reference to assembly source code.
///
/// Used by "assembly" and "break list" commands.
pub struct AsmSource {
    orig: u16,
    ast: Vec<AsmLine>,
    src: &'static str,
}

impl AsmSource {
    pub fn from(orig: u16, ast: Vec<AsmLine>, src: &'static str) -> Self {
        Self { orig, ast, src }
    }

    pub fn orig(&self) -> u16 {
        self.orig
    }

    /// Show lines surrounding instruction/directive corresponding to `address`.
    pub fn show_line_context(&self, address: u16) -> Option<&AsmLine> {
        let stmt = self.get_source_statement(address)?;
        let report = miette::miette!(
            severity = miette::Severity::Advice,
            labels = vec![miette::LabeledSpan::at(
                stmt.span,
                format!("Next instruction, at address 0x{:04x}", address),
            )],
            "",
        )
        .with_source_code(self.src);
        eprintln!("{:?}", report);
        Some(stmt)
    }

    /// Show instruction/directive corresponding to `address`, with no context.
    pub fn show_single_line(&self, address: u16) {
        let Some(stmt) = self.get_source_statement(address) else {
            return;
        };
        let range: Range<usize> = stmt.span.into();
        let line = &self.src[range];
        dprintln!(Always, Normal, "{}", line);
    }

    /// Get [`AsmLine`] corresponding to `address`.
    ///
    /// Used to access source code span.
    fn get_source_statement(&self, address: u16) -> Option<&AsmLine> {
        if address < self.orig || (address - self.orig) as usize >= self.ast.len() {
            dprintln!(
                Always,
                Error,
                "Address 0x{:04x} does not correspond to an instruction",
                address
            );
            return None;
        };
        let stmt = self
            .ast
            .get((address - self.orig) as usize)
            .expect("index was checked to be within bounds above");
        Some(stmt)
    }

    /// Get memory addresses of first and last line shown in source context.
    pub fn get_context_range(&self, orig: u16, stmt: &AsmLine) -> (u16, u16) {
        let stmt_start = stmt.span.offs();
        let stmt_end = stmt.span.end();

        // Please note that this code demands trust, not comprehension

        // Split source into characters before and after span
        // Neither string contains characters in the span, but may contain characters in the same
        // line as the instruction
        let source_above = &self.src[..stmt_start];
        let source_below = &self.src[stmt_end..];

        let start = stmt_start - count_chars_in_lines(source_above.chars().rev());
        let end = stmt_end + count_chars_in_lines(source_below.chars());

        // Ugly -- but what else can be done...
        // Please do not try to abstract this pair of expressions; it won't lead to anything good

        // Get address of earliest statement, whose span is (at least partially) within `start..`
        let start_addr = {
            let mut line = stmt.line;
            for stmt in self.ast.iter().rev() {
                if stmt.span.end() < start {
                    break;
                }
                line = stmt.line;
            }
            // -1 applied to addresses, to account for the `line` field counting from 1, not 0
            line + orig - 1
        };
        // Get address of latest statement, whose span is (at least partially) within `..end`
        let end_addr = {
            let mut line = stmt.line;
            for stmt in self.ast.iter() {
                if stmt.span.offs() >= end {
                    break;
                }
                line = stmt.line;
            }
            line + orig - 1
        };

        (start_addr, end_addr)
    }
}

/// Count characters in an iterator, within a maximum amount of lines.
fn count_chars_in_lines<I>(iter: I) -> usize
where
    I: Iterator<Item = char>,
{
    let mut count = 0;
    // Counts 0..=DIAGNOSTIC_CONTEXT_LINES
    // Note inclusive range, to account for characters in current line, outside of
    // instruction span
    let mut line = 0;
    // Previous attempts to restructure/invert this loop have been unproductive
    for ch in iter {
        if ch == '\n' {
            line += 1;
            if line > DIAGNOSTIC_CONTEXT_LINES {
                break;
            }
        }
        count += 1;
    }
    count
}
