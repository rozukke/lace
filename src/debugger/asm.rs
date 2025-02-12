use std::ops::Range;

use crate::air::AsmLine;
use crate::{dprint, DIAGNOSTIC_CONTEXT_LINES};

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
        dprint!(Always, Normal, "{}", line);
    }

    /// Get instruction/directive corresponding to `address`, with no context.
    pub fn get_single_line(&self, address: u16) -> Option<&str> {
        let stmt = self.get_source_statement(address)?;
        let range: Range<usize> = stmt.span.into();
        let line = &self.src[range];
        Some(line)
    }

    /// Get [`AsmLine`] corresponding to `address`.
    ///
    /// Used to access source code span.
    fn get_source_statement(&self, address: u16) -> Option<&AsmLine> {
        if address < self.orig || (address - self.orig) as usize >= self.ast.len() {
            return None;
        };
        let stmt = self
            .ast
            .get((address - self.orig) as usize)
            .expect("index was checked to be within bounds above");
        Some(stmt)
    }

    /// Get memory addresses of first and last line shown in source context.
    pub fn get_context_range(&self, stmt: &AsmLine) -> (u16, u16) {
        let stmt_start = stmt.span.offs();
        let stmt_end = stmt.span.end();

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
            line + self.orig - 1
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
            line + self.orig - 1
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::air::{AirStmt, AsmLine, ImmediateOrReg};
    use crate::symbol::{Register, Span, SrcOffset};
    use crate::{env, AsmParser};

    #[test]
    fn get_context_lines() {
        assert_eq!(
            DIAGNOSTIC_CONTEXT_LINES, 8,
            "value of `DIAGNOSTIC_CONTEXT_LINES` has changed. tests need rewriting."
        );

        let src = "main:
        ld r0 n
        call fib
        reg
        halt
; variables
n:      .fill #23

; n in r0, result in r1
fib:
        push r2
        ; accumulator
        and r1 r1 #0 ; target
        ; workspace
        and r2 r2 #0

        push r0
        call fib_inner
        pop r0

        pop r2
        rets

fib_inner:
        ; access stack variable
        ldr r0 r7 #1
        and r2 r2 #0
        add r2 r0 #-1
        brnz fib_inner

        add r0 r0 #-1
        push r0
        call fib_inner
        pop r0";

        let target = "and r1 r1 #0";

        let stmt_start = src.find(target).expect("target line not found in source");

        let stmt = AsmLine {
            line: 7,
            span: Span::new(SrcOffset(stmt_start), target.len()),
            stmt: AirStmt::And {
                dest: Register::R1,
                src_reg: Register::R1,
                src_reg_imm: ImmediateOrReg::Imm5(0),
            },
        };

        env::init();
        let parser = AsmParser::new(src).unwrap();
        let mut air = parser.parse().unwrap();
        air.backpatch().unwrap();
        let ast = air.ast;

        let orig = 0x3000;

        assert_eq!(ast.get(stmt.line as usize - 1), Some(&stmt));

        let asm_source = AsmSource::from(orig, ast.clone(), src);

        let (start, end) = asm_source.get_context_range(&stmt);

        assert_eq!(start, orig + 3);
        assert_eq!(end, orig + 11);
    }

    #[test]
    fn count_chars_in_lines() {
        assert_eq!(
            DIAGNOSTIC_CONTEXT_LINES, 8,
            "value of `DIAGNOSTIC_CONTEXT_LINES` has changed. tests need rewriting."
        );

        let src = "main:
        ld r0 n
        call fib
        reg
        halt
; variables
n:      .fill #23

; n in r0, result in r1
fib:
        push r2
        ; accumulator
        and r1 r1 #0 ; target
        ; workspace
        and r2 r2 #0

        push r0
        call fib_inner
        pop r0

        pop r2
        rets

fib_inner:
        ; access stack variable
        ldr r0 r7 #1
        and r2 r2 #0
        add r2 r0 #-1
        brnz fib_post

        add r0 r0 #-1
        push r0
        call fib_inner
        pop r0";

        let target = "and r1 r1 #0";

        let before_target = "        halt
; variables
n:      .fill #23

; n in r0, result in r1
fib:
        push r2
        ; accumulator
        ";
        let after_target = " ; target
        ; workspace
        and r2 r2 #0

        push r0
        call fib_inner
        pop r0

        pop r2";

        let stmt_start = src.find(target).expect("target line not found in source");
        let stmt_end = stmt_start + target.len();

        assert_eq!(&src[stmt_start..stmt_end], target);

        let source_above = &src[..stmt_start];
        let source_below = &src[stmt_end..];

        assert!(source_above.ends_with(before_target));
        assert!(source_below.starts_with(after_target));

        let start = stmt_start - super::count_chars_in_lines(source_above.chars().rev());
        let end = stmt_end + super::count_chars_in_lines(source_below.chars());

        assert_eq!(start, stmt_start - before_target.len());
        assert_eq!(end, stmt_end + after_target.len());
    }
}
