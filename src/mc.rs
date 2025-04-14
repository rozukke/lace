use std::cell::RefCell;
use std::fmt::{self, Arguments};
use std::io::{Read as _, Write as _};
use std::iter::Peekable;
use std::net::TcpStream;

use crate::runtime::{MemoryStr, RunState};

/// Interact with static connection to Minecraft server.
///
/// Opens connection if not already open.
pub fn with_connection<F, R>(func: F) -> R
where
    F: FnOnce(&mut Connection) -> R,
{
    thread_local! {
        static CONNECTION: RefCell<Option<Connection>> = const { RefCell::new(None) };
    }
    CONNECTION.with(|mc| {
        let mut mc = mc.borrow_mut();
        let mc = mc.get_or_insert_with(Connection::new);
        func(mc)
    })
}

/// Connection for Minecraft server.
#[derive(Debug)]
pub struct Connection {
    stream: TcpStream,
}

impl Connection {
    /// Default server address and port for [ELCI].
    ///
    /// [ELCI]: https://github.com/rozukke/elci
    pub const DEFAULT_ADDRESS: &'static str = "127.0.0.1:4711";

    /// Create a new connection with the default server address.
    pub fn new() -> Self {
        let Ok(stream) = TcpStream::connect(Self::DEFAULT_ADDRESS) else {
            fatal(Error::Connect);
        };
        Self { stream }
    }

    /// Serialize and send a command to the server.
    fn send(&mut self, command: Arguments) {
        if self.stream.write_fmt(command).is_err() {
            fatal(Error::Send);
        }
    }
    /// Create `[IntegerReader]` to stream read and deserialize response from server.
    fn recv(&mut self) -> IntegerReader<StreamIter> {
        IntegerReader::new(StreamIter::new(&mut self.stream).peekable())
    }

    /// Sends a message to the in-game chat, does not require a joined player.
    ///
    /// Only accepts [`CleanMemoryStr`] to ensure string sanitization.
    pub fn post_to_chat(&mut self, message: CleanMemoryStr) {
        self.send(format_args!("chat.post({})\n", message));
    }

    /// Returns a coordinate representing player position (block position of lower half of
    /// playermodel).
    pub fn get_player_position(&mut self) -> (i16, i16, i16) {
        self.send(format_args!("player.getPos()\n"));
        let mut response = self.recv();
        let x = response.next();
        let y = response.next();
        let z = response.last();
        (x, y, z)
    }

    /// Sets player position (block position of lower half of playermodel) to
    /// specified coordinate.
    pub fn set_player_position(&mut self, x: i16, y: i16, z: i16) {
        self.send(format_args!("player.setPos({},{},{})\n", x, y, z));
    }

    /// Returns block id from specified coordinate.
    pub fn get_block(&mut self, x: i16, y: i16, z: i16) -> u16 {
        self.send(format_args!("world.getBlockWithData({},{},{})\n", x, y, z));
        let mut response = self.recv();
        let id = response.next();
        let _mod = response.last::<i32>();
        id
    }

    /// Sets block id specified coordinate.
    pub fn set_block(&mut self, x: i16, y: i16, z: i16, block: u16) {
        self.send(format_args!(
            "world.setBlock({},{},{},{})\n",
            x, y, z, block
        ));
    }

    /// Returns the `y`-value of the highest solid block at the specified `x`
    /// and `z` coordinate
    pub fn get_height(&mut self, x: i16, z: i16) -> i16 {
        self.send(format_args!("world.getHeight({},{})\n", x, z));
        self.recv().last()
    }
}

/// Large enough to hold the longest realistic response (3 floats), plus some extra space.
///
/// Default buffer size for `std::io::BufReader` is currently 8kiB.
const READ_BUFFER_SIZE: usize = "12345.123456890,12345.123456890,12345.123456890\n".len() * 2;

/// Wrapper of `TcpStream` to read byte-by-byte as an `Iterator`.
struct StreamIter<'a> {
    stream: &'a mut TcpStream,
    buffer: [u8; READ_BUFFER_SIZE],
    index: usize,
    length: usize,
}

impl<'a> StreamIter<'a> {
    pub fn new(stream: &'a mut TcpStream) -> Self {
        Self {
            stream,
            buffer: [0u8; READ_BUFFER_SIZE],
            index: 0,
            length: 0,
        }
    }
}

impl<'a> Iterator for StreamIter<'a> {
    type Item = u8;
    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= self.length {
            let Ok(bytes_read) = self.stream.read(&mut self.buffer) else {
                fatal(Error::Recv)
            };
            self.index = 0;
            self.length = bytes_read;
        }
        let byte = self.buffer[self.index];
        self.index += 1;
        Some(byte)
    }
}

/// Parse an integer from an `Iterator`, byte-by-byte
///
/// Only supports the most basic decimal syntax: `^[-+]?[0-9]+(\.[0-9]+)?$`.
/// Characters after decimal point are taken into account when rounding down, but are otherwise
/// discarded.
///
/// Consumes entire integer and following byte (which must be `b','` or `b'\n'`, for `next` and
/// `last` respectively).
struct IntegerReader<I>
where
    I: Iterator<Item = u8>,
{
    inner: Peekable<I>,
}

impl<I> IntegerReader<I>
where
    I: Iterator<Item = u8>,
{
    pub fn new(inner: Peekable<I>) -> Self {
        Self { inner }
    }

    /// Read and parse next integer, expecting and consuming following `b','` (end of argument).
    pub fn next<T>(&mut self) -> T
    where
        T: TryFrom<i32>,
    {
        let (integer, is_final) = self.next_inner();
        if is_final {
            fatal(Error::UnexpectedEndOfLine);
        }
        integer
    }

    /// Read and parse next integer, expecting and consuming following `b'\n'` (end of line).
    pub fn last<T>(&mut self) -> T
    where
        T: TryFrom<i32>,
    {
        let (integer, is_final) = self.next_inner();
        if !is_final {
            fatal(Error::ExpectedEndOfLine);
        }
        integer
    }

    /// See also: [`debugger::command::parse::integer`]
    ///
    /// Boolean flag is `true` if `b'\n'` was consumed (end of line).
    fn next_inner<T>(&mut self) -> (T, bool)
    where
        T: TryFrom<i32>,
    {
        let sign = match self.inner.peek() {
            Some(b'-') => {
                self.inner.next();
                -1
            }
            Some(b'+') => {
                self.inner.next();
                1
            }
            _ => 1,
        };

        let mut integer: i32 = 0;
        let mut len = 0;

        // Take digits until any non-digit character is peeked
        while let Some(byte) = self.inner.peek().into() {
            let digit = match byte {
                Some(byte) if (b'0'..=b'9').contains(byte) => (byte - b'0') as i32,
                _ => break,
            };
            self.inner.next();

            integer *= 10;
            integer += digit;
            len += 1;
        }

        if len == 0 {
            // `^[-+]?$`
            fatal(Error::ExpectedInteger);
        }

        integer *= sign;

        // Decimal point and following digits
        if let Some(b'.') = self.inner.peek() {
            self.inner.next();

            let mut is_integer = true; // Whether all decimal digits are '0'
            loop {
                match self.inner.peek() {
                    Some(b'0') => (),
                    Some(b'1'..=b'9') => is_integer = false,
                    _ => break,
                }
                self.inner.next();
            }
            // Ensure number is always rounded down, NOT truncated
            // Without this, `-1.3` would become `-1` (instead of `-2`)
            if !is_integer && sign < 0 {
                integer -= 1;
            }
        }

        // Check and consume byte following integer
        let is_final = match self.inner.next() {
            Some(b',') => false,
            Some(b'\n') => true,
            Some(_) => fatal(Error::ExpectedEndOfArgument),
            None => fatal(Error::UnexpectedEndOfFile),
        };

        let Ok(integer) = integer.try_into() else {
            fatal(Error::IntegerTooLarge);
        };

        (integer, is_final)
    }
}

/// On [`fmt::Display::fmt`], uses [`crate::runtime::MemoryStr`] to read non-packed string from
/// memory, replacing or omitting invalid characters such as `'\n'`.
///
/// Must implement [`fmt::Display::fmt`] (not [`Iterator`]) to be used as an request argument in
/// [`Connection`]. Note that `fmt` cannot mutate `self`, so [`CleanMemoryStr`] must construct
/// [`MemoryStr`] on each format (which is cheap anyway).
pub struct CleanMemoryStr<'a> {
    state: &'a RunState,
    start: u16,
}
impl<'a> CleanMemoryStr<'a> {
    pub fn new(start: u16, state: &'a RunState) -> Self {
        Self { start, state }
    }
}
impl<'a> fmt::Display for CleanMemoryStr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for chr in MemoryStr::new(self.start, self.state) {
            match chr {
                '\n' | '\t' => write!(f, " ")?,
                '\x20'..='\x7e' => write!(f, "{}", chr)?,
                _ => (),
            }
        }
        Ok(())
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Error {
    Connect,
    Send,
    Recv,
    ExpectedInteger,
    ExpectedEndOfArgument,
    ExpectedEndOfLine,
    UnexpectedEndOfLine,
    UnexpectedEndOfFile,
    IntegerTooLarge,
}

impl std::error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Connect => write!(f, "failed to connect"),
            Self::Send => write!(f, "failed to send message"),
            Self::Recv => write!(f, "failed to read message"),
            _ => {
                write!(f, "parsing response: ")?;
                match self {
                    Self::Connect | Self::Send | Self::Recv => unreachable!(),
                    Self::ExpectedInteger => write!(f, "expected integer"),
                    Self::ExpectedEndOfArgument => write!(f, "expected end of argument"),
                    Self::ExpectedEndOfLine => write!(f, "expected end of line"),
                    Self::UnexpectedEndOfLine => write!(f, "unexpected end of line"),
                    Self::UnexpectedEndOfFile => write!(f, "unexpected end of file"),
                    Self::IntegerTooLarge => write!(f, "integer is too large"),
                }
            }
        }
    }
}

/// Print error message and exit, due to unrecoverable error interfacing with Minecraft server.
fn fatal(error: Error) -> ! {
    eprintln!("lace-mc: {}, exiting", error);
    std::process::exit(0xDD);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn integer_reader() {
        let input = "123.456,-4.024\n1.0,-242.0,24\n";
        let mut iter = IntegerReader::new(input.bytes().peekable());

        assert_eq!(iter.next::<u16>(), 123);
        assert_eq!(iter.last::<i16>(), -5);
        assert_eq!(iter.next::<i16>(), 1);
        assert_eq!(iter.next::<i16>(), -242);
        assert_eq!(iter.last::<u16>(), 24);
    }

    // TODO(test): check panics
}
