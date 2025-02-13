use std::cell::RefCell;
use std::io::{Read as _, Write as _};
use std::iter::Peekable;
use std::net::TcpStream;

thread_local! {
    /// Must only be mutated within `with_connection`.
    static CONNECTION: RefCell<Option<Connection>> = const { RefCell::new(None) };
}

/// Interact with static connection to Minecraft server.
///
/// Opens connection if not already open.
pub fn with_connection<F, R>(func: F) -> R
where
    F: FnOnce(&mut Connection) -> R,
{
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

// TODO(opt): Avoid `format!`

impl Connection {
    /// Default server address and port for [ELCI].
    ///
    /// [ELCI]: https://github.com/rozukke/elci
    pub const DEFAULT_ADDRESS: &'static str = "127.0.0.1:4711";

    /// Create a new connection with the default server address.
    pub fn new() -> Self {
        let stream = TcpStream::connect(Self::DEFAULT_ADDRESS)
            .expect("failed to establish connection to Minecraft server");
        Self { stream }
    }

    /// Serialize and send a command to the server.
    fn send(&mut self, command: impl AsRef<str>) {
        self.stream
            .write_all(command.as_ref().as_bytes())
            .expect("failed to send request to Minecraft server");
    }

    fn recv(&mut self) -> IntegerReader<StreamIter> {
        IntegerReader::new(StreamIter::new(&mut self.stream).peekable())
    }

    /// Sends a message to the in-game chat, does not require a joined player.
    pub fn post_to_chat(&mut self, message: impl AsRef<str>) {
        // TODO(fix): Sanitize string
        self.send(format!("chat.post({})\n", message.as_ref()))
    }

    /// Returns a coordinate representing player position (block position of lower half of
    /// playermodel).
    pub fn get_player_position(&mut self) -> (i16, i16, i16) {
        self.send(format!("player.getPos()\n"));
        let mut response = self.recv();
        let x = response.next();
        let y = response.next();
        let z = response.last();
        (x, y, z)
    }

    /// Sets player position (block position of lower half of playermodel) to
    /// specified coordinate.
    pub fn set_player_position(&mut self, x: i16, y: i16, z: i16) {
        self.send(format!("player.setPos({},{},{})\n", x, y, z))
    }

    /// Returns block id from specified coordinate.
    pub fn get_block(&mut self, x: i16, y: i16, z: i16) -> u16 {
        self.send(format!("world.getBlockWithData({},{},{})\n", x, y, z));
        let mut response = self.recv();
        let id = response.next();
        let _mod = response.last::<i32>();
        id
    }

    /// Sets block id specified coordinate.
    pub fn set_block(&mut self, x: i16, y: i16, z: i16, block: u16) {
        self.send(format!("world.setBlock({},{},{},{})\n", x, y, z, block))
    }

    /// Returns the `y`-value of the highest solid block at the specified `x`
    /// and `z` coordinate
    pub fn get_height(&mut self, x: i16, z: i16) -> i16 {
        self.send(format!("world.getHeight({},{})\n", x, z));
        self.recv().last()
    }
}

struct StreamIter<'a> {
    stream: &'a mut TcpStream,
}
impl<'a> StreamIter<'a> {
    pub fn new(stream: &'a mut TcpStream) -> Self {
        Self { stream }
    }
}

impl<'a> Iterator for StreamIter<'a> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        let mut buffer = [0u8; 1];
        self.stream
            .read_exact(&mut buffer)
            .expect("failed to read from stream");
        Some(buffer[0])
    }
}

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

    pub fn next<T>(&mut self) -> T
    where
        T: TryFrom<i32>,
    {
        let (integer, is_final) = self.next_inner();
        assert!(!is_final, "unexpected end of line");
        integer
    }

    pub fn last<T>(&mut self) -> T
    where
        T: TryFrom<i32>,
    {
        let (integer, is_final) = self.next_inner();
        assert!(is_final, "expected end of line");
        integer
    }

    fn next_inner<T>(&mut self) -> (T, bool)
    where
        T: TryFrom<i32>,
    {
        let sign = match self.inner.peek().into() {
            Byte::Some(b'-') => {
                self.inner.next();
                -1
            }
            Byte::Some(b'+') => {
                self.inner.next();
                1
            }
            _ => 1,
        };

        let mut integer: i32 = 0;
        let mut len = 0;

        while let Byte::Some(byte) = self.inner.peek().into() {
            let digit = match byte {
                b'0'..=b'9' => (byte - b'0') as i32,
                _ => break,
            };
            self.inner.next();

            integer *= 10;
            integer += digit;
            len += 1;
        }

        assert!(len > 0, "expected integer something");

        integer *= sign;

        if let Byte::Some(b'.') = self.inner.peek().into() {
            self.inner.next();

            let mut has_decimals = false;
            while let Byte::Some(byte) = self.inner.peek().into() {
                match byte {
                    b'0' => (),
                    b'1'..=b'9' => has_decimals = true,
                    _ => break,
                }
                self.inner.next();
            }
            if has_decimals && sign < 0 {
                integer -= 1;
            }
        }

        let is_final = match self.inner.next().into() {
            Byte::Some(_) => panic!("expected end of integer"),
            Byte::EndOfArgument => false,
            Byte::EndOfLine => true,
        };

        (
            integer.try_into().map_err(|_| ()).expect("invalid integer"),
            is_final,
        )
    }
}

#[derive(Clone, Copy, Debug)]
enum Byte {
    Some(u8),
    EndOfArgument,
    EndOfLine,
}

impl From<Option<u8>> for Byte {
    fn from(byte: Option<u8>) -> Self {
        match byte {
            Some(b',') => Byte::EndOfArgument,
            Some(b'\n') => Byte::EndOfLine,
            None => Byte::EndOfLine,
            Some(byte) => Byte::Some(byte),
        }
    }
}
impl From<Option<&u8>> for Byte {
    fn from(byte: Option<&u8>) -> Self {
        byte.copied().into()
    }
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
}
