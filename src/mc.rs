use std::cell::RefCell;
use std::io::{self, BufRead as _, BufReader, Read as _, Write as _};
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

    /// Receive and deserialize a response from the server.
    fn recv_old(&mut self) -> String {
        let mut reader = BufReader::new(&self.stream);
        // TODO(opt): Use fixed size buffer
        let mut buffer = String::new();
        reader
            .read_line(&mut buffer)
            .expect("failed to read response form Minecraft server");
        buffer
    }

    fn recv(&mut self) -> ResponseReader {
        ResponseReader::new(&mut self.stream)
    }

    /// Sends a message to the in-game chat, does not require a joined player.
    pub fn post_to_chat(&mut self, message: impl AsRef<str>) {
        // TODO(fix): Sanitize string
        self.send(format!("chat.post({})\n", message.as_ref()))
    }

    /// Sets player position (block position of lower half of playermodel) to
    /// specified coordinate.
    pub fn set_player_position(&mut self, x: i16, y: i16, z: i16) {
        self.send(format!("player.setPos({},{},{})\n", x, y, z))
    }

    /// Returns a coordinate representing player position (block position of lower half of
    /// playermodel).
    pub fn get_player_position(&mut self) -> (i16, i16, i16) {
        self.send(format!("player.getPos()\n"));
        let mut response = self.recv();
        let x = response.read_integer() as i16;
        let y = response.read_integer() as i16;
        let z = response.read_integer() as i16;
        (x, y, z)
    }

    /// Returns block id from specified coordinate.
    pub fn get_block(&mut self, x: i16, y: i16, z: i16) -> u16 {
        self.send(format!("world.getBlockWithData({},{},{})\n", x, y, z));
        let mut response = self.recv();
        let id = response.read_integer() as u16;
        let _mod = response.read_integer();
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
        self.recv().read_integer() as i16
    }
}

struct ResponseReader<'a> {
    stream: &'a mut TcpStream,
}

impl<'a> ResponseReader<'a> {
    pub fn new(stream: &'a mut TcpStream) -> Self {
        Self { stream }
    }

    pub fn read_integer(&mut self) -> i32 {
        let mut integer: i32 = 0;
        let mut sign = 1;
        while let Some(byte) = self.read_byte().unwrap() {
            if matches!(byte, b',' | b'\n') {
                break;
            }
            if byte == b'.' {
                while let Some(byte) = self.read_byte().unwrap() {
                    if matches!(byte, b',' | b'\n') {
                        break;
                    }
                }
                break;
            }
            if byte == b'-' {
                assert_eq!(sign, 1, "multiple sign characters");
                sign = -1;
                continue;
            }
            let digit = match byte {
                b'0'..=b'9' => (byte - b'0') as i32,
                _ => panic!("unexpected byte `0x{:02x}`", byte),
            };
            integer *= 10;
            integer += digit;
        }
        integer *= sign;
        integer
    }

    fn read_byte(&mut self) -> Result<Option<u8>, io::Error> {
        let mut buffer = [0u8; 1];
        let bytes = self.stream.read(&mut buffer)?;
        assert!(bytes <= 1, "too many bytes read");
        if bytes == 0 {
            return Ok(None);
        }
        Ok(Some(buffer[0]))
    }
}
