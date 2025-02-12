use std::cell::RefCell;
use std::io::{BufRead as _, BufReader, Write as _};
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
    fn recv(&mut self) -> String {
        let mut reader = BufReader::new(&self.stream);
        let mut buffer = String::new();
        reader
            .read_line(&mut buffer)
            .expect("failed to read response form Minecraft server");
        buffer
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
        let response = self.recv();
        parse_coordinate(&response).expect("malformed server response")
    }

    /// Returns block id from specified coordinate.
    pub fn get_block(&mut self, x: i16, y: i16, z: i16) -> u16 {
        self.send(format!("world.getBlockWithData({},{},{})\n", x, y, z));
        let response = self.recv();
        parse_block_id(&response).expect("malformed server response")
    }

    /// Sets block id specified coordinate.
    pub fn set_block(&mut self, x: i16, y: i16, z: i16, block: u16) {
        self.send(format!("world.setBlock({},{},{},{})\n", x, y, z, block))
    }

    /// Returns the `y`-value of the highest solid block at the specified `x`
    /// and `z` coordinate
    pub fn get_height(&mut self, x: i16, z: i16) -> i16 {
        self.send(format!("world.getHeight({},{})\n", x, z));
        let response = self.recv();
        response.trim().parse().expect("malformed server response")
    }
}

fn parse_coordinate(string: &str) -> Option<(i16, i16, i16)> {
    let mut iter = string.split(',');
    let x = next_int(&mut iter)?;
    let y = next_int(&mut iter)?;
    let z = next_int(&mut iter)?;
    if iter.next().is_some() {
        return None;
    }
    Some((x, y, z))
}

fn parse_block_id(string: &str) -> Option<u16> {
    let mut iter = string.split(',');
    let id = next_int(&mut iter)?;
    next_int::<i32>(&mut iter)?;
    if iter.next().is_some() {
        return None;
    }
    Some(id)
}

fn next_int<'a, T>(iter: &mut impl Iterator<Item = &'a str>) -> Option<T>
where
    T: TryFrom<i32>,
{
    let item = iter.next()?;
    let float: f32 = item.trim().parse().ok()?;
    let int: i32 = float.floor() as i32;
    int.try_into().ok()
}
