use crate::state::State;

// Implemented on each of the available opcodes
trait Instruction {
    // Run instruction onto program state
    fn run(&self, state: &mut State) -> ();
    // Gives instruction in binary form
    fn to_bin(&self) -> u16;
    // Gives a formatted string of the command
    fn to_string(&self) -> String;
}
