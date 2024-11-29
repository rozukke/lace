use std::{cell::RefCell, ffi::OsStr};

#[derive(Clone, Copy)]
struct Env {
    stack_enabled: bool,
}

thread_local! {
    /// Must only be mutated within `set_env`
    static ENV: RefCell<Option<Env>> = const { RefCell::new(None) };
}

pub fn init() {
    let value = Env {
        stack_enabled: var_is("LACE_STACK", "1"),
    };
    set_env(value);
}

pub fn is_stack_enabled() -> bool {
    with_env(|env| env.stack_enabled)
}

fn set_env(value: Env) {
    ENV.with(|env| {
        let mut env = env.borrow_mut();
        assert!(
            env.is_none(),
            "tried to initialize environment state multiple times"
        );
        *env = Some(value);
    });
}

fn with_env<F, R>(callback: F) -> R
where
    F: Fn(&Env) -> R,
{
    ENV.with(|env| {
        let env = env.borrow();
        let env = env.unwrap_or_else(|| {
            panic!("tried to access environment state before initialization");
        });
        callback(&env)
    })
}

fn var_is(name: impl AsRef<OsStr>, value: impl AsRef<str>) -> bool {
    std::env::var(name.as_ref()).is_ok_and(|v| &v == value.as_ref())
}
