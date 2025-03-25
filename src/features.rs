use std::cell::RefCell;
use std::fmt;
use std::str::FromStr;

#[derive(Debug, Default, Clone, Copy)]
pub struct Features {
    stack: bool,
}

thread_local! {
    /// Must only be mutated within `set_features`
    static FEATURES: RefCell<Option<Features>> = const { RefCell::new(None) };
}

pub fn stack() -> bool {
    with_features(|features| features.stack)
}

pub fn init(value: Features) {
    FEATURES.with(|features| {
        let mut features = features.borrow_mut();
        assert!(
            features.is_none(),
            "tried to initialize features state multiple times"
        );
        *features = Some(value);
    });
}

fn with_features<F, R>(callback: F) -> R
where
    F: Fn(&Features) -> R,
{
    FEATURES.with(|features| {
        let features = features.borrow();
        let features = features.unwrap_or_else(|| {
            panic!("tried to access features state before initialization");
        });
        callback(&features)
    })
}

impl FromStr for Features {
    type Err = String;
    fn from_str(string: &str) -> Result<Self, Self::Err> {
        let mut features = Self::default();
        for word in string.split(',') {
            let value = match word {
                "" => continue,
                "stack" => &mut features.stack,
                _ => return Err(format!("Unknown feature '{}'", word)),
            };
            if *value {
                return Err(format!("Cannot specify feature '{}' twice", word));
            }
            *value = true;
        }
        Ok(features)
    }
}

impl fmt::Display for Features {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let features = [("stack", self.stack)];
        let mut has_any_feature = false;
        for (name, value) in features {
            if !value {
                continue;
            }
            if has_any_feature {
                write!(f, ",")?;
            }
            write!(f, "{}", name)?;
            has_any_feature = true;
        }
        Ok(())
    }
}
