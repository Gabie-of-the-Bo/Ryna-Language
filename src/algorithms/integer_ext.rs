use malachite::integer::random::uniform_random_integer_range;
use malachite::Integer;
use malachite::rounding_modes::RoundingMode;
use malachite::num::conversion::traits::{RoundingFrom, SaturatingInto};
use rand::RngCore;

lazy_static! {
    pub static ref ZERO: Integer = Integer::from(0);
    pub static ref ONE: Integer = Integer::from(1);
    pub static ref BYTE_MAX: Integer = Integer::from(255);
    pub static ref USIZE_MAX: Integer = Integer::from(usize::MAX);
    pub static ref U64_MAX: Integer = Integer::from(u64::MAX);
}

pub fn to_f64(n: &Integer) -> f64 {
    f64::rounding_from(n, RoundingMode::Exact).0
}

pub fn to_usize(n: &Integer) -> usize {
    n.saturating_into()
}

pub fn to_u32(n: &Integer) -> u32 {
    n.saturating_into()
}

pub fn to_u64(n: &Integer) -> u64 {
    n.saturating_into()
}

pub fn to_i64(n: &Integer) -> i64 {
    n.saturating_into()
}

pub fn to_u8(n: &Integer) -> u8 {
    n.saturating_into()
}

pub fn is_valid_index(n: &Integer) -> bool {
    *n >= *ZERO && *n <= *USIZE_MAX
}

pub fn is_valid_byte(n: &Integer) -> bool {
    *n >= *ZERO && *n <= *BYTE_MAX
}

pub fn truncate(n: &Integer) -> Integer {
    n & &*U64_MAX
}

pub fn randint(f: Integer, t: Integer) -> Integer {
    let mut bytes = [0; 32];
    rand::thread_rng().fill_bytes(&mut bytes);
    let s = malachite::random::Seed { bytes };

    let mut r = uniform_random_integer_range(s, f, t);
    r.next().unwrap()
}