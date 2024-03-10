use malachite::integer::random::uniform_random_integer_range;
use malachite::Integer;
use malachite::rounding_modes::RoundingMode;
use malachite::num::conversion::traits::{RoundingFrom, SaturatingInto};
use rand::RngCore;

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

pub fn to_u8(n: &Integer) -> u8 {
    n.saturating_into()
}

pub fn is_valid_index(n: &Integer) -> bool {
    *n >= Integer::from(0) && *n <= Integer::from(usize::MAX)
}

pub fn is_valid_byte(n: &Integer) -> bool {
    *n >= Integer::from(0) && *n <= Integer::from(255)
}

pub fn truncate(n: &Integer) -> Integer {
    n & Integer::from(u64::MAX)
}

pub fn randint(f: Integer, t: Integer) -> Integer {
    let mut bytes = [0; 32];
    rand::thread_rng().fill_bytes(&mut bytes);
    let s = malachite::random::Seed { bytes };

    let mut r = uniform_random_integer_range(s, f, t);
    r.next().unwrap()
}