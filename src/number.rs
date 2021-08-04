use std::cmp::{Eq, PartialEq, PartialOrd, Ord, Ordering};
use std::arch::x86_64::*;
use std::ops;
use rayon::prelude::*;
use rand::{
    RngCore,
    distributions::{
        Distribution, 
        Uniform
    }
};

/*
                                                  ╒══════════════════╕
    ============================================= │  IMPLEMENTATION  │ =============================================
                                                  ╘══════════════════╛
*/

/*
    ╒═══════════════════════════════════════════════════════════════════════════════════╕
    │ The limb system represents integers as numbers in base 2 ^ 64 (the size of u64).  │
    │ This allows very fast calculations compared to other dynamic binary containers by │
    │ means of native operations.                                                       │
    ╘═══════════════════════════════════════════════════════════════════════════════════╛
*/

static BITS_PER_LIMB: u64 = 64;

/*
    ╔════════════════════════════╗
    ║     INTEGER ARITHMETIC     ║
    ╠════════════════════════════╣
    ║ This section includes all  ║
    ║ the logic that is used     ║
    ║ later in order to build    ║
    ║ a generalized number class ║
    ╚════════════════════════════╝
*/

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Integer{
    pub negative: bool,
    pub limbs: Vec<u64>
}

impl PartialOrd for Integer{
    fn partial_cmp(&self, b: &Integer) -> Option<Ordering>{
        return Some(self.cmp(&b));
    }
}

impl Ord for Integer{
    fn cmp(&self, b: &Integer) -> Ordering{
        return match (self.negative, b.negative){
            (true, true) => comp_limbs(&self.limbs, &b.limbs),
            (false, false) => comp_limbs(&b.limbs, &self.limbs),
            (sa, sb) => sb.cmp(&sa)
        }
    }
}

/*
    ╒══════════════════════════════════════════════════════╕
    │ Basic implementations for base change and displaying │
    ╘══════════════════════════════════════════════════════╛
*/

#[inline]
pub fn div_by_digit_lbase(number: &mut Vec<u64>, digit: u128) -> u64{
    let mut carry = 0;

    *number = number.into_iter().map(|i| {
        carry = (carry << BITS_PER_LIMB) + *i as u128;
        let res = carry / digit;

        carry %= digit;

        return res as u64;
    
    }).skip_while(|i| *i == 0).collect();

    return carry as u64;
}

#[inline]
pub fn div_by_two(number: &mut Vec<u64>, base: u128) -> u64{
    let mut carry = 0;

    *number = number.into_iter().map(|i| {
        carry = carry * base + *i as u128;
        let res = carry >> 1;
        carry &= 1;

        return res as u64;
    
    }).skip_while(|i| *i == 0).collect();

    return carry as u64;
}

#[inline]
pub fn div_by_digit_rev_lbase(number: &mut Vec<u64>, digit: u128) -> u64{
    let mut carry = 0;

    *number = number.into_iter().rev().map(|i| {
        carry = (carry << BITS_PER_LIMB) + *i as u128;
        let res = carry / digit;

        carry %= digit;

        return res as u64;
    
    }).collect();

    *number = number.into_iter().skip_while(|i| **i == 0).map(|i| *i).collect();
    number.reverse();

    return carry as u64;
}

#[inline]
pub fn div_by_two_rev_lbase(number: &mut Vec<u64>) -> u64{
    let mut carry = 0;

    *number = number.into_iter().rev().map(|i| {
        carry = (carry << BITS_PER_LIMB) + *i as u128;
        let res = carry >> 1;
        carry &= 1;

        return res as u64;
    
    }).collect();

    *number = number.into_iter().skip_while(|i| **i == 0).map(|i| *i).collect();
    number.reverse();

    return carry as u64;
}

#[inline]
pub fn base_10_to_bin(b10: &mut Vec<u64>) -> Vec<u8>{
    let mut res = Vec::with_capacity(b10.len());

    while !b10.is_empty() {
        res.push(div_by_two(b10, 10) as u8);
    }

    return res;
}

#[inline]
pub fn bin_to_base_10(bin: &mut Vec<u64>) -> Vec<u8>{
    let mut res = Vec::with_capacity(bin.len());

    while !bin.is_empty() {
        res.push(div_by_digit_lbase(bin, 10) as u8);
    }

    return res;
}

#[inline]
fn slice_to_u64(bin: &[u8]) -> u64{
    let mut res: u64 = 0;

    for (i, n) in bin.iter().rev().enumerate(){
        res <<= (i > 0) as u64;
        res += *n as u64;
    }

    return res;
} 

impl From<&str> for Integer{
    fn from(string: &str) -> Self{
        let negative = string.starts_with("-");
        let number = if negative { &string[1..] } else { string };

        let mut digits = number.chars().map(|i| char::to_digit(i, 10).unwrap() as u64).collect::<Vec<_>>();

        let mut res = Integer{
            negative: negative,
            limbs: base_10_to_bin(&mut digits).chunks(BITS_PER_LIMB as usize).map(slice_to_u64).collect()
        };

        if res.limbs.is_empty() {
            res.limbs.push(0);
        }

        return res;
    }
}

impl std::fmt::Display for Integer{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let cpy = self.clone();
        if self.negative{
            write!(f, "-").unwrap();
        }

        for d in bin_to_base_10(&mut cpy.limbs.into_iter().rev().map(|i| i as u64).collect()).iter().rev(){
            write!(f, "{}", d).unwrap();
        }

        return Ok(());
    }
}

/*
    ╒══════════════════════════════╕
    │ Integer comparison algorithm │
    ╘══════════════════════════════╛
*/

#[inline]
fn comp_limbs(a: &Vec<u64>, b: &Vec<u64>) -> Ordering{
    if a.len() > b.len() {
        return Ordering::Greater;
    
    } else if a.len() < b.len() {
        return Ordering::Less;
    }

    for (a, b) in a.iter().rev().zip(b.iter().rev()){
        if a > b {
            return Ordering::Greater;
        
        } else if a < b {
            return Ordering::Less;
        }
    }

    return Ordering::Equal;
}

#[inline]
fn comp_slices(a: &[u64], b: &[u64]) -> Ordering{
    if a.len() > b.len() {
        return Ordering::Greater;
    
    } else if a.len() < b.len() {
        return Ordering::Less;
    }

    for (a, b) in a.iter().rev().zip(b.iter().rev()){
        if a > b {
            return Ordering::Greater;
        
        } else if a < b {
            return Ordering::Less;
        }
    }

    return Ordering::Equal;
}

/*
    ╒═══════════════════════════════════╕
    │ Algorithms for bitwise operations │
    ╘═══════════════════════════════════╛
*/

#[inline]
fn shift_limbs_left(a: &mut Vec<u64>, b: u64) {
    let offset = (b / BITS_PER_LIMB) as usize;
    let rem = b % BITS_PER_LIMB;

    let mut overflow = 0;

    if rem != 0 {
        for i in a.iter_mut(){
            let curr_overflow = *i >> (BITS_PER_LIMB - rem);
            *i = (*i << rem) | overflow;
            overflow = curr_overflow;
        }

        if overflow != 0{
            a.push(overflow);
        }
    }

    if offset != 0{
        a.splice(0..0, std::iter::repeat(0).take(offset));
    }
}

#[inline]
fn shift_limbs_left_cpy(a: &Vec<u64>, b: u64) -> Vec<u64> {
    let mut limbs = a.clone();

    shift_limbs_left(&mut limbs, b);

    return limbs;
}

#[inline]
fn shift_limbs_right(a: &mut Vec<u64>, b: u64) {
    let offset = (b / BITS_PER_LIMB) as usize;
    let rem = b % BITS_PER_LIMB;

    let mut overflow = 0;

    if rem != 0 {
        for i in a.iter_mut().rev(){
            let curr_overflow = *i << (BITS_PER_LIMB - rem);
            *i = (*i >> rem) | overflow;
            overflow = curr_overflow;
        }
    }

    if offset != 0{
        a.drain(0..offset);
    }
}

#[inline]
fn shift_limbs_right_cpy(a: &Vec<u64>, b: u64) -> Vec<u64> {
    let mut limbs = a.clone();

    shift_limbs_right(&mut limbs, b);

    return limbs;
}

#[inline]
fn or_limbs(a: &Vec<u64>, b: &Vec<u64>) -> Vec<u64> {
    let (mut h, l) = if a.len() >= b.len() { (a.clone(), b) } else { (b.clone(), a) };

    for (i, j) in h.iter_mut().zip(l.iter()){
        *i |= *j;
    }
               
    while !h.is_empty() && *h.last().unwrap() == 0 {
        h.pop();
    }

    return if h.is_empty() { vec!(0) } else { h };
}

#[inline]
fn and_limbs(a: &Vec<u64>, b: &Vec<u64>) -> Vec<u64> {
    let (mut h, l) = if a.len() >= b.len() { (a.clone(), b) } else { (b.clone(), a) };
    let size_diff = h.len() - l.len();

    for (i, j) in h.iter_mut().zip(l.iter().chain(std::iter::repeat(&0).take(size_diff))){
        *i &= *j;
    }
               
    while !h.is_empty() && *h.last().unwrap() == 0 {
        h.pop();
    }

    return if h.is_empty() { vec!(0) } else { h };
}

#[inline]
fn xor_limbs(a: &Vec<u64>, b: &Vec<u64>) -> Vec<u64> {
    let (mut h, l) = if a.len() >= b.len() { (a.clone(), b) } else { (b.clone(), a) };

    for (i, j) in h.iter_mut().zip(l.iter()){
        *i ^= *j;
    }
               
    while !h.is_empty() && *h.last().unwrap() == 0 {
        h.pop();
    }

    return if h.is_empty() { vec!(0) } else { h };
}

#[inline]
fn not_limbs(a: &Vec<u64>) -> Vec<u64> {
    let mut h = a.clone();
    
    for i in h.iter_mut() {
        *i = !*i;
    }
               
    while !h.is_empty() && *h.last().unwrap() == 0 {
        h.pop();
    }

    return if h.is_empty() { vec!(0) } else { h };
}

/*
    ╒═════════════════════════════════════╕
    │ Addition and subtraction algorithms │
    ╘═════════════════════════════════════╛
*/

#[inline]
fn add_limbs(a: &Vec<u64>, b: &Vec<u64>) -> Vec<u64>{
    let (mut h, l) = if a.len() >= b.len() { (a.clone(), b) } else { (b.clone(), a) };

    let mut carry = 0;

    // First half up to the size of the smallest number
    for i in 0..l.len(){
        carry = unsafe { _addcarry_u64(carry, h[i], l[i], &mut h[i]) };
    }

    // From the smallest to the largest
    for i in l.len()..h.len(){
        carry = unsafe { _addcarry_u64(carry, h[i], 0, &mut h[i]) };
    }
                            
    if carry > 0{
        h.push(carry as u64);
    }

    return h;
}

#[inline]
fn add_limbs_offset(a: &mut Vec<u64>, b: &Vec<u64>, offset: usize){
    if a.len() < b.len() + offset{
        a.extend(std::iter::repeat(&0).take((b.len() + offset) - a.len()));
    }
    
    let mut carry = 0;

    for i in offset..a.len(){
        carry = unsafe { _addcarry_u64(carry, a[i], b[i - offset], &mut a[i]) };
    }

    if carry > 0{
        a.push(carry as u64);
    }
}

#[inline]
fn sub_limbs(a: &Vec<u64>, b: &Vec<u64>) -> Vec<u64>{
    let (mut h, l) = if a.len() >= b.len() { (a.clone(), b) } else { (b.clone(), a) };
    let size_diff = h.len() - l.len();

    let mut borrow = 0;

    for (i, j) in h.iter_mut().zip(l.iter().chain(std::iter::repeat(&0).take(size_diff))){
        borrow = unsafe { _subborrow_u64(borrow, *i, *j, i) };
    }
               
    while !h.is_empty() && *h.last().unwrap() == 0 {
        h.pop();
    }

    return if h.is_empty() { vec!(0) } else { h };
}

#[inline]
fn sub_slices(a: &mut [u64], b: &[u64]){
    // a is assumed to be at least as big as b
    let size_diff = a.len() - b.len();

    let mut borrow = 0;

    for (i, j) in a.iter_mut().zip(b.iter().chain(std::iter::repeat(&0).take(size_diff))){
        borrow = unsafe { _subborrow_u64(borrow, *i, *j, i) };
    }
}

/*
    ╒═══════════════════════════╕
    │ Multiplication algorithms │
    ╘═══════════════════════════╛
*/

#[inline]
pub fn mul_limbs(a: &Vec<u64>, b: &Vec<u64>) -> Vec<u64>{
    let mut res = vec!();

    b.iter().map(|n| {
        let mut carry = 0;

        let mut mul_res = a.iter().map(|i| unsafe {
            let mut res = 0;
            let prev_carry = carry;
            let of = _addcarry_u64(0, _mulx_u64(*i, *n, &mut carry), prev_carry, &mut res);
            carry += of as u64;

            return res;
        }).collect::<Vec<_>>();

        if carry > 0{
            mul_res.push(carry as u64);
        }

        return mul_res;

    }).enumerate().for_each(|(offset, v)| {
        if offset == 0{
            res = v;

        } else{
            add_limbs_offset(&mut res, &v, offset);
        }
    });

    return res;
}

#[inline]
pub fn karatsuba_mul(a: &Vec<u64>, b: &Vec<u64>) -> Vec<u64>{
    let shift = a.len().min(b.len()) / 2;

    let x0 = a[..shift].to_vec();
    let x1 = a[shift..].to_vec();
    let y0 = b[..shift].to_vec();
    let y1 = b[shift..].to_vec();

    let mut v = [(&x0, &y0), (&x1, &y1), (&add_limbs(&x1, &x0), &add_limbs(&y1, &y0))]
                .par_iter()
                .map(|(a, b)| select_algorithm_and_multiply(&a, &b))
                .collect::<Vec<_>>();

    let p = v.pop().unwrap();
    let z2 = v.pop().unwrap();
    let mut z0 = v.pop().unwrap();
    let z1 = sub_limbs(&sub_limbs(&p, &z2), &z0);

    add_limbs_offset(&mut z0, &z1, shift);
    add_limbs_offset(&mut z0, &z2, shift * 2);

    return z0;
}

#[inline]
pub fn toom_3_mul(a: &Vec<u64>, b: &Vec<u64>) -> Vec<u64>{
    let shift = a.len().min(b.len()) / 3 - 1;

    let m0 = a[..shift].to_vec();
    let m1 = a[shift.. shift*2].to_vec();
    let m2 = a[shift*2..].to_vec();
    let n0 = b[..shift].to_vec();
    let n1 = b[shift.. shift*2].to_vec();
    let n2 = b[shift*2..].to_vec();

    // First polynomial

    let mut s2 = add_limbs(&m2, &m2);
    s2 = add_limbs(&s2, &s2);

    let p1 = add_limbs(&add_limbs(&m0, &m1), &m2);
    let pn1 = sub_limbs(&add_limbs(&m0, &m2), &m1);
    let mut pn2 = add_limbs(&m0, &s2);
    pn2 = sub_limbs(&pn2, &m1);
    pn2 = sub_limbs(&pn2, &m1);
    let p0 = m0;
    let pi = m2;

    // Second polynomial

    let mut s2 = add_limbs(&n2, &n2);
    s2 = add_limbs(&s2, &s2);

    let q1 = add_limbs(&add_limbs(&n0, &n1), &n2);
    let qn1 = sub_limbs(&add_limbs(&n0, &n2), &n1);
    let mut qn2 = add_limbs(&n0, &s2);
    qn2 = sub_limbs(&qn2, &n1);
    qn2 = sub_limbs(&qn2, &n1);
    let q0 = n0;
    let qi = n2;

    // Product polynomial

    let mut v = [(&p0, &q0), (&p1, &q1), (&pn1, &qn1), (&pn2, &qn2), (&pi, &qi)]
                .par_iter()
                .map(|(a, b)| select_algorithm_and_multiply(&a, &b))
                .collect::<Vec<_>>();

    let ri = Integer::new(false, v.pop().unwrap());
    let rn2 = Integer::new(false, v.pop().unwrap());
    let rn1 = Integer::new(false, v.pop().unwrap());
    let r1 = Integer::new(false, v.pop().unwrap());
    let r0 = Integer::new(false, v.pop().unwrap());

    // Matrix product

    let mut f3 = &rn2 - &r1;
    div_by_digit_rev_lbase(&mut f3.limbs, 3);

    let mut f1 = &r1 - &rn1;
    div_by_two_rev_lbase(&mut f1.limbs);

    let mut f2 = &rn1 - &r0;

    f3 = &f2 - &f3;
    div_by_two_rev_lbase(&mut f3.limbs);

    f3 = &f3 + &(&ri + &ri);

    let f4 = ri;

    f2 = &(&f2 + &f1) - &f4;
    f1 = &f1 - &f3;

    let mut f0 = r0;

    // Number reconstruction

    add_limbs_offset(&mut f0.limbs, &f1.limbs, shift);
    add_limbs_offset(&mut f0.limbs, &f2.limbs, shift*2);
    add_limbs_offset(&mut f0.limbs, &f3.limbs, shift*3);
    add_limbs_offset(&mut f0.limbs, &f4.limbs, shift*4);
    
    return f0.limbs;
}

/*
    ╒═════════════════════════════════╕
    │ Division and modulus algorithms │
    ╘═════════════════════════════════╛
*/

pub fn div_limbs_long(a: &Vec<u64>, b: &Vec<u64>, rem: Option<&mut Vec<u64>>) -> Vec<u64>{
    if a.len() < b.len() {
        if rem.is_some() {
            *rem.unwrap() = a.clone();
        }

        return vec!(0);
    
    } else {
        match comp_limbs(&a, &b) {
            Ordering::Less => {
                if rem.is_some() {
                    *rem.unwrap() = a.clone();
                }

                return vec!(0);
            },

            Ordering::Equal => {
                if rem.is_some() {
                    *rem.unwrap() = vec!(0);
                }

                return vec!(1);
            },

            Ordering::Greater => {
                let mut a_cpy = a.clone();
                let mut b_cpy = b.clone();

                return div_limbs_knuthd(&mut a_cpy, &mut b_cpy, rem);
            }
        }
    }
}

fn to_wide(a0: u64, a1: u64) -> u128{
    return ((a0 as u128) << BITS_PER_LIMB) | a1 as u128;
}

fn div_wide(a0: u64, a1: u64, b0: u64) -> (u128, u128){
    let lhs = to_wide(a0, a1);

    return (lhs / b0 as u128, lhs % b0 as u128);
}

pub fn div_limbs_knuthd(u: &mut Vec<u64>, v: &mut Vec<u64>, rem: Option<&mut Vec<u64>>) -> Vec<u64>{
    let leading_zeros = v.last().unwrap().leading_zeros();
    shift_limbs_left(u, leading_zeros as u64);
    shift_limbs_left(v, leading_zeros as u64);

    while u.len() <= v.len() {
        u.push(0);
    }

    if *u.last().unwrap() != 0 {
        u.push(0);
    }
    
    let n = v.len();
    let m = u.len() - n - 1;
    let b = u64::MAX as u128 + 1;
    let mut q: Vec<u64> = vec!(0; m + 1);

    for j in (0..=m).rev(){
        let (mut q0, mut r) = div_wide(u[n + j], u[n - 1 + j], v[n - 1]);

        if q0 == b || q0 * v[n - 2] as u128 > r * b + u[n - 2 + j] as u128 {
            loop {
                let r2 = r + v[n - 1] as u128;

                if r2 < b {
                    q0 -= 1;
                    r = r2;
                    
                } else{
                    break; 
                }
            }
        }

        let mut p = mul_limbs(&v, &vec!(q0 as u64));

        if let Ordering::Less = comp_slices(&u[j..=(n + j)], &p[..]){
            sub_slices(&mut p[..], &v[..]);
            q0 -= 1;
        }

        sub_slices(&mut u[j..=(n + j)], &p[..]);
        
        q[j] = q0 as u64;
    }

    while !q.is_empty() && *q.last().unwrap() == 0 {
        q.pop();
    }

    if rem.is_some() {
        while !u.is_empty() && *u.last().unwrap() == 0 {
            u.pop();
        }

        *rem.unwrap() = shift_limbs_right_cpy(&u[0..n].to_vec(), leading_zeros as u64);
    }

    return q;
}

pub fn div_limbs_u128(a: &Vec<u64>, b: &Vec<u64>) -> Vec<u64>{
    let a_wide = to_wide(*a.get(1).unwrap_or(&0), a[0]);
    let b_wide = to_wide(*b.get(1).unwrap_or(&0), b[0]);

    let res = a_wide / b_wide;

    return if res <= u64::MAX as u128 {
        vec!(res as u64)

    } else{
        vec!(res as u64, (res >> BITS_PER_LIMB) as u64)
    }
}

pub fn mod_limbs_u128(a: &Vec<u64>, b: &Vec<u64>) -> Vec<u64>{
    let a_wide = to_wide(*a.get(1).unwrap_or(&0), a[0]);
    let b_wide = to_wide(*b.get(1).unwrap_or(&0), b[0]);

    let res = a_wide % b_wide;

    return if res <= u64::MAX as u128 {
        vec!(res as u64)

    } else{
        vec!(res as u64, (res >> BITS_PER_LIMB) as u64)
    }
}

pub fn div_limbs_digit(a: &Vec<u64>, b: &Vec<u64>) -> Vec<u64>{
    let mut res = a.clone();

    div_by_digit_rev_lbase(&mut res, b[0] as u128);

    return res;
}


/*
    ╒════════════════════════════════╕
    │ Algorithm selection heuristics │
    ╘════════════════════════════════╛
*/

#[inline]
fn select_algorithm_and_multiply(a: &Vec<u64>, b: &Vec<u64>) -> Vec<u64>{
    let min_size = a.len().min(b.len());

    if min_size < 50 {
        return mul_limbs(&a, &b);

    } else if min_size < 250 {
        return karatsuba_mul(&a, &b);

    } else{
        return toom_3_mul(&a, &b);
    }
}

#[inline]
fn select_algorithm_and_divide(a: &Vec<u64>, b: &Vec<u64>) -> Vec<u64>{
    if a.len() == 1 && b.len() == 1 {
        return vec!(a[0] / b[0]);

    } else if a.len() <= 2 && b.len() <= 2 {
        return div_limbs_u128(&a, &b);

    } else if b.len() == 1 {
        return div_limbs_digit(&a, &b);

    } else{
        return div_limbs_long(&a, &b, None);
    }
}

#[inline]
fn select_algorithm_and_mod(a: &Vec<u64>, b: &Vec<u64>) -> Vec<u64>{
    if a.len() == 1 && b.len() == 1 {
        return vec!(a[0] % b[0]);

    } else if a.len() <= 2 && b.len() <= 2 {
        return mod_limbs_u128(&a, &b);

    } else{
        let mut rem = vec!();

        div_limbs_long(&a, &b, Some(&mut rem));

        return rem;
    }
}

/*
    ╒═════════════════════════════════════════╕
    │ Wrappers for full arithmetic operations │
    ╘═════════════════════════════════════════╛
*/

#[inline]
fn full_add(a: &Integer, b: &Integer) -> Integer{
    return match (a.negative, b.negative){
        (true, true) | (false, false) => Integer{
                                            negative: b.negative, 
                                            limbs: add_limbs(&a.limbs, &b.limbs)
                                        },
        (s, _) => match (s, comp_limbs(&a.limbs, &b.limbs)){
            (true, Ordering::Less) =>   Integer{
                                            negative: false, 
                                            limbs: sub_limbs(&b.limbs, &a.limbs)
                                        },
            (true, _) =>                Integer{
                                            negative: true, 
                                            limbs: sub_limbs(&a.limbs, &b.limbs)
                                        },
            (false, Ordering::Less) =>  Integer{
                                            negative: true, 
                                            limbs: sub_limbs(&b.limbs, &a.limbs)
                                        },
            (false, _) =>               Integer{
                                            negative: false, 
                                            limbs: sub_limbs(&a.limbs, &b.limbs)
                                        }
        }
    }
}

#[inline]
fn full_sub(a: &Integer, b: &Integer) -> Integer{
    return match (a.negative, b.negative){
        (true, false) | (false, true) => Integer{
                                            negative: a.negative, 
                                            limbs: add_limbs(&a.limbs, &b.limbs)
                                        },
        (s, _) => match (s, comp_limbs(&a.limbs, &b.limbs)){
            (true, Ordering::Greater) => Integer{
                                            negative: true, 
                                            limbs: sub_limbs(&a.limbs, &b.limbs)
                                        },
            (true, _) =>                Integer{
                                            negative: false, 
                                            limbs: sub_limbs(&b.limbs, &a.limbs)
                                        },
            (false, Ordering::Less) =>  Integer{
                                            negative: true, 
                                            limbs: sub_limbs(&b.limbs, &a.limbs)
                                        },
            (false, _) =>               Integer{
                                            negative: false, 
                                            limbs: sub_limbs(&a.limbs, &b.limbs)
                                        }
        }
    }
}

#[inline]
fn full_mul(a: &Integer, b: &Integer) -> Integer{
    let mut res = Integer{
        negative: a.negative ^ b.negative,
        limbs: select_algorithm_and_multiply(&a.limbs, &b.limbs)
    };

    res.normalize_sign();

    return res;
}

#[inline]
fn full_div(a: &Integer, b: &Integer) -> Integer{
    let mut res = Integer{
        negative: a.negative ^ b.negative,
        limbs: select_algorithm_and_divide(&a.limbs, &b.limbs)
    };

    res.normalize_sign();

    return res;
}

#[inline]
fn full_mod(a: &Integer, b: &Integer) -> Integer{
    let mut res = Integer{
        negative: a.negative,
        limbs: select_algorithm_and_mod(&a.limbs, &b.limbs)
    };

    res.normalize_sign();

    return res;
}

fn full_lshift(a: &Integer, b: u64) -> Integer{
    return Integer{
        negative: a.negative,
        limbs: shift_limbs_left_cpy(&a.limbs, b)
    };
}

fn full_rshift(a: &Integer, b: u64) -> Integer{
    return Integer{
        negative: a.negative,
        limbs: shift_limbs_right_cpy(&a.limbs, b)
    };
}

fn full_or(a: &Integer, b: &Integer) -> Integer{
    return Integer{
        negative: a.negative,
        limbs: or_limbs(&a.limbs, &b.limbs)
    };
}

fn full_and(a: &Integer, b: &Integer) -> Integer{
    return Integer{
        negative: a.negative,
        limbs: and_limbs(&a.limbs, &b.limbs)
    };
}

fn full_xor(a: &Integer, b: &Integer) -> Integer{
    return Integer{
        negative: a.negative,
        limbs: xor_limbs(&a.limbs, &b.limbs)
    };
}

fn full_not(a: &Integer) -> Integer{
    return Integer{
        negative: a.negative,
        limbs: not_limbs(&a.limbs)
    };
}

/*
    ╒═════════════════════════════════════════════════════════════════════════════════════════════════╕
    │ These lines define the basic arithmetic operators for all combinations of references and values │
    ╘═════════════════════════════════════════════════════════════════════════════════════════════════╛
*/

impl_op_ex!(+ |a: &Integer, b: &Integer| -> Integer { full_add(a, b) });
impl_op_ex!(- |a: &Integer, b: &Integer| -> Integer { full_sub(a, b) });
impl_op_ex!(* |a: &Integer, b: &Integer| -> Integer { full_mul(a, b) });
impl_op_ex!(/ |a: &Integer, b: &Integer| -> Integer { full_div(a, b) });
impl_op_ex!(% |a: &Integer, b: &Integer| -> Integer { full_mod(a, b) });
impl_op_ex!(<< |a: &Integer, b: &u64| -> Integer { full_lshift(a, *b) });
impl_op_ex!(>> |a: &Integer, b: &u64| -> Integer { full_rshift(a, *b) });
impl_op_ex!(| |a: &Integer, b: &Integer| -> Integer { full_or(a, b) });
impl_op_ex!(& |a: &Integer, b: &Integer| -> Integer { full_and(a, b) });
impl_op_ex!(^ |a: &Integer, b: &Integer| -> Integer { full_xor(a, b) });
impl_op_ex!(! |a: &Integer| -> Integer { full_not(a) });

impl Integer{
    pub fn new(negative: bool, limbs: Vec<u64>) -> Integer{
        return Integer{
            negative: negative,
            limbs: limbs
        };
    }

    pub fn rand_with_size(digits: usize, can_be_negative: bool) -> Integer{
        let mut rng = rand::thread_rng();
        let distribution = Uniform::from(0..10);

        let mut string = "".to_string();

        for _j in 0..digits{
            let digit = distribution.sample(&mut rng).to_string().bytes().nth(0).unwrap() as char;

            if string.len() > 0 || digit != '0'{
                string.push(digit);
            }
        }

        if can_be_negative && distribution.sample(&mut rng) < 5{
            string.insert(0, '-');
        }

        return Integer::from(string.as_str());
    }

    pub fn rand_with_limbs(limbs: usize, can_be_negative: bool) -> Integer{
        let mut rng = rand::thread_rng();
        let mut res = Integer::new((rng.next_u64() % 2) == 0 && can_be_negative, Vec::with_capacity(limbs));

        for _j in 0..limbs{
            res.limbs.push(rng.next_u64());
        }

        return res;
    }

    pub fn is_zero(&self) -> bool{
        return self.limbs.iter().all(|&i| i == 0);
    }

    pub fn normalize_sign(&mut self) -> &Self{
        if self.is_zero(){
            self.negative = false;
        }
        
        return self;
    }

    pub fn bin(&self) -> String{
        return self.limbs.iter().rev()
                                .map(|i| format!("{:064b}", i))
                                .collect::<Vec<_>>()
                                .join("")
                                .chars()
                                .skip_while(|i| *i == '0')
                                .collect();
    }

    pub fn to_f64(&self) -> f64{
        if self.limbs.len() == 1{
            return *self.limbs.last().unwrap() as f64;

        } else{
            let first = to_wide(self.limbs[self.limbs.len() - 1], self.limbs[self.limbs.len() - 2]); 
            let exponent = (self.limbs.len() - 2) as u64 * BITS_PER_LIMB;
            return (first as f64) * 2_f64.powi(exponent as i32);
        }

    }
}

/*
    ╔════════════════════════════╗
    ║     GENERAL ARITHMETIC     ║
    ╠════════════════════════════╣
    ║ This section uses the      ║
    ║ Integer class to build a   ║
    ║ Number class, which works  ║
    ║ just like an Integer or a  ║
    ║ f64 depending on the case  ║
    ╚════════════════════════════╝
*/

#[derive(Debug, Clone, PartialEq)]
pub enum Number {
    Int(Integer),
    Float(f64)
}

/*
    ╒══════════════════════════════════════╕
    │ Conversion routines from other types │
    ╘══════════════════════════════════════╛
*/

impl From<Integer> for Number{
    fn from(obj: Integer) -> Number{
        return 
        Number::Int(obj);
    }
}

macro_rules! number_conversion_float {
    ($from_type: tt) => {
        impl From<$from_type> for Number{
            fn from(obj: $from_type) -> Number{
                return Number::Float(obj.into());
            }
        }
    };
}

macro_rules! number_conversion_signed {
    ($from_type: tt) => {
        impl From<$from_type> for Number {
            fn from(obj: $from_type) -> Number{
                let limb: i64 = obj.into();
        
                return Number::Int(Integer{
                    negative: (obj as i64) < 0_i64,
                    limbs: vec!(limb.abs() as u64)
                });
            }
        }
    };
}

macro_rules! number_conversion_unsigned {
    ($from_type: tt) => {
        impl From<$from_type> for Number {
            fn from(obj: $from_type) -> Number{        
                return Number::Int(Integer{
                    negative: false,
                    limbs: vec!(obj.into())
                });
            }
        }
    };
}

number_conversion_float!(f32);
number_conversion_float!(f64);
number_conversion_signed!(i8);
number_conversion_signed!(i16);
number_conversion_signed!(i32);
number_conversion_signed!(i64);
number_conversion_unsigned!(u8);
number_conversion_unsigned!(u16);
number_conversion_unsigned!(u32);
number_conversion_unsigned!(u64);

/*
    ╒═════════════════════════════════════╕
    │ Basic arithmetic on generic Numbers │
    ╘═════════════════════════════════════╛
*/

macro_rules! generic_op_definition {
    ($op: tt) => {
        impl_op_ex!($op |a: &Number, b: &Number| -> Number {
            return match(a, b) {
                (Number::Int(aa), Number::Int(bb)) => Number::Int(aa $op bb),
                (Number::Int(aa), Number::Float(bb)) => Number::Float(aa.to_f64() $op bb),
                (Number::Float(aa), Number::Int(bb)) => Number::Float(aa $op bb.to_f64()),
                (Number::Float(aa), Number::Float(bb)) => Number::Float(aa $op bb),
            };
        });
    };
}

generic_op_definition!(+);
generic_op_definition!(-);
generic_op_definition!(*);
generic_op_definition!(/);
generic_op_definition!(%);

macro_rules! generic_logic_op_definition {
    ($op: tt) => {
        impl_op_ex!($op |a: &Number, b: &Number| -> Number {
            return match(a, b) {
                (Number::Int(aa), Number::Int(bb)) => Number::Int(aa $op bb),
                (_, _) => panic!("Cannot apply bitwise operators to floats")
            };
        });
    };
}

generic_logic_op_definition!(|);
generic_logic_op_definition!(&);
generic_logic_op_definition!(^);

impl_op_ex!(! |a: &Number| -> Number {
    return match a {
        Number::Int(aa) => Number::Int(!aa),
        _ => panic!("Cannot apply bitwise operators to floats")
    };
});

impl_op_ex!(<< |a: &Number, b: &u64| -> Number {
    return match a {
        Number::Int(aa) => Number::Int(aa << b),
        _ => panic!("Cannot apply bitwise operators to floats")
    };
});

impl_op_ex!(>> |a: &Number, b: &u64| -> Number {
    return match a {
        Number::Int(aa) => Number::Int(aa >> b),
        _ => panic!("Cannot apply bitwise operators to floats")
    };
});

/*
    ╒════════════════════════════╕
    │ Basic assignment operators │
    ╘════════════════════════════╛
*/

macro_rules! generic_assignment_op_definition {
    ($op_assign: tt, $op: tt) => {
        impl_op_ex!($op_assign |a: &mut Number, b: &Number| { *a = &*a $op b; });
    };
}

generic_assignment_op_definition!(+=, +);
generic_assignment_op_definition!(-=, -);
generic_assignment_op_definition!(*=, *);
generic_assignment_op_definition!(/=, /);
generic_assignment_op_definition!(|=, |);
generic_assignment_op_definition!(&=, &);
generic_assignment_op_definition!(^=, ^);

impl_op_ex!(<<= |a: &mut Number, b: &u64| { *a = &*a << b; });
impl_op_ex!(>>= |a: &mut Number, b: &u64| { *a = &*a >> b; });

/*
                                                  ╒═════════╕
    ============================================= │  TESTS  │ =============================================
                                                  ╘═════════╛
*/

#[cfg(test)]
mod tests {
    use crate::number::*;
    use num_bigint::{BigInt};

    use rand::{
        distributions::{
            Distribution, 
            Uniform
        }
    };

    #[test]
    fn parsing_and_displaying() {
        for _i in 0..1000{
            let mut rng = rand::thread_rng();
            let distribution = Uniform::from(0..10);

            let mut string = "".to_string();

            for _j in 0..100{
                let digit = distribution.sample(&mut rng).to_string().bytes().nth(0).unwrap() as char;

                if string.len() > 0 || digit != '0'{
                    string.push(digit);
                }
            }

            if distribution.sample(&mut rng) < 5{
                string.insert(0, '-');
            }

            let i = Integer::from(string.as_str());

            assert_eq!(string, i.to_string());
        }
    }
    
    #[test]
    fn logic_operations() {
        let mut rng = rand::thread_rng();
        let distribution = Uniform::from(20..100);

        for _ in 0..10000{
            let a = Integer::rand_with_size(distribution.sample(&mut rng), false);
            let b = Integer::rand_with_size(distribution.sample(&mut rng), false);

            let a2 = a.to_string().parse::<BigInt>().unwrap();
            let b2 = b.to_string().parse::<BigInt>().unwrap();

            assert_eq!((&a | &b).to_string(), (&a2 | &b2).to_string());
            assert_eq!((&a & &b).to_string(), (&a2 & &b2).to_string());
            assert_eq!((&a ^ &b).to_string(), (&a2 ^ &b2).to_string());
            assert_eq!((!!&a).to_string(), (!!&a2).to_string());
            assert_eq!((!!&b).to_string(), (!!&b2).to_string());
        }
    }
    
    #[test]
    fn adding_and_subtracting() {
        let mut rng = rand::thread_rng();
        let distribution = Uniform::from(20..100);

        for _ in 0..5000{
            let a = Integer::rand_with_size(distribution.sample(&mut rng), true);
            let b = Integer::rand_with_size(distribution.sample(&mut rng), true);

            let a2 = a.to_string().parse::<BigInt>().unwrap();
            let b2 = b.to_string().parse::<BigInt>().unwrap();

            assert_eq!((&a + &b).to_string(), (&a2 + &b2).to_string());
            assert_eq!((a - b).to_string(), (a2 - b2).to_string());
        }
    }
    
    #[test]
    fn shifts() {
        let mut rng = rand::thread_rng();
        let distribution = Uniform::from(50..100);
        let distribution2 = Uniform::from(100..1000);

        for _ in 0..1000{
            let a = Integer::rand_with_limbs(distribution.sample(&mut rng), false);
            let b: u64 = distribution2.sample(&mut rng);

            let a2 = a.to_string().parse::<BigInt>().unwrap();

            assert_eq!((&a << b).to_string(), (&a2 << b).to_string());
            assert_eq!((a >> b).to_string(), (a2 >> b).to_string());
        }
    }
    
    #[test]
    fn small_divisions() {
        let mut rng = rand::thread_rng();
        let distribution = Uniform::from(20..100);

        for _ in 0..1000{
            let a = Integer::rand_with_limbs(distribution.sample(&mut rng), true);
            let b = Integer::rand_with_limbs(distribution.sample(&mut rng), true);

            let a2 = a.to_string().parse::<BigInt>().unwrap();
            let b2 = b.to_string().parse::<BigInt>().unwrap();

            assert_eq!((a / b).to_string(), (a2 / b2).to_string());
        }
    }
    
    #[test]
    fn medium_divisions() {
        let mut rng = rand::thread_rng();
        let distribution = Uniform::from(100..1000);

        for _ in 0..50{
            let a = Integer::rand_with_limbs(distribution.sample(&mut rng), true);
            let b = Integer::rand_with_limbs(distribution.sample(&mut rng), true);

            let a2 = a.to_string().parse::<BigInt>().unwrap();
            let b2 = b.to_string().parse::<BigInt>().unwrap();

            assert_eq!((a / b).to_string(), (a2 / b2).to_string());
        }
    }
    
    #[test]
    fn big_divisions() {
        let mut rng = rand::thread_rng();
        let distribution = Uniform::from(1000..3000);

        for _ in 0..5{
            let a = Integer::rand_with_limbs(distribution.sample(&mut rng), true);
            let b = Integer::rand_with_limbs(distribution.sample(&mut rng), true);

            let a2 = a.to_string().parse::<BigInt>().unwrap();
            let b2 = b.to_string().parse::<BigInt>().unwrap();

            assert_eq!((a / b).to_string(), (a2 / b2).to_string());
        }
    }

    #[test]
    fn small_mod() {
        let mut rng = rand::thread_rng();
        let distribution = Uniform::from(20..100);

        for _ in 0..1000{
            let a = Integer::rand_with_limbs(distribution.sample(&mut rng), true);
            let b = Integer::rand_with_limbs(distribution.sample(&mut rng), true);

            let a2 = a.to_string().parse::<BigInt>().unwrap();
            let b2 = b.to_string().parse::<BigInt>().unwrap();

            assert_eq!((a % b).to_string(), (a2 % b2).to_string());
        }
    }

    #[test]
    fn medium_mod() {
        let mut rng = rand::thread_rng();
        let distribution = Uniform::from(100..1000);

        for _ in 0..50{
            let a = Integer::rand_with_limbs(distribution.sample(&mut rng), true);
            let b = Integer::rand_with_limbs(distribution.sample(&mut rng), true);

            let a2 = a.to_string().parse::<BigInt>().unwrap();
            let b2 = b.to_string().parse::<BigInt>().unwrap();

            assert_eq!((a % b).to_string(), (a2 % b2).to_string());
        }
    }

    #[test]
    fn big_mod() {
        let mut rng = rand::thread_rng();
        let distribution = Uniform::from(1000..3000);

        for _ in 0..5{
            let a = Integer::rand_with_limbs(distribution.sample(&mut rng), true);
            let b = Integer::rand_with_limbs(distribution.sample(&mut rng), true);

            let a2 = a.to_string().parse::<BigInt>().unwrap();
            let b2 = b.to_string().parse::<BigInt>().unwrap();

            assert_eq!((a % b).to_string(), (a2 % b2).to_string());
        }
    }
    
    #[test]
    fn small_multiplications() {
        let mut rng = rand::thread_rng();
        let distribution = Uniform::from(20..100);

        for _ in 0..1000{
            let a = Integer::rand_with_limbs(distribution.sample(&mut rng), true);
            let b = Integer::rand_with_limbs(distribution.sample(&mut rng), true);

            let a2 = a.to_string().parse::<BigInt>().unwrap();
            let b2 = b.to_string().parse::<BigInt>().unwrap();

            assert_eq!((a * b).to_string(), (a2 * b2).to_string());
        }
    }
    
    #[test]
    fn medium_multiplications() {
        let mut rng = rand::thread_rng();
        let distribution = Uniform::from(100..1000);

        for _ in 0..50{
            let a = Integer::rand_with_limbs(distribution.sample(&mut rng), true);
            let b = Integer::rand_with_limbs(distribution.sample(&mut rng), true);

            let a2 = a.to_string().parse::<BigInt>().unwrap();
            let b2 = b.to_string().parse::<BigInt>().unwrap();

            assert_eq!((a * b).to_string(), (a2 * b2).to_string());
        }
    }
    
    #[test]
    fn big_multiplications() {
        let mut rng = rand::thread_rng();
        let distribution = Uniform::from(1000..3000);

        for _ in 0..5{
            let a = Integer::rand_with_limbs(distribution.sample(&mut rng), true);
            let b = Integer::rand_with_limbs(distribution.sample(&mut rng), true);

            let a2 = a.to_string().parse::<BigInt>().unwrap();
            let b2 = b.to_string().parse::<BigInt>().unwrap();

            assert_eq!((a * b).to_string(), (a2 * b2).to_string());
        }
    }
}