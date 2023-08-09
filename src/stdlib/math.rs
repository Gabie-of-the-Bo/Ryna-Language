use rand::Rng;
use std::cmp::Ordering;

use crate::number::*;

pub fn rand_f64() -> f64 {
    return rand::thread_rng().gen_range(0.0..1.0);
}

impl Integer {
    pub fn fact(&self) -> Result<Integer, String> {
        let one: Integer = Integer::new(false, vec!(1));
        let mut res = one.clone();
        let mut cpy = self.clone();
        
        while !cpy.is_zero() {
            res = res * &cpy;
            cpy = cpy - &one;
        }

        Ok(Integer::from(res))
    }

    /*
                                                      ╒═══════════════════╕
        ============================================= │  AUX MATHEMATICS  │ =============================================
                                                      ╘═══════════════════╛
    */

    pub fn rand_int_range(from: &Integer, to: &Integer) -> Result<Integer, String> {
        let mut rng = rand::thread_rng();

        let mut rand_limbs_range = |f: &Vec<u64>, t: &Vec<u64>| {
            let max_bits = t.len();
            let min_bits = f.len();

            let mut res = vec!(0; max_bits);
            
            // First number
            if max_bits > min_bits {
                res[max_bits - 1] = rng.gen_range(0..=*t.last().unwrap());
            
            } else {
                res[max_bits - 1] = rng.gen_range(*f.last().unwrap()..=*t.last().unwrap());
            }

            let mut zeroed = res[max_bits - 1] == 0;

            // First zeroes
            for i in min_bits..(max_bits - 1) {
                let digit = rng.gen::<u64>();
                zeroed &= digit == 0;

                res[i] = digit;
            }

            // Next zeroes
            if zeroed {
                for i in 0..(min_bits - 1) {
                    res[i] = rng.gen_range(f[i]..=u64::MAX);
                }

            } else {
                for i in 0..(min_bits - 1) {
                    res[i] = rng.gen::<u64>();
                }
            }

            return res;
        };

        if from >= to {
            return Err(format!("Invalid range for random generation [{}, {}]", String::from(from), String::from(to)));
        }

        return match (from.negative, to.negative) {
            (false, false) => Ok(Integer::from(Integer::new(false, rand_limbs_range(&from.limbs, &to.limbs)))),
            (true, true) => Ok(Integer::from(Integer::new(true, rand_limbs_range(&to.limbs, &from.limbs)))),
            (true, false) => {
                let limbs;

                if let Ordering::Less = comp_limbs(&from.limbs, &to.limbs) {
                    limbs = rand_limbs_range(&vec!(0), &to.limbs);

                } else {
                    limbs = rand_limbs_range(&vec!(0), &from.limbs);
                }

                let mut res = Integer::new(false, limbs);

                // Set as negative uniformly
                if &res > to {
                    res.negative = true;
                    
                } else if &res <= from {
                    res.negative = rng.gen::<bool>();
                }

                Ok(Integer::from(res))
            },

            _ => unreachable!()
        };
    }
}

/*
                                                  ╒═════════╕
    ============================================= │  TESTS  │ =============================================
                                                  ╘═════════╛
*/

#[cfg(test)]
mod tests {
    use crate::number::*;
    use num_bigint::BigInt;

    use rand::distributions::{
            Distribution, 
            Uniform
        };

    #[test]
    fn integer_exponentiation() {
        let mut rng = rand::thread_rng();
        let distribution = Uniform::from(1..5);

        for _ in 0..100{
            let a = Integer::rand_with_size(distribution.sample(&mut rng), false);
            let b = Integer::rand_with_size(distribution.sample(&mut rng), false);

            let a2 = a.to_string().parse::<BigInt>().unwrap();

            assert_eq!(pow(&a, &b).unwrap().to_string(), a2.pow(b.limbs[0] as u32).to_string());
        }
    }

    /*
    TODO: check this
    #[test]
    fn integer_modular_exponentiation() {
        let mut rng = rand::thread_rng();
        let distribution = Uniform::from(1..30);

        for _ in 0..10000{
            let a = Integer::rand_with_size(distribution.sample(&mut rng), false);
            let mut b = Integer::rand_with_size(distribution.sample(&mut rng), false);
            let mut m = Integer::rand_with_size(distribution.sample(&mut rng), false);

            while b.is_zero() {
                b = Integer::rand_with_size(distribution.sample(&mut rng), false);
            }

            while m.is_zero() {
                m = Integer::rand_with_size(distribution.sample(&mut rng), false);
            }

            println!("{}, {}, {}", a, b, m);

            let a2 = a.to_string().parse::<BigInt>().unwrap();
            let b2 = b.to_string().parse::<BigInt>().unwrap();
            let m2 = m.to_string().parse::<BigInt>().unwrap();

            assert_eq!(modpow(&a, &b, &m).unwrap().to_string(), a2.modpow(&b2, &m2).to_string());
        }
    }*/
}