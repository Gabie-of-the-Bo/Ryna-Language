use std::{ffi::{c_double, c_uint, c_void}, os::raw::c_longlong};

#[repr(C)]
pub enum FFIValue {
    Int(c_longlong),
    Float(c_double),
    Pointer(*const c_void)
}

#[repr(C)]
pub enum FFIReturn {
    Value(FFIValue),
    Void
}

#[repr(C)]
pub struct FFIArgs {
    args: *const FFIValue,
    count: c_uint
}

pub type RynaFFIFunction = unsafe extern fn(*const FFIArgs, *mut FFIReturn);

impl FFIValue {
    pub fn as_i64(&self) -> i64 {
        return match self {
            FFIValue::Int(i) => *i as i64,
            FFIValue::Float(_) => unreachable!(),
            FFIValue::Pointer(_) => unreachable!(),
        };
    }

    pub fn as_bool(&self) -> bool {
        return self.as_i64() != 0;
    }

    pub fn as_f64(&self) -> f64 {
        return match self {
            FFIValue::Int(_) => unreachable!(),
            FFIValue::Float(i) => *i as f64,
            FFIValue::Pointer(_) => unreachable!(),
        };
    }

    pub fn as_ptr(&self) -> *const c_void {
        return match self {
            FFIValue::Int(_) => unreachable!(),
            FFIValue::Float(_) => unreachable!(),
            FFIValue::Pointer(i) => *i,
        };
    }
}

impl FFIReturn {
    pub fn has_value(&self) -> bool {
        return matches!(self, FFIReturn::Value(_));
    }

    pub fn as_value(&self) -> &FFIValue {
        return match self {
            FFIReturn::Value(i) => i,
            FFIReturn::Void => unreachable!(),
        };
    }

    pub fn as_i64(&self) -> i64 {
        return self.as_value().as_i64();        
    }

    pub fn as_bool(&self) -> bool {
        return self.as_value().as_bool();        
    }

    pub fn as_f64(&self) -> f64 {
        return self.as_value().as_f64();        
    }

    pub fn as_ptr(&self) -> *const c_void {
        return self.as_value().as_ptr();    
    }
}

impl FFIArgs {
    pub fn new(args: &[FFIValue]) -> Self {
        Self {
            args: args.as_ptr(),
            count: args.len() as c_uint,
        }
    }

    pub fn as_slice(&self) -> &[FFIValue] {
        return unsafe { std::slice::from_raw_parts(self.args, self.count as usize) };
    }
}

impl From<i64> for FFIReturn {
    fn from(value: i64) -> Self {
        FFIReturn::Value(FFIValue::Int(value))
    }
}

impl From<f64> for FFIReturn {
    fn from(value: f64) -> Self {
        FFIReturn::Value(FFIValue::Float(value))
    }
}

impl From<*const c_void> for FFIReturn {
    fn from(value: *const c_void) -> Self {
        FFIReturn::Value(FFIValue::Pointer(value))
    }
}

macro_rules! ryna_ffi_function {
    ($name:ident ( $in: ident, $out: ident ) { $($body:tt)* }) => {
        #[no_mangle]
        fn $name(__args_ptr: *const FFIArgs, $out: *mut FFIReturn) {
            let $in = (unsafe { &*__args_ptr }).as_slice();
            
            $($body)*
        }
    };
}