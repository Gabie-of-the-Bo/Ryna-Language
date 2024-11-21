use std::{cell::RefCell, ffi::c_void, fs::File, path::PathBuf};

use crate::{compilation::message_and_exit, context::RynaContext, integer_ext::to_i64, mut_cell::MutCell, types::{Type, ARR_ID, ARR_IT_ID, BOOL, BOOL_ID, FILE, FILE_ID, FLOAT, FLOAT_ID, INT, INT_ID, LIB, LIB_FUNC, LIB_FUNC_ID, LIB_ID, PTR, PTR_ID, STR, STR_ID}, ARR_IT_OF, ARR_OF};
use libloading::{Library, Symbol};
use malachite::Integer;
use rclite::Rc;
use rynaffi::{FFIArgs, FFIReturn, FFIValue, RynaFFIFunction};
use serde::{Deserialize, Serialize};

type DataBlock = Rc<MutCell<ObjectBlock>>;

/*
                                                  ╒══════════════════╕
    ============================================= │  IMPLEMENTATION  │ =============================================
                                                  ╘══════════════════╛
*/

#[derive(Clone, PartialEq, Debug)]
pub struct RynaPointer {
    pub ptr: *const c_void
}

impl RynaPointer {
    pub fn new(ptr: *const c_void) -> RynaPointer {
        RynaPointer { ptr }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct RynaArray {
    pub elements: Vec<Object>,
    pub elem_type: Box<Type>
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct RynaTuple {
    pub elements: Vec<Object>,
    pub elem_types: Vec<Type>
}

#[derive(Clone, PartialEq, Debug)]
pub struct RynaArrayIt {
    pub pos: usize,
    pub block: DataBlock,
    pub it_type: Box<Type>,
    pub c_type: Box<Type>
}

impl RynaArrayIt {
    pub fn get_container_ref(&self) -> Object {
        Object {
            inner: self.block.clone()
        }.get_ref()
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct RynaLambda {
    pub loc: usize,
    pub captures: Vec<Object>,
    pub args_type: Box<Type>,
    pub ret_type: Box<Type>
}

#[derive(Clone, Debug)]
pub struct RynaFile {
    pub path: PathBuf,
    pub file: Option<Rc<RefCell<File>>>
}

impl PartialEq for RynaFile {
    fn eq(&self, other: &Self) -> bool {
        match (&self.file, &other.file) {
            (None, None) => self.path == other.path,
            (Some(a), Some(b)) => self.path == other.path && Rc::ptr_eq(a, b),

            _ => false
        }
    }
}

impl RynaFile {
    pub fn is_open(&self) -> bool {
        self.file.is_some()
    }

    pub fn close(&mut self) -> Result<(), String> {
        if !self.is_open() {
            return Err(format!("File at {} is already closed", self.path.to_str().unwrap()));
        }

        self.file = None;

        Ok(())
    }

    pub fn open(&mut self, read: bool, write: bool, append: bool) -> Result<(), String> {
        if self.is_open() {
            return Err(format!("File at {} is already open", self.path.to_str().unwrap()));
        }

        let file = std::fs::OpenOptions::new()
            .create(write || append)
            .read(read)
            .write(write)
            .append(append)
            .open(&self.path);

        match file {
            Ok(inner) => {
                self.file = Some(Rc::new(RefCell::new(inner)));
                Ok(())
            },

            Err(_) => Err(format!("Unable to open file file at {}", self.path.to_str().unwrap()))
        }    
    }

    pub fn exists(&self) -> Result<bool, String> {
        Ok(self.path.is_file())
    }

    pub fn delete(&mut self) -> Result<bool, String> {
        if !self.is_open() {
            return Err(format!("File at {} is closed", self.path.to_str().unwrap()));
        }

        if std::fs::remove_file(&self.path).is_ok() {
            self.file = None;
            Ok(true)
        
        } else {
            Ok(false)
        }
    }
}

#[derive(Clone, Debug)]
pub struct RynaLibrary {
    pub path: PathBuf,
    pub lib: Rc<Library>
}

impl PartialEq for RynaLibrary {
    fn eq(&self, other: &Self) -> bool {
        return self.path == other.path;
    }
}

impl RynaLibrary {
    pub fn new(path: &String, lib: Library) -> RynaLibrary {
        RynaLibrary { 
            path: PathBuf::from(path), 
            lib: Rc::new(lib) 
        }
    }

    pub fn get_function(&self, name: &String) -> Result<RynaLibraryFunction, String> {
        let sym: Symbol<RynaFFIFunction> = unsafe { self.lib.get(name.as_bytes()).map_err(|_| format!("Unable to open symbol {name}"))? };

        Ok(RynaLibraryFunction { 
            path: format!("{}:{}", self.path.to_str().unwrap(), name), 
            func: *sym
        })
    }
}

#[derive(Clone, Debug)]
pub struct RynaLibraryFunction {
    pub path: String,
    pub func: RynaFFIFunction
}

impl PartialEq for RynaLibraryFunction {
    fn eq(&self, other: &Self) -> bool {
        return self.path == other.path;
    }
}

impl RynaLibraryFunction {
    pub fn call(&self, args: &[Object]) -> Result<Object, String> {
        let args_l = args.iter().map(Object::to_ffi).collect::<Result<Vec<_>, _>>()?;
        let args_struct = FFIArgs::new(&args_l);
        let mut out = FFIReturn::Void;
        
        unsafe { (self.func)(&args_struct, &mut out); }

        match out {
            FFIReturn::Value(v) => match v {
                FFIValue::Int(i) => Ok(Object::new(Integer::from(i))),
                FFIValue::Float(i) => Ok(Object::new(i)),
                FFIValue::Pointer(p) => Ok(Object::new(RynaPointer::new(p))),
            },
            FFIReturn::Void => Ok(Object::empty()),
        }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct TypeInstance {
    pub id: usize,
    pub params: Vec<Type>,
    pub attributes: Vec<Object>
}

#[derive(Clone, PartialEq, Debug, Default, Serialize, Deserialize)]
pub enum ObjectBlock {
    #[default]
    NoValue, //  Empty is a type, this represents no value at all

    Empty,
    Int(malachite::Integer),
    Float(f64),
    Str(String),
    Bool(bool),

    Tuple(RynaTuple),
    Array(RynaArray),

    #[serde(skip)]
    ArrayIter(RynaArrayIt),

    Lambda(RynaLambda),

    #[serde(skip)]
    Pointer(RynaPointer),

    #[serde(skip)]
    File(RynaFile),

    #[serde(skip)]
    Library(RynaLibrary),

    #[serde(skip)]
    LibraryFunction(RynaLibraryFunction),

    Instance(TypeInstance),

    #[serde(skip)]
    Ref(DataBlock),
    #[serde(skip)]
    Mut(DataBlock),
}

impl Eq for ObjectBlock {}

impl ObjectBlock {
    #[inline(always)]
    pub fn to_obj(self) -> Object {
        Object { inner: Rc::new(MutCell::new(self)) }
    } 

    pub fn get_type_id(&self) -> usize {
        match self {
            ObjectBlock::NoValue => message_and_exit("Accessing moved object".into()),
            ObjectBlock::Empty => 0,
            ObjectBlock::Int(_) => INT_ID,
            ObjectBlock::Float(_) => FLOAT_ID,
            ObjectBlock::Str(_) => STR_ID,
            ObjectBlock::Bool(_) => BOOL_ID,
            ObjectBlock::Tuple(_) => 0,
            ObjectBlock::Array(_) => ARR_ID,
            ObjectBlock::ArrayIter(_) => ARR_IT_ID,
            ObjectBlock::Lambda(_) => 0,
            ObjectBlock::Pointer(_) => PTR_ID,
            ObjectBlock::File(_) => FILE_ID,
            ObjectBlock::Library(_) => LIB_ID,
            ObjectBlock::LibraryFunction(_) => LIB_FUNC_ID,
            ObjectBlock::Instance(i) => i.id,
            ObjectBlock::Ref(_) => 0,
            ObjectBlock::Mut(_) => 0,
        }
    }

    pub fn get_type(&self) -> Type {
        return match self {
            ObjectBlock::NoValue => message_and_exit("Accessing moved object".into()),
            ObjectBlock::Empty => Type::Empty,
            ObjectBlock::Int(_) => INT,
            ObjectBlock::Float(_) => FLOAT,
            ObjectBlock::Str(_) => STR,
            ObjectBlock::Bool(_) => BOOL,
            ObjectBlock::Tuple(t) => Type::And(t.elem_types.clone()),
            ObjectBlock::Array(a) => ARR_OF!(*a.elem_type.clone()),
            ObjectBlock::ArrayIter(i) => ARR_IT_OF!(*i.c_type.clone(), *i.it_type.clone()),
            ObjectBlock::Lambda(l) => Type::Function(l.args_type.clone(), l.ret_type.clone()),
            ObjectBlock::Pointer(_) => PTR,
            ObjectBlock::File(_) => FILE,
            ObjectBlock::Library(_) => LIB,
            ObjectBlock::LibraryFunction(_) => LIB_FUNC,
            ObjectBlock::Instance(i) => if i.params.is_empty() { Type::Basic(i.id) } else { Type::Template(i.id, i.params.clone()) },
            ObjectBlock::Ref(r) => Type::Ref(Box::new(r.borrow().get_type())),
            ObjectBlock::Mut(r) => Type::MutRef(Box::new(r.borrow().get_type())),
        };
    }

    pub fn is_moved(&self) -> bool {
        matches!(self, ObjectBlock::NoValue)
    }

    pub fn get_inner<T>(&self) -> &T where ObjectBlock: Get<T> {
        return Get::<T>::get(self);
    }

    pub fn mut_inner<T>(&mut self) -> &mut T where ObjectBlock: GetMut<T> {
        return GetMut::<T>::get(self);
    }

    pub fn dereference(&self) -> &DataBlock {
        if let ObjectBlock::Ref(n) | ObjectBlock::Mut(n) = self {
            return n;
        }

        unreachable!();
    }

    pub fn assign_ref(&mut self, other: ObjectBlock, ctx: &RynaContext) -> Result<(), String> {
        self.dereference().borrow_mut().assign(other, ctx)
    }

    pub fn assign(&mut self, other: ObjectBlock, ctx: &RynaContext) -> Result<(), String> {
        use ObjectBlock::*;

        match (self, other) {
            (Int(a), Int(b)) => *a = b,
            (Float(a), Float(b)) => *a = b,
            (Str(a), Str(b)) => *a = b,
            (Bool(a), Bool(b)) => *a = b,
            (Array(a), Array(b)) if a.elem_type == b.elem_type => *a = b,
            (ArrayIter(a), ArrayIter(b)) if a.it_type == b.it_type => *a = b,
            (Lambda(a), Lambda(b)) if a.args_type == b.args_type && a.ret_type == b.ret_type => *a = b,
            (Instance(a), Instance(b)) if a.id == b.id && a.params == b.params => *a = b,
            (Tuple(a), Tuple(b)) if a.elem_types == b.elem_types => *a = b,

            (a, b) => return Err(format!(
                "Unable to assign value of type {} to block of type {}", 
                b.get_type().get_name(ctx),
                a.get_type().get_name(ctx)
            ))
        };

        Ok(())
    }

    pub fn deep_clone(&self) -> Self {
        return match self {
            ObjectBlock::NoValue => message_and_exit("Accessing moved object".into()),

            ObjectBlock::Empty => ObjectBlock::Empty,
            ObjectBlock::Int(n) => ObjectBlock::Int(n.clone()),
            ObjectBlock::Float(n) => ObjectBlock::Float(*n),
            ObjectBlock::Str(s) => ObjectBlock::Str(s.clone()),
            ObjectBlock::Bool(b) => ObjectBlock::Bool(*b),
            ObjectBlock::Tuple(t) => ObjectBlock::Tuple(RynaTuple { 
                elements: t.elements.iter().map(Object::deep_clone).collect(), 
                elem_types: t.elem_types.clone()
            }),
            ObjectBlock::Array(a) => ObjectBlock::Array(RynaArray { 
                elements: a.elements.iter().map(Object::deep_clone).collect(), 
                elem_type: a.elem_type.clone()
            }),
            ObjectBlock::ArrayIter(i) => ObjectBlock::ArrayIter(RynaArrayIt { 
                pos: i.pos, 
                block: i.block.clone(), 
                it_type: i.it_type.clone(), 
                c_type: i.c_type.clone() 
            }),
            ObjectBlock::Lambda(l) => ObjectBlock::Lambda(RynaLambda { 
                loc: l.loc, 
                captures: l.captures.iter().map(Object::deep_clone).collect(),
                args_type: l.args_type.clone(), 
                ret_type: l.ret_type.clone() 
            }),
            ObjectBlock::Pointer(p) => ObjectBlock::Pointer(p.clone()),
            ObjectBlock::File(f) => ObjectBlock::File(f.clone()),
            ObjectBlock::Library(l) => ObjectBlock::Library(l.clone()),
            ObjectBlock::LibraryFunction(f) => ObjectBlock::LibraryFunction(f.clone()),
            ObjectBlock::Instance(i) => ObjectBlock::Instance(TypeInstance {
                id: i.id, 
                params: i.params.clone(), 
                attributes: i.attributes.iter().map(Object::deep_clone).collect()
            }),
            ObjectBlock::Ref(r) => ObjectBlock::Ref(r.clone()),
            ObjectBlock::Mut(r) => ObjectBlock::Mut(r.clone()),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Object {
    pub inner: DataBlock
}

impl Serialize for Object {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer {
        self.inner.borrow().serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for Object {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de> {
        let inner = ObjectBlock::deserialize(deserializer)?;
        Ok(inner.to_obj())
    }
}

impl Object {
    pub fn new<T: RynaData>(data: T) -> Self {
        data.data().to_obj()
    }

    pub fn get_ptr(&self) -> *mut ObjectBlock {
        (*self.inner).as_ptr()
    }

    pub fn arr(elements: Vec<Object>, elem_type: Type) -> Self {
        ObjectBlock::Array(RynaArray { elements, elem_type: Box::new(elem_type) }).to_obj()
    }

    pub fn arr_it(c_type: Type, it_type: Type, block: DataBlock, pos: usize) -> Self {
        ObjectBlock::ArrayIter(RynaArrayIt { pos, block, c_type: Box::new(c_type), it_type: Box::new(it_type) }).to_obj()
    }

    pub fn lambda(loc: usize, captures: Vec<Object>, args_type: Type, ret_type: Type) -> Self {
        ObjectBlock::Lambda(RynaLambda { loc, captures, args_type: Box::new(args_type), ret_type: Box::new(ret_type) }).to_obj()
    }

    pub fn tuple(elements: Vec<Object>, elem_types: Vec<Type>) -> Self {
        ObjectBlock::Tuple(RynaTuple { elements, elem_types }).to_obj()
    }

    pub fn file(path: PathBuf) -> Self {
        ObjectBlock::File(RynaFile { path, file: None }).to_obj()
    }

    pub fn instance(attributes: Vec<Object>, params: Vec<Type>, id: usize) -> Self {
        ObjectBlock::Instance(TypeInstance { params, attributes, id }).to_obj()
    }

    pub fn get<T>(&self) -> &T where ObjectBlock: Get<T> {
        Get::<T>::get(self.inner.borrow())
    }

    #[allow(clippy::should_implement_trait)]
    pub fn deref<T>(&self) -> &mut T where ObjectBlock: Deref<T> + GetMut<T> {
        Deref::<T>::deref(self.inner.borrow())
    }

    pub fn ref_count(&self) -> usize {
        Rc::strong_count(&self.inner)
    }

    pub fn deref_ref_count(&self) -> usize {
        match &mut *self.inner.borrow_mut() {
            ObjectBlock::Mut(i) | ObjectBlock::Ref(i) => Rc::strong_count(&i),
            _ => unreachable!()
        }
    }

    pub fn is_moved(&self) -> bool {
        return self.inner.borrow().is_moved();
    }

    pub fn is_moved_deref(&self) -> bool {
        match &mut *self.inner.borrow_mut() {
            ObjectBlock::Mut(i) | ObjectBlock::Ref(i) => i.borrow().is_moved(),
            _ => unreachable!()
        }
    }

    pub fn to_ffi(&self) -> Result<FFIValue, String> {
        match self.inner.borrow() {
            ObjectBlock::NoValue => message_and_exit("Accessing moved object".into()),
            
            ObjectBlock::Bool(v) => Ok(FFIValue::Int(if *v { 1 } else { 0 })),
            ObjectBlock::Int(i) => Ok(FFIValue::Int(to_i64(&i))),
            ObjectBlock::Float(i) => Ok(FFIValue::Float(*i)),
            ObjectBlock::Str(s) => Ok(FFIValue::Pointer((s as *const String) as *const c_void)),
            ObjectBlock::Pointer(p) => Ok(FFIValue::Pointer(p.ptr)),
            
            _ => Err(format!("Unable to tranfer object to FFI layer"))
        }
    }

    /*
        Moves self contents to the returned value and leaves Moved
    */
    pub fn move_contents(&self) -> Object {
        let res = ObjectBlock::default().to_obj();

        match &mut *self.inner.borrow_mut() {
            ObjectBlock::Mut(i) => std::mem::swap(&mut *i.borrow_mut(), &mut *res.inner.borrow_mut()),
            _ => unreachable!()
        };

        res
    }
    
    pub fn move_contents_if_ref(&self) -> Object {
        let res = ObjectBlock::default().to_obj();

        match &mut *self.inner.borrow_mut() {
            ObjectBlock::Mut(i) => std::mem::swap(&mut *i.borrow_mut(), &mut *res.inner.borrow_mut()),
            i => std::mem::swap(i, &mut *res.inner.borrow_mut())
        };

        res
    }

    pub fn swap_contents(&self, other: &Object) {
        match (&mut *self.inner.borrow_mut(), &mut *other.inner.borrow_mut()) {
            (ObjectBlock::Mut(a), ObjectBlock::Mut(b)) => std::mem::swap(&mut *a.borrow_mut(), &mut *b.borrow_mut()),
            _ => unreachable!()
        };
    }

    pub fn drop_contents(&self) {
        match &mut *self.inner.borrow_mut() {
            ObjectBlock::Mut(i) => std::mem::take(&mut *i.borrow_mut()),
            _ => unreachable!()
        };
    }
    
    pub fn assign(&self, other_obj: Object, ctx: &RynaContext) -> Result<(), String> {
        match Rc::try_unwrap(other_obj.inner) {
            Ok(inner) => self.inner.borrow_mut().assign_ref(inner.take(), ctx),
            Err(inner) => self.inner.borrow_mut().assign_ref(inner.borrow().clone(), ctx)
        }
    }
    
    pub fn assign_direct(&self, other_obj: Object, ctx: &RynaContext) -> Result<(), String> {
        match Rc::try_unwrap(other_obj.inner) {
            Ok(inner) => self.inner.borrow_mut().assign(inner.take(), ctx),
            Err(inner) => self.inner.borrow_mut().assign(inner.borrow().clone(), ctx)
        }
    }

    pub fn from_inner(inner: DataBlock) -> Self {
        Object { inner }
    }

    pub fn no_value() -> Object {
        ObjectBlock::NoValue.to_obj()
    }

    pub fn empty() -> Object {
        ObjectBlock::Empty.to_obj()
    }

    pub fn get_ref(&self) -> Object {
        return match self.inner.borrow() {
            ObjectBlock::Ref(i) |
            ObjectBlock::Mut(i) => ObjectBlock::Ref(i.clone()).to_obj(),

            _ => ObjectBlock::Ref(self.inner.clone()).to_obj()
        }
    }

    pub fn get_mut(&self) -> Object {
        return match self.inner.borrow() {
            ObjectBlock::Ref(i) |
            ObjectBlock::Mut(i) => ObjectBlock::Mut(i.clone()).to_obj(),

            _ => ObjectBlock::Mut(self.inner.clone()).to_obj()
        }
    }

    pub fn get_ref_nostack(&self) -> Object {
        ObjectBlock::Ref(self.inner.clone()).to_obj()
    }

    pub fn get_mut_nostack(&self) -> Object {
        ObjectBlock::Mut(self.inner.clone()).to_obj()
    }

    pub fn to_debug_string(&self) -> String {
        format!("{:?}", self.inner.borrow())
    }

    pub fn deref_if_ref(&self) -> Object {
        return match self.inner.borrow() {
            ObjectBlock::Ref(i) |
            ObjectBlock::Mut(i) => Object::from_inner(i.clone()),

            _ => self.clone()
        }
    }

    pub fn deref_deep_clone(&self) -> Object {
        return match self.inner.borrow() {
            ObjectBlock::Ref(i) |
            ObjectBlock::Mut(i) => i.borrow().deep_clone().to_obj(),

            obj => obj.deep_clone().to_obj()
        }
    }

    pub fn deep_clone(&self) -> Object {
        self.inner.borrow().deep_clone().to_obj()
    }

    pub fn get_type_id(&self) -> usize {
        return self.inner.borrow().get_type_id();
    }

    pub fn get_type(&self) -> Type {
        return self.inner.borrow().get_type();
    }

    pub fn deref_obj(&self) -> Object {
        return match self.inner.borrow() {
            ObjectBlock::Ref(r) | ObjectBlock::Mut(r) => Object::from_inner(r.clone()),
            _ => unreachable!()
        };
    }
}

pub trait RynaData {
    fn data(self) -> ObjectBlock;
}

pub trait Get<T> {
    fn get(&self) -> &T;
}

pub trait GetMut<T> {
    fn get(&mut self) -> &mut T;
}

pub trait Deref<T> {
    #[allow(clippy::mut_from_ref)]
    fn deref(&self) -> &mut T;
}

macro_rules! impl_ryna_data {
    ($t: ty, $v: tt) => {
        impl RynaData for $t {
            #[inline(always)]
            fn data(self) -> ObjectBlock {
                return ObjectBlock::$v(self)
            }
        }

        impl Get<$t> for ObjectBlock {
            #[inline(always)]
            fn get(&self) -> &$t {
                if let ObjectBlock::$v(n) = self {
                    return n;
                }
        
                unreachable!("Unable to get {:?}", self);    
            }
        }

        impl GetMut<$t> for ObjectBlock {
            #[inline(always)]
            fn get(&mut self) -> &mut $t {
                if let ObjectBlock::$v(n) = self {
                    return n;
                }
        
                unreachable!("Unable to get mut {:?}", self);    
            }
        }

        impl Deref<$t> for ObjectBlock {
            #[inline(always)]
            fn deref(&self) -> &mut $t {
                return GetMut::<$t>::get(self.dereference().borrow_mut());
            }
        }
    };
}

impl_ryna_data!(Integer, Int);
impl_ryna_data!(f64, Float);
impl_ryna_data!(String, Str);
impl_ryna_data!(bool, Bool);
impl_ryna_data!(TypeInstance, Instance);
impl_ryna_data!(RynaArray, Array);
impl_ryna_data!(RynaTuple, Tuple);
impl_ryna_data!(RynaLambda, Lambda);
impl_ryna_data!(RynaArrayIt, ArrayIter);
impl_ryna_data!(RynaPointer, Pointer);
impl_ryna_data!(RynaFile, File);
impl_ryna_data!(RynaLibrary, Library);
impl_ryna_data!(RynaLibraryFunction, LibraryFunction);

/*
                                                  ╒═════════╕
    ============================================= │  TESTS  │ =============================================
                                                  ╘═════════╛
*/

#[cfg(test)]
mod tests {
    use crate::context::standard_ctx;
    use malachite::Integer;
    use crate::object::*;

    #[test]
    fn object_construction() {
        let number = Object::new(Integer::from(10));
        
        assert_eq!(*number.get::<Integer>(), Integer::from(10));

        let string = Object::new(String::from("Test"));
        
        assert_eq!(*string.get::<String>(), "Test".to_string());

        assert_ne!(number.get_type_id(), string.get_type_id());
    }

    #[test]
    fn references() {
        let ctx = standard_ctx();

        let number = Object::new(Integer::from(10));
        
        assert_eq!(*number.get::<Integer>(), Integer::from(10));

        let reference = number.get_mut();
        let ref_of_ref = reference.get_ref();

        assert_eq!(*reference.deref::<Integer>(), Integer::from(10));
        assert_eq!(*ref_of_ref.deref::<Integer>(), Integer::from(10));

        assert_ne!(number.get_ptr(), reference.get_ptr());
        assert_ne!(number.get_ptr(), ref_of_ref.get_ptr());
        assert_ne!(reference.get_ptr(), ref_of_ref.get_ptr());
        assert_eq!(number.get_ptr(), (**reference.inner.borrow().dereference()).as_ptr());
        assert_eq!(number.get_ptr(), (**ref_of_ref.inner.borrow().dereference()).as_ptr());

        {
            *reference.deref::<Integer>() += Integer::from(5);    
        }

        assert_eq!(*number.get::<Integer>(), Integer::from(15));
        assert_eq!(*reference.deref::<Integer>(), Integer::from(15));
        assert_eq!(*ref_of_ref.deref::<Integer>(), Integer::from(15));

        reference.assign(ObjectBlock::Int(Integer::from(20)).to_obj(), &ctx).unwrap();

        assert_eq!(*number.get::<Integer>(), Integer::from(20));
        assert_eq!(*reference.deref::<Integer>(), Integer::from(20));
        assert_eq!(*ref_of_ref.deref::<Integer>(), Integer::from(20));
    }

    #[test]
    fn value_moving() {
        let number = Object::new(Integer::from(10));
        
        assert_eq!(*number.get::<Integer>(), Integer::from(10));

        let reference = number.get_mut();
        let ref_of_ref = reference.get_mut();

        assert_ne!(number.get_ptr(), reference.get_ptr());
        assert_ne!(number.get_ptr(), ref_of_ref.get_ptr());
        assert_ne!(reference.get_ptr(), ref_of_ref.get_ptr());
        assert_eq!(number.get_ptr(), (**reference.inner.borrow().dereference()).as_ptr());
        assert_eq!(number.get_ptr(), (**ref_of_ref.inner.borrow().dereference()).as_ptr());

        assert_eq!(number.ref_count(), 3);

        let number_2 = reference.move_contents();

        assert!(number.is_moved());
        assert!(reference.deref_obj().is_moved());
        assert!(ref_of_ref.deref_obj().is_moved());
        assert_eq!(number.ref_count(), 3);

        assert!(!number_2.is_moved());
        assert_eq!(number_2.ref_count(), 1);
        assert_eq!(*number_2.get::<Integer>(), Integer::from(10));
    }
}