use std::any::*;
use std::hash::Hash;
use std::rc::Rc;
use std::cell::*;

use crate::number::Integer;
use crate::{types::*, ARR_OF, ARR_IT_OF};

/*
                                                  ╒══════════════════╕
    ============================================= │  IMPLEMENTATION  │ =============================================
                                                  ╘══════════════════╛
*/

/*
    ╔═════════════════════════════╗
    ║     OBJECT TYPE ERASURE     ║
    ╠═════════════════════════════╣
    ║ Nessa's type system allows  ║
    ║ the use of "gradual typing" ║
    ║ so a type erasure object    ║
    ║ struct and trait are needed ║
    ╚═════════════════════════════╝
*/

pub trait NessaObject {
    fn get_type_id(&self) -> usize;
    fn get_type(&self) -> Type {
        return Type::Basic(self.get_type_id());
    }

    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;
    fn deep_clone(&self) -> Rc<RefCell<dyn NessaObject>>;
    fn to_string(&self) -> String;
    fn equal_to(&self, b: &dyn NessaObject) -> bool;

    fn assign(&mut self, other: Object);
}

#[derive(Clone, Debug)]
pub struct Object {
    pub inner: Rc<RefCell<dyn NessaObject>>
}

impl Hash for Object {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.to_string().hash(state);
    }
}

impl std::fmt::Debug for dyn NessaObject {
    fn fmt(&self, out: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        out.write_str(self.to_string().as_str()).unwrap();

        return Ok(());
    }
}

impl PartialEq for Object {
    fn eq(&self, b: &Object) -> bool {
        return self.inner.borrow().equal_to(&*b.inner.borrow());
    }
}

impl Eq for Object {}

impl Object {
    pub fn new<T>(inner: T) -> Object where T: NessaObject + 'static {
        return Object {
            inner: Rc::new(RefCell::new(inner))
        }
    }

    pub fn deep_clone(&self) -> Object {
        return Object {
            inner: self.inner.borrow().deep_clone()
        };
    }

    pub fn get_type_id(&self) -> usize {
        return self.inner.borrow().get_type_id();
    }

    pub fn get_type(&self) -> Type {
        return self.inner.borrow().get_type();
    }

    pub fn get_ptr(&self) -> *const dyn NessaObject{
        return self.inner.as_ptr();
    }

    pub fn deref<T>(&self) -> Ref<T> where T: 'static {
        return Ref::map(self.inner.borrow(), |i| {
            let ptr = i.as_any().downcast_ref::<Reference>().unwrap().get_ptr();

            // SAFETY: The value will not be dropped because self contains the Reference
            // and this object contains the inner object that we are returning. All these
            // are reference counted, so there is no problem.
            unsafe { (*ptr).as_any().downcast_ref::<T>().unwrap() }
        })
    }

    pub fn deref_obj(&self) -> Object {
        return Object {
            inner: self.get::<Reference>().inner.clone()
        }    
    }

    pub fn get<T>(&self) -> Ref<T> where T: 'static {
        return Ref::map(self.inner.borrow(), |i| i.as_any().downcast_ref::<T>().unwrap());
    }

    pub fn get_mut<T>(&mut self) -> RefMut<T> where T: 'static {
        return RefMut::map(self.inner.borrow_mut(), |i| i.as_any_mut().downcast_mut::<T>().unwrap());
    }

    pub fn get_ref(&self) -> Reference {
        return Reference {
            inner: if self.is_ref() { self.get::<Reference>().inner.clone() } else { self.inner.clone() },
            mutable: false
        }
    }

    pub fn get_ref_mut(&self) -> Reference {
        return Reference {
            inner: if self.is_ref() { 
                let reference = self.get::<Reference>();

                assert!(reference.mutable, "Cannot take mutable reference from constant reference");

                reference.inner.clone() 

            } else { 
                self.inner.clone() 
            },

            mutable: true
        }
    }

    pub fn get_ref_obj(&self) -> Object {
        return Object::new(self.get_ref());
    }

    pub fn get_ref_mut_obj(&self) -> Object {
        return Object::new(self.get_ref_mut());
    }

    pub fn is_ref(&self) -> bool {
        return self.inner.borrow().as_any().type_id() == TypeId::of::<Reference>();
    }

    pub fn to_string(&self) -> String {
        return self.inner.borrow().to_string();
    }

    pub fn empty() -> Object {
        return Object::new(());
    }
}

/*
    ╒═════════════════════════════════════════════╕
    │ Reference struct for high level indirection │
    ╘═════════════════════════════════════════════╛
*/

#[derive(Clone)]
pub struct Reference {
    pub inner: Rc<RefCell<dyn NessaObject>>,
    pub mutable: bool
}

impl PartialEq for Reference {
    fn eq(&self, b: &Reference) -> bool {
        return self.get_ptr() == b.get_ptr();
    }
}


impl Reference {
    pub fn get<T>(&self) -> Ref<T> where T: 'static {
        return Ref::map(self.inner.borrow(), |i| i.as_any().downcast_ref::<T>().unwrap());
    }

    pub fn get_mut<T>(&self) -> RefMut<T> where T: 'static {
        assert!(self.mutable, "Cannot take mutable data from constant reference");

        return RefMut::map(self.inner.borrow_mut(), |i| i.as_any_mut().downcast_mut::<T>().unwrap());
    }

    pub fn get_ptr(&self) -> *const dyn NessaObject{
        return self.inner.as_ptr();
    }

    pub fn assign(&mut self, other: Object) {
        assert!(self.mutable, "Cannot take mutate data from constant reference");

        self.inner.borrow_mut().assign(other);
    }
}

/*
    ╒════════════════════════════════════════════════╕
    │ Basic implementations of the NessaObject trait │
    ╘════════════════════════════════════════════════╛
*/

impl NessaObject for Reference {
    fn get_type_id(&self) -> usize {
        return self.inner.borrow().get_type_id();
    }

    fn get_type(&self) -> Type {
        if self.mutable {
            return Type::MutRef(Box::new(self.inner.borrow().get_type()));

        } else{
            return Type::Ref(Box::new(self.inner.borrow().get_type()));
        }
    }

    fn as_any(&self) -> &dyn Any {
        return self;
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        return self;
    }

    fn deep_clone(&self) -> Rc<RefCell<dyn NessaObject>> {
        return Rc::new(RefCell::new(self.clone()));
    }

    fn to_string(&self) -> String {
        return self.inner.borrow().to_string();
    }

    fn equal_to(&self, b: &dyn NessaObject) -> bool {
        let ta = self.as_any().downcast_ref::<Reference>();
        let tb = b.as_any().downcast_ref::<Reference>();

        if ta.is_some() && tb.is_some() {
            return ta.unwrap().inner.borrow().equal_to(&*tb.unwrap().inner.borrow());
        }

        return false;
    }

    fn assign(&mut self, mut other: Object) {
        // Swapping should be ok, since other will be destroyed after this function
        std::mem::swap(self, &mut other.get_mut::<Self>());
    }
}

impl NessaObject for Integer {
    fn get_type_id(&self) -> usize {
        return INT_ID;
    }

    fn as_any(&self) -> &dyn Any {
        return self;
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        return self;
    }

    fn deep_clone(&self) -> Rc<RefCell<dyn NessaObject>> {
        return Rc::new(RefCell::new(self.clone()));
    }

    fn to_string(&self) -> String {
        return String::from(self);
    }

    fn equal_to(&self, b: &dyn NessaObject) -> bool {
        let ta = self.as_any().downcast_ref::<Integer>();
        let tb = b.as_any().downcast_ref::<Integer>();

        return ta == tb;
    }

    fn assign(&mut self, mut other: Object) {
        // Swapping should be ok, since other will be destroyed after this function
        std::mem::swap(self, &mut other.get_mut::<Self>());
    }
}

impl NessaObject for f64 {
    fn get_type_id(&self) -> usize {
        return FLOAT_ID;
    }

    fn as_any(&self) -> &dyn Any {
        return self;
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        return self;
    }

    fn deep_clone(&self) -> Rc<RefCell<dyn NessaObject>> {
        return Rc::new(RefCell::new(self.clone()));
    }

    fn to_string(&self) -> String {
        return format!("{}", self);
    }

    fn equal_to(&self, b: &dyn NessaObject) -> bool {
        let ta = self.as_any().downcast_ref::<f64>();
        let tb = b.as_any().downcast_ref::<f64>();

        return ta == tb;
    }

    fn assign(&mut self, mut other: Object) {
        // Swapping should be ok, since other will be destroyed after this function
        std::mem::swap(self, &mut other.get_mut::<Self>());
    }
}

impl NessaObject for String {
    fn get_type_id(&self) -> usize {
        return STR_ID;
    }

    fn as_any(&self) -> &dyn Any {
        return self;
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        return self;
    }

    fn deep_clone(&self) -> Rc<RefCell<dyn NessaObject>> {
        return Rc::new(RefCell::new(self.clone()));
    }

    fn to_string(&self) -> String {
        return self.clone();
    }

    fn equal_to(&self, b: &dyn NessaObject) -> bool {
        let ta = self.as_any().downcast_ref::<String>();
        let tb = b.as_any().downcast_ref::<String>();

        return ta == tb;
    }

    fn assign(&mut self, mut other: Object) {
        // Swapping should be ok, since other will be destroyed after this function
        std::mem::swap(self, &mut other.get_mut::<Self>());
    }
}

impl NessaObject for bool {
    fn get_type_id(&self) -> usize {
        return BOOL_ID;
    }

    fn as_any(&self) -> &dyn Any {
        return self;
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        return self;
    }

    fn deep_clone(&self) -> Rc<RefCell<dyn NessaObject>> {
        return Rc::new(RefCell::new(self.clone()));
    }

    fn to_string(&self) -> String {
        return if *self { "true".into() } else { "false".into() };
    }

    fn equal_to(&self, b: &dyn NessaObject) -> bool {
        let ta = self.as_any().downcast_ref::<bool>();
        let tb = b.as_any().downcast_ref::<bool>();

        return ta == tb;
    }

    fn assign(&mut self, mut other: Object) {
        // Swapping should be ok, since other will be destroyed after this function
        std::mem::swap(self, &mut other.get_mut::<Self>());
    }
}

impl NessaObject for Tuple {
    fn get_type_id(&self) -> usize {
        return 0;
    }

    fn get_type(&self) -> Type {
        return Type::And(self.types.clone());
    }

    fn as_any(&self) -> &dyn Any {
        return self;
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        return self;
    }

    fn deep_clone(&self) -> Rc<RefCell<dyn NessaObject>> {
        return Rc::new(RefCell::new(self.clone()));
    }

    fn to_string(&self) -> String {
        return format!("({})", self.exprs.iter().map(|i| i.inner.borrow().to_string()).collect::<Vec<_>>().join(", "));
    }

    fn equal_to(&self, b: &dyn NessaObject) -> bool {
        let ta = self.as_any().downcast_ref::<Tuple>();
        let tb = b.as_any().downcast_ref::<Tuple>();

        return ta == tb;
    }

    fn assign(&mut self, mut other: Object) {
        // Swapping should be ok, since other will be destroyed after this function
        std::mem::swap(self, &mut other.get_mut::<Self>());
    }
}

impl NessaObject for (usize, Type, Type) {
    fn get_type_id(&self) -> usize {
        return 0;
    }

    fn get_type(&self) -> Type {
        return Type::Function(Box::new(self.1.clone()), Box::new(self.2.clone()));
    }

    fn as_any(&self) -> &dyn Any {
        return self;
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        return self;
    }

    fn deep_clone(&self) -> Rc<RefCell<dyn NessaObject>> {
        return Rc::new(RefCell::new(self.clone()));
    }

    fn to_string(&self) -> String {
        return format!("[Line {}] Lambda {:?} => {:?}", self.0, self.1, self.2);
    }

    fn equal_to(&self, b: &dyn NessaObject) -> bool {
        let ta = self.as_any().downcast_ref::<(usize, usize, Type, Type)>();
        let tb = b.as_any().downcast_ref::<(usize, usize, Type, Type)>();

        return ta == tb;
    }

    fn assign(&mut self, mut other: Object) {
        // Swapping should be ok, since other will be destroyed after this function
        std::mem::swap(self, &mut other.get_mut::<Self>());
    }
}

impl NessaObject for (Type, Vec<Object>) {
    fn get_type_id(&self) -> usize {
        return ARR_ID;
    }

    fn get_type(&self) -> Type {
        return ARR_OF!(self.0.clone());
    }

    fn as_any(&self) -> &dyn Any {
        return self;
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        return self;
    }

    fn deep_clone(&self) -> Rc<RefCell<dyn NessaObject>> {
        return Rc::new(RefCell::new(self.clone()));
    }

    fn to_string(&self) -> String {
        return format!("[{}]", self.1.iter().map(|i| i.inner.borrow().to_string()).collect::<Vec<_>>().join(", "));
    }

    fn equal_to(&self, b: &dyn NessaObject) -> bool {
        let ta = self.as_any().downcast_ref::<(Type, Vec<Object>)>();
        let tb = b.as_any().downcast_ref::<(Type, Vec<Object>)>();

        return ta == tb;
    }

    fn assign(&mut self, mut other: Object) {
        // Swapping should be ok, since other will be destroyed after this function
        std::mem::swap(self, &mut other.get_mut::<Self>());
    }
}

impl NessaObject for (Type, Reference, usize) {
    fn get_type_id(&self) -> usize {
        return ARR_IT_ID;
    }

    fn get_type(&self) -> Type {
        return ARR_IT_OF!(self.0.clone());
    }

    fn as_any(&self) -> &dyn Any {
        return self;
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        return self;
    }

    fn deep_clone(&self) -> Rc<RefCell<dyn NessaObject>> {
        return Rc::new(RefCell::new(self.clone()));
    }

    fn to_string(&self) -> String {
        return format!("ArrayIterator(index = {})", self.2);
    }

    fn equal_to(&self, b: &dyn NessaObject) -> bool {
        let ta = self.as_any().downcast_ref::<(Type, Reference, usize)>();
        let tb = b.as_any().downcast_ref::<(Type, Reference, usize)>();

        return ta == tb;
    }

    fn assign(&mut self, mut other: Object) {
        // Swapping should be ok, since other will be destroyed after this function
        std::mem::swap(self, &mut other.get_mut::<Self>());
    }
}

impl NessaObject for TypeInstance {
    fn get_type_id(&self) -> usize {
        return self.id;
    }

    fn get_type(&self) -> Type {
        if self.params.is_empty() {
            return Type::Basic(self.id);

        } else {
            return Type::Template(self.id, self.params.clone());
        }
    }

    fn as_any(&self) -> &dyn Any {
        return self;
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        return self;
    }

    fn deep_clone(&self) -> Rc<RefCell<dyn NessaObject>> {
        return Rc::new(RefCell::new(self.clone()));
    }

    fn to_string(&self) -> String {
        return format!("Class [{}]", self.attributes.iter().map(Object::to_string).collect::<Vec<_>>().join(", "));
    }

    fn equal_to(&self, b: &dyn NessaObject) -> bool {
        let ta = self.as_any().downcast_ref::<TypeInstance>();
        let tb = b.as_any().downcast_ref::<TypeInstance>();

        return ta == tb;
    }

    fn assign(&mut self, mut other: Object) {
        // Swapping should be ok, since other will be destroyed after this function
        std::mem::swap(self, &mut other.get_mut::<Self>());
    }
}

impl NessaObject for () {
    fn get_type_id(&self) -> usize {
        return 0;
    }

    fn get_type(&self) -> Type {
        return Type::Empty;
    }

    fn as_any(&self) -> &dyn Any {
        return self;
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        return self;
    }

    fn deep_clone(&self) -> Rc<RefCell<dyn NessaObject>> {
        return Rc::new(RefCell::new(self.clone()));
    }

    fn to_string(&self) -> String {
        return "()".into();
    }

    fn equal_to(&self, b: &dyn NessaObject) -> bool {
        let ta = self.as_any().downcast_ref::<()>();
        let tb = b.as_any().downcast_ref::<()>();

        return ta == tb;
    }

    fn assign(&mut self, mut other: Object) {
        // Swapping should be ok, since other will be destroyed after this function
        std::mem::swap(self, &mut other.get_mut::<Self>());
    }
}

/*
                                                  ╒═════════╕
    ============================================= │  TESTS  │ =============================================
                                                  ╘═════════╛
*/

#[cfg(test)]
mod tests {
    use crate::number::Integer;
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
        let number = Object::new(Integer::from(10));
        
        assert_eq!(*number.get::<Integer>(), Integer::from(10));

        let reference = number.get_ref_mut_obj();
        let ref_of_ref = reference.get_ref_obj();

        assert_eq!(*reference.get::<Reference>().get::<Integer>(), Integer::from(10));
        assert_eq!(*ref_of_ref.get::<Reference>().get::<Integer>(), Integer::from(10));

        assert_ne!(number.get_ptr(), reference.get_ptr());
        assert_ne!(number.get_ptr(), ref_of_ref.get_ptr());
        assert_ne!(reference.get_ptr(), ref_of_ref.get_ptr());
        assert_ne!(reference.get::<Reference>().mutable, ref_of_ref.get::<Reference>().mutable);
        assert_eq!(number.get_ptr(), reference.get::<Reference>().get_ptr());
        assert_eq!(number.get_ptr(), ref_of_ref.get::<Reference>().get_ptr());

        let struct_ref = reference.get::<Reference>();
        *struct_ref.get_mut::<Integer>() += Integer::from(5);

        assert_eq!(*number.get::<Integer>(), Integer::from(15));
        assert_eq!(*reference.get::<Reference>().get::<Integer>(), Integer::from(15));
        assert_eq!(*ref_of_ref.get::<Reference>().get::<Integer>(), Integer::from(15));
    }
}