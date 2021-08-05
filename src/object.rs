use std::any::*;
use std::rc::Rc;
use std::cell::*;

use crate::number::Number;

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

trait NessaObject {
    fn get_type(&self) -> usize;
    fn as_any(&self) -> &dyn Any;
}

#[derive(Clone)]
struct Object {
    inner: Rc<RefCell<dyn NessaObject>>
}

impl Object {
    fn new<T>(inner: T) -> Object where T: NessaObject + 'static {
        return Object {
            inner: Rc::new(RefCell::new(inner))
        }
    }

    fn get_type(&self) -> usize {
        return self.inner.borrow().get_type();
    }

    fn get_ptr(&self) -> *const dyn NessaObject{
        return self.inner.as_ptr();
    }

    fn get<T>(&self) -> Ref<T> where T: 'static {
        return Ref::map(self.inner.borrow(), |i| i.as_any().downcast_ref::<T>().unwrap());
    }

    fn get_ref(&self) -> Reference {
        return Reference {
            inner: self.inner.clone()
        }
    }

    fn get_ref_obj(&self) -> Object {
        return Object::new(self.get_ref());
    }

    fn is_ref(&self) -> bool {
        return self.inner.borrow().as_any().type_id() == TypeId::of::<Reference>();
    }
}

/*
    ╒═════════════════════════════════════════════╕
    │ Reference struct for high level indirection │
    ╘═════════════════════════════════════════════╛
*/

struct Reference {
    inner: Rc<RefCell<dyn NessaObject>>
}

impl Reference {
    fn get<T>(&self) -> Ref<T> where T: 'static {
        return Ref::map(self.inner.borrow(), |i| i.as_any().downcast_ref::<T>().unwrap());
    }

    fn get_ptr(&self) -> *const dyn NessaObject{
        return self.inner.as_ptr();
    }
}

/*
    ╒════════════════════════════════════════════════╕
    │ Basic implementations of the NessaObject trait │
    ╘════════════════════════════════════════════════╛
*/

impl NessaObject for Reference {
    fn get_type(&self) -> usize {
        return self.inner.borrow().get_type();
    }

    fn as_any(&self) -> &dyn Any {
        return self;
    }
}

impl NessaObject for Number {
    fn get_type(&self) -> usize {
        return 0;
    }

    fn as_any(&self) -> &dyn Any {
        return self;
    }
}

impl NessaObject for String {
    fn get_type(&self) -> usize {
        return 1;
    }

    fn as_any(&self) -> &dyn Any {
        return self;
    }
}

/*
                                                  ╒═════════╕
    ============================================= │  TESTS  │ =============================================
                                                  ╘═════════╛
*/

#[cfg(test)]
mod tests {
    use crate::number::Number;
    use crate::object::*;

    #[test]
    fn object_construction() {
        let number = Object::new(Number::from(10));
        
        assert_eq!(*number.get::<Number>(), Number::from(10));

        let string = Object::new(String::from("Test"));
        
        assert_eq!(*string.get::<String>(), "Test".to_string());

        assert_ne!(number.get_type(), string.get_type());
    }

    #[test]
    fn references() {
        let number = Object::new(Number::from(10));
        
        assert_eq!(*number.get::<Number>(), Number::from(10));

        let reference = number.get_ref_obj();

        assert_eq!(*reference.get::<Reference>().get::<Number>(), Number::from(10));

        assert_ne!(number.get_ptr(), reference.get_ptr());
        assert_eq!(number.get_ptr(), reference.get::<Reference>().get_ptr());
    }
}