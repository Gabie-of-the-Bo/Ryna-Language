use std::any::*;
use std::rc::Rc;
use std::cell::*;

use crate::number::Number;
use crate::operations::*;
use crate::types::Type;

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
}

#[derive(Clone)]
pub struct Object {
    inner: Rc<RefCell<dyn NessaObject>>
}

impl Object {
    pub fn new<T>(inner: T) -> Object where T: NessaObject + 'static {
        return Object {
            inner: Rc::new(RefCell::new(inner))
        }
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
        let a = Ref::map(self.inner.borrow(), |i| {
            if self.is_ref() {
                let ptr = i.as_any().downcast_ref::<Reference>().unwrap().get_ptr();

                // SAFETY: The value will not be dropped because self contains the Reference
                // and this object contains the inner object that we are returning. All these
                // are reference counted, so there is no problem.
                unsafe { (*ptr).as_any().downcast_ref::<T>().unwrap() }

            } else {
                i.as_any().downcast_ref::<T>().unwrap()
            }
        });

        return a;
    }

    pub fn get<T>(&self) -> Ref<T> where T: 'static {
        return Ref::map(self.inner.borrow(), |i| i.as_any().downcast_ref::<T>().unwrap());
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
}

/*
    ╒═════════════════════════════════════════════╕
    │ Reference struct for high level indirection │
    ╘═════════════════════════════════════════════╛
*/

pub struct Reference {
    pub inner: Rc<RefCell<dyn NessaObject>>,
    pub mutable: bool
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
}

impl NessaObject for Number {
    fn get_type_id(&self) -> usize {
        return 0;
    }

    fn as_any(&self) -> &dyn Any {
        return self;
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        return self;
    }
}

impl NessaObject for String {
    fn get_type_id(&self) -> usize {
        return 1;
    }

    fn as_any(&self) -> &dyn Any {
        return self;
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        return self;
    }
}

/*
    ╒═════════════════════════════════════╕
    │ Operator implementations for Object │
    ╘═════════════════════════════════════╛
*/

impl Object {
    pub fn apply_unary_operation(a: &Object, ctx: &UnaryOperator) -> Object {
        let op = ctx.get_unary_op(a).expect("Operation not found");

        return op(a);
    }

    pub fn apply_binary_operation(a: &Object, b: &Object, ctx: &BinaryOperator) -> Object {
        let op = ctx.get_binary_op(a, b).expect("Operation not found");

        return op(a, b);
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
    use crate::context::*;

    #[test]
    fn object_construction() {
        let number = Object::new(Number::from(10));
        
        assert_eq!(*number.get::<Number>(), Number::from(10));

        let string = Object::new(String::from("Test"));
        
        assert_eq!(*string.get::<String>(), "Test".to_string());

        assert_ne!(number.get_type_id(), string.get_type_id());
    }

    #[test]
    fn references() {
        let number = Object::new(Number::from(10));
        
        assert_eq!(*number.get::<Number>(), Number::from(10));

        let reference = number.get_ref_mut_obj();
        let ref_of_ref = reference.get_ref_obj();

        assert_eq!(*reference.get::<Reference>().get::<Number>(), Number::from(10));
        assert_eq!(*ref_of_ref.get::<Reference>().get::<Number>(), Number::from(10));

        assert_ne!(number.get_ptr(), reference.get_ptr());
        assert_ne!(number.get_ptr(), ref_of_ref.get_ptr());
        assert_ne!(reference.get_ptr(), ref_of_ref.get_ptr());
        assert_ne!(reference.get::<Reference>().mutable, ref_of_ref.get::<Reference>().mutable);
        assert_eq!(number.get_ptr(), reference.get::<Reference>().get_ptr());
        assert_eq!(number.get_ptr(), ref_of_ref.get::<Reference>().get_ptr());

        let struct_ref = reference.get::<Reference>();
        *struct_ref.get_mut::<Number>() += Number::from(5);

        assert_eq!(*number.get::<Number>(), Number::from(15));
        assert_eq!(*reference.get::<Reference>().get::<Number>(), Number::from(15));
        assert_eq!(*ref_of_ref.get::<Reference>().get::<Number>(), Number::from(15));
    }

    #[test]
    fn operators() {
        let ctx = standard_ctx();
        let plus_op = &ctx.binary_ops[0];

        let number = Object::new(Number::from(10));
        let string = Object::new(String::from("Test"));
        let number_ref = number.get_ref_obj();
        let string_ref = string.get_ref_obj();

        let num_num = Object::apply_binary_operation(&number, &number, &plus_op);
        let str_str = Object::apply_binary_operation(&string, &string, &plus_op);

        let num_num_ref = Object::apply_binary_operation(&number_ref, &number_ref, &plus_op);
        let str_str_ref = Object::apply_binary_operation(&string_ref, &string_ref, &plus_op);

        assert_eq!(*num_num.get::<Number>(), Number::from(20));
        assert_eq!(*str_str.get::<String>(), String::from("TestTest"));

        assert_eq!(*num_num_ref.get::<Number>(), *num_num.get::<Number>());
        assert_eq!(*str_str_ref.get::<String>(), *str_str.get::<String>());
    }
}