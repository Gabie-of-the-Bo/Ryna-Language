use std::any::*;
use std::rc::Rc;
use std::cell::*;

use crate::number::Number;
use crate::operations::Operator;
use crate::context::NessaContext;
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
    fn to_string(&self) -> String;
    fn equal_to(&self, b: &dyn NessaObject) -> bool;
}

#[derive(Clone, Debug)]
pub struct Object {
    inner: Rc<RefCell<dyn NessaObject>>
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

    pub fn deref_obj(&self) -> Object {
        return Object {
            inner: self.get::<Reference>().inner.clone()
        }    
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

    fn to_string(&self) -> String {
        return String::from(self);
    }

    fn equal_to(&self, b: &dyn NessaObject) -> bool {
        let ta = self.as_any().downcast_ref::<Number>();
        let tb = b.as_any().downcast_ref::<Number>();

        return ta == tb;
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

    fn to_string(&self) -> String {
        return self.clone();
    }

    fn equal_to(&self, b: &dyn NessaObject) -> bool {
        let ta = self.as_any().downcast_ref::<String>();
        let tb = b.as_any().downcast_ref::<String>();

        return ta == tb;
    }
}

impl NessaObject for bool {
    fn get_type_id(&self) -> usize {
        return 2;
    }

    fn as_any(&self) -> &dyn Any {
        return self;
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        return self;
    }

    fn to_string(&self) -> String {
        return if *self { "true".into() } else { "false".into() };
    }

    fn equal_to(&self, b: &dyn NessaObject) -> bool {
        let ta = self.as_any().downcast_ref::<bool>();
        let tb = b.as_any().downcast_ref::<bool>();

        return ta == tb;
    }
}

impl NessaObject for (Type, Vec<Object>) {
    fn get_type_id(&self) -> usize {
        return 3;
    }

    fn get_type(&self) -> Type {
        return Type::Template(3, vec!(self.0.clone()));
    }

    fn as_any(&self) -> &dyn Any {
        return self;
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        return self;
    }

    fn to_string(&self) -> String {
        return format!("{{{}}}", self.1.iter().map(|i| i.inner.borrow().to_string()).collect::<Vec<_>>().join(", "));
    }

    fn equal_to(&self, b: &dyn NessaObject) -> bool {
        let ta = self.as_any().downcast_ref::<Vec<Object>>();
        let tb = b.as_any().downcast_ref::<Vec<Object>>();

        return ta == tb;
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

    fn to_string(&self) -> String {
        return "()".into();
    }

    fn equal_to(&self, b: &dyn NessaObject) -> bool {
        let ta = self.as_any().downcast_ref::<()>();
        let tb = b.as_any().downcast_ref::<()>();

        return ta == tb;
    }
}

/*
    ╒═════════════════════════════════════╕
    │ Operator implementations for Object │
    ╘═════════════════════════════════════╛
*/

impl Object {
    pub fn apply_unary_operation(a: &Object, id: usize, ctx: &NessaContext) -> Result<Object, String> {
        let ops = ctx.get_unary_operations(id, a.get_type());

        if let Operator::Unary{representation: r, ..} = &ctx.unary_ops[id] {
            if ops.len() == 0 {
                return Err(format!("Unable to find unary operation {}{}", r, a.get_type().get_name(&ctx)));
            }

            if ops.len() > 1 {
                return Err(format!("Unary operation {}{} is ambiguous", r, a.get_type().get_name(&ctx)));
            }
        }

        return Ok(ops[0].2(a));
    }

    pub fn apply_binary_operation(a: &Object, b: &Object, id: usize, ctx: &NessaContext) -> Result<Object, String> {
        let ops = ctx.get_binary_operations(id, a.get_type(), b.get_type());

        if let Operator::Binary{representation: r, ..} = &ctx.binary_ops[id] {
            if ops.len() == 0 {
                return Err(format!("Unable to find binary operation {} {} {}", 
                                    a.get_type().get_name(&ctx), r, b.get_type().get_name(&ctx)));
            }

            if ops.len() > 1 {
                return Err(format!("Binary operation {} {} {} is ambiguous", 
                                    a.get_type().get_name(&ctx), r, b.get_type().get_name(&ctx)));
            }
        }

        return Ok(ops[0].2(a, b));
    }

    pub fn apply_nary_operation(a: &Object, b: &[&Object], id: usize, ctx: &NessaContext) -> Result<Object, String> {
        let args_type = b.iter().map(|i| i.get_type()).collect::<Vec<_>>();
        let ops = ctx.get_nary_operations(id, a.get_type(), &args_type);

        if let Operator::Nary{open_rep: or, close_rep: cr, ..} = &ctx.nary_ops[id] {
            if ops.len() == 0 {
                return Err(format!("Unable to find n-ary operation {}{}{}{}", 
                                    a.get_type().get_name(&ctx), or, 
                                    args_type.iter().map(|i| i.get_name(ctx)).collect::<Vec<_>>().join(", "), cr));
            }
    
            if ops.len() > 1 {
                return Err(format!("N-ary operation {}{}{}{} is ambiguous", 
                                    a.get_type().get_name(&ctx), or, 
                                    args_type.iter().map(|i| i.get_name(ctx)).collect::<Vec<_>>().join(", "), cr));
            }
        }

        return Ok(ops[0].2(a, b));
    }

    pub fn apply_function(args: &[&Object], templates: &[Type], id: usize, ctx: &NessaContext) -> Result<Object, String> {
        let args_type = args.iter().map(|i| i.get_type()).collect::<Vec<_>>();
        let funcs = ctx.get_function_overloads(id, &args_type);

        if funcs.len() == 0 {
            return Err(format!("Unable to find function overload for {}({})", 
                                ctx.functions[id].name,
                                args_type.iter().map(|i| i.get_name(ctx)).collect::<Vec<_>>().join(", ")));
        }

        if funcs.len() > 1 {
            return Err(format!("Function overload {}({}) is ambiguous", 
                                ctx.functions[id].name,
                                args_type.iter().map(|i| i.get_name(ctx)).collect::<Vec<_>>().join(", ")));
        }

        return Ok(funcs[0].2(templates, args));
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
        let mut ctx = standard_ctx();

        let number = Object::new(Number::from(10));
        let string = Object::new(String::from("Test"));

        let num_num = Object::apply_binary_operation(&number, &number, 0, &ctx).unwrap();
        let str_str = Object::apply_binary_operation(&string, &string, 0, &ctx).unwrap();

        assert_eq!(*num_num.get::<Number>(), Number::from(20));
        assert_eq!(*str_str.get::<String>(), String::from("TestTest"));

        let neg_num = Object::apply_unary_operation(&number, 0, &ctx).unwrap();

        assert_eq!(*neg_num.get::<Number>(), Number::from(-10));

        // Dummy call operation
        ctx.define_nary_operation(0, Type::Basic(0), &[], Type::Basic(0), |a, _| { a.clone() }).unwrap();

        let num_cpy = Object::apply_nary_operation(&number, &[], 0, &ctx).unwrap();

        assert_eq!(*num_cpy.get::<Number>(), *number.get::<Number>());
    }

    #[test]
    fn functions() {
        let ctx = standard_ctx();

        let number = Object::new(Number::from(10));
        
        let f_num = Object::apply_function(&[&number], &[], 0, &ctx).unwrap();

        assert_eq!(*f_num.get::<Number>(), Number::from(11));
    }
}