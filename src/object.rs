use std::{cell::{RefCell, Ref, RefMut}, rc::Rc};

use crate::{number::Integer, types::{Type, INT_ID, FLOAT_ID, STR_ID, BOOL_ID, ARR_IT_ID, INT, FLOAT, STR, BOOL, ARR_ID}, ARR_OF, ARR_IT_OF};

type DataBlock = Rc<RefCell<ObjectBlock>>;

/*
                                                  ╒══════════════════╕
    ============================================= │  IMPLEMENTATION  │ =============================================
                                                  ╘══════════════════╛
*/

#[derive(Clone, PartialEq, Debug)]
pub struct NessaArray {
    pub elements: Vec<Object>,
    pub elem_type: Type
}

#[derive(Clone, PartialEq, Debug)]
pub struct NessaTuple {
    pub elements: Vec<Object>,
    pub elem_types: Vec<Type>
}

#[derive(Clone, PartialEq, Debug)]
pub struct NessaArrayIt {
    pub pos: usize,
    pub block: DataBlock,
    pub it_type: Type
}

#[derive(Clone, PartialEq, Debug)]
pub struct NessaLambda {
    pub loc: usize,
    pub args_type: Type,
    pub ret_type: Type
}

#[derive(Clone, PartialEq, Debug)]
pub struct TypeInstance {
    pub id: usize,
    pub params: Vec<Type>,
    pub attributes: Vec<Object>
}

#[derive(Clone, PartialEq, Debug)]
pub enum ObjectBlock {
    Empty,
    Int(Integer),
    Float(f64),
    Str(String),
    Bool(bool),

    Tuple(NessaTuple),
    Array(NessaArray),
    ArrayIter(NessaArrayIt),

    Lambda(NessaLambda),

    Instance(TypeInstance),

    Ref(DataBlock),
    Mut(DataBlock),
}

impl Default for ObjectBlock {
    fn default() -> Self {
        return ObjectBlock::Empty;
    }
}

impl Eq for ObjectBlock {}

impl ObjectBlock {
    pub fn to_obj(self) -> Object {
        return Object { inner: Rc::new(RefCell::new(self)) }
    } 

    pub fn get_type_id(&self) -> usize {
        return match self {
            ObjectBlock::Empty => 0,
            ObjectBlock::Int(_) => INT_ID,
            ObjectBlock::Float(_) => FLOAT_ID,
            ObjectBlock::Str(_) => STR_ID,
            ObjectBlock::Bool(_) => BOOL_ID,
            ObjectBlock::Tuple(_) => 0,
            ObjectBlock::Array(_) => ARR_ID,
            ObjectBlock::ArrayIter(_) => ARR_IT_ID,
            ObjectBlock::Lambda(_) => 0,
            ObjectBlock::Instance(i) => i.id,
            ObjectBlock::Ref(_) => 0,
            ObjectBlock::Mut(_) => 0,
        };
    }

    pub fn get_type(&self) -> Type {
        return match self {
            ObjectBlock::Empty => Type::Empty,
            ObjectBlock::Int(_) => INT,
            ObjectBlock::Float(_) => FLOAT,
            ObjectBlock::Str(_) => STR,
            ObjectBlock::Bool(_) => BOOL,
            ObjectBlock::Tuple(t) => Type::And(t.elem_types.clone()),
            ObjectBlock::Array(a) => ARR_OF!(a.elem_type.clone()),
            ObjectBlock::ArrayIter(i) => ARR_IT_OF!(i.it_type.clone()),
            ObjectBlock::Lambda(l) => Type::Function(Box::new(l.args_type.clone()), Box::new(l.ret_type.clone())),
            ObjectBlock::Instance(i) => if i.params.is_empty() { Type::Basic(i.id) } else { Type::Template(i.id, i.params.clone()) },
            ObjectBlock::Ref(r) => Type::Ref(Box::new(r.borrow().get_type())),
            ObjectBlock::Mut(r) => Type::MutRef(Box::new(r.borrow().get_type())),
        };
    }

    pub fn get_inner<T>(&self) -> &T where ObjectBlock: Get<T> {
        return Get::<T>::get(self);
    }

    pub fn mut_inner<T>(&mut self) -> &mut T where ObjectBlock: GetMut<T> {
        return GetMut::<T>::get(self);
    }

    pub fn deref(&self) -> &DataBlock {
        if let ObjectBlock::Ref(n) | ObjectBlock::Mut(n) = self {
            return n;
        }

        unreachable!();
    }

    pub fn assign(&mut self, other: ObjectBlock) {
        use ObjectBlock::*;

        match (&mut *self.deref().borrow_mut(), other) {
            (Int(a), Int(b)) => *a = b,
            (Float(a), Float(b)) => *a = b,
            (Str(a), Str(b)) => *a = b,
            (Bool(a), Bool(b)) => *a = b,
            (Array(a), Array(b)) => *a = b,
            (ArrayIter(a), ArrayIter(b)) => *a = b,
            (Lambda(a), Lambda(b)) => *a = b,
            (Instance(a), Instance(b)) => *a = b,
            (Tuple(a), Tuple(b)) => *a = b,

            (a, b) => unreachable!("{:?}, {:?}", a, b)
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Object {
    pub inner: DataBlock
}

impl Object {
    pub fn new<T: NessaData>(data: T) -> Self {
        return data.data().to_obj();
    }

    pub fn get_ptr(&self) -> *mut ObjectBlock {
        return self.inner.as_ptr();
    }

    pub fn arr(elements: Vec<Object>, elem_type: Type) -> Self {
        return ObjectBlock::Array(NessaArray { elements, elem_type }).to_obj()
    }

    pub fn arr_it(it_type: Type, block: DataBlock, pos: usize) -> Self {
        return ObjectBlock::ArrayIter(NessaArrayIt { pos, block, it_type }).to_obj()
    }

    pub fn lambda(loc: usize, args_type: Type, ret_type: Type) -> Self {
        return ObjectBlock::Lambda(NessaLambda { loc, args_type, ret_type }).to_obj()
    }

    pub fn tuple(elements: Vec<Object>, elem_types: Vec<Type>) -> Self {
        return ObjectBlock::Tuple(NessaTuple { elements, elem_types }).to_obj()
    }

    pub fn instance(attributes: Vec<Object>, params: Vec<Type>, id: usize) -> Self {
        return ObjectBlock::Instance(TypeInstance { params, attributes, id }).to_obj()
    }

    pub fn get<T>(&self) -> Ref<T> where ObjectBlock: Get<T> {
        return Ref::map(self.inner.borrow(), |i| Get::<T>::get(i));
    }

    pub fn deref<T>(&self) -> RefMut<T> where ObjectBlock: Deref<T> + GetMut<T> {
        return RefMut::map(self.inner.borrow_mut(), |i| {
            let ptr = match i {
                ObjectBlock::Ref(i) |
                ObjectBlock::Mut(i) => i.as_ptr(),
    
                _ => unreachable!()
            };

            GetMut::<T>::get(unsafe { &mut *ptr })
        });
    }
    
    pub fn assign(&self, other_obj: Object) {
        match Rc::try_unwrap(other_obj.inner) {
            Ok(inner) => self.inner.borrow_mut().assign(RefCell::take(&inner)),
            Err(inner) => self.inner.borrow_mut().assign(inner.borrow().clone())
        };
    }

    pub fn from_inner(inner: DataBlock) -> Self {
        return Object { inner };
    }

    pub fn empty() -> Object {
        return ObjectBlock::Empty.to_obj();
    }

    pub fn get_ref(&self) -> Object {
        return match &*self.inner.borrow() {
            ObjectBlock::Ref(i) |
            ObjectBlock::Mut(i) => ObjectBlock::Ref(i.clone()).to_obj(),

            _ => ObjectBlock::Ref(self.inner.clone()).to_obj()
        }
    }

    pub fn get_mut(&self) -> Object {
        return match &*self.inner.borrow() {
            ObjectBlock::Ref(i) |
            ObjectBlock::Mut(i) => ObjectBlock::Mut(i.clone()).to_obj(),

            _ => ObjectBlock::Mut(self.inner.clone()).to_obj()
        }
    }

    pub fn to_debug_string(&self) -> String {
        return format!("{:?}", self.inner.borrow());
    }

    pub fn deep_clone(&self) -> Object {
        return match &*self.inner.borrow() {
            ObjectBlock::Empty => ObjectBlock::Empty.to_obj(),
            ObjectBlock::Int(n) => ObjectBlock::Int(n.clone()).to_obj(),
            ObjectBlock::Float(n) => ObjectBlock::Float(*n).to_obj(),
            ObjectBlock::Str(s) => ObjectBlock::Str(s.clone()).to_obj(),
            ObjectBlock::Bool(b) => ObjectBlock::Bool(b.clone()).to_obj(),
            ObjectBlock::Tuple(t) => ObjectBlock::Tuple(NessaTuple { 
                elements: t.elements.iter().map(Object::deep_clone).collect(), 
                elem_types: t.elem_types.clone()
            }).to_obj(),
            ObjectBlock::Array(a) => ObjectBlock::Array(NessaArray { 
                elements: a.elements.iter().map(Object::deep_clone).collect(), 
                elem_type: a.elem_type.clone()
            }).to_obj(),
            ObjectBlock::ArrayIter(i) => ObjectBlock::ArrayIter(NessaArrayIt { 
                pos: i.pos, 
                block: i.block.clone(), 
                it_type: i.it_type.clone() 
            }).to_obj(),
            ObjectBlock::Lambda(l) => ObjectBlock::Lambda(NessaLambda { 
                loc: l.loc, 
                args_type: l.args_type.clone(), 
                ret_type: l.ret_type.clone() 
            }).to_obj(),
            ObjectBlock::Instance(i) => ObjectBlock::Instance(TypeInstance {
                id: i.id, 
                params: i.params.clone(), 
                attributes: i.attributes.iter().map(Object::deep_clone).collect()
            }).to_obj(),
            ObjectBlock::Ref(r) => ObjectBlock::Ref(r.clone()).to_obj(),
            ObjectBlock::Mut(r) => ObjectBlock::Mut(r.clone()).to_obj(),
        }
    }

    pub fn get_type_id(&self) -> usize {
        return self.inner.borrow().get_type_id();
    }

    pub fn get_type(&self) -> Type {
        return self.inner.borrow().get_type();
    }

    pub fn deref_obj(&self) -> Object {
        return match &*self.inner.borrow() {
            ObjectBlock::Ref(r) | ObjectBlock::Mut(r) => Object::from_inner(r.clone()),
            _ => unreachable!()
        };
    }
}

pub trait NessaData {
    fn data(self) -> ObjectBlock;
}

pub trait Get<T> {
    fn get<'a>(&self) -> &T;
}

pub trait GetMut<T> {
    fn get<'a>(&mut self) -> &mut T;
}

pub trait Deref<T> {
    fn deref<'a>(&self) -> Ref<T>;
}

macro_rules! impl_nessa_data {
    ($t: ty, $v: tt) => {
        impl NessaData for $t {
            fn data(self) -> ObjectBlock {
                return ObjectBlock::$v(self)
            }
        }

        impl Get<$t> for ObjectBlock {
            fn get<'a>(&self) -> &$t {
                if let ObjectBlock::$v(n) = self {
                    return n;
                }
        
                unreachable!("Unable to get {:?}", self);    
            }
        }

        impl GetMut<$t> for ObjectBlock {
            fn get<'a>(&mut self) -> &mut $t {
                if let ObjectBlock::$v(n) = self {
                    return n;
                }
        
                unreachable!("Unable to get mut {:?}", self);    
            }
        }

        impl Deref<$t> for ObjectBlock {
            fn deref<'a>(&self) -> Ref<$t> {
                return Ref::map(self.deref().borrow(), |i| Get::<$t>::get(i));
            }
        }
    };
}

impl_nessa_data!(Integer, Int);
impl_nessa_data!(f64, Float);
impl_nessa_data!(String, Str);
impl_nessa_data!(bool, Bool);
impl_nessa_data!(TypeInstance, Instance);
impl_nessa_data!(NessaArray, Array);
impl_nessa_data!(NessaTuple, Tuple);
impl_nessa_data!(NessaLambda, Lambda);
impl_nessa_data!(NessaArrayIt, ArrayIter);

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

        let reference = number.get_mut();
        let ref_of_ref = reference.get_ref();

        assert_eq!(*reference.deref::<Integer>(), Integer::from(10));
        assert_eq!(*ref_of_ref.deref::<Integer>(), Integer::from(10));

        assert_ne!(number.get_ptr(), reference.get_ptr());
        assert_ne!(number.get_ptr(), ref_of_ref.get_ptr());
        assert_ne!(reference.get_ptr(), ref_of_ref.get_ptr());
        assert_eq!(number.get_ptr(), reference.inner.borrow().deref().as_ptr());
        assert_eq!(number.get_ptr(), ref_of_ref.inner.borrow().deref().as_ptr());

        {
            let mut struct_ref = reference.deref::<Integer>();
            *struct_ref += Integer::from(5);    
        }

        assert_eq!(*number.get::<Integer>(), Integer::from(15));
        assert_eq!(*reference.deref::<Integer>(), Integer::from(15));
        assert_eq!(*ref_of_ref.deref::<Integer>(), Integer::from(15));

        reference.assign(ObjectBlock::Int(Integer::from(20)).to_obj());

        assert_eq!(*number.get::<Integer>(), Integer::from(20));
        assert_eq!(*reference.deref::<Integer>(), Integer::from(20));
        assert_eq!(*ref_of_ref.deref::<Integer>(), Integer::from(20));
    }
}