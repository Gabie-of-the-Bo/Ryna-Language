use std::cell::UnsafeCell;

#[derive(Debug)]
pub struct MutCell<T> where T: Clone + PartialEq + Default {
    inner: UnsafeCell<T>
}

impl<T: Clone + PartialEq + Default> Clone for MutCell<T> {
    fn clone(&self) -> Self {
        Self { inner: UnsafeCell::new(self.borrow().clone()) }
    }
}

impl<T: Clone + PartialEq + Default> PartialEq for MutCell<T> {
    fn eq(&self, other: &Self) -> bool {
        self.borrow() == other.borrow()
    }
} 

impl<T: Clone + PartialEq + Default> MutCell<T> {
    pub fn new(obj: T) -> Self {
        MutCell { inner: UnsafeCell::new(obj) }
    }

    #[inline(always)]
    pub fn borrow(&self) -> &T {
        unsafe { &*self.inner.get() }
    }

    #[inline(always)]
    pub fn borrow_mut(&self) -> &mut T {
        unsafe { &mut *self.inner.get() }
    }

    #[inline(always)]
    pub fn as_ptr(&self) -> *mut T {
        self.inner.get()
    }

    #[inline(always)]
    pub fn take(&self) -> T {
        std::mem::take(self.borrow_mut())
    }
}

impl<T: Clone + PartialEq + Default> Eq for MutCell<T> {}