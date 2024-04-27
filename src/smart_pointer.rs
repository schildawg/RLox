macro_rules! copy_ref {
    ($a:expr) => {
        Rc::clone($a)
    };
}

macro_rules! new_cell_ref {
    ($a:expr) => {
        Rc::new(RefCell::new($a))
    };
}

pub (crate) use {copy_ref, new_cell_ref};
