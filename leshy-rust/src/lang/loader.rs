use std::{rc::Rc, collections::HashMap, path::Path};

use crate::lang::{ast::Func, parser::parse};

trait FuncLoader {
    fn load(&self, name: &str) -> Option<Rc<Func>>;
}

struct HashMapLoader {
    funcs: HashMap<String, Rc<Func>>
}

impl FuncLoader for HashMapLoader {
    fn load(&self, name: &str) -> Option<Rc<Func>> {
        self.funcs.get(name).map(|func| func.clone())
    }
}

pub fn files_loader(paths: Vec<&Path>) -> impl FuncLoader {
    let funcs: HashMap<String, Rc<Func>> = paths.iter().flat_map(|path| parse(path)).map(|func| {
        (func.name.clone(), Rc::new(func))
    }).collect();
    HashMapLoader { funcs: funcs }
}
