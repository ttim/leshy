use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::hash::{Hash, Hasher};
use crate::core::api::{Node, NodeKind};

pub struct Cache<'a, N: Node> {
    cell: RefCell<CacheInner<'a, N>>
}

struct CacheInner<'a, N: Node> {
    originals: Vec<N>,
    idx: HashMap<N, u32>,
    cached: Vec<Option<NodeKind<CachedNode<'a, N>>>>,
}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct CachedNode<'a, N: Node> {
    cache: &'a Cache<'a, N>,
    id: u32,
}

impl<'a, N: Node> Cache<'a, N>{
    pub fn new() -> Cache<'a, N> {
        Cache { cell: RefCell::new(CacheInner { originals: vec![], idx: HashMap::new(), cached: vec![] }) }
    }

    pub fn cache(&'a self, node: N) -> CachedNode<'a, N> {
        CachedNode { cache: self, id: Self::get_inner(&mut self.cell.borrow_mut(), node) }
    }

    fn kind(&'a self, cache: &mut CacheInner<'a, N>, id: u32) -> NodeKind<CachedNode<'a, N>> {
        match cache.cached.get(id as usize).unwrap() {
            Some(cached) => { cached.clone() }
            None => {
                let original = cache.originals.get(id as usize).unwrap().get();
                let computed: NodeKind<CachedNode<'a, N>> = match original {
                    NodeKind::Command { command, next } => {
                        NodeKind::Command {command, next: CachedNode { cache: self, id: Self::get_inner(cache, next)} }
                    }
                    NodeKind::Branch { condition, if_true, if_false } => {
                        NodeKind::Branch {
                            condition,
                            if_true: CachedNode { cache: self, id: Self::get_inner(cache, if_true)},
                            if_false: CachedNode { cache: self, id: Self::get_inner(cache, if_false)}
                        }
                    }
                    NodeKind::Call { offset, call, next } => {
                        NodeKind::Call {
                            offset,
                            call:  CachedNode { cache: self, id: Self::get_inner(cache, call)},
                            next:  CachedNode { cache: self, id: Self::get_inner(cache, next)}
                        }
                    }
                    NodeKind::Final => { NodeKind::Final }
                };
                *(cache.cached.get_mut(id as usize).unwrap()) = Some(computed.clone());
                computed
            }
        }
    }

    fn get_inner(cache: &mut CacheInner<'a, N>, node: N) -> u32 {
        let id = if !cache.idx.contains_key(&node) {
            cache.originals.len() as u32
        } else {
            *cache.idx.get(&node).unwrap()
        };

        cache.originals.push(node.clone());
        cache.idx.insert(node, id);
        cache.cached.push(None);

        id
    }
}

impl<'a, N: Node> Debug for Cache<'a, N> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("node cache at ")?;
        (self as *const Cache<'a, N> as usize).fmt(f)
    }
}

impl<'a, N: Node> PartialEq<Self> for Cache<'a, N> {
    fn eq(&self, other: &Self) -> bool {
        self as *const Cache<'a, N> == other as *const Cache<'a, N>
    }
}

impl<'a, N: Node> Eq for Cache<'a, N> {}

impl<'a, N: Node> Hash for Cache<'a, N> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_usize(self as *const Cache<'a, N> as usize);
    }
}

impl<'a, N: Node> Node for CachedNode<'a, N> {
    fn get(&self) -> NodeKind<Self> {
        let mut inner_cache = self.cache.cell.borrow_mut();
        Cache::kind(self.cache, &mut inner_cache, self.id)
    }
}
