use rustc_hash::FxHashMap;
use typed_arena::Arena;

pub type IString = usize;

pub struct Interner<'a> {
    map: FxHashMap<&'a str, IString>,
    vec: Vec<&'a str>,
    arena: &'a Arena<u8>,
}

impl<'a> Interner<'a> {
    pub fn new(arena: &'a Arena<u8>) -> Self {
        Interner {
            map: FxHashMap::default(),
            vec: Vec::new(),
            arena,
        }
    }

    pub fn intern(&mut self, name: &str) -> IString {
        if let Some(&i) = self.map.get(name) {
            return i;
        }

        let i = self.vec.len();
        let name = self.arena.alloc_str(name);
        self.map.insert(name, i);
        self.vec.push(name);
        i
    }

    pub fn lookup(&self, istring: IString) -> &str {
        self.vec[istring]
    }
}
