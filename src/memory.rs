use crate::object::{Object, ObjectData};
use rustc_hash::FxHashMap;

pub type HeapId = usize;

pub struct Heap {
    strings: FxHashMap<String, HeapId>,
    objects: Vec<Object>,
}

impl Heap {
    pub fn new() -> Self {
        Self {
            strings: FxHashMap::default(),
            objects: Vec::new(),
        }
    }

    pub fn alloc(&mut self, data: ObjectData) -> HeapId {
        self.collect_garbage();

        self.objects.push(Object::new(data));
        self.objects.len() - 1
    }

    pub fn intern(&mut self, name: &str) -> HeapId {
        if let Some(&i) = self.strings.get(name) {
            return i;
        }

        let i = self.alloc(ObjectData::String(name.to_string()));
        self.strings.insert(name.to_string(), i);
        i
    }

    pub fn lookup(&self, i: HeapId) -> &ObjectData {
        &self.objects[i].data
    }

    pub fn lookup_mut(&mut self, i: HeapId) -> &mut ObjectData {
        &mut self.objects[i].data
    }

    fn collect_garbage(&self) {}
}
