use bridge::data::board::BoardNumber;
use bridge::data::match_protocol::Room;
use bridge::data::scoring::{Score, IMP};
use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

pub type Id = u64;

pub trait Item {
    fn insert_score(
        &mut self,
        room: Room,
        board: BoardNumber,
        score: Score,
    ) -> Result<Option<IMP>, StateError>;
}

pub enum StateError {
    ItemNotFound(Id),
    NotImplemented,
}

pub struct State(BTreeMap<Id, Arc<Mutex<dyn Item + 'static>>>);

impl State {
    pub const fn new() -> State {
        State(BTreeMap::new())
    }

    pub fn create<T: Item + 'static>(&mut self, item: T) -> Id {
        let id = self.0.last_key_value().map(|(k, _)| k + 1).unwrap_or(0);
        self.0.insert(id, Arc::new(Mutex::new(item)));
        id
    }

    pub fn insert_score(
        &mut self,
        id: Id,
        room: Room,
        board: BoardNumber,
        score: Score,
    ) -> Result<Option<IMP>, StateError> {
        self.0
            .get(&id)
            .ok_or(StateError::ItemNotFound(id))?
            .lock()
            .or_else(|e| Ok(e.into_inner()))?
            .insert_score(room, board, score)
    }
}
