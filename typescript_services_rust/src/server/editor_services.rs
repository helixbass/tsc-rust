use gc::{Finalize, Gc, Trace};

pub enum ProjectServiceEvent {}

pub type ProjectServiceEventHandler = Gc<Box<dyn HandleProjectServiceEvent>>;

pub trait HandleProjectServiceEvent: Trace + Finalize {
    fn call(&self, something: &ProjectServiceEvent);
}

pub struct ProjectService;
