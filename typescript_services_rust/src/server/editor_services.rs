use std::rc::Rc;

pub enum ProjectServiceEvent {}

pub type ProjectServiceEventHandler = Rc<dyn Fn(&ProjectServiceEvent)>;

pub struct ProjectService;
