use super::func::Func;

pub fn optimize_func(func: &mut Func) {
    let mut optimizer = Optimizer::new(func);

    // compact the cfg
    optimizer.dead_code_elimination();
    optimizer.global_value_numbering();
    optimizer.dead_code_elimination();
    optimizer.global_value_numbering();
    optimizer.dead_code_elimination();
}

pub struct Optimizer {
}

impl Optimizer {
    pub fn new(func: &mut Func) -> Self {
        todo!()
    }

    fn dead_code_elimination(&mut self) {
        // remove dead blocks
    }

    fn global_value_numbering(&mut self) {
        // escape analysis 
    }
}
