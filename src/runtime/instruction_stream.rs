use core::cell::Cell;

use super::call_frame::CallFrame;
use super::ByteCode;

#[derive(Clone)]
pub struct InstructionStream<'a> {
    code: &'a [ByteCode],
    ip: &'a Cell<usize>
}

impl<'gc> From<&'gc CallFrame<'gc>> for InstructionStream<'gc> {
    fn from(cf: &'gc CallFrame<'gc>) -> Self {
        Self {
            code: cf.get_func().get_code().scoped_deref(),
            ip: cf.get_ip()
        }
    }
}

impl<'gc> InstructionStream<'gc> {
    pub fn advance(&mut self) -> ByteCode {
        let b = self.code[self.ip.get()];
        self.ip.set(self.ip.get() + 1);
        b
    }

    pub fn rewind(&mut self) -> ByteCode {
        self.ip.set(self.ip.get() - 1);
        self.code[self.ip.get()]
    }

    pub fn prev(&mut self) -> ByteCode {
        self.code[self.ip.get() - 1]
    }

    pub fn jump(&mut self, offset: i16) {
        if offset > 0 {
            self.ip.set(self.ip.get() + offset as usize);
        } else {
            self.ip.set(self.ip.get() - (offset.unsigned_abs() as usize));
        }
    }
    
    pub fn get_ip(&self) -> usize {
        self.ip.get()
    }

    pub fn get_instr_at(&self, i: usize) -> ByteCode {
        self.code[i]
    }
}
