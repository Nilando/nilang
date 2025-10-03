use super::call_frame::CallFrame;
use super::tagged_value::TaggedValue;
use super::ByteCode;

#[derive(Clone)]
pub struct InstructionStream<'a> {
    code: &'a [ByteCode],
    ip: usize
}

impl<'gc> From<&CallFrame<'gc>> for InstructionStream<'gc> {
    fn from(cf: &CallFrame<'gc>) -> Self {
        Self {
            code: cf.get_func().get_code().scoped_deref(),
            ip: cf.get_ip()
        }
    }
}

impl<'gc> InstructionStream<'gc> {
    pub fn advance(&mut self) -> ByteCode {
        let b = self.code[self.ip];
        self.ip += 1;
        b
    }

    pub fn rewind(&mut self) -> ByteCode {
        self.ip -= 1;
        self.code[self.ip]
    }

    pub fn prev(&mut self) -> ByteCode {
        self.code[self.ip - 1]
    }

    pub fn jump(&mut self, offset: i16) {
        if offset > 0 {
            self.ip = self.ip + offset as usize;
        } else {
            self.ip = self.ip - (offset.abs() as usize);
        }
    }
    
    pub fn get_ip(&self) -> usize {
        self.ip
    }

    pub fn get_instr_at(&self, i: usize) -> ByteCode {
        self.code[i]
    }

    pub fn get_code_len(&self) -> usize {
        self.code.len()
    }

    pub fn get_next_arg(&self) -> Option<TaggedValue<'gc>> {
        todo!()
    }
}
