use super::BasicBlockIdx;

/// A "virtual" register, in contrast to actual CPU registers.
#[derive(Debug, Copy, Clone)]
#[repr(C)]
pub struct VReg(pub u32);

#[derive(Debug)]
pub struct Inst {
    pub opcode: Opcode,
}

impl Inst {
    pub fn opcode(opcode: Opcode) -> Inst {
        Inst { opcode }
    }
}

#[derive(Debug)]
pub enum Opcode {
    /// dest <- op1 + op2
    Add(VReg, VReg, VReg),
    /// dest <- op1 - op2
    Sub(VReg, VReg, VReg),
    /// dest <- op1 * op2
    Mul(VReg, VReg, VReg),
    /// dest <- op1 / op2
    Div(VReg, VReg, VReg),

    /// dest <- ! op
    Neg(VReg, VReg),
    /// dest <- ~ op
    Not(VReg, VReg),

    /// dest <- op1 == op2
    Equal(VReg, VReg, VReg),
    /// dest <- op1 < op2
    Less(VReg, VReg, VReg),
    /// dest <- op1 <= op2
    LessEqual(VReg, VReg, VReg),

    /// dest <- string at index op
    LoadString(VReg, u32),
    /// dest <- number op
    LoadNumber(VReg, f64),
    /// dest <- bool op
    LoadBool(VReg, bool),
    /// dest <- nil
    LoadNil(VReg),

    /// Unconditionally jump to the target block.
    Branch(BasicBlockIdx),
    /// (condition, true block, false block)
    CondBranch(VReg, BasicBlockIdx, BasicBlockIdx),

    /// Print the op.
    Print(VReg),

    /// Return the op.
    Ret(VReg),
}

impl Inst {
    pub fn is_terminator(&self) -> bool {
        use Opcode::*;
        match &self.opcode {
            Branch(..) => true,
            CondBranch(..) => true,
            Ret(..) => true,
            _ => false,
        }
    }

    pub fn def(&self) -> Option<VReg> {
        use Opcode::*;
        match self.opcode {
            Add(reg, _, _) => Some(reg),
            Sub(reg, _, _) => Some(reg),
            Mul(reg, _, _) => Some(reg),
            Div(reg, _, _) => Some(reg),

            Neg(reg, _) => Some(reg),
            Not(reg, _) => Some(reg),

            Equal(reg, _, _) => Some(reg),
            Less(reg, _, _) => Some(reg),
            LessEqual(reg, _, _) => Some(reg),

            LoadString(reg, _) => Some(reg),
            LoadNumber(reg, _) => Some(reg),
            LoadBool(reg, _) => Some(reg),
            LoadNil(reg) => Some(reg),

            Branch(_) => None,
            CondBranch(_, _, _) => None,

            Print(_) => None,

            Ret(_) => None,
        }
    }

    pub fn for_each_use<F>(&self, mut cb: F)
    where
        F: FnMut(VReg),
    {
        use Opcode::*;
        match self.opcode {
            Add(_, op1, op2) => {
                cb(op1);
                cb(op2);
            }
            Sub(_, op1, op2) => {
                cb(op1);
                cb(op2);
            }
            Mul(_, op1, op2) => {
                cb(op1);
                cb(op2);
            }
            Div(_, op1, op2) => {
                cb(op1);
                cb(op2);
            }

            Neg(op1, op2) => {
                cb(op1);
                cb(op2);
            }
            Not(op1, op2) => {
                cb(op1);
                cb(op2);
            }

            Equal(_, op1, op2) => {
                cb(op1);
                cb(op2);
            }
            Less(_, op1, op2) => {
                cb(op1);
                cb(op2);
            }
            LessEqual(_, op1, op2) => {
                cb(op1);
                cb(op2);
            }

            LoadString(op1, op2) => {
                cb(op1);
            }
            LoadNumber(op1, op2) => {
                cb(op1);
            }
            LoadBool(op1, op2) => {
                cb(op1);
            }
            LoadNil(op1) => {
                cb(op1);
            }

            Branch(_) => {}
            CondBranch(op1, _, _) => {
                cb(op1);
            }

            Print(_) => {}

            Ret(_) => {}
        };
    }
}
