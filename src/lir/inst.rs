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
}
