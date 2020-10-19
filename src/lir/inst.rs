use super::BasicBlockIdx;

/// A "virtual" register, in contrast to actual CPU registers.
#[derive(Debug, Copy, Clone)]
pub struct VReg(pub u32);

#[derive(Debug)]
pub struct Inst {
    pub opcode: Opcode,
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

    /// Unconditionally jump to the target block.
    Branch(BasicBlockIdx),
    /// Jump to the target block if the op is true-ish.
    CondBranch(VReg, BasicBlockIdx),
}
