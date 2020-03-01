mod reg;

pub use reg::{AddrMode, Reg, S};

// (base, index, offset)
type RM = (Reg, Reg, i32);

struct Emitter<'buf> {
    buf: &'buf mut [u8],
    index: usize,
}

impl<'buf> Emitter<'buf> {
    pub fn new(buf: &'buf mut [u8]) -> Emitter {
        Emitter { buf, index: 0 }
    }

    fn emit(&mut self, byte: u8) {
        self.buf[self.index] = byte;
        self.index += 1;
    }

    fn emit_u32(&mut self, num: u32) {
        self.emit((num & 0xff) as u8);
        self.emit(((num >> 8) & 0xff) as u8);
        self.emit(((num >> 16) & 0xff) as u8);
        self.emit(((num >> 24) & 0xff) as u8);
    }

    fn emit_rex(&mut self, s: S, rm: Reg, sib: Reg, reg: Reg) {
        let b: u8 = rm.ord() >> 3;
        let x: u8 = sib.ord() >> 3;
        let r: u8 = reg.ord() >> 3;
        let w: u8 = (s == S::Q || s == S::SLQ).into();
        if (w, b, x, r) != (0, 0, 0, 0) {
            // REX is required.
            let rex = 0b0100_0000 | (w << 3) | (r << 2) | (x << 1) | b;
            self.emit(rex);
        }
    }

    fn emit_reg_rm(&mut self, s: S, scale: u32, opcode: u8, dst: Reg, src: RM) {
        self.emit_mod_rm_full(s, scale, opcode, dst, src);
    }

    fn emit_mod_rm_full(
        &mut self,
        s: S,
        scale: u32,
        mut opcode: u8,
        reg: Reg,
        rm: RM,
    ) {
        let (base, index, _) = rm;
        if s != S::B {
            opcode += 1;
        }
        self.emit_rex(s, base, index, reg);
        self.emit(opcode);
        self.emit_mod_rm(s, scale, opcode, reg, rm);
    }

    fn emit_mod_rm(
        &mut self,
        s: S,
        scale: u32,
        opcode: u8,
        reg: Reg,
        (base, index, offset): RM,
    ) {
        if offset == 0 {
            self.emit(encode_mod_rm(AddrMode::AtBase, MOD_SIB, reg));
            self.emit(encode_sib(scale, (base, index, offset)));
        } else if offset < 0xff {
            dbg!(base, index, offset);
            self.emit(encode_mod_rm(
                AddrMode::AtBaseDisp8,
                MOD_SIB,
                reg,
            ));
            self.emit(encode_sib(scale, (base, index, offset)));
            self.emit(offset as u8);
        } else {
            dbg!(base, index, offset);
            self.emit(encode_mod_rm(
                AddrMode::AtBaseDisp32,
                MOD_SIB,
                reg,
            ));
            self.emit(encode_sib(scale, (base, index, offset)));
            self.emit_u32(offset as u32);
        }
    }

    pub fn pushq(&mut self, reg: Reg) {
        self.emit(0x50 + reg.ord7());
    }
    pub fn popq(&mut self, reg: Reg) {
        self.emit(0x58 + reg.ord7());
    }

    pub fn mov_reg_rm(&mut self, s: S, scale: u32, dst: Reg, src: RM) {
        self.emit_reg_rm(s, scale, 0x8a, dst, src);
    }

    pub fn leave(&mut self) {
        self.emit(0xc9);
    }
    pub fn ret(&mut self) {
        self.emit(0xc3);
    }
}

const MOD_SIB: u8 = 0x5;

/// * `mode` - The AddrMode indicating displacement and starting.
/// * `rm` - The R/M field in the table indicating effective address.
/// * `reg` - The REG field in the table, indicating register.
fn encode_mod_rm(mode: AddrMode, mut rm: u8, reg: Reg) -> u8 {
    // If using SIB mode, then use 4 for RM.
    // Table 2-2 (Note 1).
    if mode.mode_sib() != 0 {
        rm = 0b100;
    }
    (mode.mode_mod() << 6) | ((reg.ord7()) << 3) | (rm & 0b111)
}

fn log(scale: u32) -> u8 {
    match scale {
        1 => 0b00,
        2 => 0b01,
        4 => 0b10,
        8 => 0b11,
        _ => unreachable!("invalid scale"),
    }
}

fn encode_sib(scale: u32, (base, index, _offset): RM) -> u8 {
    let ss = log(scale);
    let i = index.ord7();
    let b = base.ord7();
    (ss << 6) | (i << 3) | b
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pushq() {
        let mut buf = [0u8; 0x100];
        let mut emit = Emitter::new(&mut buf);
        emit.pushq(Reg::RBP);
        emit.pushq(Reg::RSP);
        emit.pushq(Reg::RAX);
        assert_eq!(buf[0..3], [0x55, 0x54, 0x50]);
    }

    #[test]
    fn mov() {
        let mut buf = [0u8; 0x100];
        let mut emit = Emitter::new(&mut buf);
        emit.mov_reg_rm(S::Q, 1, Reg::RCX, (Reg::RDX, Reg::RBX, 13));
        assert_eq!(buf[0..5], [0x48, 0x8b, 0x4c, 0x1a, 0x0d]);
    }
}
