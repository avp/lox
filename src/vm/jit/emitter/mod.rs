mod reg;

pub use reg::{AddrMode, Reg, FP, S};

// (base, index, offset)
type RM = (Reg, Reg, i32);

struct Emitter<'buf> {
    buf: &'buf mut [u8],
    index: usize,
}

enum Scale {
    Scale(u32),
    NoScale,
    RegScale,
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

    fn emit_rex(&mut self, s: S, (base, index, _): RM, reg: Reg) {
        let b: u8 = base.ord() >> 3;
        let x: u8 = index.ord() >> 3;
        let r: u8 = reg.ord() >> 3;
        let w: u8 = (s == S::Q || s == S::SLQ).into();
        if (w, b, x, r) != (0, 0, 0, 0) {
            // REX is required.
            let rex = 0b0100_0000 | (w << 3) | (r << 2) | (x << 1) | b;
            self.emit(rex);
        }
    }

    fn emit_mod_rm_full(
        &mut self,
        s: S,
        scale: Scale,
        mut opcode: u8,
        reg: Reg,
        rm: RM,
    ) {
        let (base, index, _) = rm;
        if s != S::B {
            opcode += 1;
        }
        self.emit_rex(s, rm, reg);
        self.emit(opcode);
        self.emit_mod_rm(s, scale, reg, rm);
    }

    fn emit_mod_rm(
        &mut self,
        s: S,
        scale: Scale,
        reg: Reg,
        (base, index, offset): RM,
    ) {
        match scale {
            Scale::NoScale => {
                let rm = base;
                if offset == 0 {
                    self.emit(encode_mod_rm(AddrMode::AtReg, rm, reg));
                } else if offset < 0xff {
                    self.emit(encode_mod_rm(AddrMode::AtRegDisp8, rm, reg));
                    self.emit(offset as u8);
                } else {
                    self.emit(encode_mod_rm(AddrMode::AtRegDisp32, rm, reg));
                    self.emit_u32(offset as u32);
                }
            }
            Scale::RegScale => {
                let rm = base;
                self.emit(encode_mod_rm(AddrMode::Reg, rm, reg));
            }
            Scale::Scale(scale) => {
                if offset == 0 {
                    self.emit(encode_mod_rm(
                        AddrMode::AtBase,
                        Reg::ModSIB,
                        reg,
                    ));
                    self.emit(encode_sib(scale, (base, index, offset)));
                } else if offset < 0xff {
                    self.emit(encode_mod_rm(
                        AddrMode::AtBaseDisp8,
                        Reg::ModSIB,
                        reg,
                    ));
                    self.emit(encode_sib(scale, (base, index, offset)));
                    self.emit(offset as u8);
                } else {
                    self.emit(encode_mod_rm(
                        AddrMode::AtBaseDisp32,
                        Reg::ModSIB,
                        reg,
                    ));
                    self.emit(encode_sib(scale, (base, index, offset)));
                    self.emit_u32(offset as u32);
                }
            }
        }
    }

    fn emit_fptype(&mut self, fp: FP) {
        self.emit(match fp {
            FP::Double => 0xf2,
            FP::Float => 0xf3,
        })
    }

    fn emit_reg_fpreg(&mut self, fp: FP, opcode: u8, dst: Reg, src: Reg) {
        self.emit_fptype(fp);
        self.emit(0x0f);
        self.emit(opcode);
        self.emit(encode_mod_rm(AddrMode::Reg, src, dst));
    }

    fn emit_fpreg_rm(
        &mut self,
        fp: FP,
        scale: Scale,
        opcode: u8,
        dst: Reg,
        src: RM,
    ) {
        self.emit_fptype(fp);
        self.emit_rex(S::L, src, dst);
        self.emit(0x0f);
        self.emit(opcode);
        self.emit_mod_rm(S::L, scale, dst, src);
    }

    fn emit_rm_fpreg(
        &mut self,
        fp: FP,
        scale: Scale,
        opcode: u8,
        dst: RM,
        src: Reg,
    ) {
        self.emit_fptype(fp);
        self.emit_rex(S::L, dst, src);
        self.emit(0x0f);
        self.emit(opcode);
        self.emit_mod_rm(S::L, scale, src, dst);
    }

    pub fn pushq(&mut self, reg: Reg) {
        self.emit(0x50 + reg.ord7());
    }
    pub fn popq(&mut self, reg: Reg) {
        self.emit(0x58 + reg.ord7());
    }

    pub fn mov_reg_reg(&mut self, s: S, dst: Reg, src: Reg) {
        self.mov_rm_reg(s, Scale::RegScale, (dst, Reg::NoIndex, 0), src);
    }
    pub fn mov_rm_reg(&mut self, s: S, scale: Scale, dst: RM, src: Reg) {
        self.emit_mod_rm_full(s, scale, 0x88, src, dst);
    }
    pub fn mov_reg_rm(&mut self, s: S, scale: Scale, dst: Reg, src: RM) {
        self.emit_mod_rm_full(s, scale, 0x8a, dst, src);
    }
    pub fn mov_fpreg_fpreg(&mut self, fp: FP, dst: Reg, src: Reg) {
        assert!(dst.is_fp() && src.is_fp());
        self.emit_reg_fpreg(fp, 0x10, dst, src);
    }
    pub fn mov_rm_fpreg(&mut self, fp: FP, scale: Scale, dst: RM, src: Reg) {
        assert!(src.is_fp());
        self.emit_rm_fpreg(fp, scale, 0x11, dst, src);
    }
    pub fn mov_fpreg_rm(&mut self, fp: FP, scale: Scale, dst: Reg, src: RM) {
        assert!(dst.is_fp());
        self.emit_fpreg_rm(fp, scale, 0x10, dst, src);
    }

    pub fn leave(&mut self) {
        self.emit(0xc9);
    }
    pub fn ret(&mut self) {
        self.emit(0xc3);
    }
}

/// * `mode` - The AddrMode indicating displacement and starting.
/// * `rm` - The R/M field in the table indicating effective address.
/// * `reg` - The REG field in the table, indicating register.
fn encode_mod_rm(mode: AddrMode, rm: Reg, reg: Reg) -> u8 {
    // If using SIB mode, then use 4 for RM.
    // Table 2-2 (Note 1).
    let rm = if mode.mode_sib() != 0 {
        0b100
    } else {
        rm.ord7()
    };
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
    // 0bSSIIIBBB
    let ss = log(scale);
    let iii = index.ord7();
    let bbb = base.ord7();
    (ss << 6) | (iii << 3) | bbb
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! reset {
        ($emit:expr) => {{
            $emit.index = 0;
            for i in 0..($emit.buf.len()) {
                $emit.buf[i] = 0xff;
            }
        }};
    }

    macro_rules! check {
        ($emit:expr, $buf:expr) => {{
            let expected = $buf;
            let len = expected.len();
            assert_eq!($emit.buf[0..len], expected);
            assert_eq!($emit.buf[len], 0xff);
            reset!($emit);
        }};
    }

    #[test]
    fn pushq() {
        let mut buf = [0u8; 0x100];
        let mut e = Emitter::new(&mut buf);
        reset!(e);
        e.pushq(Reg::RBP);
        check!(e, [0x55]);
        e.pushq(Reg::RSP);
        check!(e, [0x54]);
        e.pushq(Reg::RAX);
        check!(e, [0x50]);
    }

    #[test]
    fn mov() {
        let mut buf = [0u8; 0x100];
        let mut e = Emitter::new(&mut buf);
        reset!(e);
        e.mov_reg_rm(
            S::Q,
            Scale::Scale(1),
            Reg::RCX,
            (Reg::RDX, Reg::RBX, 9),
        );
        check!(e, [0x48, 0x8b, 0x4c, 0x1a, 0x09]);
        e.mov_reg_rm(
            S::Q,
            Scale::NoScale,
            Reg::RCX,
            (Reg::RDX, Reg::NoIndex, 0),
        );
        check!(e, [0x48, 0x8b, 0x0a]);
        e.mov_reg_rm(
            S::Q,
            Scale::Scale(1),
            Reg::RCX,
            (Reg::RDX, Reg::RBX, 0),
        );
        check!(e, [0x48, 0x8b, 0x0c, 0x1a]);
        e.mov_rm_reg(
            S::Q,
            Scale::NoScale,
            (Reg::RCX, Reg::NoIndex, 0),
            Reg::RDX,
        );
        check!(e, [0x48, 0x89, 0x11]);
        e.mov_reg_reg(S::Q, Reg::RCX, Reg::RDX);
        check!(e, [0x48, 0x89, 0xd1]);
    }

    #[test]
    fn fp() {
        let mut buf = [0u8; 0x100];
        let mut e = Emitter::new(&mut buf);
        reset!(e);
        e.mov_rm_fpreg(
            FP::Double,
            Scale::NoScale,
            (Reg::RAX, Reg::NoIndex, 0),
            Reg::XMM0,
        );
        check!(e, [0xf2, 0x0f, 0x11, 0x00]);
        e.mov_fpreg_rm(
            FP::Double,
            Scale::NoScale,
            Reg::XMM0,
            (Reg::RAX, Reg::NoIndex, 0),
        );
        check!(e, [0xf2, 0x0f, 0x10, 0x00]);
        e.mov_fpreg_fpreg(FP::Double, Reg::XMM0, Reg::XMM1);
        check!(e, [0xf2, 0x0f, 0x10, 0xc1]);
    }
}
