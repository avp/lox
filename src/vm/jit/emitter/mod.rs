mod cond;
mod imm;
mod reg;

pub use cond::{CCode, OffsetType};
pub use imm::Immediate;
pub use reg::{AddrMode, Reg, FP, S};

// (base, index, offset)
type RM = (Reg, Reg, i32);

pub struct Emitter<'buf> {
    pub buf: &'buf mut [u8],
    pub index: usize,
}

pub enum Scale {
    Scale(u32),
    NoScale,
    RegScale,
}

#[allow(dead_code)]
impl<'buf> Emitter<'buf> {
    pub fn new(buf: &'buf mut [u8]) -> Emitter {
        Emitter { buf, index: 0 }
    }

    fn emit(&mut self, byte: u8) {
        self.buf[self.index] = byte;
        self.index += 1;
    }

    pub fn emit_imm<T: Immediate>(&mut self, imm: T) {
        let size = std::mem::size_of::<T>();
        let buf: *mut u8 = self.buf[self.index..self.index + size].as_mut_ptr();
        let src: *const u8 = (&imm) as *const T as *const u8;
        unsafe {
            std::ptr::copy_nonoverlapping(src, buf, size);
        }
        self.index += size;
    }

    fn emit_u32(&mut self, num: u32) {
        self.emit((num & 0xff) as u8);
        self.emit(((num >> 8) & 0xff) as u8);
        self.emit(((num >> 16) & 0xff) as u8);
        self.emit(((num >> 24) & 0xff) as u8);
    }

    fn emit_rex(&mut self, s: S, (base, index, _): RM, reg_opcode: u8) {
        let b: u8 = base.ord() >> 3;
        let x: u8 = index.ord() >> 3;
        let r: u8 = reg_opcode >> 3;
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
        opcode: u8,
        reg_opcode: u8,
        rm: RM,
    ) {
        self.emit_rex(s, rm, reg_opcode);
        self.emit(opcode);
        self.emit_mod_rm(s, scale, reg_opcode, rm);
    }

    fn emit_mod_rm(
        &mut self,
        s: S,
        scale: Scale,
        reg_opcode: u8,
        (base, index, offset): RM,
    ) {
        match scale {
            Scale::NoScale => {
                if base == Reg::RSP {
                    self.emit_mod_rm(
                        s,
                        Scale::Scale(1),
                        reg_opcode,
                        (base, Reg::NoIndex, offset),
                    );
                } else {
                    let rm = base;
                    if offset == 0 {
                        self.emit(encode_mod_rm(
                            AddrMode::AtReg,
                            rm,
                            reg_opcode,
                        ));
                    } else if is_i8(offset) {
                        self.emit(encode_mod_rm(
                            AddrMode::AtRegDisp8,
                            rm,
                            reg_opcode,
                        ));
                        self.emit(offset as i8 as u8);
                    } else {
                        self.emit(encode_mod_rm(
                            AddrMode::AtRegDisp32,
                            rm,
                            reg_opcode,
                        ));
                        self.emit_u32(offset as u32);
                    }
                }
            }
            Scale::RegScale => {
                let rm = base;
                self.emit(encode_mod_rm(AddrMode::Reg, rm, reg_opcode));
            }
            Scale::Scale(scale) => {
                if offset == 0 {
                    self.emit(encode_mod_rm(
                        AddrMode::AtBase,
                        Reg::ModSIB,
                        reg_opcode,
                    ));
                    self.emit(encode_sib(scale, (base, index, offset)));
                } else if offset < 0xff {
                    self.emit(encode_mod_rm(
                        AddrMode::AtBaseDisp8,
                        Reg::ModSIB,
                        reg_opcode,
                    ));
                    self.emit(encode_sib(scale, (base, index, offset)));
                    self.emit(offset as u8);
                } else {
                    self.emit(encode_mod_rm(
                        AddrMode::AtBaseDisp32,
                        Reg::ModSIB,
                        reg_opcode,
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

    fn emit_fp_fp(&mut self, fp: FP, opcode: u8, dst: Reg, src: Reg) {
        self.emit_fptype(fp);
        self.emit(0x0f);
        self.emit(opcode);
        self.emit(encode_mod_rm(AddrMode::Reg, src, dst.ord7()));
    }

    fn emit_fp_rm(
        &mut self,
        fp: FP,
        scale: Scale,
        opcode: u8,
        dst: Reg,
        src: RM,
    ) {
        self.emit_fptype(fp);
        self.emit_rex(S::L, src, dst.ord7());
        self.emit(0x0f);
        self.emit(opcode);
        self.emit_mod_rm(S::L, scale, dst.ord7(), src);
    }

    fn emit_rm_fp(
        &mut self,
        fp: FP,
        scale: Scale,
        opcode: u8,
        dst: RM,
        src: Reg,
    ) {
        self.emit_fptype(fp);
        self.emit_rex(S::L, dst, src.ord7());
        self.emit(0x0f);
        self.emit(opcode);
        self.emit_mod_rm(S::L, scale, src.ord7(), dst);
    }

    pub fn pushq(&mut self, reg: Reg) {
        self.emit(0x50 + reg.ord7());
    }
    pub fn popq(&mut self, reg: Reg) {
        self.emit(0x58 + reg.ord7());
    }

    pub fn op_reg_rm(
        &mut self,
        s: S,
        scale: Scale,
        opcode: u8,
        dst: Reg,
        src: RM,
    ) {
        self.emit_mod_rm_full(
            s,
            scale,
            if s == S::B {
                opcode
            } else {
                opcode + 1
            },
            dst.ord7(),
            src,
        );
    }
    pub fn op_rm_imm<T: Immediate>(
        &mut self,
        s: S,
        scale: Scale,
        opcode: u8,
        opcode_extra: u8,
        dst: RM,
        src: T,
    ) {
        self.emit_mod_rm_full(
            s,
            scale,
            if s == S::B {
                opcode
            } else {
                opcode + 1
            },
            opcode_extra,
            dst,
        );
        self.emit_imm(src);
    }

    pub fn mov_reg_reg(&mut self, s: S, dst: Reg, src: Reg) {
        assert!(!dst.is_fp() && !src.is_fp());
        self.mov_rm_reg(s, Scale::RegScale, (dst, Reg::NoIndex, 0), src);
    }
    pub fn mov_rm_reg(&mut self, s: S, scale: Scale, dst: RM, src: Reg) {
        self.op_reg_rm(s, scale, 0x88, src, dst);
    }
    pub fn mov_reg_rm(&mut self, s: S, scale: Scale, dst: Reg, src: RM) {
        assert!(!dst.is_fp());
        self.op_reg_rm(s, scale, 0x8a, dst, src);
    }
    pub fn mov_fp_fp(&mut self, fp: FP, dst: Reg, src: Reg) {
        assert!(dst.is_fp() && src.is_fp());
        self.emit_fp_fp(fp, 0x10, dst, src);
    }
    pub fn mov_rm_fp(&mut self, fp: FP, scale: Scale, dst: RM, src: Reg) {
        assert!(src.is_fp());
        self.emit_rm_fp(fp, scale, 0x11, dst, src);
    }
    pub fn mov_fp_rm(&mut self, fp: FP, scale: Scale, dst: Reg, src: RM) {
        assert!(dst.is_fp());
        self.emit_fp_rm(fp, scale, 0x10, dst, src);
    }
    pub fn mov_reg_imm<T: Immediate>(&mut self, reg: Reg, imm: T)
    where
        T: std::fmt::Debug,
    {
        let s = T::s();
        self.emit_rex(s, (reg, Reg::NoIndex, 0), Reg::NONE.ord7());
        self.emit(if s == S::B { 0xb0 } else { 0xb8 } + reg.ord7());
        self.emit_imm(imm);
    }

    pub fn add_reg_reg(&mut self, s: S, dst: Reg, src: Reg) {
        self.add_rm_reg(s, Scale::RegScale, (dst, Reg::NoIndex, 0), src);
    }
    pub fn add_rm_reg(&mut self, s: S, scale: Scale, dst: RM, src: Reg) {
        self.op_reg_rm(s, scale, 0x00, src, dst);
    }
    pub fn add_reg_rm(&mut self, s: S, scale: Scale, dst: Reg, src: RM) {
        self.op_reg_rm(s, scale, 0x02, dst, src);
    }
    pub fn add_reg_imm<T: Immediate>(&mut self, s: S, reg: Reg, imm: T) {
        self.emit_rex(s, (reg, Reg::NoIndex, 0), Reg::NONE.ord7());
        self.emit(if s == S::B { 0x80 } else { 0x81 });
        self.emit(encode_mod_rm(AddrMode::Reg, reg, 0));
        self.emit_imm(imm);
    }

    pub fn sub_reg_imm<T: Immediate>(&mut self, s: S, reg: Reg, imm: T) {
        self.emit_rex(s, (reg, Reg::NoIndex, 0), Reg::NONE.ord7());
        self.emit(if s == S::B { 0x80 } else { 0x81 });
        self.emit(encode_mod_rm(AddrMode::Reg, reg, 5));
        self.emit_imm(imm);
    }

    pub fn cmp_reg_rm(&mut self, s: S, scale: Scale, dst: Reg, src: RM) {
        self.op_reg_rm(s, scale, 0x3a, dst, src);
    }
    pub fn cmp_rm_reg(&mut self, s: S, scale: Scale, dst: RM, src: Reg) {
        self.op_reg_rm(s, scale, 0x38, src, dst);
    }
    pub fn cmp_rm_imm<T: Immediate>(
        &mut self,
        s: S,
        scale: Scale,
        dst: RM,
        src: T,
    ) {
        self.op_rm_imm(s, scale, 0x80, 7, dst, src);
    }

    pub fn add_fp_fp(&mut self, fp: FP, dst: Reg, src: Reg) {
        assert!(dst.is_fp() && src.is_fp());
        self.emit_fp_fp(fp, 0x58, dst, src);
    }
    pub fn sub_fp_fp(&mut self, fp: FP, dst: Reg, src: Reg) {
        assert!(dst.is_fp() && src.is_fp());
        self.emit_fp_fp(fp, 0x5c, dst, src);
    }
    pub fn mul_fp_fp(&mut self, fp: FP, dst: Reg, src: Reg) {
        assert!(dst.is_fp() && src.is_fp());
        self.emit_fp_fp(fp, 0x59, dst, src);
    }
    pub fn div_fp_fp(&mut self, fp: FP, dst: Reg, src: Reg) {
        assert!(dst.is_fp() && src.is_fp());
        self.emit_fp_fp(fp, 0x5e, dst, src);
    }

    pub fn call_reg(&mut self, dst: Reg) {
        self.emit_mod_rm_full(
            S::L,
            Scale::RegScale,
            0xff,
            0x02,
            (dst, Reg::NoIndex, 2),
        );
    }

    pub fn cjump(&mut self, cond: CCode, t: OffsetType, target: i32) {
        // Account for the two bytes of the instruction itself.
        let offset = target - self.index as i32 - 2;
        match t {
            OffsetType::Auto => self.cjump(
                cond,
                if is_i8(offset) {
                    OffsetType::Int8
                } else {
                    OffsetType::Int32
                },
                offset,
            ),
            OffsetType::Int8 => {
                self.emit(cond.opcode());
                self.emit_imm::<i8>(offset as i8);
            }
            OffsetType::Int32 => {
                // This one's 6 bytes long so subtract another 4.
                self.emit(0x0f);
                self.emit(cond.opcode() + 0x10);
                self.emit_imm::<i32>(offset - 4);
            }
        }
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
fn encode_mod_rm(mode: AddrMode, rm: Reg, reg_opcode: u8) -> u8 {
    // If using SIB mode, then use 4 for RM.
    // Table 2-2 (Note 1).
    let rm = if mode.mode_sib() != 0 {
        0b100
    } else {
        rm.ord7()
    };
    (mode.mode_mod() << 6) | (reg_opcode << 3) | (rm & 0b111)
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

fn is_i8(x: i32) -> bool {
    x as u8 as i8 as i32 == x
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

    macro_rules! check_str {
        ($emit:expr, $str:expr) => {{
            use capstone::arch::x86::*;
            use capstone::arch::*;
            use capstone::*;
            let cs = Capstone::new()
                .x86()
                .mode(ArchMode::Mode64)
                .detail(false)
                .build()
                .unwrap();
            let asm = format!("{}", cs.disasm_count($emit.buf, 0, 1).unwrap());
            assert!(
                asm.trim().ends_with($str),
                format!("Found: {}", asm)
            );
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
    fn sub_rsp() {
        let mut buf = [0u8; 0x100];
        let mut e = Emitter::new(&mut buf);
        reset!(e);
        e.sub_reg_imm(S::Q, Reg::RSP, 0xabu8);
        check!(e, [0x48, 0x81, 0xec, 0xab]);
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
        e.mov_rm_reg(
            S::Q,
            Scale::NoScale,
            (Reg::RCX, Reg::NoIndex, 0xabcd),
            Reg::RAX,
        );
        check!(e, [0x48, 0x89, 0x81, 0xcd, 0xab, 0x00, 0x00]);
        e.mov_rm_reg(
            S::Q,
            Scale::NoScale,
            (Reg::RCX, Reg::NoIndex, -0xabcd),
            Reg::RAX,
        );
        check!(e, [0x48, 0x89, 0x81, 0x33, 0x54, 0xff, 0xff]);
        e.mov_rm_reg(
            S::Q,
            Scale::NoScale,
            (Reg::RCX, Reg::NoIndex, -0xabcd),
            Reg::RAX,
        );
        check!(e, [0x48, 0x89, 0x81, 0x33, 0x54, 0xff, 0xff]);
        e.mov_rm_reg(
            S::Q,
            Scale::NoScale,
            (Reg::RBP, Reg::NoIndex, -0x8),
            Reg::RAX,
        );
        check!(e, [0x48, 0x89, 0x45, 0xf8]);
        e.mov_reg_reg(S::Q, Reg::RCX, Reg::RDX);
        check!(e, [0x48, 0x89, 0xd1]);
        e.mov_reg_imm(Reg::RCX, 0xaabbccddeeffu64);
        check!(
            e,
            [0x48, 0xb9, 0xff, 0xee, 0xdd, 0xcc, 0xbb, 0xaa, 0x00, 0x00,]
        );
    }

    #[test]
    fn add() {
        let mut buf = [0u8; 0x100];
        let mut e = Emitter::new(&mut buf);
        reset!(e);
        e.add_reg_reg(S::Q, Reg::RCX, Reg::RDX);
        check!(e, [0x48, 0x01, 0xd1]);
    }

    #[test]
    fn cmp() {
        let mut buf = [0u8; 0x100];
        let mut e = Emitter::new(&mut buf);
        reset!(e);
        e.cmp_reg_rm(
            S::Q,
            Scale::NoScale,
            Reg::RAX,
            (Reg::RBX, Reg::NoIndex, 0),
        );
        check_str!(e, "cmp rax, qword ptr [rbx]");
        e.cmp_rm_reg(
            S::Q,
            Scale::NoScale,
            (Reg::RBX, Reg::NoIndex, 0),
            Reg::RAX,
        );
        check_str!(e, "cmp qword ptr [rbx], rax");
        e.cmp_rm_imm(
            S::Q,
            Scale::NoScale,
            (Reg::RBX, Reg::NoIndex, 0),
            0xabu32,
        );
        check_str!(e, "cmp qword ptr [rbx], 0xab");
    }

    #[test]
    fn cjump() {
        let mut buf = [0u8; 0x100];
        let mut e = Emitter::new(&mut buf);
        reset!(e);
        e.cjump(CCode::E, OffsetType::Int8, 0x12);
        check_str!(e, "je 0x12");
        e.cjump(CCode::E, OffsetType::Int8, -0x12);
        check_str!(e, "je 0xffffffffffffffee");
        e.cjump(CCode::E, OffsetType::Int8, 0x0);
        check_str!(e, "je 0");
    }

    #[test]
    fn fp() {
        let mut buf = [0u8; 0x100];
        let mut e = Emitter::new(&mut buf);
        reset!(e);
        e.mov_rm_fp(
            FP::Double,
            Scale::NoScale,
            (Reg::RAX, Reg::NoIndex, 0),
            Reg::XMM0,
        );
        check!(e, [0xf2, 0x0f, 0x11, 0x00]);
        e.mov_fp_rm(
            FP::Double,
            Scale::NoScale,
            Reg::XMM0,
            (Reg::RAX, Reg::NoIndex, 0),
        );
        check!(e, [0xf2, 0x0f, 0x10, 0x00]);
        e.mov_fp_fp(FP::Double, Reg::XMM0, Reg::XMM1);
        check!(e, [0xf2, 0x0f, 0x10, 0xc1]);
        e.add_fp_fp(FP::Double, Reg::XMM0, Reg::XMM1);
        check!(e, [0xf2, 0x0f, 0x58, 0xc1]);
        e.mov_fp_rm(
            FP::Double,
            Scale::NoScale,
            Reg::XMM0,
            (Reg::RSP, Reg::NoIndex, 0),
        );
        check!(e, [0xf2, 0x0f, 0x10, 0x04, 0x24]);
    }

    #[test]
    fn call_rm() {
        let mut buf = [0u8; 0x100];
        let mut e = Emitter::new(&mut buf);
        reset!(e);
        e.call_reg(Reg::RAX);
        check!(e, [0xff, 0xd0]);
    }

    #[test]
    fn is_i8() {
        use super::is_i8;
        assert!(is_i8(-120));
        assert!(is_i8(120));
        assert!(!is_i8(300));
        assert!(!is_i8(-300));
    }
}
