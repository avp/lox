#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum S {
    B,
    W,
    L,
    SLQ, // Sign extend long -> quad
    Q,
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum FP {
    Float,
    Double,
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Reg {
    RAX,
    EAX,
    AX,
    AL,

    RCX,
    ECX,
    CX,
    CL,

    RDX,
    EDX,
    DX,
    DL,

    RBX,
    EBX,
    BX,
    BL,

    RSP,
    ESP,
    SP,
    AH,

    RBP,
    EBP,
    BP,
    CH,

    RSI,
    ESI,
    SI,
    DH,

    RDI,
    EDI,
    DI,
    BH,

    R8,
    R8D,
    R8W,
    R8B,

    R9,
    R9D,
    R9W,
    R9B,

    R10,
    R10D,
    R10W,
    R10B,

    R11,
    R11D,
    R11W,
    R11B,

    R12,
    R12D,
    R12W,
    R12B,

    R13,
    R13D,
    R13W,
    R13B,

    R14,
    R14D,
    R14W,
    R14B,

    R15,
    R15D,
    R15W,
    R15B,

    MM0,
    XMM0,
    MM1,
    XMM1,
    MM2,
    XMM2,
    MM3,
    XMM3,
    MM4,
    XMM4,
    MM5,
    XMM5,
    MM6,
    XMM6,
    MM7,
    XMM7,

    NONE,
    NoIndex,
    ModSIB,
}

impl Reg {
    pub fn ord(&self) -> u8 {
        use Reg::*;
        match self {
            RAX => 0,
            EAX => 0,
            AX => 0,
            AL => 0,

            RCX => 1,
            ECX => 1,
            CX => 1,
            CL => 1,

            RDX => 2,
            EDX => 2,
            DX => 2,
            DL => 2,

            RBX => 3,
            EBX => 3,
            BX => 3,
            BL => 3,

            RSP => 4,
            ESP => 4,
            SP => 4,
            AH => 4,

            RBP => 5,
            EBP => 5,
            BP => 5,
            CH => 5,

            RSI => 6,
            ESI => 6,
            SI => 6,
            DH => 6,

            RDI => 7,
            EDI => 7,
            DI => 7,
            BH => 7,

            R8 => 8,
            R8D => 8,
            R8W => 8,
            R8B => 8,

            R9 => 9,
            R9D => 9,
            R9W => 9,
            R9B => 9,

            R10 => 10,
            R10D => 10,
            R10W => 10,
            R10B => 10,

            R11 => 11,
            R11D => 11,
            R11W => 11,
            R11B => 11,

            R12 => 12,
            R12D => 12,
            R12W => 12,
            R12B => 12,

            R13 => 13,
            R13D => 13,
            R13W => 13,
            R13B => 13,

            R14 => 14,
            R14D => 14,
            R14W => 14,
            R14B => 14,

            R15 => 15,
            R15D => 15,
            R15W => 15,
            R15B => 15,

            MM0 => 0,
            XMM0 => 0,
            MM1 => 1,
            XMM1 => 1,
            MM2 => 2,
            XMM2 => 2,
            MM3 => 3,
            XMM3 => 3,
            MM4 => 4,
            XMM4 => 4,
            MM5 => 5,
            XMM5 => 5,
            MM6 => 6,
            XMM6 => 6,
            MM7 => 7,
            XMM7 => 7,

            NONE => 0,
            NoIndex => 4,
            ModSIB => 5,
        }
    }

    pub fn low_word(&self) -> Reg {
        use Reg::*;
        match self {
            RAX => EAX,
            EAX => EAX,
            AX => EAX,
            AL => EAX,

            RCX => ECX,
            ECX => ECX,
            CX => ECX,
            CL => ECX,

            RDX => EDX,
            EDX => EDX,
            DX => EDX,
            DL => EDX,

            RBX => EBX,
            EBX => EBX,
            BX => EBX,
            BL => EBX,

            RSP => ESP,
            ESP => ESP,
            SP => ESP,
            AH => ESP,

            RBP => EBP,
            EBP => EBP,
            BP => EBP,
            CH => EBP,

            RSI => ESI,
            ESI => ESI,
            SI => ESI,
            DH => ESI,

            RDI => EDI,
            EDI => EDI,
            DI => EDI,
            BH => EDI,

            R8 => R8W,
            R8D => R8W,
            R8W => R8W,
            R8B => R8W,

            R9 => R9W,
            R9D => R9W,
            R9W => R9W,
            R9B => R9W,

            R10 => R10W,
            R10D => R10W,
            R10W => R10W,
            R10B => R10W,

            R11 => R11W,
            R11D => R11W,
            R11W => R11W,
            R11B => R11W,

            R12 => R12W,
            R12D => R12W,
            R12W => R12W,
            R12B => R12W,

            R13 => R13W,
            R13D => R13W,
            R13W => R13W,
            R13B => R13W,

            R14 => R14W,
            R14D => R14W,
            R14W => R14W,
            R14B => R14W,

            R15 => R15W,
            R15D => R15W,
            R15W => R15W,
            R15B => R15W,

            _ => panic!("Invalid low byte: {:?}", self),
        }
    }

    pub fn low_byte(&self) -> Reg {
        use Reg::*;
        match self {
            RAX => AL,
            EAX => AL,
            AX => AL,
            AL => AL,

            RCX => CL,
            ECX => CL,
            CX => CL,
            CL => CL,

            RDX => DL,
            EDX => DL,
            DX => DL,
            DL => DL,

            RBX => BL,
            EBX => BL,
            BX => BL,
            BL => BL,

            RSP => AH,
            ESP => AH,
            SP => AH,
            AH => AH,

            RBP => CH,
            EBP => CH,
            BP => CH,
            CH => CH,

            RSI => DH,
            ESI => DH,
            SI => DH,
            DH => DH,

            RDI => BH,
            EDI => BH,
            DI => BH,
            BH => BH,

            R8 => R8B,
            R8D => R8B,
            R8W => R8B,
            R8B => R8B,

            R9 => R9B,
            R9D => R9B,
            R9W => R9B,
            R9B => R9B,

            R10 => R10B,
            R10D => R10B,
            R10W => R10B,
            R10B => R10B,

            R11 => R11B,
            R11D => R11B,
            R11W => R11B,
            R11B => R11B,

            R12 => R12B,
            R12D => R12B,
            R12W => R12B,
            R12B => R12B,

            R13 => R13B,
            R13D => R13B,
            R13W => R13B,
            R13B => R13B,

            R14 => R14B,
            R14D => R14B,
            R14W => R14B,
            R14B => R14B,

            R15 => R15B,
            R15D => R15B,
            R15W => R15B,
            R15B => R15B,

            _ => panic!("Invalid low byte: {:?}", self),
        }
    }

    pub fn ord7(&self) -> u8 {
        self.ord() & 0x7
    }

    pub fn is_fp(&self) -> bool {
        use Reg::*;
        match self {
            MM0 | XMM0 | MM1 | XMM1 | MM2 | XMM2 | MM3 | XMM3 | MM4 | XMM4
            | MM5 | XMM5 | MM6 | XMM6 | MM7 | XMM7 => true,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum AddrMode {
    AtReg,
    AtBase,

    AtRegDisp8,
    AtBaseDisp8,

    AtRegDisp32,
    AtBaseDisp32,

    Reg,
}

impl AddrMode {
    fn ord(&self) -> u8 {
        use AddrMode::*;
        // 0b0mMM_00SS
        // m is special
        // MM is the mod
        // SS is the SIB indicator
        match self {
            AtReg => 0b0000_0000,
            AtBase => 0b0000_0001,

            AtRegDisp8 => 0b0001_0000,
            AtBaseDisp8 => 0b0001_0001,

            AtRegDisp32 => 0b0010_0000,
            AtBaseDisp32 => 0b0010_0001,

            Reg => 0b0011_0000,
        }
    }

    pub fn mode_mod(&self) -> u8 {
        (self.ord() >> 4) & 0b111
    }

    pub fn mode_sib(&self) -> u8 {
        self.ord() & 0b11
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mode_mod() {
        assert_eq!(AddrMode::AtRegDisp32.mode_mod(), 0b10);
    }
}
