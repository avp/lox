#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum S {
    B,
    W,
    L,
    SLQ, // Sign extend long -> quad
    Q,
}

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

    R11,
    R11D,

    R12,
    R12D,

    R13,
    R13D,

    R14,
    R14D,

    R15,
    R15D,

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

    NoIndex,
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

            R11 => 11,
            R11D => 11,

            R12 => 12,
            R12D => 12,

            R13 => 13,
            R13D => 13,

            R14 => 14,
            R14D => 14,

            R15 => 15,
            R15D => 15,

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

            NoIndex => 4,
        }
    }

    pub fn ord7(&self) -> u8 {
        self.ord() & 0x7
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum AddrMode {
    AtBase,
    AtBaseDisp8,
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
            AtBase => 0b0000_0001,
            AtBaseDisp8 => 0b0001_0001,
            AtBaseDisp32 => 0b0010_0001,
            Reg => 0b0011_0000,
        }
    }

    pub fn mode_mod(&self) -> u8 {
        dbg!((self.ord() >> 4) & 0b111)
    }

    pub fn mode_sib(&self) -> u8 {
        self.ord() & 0b11
    }
}
