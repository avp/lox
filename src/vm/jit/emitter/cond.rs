#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum CCode {
    A,
    AE,
    B,
    BE,
    C,
    E,
    G,
    GE,
    L,
    LE,
    NA,
    NAE,
    NB,
    NBE,
    NC,
    NE,
    NG,
    NGE,
    NL,
    NLE,
    NO,
    O,
    Z,
}

impl CCode {
    pub fn opcode(&self) -> u8 {
        use CCode::*;
        match *self {
            A => 0x77,
            AE => 0x73,
            B => 0x72,
            BE => 0x76,
            C => 0x72,
            E => 0x74,
            G => 0x7f,
            GE => 0x7d,
            L => 0x7c,
            LE => 0x7e,
            NA => 0x76,
            NAE => 0x72,
            NB => 0x73,
            NBE => 0x77,
            NC => 0x73,
            NE => 0x75,
            NG => 0x7e,
            NGE => 0x7c,
            NL => 0x7d,
            NLE => 0x7f,
            NO => 0x71,
            O => 0x70,
            Z => 0x74,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum OffsetType {
    Auto,
    Int8,
    Int32,
}
