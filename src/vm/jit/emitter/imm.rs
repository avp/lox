use super::S;

pub trait Immediate: Copy + std::fmt::Debug {
    fn s() -> S;
}

impl Immediate for u8 {
    fn s() -> S {
        S::B
    }
}

impl Immediate for u32 {
    fn s() -> S {
        S::L
    }
}

impl Immediate for u64 {
    fn s() -> S {
        S::Q
    }
}

impl Immediate for usize {
    fn s() -> S {
        if std::mem::size_of::<usize>() == 8 {
            S::Q
        } else {
            S::L
        }
    }
}

impl Immediate for f64 {
    fn s() -> S {
        S::Q
    }
}
