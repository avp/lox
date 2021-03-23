use std::mem::transmute;

pub unsafe fn slice_get_2_mut<T>(
    t: &mut [T],
    i1: usize,
    i2: usize,
) -> (&mut T, &mut T) {
    debug_assert!(i1 != i2, "Can't reference same index twice");
    let ref1 = &mut *(&mut t[i1] as *mut T);
    let ref2 = &mut *(&mut t[i2] as *mut T);
    (ref1, ref2)
}
