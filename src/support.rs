use std::mem::transmute;

pub unsafe fn slice_get_2_mut<T>(
    t: &mut [T],
    i1: usize,
    i2: usize,
) -> (&mut T, &mut T) {
    debug_assert!(i1 != i2, "Can't reference same index twice");
    let ref1 = &mut *transmute::<&mut T, *mut T>(&mut t[i1]);
    let ref2 = &mut *transmute::<&mut T, *mut T>(&mut t[i2]);
    (ref1, ref2)
}
