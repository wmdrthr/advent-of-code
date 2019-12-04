
pub type V2 = (i32, i32);

pub fn manhattan(a: V2, b: V2) -> i32 {
    (a.0 - b.0).abs() + (a.1 - b.1).abs()
}
