pub fn fib4(n: u32) -> u32 {
    if n == 0 {
        0
    } else {
        if n == 1 {
            1
        } else {
            fib4(n - 1) + fib4(n - 2)
        }
    }
}

pub fn fib8(n: u32) -> u64 {
    if n == 0 {
        0
    } else {
        if n == 1 {
            1
        } else {
            fib8(n - 1) + fib8(n - 2)
        }
    }
}

pub fn ffact4(n: u32) -> u32 {
    let mut i: u32 = n;
    let mut ans: u32 = 1;
    while i > 2 {
        ans *= i;
        i -= 2;
    }
    ans * i
}

pub fn ffact8(n: u32) -> u64 {
    let mut i: u64 = u64::from(n);
    let mut ans: u64 = 1;
    while i > 2 {
        ans *= i;
        i -= 2;
    }
    ans * i
}

#[test]
fn test_impls() {
    assert_eq!(21, fib4(8));
    assert_eq!(21, fib8(8));

    assert_eq!(7 * 5 * 3, ffact4(7));
    assert_eq!(8 * 6 * 4 * 2, ffact4(8));
    assert_eq!(7 * 5 * 3, ffact8(7));
    assert_eq!(8 * 6 * 4 * 2, ffact8(8));
}
