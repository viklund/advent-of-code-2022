use std::fs;

fn main() {
    let contents = fs::read_to_string("01.input").expect("Should have been a file");
    let mut maxs  = vec![0,0,0];
    let mut count = 0;

    for line in contents.lines() {
        match line.parse::<i32>() {
            Ok(v) => count += v,
            Err(_) => {
                for i in &mut maxs {
                    if count > *i {
                        let tmp = *i;
                        *i = count;
                        count = tmp;
                    }
                }
                count = 0;
            }
        }
    }
    // Make sure we get the last one as well
    if count > 0 {
        for i in &mut maxs {
            if count > *i {
                let tmp = *i;
                *i = count;
                count = tmp;
            }
        }
    }

    println!("{}", &maxs[0]);
    println!("{}", &maxs[0] + &maxs[1] + &maxs[2]);
}
