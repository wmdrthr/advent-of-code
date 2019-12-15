fn solve_for_dimensions(data: String, width: usize, height: usize) {
    let pixels: Vec<usize> = data
        .chars()
        .map(|i| i.to_digit(10).unwrap() as usize)
        .collect();
    let area: usize = (width * height) as usize;

    // Part 1
    let mut min_zeros = width;
    let mut product: usize = 0;
    pixels.chunks_exact(area).for_each(|layer| {
        let zeros: usize = layer.iter().filter(|p| **p == 0).count();
        if zeros < min_zeros {
            min_zeros = zeros;
            product = layer.iter().filter(|p| **p == 1).count()
                * layer.iter().filter(|p| **p == 2).count();
        }
    });
    println!("Part 1: {}", product);

    // Part 2
    let num_layers = data.len() / area;
    let mut image: Vec<usize> = Vec::with_capacity(area);

    for n in 0..area {
        for l in 0..num_layers {
            let pixel = pixels[(l * area) + n];
            if pixel != 2 {
                image.push(pixel);
                break;
            }
        }
    }

    for row in 0..height {
        for col in 0..width {
            match image[row * width + col] {
                0 => print!("░ "),
                1 => print!("█ "),
                _ => panic!("world gone mad"),
            }
        }
        println!();
    }
    println!("CYKBY");
}

// Day 8: Space Image Format

pub fn solve(data: String) {
    solve_for_dimensions(data, 25, 6);
}
