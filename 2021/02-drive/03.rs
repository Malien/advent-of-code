fn main() {
    let mut file = std::fs::File::open("input.txt").unwrap();
    let lines: Vec<_> = file.lines().collect();
}
