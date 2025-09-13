use std::convert::TryInto;
use std::io;
use std::io::Write;

use rand::seq::SliceRandom;

type Code = [u8; 4];

fn main() {
    let secret = generate_code();
    println!("Welcome to Mastermind!");
    let mut attempts = 10;

    while attempts > 0 {
        print!("You have {} attempts left. Insert your guess: ", attempts);
        let _ = io::stdout().flush();
        let stdin = io::stdin().lock();
        if let Ok(guess) = user_input(stdin) {
            let x = xes(&guess, &secret);
            let o = oes(&guess, &secret);
            println!("Result: {}", output(x, o));

            if x == 4 {
                println!("Congratulations, you won!");
                return;
            } else {
                attempts -= 1;
            }
        } else {
            println!("Invalid input, insert a 4 digit value (no duplicates)");
        }
    }
    println!("You lose! The code was {:?}", secret);
}

fn generate_code() -> Code {
    let mut rng = rand::rng();
    let mut numbers: Vec<u8> = (0..10u8).collect();
    numbers.shuffle(&mut rng);
    numbers
        .into_iter()
        .take(4)
        .collect::<Vec<u8>>()
        .try_into()
        .unwrap()
}

fn user_input<R: io::BufRead>(mut reader: R) -> Result<Code, &'static str> {
    let mut line = String::new();
    reader
        .read_line(&mut line)
        .map_err(|_| "error reading from stdin")?;

    let mut numbers: Vec<u8> = line
        .chars()
        .filter(|c| ('0'..='9').contains(c))
        .map(|c| (c as u8) - 48)
        .collect();

    numbers.dedup();

    Ok(numbers.try_into().map_err(|_| "invalid input")?)
}

fn output(x: u8, o: u8) -> String {
    let xes = "X".repeat(x as usize);
    let oes = "O".repeat(o as usize);

    xes + &oes
}

fn oes(guess: &Code, secret: &Code) -> u8 {
    let mut ct = 0;
    for i in 0..4 {
        for j in 0..4 {
            if i != j && guess[i] == secret[j] {
                ct += 1;
            }
        }
    }

    ct
}

fn xes(guess: &Code, secret: &Code) -> u8 {
    (0..4usize).filter(|&i| guess[i] == secret[i]).count() as u8
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_xes() {
        assert_eq!(0, xes(&[1, 2, 3, 4], &[5, 6, 7, 8]));
        assert_eq!(1, xes(&[1, 2, 3, 4], &[5, 6, 3, 7]));
        assert_eq!(2, xes(&[1, 2, 3, 4], &[1, 2, 4, 5]));
        assert_eq!(3, xes(&[1, 2, 3, 4], &[1, 2, 3, 5]));
        assert_eq!(4, xes(&[1, 2, 3, 4], &[1, 2, 3, 4]));
    }

    #[test]
    fn test_oes() {
        assert_eq!(0, oes(&[1, 2, 3, 4], &[1, 2, 3, 4]));
        assert_eq!(1, oes(&[1, 2, 3, 4], &[1, 2, 4, 5]));
        assert_eq!(2, oes(&[1, 2, 3, 4], &[1, 2, 4, 3]));
        assert_eq!(3, oes(&[1, 2, 3, 4], &[2, 3, 1, 4]));
        assert_eq!(4, oes(&[1, 2, 3, 4], &[2, 1, 4, 3]));
    }

    #[test]
    fn test_user_input() {
        assert_eq!(Ok([1, 2, 3, 4]), user_input(io::Cursor::new(b"1234")));
        assert!(user_input(io::Cursor::new(b"123")).is_err());
        assert!(user_input(io::Cursor::new(b"12345")).is_err());
        assert_eq!(Ok([1, 2, 3, 4]), user_input(io::Cursor::new(b"1 2 3 4")));
        assert_eq!(Ok([1, 2, 3, 4]), user_input(io::Cursor::new(b"1 2-3 4aaa")));
        assert!(user_input(io::Cursor::new(b"1123")).is_err());
    }

    #[test]
    fn test_output() {
        assert_eq!(output(1, 3), "XOOO");
        assert_eq!(output(0, 0), "");
        assert_eq!(output(3, 1), "XXXO");
        assert_eq!(output(1, 1), "XO");
    }
}
