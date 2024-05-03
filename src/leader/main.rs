extern crate bridge;

use bridge::data::card::Rank;
use bridge::data::holding::Holding;

fn input_lead() -> Rank {
    let mut input = String::new();
    std::io::stdin().read_line(&mut input).unwrap();
    match input.trim().parse::<Rank>() {
        Ok(c) => c,
        Err(_) => {
            println!("Invalid card. Please try again.");
            input_lead()
        }
    }
}

// we don't want to it to be empty or to long, because we also want to test
// leads from short holdings, which are unlikely to be chosen completely at
// random.
fn random_holding() -> Holding {
    let holding = Holding::random();
    if holding.is_empty() || holding.length() > 5 {
        random_holding()
    } else {
        holding
    }
}

fn correct_lead(holding: &Holding) -> Rank {
    let best_seq = holding.best_sequence();
    match holding.length() {
        _ if best_seq.contains_high_card() => best_seq.iter().nth(0),
        1 => holding.iter().nth(0),
        2 if holding.contains_high_card() => holding.iter().nth(0),
        2 => holding.iter().last(),
        3 => holding.iter().nth(1), // middle
        _ if holding.contains_high_card() => holding.iter().nth(3),
        _ => holding.iter().nth(1), // second best
    }
    .unwrap()
}

fn main() {
    let mut score = 0;
    let mut max_score = 0;
    println!("Hello! Let's check your leading skills!");
    loop {
        let holding = random_holding();
        println!(
            "This is your suit holding: {}.\nWhich card do you lead?",
            holding
        );
        let lead = input_lead();
        let correct_lead = correct_lead(&holding);
        if correct_lead == lead {
            println!("Correct!");
            score += 1;
        } else {
            println!("Incorrect! You should have led a {}.", correct_lead)
        }
        max_score += 1;
        println!("Your score is: {}/{}", score, max_score);
    }
}
