use samodiva;

fn main() {
    samodiva::ulala();
    let mut variables = Vec::new();
    variables.push(String::from("Top"));
    variables.push(String::from("Mid"));
    variables.push(String::from("Bot"));

    let mut constraints = Vec::new();
    let c = samodiva::constraints::Constraint::OneOf(vec![0, 1, 2], 9);
    constraints.push(c);

    let mut domains = Vec::new();
    domains.push(vec![7, 8]);
    domains.push(vec![7, 8, 9]);
    domains.push(vec![7, 8]);

    
    let mut p = samodiva::constraints::ConstraintProblem {
        variables, domains, constraints
    };
    p.ac2001();
}
