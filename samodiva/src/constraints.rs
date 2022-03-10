
pub type Variable = String;
pub type VariableIndex = usize;
pub type Value = u32;
pub type Domain = Vec<Value>;
// TODO: Optimization idea #1 - use this struct to keep track of what changed
// This holds why do we need to revise a constraint - a value that was dropped from the domain of the given variable
type ConstraintRevisionReason = (VariableIndex, Value);
// TODO: Optimization idea #2 - trigger events when a domain is reduced - "var X takes on a value", "value deleted in all domains", etc.
// and use these events to only trigger specific constraint revisions

#[derive(PartialEq)]
pub enum Constraint {
    // All of the variables must have different values
    AllDifferent(Vec<VariableIndex>),
    // Of these variables, at least one must have the given value
    OneOf(Vec<VariableIndex>, Value)
}

pub struct ConstraintProblem {
     pub variables: Vec<Variable>,
     pub domains: Vec<Domain>,
     pub constraints: Vec<Constraint>
     //lastKnownSupport: HashMap<(Variable, Value, Variable), Value)>
}

fn erase_value<T: PartialEq>(vec: &mut Vec<T>, value: T) {
    vec.remove(vec.iter().position(|x| *x == value).expect("Value not found"));
}

fn revise(domains: &mut Vec<Domain>, c: &Constraint) -> bool {
    match c {
        Constraint::AllDifferent(vars) => {
            // We may only reduce the domain of x if some of other domains takes on a specific value
            let mut domains_on_this_constraint: Vec<&mut Domain> = domains.iter_mut().enumerate().filter(|(idx, _)| vars.contains(idx)).map(|(_, domain)| domain).collect();
            let single_values: Vec<Value> = domains_on_this_constraint.iter().filter(|d| d.len() == 1).map(|d| d[0]).collect();

            for &taken_value in &single_values {
                domains_on_this_constraint.iter_mut()
                    .filter(|d| d.len() > 1 && d.contains(&taken_value))
                    .for_each(|d| erase_value(d, taken_value));
            }
            let single_values_post_count = domains_on_this_constraint.iter().filter(|d| d.len() == 1).count();
            return single_values.len() !=  single_values_post_count;
        },
        Constraint::OneOf(vars, val) => {
            // If Value is present in only of the other variable domains, then this variable must take it as value
            let mut domains_indices_containing_value = vars.iter().filter(|&&var_index| domains[var_index].contains(val));
            let first_var_index: Option<&VariableIndex> = domains_indices_containing_value.next();
            if first_var_index.is_none() {
                println!("The problem is unsolvable!");
                return false;
            }
            if domains_indices_containing_value.next().is_none() {
                // Alright, first_var_index is the only variable still holding that value in its domain, it must takes on the value
                let first_domain_index = *first_var_index.unwrap();
                let first_domain = &mut domains[first_domain_index];
                if first_domain.len() != 1 {
                    first_domain.clear();
                    first_domain.push(*val);
                    println!("ONEOF| Domain <{}> takes on value <{}>", first_domain_index, val);
                    return true;
                }
            }
            return false;
        }
    }
}

fn do_vecs_intersect(vec1: &Vec<VariableIndex>, vec2: &Vec<VariableIndex>) -> bool {
    return vec1.iter().any(|&var| vec2.contains(&var));
}

fn get_vars_of_constraint(c: &Constraint) -> &Vec<VariableIndex> {
    return match c {
        Constraint::AllDifferent(vars) => &vars,
        Constraint::OneOf(vars, _) => &vars
    };
}

fn find_related_constraints<'a>(constraint_to_check: &'a Constraint, all_constraints: &'a Vec<Constraint>) -> impl Iterator<Item = &'a Constraint> {
    let vars_to_check = get_vars_of_constraint(constraint_to_check);
    return all_constraints.iter().filter(|&c| do_vecs_intersect(get_vars_of_constraint(c), vars_to_check));
}

impl ConstraintProblem {    
    /// This runs AC3 on the given constraint-programming problem
    pub fn propagate_constraints(&mut self) {
        let mut constraints_to_revise: Vec<&Constraint> = self.constraints.iter().collect();

        while self.domains.iter().all(|d| d.len() != 0) && constraints_to_revise.len() != 0  {
            let c: &Constraint = constraints_to_revise.pop().unwrap();
            if revise(&mut self.domains, &c) {
                let all_related_contstraints = find_related_constraints(&c, &self.constraints);
                // TODO: Only move unique constraints
                constraints_to_revise.extend(all_related_contstraints);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn one_of_constraint() {    
        let mut domains = Vec::new();
        domains.push(vec![1, 2]);
        domains.push(vec![1, 2, 3]);
        domains.push(vec![1, 2]);
        
        let one_of_constraint = super::Constraint::OneOf(vec![0, 1, 2], 3);

        super::revise(&mut domains, &one_of_constraint);
        // After the revise, the domain of the 2nd variable should be reduced to the value 3
        assert_eq!(domains[1].len(), 1, "The domain of the variable must be reduced to a single value");
        assert_eq!(domains[1][0], 3, "The domain of the variable must be reduced to the value 3");
        assert_eq!(domains[0], domains[2], "The domains of the other variables must not be changed");
        assert_eq!(domains[0][0], 1, "The domains of the other variables must not be changed");
        assert_eq!(domains[0][1], 2, "The domains of the other variables must not be changed");
    }

    #[test]
    fn all_different_constraint() {    
        let mut domains = Vec::new();
        domains.push(vec![1]);
        domains.push(vec![1, 2, 3, 4]);
        domains.push(vec![1, 2, 3, 4]);
        domains.push(vec![1, 2, 3, 4]);
        domains.push(vec![1, 2, 3, 4]);
        let expected_value_to_be_dropped: super::Value = 1;
        let reduced_domain_size = domains[1].len() - 1;
        let initial_domain_size = domains[1].len();
        
        let all_different_constraint = super::Constraint::AllDifferent(vec![0, 1, 2, 3]); // Not all domains are constrained

        super::revise(&mut domains, &all_different_constraint);
        // After the revise, the domain the value 1 should be dropped from the domains of all other variables
        for d in &domains[1..domains.len()-1] {
            assert_eq!(d.len(), reduced_domain_size, "The domain of the variable must have been reduced");
            assert!(!d.contains(&expected_value_to_be_dropped), "The domain of the variable must not include the value {}", expected_value_to_be_dropped);   
        }
        let last_domain = domains.last().unwrap();
        assert_eq!(last_domain.len(), initial_domain_size, "The domain of unconstrained variables must not be changed");
        assert!(last_domain.contains(&expected_value_to_be_dropped), "The domain of unconstrained variables must not be changed");
    }

    #[test]
    fn easy_mini_sudoku() {
        // Generated from https://www.minisudoku.com/
        let mut domains = Vec::new();

        // Generate all initial domains
        for _ in 0..16 {
            domains.push(vec![1, 2, 3, 4]);
        }

        let mut constraints = Vec::new();

        let generate_row_indices = |i: usize| vec![i * 4 + 0, i * 4 + 1, i * 4 + 2, i * 4 + 3];
        let generate_col_indices = |i: usize| vec![i + 4 * 0, i + 4 * 1, i + 4 * 2, i + 4 * 3];
        let generate_square_indices = |i: usize| vec![i * 4 - 2*(i%2), i * 4 - 2*(i%2) + 1, i * 4 - 2*(i%2) + 4, i * 4 - 2*(i%2) + 4 + 1];
        // Create constraints
        for i in 0..4 {
            let all_different_row = super::Constraint::AllDifferent(generate_row_indices(i));
            let all_different_col = super::Constraint::AllDifferent(generate_col_indices(i));
            let all_different_sqr = super::Constraint::AllDifferent(generate_square_indices(i));
            constraints.push(all_different_row);
            constraints.push(all_different_col);
            constraints.push(all_different_sqr);

            for value in 1..5 {
                let one_of_row = super::Constraint::OneOf(generate_row_indices(i), value);
                let one_of_col = super::Constraint::OneOf(generate_col_indices(i), value);
                let one_of_sqr = super::Constraint::OneOf(generate_square_indices(i), value);
                
                constraints.push(one_of_row);
                constraints.push(one_of_col);
                constraints.push(one_of_sqr);
            }
        }
        
        // Initial setup
        // - 4 2 3
        // - - - 4
        // - - - 2
        // 2 3 - -
        let initial_setup: Vec<super::Value> = vec![0, 4, 2, 3,
                                                    0, 0, 0, 4,
                                                    0, 0, 0, 2,
                                                    2, 3, 0, 0];
        for i in 0..16 {
            if initial_setup[i] > 0 {
                domains[i].clear();
                domains[i].push(initial_setup[i]);
            }
        }

        let variables = Vec::new();
        let mut p = super::ConstraintProblem {
            variables, domains, constraints
        };
        p.propagate_constraints();

        // Expected result
        // 1 4 2 3
        // 3 2 1 4
        // 4 1 3 2
        // 2 3 4 1
        let expected_domain_values: Vec<u32> = vec![1, 4, 2, 3,
                                                    3, 2, 1, 4,
                                                    4, 1, 3, 2,
                                                    2, 3, 4, 1];
        for (idx, val) in expected_domain_values.iter().enumerate() {
            assert_eq!(p.domains[idx].len(), 1usize);
            assert_eq!(p.domains[idx][0], *val);
        }
    }
}
