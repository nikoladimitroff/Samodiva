
pub type Variable = String;
pub type VariableIndex = usize;
pub type Value = u32;
pub type Domain = Vec<Value>;
// This holds why do we need to revise a constraint - a value that was dropped from the domain of the given variable
pub type ConstraintRevisionReason = (VariableIndex, Value);

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

fn revise(domains: &mut Vec<Domain>, c: &Constraint, reason: Option<ConstraintRevisionReason>) -> bool {
    match c {
        Constraint::AllDifferent(_vars) => {
            // We may only reduce the domain of x if some of other domains takes on a specific value
            let changed_var_index: VariableIndex = reason.unwrap().0;
            if domains[changed_var_index].len() == 1 {
                let taken_value: Value = domains[changed_var_index][0];
                domains.iter_mut().enumerate()
                    .filter(|(idx, _)| *idx != changed_var_index)
                    .for_each(|(_, d)| erase_value(d, taken_value));

                return true;
            }
            return false;
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
                println!("Yes, only this value remains: {}", val);
                let domain = &mut domains[*first_var_index.unwrap()];
                domain.clear();
                domain.push(*val);
                return true;
            }
            return false;
        }
    }
}

impl ConstraintProblem {    
    /// This runs AC2001 on the given constraint-programming problem
    pub fn ac2001(&mut self) -> bool {
        return revise(&mut self.domains, &self.constraints[0], None);
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

        super::revise(&mut domains, &one_of_constraint, None);
        // After the revise, the domain of the 2nd variable should be reduced to the value 3
        assert_eq!(domains[1].len(), 1, "The domain of the variable must be reduced to a single value");
        assert_eq!(domains[1][0], 3, "The domain of the variable must be reduced to the value 3");
    }

    #[test]
    fn all_different_constraint() {    
        let mut domains = Vec::new();
        domains.push(vec![1]);
        domains.push(vec![1, 2, 3, 4]);
        domains.push(vec![1, 2, 3, 4]);
        domains.push(vec![1, 2, 3, 4]);
        let expected_value_to_be_dropped: super::Value = 1;
        let expected_domain_size = domains[1].len() - 1;
        
        let all_different_constraint = super::Constraint::AllDifferent(vec![0, 1, 2, 3]);
        let reason = (0, 1);

        super::revise(&mut domains, &all_different_constraint, Some(reason));
        // After the revise, the domain the value 1 should be dropped from the domains of all other variables
        for d in &domains[1..] {
            assert_eq!(d.len(), expected_domain_size, "The domain of the variable must be have been reduced");
            assert!(!d.contains(&expected_value_to_be_dropped), "The domain of the variable must not include the value {}", expected_value_to_be_dropped);   
        }
    }
}