
pub type Variable = String;
pub type VariableIndex = usize;
pub type Value = u32;
pub type Domain = Vec<Value>;

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


fn revise(domains: &mut Vec<Domain>, c: &Constraint) -> bool {
    match c {
        Constraint::AllDifferent(_vars) => {
            // We may only reduce the domain of x if some of other domains takes on a specific value
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
            }
            return false;
        }
    }
}

impl ConstraintProblem {    
    /// This runs AC2001 on the given constraint-programming problem
    pub fn ac2001(&mut self) -> bool {
        return revise(&mut self.domains, &self.constraints[0]);
    }
}
