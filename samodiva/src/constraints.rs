pub type Variable = String;
pub type VariableIndex = usize;
pub type Value = usize;
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
    OneOf(Vec<VariableIndex>, Value),
    // Of these variables, exactly a certain num must have the given Value
    Exactly(Vec<VariableIndex>, Value, usize)
}

pub struct ConstraintProblem {
     pub variables: Vec<Variable>,
     pub domains: Vec<Domain>,
     pub constraints: Vec<Constraint>,
     pub domain_names: Vec<String>
     //lastKnownSupport: HashMap<(Variable, Value, Variable), Value)>
}

impl std::fmt::Debug for ConstraintProblem {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::result::Result<(), std::fmt::Error> {
        let mut print_constraint = |c: &Constraint| {
            match c {
                Constraint::AllDifferent(vars) =>
                    f.write_fmt(format_args!("AllDifferent({:?})\n", get_var_names(&vars, &self))),
                Constraint::OneOf(vars, val) =>
                    f.write_fmt(format_args!("OneOf({:?}) = {}\n", get_var_names(&vars, &self), val)),
                Constraint::Exactly(vars, val, num) =>
                    f.write_fmt(format_args!("Exactly {} of ({:?}) = {}\n", num, get_var_names(&vars, &self), self.domain_names[*val]))
            }
        };
        for c in &self.constraints {
            print_constraint(&c);
        }
        return Ok(());
    }
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
                    println!("ONEOF | Domain <{}> takes on value <{}>", first_domain_index, val);
                    return true;
                }
            }
            return false;
        },
        Constraint::Exactly(vars, val, count) => {
            let domains_indices_containing_value: Vec<&VariableIndex> = vars.iter()
                .filter(|&&var_index| domains[var_index].contains(val))
                .collect();
            let available_domain_count = domains_indices_containing_value.len();
            if available_domain_count < *count {
                println!("EXACTLY | The problem is unsolvable!");
                return false;
            }
            // Constraint is satisfied?
            if domains_indices_containing_value.iter().filter(|&&var_index| domains[*var_index].len() == 1).count() == *count {
                // Do we need to remove value from other domains?
                let domains_to_clean: Vec<VariableIndex> = domains_indices_containing_value.iter()
                    .filter(|&&&var_index| domains[var_index].len() != 1)
                    .map(|var_index| **var_index)
                    .collect();
                // If no values were removed, return false
                if domains_to_clean.len() == 0 {
                    return false;
                }
                for domain_index in domains_to_clean {
                    let domain: &mut Domain = &mut domains[domain_index];
                    erase_value(domain, *val);
                }
                
                return true;
            }
            // If the value is only present in exactly num of the domains, then all variables must take on it
            if available_domain_count == *count {
                for domain_index in domains_indices_containing_value {
                    let domain: &mut Domain = &mut domains[*domain_index];
                    domain.clear();
                    domain.push(*val);
                }
                return true;
            }
            // TODO: If we have matched the amount of variables, remove this value from the domain of the rest of them
            let mut singleton_domains_indices_containing_value = vars.iter()
                .filter(|&&var_index| domains[var_index].len() == 1 && domains[var_index][0] == *val);
            if singleton_domains_indices_containing_value.count() == *count {

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
        Constraint::OneOf(vars, _) => &vars,
        Constraint::Exactly(vars, _, _) => &vars
    };
}

fn get_var_names<'a>(vars: &Vec<VariableIndex>, p: &'a ConstraintProblem) -> Vec<&'a Variable> {
    return vars.iter().map(|i: &VariableIndex| &p.variables[*i]).collect();
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
        // Pick a value from the smallest domain, recurse
        let mut smallest_domain: Option<&mut Domain> = self.domains.iter_mut()
            .filter(|domain| domain.len() > 1)
            .min_by(|lhs, rhs| lhs.len().cmp(&rhs.len()));
        if smallest_domain.is_some() {
            let mut domain: &mut Domain = &mut smallest_domain.unwrap();
            let chosen_value = domain[0];
            domain.clear();
            domain.push(chosen_value);

            println!("Next pick: {}", chosen_value);
            println!("Problem: {:?}", self);

            self.propagate_constraints();
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{iter, ops::Index};

    use itertools::Itertools;

    use crate::constraints::VariableIndex;

    use super::{Variable, Constraint};

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
        let domain_names: Vec<String> = Vec::new();
        let mut p = super::ConstraintProblem {
            variables, domains, constraints, domain_names
        };
        p.propagate_constraints();

        // Expected result
        // 1 4 2 3
        // 3 2 1 4
        // 4 1 3 2
        // 2 3 4 1
        let expected_domain_values: Vec<usize> = vec![1, 4, 2, 3,
                                                    3, 2, 1, 4,
                                                    4, 1, 3, 2,
                                                    2, 3, 4, 1];
        for (idx, val) in expected_domain_values.iter().enumerate() {
            assert_eq!(p.domains[idx].len(), 1usize);
            assert_eq!(p.domains[idx][0], *val);
        }
    }
    
    #[test]
    fn symmetric_schedule() {
        // Here's the goal 2 classes with 3 subjects, with same amount of time allocated to each class; Teachers are disregarded
        // This naturally will result in 2 very similar valid symmetrical solutions so we'll add 1 more constraints to force 1
        // Variables:
        // Create 1 variable per each time slot - t_<class>_<day>_<slot_index>
        //       Monday  | Tuesday
        // c1    t_1_m_1 | t_1_t_2
        // Domains: Each time slot could be either Math, Science or Literature
        // Constraints:
        // - Classes can't have the same subject at the same time
        // - Both classes need to have 2 hour of literature, 4 hours of math, 6 hours of science
        // - The science teacher wants to teach c1 on Monday
        // - The math teacher can't make it during certain hours

        let days = vec!["mon", "tue", "wed", "thu", "fri"];
        let classes = vec!["a", "b", "c"];
        let subjects: Vec<String> = vec!["Math".to_string(), "Literature".to_string(), "PE".to_string(), "Science".to_string(), "Free".to_string()];
        let lessons_per_subject = vec![3, 4, 2, 3, 0];
        assert!(subjects.len() == lessons_per_subject.len());

        let slot_indices = vec![1, 2, 3, 4, 5, 6];
        let slot_names: Vec<Variable> = itertools::iproduct!(days.iter(), slot_indices.iter(), classes.iter())
            .map(|(&d, &s, &c)| format!("{}_{}_{}", d, s, c))
            .collect();
        let slot_domains = slot_names.iter().map(|_| vec![0, 1, 2, 3, 4]).collect();

        println!("Slot names: {:?}", slot_names);

        let mut constraints = Vec::new();

        // Can't have the same subject at the same time
        let mut same_time_slots: Vec<Constraint> = (0..days.len()*slot_indices.len())
            .map(|pair_index| super::Constraint::AllDifferent((pair_index*classes.len()..(pair_index + 1) * classes.len()).collect_vec()))
            .collect();

        // Each class needs to receive a certain number of sessions of each subject
        let slots_per_class:Vec<Vec<VariableIndex>> = classes.iter()
            .enumerate()
            .map(|(class_index, _)| (class_index..slot_names.len()).step_by(classes.len()).collect_vec())
            .collect_vec();
        let mut constraints_per_subject: Vec<Constraint> = itertools::iproduct!(slots_per_class.iter(), 0..subjects.len()-1)
            .map(|(slots, subject_index)| super::Constraint::Exactly(slots.to_vec(), subject_index, lessons_per_subject[subject_index]))
            .collect();

        constraints.append(&mut same_time_slots);
        constraints.append(&mut constraints_per_subject);

        let mut p = super::ConstraintProblem {
            variables: slot_names, domains: slot_domains, constraints, domain_names: subjects.to_vec()
        };
        println!("Problem: {:?}", p);
        p.propagate_constraints();
        

        // Expected result
        // Class | Monday  | Tuesday | Wed | Thu | Fri
        //       | Math    |
        // A     | Science |
        println!("heey!");
        println!("Day    | A    | B    | C     |");
        for (idx, _val) in p.domains.iter().enumerate().step_by(classes.len()) {
            let day = days[idx / (classes.len() * slot_indices.len())];
            let subjects_per_class = p.domains[idx..idx+classes.len()].iter()
                .map(|domain| &subjects[domain[0]])
                .join(" |");
            println!("{}      | {}", day, subjects_per_class);
            //assert_eq!(p.domains[idx].len(), 1usize);
            //assert_eq!(p.domains[idx][0], *val);
        }
        assert!(false);
    }
}
