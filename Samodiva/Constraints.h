#pragma once
#include <string>
#include <variant>
#include <vector>

using VariableName = std::string;
// this is an index in the list of variables
using VariableIndex = uint16_t;
using VariableCount = uint16_t;
// this is an index in the list of domain values
using VariableValue = uint16_t;
using Domain = std::vector<VariableValue>;

namespace ConstraintTypes
{
	// All of the variables must have different values
	struct AllDifferent
	{
		auto operator<=>(const AllDifferent&) const = default;
	};
	// Of these variables, at least one must have the given value
	struct OneOf
	{
		VariableValue ValueToHave;
		OneOf(VariableValue value)
			: ValueToHave(value)
		{}
		auto operator<=>(const OneOf&) const = default;
	};
	// Of these variables, exactly a certain num must have the given Value
	struct Exactly
	{
		VariableValue ValueToHave;
		VariableCount NumberToHaveIt;
		Exactly(VariableValue value, VariableCount numberToHaveIt)
			: ValueToHave(value)
			, NumberToHaveIt(NumberToHaveIt)
		{}
		auto operator<=>(const Exactly&) const = default;
	};
	// Of these variables, at most a certain num must have the given Value
	struct AtMost
	{
		VariableValue ValueToHave;
		VariableCount NumberToHaveIt;
		AtMost(VariableValue value, VariableCount numberToHaveIt)
			: ValueToHave(value)
			, NumberToHaveIt(NumberToHaveIt)
		{}
		auto operator<=>(const AtMost&) const = default;
	};

}
using AnyConstraintType = std::variant<ConstraintTypes::AllDifferent, ConstraintTypes::OneOf, ConstraintTypes::Exactly, ConstraintTypes::AtMost>;

struct Constraint
{
	std::vector<VariableIndex> Variables;
	AnyConstraintType Type;
	auto operator<=>(const Constraint&) const = default;
};

template<class... Ts>
struct overload : Ts... { using Ts::operator()...; };

class ConstraintProblem
{
public:
	ConstraintProblem(
		std::vector<VariableName>&& variableNames,
		std::vector<Domain>&& domains,
		std::vector<Constraint>&& constraints,
		std::vector<std::string>&& domainValueNames);

	std::string Print() const;
	void PropagateConstraints();
	const std::vector<Domain>& GetDomains() const { return m_domains; }

private:
	// List of all variables in the problem
	std::vector<VariableName> m_variableNames;
	// List of domains for every variable; to make things fast
	// only numbers are used for variable values in exchange for their actual domain values
	std::vector<Domain> m_domains;
	std::vector<Constraint> m_constraints;
	// The names of actual values in the domains of all variables
	std::vector<std::string> m_domainValueNames;

	//lastKnownSupport: HashMap<(VariableName, Value, VariableName), Value)>
};
