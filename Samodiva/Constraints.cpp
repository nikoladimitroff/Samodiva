#include "Constraints.h"
#include <algorithm>
#include <format>
#include <functional>
#include <iostream>
#include <ranges>
#include <sstream>
#include <doctest/doctest.h>


ConstraintProblem::ConstraintProblem(
	std::vector<VariableName>&& variableNames,
	std::vector<Domain>&& domains,
	std::vector<Constraint>&& constraints,
	std::vector<std::string>&& domainValueNames)
	: m_variableNames(variableNames)
	, m_domains(domains)
	, m_constraints(constraints)
	, m_domainValueNames(domainValueNames)
{
}

// TODO: Optimization idea #1 - use this struct to keep track of what changed
// This holds why do we need to revise a constraint - a value that was dropped from the domain of the given variable
struct ConstraintRevisionReason
{
	VariableIndex Index;
	VariableValue Value;
};
// TODO: Optimization idea #2 - trigger events when a domain is reduced - "var X takes on a value", "value deleted in all domains", etc.
// and use these events to only trigger specific constraint revisions

namespace Printer
{
	void PrintVariableNames(std::stringstream& stream, const std::vector<VariableName>& variables, const std::vector<VariableIndex>& indices)
	{
		// print all but the last one with commas
		for (int i = 0; i < indices.size() - 1; i++)
		{
			const VariableIndex index = indices[i];
			stream << variables[index] << ",";
		}
		// print the last one
		stream << variables[indices.size() - 1];
	}

	void Print(const ConstraintTypes::AllDifferent& c, std::stringstream& stream, const std::vector<VariableName>& variables, const std::vector<VariableIndex>& indices)
	{
		// AllDifferent(X, Y, Z)
		stream << "AllDifferent(";
			PrintVariableNames(stream, variables, indices);
		stream << ")" << std::endl;
	}

	void Print(const ConstraintTypes::OneOf& c, std::stringstream& stream, const std::vector<VariableName>& variables, const std::vector<VariableIndex>& indices)
	{
		// OneOf(X, Y, Z) = 42
		stream << "OneOf(";
			PrintVariableNames(stream, variables, indices);
		stream << ") = " << c.ValueToHave << std::endl;
	}

	void Print(const ConstraintTypes::Exactly& c, std::stringstream& stream, const std::vector<VariableName>& variables, const std::vector<VariableIndex>& indices)
	{
		// Exactly 5 of (X, Y, Z) = 42
		stream << "Exactly " << c.NumberToHaveIt << " of (";
			PrintVariableNames(stream, variables, indices);
		stream << ") = " << c.ValueToHave << std::endl;
	}

	void Print(const ConstraintTypes::AtMost& c, std::stringstream& stream, const std::vector<VariableName>& variables, const std::vector<VariableIndex>& indices)
	{
		// AtMost 5 of (X, Y, Z) = 42
		stream << "AtMost " << c.NumberToHaveIt << " of (";
			PrintVariableNames(stream, variables, indices);
		stream << ") = " << c.ValueToHave << std::endl;
	}
}

std::string ConstraintProblem::Print() const
{
	std::stringstream stream;
	for (const Constraint& c : m_constraints)
	{
		auto printAnyConstraint = [this, &stream, &c](auto&& constraintType)
			{
				Printer::Print(constraintType, stream, m_variableNames, c.Variables);
			};
		std::visit(printAnyConstraint, c.Type);
	}
	return stream.str();
}

enum class ERevisionResult : uint8_t
{
	NoChanges,
	ReducedDomain,
	Unsolveable
};
namespace
{
	// Returns if this domain has been reduced to a single value
	bool IsSingleton(const Domain& domain)
	{
		return domain.size() == 1;
	}
	void MakeSingleton(Domain& domain, VariableValue value)
	{
		domain.clear();
		domain.push_back(value);
	}

	ERevisionResult Revise(const ConstraintTypes::AllDifferent& c, std::vector<Domain>& domains, const std::vector<VariableIndex>& indices)
	{
		// We may only reduce the domain of x if some of other domains takes on a specific value
		// For any such domain, we need to delete it's specific value from all other domains
		auto mapVarToDomain = [&domains](const VariableIndex& index) { return domains[index]; };

		auto singletons = std::views::transform(indices, mapVarToDomain) | std::views::filter(IsSingleton);
		size_t erasedValuesCount = 0;
		for (const Domain& singleton : singletons)
		{
			for (const VariableIndex& index : indices)
			{
				if (!IsSingleton(domains[index]))
				{
					erasedValuesCount += std::erase(domains[index], singleton[0]);
				}
			}
		}
		if (erasedValuesCount > 0)
		{
			return ERevisionResult::ReducedDomain;
		}
		return ERevisionResult::NoChanges;

	}
	ERevisionResult Revise(const ConstraintTypes::OneOf& c, std::vector<Domain>& domains, const std::vector<VariableIndex>& indices)
	{
		// If Value is present in only of the other variable domains, then this variable must take it as value
		auto mapVarToDomain = [&domains](const VariableIndex& index) { return std::ref(domains[index]); };
		auto domainsOnConstraint = std::views::transform(indices, mapVarToDomain);
		auto containsValue = [&c](const std::reference_wrapper<Domain>& domain) { return std::ranges::contains(domain.get(), c.ValueToHave); };
		std::vector<std::reference_wrapper<Domain>> domainsContainingValue;
		std::copy_if(domainsOnConstraint.begin(), domainsOnConstraint.end(), std::back_inserter(domainsContainingValue), containsValue);
		size_t numDomains = domainsContainingValue.size();
		if (numDomains == 0)
		{
			return ERevisionResult::Unsolveable;
		}
		if (numDomains == 1 && !IsSingleton(domainsContainingValue[0].get()))
		{
			// Alright, only 1 domain still has that value, turn it into a singleton if it's not already
			MakeSingleton(domainsContainingValue[0].get(), c.ValueToHave);
			return ERevisionResult::ReducedDomain;
		}
		return ERevisionResult::NoChanges;

	}
	ERevisionResult Revise(const ConstraintTypes::Exactly& c, std::vector<Domain>& domains, const std::vector<VariableIndex>& indices)
	{
		auto mapVarToDomain = [&domains](const VariableIndex& index) { return std::ref(domains[index]); };
		auto domainsOnConstraint = std::views::transform(indices, mapVarToDomain);
		auto containsValue = [&c](const std::reference_wrapper<Domain>& domain) { return std::ranges::contains(domain.get(), c.ValueToHave); };
		std::vector<std::reference_wrapper<Domain>> domainsContainingValue;
		std::copy_if(domainsOnConstraint.begin(), domainsOnConstraint.end(), std::back_inserter(domainsContainingValue), containsValue);
		size_t numDomains = domainsContainingValue.size();
		if (numDomains < c.NumberToHaveIt)
		{
			return ERevisionResult::Unsolveable;
		}
		// Are all domains containing the value already singletons? Nothing to do then;
		// this is mostly to prevent unnecessary making singletons all the time in the next step
		const bool isSatisfiedAlready = std::ranges::count_if(domainsContainingValue, IsSingleton) == domainsContainingValue.size();
		if (isSatisfiedAlready)
		{
			return ERevisionResult::NoChanges;
		}
		// If the value is only present in exactly NumberToHaveIt of the domains, then all variables must take it so convert to singletons
		if (domainsContainingValue.size() == c.NumberToHaveIt)
		{
			for (std::reference_wrapper<Domain>& domain : domainsContainingValue)
			{
				MakeSingleton(domain.get(), c.ValueToHave);
			}
			return ERevisionResult::ReducedDomain;
		}
		return ERevisionResult::NoChanges;
	}
	ERevisionResult Revise(const ConstraintTypes::AtMost& c, std::vector<Domain>& domains, const std::vector<VariableIndex>& indices)
	{
		auto mapVarToDomain = [&domains](const VariableIndex& index) { return std::ref(domains[index]); };
		auto domainsOnConstraint = std::views::transform(indices, mapVarToDomain);
		auto containsValue = [&c](const std::reference_wrapper<Domain>& domain) { return std::ranges::contains(domain.get(), c.ValueToHave); };
		std::vector<std::reference_wrapper<Domain>> domainsContainingValue;
		std::copy_if(domainsOnConstraint.begin(), domainsOnConstraint.end(), std::back_inserter(domainsContainingValue), containsValue);
		const bool isSatisfiedAlready = domainsContainingValue.size() <= c.NumberToHaveIt;
		if (isSatisfiedAlready)
		{
			// Nothing to do
			return ERevisionResult::NoChanges;
		}
		const size_t numberOfSingletons = std::ranges::count_if(domainsContainingValue, IsSingleton);
		// If NumberToHaveIt of the domains are already singletons, then value should be removed from all other domains
		if (numberOfSingletons == c.NumberToHaveIt)
		{
			for (std::reference_wrapper<Domain>& domain : domainsContainingValue)
			{
				if (!IsSingleton(domain.get()))
				{
					std::erase(domain.get(), c.ValueToHave);
				}
			}
			return ERevisionResult::ReducedDomain;
		}
		return ERevisionResult::NoChanges;
	}
}

ERevisionResult ReviseConstraint(const Constraint& c, std::vector<Domain>& domains)
{
	auto visitor = [&domains, &c](auto&& type) { return Revise(type, domains, c.Variables); };
	return std::visit(visitor, c.Type);
}

bool AreConstraintsRelated(const Constraint& lhs, const Constraint& rhs)
{
	return std::ranges::any_of(lhs.Variables, [&rhs](const VariableIndex& i)
		{
			return std::ranges::contains(rhs.Variables, i);
		});
}

void ConstraintProblem::PropagateConstraints()
{
	/// This runs AC3 on the given constraint-programming problem
	std::vector<std::reference_wrapper<const Constraint>> revisionList;
	std::copy(m_constraints.begin(), m_constraints.end(), std::back_inserter(revisionList));

	while (!revisionList.empty())
	{
		// TODO
		const bool emptyDomainExists = std::any_of(m_domains.begin(), m_domains.end(), [](const Domain& domain)
			{
				return domain.empty();
			});
		if (emptyDomainExists)
		{
			// Problem is unsolvable, constraints removed all values from a variable's domain
			break;
		}
		const Constraint& constraintUnderRevision = revisionList.back();
		switch (ReviseConstraint(constraintUnderRevision, m_domains))
		{
		case ERevisionResult::NoChanges:
			// Nothing to do, next constraint
			break;
		case ERevisionResult::ReducedDomain:
		{
			// We successfully revised a domain, find all constraints that share a variable with the current one
			// and re-revise them
			// TODO: Don't append existing ones
			for (const Constraint& currentConstraint : m_constraints)
			{
				const bool areRelated = AreConstraintsRelated(currentConstraint, constraintUnderRevision);
				// For some reason can't get this to compile with ranges::contains
				const bool hasBeenAdded = std::find(revisionList.begin(), revisionList.end(), currentConstraint) != revisionList.end();
				if (areRelated && !hasBeenAdded)
				{
					revisionList.push_back(std::cref(currentConstraint));
				}
			}
			break;
		}
		case ERevisionResult::Unsolveable:
		{
			// TODO: Replace cout with a logger
			std::cout << "Unsolveable problem!" << std::endl;
			return;
		}
		}
		revisionList.pop_back();
	}

	auto nonSingletons = m_domains | std::views::filter(std::not_fn(IsSingleton));
	if (std::ranges::empty(nonSingletons))
	{
		// All variables are singleton, nothing to do
		return;
	}

	// Some values are not singletons, pick a value from the smallest domain at random and recurse
	auto domainComparator = [](const Domain& lhs, const Domain& rhs) { return lhs.size() - rhs.size(); };
	auto smallestNonSingleton = std::ranges::min_element(nonSingletons, domainComparator);

	//assert(smallestNonSingleton != std::ranges::end(nonSingletons));
	// todo: do proper random
	const VariableValue randomValue = smallestNonSingleton->at(rand() % smallestNonSingleton->size());
	MakeSingleton(*smallestNonSingleton, randomValue);
	std::cout << "Picking " << randomValue << " at random." << std::endl;
	std::cout << Print();
}

#pragma region Tests
TEST_CASE("OneOf")
{
	std::vector<Domain> domains;
	domains.push_back(Domain{ 1, 2 });
	domains.push_back(Domain{ 1, 2, 3 });
	domains.push_back(Domain{ 1, 2 });

	Constraint oneOf
	{
		std::vector<VariableIndex>{0, 1, 2},
		ConstraintTypes::OneOf(3)
	};

	ReviseConstraint(oneOf, domains);
	// After the revise, the domain of the 2nd variable should be reduced to the value 3
	CHECK_MESSAGE(domains[1].size() == 1, "The domain of the variable must be reduced to a single value");
	CHECK_MESSAGE(domains[1][0] == 3, "The domain of the variable must be reduced to the value 3");
	CHECK_MESSAGE(domains[0] == domains[2], "The domains of the other variables must not be changed");
	CHECK_MESSAGE(domains[0][0] == 1, "The domains of the other variables must not be changed");
	CHECK_MESSAGE(domains[0][1] == 2, "The domains of the other variables must not be changed");
}

TEST_CASE("AllDifferent")
{
	std::vector<Domain> domains;
	domains.push_back(Domain{ 1 });
	domains.push_back(Domain{1, 2, 3, 4});
	domains.push_back(Domain{1, 2, 3, 4});
	domains.push_back(Domain{1, 2, 3, 4});
	domains.push_back(Domain{1, 2, 3, 4});

	VariableValue valueToBeDropped = 1;
	size_t reducedDomainSize = domains[1].size() - 1;
	size_t initial_domain_size = domains[1].size();

	Constraint allDifferent
	{
		std::vector<VariableIndex>{0, 1, 2, 3}, // Not all domains are constrained
		ConstraintTypes::AllDifferent()
	};

	ReviseConstraint(allDifferent, domains);
	// After the revise, the value 1 should be dropped from the domains of variables 1,2,3
	for (int i = 1; i < domains.size() - 1; i++)
	{
		const Domain& domain = domains[i];
		CHECK_MESSAGE(domain.size() == reducedDomainSize, "The domain of the variable must have been reduced");
		CHECK_MESSAGE(!std::ranges::contains(domain, valueToBeDropped), "The domain of the variable must not include the value ", valueToBeDropped);
	}

	const Domain& firstDomain = domains[0];
	CHECK_MESSAGE(firstDomain.size() == 1, "Singleton shouldn't be changed");
	CHECK_MESSAGE(firstDomain[0] == 1, "Singleton shouldn't be changed");

	const Domain& lastDomain = domains[domains.size() - 1];
	CHECK_MESSAGE(lastDomain.size() == initial_domain_size, "The domain of unconstrained variables must not be changed");
	CHECK_MESSAGE(std::ranges::contains(lastDomain, valueToBeDropped), "The domain of unconstrained variables must not be changed");
}

TEST_CASE("Mini Sudoku 4x4 instead of 9x9")
{
	// Generated from https://www.minisudoku.com/
	std::vector<Domain> domains;

	// Generate all initial domains
	for (int i = 0; i < 16; i++)
	{
		domains.push_back(Domain{ 1, 2, 3, 4 });
	}

	std::vector<Constraint> constraints;

	auto generateRowIndices = [](int i)
		{
			return std::vector<VariableIndex>
			{
				VariableIndex(i * 4 + 0), VariableIndex(i * 4 + 1), VariableIndex(i * 4 + 2), VariableIndex(i * 4 + 3)
			};
		};

	auto generateColIndices = [](int i)
		{
			return std::vector<VariableIndex>
			{
				VariableIndex(i + 4 * 0), VariableIndex(i + 4 * 1), VariableIndex(i + 4 * 2), VariableIndex(i + 4 * 3)
			};
		};

	auto generateSqrIndices = [](int i)
		{
			return std::vector<VariableIndex>
			{
				VariableIndex(i * 4 - 2 * (i % 2)), VariableIndex(i * 4 - 2 * (i % 2) + 1),
					VariableIndex(i * 4 - 2 * (i % 2) + 4), VariableIndex(i * 4 - 2 * (i % 2) + 4 + 1)
			};
		};

	// Create constraints
	for (int i = 0; i < 4; i++)
	{
		Constraint allDifferentRow{ generateRowIndices(i), ConstraintTypes::AllDifferent() };
		Constraint allDifferentCol{ generateColIndices(i), ConstraintTypes::AllDifferent() };
		Constraint allDifferentSqr{ generateSqrIndices(i), ConstraintTypes::AllDifferent() };
		constraints.push_back(allDifferentRow);
		constraints.push_back(allDifferentCol);
		constraints.push_back(allDifferentSqr);

		for (int value = 1; value < 5; value++)
		{
			Constraint oneOfRow{ generateRowIndices(i), ConstraintTypes::OneOf(value) };
			Constraint oneOfCol{ generateColIndices(i), ConstraintTypes::OneOf(value) };
			Constraint oneOfSqr{ generateSqrIndices(i), ConstraintTypes::OneOf(value) };

			constraints.push_back(oneOfRow);
			constraints.push_back(oneOfCol);
			constraints.push_back(oneOfSqr);
		}
	}

	// Initial setup
	// - 4 2 3
	// - - - 4
	// - - - 2
	// 2 3 - -
	std::vector<VariableValue> initialSetup
	{
		0, 4, 2, 3,
		0, 0, 0, 4,
		0, 0, 0, 2,
		2, 3, 0, 0
	};
	for (int i = 0; i < initialSetup.size(); i++)
	{
		if (initialSetup[i] > 0)
		{
			MakeSingleton(domains[i], initialSetup[i]);
		}
	}

	std::vector<VariableName> variables;
	std::vector<VariableName> domainNames;
	ConstraintProblem p
	(
		std::move(variables),
		std::move(domains),
		std::move(constraints),
		std::move(domainNames)
	);
	p.PropagateConstraints();

	// Expected result
	// 1 4 2 3
	// 3 2 1 4
	// 4 1 3 2
	// 2 3 4 1
	std::vector<VariableValue> expectedDomainValues
	{
		1, 4, 2, 3,
		3, 2, 1, 4,
		4, 1, 3, 2,
		2, 3, 4, 1
	};
	const std::vector<Domain>& finalDomains = p.GetDomains();
	for (int i = 0; i < expectedDomainValues.size(); i++)
	{
		CHECK(finalDomains[i].size() == 1);
		CHECK(finalDomains[i][0] == expectedDomainValues[i]);
	}
}

#pragma endregion 