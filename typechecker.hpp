#include "combinator.h"

template <typename T> struct SharedWrapper {
    std::shared_ptr<T> shared;

    SharedWrapper(std::shared_ptr<T> thing) : shared(std::move(thing)) {}

    bool operator==(SharedWrapper<T> const &other) const {
        return *shared.get() == *other.get();
    }

    T *get() { return shared.get(); }
    T const *get() const { return shared.get(); }
};

struct TermType {
    enum class BaseType {
        INTEGER,
        CHARACTER,
        BOOLEAN,
        INVALID,
    };

    enum {
        BASE,
        LAMBDA,
    } tag;

    using lambda_t =
        std::pair<SharedWrapper<TermType>, SharedWrapper<TermType>>;

    std::variant<BaseType, lambda_t> term_type;

    TermType() : tag(BASE), term_type(BaseType::INVALID) {}
    TermType(BaseType base_type) : tag(BASE) { term_type = base_type; }
    TermType(std::shared_ptr<TermType> a, std::shared_ptr<TermType> b)
        : tag(LAMBDA) {
        term_type = std::make_pair(std::move(a), std::move(b));
    }

    TermType(std::pair<SharedWrapper<TermType>, SharedWrapper<TermType>> const
                 &pair_tt)
        : tag(LAMBDA) {
        term_type = pair_tt;
    }

    bool operator==(TermType const &other) const {
        return term_type == other.term_type;
    }

    bool operator==(BaseType const &base_type) const {
        return term_type == std::variant<BaseType, lambda_t>(base_type);
    }

    bool is_valid() const { return *this != BaseType::INVALID; }

    friend std::ostream &operator<<(std::ostream &os, TermType const &tt) {
        auto visit_print = [&os](auto &&term_type) -> void {
            print(os, term_type);
        };
        std::visit(visit_print, tt.term_type);
        return os;
    }

    static void print(std::ostream &os, BaseType base_type) {
        using enum BaseType;
        switch (base_type) {
        case INTEGER:
            os << "INTEGER";
            break;
        case CHARACTER:
            os << "CHARACTER";
            break;
        case BOOLEAN:
            os << "BOOLEAN";
            break;
        case INVALID:
            os << "INVALID";
            break;
        }
    }

    static void print(std::ostream &os, lambda_t lambda_type) {
        os << "( ";
        os << *lambda_type.first.get();
        os << " )";
        os << " -> ";
        os << "( ";
        os << *lambda_type.second.get();
        os << " )";
    }

    bool is_base_type() const { return tag == BASE; }
    bool is_lambda_type() const { return tag == LAMBDA; }
    BaseType get_base_type() const { return std::get<0>(term_type); };
    lambda_t get_lambda_type() const { return std::get<1>(term_type); };
    bool can_apply_on(TermType const &applied) const {
        if (!is_lambda_type()) {
            return false;
        }
        return *std::get<1>(term_type).first.get() == applied;
    }
    TermType get_input_type() const {
        if (!is_lambda_type()) {
            return TermType(); // defaults to error type
        }

        return *get_lambda_type().first.get();
    }
    TermType get_output_type() const {
        if (!is_lambda_type()) {
            return TermType(); // defaults to error type
        }
        return *get_lambda_type().second.get();
    }
};

struct Term {
    // NOTE: for a lambda, this is the param type
    using term_type_t = std::shared_ptr<TermType>;
    std::shared_ptr<TermType> term_type;

    using var_t = std::string;

    // the first term should be a lambda: A -> B
    // the second term should be of type: A
    using app_t = std::pair<std::shared_ptr<Term>, std::shared_ptr<Term>>;

    // arg name, lambda body?
    using lambda_t = std::pair<var_t, std::shared_ptr<Term>>;

    std::variant<var_t, app_t, lambda_t> term;
    Term(term_type_t tt, var_t var_name)
        : term_type(std::move(tt)), term(std::string(var_name)) {}
    Term(term_type_t tt, app_t application)
        : term_type(std::move(tt)), term(std::move(application)) {}
    Term(term_type_t tt, lambda_t lambda)
        : term_type(std::move(tt)), term(std::move(lambda)) {}

    friend std::ostream &operator<<(std::ostream &os, Term const &tt) {
        std::visit([&os](auto &&thing) { print(os, thing); }, tt.term);
        return os;
    }

    // gets the actual type of term
    static term_type_t
    eval_type(Term const &term,
              std::unordered_map<std::string_view, term_type_t> &env) {
        // need to compare term.term against term.term_type
        // get the type from the term
        term_type_t actual_type = std::visit(
            [&env](auto &&term) { return eval_type(term, env); }, term.term);
        return actual_type;
    }

    friend bool type_check(Term const &term) {
        std::unordered_map<std::string_view, term_type_t> var_name_to_type;
        return *eval_type(term, var_name_to_type).get() ==
               *term.term_type.get();
    }

    static term_type_t
    eval_type(var_t const &var_name,
              std::unordered_map<std::string_view, term_type_t> environment) {

        if (environment.contains(var_name)) {
            return environment[var_name];
        } else {
            return std::make_shared<TermType>(TermType::BaseType::INVALID);
        }
    }

    static term_type_t
    eval_type(app_t const &application_term,
              std::unordered_map<std::string_view, term_type_t> environment) {
        // app ( TERM1 ) ( TERM2 )

        // get type of TERM2
        term_type_t input_type =
            eval_type(*application_term.second, environment);

        // get that we can apply TERM1 onto TERM2
        if (!application_term.first->term_type->can_apply_on(*input_type)) {
            return std::make_shared<TermType>(TermType::BaseType::INVALID);
        }

        // if we can, we want the output type of TERM1
        return std::make_shared<TermType>(
            application_term.first->term_type->get_output_type());
    }

    static term_type_t
    eval_type(lambda_t const &lambda_term,
              std::unordered_map<std::string_view, term_type_t> environment) {
        // update the environment
        term_type_t param_type = lambda_term.second->term_type;

        // bind param_type to the lambda_term's first name
        environment.insert({lambda_term.first, param_type});

        // get the body's type
        term_type_t body_type = eval_type(*lambda_term.second, environment);

        // unbind
        environment.erase(lambda_term.first);

        return std::make_shared<TermType>(param_type, body_type);
    }

    static void print(std::ostream &os, var_t const &var_name) {
        os << "term type - variable name: ";
        os << var_name << " ";
    }

    static void print(std::ostream &os, app_t const &application) {
        os << "term type - application: ";
        os << *application.first.get() << " ";
        os << *application.second.get();
    }

    static void print(std::ostream &os, lambda_t const &lambda) {
        os << "term type - lambda arg[" << lambda.first << "]{";
        os << *lambda.second.get() << "} ";
    }
};

inline Parser<std::shared_ptr<Term>> type_parser() {
    // maps
    static auto int_base_type = []([[maybe_unused]] std::string_view) {
        return std::make_shared<TermType>(TermType::BaseType::INTEGER);
    };

    static auto char_base_type = []([[maybe_unused]] std::string_view) {
        return std::make_shared<TermType>(TermType::BaseType::CHARACTER);
    };

    static auto bool_base_type = []([[maybe_unused]] std::string_view) {
        return std::make_shared<TermType>(TermType::BaseType::BOOLEAN);
    };

    static auto skip_ws = skip_many(char_parser(' '));
    static auto l_paren = between(skip_ws, skip_ws, char_parser('('));
    static auto r_paren = between(skip_ws, skip_ws, char_parser(')'));

    static Parser<std::shared_ptr<TermType>> INTEGER =
        between(skip_ws, skip_ws, str_parser("int")) <<= int_base_type;
    static Parser<std::shared_ptr<TermType>> CHAR =
        between(skip_ws, skip_ws, str_parser("char")) <<= char_base_type;
    static Parser<std::shared_ptr<TermType>> BOOLEAN =
        between(skip_ws, skip_ws, str_parser("bool")) <<= bool_base_type;

    static Parser<std::shared_ptr<TermType>> base_types =
        any_of_parser(INTEGER, CHAR, BOOLEAN);

    using base_arrow_type_t =
        std::pair<std::shared_ptr<TermType>, std::shared_ptr<TermType>>;

    static Parser<std::shared_ptr<TermType>> base_arrow_type =
        (base_types * base_types) <<=
        [](base_arrow_type_t const &pair) -> std::shared_ptr<TermType> {
        return std::make_shared<TermType>(pair);
    };

    static auto pair_into_lambda_type =
        [](base_arrow_type_t const &pair) -> std::shared_ptr<TermType> {
        return std::make_shared<TermType>(pair);
    };

    static auto arrow_keyword = between(skip_ws, skip_ws, str_parser("->"));

    // I need address stability for this thing (after we return this parser
    // needs to remain in scope)
    static Parser<std::shared_ptr<TermType>> type;
    static Parser<std::shared_ptr<TermType>> type_case_1 =
        surrounding(arrow_keyword, between(l_paren, r_paren, ref_parser(&type)),
                    ref_parser(&type)) <<= pair_into_lambda_type;

    static Parser<std::shared_ptr<TermType>> type_case_2 =
        between(l_paren, r_paren, ref_parser(&type));

    static Parser<std::shared_ptr<TermType>> type_case_3 =
        surrounding(arrow_keyword, base_types, ref_parser(&type)) <<=
        pair_into_lambda_type;

    static Parser<std::shared_ptr<TermType>> type_case_4 = base_types;
    type = any_of_parser(type_case_1, type_case_2, type_case_3, type_case_4);

    static auto colon = between(skip_ws, skip_ws, char_parser(':'));

    auto into_term = []<typename T>(std::pair<Term::term_type_t, T> const &pair)
        -> std::shared_ptr<Term> {
        return std::make_shared<Term>(pair.first, pair.second);
    };
    static Parser<Term::var_t> varname =
        skip_ws > some(any_of_char_parser("abcdefghijklmnopqrstuvwxyz")) <<=
        [](std::vector<char> const &char_list) {
            std::string str;
            str.reserve(char_list.size());
            for (char c : char_list) {
                str += c;
            }
            return str;
        };

    static Parser<std::shared_ptr<Term>> term;

    static Parser<Term::lambda_t> lambda =
        skip_ws >
        (str_parser("\\") > ((varname) * (skip_ws > ref_parser(&term))));

    static Parser<Term::app_t> application =
        between(skip_ws, skip_ws, str_parser("app")) >
        between(l_paren, r_paren, ref_parser(&term)) *
            between(l_paren, r_paren, ref_parser(&term));

    term = any_of_parser(surrounding(colon, type, lambda) <<= into_term,
                         surrounding(colon, type, application) <<= into_term,
                         surrounding(colon, type, varname) <<= into_term);
    return term;
}
