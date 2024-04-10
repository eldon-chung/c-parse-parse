#pragma once

#include <cassert>
#include <stddef.h>

#include <functional>
#include <iostream>
#include <memory>
#include <optional>
#include <ostream>
#include <string>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

#include "combinator.h"

// some nice template utils
template <class T, class... Ts> struct head_type { using type = T; };

template <class T, class... Ts>
struct is_all_same
    : std::conditional_t<std::same_as<typename head_type<Ts...>::type, T>,
                         is_all_same<Ts...>, std::false_type> {};

template <class T, class U> struct is_all_same<T, U> : std::is_same<T, U> {};
template <class T> struct is_all_same<T> : std::true_type {};

template <typename... Ts>
concept Homogenous = requires {
    is_all_same<Ts..., int>();
};

struct NullParseType {
    NullParseType() {}

    friend std::ostream &operator<<(std::ostream &os,
                                    NullParseType const &npt) {
        os << "null";
        return os;
    }
};

// This is how we store
template <typename T> struct ParseResult {

    using result_type = T;
    using null_result_type = NullParseType;

    std::optional<T> maybe_result;
    std::string_view remaining;

    ParseResult(std::string_view remaining)
        : maybe_result(std::nullopt), remaining(remaining) {}
    ParseResult(T t, std::string_view remaining)
        : maybe_result(std::move(t)), remaining(remaining) {}

    operator bool() const { return maybe_result.has_value(); }
    // operator std::string_view() const { return remaining; }
    T const &get_result() const { return maybe_result.value(); }
    bool has_result() const { return maybe_result.has_value(); }

    // template <typename F>
    // ParseResult<std::result_of<F(T)>> operator()(F map) const {
    //     if (maybe_result) {
    //         return ParseResult<std::result_of<F(T)>>(map(*maybe_result),
    //                                                  remaining);
    //     } else {
    //         return ParseResult<std::result_of<F(T)>>(remaining);
    //     }
    // }
};

template <> struct ParseResult<NullParseType> {
    std::string_view remaining;
    bool success;
    ParseResult(std::string_view remaining, bool success = true)
        : remaining(remaining), success(success) {}

    operator bool() const { return success; }
    operator std::string_view() const { return remaining; }
};

// This is the parser container
template <typename T> struct Parser {
    using result_type = T;
    using return_type = ParseResult<T>;

    using parse_func_type = std::function<ParseResult<T>(std::string_view)>;
    parse_func_type parse_func;

    constexpr Parser() {}

    constexpr Parser(parse_func_type pf) : parse_func(std::move(pf)) {}

    constexpr ParseResult<T> operator()(std::string_view sv) const {
        return parse_func(sv);
    }

    friend std::ostream &operator<<(std::ostream &os, Parser<T> const &parser) {
        os << parser.name << std::endl;
        return os;
    }
};

template <typename... Parsers> auto any_of_parser(Parsers... parsers) {
    // Parser<A>, Parser<B>, ...
    // get A, B, ...

    using return_type =
        typename head_type<typename Parsers::result_type...>::type;

    return Parser<return_type>(
        [... parsers = std::forward<Parsers>(parsers)](
            std::string_view sv) -> ParseResult<return_type> {
            return any_of_eval<return_type>(sv, parsers...);
        });
}

template <typename T, typename F>
Parser<std::result_of_t<F(T)>> operator<<=(Parser<T> const &parser,
                                           F post_application) {
    return map(post_application, parser);
}

inline Parser<NullParseType> eof_parser() {
    return Parser<NullParseType>(
        [](std::string_view sv) -> ParseResult<NullParseType> {
            if (sv.empty()) {
                return ParseResult<NullParseType>(sv);
            } else {
                return ParseResult<NullParseType>(sv, false);
            }
        });
}

template <typename T> Parser<T> pure_parser(T t) {
    return Parser<T>([t](std::string_view sv) -> ParseResult<T> {
        return ParseResult(t, sv);
    });
}

inline Parser<char> char_parser(char ch) {
    return Parser<char>([ch](std::string_view sv) -> ParseResult<char> {
        if (sv.empty() || sv.front() != ch) {
            return ParseResult<char>(sv);
        }
        return ParseResult<char>(ch, sv.substr(1));
    });
};

inline Parser<std::string_view> str_parser(std::string_view expect) {
    return Parser<std::string_view>(
        [expect](std::string_view sv) -> ParseResult<std::string_view> {
            if (sv.starts_with(expect)) {
                return ParseResult(expect, sv.substr(expect.size()));
            }
            return ParseResult<std::string_view>(sv);
        });
};

// this requires T to be default constructible,
// can we make do without it?
template <typename T> Parser<NullParseType> skip(Parser<T> const &to_skip) {
    return Parser<NullParseType>([to_skip](std::string_view sv) {
        ParseResult<T> res = to_skip(sv);
        return ParseResult<NullParseType>(res.remaining);
    });
}

template <typename T>
Parser<NullParseType> skip_many(Parser<T> const &to_skip) {
    return Parser<NullParseType>([to_skip](std::string_view sv) {
        std::string_view remaining = sv;
        for (ParseResult<T> res = to_skip(remaining); res;
             remaining = res.remaining, res = to_skip(remaining)) {
        }

        return ParseResult<NullParseType>(remaining);
    });
}

// returns at some of P, with separators
template <typename T, typename U>
Parser<std::vector<T>> seperated_by(Parser<T> const &p,
                                    Parser<U> const &separator) {
    return Parser<std::vector<T>>([p, separator](std::string_view sv) {
        std::vector<T> parsed_results;

        ParseResult<T> last_result = p(sv);
        if (!last_result) {
            return ParseResult<std::vector<T>>({}, sv);
        }

        assert(last_result);
        parsed_results.push_back(std::move(last_result.get_result()));

        std::string_view remaining = last_result.remaining;
        ParseResult<U> sep_res(sv);
        while (!remaining.empty() && (sep_res = separator(remaining)) &&
               (last_result = p(sep_res.remaining))) {
            parsed_results.push_back(last_result.get_result());
            remaining = last_result.remaining;
        }
        return ParseResult<std::vector<T>>(std::move(parsed_results),
                                           remaining);
    });
}

template <typename M, typename L, typename R>
Parser<std::pair<L, R>> surrounding(Parser<M> const &middle,
                                    Parser<L> const &left,
                                    Parser<R> const &right) {
    return (left < middle) * right;
}

// returns at least one of P, with some separators
template <typename T, typename L, typename R>
Parser<T> between(Parser<L> const &left, Parser<R> const &right,
                  Parser<T> const &middle) {
    return Parser<T>([middle, left, right](std::string_view sv) {
        auto left_res = left(sv);

        if (!left_res) {
            return ParseResult<T>(sv);
        }

        auto mid_res = middle(left_res.remaining);

        if (!mid_res) {
            return ParseResult<T>(sv);
        }

        auto right_res = right(mid_res.remaining);
        if (!right_res) {
            return ParseResult<T>(sv);
        }

        return ParseResult<T>(mid_res.get_result(), right_res.remaining);
    });
}

inline Parser<char> any_char_except(std::string_view except) {
    return Parser<char>([except](std::string_view sv) -> ParseResult<char> {
        if (sv.empty()) {
            return ParseResult<char>(sv);
        }

        size_t found = except.find(sv.front());
        if (found != std::string::npos) {
            return ParseResult<char>(sv);
        }
        return ParseResult<char>(sv.front(), sv.substr(1));
    });
}

inline Parser<char> any_of_char_parser(std::string_view list_of_char) {
    return Parser<char>(
        [list_of_char](std::string_view sv) -> ParseResult<char> {
            if (sv.empty()) {
                return ParseResult<char>(sv);
            }

            if (size_t found_idx = list_of_char.find(sv.front());
                std::string::npos != found_idx) {
                return ParseResult<char>(list_of_char[found_idx], sv.substr(1));
            }
            return ParseResult<char>(sv);
        });
};

template <typename T>
Parser<T> operator+(Parser<T> const &left, Parser<T> const &right) {
    return Parser<T>([left, right](std::string_view sv) -> ParseResult<T> {
        ParseResult<T> result1 = left(sv);
        if (result1) {
            return result1;
        }

        ParseResult<T> result2 = right(sv);
        if (result2) {
            return result2;
        }

        return ParseResult<T>(sv);
    });
}

template <typename T, typename U>
Parser<std::variant<T, U>> operator+(Parser<T> const &left,
                                     Parser<U> const &right) {
    return Parser<std::variant<T, U>>(
        [left, right](std::string_view sv) -> ParseResult<std::variant<T, U>> {
            using variant_type = std::variant<T, U>;

            ParseResult<T> result1 = left(sv);
            if (result1) {
                return ParseResult<variant_type>(
                    std::variant<T, U>(std::in_place_index<0>,
                                       result1.get_result()),
                    result1.remaining);
            }

            ParseResult<U> result2 = right(sv);
            if (result2) {
                return ParseResult<variant_type>(
                    std::variant<T, U>(std::in_place_index<1>,
                                       result2.get_result()),
                    result2.remaining);
            }

            return ParseResult<variant_type>(sv);
        });
}

template <typename T, typename U>
Parser<std::pair<T, U>> operator*(Parser<T> const &left,
                                  Parser<U> const &right) {
    return Parser<std::pair<T, U>>(
        [left, right](std::string_view sv) -> ParseResult<std::pair<T, U>> {
            using pair_type = std::pair<T, U>;

            ParseResult<T> result1 = left(sv);
            if (!result1) {
                return ParseResult<pair_type>(sv);
            }

            ParseResult<U> result2 = right(result1.remaining);
            if (!result2) {
                return ParseResult<pair_type>(sv);
            }

            return ParseResult<pair_type>(
                std::make_pair(result1.get_result(), result2.get_result()),
                result2.remaining);
        });
}

template <typename T, typename F>
Parser<std::result_of_t<F(T)>> map(F mapper_func, Parser<T> const &parser) {
    return Parser<std::result_of_t<F(T)>>(
        [parser, mapper_func](std::string_view sv) {
            auto result = parser(sv);
            if (!result) {
                return ParseResult<std::result_of_t<F(T)>>(sv);
            }

            return ParseResult<std::result_of_t<F(T)>>(
                mapper_func(std::move(result.get_result())), result.remaining);
        });
}

template <typename F> auto lift(F lift_func) {
    return [lift_func]<typename T>(
               Parser<T> const &parser) -> std::result_of_t<F(Parser<T>)> {
        return lift_func(parser);
    };
}

template <typename T> auto many(Parser<T> const &parser) {
    return Parser<std::vector<T>>([parser](std::string_view sv) {
        std::vector<T> parsed_list;

        auto parse_result = parser(sv);
        while (parse_result) {
            parsed_list.push_back(parse_result.get_result());
            parse_result = parser(parse_result.remaining);
        }
        return ParseResult<std::vector<T>>(std::move(parsed_list),
                                           parse_result.remaining);
    });
}

template <typename T> auto some(Parser<T> const &parser) {
    return Parser<std::vector<T>>([parser](std::string_view sv) {
        std::vector<T> parsed_list;
        auto parse_result = parser(sv);

        if (!parse_result) {
            return ParseResult<std::vector<T>>(sv);
        }

        parsed_list.push_back(parse_result.get_result());
        for (parse_result = parser(parse_result.remaining); parse_result;
             parse_result = parser(parse_result.remaining)) {
            parsed_list.push_back(parse_result.get_result());
        }
        return ParseResult<std::vector<T>>(std::move(parsed_list),
                                           parse_result.remaining);
    });
}

template <typename T> auto repeat(Parser<T> const &parser, size_t count) {
    return Parser<std::vector<T>>([parser, count](std::string_view sv) {
        std::vector<T> parsed_list;
        ParseResult<T> parse_result = parser(sv);
        for (size_t c = 0; c < count;
             ++c, parse_result = parser(parse_result.remaining)) {
            if (!parse_result) {
                return ParseResult<std::vector<T>>(sv);
            }
            parsed_list.push_back(parse_result.get_result());
        }

        return ParseResult<std::vector<T>>(std::move(parsed_list),
                                           parse_result.remaining);
    });
}

template <typename T, typename U>
Parser<T> operator<(Parser<T> const &left, Parser<U> const &right) {
    return Parser<T>([left, right](std::string_view sv) {
        auto left_result = left(sv);
        if (!left_result) {
            return ParseResult<T>(sv);
        }

        auto right_result = right(left_result.remaining);
        if (!right_result) {
            return ParseResult<T>(sv);
        }

        return ParseResult<T>(left_result.get_result(), right_result.remaining);
    });
}

template <typename T, typename U>
Parser<U> operator>(Parser<T> const &left, Parser<U> const &right) {
    return Parser<U>([left, right](std::string_view sv) {
        auto left_result = left(sv);
        if (!left(sv)) {
            return ParseResult<U>(sv);
        }
        auto right_result = right(left_result.remaining);
        if (!right_result) {
            return ParseResult<U>(sv);
        }

        return right_result;
    });
}

// Want ReturnType<std::variant<A, B, ...>>
// TODO: think about an alternative where
// we dont use std::in_place_index<N>
template <size_t N, typename... ResultTypes, typename T, typename... Parsers>
ParseResult<std::variant<ResultTypes...>>
alt_eval(std::string_view sv, Parser<T> const &parser, Parsers... parsers) {
    ParseResult<T> parse_result = parser(sv);
    if (parse_result) {
        return ParseResult(
            std::variant<ResultTypes...>(std::in_place_index<N>,
                                         parse_result.get_result()),
            parse_result.remaining);
    }

    if constexpr (sizeof...(parsers) == 0) {
        return ParseResult<std::variant<ResultTypes...>>(sv);
    } else {
        return alt_eval<N + 1, ResultTypes...>(sv, parsers...);
    }
}

template <typename... Parsers> auto alt(Parsers... parsers) {
    // Parser<A>, Parser<B>, ...
    // get A, B, ...

    // This is std::variant<A, B, ...>
    using variant_type =
        typename std::variant<typename Parsers::result_type...>;

    return Parser<variant_type>(
        [... parsers = std::forward<Parsers>(parsers)](
            std::string_view sv) -> ParseResult<variant_type> {
            return alt_eval<0, typename Parsers::result_type...>(sv,
                                                                 parsers...);
        });
}

// used for debugging
inline Parser<NullParseType> debug(std::ostream &os, std::string_view msg) {
    return Parser<NullParseType>([&os, msg](std::string_view sv) {
        os << "at " << sv << ":" << msg << std::endl;
        return ParseResult<NullParseType>(sv);
    });
}

template <typename T> Parser<T> ref_parser(Parser<T> const *parser) {
    return Parser<T>([parser](std::string_view sv) -> ParseResult<T> {
        return (*parser)(sv);
    });
}

template <typename T, typename... Parsers>
ParseResult<std::tuple<T, typename Parsers::result_type...>>
seq_eval(std::string_view sv, Parser<T> const &parser, Parsers... parsers) {
    if constexpr (sizeof...(parsers) == 0) {
        ParseResult<T> res = parser(sv);
        if (!res) {
            return ParseResult<std::tuple<T>>(sv);
        } else {
            return ParseResult<std::tuple<T>>(std::tuple<T>(res.get_result()),
                                              res.remaining);
        }
    } else {
        ParseResult<T> res = parser(sv);
        // otherwise we recurse
        if (!res) { // else fail all the results
            return ParseResult<std::tuple<T, typename Parsers::result_type...>>(
                sv);
        }
        // get the remaining reslts
        ParseResult<std::tuple<typename Parsers::result_type...>> remaining =
            seq_eval(res.remaining, parsers...);
        // then we need to append our result and apply it
        if (!remaining) {
            return ParseResult<std::tuple<T, typename Parsers::result_type...>>(
                sv);
        } else {
            auto final_tuple = std::tuple_cat(std::tuple<T>(res.get_result()),
                                              remaining.get_result());
            return ParseResult<std::tuple<T, typename Parsers::result_type...>>(
                final_tuple, remaining.remaining);
        }
    }
}

template <typename... Parsers> auto seq(Parsers... parsers) {
    // Parser<A>, Parser<B>, ...
    // get A, B, ...

    // This is std::tuple<A, B, ...>
    using tuple_type = typename std::tuple<typename Parsers::result_type...>;

    return Parser<tuple_type>(
        [... parsers = std::forward<Parsers>(parsers)](std::string_view sv)
            -> ParseResult<tuple_type> { return seq_eval(sv, parsers...); });
}

// takes a map function, outputs a parser combinator with that
// as the map
template <typename F> auto lift1(F mapper_func) {
    return [mapper_func]<typename T>(Parser<T> const &parser1) {
        return map(mapper_func, parser1);
    };
}

// takes a map function, outputs a parser combinator with that
// as the map
template <typename F> auto lift2(F mapper_func) {
    return [mapper_func]<typename T>(Parser<T> const &parser1,
                                     Parser<T> const &parser2) {
        return map(mapper_func, parser1, parser2);
    };
}

template <typename T, typename F>
auto maybe(Parser<T> const &parser, F const &success_value,
           F const &default_value) {
    return Parser<F>(
        [parser, success_value, default_value](std::string_view sv) {
            auto parse_result = parser(sv);
            if (!parse_result) {
                return ParseResult(default_value, sv);
            } else {
                return ParseResult(success_value, parse_result.remaining);
            }
        });
}

template <typename T, typename... Parsers>
ParseResult<T> any_of_eval(std::string_view sv, Parser<T> const &parser,
                           Parsers... parsers) {
    ParseResult<T> parse_result = parser(sv);

    if (parse_result) {
        return parse_result;
    }

    if constexpr (sizeof...(parsers) == 0) {
        return ParseResult<T>(sv);
    } else {
        return any_of_eval<T>(sv, parsers...);
    }
}

template <typename T>
Parser<T> char_switch_parser(std::string_view switch_char,
                             std::vector<T> to_return) {
    assert(switch_char.size() + 1 == to_return.size());
    return Parser<T>(
        [to_return, switch_char](std::string_view sv) -> ParseResult<T> {
            if (switch_char.empty()) {
                return ParseResult<T>(sv);
            }

            size_t found_idx = switch_char.find(sv.front());
            if (found_idx == std::string::npos) {
                // return the default value
                return ParseResult<T>(to_return.back(), sv);
            }

            assert(found_idx < to_return.size());
            return ParseResult<T>(to_return[found_idx], sv.substr(1));
        });
}

class Printer {

    template <typename T> static void print_help(std::vector<T> const &vec) {
        for (auto const &v : vec) {
            std::cout << "{ elem: ";
            print_help(v);
            std::cout << "}";
        }
    }

    template <typename T, typename... Ts>
    static void print_tuple(T const &t, Ts const &...ts) {
        std::cout << "{ parsed_obj: " << t << "}";
        if constexpr (sizeof...(ts) >= 1) {
            std::cout << ",";
            print_help(ts...);
        }
    }

    template <typename... Ts>
    static void print_help(std::tuple<Ts...> const &items) {

        auto tuple_printer = [](Ts const &...items) {
            std::cout << "< tuple - ";
            ((print_help(items), std::cout << ","), ...);
            std::cout << ">";
        };
        // now we do a recursive print?
        std::apply(tuple_printer, items);
    }

    template <typename... Ts>
    static void print_help(std::variant<Ts...> const &pr) {
        std::visit(
            [](auto &&parsed_item) {
                std::cout << "{ variant: ";
                print_help(parsed_item);
                std::cout << "}";
            },
            pr);
    }

    template <typename T, typename U>
    static void print_help(std::pair<T, U> const &pair) {
        std::cout << "{ pair: ";
        print_help(pair.first);
        std::cout << ",";
        print_help(pair.second);
        std::cout << "}";
    }

    template <typename T> static void print_help(std::shared_ptr<T> const &pr) {
        std::cout << "{ shared_ptr @ " << pr << " : value = " << *pr.get()
                  << "}";
    }
    template <typename T> static void print_help(T const &t) {
        std::cout << "{ parsed_obj: " << t << "}";
    }

  public:
    template <typename T> static void print_result(ParseResult<T> const &pr) {
        std::cout << "Parse Result Print ==================" << std::endl;
        if (pr) {
            std::cout << "parse success" << std::endl;
            print_help(pr.get_result());
            std::cout << std::endl;
        } else {
            std::cout << "{ parse fail } " << std::endl;
        }
        std::cout << "^ remaining str \"" << pr.remaining << "\"" << std::endl;
        std::cout << "====================================" << std::endl;
    }

    template <> void print_result(ParseResult<NullParseType> const &pr) {
        std::cout << "Parse Result Print ==================" << std::endl;
        if (pr) {
            std::cout << "parse success." << std::endl;
            std::cout << "null type.";
            std::cout << std::endl;
        } else {
            std::cout << "{ parse fail } " << std::endl;
        }
        std::cout << "^ remaining str \"" << pr.remaining << "\"" << std::endl;
        std::cout << "====================================" << std::endl;
    }
};

template <typename T, typename F>
Parser<T> chainl(Parser<T> const &p, F bin_op) {
    return Parser<T>([p, bin_op](std::string_view sv) -> ParseResult<T> {
        if (sv.empty()) {
            return ParseResult<T>(sv);
        }

        ParseResult<T> first = p(sv);
        if (!first) {
            return ParseResult<T>(sv);
        }

        ParseResult<T> second = p(first.remaining);
        if (!second) {
            return first;
        }

        T intermediate = bin_op(first.get_result(), second.get_result());
        std::string_view remaining = second.remaining;

        for (auto next = p(remaining); next && !remaining.empty();
             next = p(remaining)) {
            intermediate = bin_op(intermediate, next.get_result());
            remaining = next.remaining;
        }
        return ParseResult<T>(intermediate, remaining);
    });
}

template <typename T, typename F, typename A>
Parser<A> foldl(Parser<T> const &p, F bin_op, A initial_value) {
    return Parser<A>([p, bin_op,
                      initial_value](std::string_view sv) -> ParseResult<A> {
        if (sv.empty()) {
            return ParseResult<A>(initial_value, sv);
        }

        ParseResult<T> first = p(sv);
        if (!first) {
            return ParseResult<A>(initial_value, sv);
        }

        A intermediate = bin_op(initial_value, first.get_result());

        ParseResult<T> second = p(first.remaining);
        if (!second) {
            return ParseResult<A>(std::move(intermediate), first.remaining);
        }

        intermediate =
            std::move(bin_op(std::move(intermediate), second.get_result()));
        std::string_view remaining = second.remaining;

        for (auto next = p(remaining); next && !remaining.empty();
             next = p(remaining)) {
            intermediate = std::move(bin_op(intermediate, next.get_result()));
            remaining = next.remaining;
        }
        return ParseResult<A>(std::move(intermediate), remaining);
    });
}

// Note: this greedily consumes from the left still.
template <typename T, typename F>
Parser<T> chainr(Parser<T> const &p, F bin_op) {
    return Parser<T>([p, bin_op](std::string_view sv) -> ParseResult<T> {
        ParseResult<T> left = p(sv);
        if (!left) {
            return ParseResult<T>(sv);
        }

        ParseResult<T> remaining = (chainr(p, bin_op))(left.remaining);
        if (!remaining) {
            return left;
        }
        return ParseResult<T>(bin_op(left.get_result(), remaining.get_result()),
                              remaining.remaining);
    });
}

template <typename T> Parser<NullParseType> lookahead(Parser<T> const &p) {
    return Parser<NullParseType>([p](std::string_view sv) {
        if (sv.empty()) {
            return ParseResult<NullParseType>(sv, false);
        }
        auto lookahead = p(sv);
        if (p(sv)) {
            return ParseResult<NullParseType>(sv, true);
        } else {
            return ParseResult<NullParseType>(sv, false);
        }
    });
};

template <typename F, typename T1, typename T2>
Parser<std::invoke_result_t<F, T1, T2>>
map(F prefix_op, Parser<T1> const &parser1, Parser<T2> const &parser2) {
    return Parser<std::invoke_result_t<F, T1, T2>>(
        [prefix_op, parser1, parser2](std::string_view sv)
            -> ParseResult<std::invoke_result_t<F, T1, T2>> {
            auto op1 = parser1(sv);
            if (!op1) {
                return ParseResult<std::invoke_result_t<F, T1, T2>>(sv);
            }

            auto op2 = parser2(op1.remaining);
            if (!op2) {
                return ParseResult<std::invoke_result_t<F, T1, T2>>(sv);
            }

            return ParseResult<std::invoke_result_t<F, T1, T2>>(
                prefix_op(op1.get_result(), op2.get_result()), op2.remaining);
        });
}
