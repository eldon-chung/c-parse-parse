#include "json.hpp"
#include "combinator.h"

#include <charconv>
#include <cstddef>
#include <initializer_list>
#include <memory>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

namespace Nson {

Json::Json(std::initializer_list<std::pair<std::string, Json>> list)
    : ptr(std::make_shared<Value>(Object(list))) {}
Json::Json(std::initializer_list<Json> list)
    : ptr(std::make_shared<Value>(Array(list))) {}
Json::Json(char const *chr) : ptr(std::make_shared<Value>(std::string(chr))) {}
Json::Json(double d) : ptr(std::make_shared<Value>(Number(d))) {}
Json::Json(int d) : ptr(std::make_shared<Value>(Number(d))) {}
Json::Json(std::string str)
    : ptr(std::make_shared<Value>(String(std::move(str)))) {}
Json::Json(std::vector<Json> v)
    : ptr(std::make_shared<Value>(Array(std::move(v)))) {}
Json::Json(std::unordered_map<std::string, Json> m)
    : ptr(std::make_shared<Value>(Object(std::move(m)))) {}
Json::Json(bool b) : ptr(std::make_shared<Value>(Literal(b))) {}
Json::Json(std::nullptr_t) : ptr(std::make_shared<Value>(Literal(nullptr))) {}

bool Json::operator==(Json const &other) const { return *ptr == *other.ptr; }

Json Json::array() { return std::make_shared<Value>(Array()); }
Json Json::array(std::vector<Json> v) {
    return std::make_shared<Value>(std::move(v));
}
Json Json::array(std::initializer_list<Json> init_list) {
    return Json::array(std::vector<Json>(init_list));
}

Json Json::object() { return std::make_shared<Value>(Object()); }
Json Json::object(std::unordered_map<std::string, Json> m) {
    return std::make_shared<Value>(Object(std::move(m)));
}
Json Json::object(
    std::initializer_list<std::pair<std::string, Json>> init_list) {
    std::unordered_map<std::string, Json> m(init_list.begin(), init_list.end());
    return Json::object(std::move(m));
}

Json Json::number(double num) { return std::make_shared<Value>(Number(num)); }
Json Json::string(std::string str) {
    return std::make_shared<Value>(String(std::move(str)));
}
Json Json::string() { return std::make_shared<Value>(String()); }
Json Json::true_literal() {
    return std::make_shared<Value>(Literal::true_literal());
}
Json Json::false_literal() {
    return std::make_shared<Value>(Literal::false_literal());
}
Json Json::null_literal() {
    return std::make_shared<Value>(Literal::null_literal());
}
Json Json::value(double n) { return Json::number(n); }
Json Json::value(int n) { return Json::number(n); }
Json Json::value(std::string str) { return Json::string(std::move(str)); }
Json Json::value(std::vector<Json> v) { return Json::array(std::move(v)); }
Json Json::value(std::unordered_map<std::string, Json> m) {
    return Json::object(std::move(m));
}
Json Json::value(bool b) {
    return (b) ? Json::true_literal() : Json::false_literal();
}
Json Json::value(std::nullptr_t) { return Json::null_literal(); }

Json &Json::operator[](std::string const &key) {
    return std::get<Object>(ptr->alternation)[key];
}
Json const &Json::operator[](size_t idx) const {
    return std::get<Array>(ptr->alternation)[idx];
}
Json &Json::operator[](size_t idx) {
    return std::get<Array>(ptr->alternation)[idx];
}

bool Json::is_list() const {
    return std::holds_alternative<Array>(ptr->alternation);
}
bool Json::is_literal() const {
    return std::holds_alternative<Literal>(ptr->alternation);
}
bool Json::is_object() const {
    return std::holds_alternative<Object>(ptr->alternation);
}
bool Json::is_num() const {
    return std::holds_alternative<Number>(ptr->alternation);
}
bool Json::is_string() const {
    return std::holds_alternative<String>(ptr->alternation);
}

Literal Json::as_json_literal() const {
    return std::get<Literal>(ptr->alternation);
}

String Json::as_json_string() const {
    return std::get<String>(ptr->alternation);
}

Number Json::as_json_number() const {
    return std::get<Number>(ptr->alternation);
}

Array const &Json::as_json_array() const {
    return std::get<Array>(ptr->alternation);
}

Object const &Json::as_json_object() const {
    return std::get<Object>(ptr->alternation);
}

std::string Json::as_string() const {
    return std::get<String>(ptr->alternation).value();
}

void Json::push_back(Json item) {
    std::get<Array>(ptr->alternation).json_array.push_back(std::move(item));
}

void Json::insert(std::string key, Json item) {
    std::get<Object>(ptr->alternation)
        .json_map.insert({std::move(key), std::move(item)});
}

void Json::insert(std::pair<std::string, Json> kvp) {
    std::get<Object>(ptr->alternation).json_map.insert(std::move(kvp));
}

// Parser stuff here

Parser<Json> number_parser();
Parser<Json> string_parser();
Parser<Json> literal_parser();

Parser<Json> literal_parser() {
    auto true_literal = str_parser("true") > pure_parser(Json::true_literal());
    auto false_literal =
        str_parser("false") > pure_parser(Json::false_literal());
    auto null_literal = str_parser("null") > pure_parser(Json::null_literal());

    return (true_literal + false_literal + null_literal);
}

Parser<Json> string_parser() {

    auto code_point_to_utf = [](std::vector<char> const &v) -> std::string {
        std::string str;
        str.push_back(v[3]);
        if (v[0] == 0 && v[1] == 0 && v[2] < 8) {
            str.back() |= ((v[2] & 7) << 4);
            return str;
        }

        str.front() |= ((v[2] & 3) << 4);
        str.front() |= (1 << 7);
        str.insert(str.begin(), (v[2] & 12) >> 2);

        if (v[0] == 0 && v[1] < 7) {
            str.front() |= ((v[1] & 7) << 2);
            str.front() |= (6 << 5);
            return str;
        }

        str.front() |= (v[1] << 2);
        str.front() |= (1 << 7);
        str.insert(str.begin(), v[0]);
        str.front() |= 0xE0;
        return str;
    };

    auto str_concat_combinator = lift2(std::plus());
    auto escaped_to_str = [](char c) -> std::string {
        switch (c) {
        case 'n':
            return "\n";
        case 'b':
            return "\b";
        case 'f':
            return "\f";
        case 'r':
            return "\r";
        case 't':
            return "\t";
        default:
            return std::string{c};
        }
    };

    Parser<std::vector<char>> code_point =
        str_parser("\\u") > repeat(
                                any_of_char_parser("0123456789ABCDEFabcdef") <<=
                                [](char c) -> char {
                                    if (c <= '9') {
                                        return c - '0';
                                    } else if (c <= 'F') {
                                        return 10 + c - 'A';
                                    } else {
                                        return 10 + c - 'a';
                                    }
                                },
                                4);

    Parser<std::string> utf = code_point <<= code_point_to_utf;
    Parser<std::string> control_char =
        utf + (char_parser('\\') > any_of_char_parser("\"/\\bfnrt") <<=
               escaped_to_str);

    Parser<std::string> normal_char = any_char_except("\"") <<=
        [](char c) { return std::string{c}; };
    Parser<std::string> string =
        between(char_parser('\"'), char_parser('\"'),
                foldl(control_char + normal_char, std::plus(), std::string()));
    return string <<=
           [](std::string str) { return Json::string(std::move(str)); };
}

Parser<Json> number_parser() {
    auto str_concat_combinator = lift2(std::plus());

    Parser<std::string> zero =
        any_of_char_parser("0") > pure_parser<std::string>("0");

    Parser<std::string> digit_1_to_9 = any_of_char_parser("123456789") <<=
        [](char c) { return std::string{c}; };

    Parser<std::string> digit = any_of_char_parser("0123456789") <<=
        [](char c) { return std::string{c}; };

    Parser<std::string> no_leading_zero =
        lookahead(digit_1_to_9) > (foldl(digit, std::plus(), std::string("")));

    Parser<std::string> some_digits =
        (foldl(digit, std::plus(), std::string("")));

    // this forbids leading zeros
    Parser<std::string> nonnegative_int = zero + no_leading_zero;

    Parser<std::string> integer = str_concat_combinator(
        maybe(char_parser('-'), std::string{"-"}, std::string{""}),
        nonnegative_int);

    Parser<std::string> fractional =
        char_parser('.') >
        str_concat_combinator(pure_parser(std::string{"."}), some_digits);

    Parser<std::string> fraction = str_concat_combinator(integer, fractional);

    // this is the base
    Parser<std::string> integer_or_frac = fraction + integer;

    Parser<std::string> exponent =
        any_of_char_parser("eE") >
        str_concat_combinator(
            char_switch_parser<std::string>("-+", {"e-", "e+", "e"}),
            some_digits);

    Parser<std::string> number =
        map(std::plus<std::string>(), integer_or_frac, exponent) +
        integer_or_frac;

    return (number <<= [](std::string str) {
        double val;
        std::from_chars(str.c_str(), str.c_str() + str.size(), val);
        return Json::number(val);
    });
}

Parser<Json> json_parser() {
    auto json_array_into_json_type = [](std::vector<Json> v) -> Json {
        return Json::array(std::move(v));
    };

    auto pair_list_to_object =
        [](std::vector<std::pair<Json, Json>> v_of_p) -> Json {
        Json object = Json::object();
        for (auto p : v_of_p) {
            assert(p.first.is_string());
            object.insert({std::move(p.first.as_string()), p.second});
        }
        return object;
    };

    auto skip_ws = many(any_of_char_parser(" \t\n\r"));
    auto comma = between(skip_ws, skip_ws, char_parser(','));
    auto colon = between(skip_ws, skip_ws, char_parser(':'));

    Parser<Json> json_value;

    Parser<Json> json_array =
        between(char_parser('[') < skip_ws, skip_ws > char_parser(']'),
                seperated_by(ref_parser(&json_value), comma)) <<=
        json_array_into_json_type;

    Parser<std::pair<Json, Json>> object_pair =
        between(skip_ws, skip_ws,
                surrounding(colon, string_parser(), ref_parser(&json_value)));

    Parser<Json> json_object =
        between(char_parser('{') < skip_ws, skip_ws > char_parser('}'),
                seperated_by(object_pair, comma) <<= pair_list_to_object);

    json_value =
        between(skip_ws, skip_ws,
                any_of_parser(json_object, string_parser(), number_parser(),
                              literal_parser(), json_array));
    return json_value;
}

std::string Json::dump(size_t indent_width, size_t indent_level,
                       bool indent_first_line) const {
    return std::visit(
        [indent_width, indent_level,
         indent_first_line](auto &&alt) -> std::string {
            return alt.dump(indent_width, indent_level, indent_first_line);
        },
        ptr->alternation);
}

std::string Json::dump(size_t indent_width) const {
    return Json::dump(indent_width, 0);
}

} // namespace Nson
