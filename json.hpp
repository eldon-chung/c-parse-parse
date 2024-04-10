#pragma once

#include "combinator.h"

#include <charconv>
#include <cstddef>
#include <memory>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

namespace Nson {

struct Value;
struct Object;
struct Array;
struct String;
struct Number;
struct Literal;

class Json {

    std::shared_ptr<Value> ptr;

    Json(std::shared_ptr<Value> p) : ptr(std::move(p)) {}

  public:
    Json(double);
    Json(int);
    Json(char const *);
    Json(std::string);
    Json(std::vector<Json>);
    Json(std::unordered_map<std::string, Json>);
    Json(bool);
    template <size_t N>
    Json(const char (&ar)[N])
        : ptr(std::make_shared<Value>(String(std::string(ar, N)))) {}
    Json(std::nullptr_t);

    template <typename... T> Json(T... items);
    Json(std::initializer_list<std::pair<std::string, Json>>);
    Json(std::initializer_list<Json>);
    ~Json() {}
    bool operator==(Json const &other) const;

    friend std::ostream &operator<<(std::ostream &os, Json const &sw) {
        os << sw.dump(2);
        return os;
    }

    std::string dump(size_t indent_width, size_t indent_level,
                     bool indent_first_line = true) const;
    std::string dump(size_t indent_width) const;

    // static Json value(std::initializer_list<std::pair<std::string, Json>>);
    // static Json value(std::initializer_list<Json>);
    static Json value(double);
    static Json value(int);
    static Json value(std::string);
    static Json value(std::vector<Json>);
    static Json value(Value);
    static Json value(std::unordered_map<std::string, Json>);
    static Json value(bool);
    template <size_t N> static Json value(const char (&ar)[N]) {
        return string(std::string(ar));
    }
    static Json value(std::nullptr_t);

    static Json array();
    static Json array(std::vector<Json>);
    static Json array(std::initializer_list<Json>);
    static Json object();
    static Json object(std::initializer_list<std::pair<std::string, Json>>);
    static Json object(std::unordered_map<std::string, Json>);
    static Json number(double);
    static Json string(std::string str);
    static Json string();
    template <size_t N> static Json string(const char (&ar)[N]) {
        return string(std::string(ar));
    }
    static Json true_literal();
    static Json false_literal();
    static Json null_literal();

    Json &operator[](std::string const &key);
    Json const &operator[](size_t idx) const;
    Json &operator[](size_t idx);

    bool is_list() const;
    bool is_literal() const;
    bool is_object() const;
    bool is_num() const;
    bool is_string() const;

    // going to forego typecast overloading for this.
    Literal as_json_literal() const;
    String as_json_string() const;
    Number as_json_number() const;
    Array const &as_json_array() const;
    Object const &as_json_object() const;

    std::string as_string() const;

    void push_back(Json item);
    void insert(std::string key, Json value);
    void insert(std::pair<std::string, Json> kvp);
};

inline std::vector<Json> lift_to_json_reversed(std::string const &str) {
    return std::vector<Json>({Json(str)});
}

template <typename T, typename... U>
std::vector<Json> lift_to_json_reversed(T item, U... items) {
    if constexpr (sizeof...(items) == 0) {
        std::vector<Json> to_return;
        to_return.push_back(std::move(Json(item)));
        return to_return;
    } else {
        std::vector<Json> ret_val = lift_to_json_reversed<U...>(items...);
        ret_val.push_back(Json(item));
        return ret_val;
    }
}

struct String {
    std::string json_string;

    String() : json_string() {}
    String(std::string s) : json_string(std::move(s)) {}

    bool operator==(String const &other) const = default;

    std::string const &value() const { return json_string; }

    std::string dump(size_t indent_width, size_t indent_level,
                     bool indent_first_line = true) const {

        std::string dump_str;
        if (indent_first_line) {
            dump_str += std::string(indent_width * indent_level, ' ');
        }
        dump_str += "\"" + json_string + "\"";
        return dump_str;
    }
};

struct Literal {

    enum class Lit {
        True,
        False,
        Null,
    };

    // how? what do we want to store here?
    Literal::Lit literal;
    Literal(Literal::Lit lit) : literal(lit) {}
    Literal(bool b) : literal((b) ? Lit::True : Lit::False) {}
    Literal(std::nullptr_t) : literal(Lit::Null) {}

    static Literal true_literal() { return Lit::True; }
    static Literal false_literal() { return Lit::False; }
    static Literal null_literal() { return Lit::Null; }

    bool is_true() { return literal == Lit::True; }
    bool is_false() { return literal == Lit::False; }
    bool is_null() { return literal == Lit::Null; }

    bool operator==(Literal const &other) const = default;

    operator bool() const { return (literal == Lit::True); }

    Literal value() const { return literal; }

    std::string dump(size_t indent_width, size_t indent_level,
                     bool indent_first_line = true) const {
        std::string dump_str;

        if (indent_first_line) {
            dump_str += std::string(indent_width * indent_level, ' ');
        }

        switch (literal) {
        case Lit::False:
            dump_str += "false";
            break;
        case Lit::True:
            dump_str += "true";
            break;
        case Lit::Null:
            dump_str += "null";
            break;
        }

        return dump_str;
    }
};

struct Number {
    double json_number;
    Number(double num) : json_number(num) {}

    bool operator==(Number const &other) const = default;

    double value() const { return json_number; }

    std::string dump(size_t indent_width, size_t indent_level,
                     bool indent_first_line = true) const {

        std::string dump_str;
        if (indent_first_line) {
            dump_str += std::string(indent_width * indent_level, ' ');
        }
        char buf[256];
        buf[255] = '\0';

        auto [ptr, errc] = std::to_chars(buf, buf + 255, json_number);
        *ptr = '\0';
        dump_str += buf;
        return dump_str;
    }
};

struct Object {
    std::unordered_map<std::string, Json> json_map;

    Object() : json_map({}) {}
    Object(std::unordered_map<std::string, Json> json_map)
        : json_map(std::move(json_map)) {}
    Object(std::initializer_list<std::pair<std::string, Json>> list)
        : json_map(list.begin(), list.end()) {}

    bool operator==(Object const &other) const = default;
    Json &operator[](std::string const &sv) { return json_map.at(sv); }

    std::string dump(size_t indent_width, size_t indent_level,
                     bool indent_first_line = true) const {
        std::string indent_pad = std::string(indent_width * indent_level, ' ');

        std::string dump_str;

        if (json_map.empty()) {
            if (indent_first_line) {
                return indent_pad + "{ }";
            } else {
                return "{ }";
            }
        }

        if (indent_first_line) {
            dump_str += indent_pad;
        }
        dump_str += "{\n";

        std::string closing_brace = indent_pad + "}";
        // grow by one more width for all your elements.
        indent_pad.resize(indent_pad.size() + indent_width, ' ');
        auto c_it = json_map.cbegin();
        while (c_it != json_map.cend()) {
            dump_str += indent_pad;

            dump_str += "\"";
            dump_str += c_it->first;
            dump_str += "\": ";
            dump_str +=
                c_it->second.dump(indent_width, indent_level + 1, false);

            if (std::next(c_it) != json_map.cend()) {
                dump_str += ",";
            }
            dump_str += "\n";
            ++c_it;
        }
        dump_str += closing_brace;
        return dump_str;
    }
};

struct Array {
    std::vector<Json> json_array;

    Array() : json_array({}) {}
    Array(std::vector<Json> array) : json_array(std::move(array)) {}

    bool operator==(Array const &other) const = default;

    Json const &operator[](size_t idx) const { return json_array[idx]; }
    Json &operator[](size_t idx) { return json_array[idx]; }

    std::string dump(size_t indent_width, size_t indent_level,
                     bool indent_first_line = true) const {
        std::string indent_pad = std::string(indent_width * indent_level, ' ');

        if (json_array.empty()) {
            if (indent_first_line) {
                return indent_pad + "[ ]";
            } else {
                return "[ ]";
            }
        }
        std::string dump_str;

        if (indent_first_line) {
            dump_str += indent_pad;
        }
        dump_str += "[\n";
        auto c_it = json_array.cbegin();
        while (c_it != json_array.cend()) {
            // dump_str += indent_pad;
            dump_str += c_it->dump(indent_width, indent_level + 1);
            if (std::next(c_it) != json_array.cend()) {
                dump_str += ",";
            }
            dump_str += "\n";
            ++c_it;
        }
        dump_str += indent_pad;
        dump_str += "]";
        return dump_str;
    }
};

struct Value {
    std::variant<Object, Array, String, Number, Literal> alternation;

    Value(Array vec) : alternation((std::move(vec))) {}
    Value(Object map) : alternation((std::move(map))) {}
    Value(String str) : alternation((std::move(str))) {}
    Value(Number num) : alternation((std::move(num))) {}
    Value(Literal lit) : alternation((std::move(lit))) {}

    bool operator==(Value const &other) const = default;
};

template <typename... T> Json::Json(T... items) {
    std::vector<Json> list = lift_to_json_reversed(items...);

    std::reverse(list.begin(), list.end());

    ptr = std::make_shared<Value>(Array(std::move(list)));
}

Parser<Json> json_parser();

} // namespace Nson
