#include "json.hpp"

#include <string_view>

void test_case(Parser<Nson::Json> const &parser, std::string_view to_parse,
               Nson::Json const &success_value) {
    std::cout << "=============================" << std::endl;
    std::cout << "Input: \"" << to_parse << "\"" << std::endl;
    ParseResult<Nson::Json> result = parser(to_parse);

    if (!result || result.get_result() != success_value) {
        std::cout << "....Expected: " << std::endl << success_value;
        if (result) {
            std::cout << " but instead got: " << std::endl
                      << result.get_result() << std::endl;
        } else {
            std::cout << " but there was a parser error. " << std::endl;
        }
    } else {
        std::cout << "....Success. Parsed value: " << std::endl
                  << result.get_result() << std::endl;
    }
}

int main() {
    using namespace Nson;
    std::cout << "number parse tests:" << std::endl;
    test_case(json_parser(), "0", Json::value(0));
    test_case(json_parser(), "1", Json::value(1));
    test_case(json_parser(), "123", Json::value(123));
    test_case(json_parser(), "-0", Json::value(-0));
    test_case(json_parser(), "-2", Json::value(-2));
    test_case(json_parser(), "-0.1", Json::value(-0.1));
    test_case(json_parser(), "-8.001", Json::value(-8.001));
    test_case(json_parser(), "1.2018", Json::value(1.2018));
    test_case(json_parser(), "2e0", Json::value(2));
    test_case(json_parser(), "2e1", Json::value(20));
    test_case(json_parser(), "2e-1", Json::value(0.2));
    test_case(json_parser(), "-2e-1", Json::value(-0.2));

    std::cout << std::endl << "string parse tests:" << std::endl;
    test_case(json_parser(), "\"\"", Json::value(""));
    test_case(json_parser(), "\"\\n\"", Json::value("\n"));
    test_case(json_parser(), "\"\\t\"", Json::value("\t"));
    test_case(json_parser(), "\"\\r\"", Json::value("\r"));
    test_case(json_parser(), "\"\\f\"", Json::value("\f"));
    test_case(json_parser(), "\"\\b\"", Json::value("\b"));
    test_case(json_parser(), "\"a\\b\"", Json::value("a\b"));
    test_case(json_parser(), "\"0\"", Json::value("0"));
    test_case(json_parser(), "\"abc\"", Json::value("abc"));
    test_case(json_parser(), "\"\\u0024\"", Json::value("$"));
    test_case(json_parser(), "\"\\u00A3\"", Json::value("£"));
    test_case(json_parser(), "\"\\u0418\"", Json::value("И"));
    test_case(json_parser(), "\"\\u0939\"", Json::value("ह"));
    test_case(json_parser(), "\"\\u20AC\"", Json::value("€"));
    test_case(json_parser(), "\"\\uD55C\"", Json::value("한"));
    test_case(json_parser(), "\"\\uD55C\"", Json::value("한"));
    test_case(
        json_parser(),
        "\"\\u004D\\u00EC\\u006E\\u0068\\u0020\\u006E\\u00F3\\u0069\\u0020\\u00"
        "74\\u0069\\u1EBF\\u006E\\u0067\\u0020\\u0056\\u0069\\u1EC7\\u0074\"",
        Json::value("Mình nói tiếng Việt"));
    test_case(json_parser(), "\"Mình nói tiếng Việt\"",
              Json::value("Mình nói tiếng Việt"));

    test_case(json_parser(), "\"true\"", Json::value("true"));
    test_case(json_parser(), "\"false\"", Json::value("false"));
    test_case(json_parser(), "\"null\"", Json::value("null"));

    test_case(json_parser(), "true", Json::value(true));
    test_case(json_parser(), "false", Json::value(false));
    test_case(json_parser(), "null", Json::value(nullptr));

    std::cout << std::endl << "array parse tests:" << std::endl;
    test_case(json_parser(), "[   ]", Json::Json::array());
    test_case(json_parser(), "[]", Json::Json::array());
    test_case(json_parser(), "[\n  ]", Json::Json::array());

    {
        auto singleton = Json::array();
        singleton.push_back(Json::number(5));
        test_case(json_parser(), "[5]", singleton);
    }

    {
        auto singleton = Json::array();
        singleton.push_back(Json::string("5"));
        test_case(json_parser(), "[ \"5\" ]", singleton);
    }

    {
        auto nested = Json::array();
        nested.push_back(Json::array());
        test_case(json_parser(), "[ [  ] ]", nested);
    }

    {
        auto mixed = Json::array();
        mixed.push_back(Json::array());
        mixed.push_back(Json::string("5"));
        mixed.push_back(Json::number(20));
        mixed.push_back(Json::object());
        test_case(json_parser(), "[ [  ] , \"5\", 20 , {} ]", mixed);
    }

    // std::cout << std::endl << "object parse tests:" << std::endl;
    // {
    //     auto empty_object = Json::object();
    //     test_case(json_parser(), "{}", empty_object);
    // }

    // {
    //     auto singleton_object = Json::object();
    //     singleton_object.insert(std::string{"a"}, Json::number(1));
    //     test_case(json_parser(), "{ \"a\" : 1 }", singleton_object);
    // }

    // {
    //     Json innermost = {1, 2};

    //     Json inner2 = Json::value({innermost});
    //     Json inner3 = Json::value({inner2});

    //     auto mixed = Json::object();
    //     mixed.insert("some nested stuff", inner3);
    //     mixed.insert(std::make_pair("a", Json::number(1)));
    //     mixed.insert("b", Json::string("something else"));
    //     mixed.insert(std::make_pair("key2", Json::array()));
    //     mixed.insert(std::make_pair("inner obj", Json::object()));
    //     std::cout << mixed << std::endl;
    // }

    // {
    //     Json object = Json::object();
    //     object.insert("some other stuff", true);

    //     Json mixed_initialiser = {"abc",      nullptr, true,   56,    false,
    //                               {1, "234"}, 26,      object, {true}};

    //     std::cout << mixed_initialiser << std::endl;
    // }

    return 0;
}
