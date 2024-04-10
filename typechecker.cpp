#include "typechecker.hpp"

int main() {
    auto term = type_parser();

    auto test_case = [&term](std::string_view test_case_name,
                             std::string_view test_case) {
        std::cout << "test case - " << test_case_name << ": \"" << test_case
                  << "\"" << std::endl;
        auto parse_result = term(test_case);
        if (parse_result) {
            std::cout << std::boolalpha << "type check result: "
                      << type_check(*parse_result.get_result()) << std::endl;
        } else {
            std::cout << "Parsing failed." << std::endl;
        }
        std::cout << "====================================" << std::endl;
    };
    // now we do typecheck
    test_case("tc1", "int :   x  ");
    test_case("tc1", "int -> int : \\x int : x");
    test_case("tc2", "int -> char : \\x int : x");
    test_case("tc3", "int -> char : \\x char : x");
    test_case("tc4", "int -> int -> int : \\x int : \\y int : x");
    test_case("tc5", "int   -> char    -> int :   \\x int : \\y char : x");
    test_case("tc6", "int : \\x int : x");
    test_case("tc7", "int -> int : \\x int -> int : app (int : x)(int : x)");
    test_case("tc7b", "int -> int : \\x  int : app (int : x)(int : x)");
    test_case("tc8",
              "(int -> char) -> int -> char :"
              "\\f int -> char : \\x int : app ( int -> char : f )(int : x)");

    return 0;
}
