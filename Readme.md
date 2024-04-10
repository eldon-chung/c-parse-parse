# C-Parse-Parse
C-Parse-Parse is a parser combinator library. This means you can use it to either create basic parsers, or take two parsers, and combine them to create another parser. Hence the name Parse-Parse (and it's written in C++). For now the library only creates parsers for string types. It's a future goal to make this parse any iterator type.

#### The Parser Type
The parser type `Parser<T>` is templated on type `T`. And a parser `p` of type `Parser<T>` is a callable on strings (`std::string` or `std::string_view`). And, `p(string)` returns a value of type `ParseResult<T>`. For now, think of this as saying that a `Parser<T>` is a parser function that takes in as input a string, and outputs something of type `T`.

#### Basic Parsers
The library provides parsers that match either chars or strings.

```CPP
Parser<char> char_parser(char ch);
Parser<std::string_view> str_parser(std::string_view expect);
```

So for example: `char_parser('b')` is a parser that matches the first input of its character with `b` and returns `b` as a result (if the first character of the input is indeed `b`).  Similarly, `str_parser("blabla")` is a parser that matches strings that start with `"blabla"` as a prefix. Okay that's fine and all, but that's perhaps not too interesting. What we can do is take some of these basic parsers, and create bigger ones. You can think of this as incrementally building bigger and bigger parsers.
#### Some Example Combinations
We can think about how in general, if we had a `Parser<A>` and a `Parser<B>`, we might want to run the first parser, followed by the second one for some input. As a really simple example, perhaps we wanted to parse the following kinds of variable declarations:

```C++
int i;
char c;
bool b;
```

And we can grammatically think of this as `declaration := type var_name semicolon`. Assuming we had three parsers `Parser<Type>`, `Parser<VarName>`, and `Parser<Semicolon>`, then we can mimic the grammar by creating a new parser in the following way:

```C++
Parser<Type> type = /* assume it's magically defined */;
Parser<VarName> var_name = /* assume it's magically defined */;
Parser<char> semicolon = char_parser(':');

using Declaration = std::pair< std::pair<Type,VarName>, char>;

Parser<Declaration> declaration = type * var_name * semicolon;
```

So the `*` is a combinator that takes two parsers `Parser<A>`, `Parser<B>` and outputs a `Parser<std::pair<A, B>>` whose behaviour is defined as running the first parser, followed by the second parser, and outputting the pair that contains their results. For the remainder of this section, we'll be listing out all the library functions (and operations).

### Library functions:

#### Binary operators on Parsers
```C++
Parser<A> parser_a; // parses term A
Parser<B> parser_b; // parsers term B

auto a_then_b = parser_a * parser_b; // parses term A B ("A then B")
auto a_or_b = parser_a + parser_b; // parses term A | B ("A or B")
			// This means we fall back to parser_b only if parser_a fails.

auto a_then_a_take_b = parser_a > parser_b; 
auto a_then_a_take_a = parser_a < parser_b;
```

The first two might be intuitive, the second pair of combinators might not. Let's revisit the type declaration example. Notice how the return type of the parser is  `std::pair< std::pair<Type,VarName>, char>;`. We've included the `char` semicolon as part of the parse result. But what we probably want is to just make sure that a semicolon exists, without having to return it. That's where the `<` operator would come in. We can do this instead:
```C++
using Declaration = std::pair<Type,VarName>;

Parser<Declaration> declaration = type * var_name < semicolon;
Parser<Declaration> declaration = (type * var_name) < semicolon; // equivalent
```

It helps that in C++, the relational operators `<` and `>` have lower precedence than the multiplication and addition operators. So what the parser will now do instead is parse a type, followed by a variable name, and then it'll go onto parse a semicolon character. But it will only return the result of the "left" parser, which happens to be the pair that contains the type and the variable name. We have a myriad of other combinators too. We'll just list them out there and briefly mention their behaviour.

#### Variable argument combinators
```C++
// Equivalent to p1 + p2 + ... + pn;
template <typename... Parsers> auto alt(Parsers... parsers)
alt(p1, p2, ..., pn); // example usage

// Conceptually equivalent to p1 * p2 * ... * pn;
// But returns std::tuple<A, B, ..., N> rather than std::pair<A, std::pair<...>>
template <typename... Parsers> auto seq(Parsers... parsers);
seq(p1, p2, ..., pn); // example usage

// Similar to alt(p1, p2, ..., pn); but for when all parsers
// return the same type;
template <typename... Parsers> auto any_of_parser(Parsers... parsers);
any_of_parser(p1, p2, ..., pn); // example usage
```
#### Zero, 1, or more repetitions of parsers
```C++
// Parses at least zero or more occurences of parser
template <typename T> auto many(Parser<T> const &parser);

// Parses at least one or more occurences of parser
template <typename T> auto some(Parser<T> const &parser);

// Parses at exactly count occurences of parser
template <typename T> auto repeat(Parser<T> const &parser, size_t count);

// Returns success_value if parser succeeds, else default_value
template <typename T, typename F>
	auto maybe(Parser<T> const &parser, F const &success_value, F const &default_value)

// Parses at least zero occurences of p, delimited by separator
template <typename T, typename U>
Parser<std::vector<T>> seperated_by(Parser<T> const &p, Parser<U> const &separator)
```

#### Maps, and Lifts
We have basic map, and map2:
```C++
// (Overload 1) Runs parser, then applies mapper_func onto the result of the 
// parsed value if it succeeds
template <typename T, typename F> Parser<std::result_of_t<F(T)>>
	map(F mapper_func, Parser<T> const &parser);

// (Overload 2) Runs parser 1 and parser 2, then applies prefix_op onto the 
// result of the parsed values if they succeed.
template <typename F, typename T1, typename T2> Parser<std::invoke_result_t<F, T1, T2>>
map(F prefix_op, Parser<T1> const &parser1, Parser<T2> const &parser2)

// example usage of overload 1:
// parser that parses a digit char and returns it as an integer
auto into_int_closure = [](char c) -> int { return c - '0'; };
auto digit = map(into_int_closure, any_of_char_parser("0123456789")); 

// operator overloaded <<= version:
auto digit = any_of_char_parser("0123456789") <<= into_int_closure; 

// example usage of overload 2:
// parser that parses two characters, converts them to numbers
// and adds them together
auto digit_adder = map( std::plus(), digit, digit);
```

We can also lift functions to turn them into maps:
```C++
template <typename F> auto lift(F lift_func);
template <typename F> auto lift2(F mapper_func);

// example usage of lift2:
auto plus_map = lift2(std::plus());

auto digit_adder = plus_map(digit, digit); // digit is type Parser<int>
auto str_concat = plus_map(str1, str2); // str1, str2 are type Parser<std::string>
```
#### Combination Parsers
```C++
// Parses left, middle, right, then returns the left and right
template <typename M, typename L, typename R>  Parser<std::pair<L, R>>
	surrounding(Parser<M> const &middle, Parser<L> const &left, Parser<R> const &right);

// Parses left, middle, right, then returns the middle
template <typename T, typename L, typename R> Parser<T>
	between(Parser<L> const &left, Parser<R> const &right, Parser<T> const &middle);
```

#### Other Basic Parsers
```C++
// Does not parse anything, just returns value t
template <typename T> Parser<T> pure_parser(T t);

// Matches any char that is in list_of_char
Parser<char> any_of_char_parser(std::string_view list_of_char);

// Matches any char that is not in the string except
Parser<char> any_char_except(std::string_view except);

// Given a pointer to a parser, dereferences that pointer during parse time
//     and uses it to parse the text
// Note: You need this for when your grammar is recursive.
template <typename T> Parser<T> ref_parser(Parser<T> const *parser);

// Matches eof
Parser<NullParseType> eof_parser();
```

### Accessing Parse Results
The parse results are given in a struct of type `Parse<T>`. To get your result, you can call `get_result()`. You can also use `has_result()` to check if the parser succeeded and there was a result. Lastly, you can access the member `.remaining` to see what remains to be parsed after the parser has run.

You can also use the static method `print_result()` in the `Printer` class to print what the result was. This is especially helpful for nested result types. Just make sure that if you're parsing for your own type `T`, you specialize the templated `<<` operator for  `std::ostream` for your type `T` so that it prints properly. So now it only prints to `std::cout`, in the future we will support the `<<` operator to any stream.

```C++
// Example usage
Parser<int> number_parser = /* defined accordingly */;
ParseResult<int> number_result = number_parser("1231");

// Prints out that the parser suceeded, and parsed object being 1231
Printer::print_result(number_result); 

Parser<bool> str_to_bool_parser = /* defined accordingly */;
ParseResult<bool> bool_result1 = number_parser("false");

// Prints out that the parser suceeded, and parsed object being false
Printer::print_result(bool_result1);

// Prints out that the parser failed, and that the remaining string is "blabla"
Printer::print_result(number_parser("blabla"); 

/* User defined type T */
friend std::ostream &operator<<(std::ostream &os, T const &t) { ... };
// ^ User also needs to define this for type T in order for Printer::print_result to work.

Parser<T> some_parser = /* defined accordinglly */
ParseResultT<T> some_result = some_parser(some_string);
Printer::print_result(number_result); // Prints t to std::cout

```

## Example 1: A Json Parser
Okay! Let's try building a Json parser with this library as a tool. The Json grammar defines terms (and also rules for) `whitespace`, `number`, `string`, `value`, `array`, `object`. 

Let's look at the first of the interesting examples: `array`. The rules for an array are basically:
```EBNF
array = "[" (items | whitespace) "]";
items = value { "," value }; /(* value followed by 0 or more "," value *)
```

We can read `items` as essentially saying it's `value` but separated by the comma symbols `,`.
So the corresponding parser for `array` is written like this:
```C++
auto skip_ws = skip_many(whitespace);
Parser<Json> json_array = between(char_parser('[') < skip_ws, skip_ws > char_parser(']'),
		seperated_by(ref_parser(&json_value), comma)) <<= json_array_into_json_type;
```

Let's break it down. A `json_array`, is defined as the stuff in between `[` and `]` , which is 0 or more occurrences of a `json_value` , delimited by `comma`.

What about `object`? The rules for an object are quite similar:
```EBNF
object = "{" (items | whitespace) "}";
items = pair { "," pair }; /(* pair followed by 0 or more "," pair *)
pair = whitespace string whitespace ":" value;
```

So similarly, here's how we would write the parser for `json_object` using the library:
```C++
auto skip_ws = skip_many(whitespace);
Parser<std::pair<Json, Json>> object_pair =
	between(skip_ws, skip_ws,
			surrounding(colon, string_parser(), ref_parser(&json_value)));

Parser<Json> json_object =
	between(char_parser('{') < skip_ws, skip_ws > char_parser('}'),
			seperated_by(object_pair, comma) <<= pair_list_to_object);
```

So an `object_pair` is a parser that parses and returns a `string` and `json_value` pair that surrounds a `colon` (which may be surrounded by `whitespace`.  

Likewise, `json_object` is a parser that returns a list of `object_pair`s that are in between `{` and `}`.

Let's just look at one more example, `value`:
```EBNF
value = whitespace 
	( string | number | object | array | "true" | "false" | "null" ) 
	whitespace;
```

So `value` is essentially the stuff between `whitespace` and it's any of those cases. Well in our code, we would simply write:

```C++
json_value = between(skip_ws, skip_ws,
		any_of_parser(json_object, string_parser(), number_parser(), literal_parser(), json_array)
	     );
```

Pretty simple, right?
## Example 2: A Simply Typed Lamba Calculus Parser

How about parsing a simple grammar for STLC?  Let's go with the following grammar (I think I left out some whitespace skipping, but it'll just make it super verbose if I included all of them):
```EBNF
term = type ":" (varname | application | lambda)
varname = string
app = whitespace "app" whitespace "(" term ")" "(" term ")"
lambda = "\\" var term
type = ( type ) arrow type | (type) | base_type arrow type | base_type
arrow = whitespace type whitespace "->" whitespace type whitespace
base_types = whitespace ("int" | "char" | "bool") whitespace
whitespace = { " " } (* zero or more occurences of " " *)
```

Let's start from the top this time. A term is a `type`, and either a `varname`, `application` or `lambda` that surround a `:`. So in our code, we can write this:

```C++
term = any_of_parser(surrounding(colon, type, lambda) <<= into_term,
		 	surrounding(colon, type, application) <<= into_term,
		 	surrounding(colon, type, varname) <<= into_term);
```

Likewise, an `app` (for application) is basically an `"app"` keyword followed by two terms, each of which are surrounded by enclosing `(` and `)`. 

```C++
static Parser<Term::app_t> application = between(skip_ws, skip_ws, str_parser("app")) >
	between(l_paren, r_paren, ref_parser(&term)) * between(l_paren, r_paren, ref_parser(&term));
```

Honestly, building the rest of the parser in pieces like this is pretty much going to look super similar. And I don't want to bore you, so I'll just show one last snippet that I think is pretty interesting. Notice how `type` sometimes refers to itself? For example, a `type` can be defined as `( type )` in the second case for its rule in the grammar. How do we do that in C++? We can't define a value with respect to itself, that's just circular reasoning. But conceptually you can see how it would make sense that we could have to do that for recursive rules. So in C++, the idea is that we don't actually need the parser itself, we just need a _reference_ (or pointer) to the parser. And that's exactly how we use `ref_parser()` to make this happen!

```C++
Parser<std::shared_ptr<TermType>> type;
Parser<std::shared_ptr<TermType>> type_case_2 = between(l_paren, r_paren, ref_parser(&type));
// Definitions for other type_cases here
...
type = any_of_parser(type_case_1, type_case_2, type_case_3, type_case_4);
```

And this is exactly how you make it happen. You might wonder what happens if we had instead done:
```C++
Parser<std::shared_ptr<TermType>> type_case_2 = between(l_paren, r_paren, type);
```

This actually has to do more with the internals of the library itself but basically it would have defined `type_case_2` to be the `type` that is between `l_paren` and `r_paren`. Which looks good so far, until I tell you that as of this point, since  `type` was just some empty parser, creating `type_case_2` in this manner holds a copy of this version of `type`. Subsequently when we define `type` as any of the 4 cases, that's a different copy than what `type_case_2` has. So it would not be able to "recurse", so to speak. Instead, if we gave it a pointer to `type`, then we are free to alter `type` however we choose, and eventually `type_case_2` would just have to dereference that pointer to get to the original `type` instead. 

Anyway, the rest of the code in  has to do with the logic on how to even typecheck in the first place, which isn't the concern of the parser, but rather just typechecking stuff. I did have to bake into the parser the logic on how to create a representation of an STLC expression that also make it typecheckeable, but that probably belongs in a different document.
