#[cfg(test)]
mod tests {
    use crate::DATE_FORMAT_TIMEZONE;
    use crate::compiler::{compile, run};
    use crate::errors::CompilerError::{IllegalArgumentsException, ReservedFunctionName};
    use crate::errors::CompilerErrorAtLine;
    use crate::errors::RuntimeError::{IllegalArgumentException, IndexOutOfBounds};
    use crate::errors::TipiLangError::{Compiler, Runtime};
    use crate::value::{Value, string};
    use chrono::DateTime;

    #[test]
    fn literal_int() {
        assert_eq!(run("1"), Ok(Value::I64(1)));
    }

    #[test]
    fn literal_float() {
        assert_eq!(run("2.1"), Ok(Value::F64(2.1)));
    }

    #[test]
    fn literal_float_scientific() {
        assert_eq!(run("2.1e5"), Ok(Value::F64(2.1e5)));
    }

    #[test]
    fn literal_string() {
        assert_eq!(run(r#""a""#), Ok(string("a")));
    }

    #[test]
    fn literal_list() {
        assert_eq!(
            run(r#"["abc","def"]"#),
            Ok(Value::List(vec![string("abc"), string("def")]))
        );
    }

    #[test]
    fn index_in_list_literal() {
        assert_eq!(run(r#"["abc","def"][1]"#), Ok(string("def")))
    }

    #[test]
    fn index_in_list_as_var() {
        assert_eq!(
            run(r#"let a:list = ["abc","def"]
a[1]"#),
            Ok(string("def"))
        )
    }

    #[test]
    fn infer_type() {
        assert_eq!(
            run(r#"let a=1
a"#),
            Ok(Value::I64(1))
        );
    }

    #[test]
    fn define_u32() {
        assert_eq!(
            run(r#"let a:u32=1
a"#),
            Ok(Value::U32(1))
        );
    }

    #[test]
    fn define_char() {
        assert_eq!(
            run(r#"let a:char='a'
a"#),
            Ok(Value::Char('a'))
        );
    }

    #[test]
    fn define_u32_invalid_value_negative() {
        let r = compile("let a:u32=-1");
        assert!(r.is_err());
        if let Err(e) = &r {
            assert_eq!(
                e.to_string(),
                "Compilation failed: error at line 1, Expected u32, found i32/64"
            );
        }
    }

    #[test]
    fn define_u64_invalid_value_negative() {
        let r = compile("let a:u64=-1");
        assert!(r.is_err());
        if let Err(e) = &r {
            assert_eq!(
                e.to_string(),
                "Compilation failed: error at line 1, Expected u64, found i32/64"
            );
        }
    }

    #[test]
    fn let_u64_invalid_value_string() {
        let r = compile(r#"let a:u64="not ok""#);
        assert!(r.is_err());
        if let Err(e) = &r {
            assert_eq!(
                e.to_string(),
                "Compilation failed: error at line 1, Expected u64, found string"
            );
        }
    }

    #[test]
    fn call_fn_with_args_returns_value() {
        assert_eq!(
            run(r#"fn add_hello(name: string) -> string:
    "Hello " + name
add_hello("world")"#),
            Ok(string("Hello world"))
        );
    }

    #[test]
    fn define_object() {
        let r = compile(
            r#"
object Person:
   name: string"#,
        );
        assert!(r.is_ok()); // does nothing runtime
    }

    #[test]
    fn declare_and_instantiate_object() {
        let r = run(r#"
object Person:
   name: string

let p = Person(name: "Sander")
p"#);
        assert!(r.is_ok());
        assert_eq!(
            r#"Person: [("name", String("Sander"))]"#,
            format!("{}", r.unwrap().to_string())
        );
    }

    #[test]
    fn declare_and_instantiate_object_wrong_type() {
        let r = run(r#"
object Person:
   name: string

let p = Person(name: 0x42)
p"#);
        // assert!(r.is_err());
        assert_eq!(
            r#"Compilation failed: error at line 5, Expected string, found integer"#,
            format!("{}", r.unwrap_err().to_string())
        );
    }

    #[test]
    fn literal_map() {
        let result = run(r#"{"name": "Dent", "age": 40 }"#);
        assert!(result.is_ok());
        let result = result.unwrap();
        if let Value::Map(map) = result {
            assert_eq!(map.get(&string("name")).unwrap(), &string("Dent"));
            assert_eq!(map.get(&string("age")).unwrap(), &Value::I64(40));
        }
    }

    #[test]
    fn assign_map() {
        let result = run(r#"let m = {"name": "Dent"}
m"#);

        let result = result.unwrap();
        if let Value::Map(map) = result {
            assert_eq!(map.get(&string("name")).unwrap(), &string("Dent"));
        }
    }

    #[test]
    fn access_map() {
        let result = run(r#"let m = {"name": "Dent"}
m["name"]"#);

        let result = result.unwrap();
        if let Value::String(v) = result {
            assert_eq!(v.as_str(), "Dent");
        }
    }

    #[test]
    fn keyword_error() {
        let result = run(r#"let map = {"name": "Dent"}"#);
        assert!(result.is_err());
        assert_eq!(
            "Compilation failed: error at line 1, 'map' is a keyword. You cannot use it as an identifier",
            result.unwrap_err().to_string()
        );
    }

    #[test]
    fn add_strings() {
        assert_eq!(run(r#""a"+"b""#), Ok(string("ab")));
    }

    #[test]
    fn add_string_and_int() {
        assert_eq!(run(r#""a"+42"#), Ok(string("a42")));
    }

    #[test]
    fn add_string_and_bool() {
        assert_eq!(run(r#""a"+false"#), Ok(string("afalse")));
    }

    #[test]
    fn add_string_and_scientific_float() {
        assert_eq!(
            run(r#""a"+4.2e10"#),
            Ok(Value::String("a42000000000".into()))
        );
    }

    #[test]
    fn add_hex_ints() {
        assert_eq!(run(r#"0x10 + 0x20"#), Ok(Value::U32(48)));
    }

    #[test]
    fn date_literal() {
        assert_eq!(
            run(r#"let date:datetime = d"2025-11-09 16:44:28.000 +0100"
date"#),
            Ok(Value::DateTime(Box::new(
                DateTime::parse_from_str("2025-11-09 16:44:28.000 +0100", DATE_FORMAT_TIMEZONE)
                    .unwrap()
                    .into()
            )))
        );
    }

    #[test]
    fn string_reverse() {
        assert_eq!(run(r#""abc".reverse()"#), Ok(string("cba")));
    }

    #[test]
    fn string_to_upper() {
        assert_eq!(run(r#""abc".to_uppercase()"#), Ok(string("ABC")));
    }

    #[test]
    fn string_len() {
        assert_eq!(run(r#""abc".len()"#), Ok(Value::U64(3)));
    }

    #[test]
    fn string_replace() {
        assert_eq!(run(r#""Hello".replace_all("l","p")"#), Ok(string("Heppo")));
    }

    #[test]
    fn string_replace_wrong_nr_of_args() {
        assert_eq!(
            run(r#""Hello".replace_all("l")"#),
            Err(Compiler(CompilerErrorAtLine {
                error: IllegalArgumentsException("string.replace_all".to_string(), 2, 1),
                line: 1
            }))
        );
    }

    #[test]
    fn string_replace_wrong_type_of_args() {
        assert_eq!(
            run(r#""Hello".replace_all("l", 1)"#),
            Err(Runtime(IllegalArgumentException(
                "Illegal replacement. Expected a string but got 1".to_string()
            )))
        );
    }

    #[test]
    fn string_contains() {
        assert_eq!(run(r#""Hello".contains("l")"#), Ok(Value::Bool(true)));
    }

    #[test]
    fn list_length() {
        assert_eq!(run(r#"[1,2,3].len()"#), Ok(Value::U64(3)));
    }

    #[test]
    fn list_push() {
        assert_eq!(
            run(r#"[1,2].push(3)"#),
            Ok(Value::List(vec![
                Value::I64(1),
                Value::I64(2),
                Value::I64(3)
            ]))
        );
    }

    #[test]
    fn list_remove() {
        assert_eq!(
            run(r#"[1,2,3].remove(0)"#),
            Ok(Value::List(vec![Value::I64(2), Value::I64(3)]))
        );
    }

    #[test]
    fn list_remove_out_of_bounds() {
        assert_eq!(
            run(r#"[1,2,3].remove(4)"#),
            Err(Runtime(IndexOutOfBounds(4, 3)))
        );
    }

    #[test]
    fn reassign() {
        assert_eq!(
            run(r#"let a=1
a=2"#),
            Ok(Value::Void)
        );
    }

    #[test]
    fn simple_if_expr() {
        assert_eq!(
            run(r#"
let a = 2
let b = 3
if true:
  a
else:
  b
"#),
            Ok(Value::I64(2))
        );
    }

    #[test]
    fn if_expr() {
        assert_eq!(
            run(r#"
if 1==1:
  2
"#),
            Ok(Value::I64(2))
        );
    }

    #[test]
    fn if_var_expr() {
        assert_eq!(
            run(r#"
let a=1
if a==1:
  2
"#),
            Ok(Value::I64(2))
        );
    }

    #[test]
    fn if_var_expr_else() {
        assert_eq!(
            run(r#"
let a=1
if a==2:
  1
else:
  2
"#),
            Ok(Value::I64(2))
        );
    }

    #[test]
    fn inline_comment() {
        assert_eq!(run(r#"// this is a comment"#), Ok(Value::Void));
    }

    #[test]
    fn range_loop() {
        assert_eq!(
            run(r#"
let sum=0
for a in 1..4:
    sum = sum + a
sum
"#),
            Ok(Value::I64(10))
        );
    }

    #[test]
    fn global_function_call() {
        let value = run(r#"now()"#);
        assert!(value.is_ok());
        let value = value.unwrap();
        let date_time_string = value.to_string();
        assert!(DateTime::parse_from_str(&date_time_string, DATE_FORMAT_TIMEZONE).is_ok());
    }

    #[test]
    fn global_fns_are_not_allowed() {
        let value = run(r#"fn now():"#);
        assert_eq!(
            value,
            Err(Compiler(CompilerErrorAtLine {
                error: ReservedFunctionName("now".to_string()),
                line: 1
            }))
        );
    }

    #[test]
    fn test_if_expression_else() {
        run(r#"
let a:i64 = if true:
    42
else:
    0
a"#).unwrap();
    }
    // #[test]
    // fn package() {
    //     assert_eq!(run(r#"a.b.c()"#), Ok(Value::U32(48)));
    // }

    // #[test]
    // fn guards() {
    //     assert_eq!(
    //         run(r#"fn get_all_users() -> list:
    // | /{uuid} -> service.get_by_uuid(uuid)?"#),
    //         Ok(Value::Void)
    //     );
    // }
}
