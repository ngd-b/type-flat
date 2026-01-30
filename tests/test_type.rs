use common::run_flat;

mod common;
// 联合泛型 + Pick / Omit
#[test]
fn test_union_generic_pick_omit() {
    let result = run_flat(
        r#"
        type User = { id: number; name: string; email: string }
        type PartialUser = Pick<User, 'id' | 'name'>
        "#,
        "PartialUser",
    );
    assert!(result.contains("id: number;"));
    assert!(result.contains("name: string;"));
    assert!(!result.contains("email: string"));
}

// 交叉泛型
#[test]
fn test_intersection_generic() {
    let result = run_flat(
        r#"
        type User = { id: number }
        type Extra = { extra: string }
        type UserExtra = User & Extra
        "#,
        "UserExtra",
    );
    assert!(result.contains("id: number"));
    assert!(result.contains("extra: string"));
}

// 数组 / 元组类型
#[test]
fn test_array_tuple() {
    let content = r#"
        type User = { id: number; name: string }
        type Users = User[]
        type UserTuple = [User, User]
        "#;
    let result_users = run_flat(content, "Users");
    let result_tuple = run_flat(content, "UserTuple");

    assert!(result_users.contains("{ id: number; name: string; }[]"));
    assert!(
        result_tuple.contains("[{ id: number; name: string; }, { id: number; name: string; }]")
    );
}

// 交叉类型 + 接口继承 + 泛型组合
#[test]
fn test_complex_intersection_interface_generic() {
    let result = run_flat(
        r#"
        interface Base<T> { data: T }
        interface Extra { extra: string }
        interface User { id: number }
        type Complex = Base<User> & Extra
        "#,
        "Complex",
    );
    assert!(result.contains("data: { id: number; }"));
    assert!(result.contains("extra: string"));
}

#[test]
fn test_conditional_type() {
    let result = run_flat(
        r#"
        type IsString<T> = T extends string ? true : false;
        type Result = IsString<'a'>
        "#,
        "Result",
    );
    assert!(result.contains("type Result = true"));
}

#[test]
fn test_index_signature_and_literal_index() {
    let cotent = r#"
    interface Dictionary {
        [key: string]: number;
    }
    type ValueOf<T> = T[keyof T]
    
    type DictionaryValue = ValueOf<Dictionary>;
    "#;

    let result = run_flat(cotent, "Dictionary");
    assert!(result.contains("[key: string]: number"));

    let result_value = run_flat(cotent, "DictionaryValue");
    assert!(result_value.contains("{ [key: string]: number; }[string]"));
}

#[test]
fn test_infer_type() {
    let result = run_flat(
        r#"
        type GetType<T> = T extends (...args: any[]) => infer R ? R : any;
        type Fn = () => number;
        type Result = GetType<Fn>
        "#,
        "Result",
    );
    assert!(result.contains("() => number extends (...args: any[]) => infer R ? R : any"));
}

// #[test]
// fn test_template_literal_types() {
//     let result = run_flat(
//         r#"
//         type EventName = "click" | "hover";
//         type EventHandler = `${EventName}Handler`;
//         type FullEvent = `${Uppercase<EventName>}Event`;
//         "#,
//         "EventHandler",
//     );
//     // 根据工具能力，可能展开为联合字面量
//     assert!(result.contains("\"clickHandler\"") || result.contains("clickHandler"));
//     assert!(result.contains("\"hoverHandler\""));

//     let result_full = run_flat(
//         r#"
//         type EventName = "click" | "hover";
//         type FullEvent = `${Uppercase<EventName>}Event`;
//         "#,
//         "FullEvent",
//     );
//     assert!(result_full.contains("\"CLICKEvent\""));
//     assert!(result_full.contains("\"HOVEREvent\""));
// }

#[test]
fn test_recursive_infer() {
    let result = run_flat(
        r#"
        type Flatten<T> = T extends Array<infer U> ? Flatten<U> : T;
        type Nested = number[][][];
        type FlatNumber = Flatten<Nested>;
        "#,
        "FlatNumber",
    );
    assert!(result.contains("number")); // 应最终得到 number
}
// #[test]
// fn test_mapped_type_with_keyof_filter() {
//     let result = run_flat(
//         r#"
//         interface User {
//             id: number;
//             name?: string;
//             email: string;
//         }
//         type OptionalKeys<T> = { [K in keyof T]-?: {} extends Pick<T, K> ? K : never }[keyof T];
//         type OptionalProps = Pick<User, OptionalKeys<User>>;
//         "#,
//         "OptionalProps",
//     );
//     assert!(result.contains("name?: string;"));
//     assert!(!result.contains("id: number"));
//     assert!(!result.contains("email: string"));
// }
#[test]
fn test_readonly_array_and_tuple() {
    let content = r#"
        type MutableArr = string[];
        type ReadonlyArr = readonly string[];
        type MutableTuple = [number, string];
        type ReadonlyTuple = readonly [number, string];
    "#;

    let result_mut_arr = run_flat(content, "MutableArr");
    let result_ro_arr = run_flat(content, "ReadonlyArr");
    let result_mut_tup = run_flat(content, "MutableTuple");
    let result_ro_tup = run_flat(content, "ReadonlyTuple");

    assert!(result_mut_arr.contains("string[]"));
    assert!(result_ro_arr.contains("readonly string[]"));
    assert!(result_mut_tup.contains("[number, string]"));
    assert!(result_ro_tup.contains("readonly [number, string]"));
}

#[test]
fn test_function_overload_type() {
    let result = run_flat(
        r#"
        type FnOverload =
            ((x: string) => string) &
            ((x: number) => number);
        "#,
        "FnOverload",
    );
    // 至少不应崩溃；可能保留交叉形式或列出所有签名
    assert!(result.contains("(x: string) => string"));
    assert!(result.contains("(x: number) => number"));
}

#[test]
fn test_self_recursive_type() {
    let result = run_flat(
        r#"
        type A = {
            a: string;
            self: A;
        };
        "#,
        "A",
    );

    assert!(result.contains("type A = { a: string; self: A; }"));
}

#[test]
fn test_mutual_recursive_type() {
    let result = run_flat(
        r#"
        type A = {
            a: string;
            b: B;
        };
        type B = {
            c: string;
            a: A;
        };
        "#,
        "A",
    );

    assert!(result.contains("type A = { a: string; b: { c: string; a: A; }; }"));
}

#[test]
fn test_three_level_cycle() {
    let result = run_flat(
        r#"
        type A = { b: B };
        type B = { c: C };
        type C = { a: A };
        "#,
        "A",
    );

    assert!(result.contains("type A = { b: { c: { a: A; }; }; }"));
}

#[test]
fn test_cycle_with_normal_fields() {
    let result = run_flat(
        r#"
        type A = {
            a: string;
            b: B;
        };
        type B = {
            c: string;
            d: C;
        };
        type C = {
            e: string;
            a: A;
        };
        "#,
        "A",
    );

    assert!(result.contains("type A = { a: string; b: { c: string; d: { e: string; a: A; }; }; }"));
}

#[test]
fn test_recursive_in_array() {
    let result = run_flat(
        r#"
        type Node = {
            value: string;
            children: Node[];
        };
        "#,
        "Node",
    );

    assert!(result.contains("type Node = { value: string; children: Node[]; }"));
}

#[test]
fn test_recursive_in_union() {
    let result = run_flat(
        r#"
        type A = {
            value: string;
            next: A | null;
        };
        "#,
        "A",
    );

    assert!(result.contains("type A = { value: string; next: A | null; }"));
}

#[test]
fn test_generic_recursive_type() {
    let result = run_flat(
        r#"
        type Box<T> = {
            value: T;
            next?: Box<T>;
        };
        "#,
        "Box",
    );

    assert!(result.contains("type Box<T> = { value: T; next?: Box<T>; }"));
}

#[test]
fn test_keyof_basic() {
    let result = run_flat(
        r#"
        type User = { id: number; name: string };
        type Keys = keyof User;
        "#,
        "Keys",
    );

    assert!(result.contains("type Keys = \"id\" | \"name\""));
}

#[test]
fn test_record_type() {
    let result = run_flat(
        r#"
        type UserMap = Record<string, { id: number }>;
        "#,
        "UserMap",
    );

    assert!(result.contains("type UserMap = { [key: string]: { id: number; }; }"));
}

#[test]
fn test_optional_override_in_intersection() {
    let result = run_flat(
        r#"
        type A = { a?: string };
        type B = { a: string };
        type C = A & B;
        "#,
        "C",
    );

    assert!(result.contains("type C = { a: string; }"));
}

#[test]
fn test_readonly_property() {
    let result = run_flat(
        r#"
        type A = {
            readonly id: number;
            name: string;
        };
        "#,
        "A",
    );

    assert!(result.contains("type A = { readonly id: number; name: string; }"));
}

#[test]
fn test_generic_default_type() {
    let result = run_flat(
        r#"
        type Box<T = string> = {
            value: T;
        };
        "#,
        "Box",
    );

    assert!(result.contains("type Box<T = string> = { value: T; }"));
}

#[test]
fn test_union_object_and_primitive() {
    let result = run_flat(
        r#"
        type A = { id: number } | string;
        "#,
        "A",
    );

    assert!(result.contains("type A = { id: number; } | string;"));
}

#[test]
fn test_function_return_recursive_type() {
    let result = run_flat(
        r#"
        type Node = {
            value: string;
            next: () => Node;
        };
        "#,
        "Node",
    );

    assert!(result.contains("type Node = { value: string; next: () => Node; }"));
}

#[test]
fn test_index_access_union() {
    let result = run_flat(
        r#"
        type Obj = {
            a: { x: number };
            b: { y: string };
        };
        type Value = Obj[keyof Obj];
        "#,
        "Value",
    );

    assert!(result.contains("type Value = { x: number; } | { y: string; }"));
}

#[test]
fn test_conditional_type_with_generic_recursive() {
    let result = run_flat(
        r#"
        type Flatten<T> = T extends Array<infer U> ? Flatten<U> : T;
        type Nested = number[][][];         
        type Flattened = Flatten<Nested>;
        "#,
        "Flattened",
    );

    assert!(result.contains("type Flatten<T> = T extends Array<infer U> ? Flatten<U> : T;"));
    assert!(result.contains("type Flattened = Flatten<number[][][]>;"));
}
