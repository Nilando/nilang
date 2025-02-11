use nilang::{Driver, Config};
use std::thread;

fn get_test_name() -> String {
    thread::current()
        .name()
        .unwrap()
        .to_string()
}

fn test_ast_helper(program: &str) {
    let project_path = std::env!("CARGO_MANIFEST_DIR");
    let test_name = get_test_name();
    let expected_file = format!("{}/tests/fixtures/ast/{}.json", project_path, test_name);
    let output_file = format!("{}/target/test_outputs/{}.json", project_path, test_name);
    let args = vec!["nlang", "-a", &output_file, "-i", program, "--dry-run"];
    let config = Config::try_from(args).unwrap();
    let driver = Driver::new(config);

    driver.run();

    let expected_content =
        std::fs::read_to_string(&expected_file).expect("Failed to read expected file");
    let generated_content =
        std::fs::read_to_string(&output_file).expect("Failed to read output file");

    assert_eq!(
        generated_content, expected_content,
        "Generated AST does not match fixture"
    );
}

#[test]
fn test_simple_ast() {
    test_ast_helper("a = 1 + 1;");
}

#[test]
fn test_fn_declaration() {
    test_ast_helper("a = fn { return @ + @; };");
}
