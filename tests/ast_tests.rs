use nlang::{Driver, Config};
use std::thread;

fn get_test_name() -> String {
    thread::current()
        .name()
        .unwrap()
        .to_string()
}

#[test]
fn test_simple_ast() {
    let project_path = std::env!("CARGO_MANIFEST_DIR");
    let test_name = get_test_name();
    let program = "a = 1 + 1;";
    let expected_file = format!("{}/tests/fixtures/{}.json", project_path, test_name);
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
