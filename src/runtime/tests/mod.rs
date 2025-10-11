mod fixture_tests;

use std::fs::File;
use std::io::Write;

use pretty_assertions::assert_eq;

use crate::runtime::Config;
use crate::SymbolMap;

use super::{InterpreterError, Runtime};

fn fixture_test(test_name: &str) {
    let full_program_path = format!("./src/runtime/tests/test_programs/{}.nl", test_name);
    let fixture_folder = format!("./src/runtime/tests/fixtures/{}", test_name);

    let output_path_actual = format!("{}/{}.output.actual", fixture_folder, test_name);
    let err_path_actual = format!("{}/{}.error.actual", fixture_folder, test_name);
    let ast_path_actual = format!("{}/{}.ast.actual", fixture_folder, test_name);
    let ir_path_actual = format!("{}/{}.ir.actual", fixture_folder, test_name);
    let bytecode_path_actual = format!("{}/{}.bytecode.actual", fixture_folder, test_name);

    let output_path_expected = format!("{}/{}.output.expected", fixture_folder, test_name);
    let err_path_expected = format!("{}/{}.error.expected", fixture_folder, test_name);
    let ast_path_expected = format!("{}/{}.ast.expected", fixture_folder, test_name);
    let ir_path_expected = format!("{}/{}.ir.expected", fixture_folder, test_name);
    let bytecode_path_expected = format!("{}/{}.bytecode.expected", fixture_folder, test_name);

    let config = Config {
        __output_override: Some(output_path_actual.clone()),
        ast_output_path: Some(Some(ast_path_actual.clone())),
        ir_output_path: Some(Some(ir_path_actual.clone())),
        bytecode_output_path: Some(Some(bytecode_path_actual.clone())),
        pretty_ir: true,
        no_optimize: false,
        inline: None,
        stdin: false,
        dry_run: false,
        file: Some(full_program_path),
        script_args: vec![],
    };

    let expected_actual_pairs = vec![
        (     &err_path_actual,      &err_path_expected),
        (  &output_path_actual,   &output_path_expected),
        (     &ast_path_actual,      &ast_path_expected),
        (      &ir_path_actual,       &ir_path_expected),
        (&bytecode_path_actual, &bytecode_path_expected),
    ];

    std::fs::create_dir_all(fixture_folder).unwrap();
    File::create(&output_path_actual).unwrap();

    for (actual_path, _) in expected_actual_pairs.iter() {
        let _ = std::fs::remove_file(&actual_path);
    }

    let mut file = File::create(err_path_actual.clone()).unwrap(); // Creates or truncates the file
                                                                   //let 
    let source_path = config.get_source_path().unwrap();
    let runtime = Runtime::init(SymbolMap::new(), config);
    if let Err(err) = run_script(runtime, source_path) {
        let content = err.render();
    
        file.write_all(content.as_bytes()).unwrap();
    } else {
        file.write_all(&[] as &[u8]).unwrap();
    }



    if std::env::var("OVERWRITE_FIXTURES").is_ok() {
        for (actual_path, expected_path) in expected_actual_pairs.iter() {
            let actual = std::fs::read_to_string(&actual_path).unwrap_or(String::new());

            if let Ok(mut expected_file) = File::create(&expected_path) {
                expected_file.write_all(actual.as_bytes()).unwrap();
            }
        }
    }

    for (actual_path, expected_path) in expected_actual_pairs.iter() {
        let actual = std::fs::read_to_string(&actual_path).unwrap_or(String::new());
        let expected = std::fs::read_to_string(&expected_path).unwrap_or(String::new());

        assert_eq!(expected, actual);
        let _ = std::fs::remove_file(&actual_path);
    }
}

fn run_script(mut runtime: Runtime, path: String) -> Result<(), InterpreterError> {
    runtime.load_module(&path)?;

    runtime.run()
}
