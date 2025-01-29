mod generator;
mod parser;
mod vm;
mod symbol_map;
mod driver;

use driver::Driver;
fn main() {
    let driver = Driver::new();

    driver.run();
}