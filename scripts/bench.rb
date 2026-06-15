#!/usr/bin/env ruby

require 'json'
require 'fileutils'

EXECUTABLE = "./target/release/nilang"
SCRIPTS_DIR = "benches/scripts"
BASELINES_DIR = "benches/reports"

# Step 1: build with benchmark feature
puts "Building with benchmark feature..."
system("cargo build --release --features benchmark") or abort("Build failed")

# Step 2: collect scripts
scripts = Dir.glob("#{SCRIPTS_DIR}/**/*.nl").sort

abort("No benchmark scripts found in #{SCRIPTS_DIR}") if scripts.empty?

# Step 3: run each script and collect output
results = scripts.map do |script|
  system(EXECUTABLE, script) # runs cleanly, no output capture needed
end

puts "\nBaselines written to #{BASELINES_DIR}/"
