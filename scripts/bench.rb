#!/usr/bin/env ruby

require 'json'
require 'fileutils'

EXECUTABLE = "./target/release/nilang"
SCRIPTS_DIR = "benches/scripts"
BASELINES_DIR = "benches/baselines"

def humanize_bytes(n)
  return "0 B" if n.nil? || n == 0
  units = %w[B KB MB GB]
  exp = (Math.log(n) / Math.log(1024)).floor.clamp(0, units.length - 1)
  "#{(n.to_f / 1024**exp).round(2)} #{units[exp]}"
end

def humanize_time(value, unit = :ms)
  return "0 ns" if value.nil? || value == 0
  ns = case unit
       when :ns then value.to_f
       when :us then value.to_f * 1_000
       when :ms then value.to_f * 1_000_000
       when :s  then value.to_f * 1_000_000_000
       else raise ArgumentError, "unknown time unit: #{unit}"
       end
  if ns < 1_000
    "#{ns.round(2)} ns"
  elsif ns < 1_000_000
    "#{(ns / 1_000.0).round(2)} µs"
  elsif ns < 1_000_000_000
    "#{(ns / 1_000_000.0).round(2)} ms"
  else
    "#{(ns / 1_000_000_000.0).round(2)} s"
  end
end

def pct(part, whole)
  return "  -  " if whole.nil? || whole == 0
  "%5.1f%%" % (part.to_f / whole * 100)
end

def format_report(script_path, d)
  out = String.new
  out << "#{File.basename(script_path)}\n"
  out << ("-" * 40) << "\n"

  unless d
    out << "  (no benchmark data)\n"
    return out
  end

  out << "  COMPILATION\n"
  out << "    parse:          #{humanize_time(d['parse_ms']).ljust(12)} (#{pct(d['parse_ms'], d['compile_ms'])} of compile)\n"
  out << "    optimize:       #{humanize_time(d['optimize_ms']).ljust(12)} (#{pct(d['optimize_ms'], d['compile_ms'])} of compile)\n"
  out << "    codegen:        #{humanize_time(d['codegen_ms']).ljust(12)} (#{pct(d['codegen_ms'], d['compile_ms'])} of compile)\n"
  out << "    total:          #{humanize_time(d['compile_ms']).ljust(12)} (#{pct(d['compile_ms'], d['wall_time_ms'])} of wall)\n"

  out << "  DISPATCH / VM\n"
  out << "    instructions:   #{d['instructions']}\n"
  out << "    dispatch loops: #{d['dispatch_loops']}\n"
  out << "    dispatch time:  #{humanize_time(d['dispatch_time_ms']).ljust(12)} (#{pct(d['dispatch_time_ms'], d['wall_time_ms'])} of wall)\n"
  out << "    avg dispatch:   #{humanize_time(d['avg_dispatch_ns'], :ns)}\n"
  out << "    fn calls:       #{d['function_calls']}\n"
  out << "    native calls:   #{d['native_calls']}\n"
  out << "    stack avg/peak: #{d['stack_depth_avg'].round(1)} / #{d['stack_depth_peak']}\n"

  out << "  GC\n"
  out << "    collections:    #{d['gc_collections']} (major: #{d['major_collections']}, minor: #{d['minor_collections']})\n"
  out << "    major avg:      #{humanize_time(d['major_collect_avg_ms'])}\n"
  out << "    minor avg:      #{humanize_time(d['minor_collect_avg_ms'])}\n"
  out << "    pause count:    #{d['gc_pause_count']}\n"
  out << "    pause total:    #{humanize_time(d['gc_pause_total_ms']).ljust(12)} (#{pct(d['gc_pause_total_ms'], d['wall_time_ms'])} of wall)\n"
  out << "    pause avg:      #{humanize_time(d['gc_pause_avg_ms'])}\n"
  out << "    pause max:      #{humanize_time(d['gc_pause_max_ms'])}\n"
  out << "    arena peak:     #{humanize_bytes(d['arena_peak_bytes'])}\n"
  out << "    arena avg:      #{humanize_bytes(d['arena_avg_bytes'])}\n"
  out << "    old obj peak:   #{d['old_objects_peak']}\n"
  out << "    old obj avg:    #{d['old_objects_avg']}\n"
  out << "    snapshots:      #{d['gc_snapshots']}\n"

  out << "  RUNTIME\n"
  out << "    wall:           #{humanize_time(d['wall_time_ms'])}\n"

  if d['opcode_histogram'] && !d['opcode_histogram'].empty?
    out << "  OPCODE HISTOGRAM (top 10)\n"
    d['opcode_histogram']
      .sort_by { |_, count| -count }
      .first(10)
      .each { |op, count| out << "    #{op.ljust(20)} #{count}\n" }
  end

  out
end

# Step 1: build with benchmark feature
puts "Building with benchmark feature..."
system("cargo build --release --features benchmark") or abort("Build failed")

# Step 2: collect scripts
scripts = Dir.glob("#{SCRIPTS_DIR}/**/*.nl").sort

abort("No benchmark scripts found in #{SCRIPTS_DIR}") if scripts.empty?

# Step 3: run each script and collect output
results = scripts.map do |script|
  system(EXECUTABLE, script) # runs cleanly, no output capture needed
  
  bench_file = "target/bench/#{File.basename(script, '.*')}.json"
  bench_data = File.exist?(bench_file) ? JSON.parse(File.read(bench_file)) : nil

  { script: script, data: bench_data }
end

# Step 4: print results and write baselines
FileUtils.mkdir_p(BASELINES_DIR)
puts "\n" + "=" * 60

results.each do |r|
  report = format_report(r[:script], r[:data])
  puts "\n" + report
  baseline_path = "#{BASELINES_DIR}/#{File.basename(r[:script], '.*')}.txt"
  File.write(baseline_path, report)
end

puts "\nBaselines written to #{BASELINES_DIR}/"
