#!/usr/bin/env ruby

require 'json'

EXECUTABLE = "./target/release/nilang"
SCRIPTS_DIR = "benches/scripts"

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

# Step 4: print results
puts "\n" + "=" * 60

results.each do |r|
  d = r[:data]
  puts "\n#{File.basename(r[:script])}"
  puts "-" * 40

  unless d
    puts "  (no benchmark data)"
    next
  end

  puts "  COMPILATION"
  puts "    parse:          #{humanize_time(d['parse_ms']).ljust(12)} (#{pct(d['parse_ms'], d['compile_ms'])} of compile)"
  puts "    optimize:       #{humanize_time(d['optimize_ms']).ljust(12)} (#{pct(d['optimize_ms'], d['compile_ms'])} of compile)"
  puts "    codegen:        #{humanize_time(d['codegen_ms']).ljust(12)} (#{pct(d['codegen_ms'], d['compile_ms'])} of compile)"
  puts "    total:          #{humanize_time(d['compile_ms']).ljust(12)} (#{pct(d['compile_ms'], d['wall_time_ms'])} of wall)"

  puts "  DISPATCH / VM"
  puts "    instructions:   #{d['instructions']}"
  puts "    dispatch loops: #{d['dispatch_loops']}"
  puts "    dispatch time:  #{humanize_time(d['dispatch_time_ms']).ljust(12)} (#{pct(d['dispatch_time_ms'], d['wall_time_ms'])} of wall)"
  puts "    avg dispatch:   #{humanize_time(d['avg_dispatch_ns'], :ns)}"
  puts "    fn calls:       #{d['function_calls']}"
  puts "    native calls:   #{d['native_calls']}"
  puts "    yield gaps:     #{d['yield_gap_count']}"
  puts "    total yield:    #{humanize_time(d['yield_total_ms']).ljust(12)} (#{pct(d['yield_total_ms'], d['wall_time_ms'])} of wall)"
  puts "    avg yield:      #{humanize_time(d['yield_avg_ms'])}"
  puts "    max yield:      #{humanize_time(d['yield_max_ms'])}"
  puts "    stack avg/peak: #{d['stack_depth_avg'].round(1)} / #{d['stack_depth_peak']}"
  if d['inline_cache_hits'] + d['inline_cache_misses'] > 0
    hit_rate = d['inline_cache_hits'].to_f / (d['inline_cache_hits'] + d['inline_cache_misses']) * 100
    puts "    cache hit rate: #{hit_rate.round(1)}%"
  end

  puts "  MEMORY"
  puts "    peak heap:      #{humanize_bytes(d['peak_heap_bytes'])}"
  puts "    total alloc:    #{humanize_bytes(d['total_allocated_bytes'])}"
  puts "    num allocs:     #{d['total_allocations']}"
  puts "    alloc rate:     #{humanize_bytes(d['allocation_rate_bytes_per_sec'])}/s"

  puts "  GC"
  puts "    collections:    #{d['gc_collections']} (major: #{d['major_collections']}, minor: #{d['minor_collections']})"
  puts "    arena peak:     #{humanize_bytes(d['arena_peak_bytes'])}"
  puts "    arena avg:      #{humanize_bytes(d['arena_avg_bytes'])}"
  puts "    old obj peak:   #{d['old_objects_peak']}"
  puts "    old obj avg:    #{d['old_objects_avg']}"
  puts "    snapshots:      #{d['gc_snapshots']}"

  puts "  I/O"
  puts "    syscalls:       #{d['syscall_count']}"
  puts "    io time:        #{humanize_time(d['io_time_ms'])}"

  puts "  RUNTIME"
  puts "    wall:           #{humanize_time(d['wall_time_ms'])}"
  puts "    cpu:            #{humanize_time(d['cpu_time_ms'])}"
  puts "    user:           #{humanize_time(d['user_time_ms'])}"
  puts "    system:         #{humanize_time(d['system_time_ms'])}"

  if d['opcode_histogram'] && !d['opcode_histogram'].empty?
    puts "  OPCODE HISTOGRAM (top 10)"
    d['opcode_histogram']
      .sort_by { |_, count| -count }
      .first(10)
      .each { |op, count| puts "    #{op.ljust(20)} #{count}" }
  end
end
