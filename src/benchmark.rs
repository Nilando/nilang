use std::sync::atomic::{AtomicPtr, AtomicU64, Ordering::Relaxed};
use std::collections::HashMap;
use std::sync::{Mutex, OnceLock};
use std::time::Instant;

pub static SANDPIT_METRICS_PTR: AtomicPtr<sandpit::Metrics> =
    AtomicPtr::new(std::ptr::null_mut());

pub fn register_sandpit_metrics(metrics: &sandpit::Metrics) {
    SANDPIT_METRICS_PTR.store(metrics as *const _ as *mut _, Relaxed);
}

pub fn sample_sandpit_metrics() {
    let ptr = SANDPIT_METRICS_PTR.load(Relaxed);
    if !ptr.is_null() {
        // SAFETY: pointer is set by register_sandpit_metrics at the top of
        // Runtime::run with a reference to the live arena's metrics. The arena
        // outlives all VM execution.
        let metrics = unsafe { &*ptr };
        record_gc_metrics(metrics);
    }
}

pub static OPCODE_HISTOGRAM: Mutex<Option<HashMap<String, u64>>> = Mutex::new(None);
pub static START_INSTANT: OnceLock<Instant> = OnceLock::new();

pub static INSTRUCTIONS:            AtomicU64 = AtomicU64::new(0);
pub static DISPATCH_LOOPS:          AtomicU64 = AtomicU64::new(0);
pub static FUNCTION_CALLS:          AtomicU64 = AtomicU64::new(0);
pub static NATIVE_CALLS:            AtomicU64 = AtomicU64::new(0);
pub static STACK_DEPTH_PEAK:        AtomicU64 = AtomicU64::new(0);
pub static CURRENT_STACK_DEPTH:     AtomicU64 = AtomicU64::new(0);
pub static STACK_DEPTH_SUM:         AtomicU64 = AtomicU64::new(0);
pub static STACK_DEPTH_SAMPLES:     AtomicU64 = AtomicU64::new(0);
pub static TOTAL_ALLOCATIONS:       AtomicU64 = AtomicU64::new(0);
pub static TOTAL_ALLOCATED_BYTES:   AtomicU64 = AtomicU64::new(0);
pub static PEAK_HEAP_BYTES:         AtomicU64 = AtomicU64::new(0);
pub static DISPATCH_TIME_US:        AtomicU64 = AtomicU64::new(0);
pub static GC_COLLECTIONS:          AtomicU64 = AtomicU64::new(0);
pub static GC_TOTAL_PAUSE_US:       AtomicU64 = AtomicU64::new(0);
pub static GC_MAX_PAUSE_US:         AtomicU64 = AtomicU64::new(0);
pub static MAJOR_COLLECTIONS:       AtomicU64 = AtomicU64::new(0);
pub static MINOR_COLLECTIONS:       AtomicU64 = AtomicU64::new(0);
pub static MAJOR_COLLECT_AVG_NS:    AtomicU64 = AtomicU64::new(0);
pub static MINOR_COLLECT_AVG_NS:    AtomicU64 = AtomicU64::new(0);
pub static ARENA_PEAK_BYTES:        AtomicU64 = AtomicU64::new(0);
pub static ARENA_SIZE_SUM_BYTES:    AtomicU64 = AtomicU64::new(0);
pub static ARENA_SIZE_SAMPLES:      AtomicU64 = AtomicU64::new(0);
pub static OLD_OBJECTS_PEAK:        AtomicU64 = AtomicU64::new(0);
pub static OLD_OBJECTS_SUM:         AtomicU64 = AtomicU64::new(0);
pub static YIELD_GAP_COUNT:         AtomicU64 = AtomicU64::new(0);
pub static YIELD_GAP_TOTAL_NS:      AtomicU64 = AtomicU64::new(0);
pub static YIELD_GAP_MAX_NS:        AtomicU64 = AtomicU64::new(0);
pub static PARSE_TIME_US:           AtomicU64 = AtomicU64::new(0);
pub static OPTIMIZE_TIME_US:        AtomicU64 = AtomicU64::new(0);
pub static CODEGEN_TIME_US:         AtomicU64 = AtomicU64::new(0);
pub static SYSCALL_COUNT:           AtomicU64 = AtomicU64::new(0);
pub static IO_TIME_US:              AtomicU64 = AtomicU64::new(0);
pub static WALL_TIME_US:            AtomicU64 = AtomicU64::new(0);

pub enum Action<'a> {
    // start the wall clock
    Start,

    // simple increments
    IncrementInstructions,
    IncrementDispatchLoops,
    IncrementGcCollections,
    IncrementSyscalls,
    IncrementAllocations,
    IncrementFunctionCalls,
    IncrementNativeCalls,
    IncrementStackDepth,
    DecrementStackDepth,

    // increments by a value
    AddAllocatedBytes(u64),
    AddGcPauseUs(u64),
    AddIoTimeUs(u64),
    RecordYieldGapNs(u64),

    // peak updates (CAS loop)
    UpdatePeakHeap(u64),
    UpdatePeakStackDepth(u64),
    UpdateMaxGcPause(u64),

    // timed blocks
    TimeParse(&'a dyn Fn()),
    TimeCodegen(&'a dyn Fn()),

    // opcode histogram
    RecordOpcode(&'a str),

    // write benchmark report json
    WriteReport(&'a str),
}

pub fn record_gc_metrics(metrics: &sandpit::Metrics) {
    MAJOR_COLLECTIONS.store(metrics.major_collections.load(Relaxed), Relaxed);
    MINOR_COLLECTIONS.store(metrics.minor_collections.load(Relaxed), Relaxed);
    MAJOR_COLLECT_AVG_NS.store(metrics.major_collect_avg_time.load(Relaxed), Relaxed);
    MINOR_COLLECT_AVG_NS.store(metrics.minor_collect_avg_time.load(Relaxed), Relaxed);

    let size = metrics.arena_size.load(Relaxed);
    update_peak(&ARENA_PEAK_BYTES, size);
    ARENA_SIZE_SUM_BYTES.fetch_add(size, Relaxed);

    let old = metrics.old_objects_count.load(Relaxed);
    update_peak(&OLD_OBJECTS_PEAK, old);
    OLD_OBJECTS_SUM.fetch_add(old, Relaxed);

    ARENA_SIZE_SAMPLES.fetch_add(1, Relaxed);
}

pub fn update_peak(stat: &AtomicU64, val: u64) {
    let mut current = stat.load(Relaxed);
    while val > current {
        match stat.compare_exchange_weak(current, val, Relaxed, Relaxed) {
            Ok(_) => break,
            Err(actual) => current = actual,
        }
    }
}

pub fn dispatch(action: Action) {
    match action {
        Action::Start                       => { let _ = START_INSTANT.set(Instant::now()); }

        Action::IncrementInstructions       => { INSTRUCTIONS.fetch_add(1, Relaxed); }
        Action::IncrementDispatchLoops      => { DISPATCH_LOOPS.fetch_add(1, Relaxed); }
        Action::IncrementGcCollections      => { GC_COLLECTIONS.fetch_add(1, Relaxed); }
        Action::IncrementSyscalls           => { SYSCALL_COUNT.fetch_add(1, Relaxed); }
        Action::IncrementAllocations        => { TOTAL_ALLOCATIONS.fetch_add(1, Relaxed); }
        Action::IncrementFunctionCalls      => { FUNCTION_CALLS.fetch_add(1, Relaxed); }
        Action::IncrementNativeCalls        => { NATIVE_CALLS.fetch_add(1, Relaxed); }
        Action::IncrementStackDepth         => {
            let depth = CURRENT_STACK_DEPTH.fetch_add(1, Relaxed) + 1;
            update_peak(&STACK_DEPTH_PEAK, depth);
            STACK_DEPTH_SUM.fetch_add(depth, Relaxed);
            STACK_DEPTH_SAMPLES.fetch_add(1, Relaxed);
        }
        Action::DecrementStackDepth         => { CURRENT_STACK_DEPTH.fetch_sub(1, Relaxed); }

        Action::AddAllocatedBytes(n)        => { TOTAL_ALLOCATED_BYTES.fetch_add(n, Relaxed); }
        Action::AddGcPauseUs(n)             => { GC_TOTAL_PAUSE_US.fetch_add(n, Relaxed); }
        Action::AddIoTimeUs(n)              => { IO_TIME_US.fetch_add(n, Relaxed); }
        Action::RecordYieldGapNs(n)         => {
            YIELD_GAP_COUNT.fetch_add(1, Relaxed);
            YIELD_GAP_TOTAL_NS.fetch_add(n, Relaxed);
            update_peak(&YIELD_GAP_MAX_NS, n);
        }

        Action::UpdatePeakHeap(n)           => update_peak(&PEAK_HEAP_BYTES, n),
        Action::UpdatePeakStackDepth(n)     => update_peak(&STACK_DEPTH_PEAK, n),
        Action::UpdateMaxGcPause(n)         => update_peak(&GC_MAX_PAUSE_US, n),

        Action::TimeParse(f)                => {
            let t = std::time::Instant::now();
            f();
            PARSE_TIME_US.fetch_add(t.elapsed().as_micros() as u64, Relaxed);
        }
        Action::TimeCodegen(f)              => {
            let t = std::time::Instant::now();
            f();
            CODEGEN_TIME_US.fetch_add(t.elapsed().as_micros() as u64, Relaxed);
        }

        Action::RecordOpcode(op)            => {
            // mutex only taken here
            crate::benchmark::OPCODE_HISTOGRAM
                .lock().unwrap()
                .get_or_insert_with(Default::default)
                .entry(op.to_string())
                .and_modify(|c| *c += 1)
                .or_insert(1);
        }

        Action::WriteReport(path)           => write_report(path),
    }
}

fn write_report(path: &str) {
    let basename = std::path::Path::new(path)
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("bench");

    let _ = std::fs::create_dir_all("target/bench");
    let output_path = format!("target/bench/{}.json", basename);

    let parse_ms    = PARSE_TIME_US.load(Relaxed) as f64 / 1000.0;
    let optimize_ms = OPTIMIZE_TIME_US.load(Relaxed) as f64 / 1000.0;
    let codegen_ms  = CODEGEN_TIME_US.load(Relaxed) as f64 / 1000.0;
    let compile_ms  = parse_ms + optimize_ms + codegen_ms;

    let instructions    = INSTRUCTIONS.load(Relaxed);
    let dispatch_loops  = DISPATCH_LOOPS.load(Relaxed);
    let stack_peak      = STACK_DEPTH_PEAK.load(Relaxed);
    let stack_samples   = STACK_DEPTH_SAMPLES.load(Relaxed);
    let stack_avg       = if stack_samples > 0 {
        STACK_DEPTH_SUM.load(Relaxed) as f64 / stack_samples as f64
    } else { 0.0 };
    let peak_heap       = PEAK_HEAP_BYTES.load(Relaxed);
    let total_allocs    = TOTAL_ALLOCATIONS.load(Relaxed);
    let total_alloc_b   = TOTAL_ALLOCATED_BYTES.load(Relaxed);
    let dispatch_us     = DISPATCH_TIME_US.load(Relaxed);
    let dispatch_ms     = dispatch_us as f64 / 1000.0;
    let function_calls  = FUNCTION_CALLS.load(Relaxed);
    let native_calls    = NATIVE_CALLS.load(Relaxed);
    let gc_total_us     = GC_TOTAL_PAUSE_US.load(Relaxed);
    let gc_max_us       = GC_MAX_PAUSE_US.load(Relaxed);
    let major_colls     = MAJOR_COLLECTIONS.load(Relaxed);
    let minor_colls     = MINOR_COLLECTIONS.load(Relaxed);
    let gc_collections  = major_colls + minor_colls;
    let major_avg_ms    = MAJOR_COLLECT_AVG_NS.load(Relaxed) as f64 / 1_000_000.0;
    let minor_avg_ms    = MINOR_COLLECT_AVG_NS.load(Relaxed) as f64 / 1_000_000.0;
    let arena_peak      = ARENA_PEAK_BYTES.load(Relaxed);
    let arena_samples   = ARENA_SIZE_SAMPLES.load(Relaxed);
    let arena_avg_bytes = if arena_samples > 0 {
        ARENA_SIZE_SUM_BYTES.load(Relaxed) / arena_samples
    } else { 0 };
    let old_peak        = OLD_OBJECTS_PEAK.load(Relaxed);
    let old_avg         = if arena_samples > 0 {
        OLD_OBJECTS_SUM.load(Relaxed) / arena_samples
    } else { 0 };
    let yield_count     = YIELD_GAP_COUNT.load(Relaxed);
    let yield_total_ns  = YIELD_GAP_TOTAL_NS.load(Relaxed);
    let yield_max_ns    = YIELD_GAP_MAX_NS.load(Relaxed);
    let yield_total_ms  = yield_total_ns as f64 / 1_000_000.0;
    let yield_max_ms    = yield_max_ns as f64 / 1_000_000.0;
    let yield_avg_ms    = if yield_count > 0 {
        yield_total_ns as f64 / yield_count as f64 / 1_000_000.0
    } else { 0.0 };
    let syscalls        = SYSCALL_COUNT.load(Relaxed);
    let io_us           = IO_TIME_US.load(Relaxed);

    let wall_us = START_INSTANT.get()
        .map(|t| t.elapsed().as_micros() as u64)
        .unwrap_or(0);
    let wall_ms = wall_us as f64 / 1000.0;

    let avg_dispatch_ns = if instructions > 0 {
        (dispatch_us as f64 * 1000.0) / instructions as f64
    } else { 0.0 };

    let avg_gc_pause_ms = if gc_collections > 0 {
        (gc_total_us as f64 / gc_collections as f64) / 1000.0
    } else { 0.0 };
    let max_gc_pause_ms = gc_max_us as f64 / 1000.0;
    let gc_time_pct = if wall_us > 0 {
        gc_total_us as f64 / wall_us as f64 * 100.0
    } else { 0.0 };

    let alloc_rate = if wall_us > 0 {
        (total_alloc_b as f64 * 1_000_000.0 / wall_us as f64) as u64
    } else { 0 };

    let mut histogram_entries: Vec<String> = Vec::new();
    if let Some(map) = OPCODE_HISTOGRAM.lock().unwrap().as_ref() {
        for (op, count) in map {
            let escaped = op.replace('\\', "\\\\").replace('"', "\\\"");
            histogram_entries.push(format!("\"{}\":{}", escaped, count));
        }
    }
    let histogram_json = format!("{{{}}}", histogram_entries.join(","));

    let json = format!(
        "{{\
\"lex_ms\":0.0,\"parse_ms\":{parse:?},\"analysis_ms\":0.0,\
\"optimize_ms\":{optimize:?},\"codegen_ms\":{codegen:?},\"compile_ms\":{compile:?},\
\"instructions\":{instructions},\"dispatch_loops\":{dispatch_loops},\
\"dispatch_time_ms\":{dispatch_ms:?},\"avg_dispatch_ns\":{avg_dispatch_ns:?},\
\"function_calls\":{function_calls},\"native_calls\":{native_calls},\
\"stack_depth_avg\":{stack_avg:?},\"stack_depth_peak\":{stack_peak},\
\"inline_cache_hits\":0,\"inline_cache_misses\":0,\
\"peak_heap_bytes\":{peak_heap},\"total_allocated_bytes\":{total_alloc_b},\
\"total_allocations\":{total_allocs},\
\"allocation_rate_bytes_per_sec\":{alloc_rate},\
\"gc_collections\":{gc_collections},\
\"major_collections\":{major_colls},\"minor_collections\":{minor_colls},\
\"major_collect_avg_ms\":{major_avg_ms:?},\"minor_collect_avg_ms\":{minor_avg_ms:?},\
\"arena_peak_bytes\":{arena_peak},\"arena_avg_bytes\":{arena_avg_bytes},\
\"old_objects_peak\":{old_peak},\"old_objects_avg\":{old_avg},\
\"gc_snapshots\":{arena_samples},\
\"avg_gc_pause_ms\":{avg_gc_pause_ms:?},\"max_gc_pause_ms\":{max_gc_pause_ms:?},\
\"gc_time_pct\":{gc_time_pct:?},\"avg_arena_bytes\":0,\"heap_growth_rate\":0,\
\"yield_gap_count\":{yield_count},\"yield_total_ms\":{yield_total_ms:?},\
\"yield_avg_ms\":{yield_avg_ms:?},\"yield_max_ms\":{yield_max_ms:?},\
\"syscall_count\":{syscalls},\"io_time_ms\":{io_ms:?},\
\"wall_time_ms\":{wall_ms:?},\"cpu_time_ms\":0.0,\
\"user_time_ms\":0.0,\"system_time_ms\":0.0,\
\"opcode_histogram\":{histogram_json}\
}}",
        parse = parse_ms,
        optimize = optimize_ms,
        codegen = codegen_ms,
        compile = compile_ms,
        dispatch_ms = dispatch_ms,
        io_ms = io_us as f64 / 1000.0,
    );

    let _ = std::fs::write(&output_path, json);
}
