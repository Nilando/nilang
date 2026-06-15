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
pub static MAJOR_COLLECTIONS:       AtomicU64 = AtomicU64::new(0);
pub static MINOR_COLLECTIONS:       AtomicU64 = AtomicU64::new(0);
pub static MAJOR_COLLECT_AVG_NS:    AtomicU64 = AtomicU64::new(0);
pub static MINOR_COLLECT_AVG_NS:    AtomicU64 = AtomicU64::new(0);
pub static ARENA_PEAK_BYTES:        AtomicU64 = AtomicU64::new(0);
pub static ARENA_SIZE_SUM_BYTES:    AtomicU64 = AtomicU64::new(0);
pub static ARENA_SIZE_SAMPLES:      AtomicU64 = AtomicU64::new(0);
pub static OLD_OBJECTS_PEAK:        AtomicU64 = AtomicU64::new(0);
pub static OLD_OBJECTS_SUM:         AtomicU64 = AtomicU64::new(0);
pub static GC_PAUSE_COUNT:          AtomicU64 = AtomicU64::new(0);
pub static GC_PAUSE_TOTAL_NS:       AtomicU64 = AtomicU64::new(0);
pub static GC_PAUSE_MAX_NS:         AtomicU64 = AtomicU64::new(0);
pub static PARSE_TIME_US:           AtomicU64 = AtomicU64::new(0);
pub static OPTIMIZE_TIME_US:        AtomicU64 = AtomicU64::new(0);
pub static LOWER_TIME_US:           AtomicU64 = AtomicU64::new(0);
pub static VM_LOAD_TIME_US:         AtomicU64 = AtomicU64::new(0);
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
    AddIoTimeUs(u64),
    RecordGcPauseNs(u64),

    // peak updates (CAS loop)
    UpdatePeakHeap(u64),
    UpdatePeakStackDepth(u64),

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
        Action::AddIoTimeUs(n)              => { IO_TIME_US.fetch_add(n, Relaxed); }
        Action::RecordGcPauseNs(n)          => {
            GC_PAUSE_COUNT.fetch_add(1, Relaxed);
            GC_PAUSE_TOTAL_NS.fetch_add(n, Relaxed);
            update_peak(&GC_PAUSE_MAX_NS, n);
        }

        Action::UpdatePeakHeap(n)           => update_peak(&PEAK_HEAP_BYTES, n),
        Action::UpdatePeakStackDepth(n)     => update_peak(&STACK_DEPTH_PEAK, n),

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

    let _ = std::fs::create_dir_all("benches/reports");
    let output_path = format!("benches/reports/{}.txt", basename);

    let parse_ms    = PARSE_TIME_US.load(Relaxed) as f64 / 1000.0;
    let optimize_ms = OPTIMIZE_TIME_US.load(Relaxed) as f64 / 1000.0;
    let lower_ms    = LOWER_TIME_US.load(Relaxed) as f64 / 1000.0;
    let codegen_ms  = CODEGEN_TIME_US.load(Relaxed) as f64 / 1000.0;
    let compile_ms  = parse_ms + lower_ms + optimize_ms + codegen_ms;

    let vm_load_time_ms = VM_LOAD_TIME_US.load(Relaxed) as f64 / 1000.0;
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
    let gc_pause_count    = GC_PAUSE_COUNT.load(Relaxed);
    let gc_pause_total_ns = GC_PAUSE_TOTAL_NS.load(Relaxed);
    let gc_pause_max_ns   = GC_PAUSE_MAX_NS.load(Relaxed);
    let gc_pause_total_ms = gc_pause_total_ns as f64 / 1_000_000.0;
    let gc_pause_max_ms   = gc_pause_max_ns as f64 / 1_000_000.0;
    let gc_pause_avg_ms   = if gc_pause_count > 0 {
        gc_pause_total_ns as f64 / gc_pause_count as f64 / 1_000_000.0
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


    let compilation_report = format!(
"\
COMPILATION
\tparse:          {parse_ms} ({parse_pct} of compile)
\tlower:          {lowering_ms} ({lowering_pct} of compile)
\toptimize:       {optimize_ms} ({optimize_pct} of compile)
\tcodegen:        {codegen_ms} ({codegen_pct} of compile)
\ttotal:          {total_ms} ({total_pct} of wall)
",
parse_ms = format_ms(parse_ms),
parse_pct = format_pct(parse_ms, compile_ms),
lowering_ms = format_ms(lower_ms),
lowering_pct = format_pct(lower_ms, compile_ms),
optimize_ms = format_ms(optimize_ms),
optimize_pct = format_pct(optimize_ms, compile_ms),
codegen_ms = format_ms(codegen_ms),
codegen_pct = format_pct(codegen_ms, compile_ms),
total_ms = format_ms(compile_ms),
total_pct = format_pct(compile_ms, wall_ms),
);


    let dispatch_vm_report = format!(
"\
DISPATCH / VM
\tVM load:        {loading_ms} ({loading_pct} of *wall)
\tinstructions:   {instructions}
\tdispatch loops: {dispatch_loops}
\tdispatch time:  {dispatch_ms} ({dispatch_pct} of wall)
\tavg dispatch:   {avg_dispatch_ns} ns
\tfn calls:       {function_calls}
\tnative calls:   {native_calls}
\tstack max:      {stack_peak}
",
loading_ms = format_ms(vm_load_time_ms),
loading_pct = format_pct(vm_load_time_ms, wall_ms),
dispatch_ms = format_ms(dispatch_ms),
dispatch_pct = format_pct(dispatch_ms, wall_ms),
avg_dispatch_ns = format!("{avg_dispatch_ns:<4.1}")
);

    let gc_report = format!(
"\
GC / MEMORY
\ttotal pause:      {total_pause} ({total_pause_pct} of wall)
\tpause avg:        {pause_avg}
\tcollections:      {gc_collections} (major: {major_colls}, minor: {minor_colls})
\tarena peak:       {arena_peak}
\tarena avg:        {arena_avg}
\tgc/mem samples:   {arena_samples}
\told obj peak:     {old_peak}
\told obj avg:      {old_avg}
",
total_pause = format_ms(gc_pause_total_ms),
total_pause_pct = format_pct(gc_pause_total_ms, wall_ms),
arena_peak = format_bytes(arena_peak),
arena_avg = format_bytes(arena_avg_bytes),
pause_avg = format_ms(gc_pause_avg_ms),
);

    let totals_report = format!(
"\
TOTALS
\twall time:        {wall_time}
",
wall_time = format_ms(wall_ms),
);

    let report = format!("{compilation_report}{dispatch_vm_report}{gc_report}{totals_report}");
    let _ = std::fs::write(&output_path, report);
}

fn format_bytes(bytes: u64) -> String {
    if bytes < 1000 {
        format!("{bytes:<4.1} bytes")
    } else if bytes < 1000_u64.pow(2) {
        let kb = bytes as f64 / 1000.0_f64.powi(1);

        format!("{kb:<4.1} kb")
    } else if bytes < 1000_u64.pow(3) {
        let mb = bytes as f64 / 1000.0_f64.powi(2);

        format!("{mb:<4.1} mb")
    } else if bytes < 1000_u64.pow(4) {
        let gb = bytes as f64 / 1000.0_f64.powi(3);

        format!("{gb:<4.1} gb")
    } else {
        panic!("bad byte count while formatting benchmark report")
    }
}

fn format_pct(part: f64, total: f64) -> String {
    let pct = (part / total) * 100.0;

    format!("%{pct:<4.1}")
}

fn format_ms(ms: f64) -> String {
    if ms >= 1000.0 {
        let secs = ms / 1000.0;
        format!("{secs:<4.1} s ")
    } else {
        format!("{ms:<4.1} ms")
    }
}
