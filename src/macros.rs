#[cfg(feature = "benchmark")]
macro_rules! instrument {
    ($action:expr) => {
        crate::benchmark::dispatch($action)
    };
}

#[cfg(not(feature = "benchmark"))]
macro_rules! instrument {
    ($action:expr) => {};
}

#[cfg(feature = "benchmark")]
macro_rules! instrument_timed {
    ($action:ident, $block:expr) => {{
        let __t = std::time::Instant::now();
        let __r = $block;
        crate::benchmark::$action.fetch_add(__t.elapsed().as_micros() as u64,
            std::sync::atomic::Ordering::Relaxed);
        __r
    }};
}

#[cfg(not(feature = "benchmark"))]
macro_rules! instrument_timed {
    ($action:ident, $block:expr) => { $block };
}

#[cfg(feature = "benchmark")]
macro_rules! instrument_vm_mutation {
    ($action:ident, $block:expr) => {{
        let __t = std::time::Instant::now();
        let __r = $block;
        crate::benchmark::$action.fetch_add(__t.elapsed().as_micros() as u64,
            std::sync::atomic::Ordering::Relaxed);
        __r
    }};
}

pub(crate) use instrument;
pub(crate) use instrument_timed;
