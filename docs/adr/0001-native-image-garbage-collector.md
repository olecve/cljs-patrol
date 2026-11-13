# ADR 0001: Build The Linux Native Binary With The G1 Garbage Collector

## Status

Rejected, 2026-11-12 (proposed 2026-11-12)

## Outcome

2026-11-12. The Linux measurement did not confirm G1. Keep the Serial GC on all platforms. The sections after this one
show the proposal as it was before the measurement.

Measurement conditions:

- A Linux ARM64 container, with Oracle GraalVM 21.0.12, on an Apple Silicon laptop.
- Two native binaries from the same uberjar (v0.0.54). One binary uses the `build.clj` flags (Serial GC). The other
  binary uses the same flags and `--gc=G1`.
- The same large codebase as the first test. The command is
  `--only a11y,tests,react-testing-library-cljs,reagent --baseline --quiet-baseline`.
- Each row is the median of 3 runs. Serial GC and G1 runs alternate in the same container.

| CPUs   | `--jobs` | Time, Serial GC | Time, G1 | G1 / Serial GC | Peak memory, Serial GC | Peak memory, G1 |
| ------ | -------- | --------------- | -------- | -------------- | ---------------------- | --------------- |
| 1 core | 1        | 19.98 s         | 22.65 s  | 1.13           | 285 MB                 | 934 MB          |
| 1 core | 2        | 19.86 s         | 26.80 s  | 1.35           | 260 MB                 | 1049 MB         |
| 2 core | 2        | 15.63 s         | 15.65 s  | 1.00           | 228 MB                 | 1066 MB         |
| 4 core | 4        | 10.35 s         | 10.58 s  | 1.02           | 278 MB                 | 1291 MB         |

- G1 is slower on 1 core, and not faster on 2 or 4 cores. G1 has shorter pauses, but its concurrent threads use more CPU
  time: 31.0 s against 24.2 s at 4 cores.
- G1 uses 3.3x to 4.7x more peak memory.
- The G1 binary is 22% larger: 35.4 MB against 29.0 MB.
- On Linux, the Serial GC used only 5-7% of the run at `--jobs` 1 and 2, and 14-15% at `--jobs` 4. This is less than the
  first test on macOS showed.
- All runs gave the same output.

The CI runners had fewer cores than the Context below expects. A private-repository runner showed `nproc` 2, but `lscpu`
showed 1 core with 2 threads. On that runner, the gate command took 34.9 s and 34.2 s at `--jobs 1`, and 32.5 s and 31.9
s at `--jobs 2`. Thus parallel analysis gives little on such runners, and a GC that collects on more threads gives less.

The Linux container has no hyperthreads, so 1 core in the container is not the same as 1 core with 2 threads. G1 lost or
tied with 1 core and with 2 cores, so a measurement on x86-64 is not necessary for this decision.

## Context

cljs-patrol analyzes files in parallel since merge commit `cd79eeb551`. Before that commit, one thread parsed and walked
all files, one file after the other. Now a fixed pool of worker threads analyzes the files, and each worker reads one
file at a time. The results merge in file order, so the output is the same for all thread counts. `--jobs` sets the
number of worker threads, from 1 to 256. The default is the number of processors.

The release builds two native binaries, `linux-x86_64` and `macos-aarch64`. It uses Oracle GraalVM 21
(`distribution: 'graalvm'` in `.github/workflows/release.yml`) and the flags in the `native` task of `build.clj`. These
flags do not set a garbage collector (GC). Thus both binaries use the native-image default, the Serial GC. The Serial GC
collects on one thread, and all worker threads stop while it collects.

A first test of the macOS binary on a large codebase showed this cost. GC used 13-16% of the run at `--jobs 2`, and
25-29% at `--jobs 4`. The part increases with `--jobs`, because more threads allocate memory while one thread collects.
Different young generation sizes for the Serial GC did not decrease this cost.

CI pipelines usually download the Linux binary and run it on runners with 2 or 4 vCPUs.

## Decision

Build the `linux-x86_64` native binary with `--gc=G1`, if a measurement on Linux confirms that G1 is faster. Keep the
`macos-aarch64` binary on the Serial GC.

[Options](#options) compares this choice with the other options.

Less memory allocation per run helps all GCs and all `--jobs` values. It is a different task, not an alternative to this
decision.

## Options

### Keep the Serial GC on all platforms (rejected)

- Good, because it keeps one GC on all platforms.
- Good, because it needs no build change.
- Bad, because the Serial GC stops all threads and collects on one thread. This limits the gain from parallel analysis,
  and the limit is larger on machines with more cores.

### Tune the Serial GC (rejected)

Rejected because different young generation sizes did not decrease the GC cost in the first test.

### Use G1 on Linux (chosen)

- Good, because G1 collects on more than one thread. It also does part of its work while the program runs.
- Good, because Oracle GraalVM supports G1 in native images on Linux only, and CI pipelines run on Linux.
- Good, because the change is one build flag.
- Bad, because G1 can also have costs, for example a larger binary or more memory.
- Bad, because nobody measured G1 yet.

### Use G1 on all platforms (rejected)

Rejected because native-image supports G1 on Linux only.

## Consequences

- The `native` task in `build.clj` needs a way to add `--gc=G1` for the Linux build only. `native-verify` in
  `.github/workflows/test.yml` must build the binary the same way, so that it checks the binary that the release ships.
- The two release binaries use different GCs. A memory or time problem can occur on one binary only.
- The build needs Oracle GraalVM. GraalVM Community Edition has no G1 for native images.

## Confirmation

Before the status changes to `Accepted`, measure G1 on Linux. Compare a Serial GC build and a G1 build of the same
commit on the same project at `--jobs` 1, 2 and 4. Record the time, the binary size and the peak memory.

## Revisit when

Propose G1 again only if one of these conditions occurs:

- CI runners get 4 or more physical cores, and runs with all groups become the slow part of CI. In the measurement, G1
  was 5-14% faster for all groups on 2 and 4 cores, from 1 run for each configuration.
- A new GraalVM release changes the CPU or memory cost of G1 in native images.

## More Information

- Parallel analysis: merge commit `cd79eeb551`, pull request https://github.com/olecve/cljs-patrol/pull/138
