# ADR 0001: Build The Linux Native Binary With The G1 Garbage Collector

## Status

Proposed

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

Options considered:

- **Keep the Serial GC on all platforms.** This option keeps one GC on all platforms and needs no build change. It lost
  because the Serial GC stops all threads and collects on one thread. This limits the gain from parallel analysis, and
  the limit is larger on machines with more cores.
- **Tune the Serial GC.** Different young generation sizes did not decrease the GC cost in the first test.
- **Use G1 on Linux (chosen).** G1 collects on more than one thread. It also does part of its work while the program
  runs. Oracle GraalVM supports G1 in native images on Linux only, and CI pipelines run on Linux. The change is one
  build flag. G1 can also have costs, for example a larger binary or more memory. Nobody measured G1 yet.
- **Use G1 on all platforms.** This option is not available, because native-image supports G1 on Linux only.

Less memory allocation per run helps all GCs and all `--jobs` values. It is a different task, not an alternative to this
decision.

## Consequences

- Before the status changes to `Accepted`, measure G1 on Linux. Compare a Serial GC build and a G1 build of the same
  commit on the same project at `--jobs` 1, 2 and 4. Record the time, the binary size and the peak memory.
- The `native` task in `build.clj` needs a way to add `--gc=G1` for the Linux build only. `native-verify` in
  `.github/workflows/test.yml` must build the binary the same way, so that it checks the binary that the release ships.
- The two release binaries use different GCs. A memory or time problem can occur on one binary only.
- The build needs Oracle GraalVM. GraalVM Community Edition has no G1 for native images.

## Revisit when

- The Linux measurement shows that G1 is not faster than the Serial GC. Then keep the Serial GC and write a new ADR to
  supersede this one.
- The release changes from Oracle GraalVM to GraalVM Community Edition.
- Less memory allocation per run makes the GC cost of the Serial GC small.
- native-image adds G1, or a different GC that collects on more than one thread, on macOS.

## More Information

- Parallel analysis: merge commit `cd79eeb551`, pull request https://github.com/olecve/cljs-patrol/pull/138
