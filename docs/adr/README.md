# Architecture Decision Records

Each record states one decision, the options that were considered, and the condition to revisit it.

| ADR                                            | Title                                                       | Status   | Date       |
| ---------------------------------------------- | ----------------------------------------------------------- | -------- | ---------- |
| [0001](0001-native-image-garbage-collector.md) | Build The Linux Native Binary With The G1 Garbage Collector | Rejected | 2026-11-12 |
| [0002](0002-per-file-result-cache.md)          | Cache The Analysis Result Of Each File                      | Proposed | 2026-11-12 |
| [0003](0003-tell-data-vectors-from-hiccup.md)  | Tell Data Vectors From Hiccup                               | Proposed | 2026-11-13 |

## Format

Each record has these sections, in this order. The sections in parentheses are optional.

1. **Status.** One line: the status and its date, such as `Proposed, 2026-11-13`. A later status keeps the first date in
   parentheses, such as `Rejected, 2026-11-12 (proposed 2026-11-12)`.
2. **(Outcome).** A dated addendum after a measurement or a change of status. The sections after it keep the proposal as
   it was.
3. **Context.** The problem, and the facts that a reader can check.
4. **Decision.** One or two sentences that state the decision first. Then the parts of the decision.
5. **Options.** One `###` heading for each option, with its label in parentheses: chosen, rejected or deferred. An
   option with a real trade-off gets one flat list in the MADR style, with items such as `Good, because …` and
   `Bad, because …`. An option that is not possible, or that loses for one clear reason, gets one line: "Rejected
   because …".
6. **Consequences.** What changes, good and bad.
7. **Confirmation.** What must be true before the status changes to `Accepted`.
8. **(Implementation).** The steps, one pull request each.
9. **Revisit when.** The conditions that make the decision worth a new look. Each one is a fact that a reader can check.
10. **More Information.** Links to pull requests, commits, code and sources, with each link pinned to a tag or a commit.

A record names no author. It is written in Simplified Technical English. A new record that replaces an old one says so
in its Context, and the old record gets the status `Superseded by ADR NNNN`.
