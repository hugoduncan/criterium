# Munera Plan

Project task backlog. Open tasks live under `open/NNN-slug/`, completed
tasks move to `closed/`.

## Open

- `002-escape-opaque-cells` — make each opaque input cell escape once at
  setup so HotSpot escape analysis cannot fold the measured expression; fixes
  the task-001 barrier, which is defeated on a default JVM (design authority
  D-5 / I-4 / C-004).
