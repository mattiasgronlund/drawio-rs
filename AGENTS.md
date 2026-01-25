# AGENTS

This repository uses Codex-driven tasks. When working on tasks, follow these rules.

## Project goals

- Implement a Rust-native parser/renderer for `.drawio` files with an incremental, test-driven approach.
- Prefer correctness, determinism, and good error messages over cleverness.
- Avoid adding heavy dependencies unless necessary.

## Repo structure (high level)

- `crates/drawio-core`: parsing + internal model + tests (golden JSON fixtures)
- Other crates may build on `drawio-core` later.

## Development workflow

- Use the devcontainer when possible.
- Run these before providing a patch:
  - `cargo fmt`
  - `cargo clippy --all-targets -- -D warnings`
  - `cargo test -p drawio-core`

## Testing conventions

- Golden tests live under `crates/drawio-core/tests/`.
- Fixture inputs: `fixtures/corpus/**.drawio`
- Expected outputs:
  - `fixtures/expected/**.json`
  - `fixtures/expected/**.svg`
- Updating fixtures is not allowed
- It is not allowed to update any file under: fixtures

## PR conventions

- Keep PRs small and focused (one activity / milestone).
- Update or add fixtures whenever behavior changes intentionally.
- Include a short “How tested” section in the PR description with exact commands.

## Coding conventions

- Rust 2024 edition
- Prefer `thiserror` for error types.
- Avoid panics in library code.
