# List available recipes
default:
  @just --list --justfile {{justfile()}}

validate: test format lint

lint:
  cargo clippy --all-targets -- -D warnings

format:
  cargo fmt

# Run all tests
test:
  cargo test


# Update fixtures
update-fixtures:
  UPDATE_FIXTURES=1 cargo test
  just test

# Run tests but keep keep cannonicalized svg files
keep-cannonicalized:
  KEEP_CANNONICALIZED_SVG=1 cargo test

# Run coverage
coverage:
  cargo llvm-cov --workspace --all-targets
