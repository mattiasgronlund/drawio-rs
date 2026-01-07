# List available recipes
default:
  @just --list --justfile {{justfile()}}

# Run all tests
test:
  cargo test


# Update fixtures
update-fixtures:
  UPDATE_FIXTURES=1 cargo test

# Run coverage
coverage:
  cargo llvm-cov --workspace --all-targets
