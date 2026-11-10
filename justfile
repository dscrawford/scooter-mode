# scooter-mode task runner: `just` with no args lists recipes.

default:
    @just --list

# Evaluate the flake structure (fast; does not build derivations).
check-flake:
    nix flake show

# Fail on unformatted Go files.
check-fmt:
    test -z "$(gofmt -l .)"

# go vet plus golangci-lint's default linters.
lint: compile
    go vet ./...
    golangci-lint run ./...

# Go unit tests against a fake Scooter server, with the race detector.
test-go:
    go test -race ./...

# Byte-compile the Elisp, warnings as errors.
compile:
    emacs -Q --batch -L lisp --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile lisp/*.el
    rm -f lisp/*.elc

# Each file loads (or autoloads) every function it calls.
deps:
    ok=0; for f in lisp/*.el; do emacs -Q --batch -L lisp -l test/check-deps.el "$f" || ok=1; done; exit $ok

# ERT tests with the dev shell's pinned nixpkgs dependencies.
ert:
    emacs -Q --batch -L lisp -L test -l test/scooter-test.el -l test/scooter-kanban-test.el -f ert-run-tests-batch-and-exit

# ERT tests via Cask against the latest MELPA dependencies (needs network).
ert-melpa:
    cask install
    cask exec ert-runner

# All unit tests, Go and Elisp.
test-unit: test-go deps ert

# Fast checks, as CI runs them.
ci: check-flake check-fmt lint test-unit
    @echo "✅ ci passed"

# Static release binaries in dist/, one per OS/arch, plus SHA256SUMS.
dist version="dev":
    #!/usr/bin/env bash
    set -euo pipefail
    rm -rf dist && mkdir dist
    for target in linux/amd64 linux/arm64 darwin/amd64 darwin/arm64; do
      os=${target%/*} arch=${target#*/}
      CGO_ENABLED=0 GOOS=$os GOARCH=$arch go build -trimpath \
        -ldflags "-s -w -X main.version={{version}}" \
        -o "dist/scooter-$os-$arch" ./cmd/scooter
    done
    (cd dist && sha256sum scooter-* > SHA256SUMS)
