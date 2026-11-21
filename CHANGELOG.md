# Changelog for `bel-expr`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

### Added
- `evalText :: Env -> Text -> Aeson.Value`: pure evaluation of a single
  `{{...}}` expression. Unparsable input is returned as `String input`;
  `debug`/`copy` yield the traced argument's value with no clipboard side effect.

### Changed
- `render` now emits `Aeson.String` values verbatim (no Haskell `show`
  escaping of `"`, `\` or non-ASCII characters), and objects/arrays as
  compact UTF-8 JSON instead of Haskell `show` output. Numbers, booleans and
  null are unchanged.

### Removed
- Debug output on production paths: `eval` no longer prints each evaluated
  expression to stdout, and `queryEnvRespBody`, `queryEnvRespHeaders`,
  `match` and `queryBody` no longer `trace` to stderr (the stored response
  could contain secrets).

## 0.1.0.0 - YYYY-MM-DD
