{ s }:
rec
{
  ghcidScript = s "dev" "ghcid --command 'cabal new-repl lib:aoc' --allow-eval --warnings";
}
