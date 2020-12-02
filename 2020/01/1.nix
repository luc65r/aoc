#!/usr/bin/env -S nix eval -f

with builtins;

let
  input = readFile ./input;

  lines = s: filter (x: isString x && stringLength x > 0) (split "\n" s);

  comb = n: l: if n == 0
                  then [[]]
                  else if length l == 0
                          then []
                          else let
                            x = head l;
                            xs = tail l;
                          in map (y: [x] ++ y) (comb (n - 1) xs) ++ comb n xs;

  sum = foldl' add 0;
  prod = foldl' mul 1;

  report = n: prod (head (filter (l: sum l == 2020) (comb n (map fromJSON (lines input)))));

in
  report 2 # should work for 3, but stack overflows
