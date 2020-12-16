#!/usr/bin/env raku

use v6;

grammar Ticket {
    rule TOP { ^ <tRule>+ <your> <ticket> <nearby> <ticket>+ % \n $ }

    rule tRule { <field> ':' <range> 'or' <range> }
    token field { <-[ : ]>+ }
    token range { $<mini> = <.nb> '-' $<maxi> = <.nb> }

    token your { 'your ticket:' }
    token nearby { 'nearby tickets:' }
    token ticket { <nb>+ % ',' }
    token nb { \d+ }
}

class Scanning {
    method TOP($/) {
        my %rules = $<tRule>».made;
        my $range = [∪] %rules.values;
        my $ticket = $<ticket>[0].made;
        my $tickets = $<ticket>[1..*]».made;
        my $rate = [+] $tickets.flat».&{$_ ∈ $range ?? 0 !! $_};
        my @valid := $tickets.grep(so *».&{$_ ∈ $range}.all).list;
        my %field;
        for %rules.kv -> $name, $r {
            %field{$name} = [∩] @valid.map: *.grep(* ∈ $r, :k);
        }
        my %final;
        until %field.elems == 0 {
            my $uniq = %field.first: *.value.elems == 1;
            %final.push: $uniq;
            %field{$uniq.key}:delete;
            for %field.values <-> $v {
                $v ∖= $uniq.value;
            }
        }
        my $p = [×] %final.grep(*.key.starts-with: "departure")».values.flat».keys.flat».&{$ticket[$_]};
        make ($rate, $p);
    }
    method tRule($/) { make ~$<field> => [∪] $<range>».made; }
    method range($/) { make $<mini> .. $<maxi>; }
    method ticket($/) { make $<nb>».made; }
    method nb($/) { make +$/; }
}

.say for Ticket.parse(slurp, actions => Scanning).made;
