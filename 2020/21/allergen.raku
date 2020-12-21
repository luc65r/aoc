#!/usr/bin/env raku

use v6;

grammar Ingredients {
    rule TOP { [ <ingredient> ]+ '(' 'contains' [ <allergen> ]+ % ',' ')' }
    token ingredient { \w+ }
    token allergen { \w+ }
}

class MakeIngredients {
    method TOP($/) { make $<allergen>».&{~$_ => ~«$<ingredient>}.Map }
}

my Str @ingredients;
my Set %allergens{Str};
for slurp.lines -> $l {
    for Ingredients.parse($l, actions => MakeIngredients).made.kv -> $k, $v {
        once @ingredients.append: @$v;
        if %allergens{$k}:exists {
            %allergens{$k} = %allergens{$k} ∩ $v;
        } else {
            %allergens{$k} = $v.Set;
        }
    }
}

my Str %final{Str};
until %allergens.elems == 0 {
    my $uniq = %allergens.first: *.value.elems == 1;
    %final.push: $uniq.key => $uniq.value.keys.head;
    %allergens{$uniq.key}:delete;
    for %allergens.values <-> $v {
        $v ∖= $uniq.value;
    }
}

say @ingredients.grep(* ∉ %final.values).elems;
say %final.sort(*.key)».value.join: ',';
