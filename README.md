# DAM4G Simulator

DAM4G: Demonstrative Abstract Machine for GDGoC Osaka Univ.

This project is solely for presentation purposes at the conference [低レイヤー講演会](https://gdsc-osaka-univ.connpass.com/event/333363/)

## Syntax

### Algebraic Data Types

Currently, only two types `Int` and `Bool` are exported from `Base` library. On top of these atomic types, one can define your custom data type with `def set` syntax:

```plain
def set Suit = Heart | Diamond | Spade | Club

def set Card =
  | Joker
  | Card of Suit * Int
```

### Pattern matching

You can write functions doing pattern-matching on arguments
with `match ... with` syntax:

```plain
let isJoker (c: Card) = match c with 
  | Joker -> true
  | _ -> false
  ;
```

If your function returns immediately matching on arguments,
you can write the function with more consise `matchfn` syntax:

```
let isJoker = matchfn (_: Card)
  | Joker -> true
  | _ -> false
  ;
```

### Binary operator aliases

You can define operator aliases for defined functions with
  `def alias name as operator` syntax:

```
let xor (b1: Bool) (b1: Bool) = 
  match b1, b2 with 
  | true, false -> true
  | false, true -> true
  | _, _ -> false 
  ;

def alias xor as ^ (assoc left, 5)

let it = true ^ false
```

## Compilation Pass

## References

- <https://xavierleroy.org/bibrefs/Leroy-ZINC.html>
- <http://logic.cs.tsukuba.ac.jp/jikken/zam.html>
- <https://museum.ipsj.or.jp/guide/pdf/magazine/IPSJ-MGN430205.pdf>
