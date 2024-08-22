import gleam/list
import gleam/order.{type Order}
import gleam/result

type Digits(u) {
  None
  One(u)
  Two(u, u)
  Three(u, u, u)
  Four(u, u, u, u)
}

pub opaque type FingerTree(u) {
  Empty
  Single(val: u)
  Deep(size: Int, left: Digits(u), trunk: FingerTree(u), right: Digits(u))
}

pub fn unshift(tree: FingerTree(u), val: u) -> FingerTree(u) {
  case tree {
    Empty -> Single(val)
    Single(v) -> Deep(2, Two(val, v), Empty, None)
    Deep(s, None, t, r) -> Deep(s + 1, One(val), t, r)
    Deep(s, One(v), t, r) -> Deep(s + 1, Two(val, v), t, r)
    Deep(s, Two(v1, v2), t, r) -> Deep(s + 1, Three(val, v1, v2), t, r)
    Deep(s, Three(v1, v2, v3), t, r) -> Deep(s + 1, Four(val, v1, v2, v3), t, r)
    Deep(s, Four(v1, v2, v3, v4), Empty, r) ->
      Deep(s + 1, Two(val, v1), Deep(3, Three(v2, v3, v4), Empty, None), r)
    Deep(s, Four(v1, v2, v3, v4), t, r) ->
      Deep(
        s + 1,
        Two(val, v1),
        t
          |> unshift(v4)
          |> unshift(v3)
          |> unshift(v2),
        r,
      )
  }
}

pub fn push(tree: FingerTree(u), val: u) -> FingerTree(u) {
  case tree {
    Empty -> Single(val)
    Single(v0) -> Deep(2, None, Empty, Two(v0, val))
    Deep(s, l, t, None) -> Deep(s + 1, l, t, One(val))
    Deep(s, l, t, One(v1)) -> Deep(s + 1, l, t, Two(v1, val))
    Deep(s, l, t, Two(v1, v2)) -> Deep(s + 1, l, t, Three(v1, v2, val))
    Deep(s, l, t, Three(v1, v2, v3)) -> Deep(s + 1, l, t, Four(v1, v2, v3, val))
    Deep(s, l, Empty, Four(v1, v2, v3, v4)) ->
      Deep(s + 1, l, Deep(3, None, Empty, Three(v1, v2, v3)), Two(v4, val))
    Deep(s, l, t, Four(v1, v2, v3, v4)) ->
      Deep(
        s + 1,
        l,
        t
          |> push(v1)
          |> push(v2)
          |> push(v3),
        Two(v4, val),
      )
  }
}

pub fn shift(tree: FingerTree(u)) -> Result(#(u, FingerTree(u)), Nil) {
  case tree {
    Empty -> Error(Nil)
    Single(v) -> Ok(#(v, Empty))
    Deep(s, Four(v1, v2, v3, v4), t, r) ->
      Ok(#(v1, Deep(s - 1, Three(v2, v3, v4), t, r)))
    Deep(s, Three(v1, v2, v3), t, r) ->
      Ok(#(v1, Deep(s - 1, Two(v2, v3), t, r)))
    Deep(s, Two(v1, v2), t, r) -> Ok(#(v1, Deep(s - 1, One(v2), t, r)))
    Deep(s, One(v1), t, r) -> Ok(#(v1, Deep(s - 1, None, t, r)))
    Deep(s, None, Empty, Four(v1, v2, v3, v4)) ->
      Ok(#(v1, Deep(s - 1, None, Empty, Three(v2, v3, v4))))
    Deep(s, None, Empty, Three(v1, v2, v3)) ->
      Ok(#(v1, Deep(s - 1, None, Empty, Two(v2, v3))))
    Deep(s, None, Empty, Two(v1, v2)) ->
      Ok(#(v1, Deep(s - 1, None, Empty, One(v2))))
    Deep(s, None, Empty, One(v1)) -> Ok(#(v1, Deep(s - 1, None, Empty, None)))
    Deep(_, None, Empty, None) -> Error(Nil)
    Deep(s, None, Single(v1), r) -> Ok(#(v1, Deep(s - 1, None, Empty, r)))
    Deep(s, None, t, r) ->
      case shift(t) {
        Ok(#(v, t)) -> Ok(#(v, Deep(s - 1, None, t, r)))
        Error(e) -> Error(e)
      }
  }
}

pub fn pop(tree: FingerTree(u)) -> Result(#(u, FingerTree(u)), Nil) {
  case tree {
    Empty -> Error(Nil)
    Single(v) -> Ok(#(v, Empty))
    Deep(s, l, t, Four(v1, v2, v3, v4)) ->
      Ok(#(v4, Deep(s - 1, l, t, Three(v1, v2, v3))))
    Deep(s, l, t, Three(v1, v2, v3)) ->
      Ok(#(v3, Deep(s - 1, l, t, Two(v1, v2))))
    Deep(s, l, t, Two(v1, v2)) -> Ok(#(v2, Deep(s - 1, l, t, One(v1))))
    Deep(s, l, t, One(v1)) -> Ok(#(v1, Deep(s - 1, l, t, None)))
    Deep(s, Four(v1, v2, v3, v4), Empty, None) ->
      Ok(#(v4, Deep(s - 1, Three(v1, v2, v3), Empty, None)))
    Deep(s, Three(v1, v2, v3), Empty, None) ->
      Ok(#(v3, Deep(s - 1, Two(v1, v2), Empty, None)))
    Deep(s, Two(v1, v2), Empty, None) ->
      Ok(#(v2, Deep(s - 1, One(v1), Empty, None)))
    Deep(s, One(v1), Empty, None) -> Ok(#(v1, Deep(s - 1, None, Empty, None)))
    Deep(_, None, Empty, None) -> Error(Nil)
    Deep(s, l, Single(v1), None) -> Ok(#(v1, Deep(s - 1, l, Empty, None)))
    Deep(s, l, t, None) ->
      case pop(t) {
        Ok(#(v, t)) -> Ok(#(v, Deep(s - 1, l, t, None)))
        Error(e) -> Error(e)
      }
  }
}

pub fn reverse(tree: FingerTree(u)) -> FingerTree(u) {
  case tree {
    Empty -> Empty
    Single(v) -> Single(v)
    Deep(s, l, t, r) ->
      Deep(s, r |> reverse_digits, t |> reverse, l |> reverse_digits)
  }
}

fn reverse_digits(digits: Digits(u)) -> Digits(u) {
  case digits {
    None -> None
    One(v) -> One(v)
    Two(v1, v2) -> Two(v2, v1)
    Three(v1, v2, v3) -> Three(v3, v2, v1)
    Four(v1, v2, v3, v4) -> Four(v4, v3, v2, v1)
  }
}

pub fn from_list(items: List(u)) -> FingerTree(u) {
  let length = items |> list.length
  do_from_list(items, length)
}

fn do_from_list(items: List(u), length: Int) -> FingerTree(u) {
  case items {
    [v0, v1, v2, v3, ..rest] ->
      Deep(length, Four(v0, v1, v2, v3), do_from_list(rest, length - 4), None)
    [v0, v1, v2] -> Deep(3, Three(v0, v1, v2), Empty, None)
    [v0, v1] -> Deep(2, Two(v0, v1), Empty, None)
    [v0] -> Single(v0)
    [] -> Empty
  }
}

pub fn to_list(tree: FingerTree(u)) -> List(u) {
  case tree {
    Empty -> []
    Single(v) -> [v]
    Deep(_, l, t, r) ->
      list.concat([l |> digits_to_list, t |> to_list, r |> digits_to_list])
  }
}

fn digits_to_list(digits: Digits(u)) -> List(u) {
  case digits {
    None -> []
    One(v) -> [v]
    Two(v1, v2) -> [v2, v1]
    Three(v1, v2, v3) -> [v3, v2, v1]
    Four(v1, v2, v3, v4) -> [v4, v3, v2, v1]
  }
}

pub fn sort(tree: FingerTree(u), cmp: fn(u, u) -> Order) -> FingerTree(u) {
  tree
  |> to_list
  |> list.sort(cmp)
  |> from_list
}

pub fn size(tree: FingerTree(u)) -> Int {
  case tree {
    Empty -> 0
    Single(_) -> 1
    Deep(s, _, _, _) -> s
  }
}

pub fn append(t1: FingerTree(u), t2: FingerTree(u)) -> FingerTree(u) {
  case t1, t2 {
    Empty, t2 -> t2
    Single(v), t2 -> Deep(1 + { t2 |> size }, One(v), t2, None)
    Deep(0, _, _, _), t2 -> t2
    Deep(s1, l1, Empty, None), t2 -> Deep(s1 + { t2 |> size }, l1, t2, None)
    t1, Deep(s2, None, Empty, r) -> Deep({ t1 |> size } + s2, None, t1, r)
    t1, t2 -> do_append_shift_pop(t1, t2)
  }
}

fn do_append_shift_pop(t1: FingerTree(u), t2: FingerTree(u)) -> FingerTree(u) {
  case shift(t2) {
    Error(Nil) -> t1
    Ok(#(v, t2)) -> do_append_shift_pop(t1 |> push(v), t2)
  }
}

pub fn new() {
  Empty
}

pub fn try_fold(tree: FingerTree(v), init: u, f: fn(u, v) -> Result(u, w)) {
  do_try_fold(tree, init, f)
}

fn do_try_fold(tree: FingerTree(v), init: u, f: fn(u, v) -> Result(u, w)) {
  case tree {
    Empty -> Ok(init)
    Single(v) -> f(init, v)
    Deep(_, l, t, r) -> {
      use init <- result.try(do_try_fold_digits(l, init, f))
      use init <- result.try(do_try_fold(t, init, f))
      do_try_fold_digits(r, init, f)
    }
  }
}

fn do_try_fold_digits(digits: Digits(v), init: u, f: fn(u, v) -> Result(u, w)) {
  case digits {
    None -> Ok(init)
    One(v) -> f(init, v)
    Two(v1, v2) -> {
      use init <- result.try(f(init, v1))
      f(init, v2)
    }
    Three(v1, v2, v3) -> {
      use init <- result.try(f(init, v1))
      use init <- result.try(f(init, v2))
      f(init, v3)
    }
    Four(v1, v2, v3, v4) -> {
      use init <- result.try(f(init, v1))
      use init <- result.try(f(init, v2))
      use init <- result.try(f(init, v3))
      f(init, v4)
    }
  }
}

pub fn fold(tree: FingerTree(v), init: u, f: fn(u, v) -> u) -> u {
  do_fold(tree, init, f)
}

fn do_fold(tree: FingerTree(v), init: u, f: fn(u, v) -> u) -> u {
  case tree {
    Empty -> init
    Single(v) -> f(init, v)
    Deep(_, l, t, r) ->
      do_fold_digits(l, init, f)
      |> do_fold(t, _, f)
      |> do_fold_digits(r, _, f)
  }
}

fn do_fold_digits(digits: Digits(v), init: u, f: fn(u, v) -> u) -> u {
  case digits {
    None -> init
    One(v) -> f(init, v)
    Two(v1, v2) -> init |> f(v1) |> f(v2)
    Three(v1, v2, v3) -> init |> f(v1) |> f(v2) |> f(v3)
    Four(v1, v2, v3, v4) -> init |> f(v1) |> f(v2) |> f(v3) |> f(v4)
  }
}

pub fn drop(tree: FingerTree(u), n: Int) -> Result(FingerTree(u), Nil) {
  case tree {
    t if n == 0 -> Ok(t)
    Empty -> Error(Nil)
    Single(_) if n > 1 -> Error(Nil)
    Single(_) -> Ok(Empty)
    Deep(0, _, _, _) | Deep(_, None, Empty, None) if n > 1 -> Error(Nil)
    Deep(s, l, t, r) -> {
      case drop_digit(l) {
        Ok(l) -> drop(Deep(s - 1, l, t, r), n - 1)
        Error(_) ->
          case drop(t, n) {
            Ok(t) -> drop(Deep(s - 1, l, t, r), n - 1)
            Error(_) ->
              case drop_digit(r) {
                Ok(r) ->
                  drop(
                    Deep(count_digits(l) + size(t) + count_digits(r), l, t, r),
                    n - 1,
                  )
                Error(_) -> Error(Nil)
              }
          }
      }
    }
  }
}

fn drop_digit(digits: Digits(u)) -> Result(Digits(u), Nil) {
  case digits {
    None -> Error(Nil)
    One(_) -> Ok(None)
    Two(_, a) -> Ok(One(a))
    Three(_, a, b) -> Ok(Two(a, b))
    Four(_, a, b, c) -> Ok(Three(a, b, c))
  }
}

fn count_digits(digits: Digits(u)) -> Int {
  case digits {
    None -> 0
    One(_) -> 1
    Two(_, _) -> 2
    Three(_, _, _) -> 3
    Four(_, _, _, _) -> 4
  }
}

pub fn filter(tree: FingerTree(v), f: fn(v) -> Bool) -> FingerTree(v) {
  do_filter(tree, Empty, f)
}

fn do_filter(
  tree: FingerTree(v),
  acc: FingerTree(v),
  f: fn(v) -> Bool,
) -> FingerTree(v) {
  case tree {
    Empty -> acc
    Single(v) ->
      case f(v) {
        True -> acc |> push(v)
        False -> acc
      }
    Deep(_, l, t, r) ->
      do_filter_digits(l, acc, f)
      |> do_filter(t, _, f)
      |> do_filter_digits(r, _, f)
  }
}

fn do_filter_digits(
  digits: Digits(v),
  acc: FingerTree(v),
  f: fn(v) -> Bool,
) -> FingerTree(v) {
  case digits {
    None -> acc
    One(v) -> acc |> filter_digit(v, f)
    Two(v1, v2) -> acc |> filter_digit(v1, f) |> filter_digit(v2, f)
    Three(v1, v2, v3) ->
      acc |> filter_digit(v1, f) |> filter_digit(v2, f) |> filter_digit(v3, f)
    Four(v1, v2, v3, v4) ->
      acc
      |> filter_digit(v1, f)
      |> filter_digit(v2, f)
      |> filter_digit(v3, f)
      |> filter_digit(v4, f)
  }
}

fn filter_digit(acc: FingerTree(v), item: v, f: fn(v) -> Bool) -> FingerTree(v) {
  case f(item) {
    True -> acc |> push(item)
    False -> acc
  }
}

pub fn try_map(
  tree: FingerTree(v),
  f: fn(v) -> Result(u, String),
) -> Result(FingerTree(u), String) {
  case tree {
    Empty -> Ok(Empty)
    Single(v) -> {
      use result <- result.map(f(v))
      Single(result)
    }
    Deep(s, l, t, r) -> {
      use l <- result.try(l |> try_map_digits(f))
      use t <- result.try(t |> try_map(f))
      use r <- result.map(r |> try_map_digits(f))
      Deep(s, l, t, r)
    }
  }
}

fn try_map_digits(
  digits: Digits(v),
  f: fn(v) -> Result(u, String),
) -> Result(Digits(u), String) {
  case digits {
    None -> Ok(None)
    One(v) -> {
      use v <- result.map(f(v))
      One(v)
    }
    Two(v1, v2) -> {
      use v1 <- result.try(f(v1))
      use v2 <- result.map(f(v2))
      Two(v1, v2)
    }
    Three(v1, v2, v3) -> {
      use v1 <- result.try(f(v1))
      use v2 <- result.try(f(v2))
      use v3 <- result.map(f(v3))
      Three(v1, v2, v3)
    }
    Four(v1, v2, v3, v4) -> {
      use v1 <- result.try(f(v1))
      use v2 <- result.try(f(v2))
      use v3 <- result.try(f(v3))
      use v4 <- result.map(f(v4))
      Four(v1, v2, v3, v4)
    }
  }
}

pub fn map(tree: FingerTree(v), f: fn(v) -> u) -> FingerTree(u) {
  case tree {
    Empty -> Empty
    Single(v) -> Single(f(v))
    Deep(s, l, t, r) ->
      Deep(s, l |> map_digits(f), t |> map(f), r |> map_digits(f))
  }
}

fn map_digits(digits: Digits(v), f: fn(v) -> u) -> Digits(u) {
  case digits {
    None -> None
    One(v) -> One(f(v))
    Two(v1, v2) -> Two(f(v1), f(v2))
    Three(v1, v2, v3) -> Three(f(v1), f(v2), f(v3))
    Four(v1, v2, v3, v4) -> Four(f(v1), f(v2), f(v3), f(v4))
  }
}

pub fn map_index(tree: FingerTree(v), f: fn(v, Int) -> u) -> FingerTree(u) {
  let #(tree, _) = do_map_index(tree, 0, Empty, f)
  tree
}

fn do_map_index(
  tree: FingerTree(v),
  i: Int,
  acc: FingerTree(u),
  f: fn(v, Int) -> u,
) {
  case tree {
    Empty -> #(acc, i)
    Single(v) -> #(acc |> push(f(v, i)), i + 1)
    Deep(_, l, t, r) -> {
      let #(acc, i) = do_map_index_digits(l, i, acc, f)
      let #(acc, i) = do_map_index(t, i, acc, f)
      do_map_index_digits(r, i, acc, f)
    }
  }
}

fn do_map_index_digits(
  digits: Digits(v),
  i: Int,
  acc: FingerTree(u),
  f: fn(v, Int) -> u,
) {
  case digits {
    None -> #(acc, i)
    One(v) -> #(acc |> push(f(v, i)), i + 1)
    Two(v1, v2) -> #(acc |> push(f(v1, i)) |> push(f(v2, i + 1)), i + 2)
    Three(v1, v2, v3) -> #(
      acc |> push(f(v1, i)) |> push(f(v2, i + 1)) |> push(f(v3, i + 2)),
      i + 3,
    )
    Four(v1, v2, v3, v4) -> #(
      acc
        |> push(f(v1, i))
        |> push(f(v2, i + 1))
        |> push(f(v3, i + 2))
        |> push(f(v4, i + 3)),
      i + 4,
    )
  }
}

pub fn flat(tree: FingerTree(FingerTree(v))) -> FingerTree(v) {
  tree |> fold(Empty, append)
}

pub fn take(
  tree: FingerTree(v),
  n: Int,
) -> Result(#(List(v), FingerTree(v)), String) {
  do_take(tree, n, [])
}

fn do_take(
  tree: FingerTree(v),
  n: Int,
  acc: List(v),
) -> Result(#(List(v), FingerTree(v)), String) {
  case n {
    0 -> Ok(#(acc |> list.reverse, tree))
    _ -> {
      case shift(tree) {
        Ok(#(item, tree)) -> do_take(tree, n - 1, [item, ..acc])
        Error(_) -> Error("Not enough items")
      }
    }
  }
}

pub fn get(tree: FingerTree(v), n: Int) -> Result(v, String) {
  do_get(tree, n)
  |> result.replace_error("Invalid index")
}

fn do_get(tree: FingerTree(v), n: Int) {
  case tree, n {
    _, n if n < 0 -> Error(0)
    Empty, _ -> Error(0)
    Single(v), n if n == 1 -> Ok(v)
    Single(_), _ -> Error(1)
    Deep(s, _, _, _), n if n >= s -> Error(s)
    Deep(_, l, t, r), n -> {
      use count_l <- result.try_recover(do_get_digits(l, n))
      use count_t <- result.try_recover(do_get(t, n - count_l))
      use count_r <- result.try_recover(do_get_digits(r, n - count_l - count_t))
      Error(count_l + count_t + count_r)
    }
  }
}

fn do_get_digits(digits: Digits(v), n: Int) {
  case digits, n {
    _, n if n < 0 -> Error(0)
    None, _ -> Error(0)
    One(v), 0 -> Ok(v)
    One(_), _ -> Error(1)
    Two(v1, _), 0 -> Ok(v1)
    Two(_, v2), 1 -> Ok(v2)
    Two(_, _), _ -> Error(2)
    Three(v1, _, _), 0 -> Ok(v1)
    Three(_, v2, _), 1 -> Ok(v2)
    Three(_, _, v3), 2 -> Ok(v3)
    Three(_, _, _), _ -> Error(3)
    Four(v1, _, _, _), 0 -> Ok(v1)
    Four(_, v2, _, _), 1 -> Ok(v2)
    Four(_, _, v3, _), 2 -> Ok(v3)
    Four(_, _, _, v4), 3 -> Ok(v4)
    Four(_, _, _, _), _ -> Error(4)
  }
}

pub fn set(tree: FingerTree(v), n: Int, v: v) -> Result(FingerTree(v), String) {
  do_set(tree, n, v)
  |> result.replace_error("Invalid index")
}

fn do_set(tree: FingerTree(v), n: Int, v: v) {
  case tree, n {
    _, n if n < 0 -> Error(0)
    Empty, _ -> Error(0)
    Single(_), 0 -> Ok(Single(v))
    Single(_), _ -> Error(1)
    Deep(s, _, _, _), n if n >= s -> Error(s)
    Deep(s, l, t, r), n -> {
      case do_set_digits(l, n, v) {
        Ok(l) -> Ok(Deep(s, l, t, r))
        Error(count_l) ->
          case do_set(t, n - count_l, v) {
            Ok(t) -> Ok(Deep(s, l, t, r))
            Error(count_t) ->
              case do_set_digits(r, n - count_l - count_t, v) {
                Ok(r) -> Ok(Deep(s, l, t, r))
                Error(count_r) -> Error(count_l + count_t + count_r)
              }
          }
      }
    }
  }
}

fn do_set_digits(digits: Digits(v), n: Int, v: v) {
  case digits, n {
    _, n if n < 0 -> Error(0)
    None, _ -> Error(0)
    One(_), 0 -> Ok(One(v))
    One(_), _ -> Error(1)
    Two(_, v1), 0 -> Ok(Two(v, v1))
    Two(v1, _), 1 -> Ok(Two(v1, v))
    Two(_, _), _ -> Error(2)
    Three(_, v1, v2), 0 -> Ok(Three(v, v1, v2))
    Three(v1, _, v2), 1 -> Ok(Three(v1, v, v2))
    Three(v1, v2, _), 2 -> Ok(Three(v1, v2, v))
    Three(_, _, _), _ -> Error(3)
    Four(_, v1, v2, v3), 0 -> Ok(Four(v, v1, v2, v3))
    Four(v1, _, v2, v3), 1 -> Ok(Four(v1, v, v2, v3))
    Four(v1, v2, _, v3), 2 -> Ok(Four(v1, v2, v, v3))
    Four(v1, v2, v3, _), 3 -> Ok(Four(v1, v2, v3, v))
    Four(_, _, _, _), _ -> Error(4)
  }
}

pub const empty = Empty

pub fn one(v: v) -> FingerTree(v) {
  Single(v)
}
