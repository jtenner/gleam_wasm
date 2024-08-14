import gleam/list
import gleam/order.{type Order}

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
    Deep(s, None, Empty, None) -> Error(Nil)
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
    Deep(s, None, Empty, None) -> Error(Nil)
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
      Deep(
        4 + length,
        Four(v0, v1, v2, v3),
        do_from_list(rest, length - 4),
        None,
      )
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
  case t1 {
    Empty -> t2
    Single(v) -> Deep(1 + { t2 |> size }, One(v), t2, None)
    Deep(s1, l1, Empty, None) -> Deep(s1 + { t2 |> size }, l1, t2, None)
    _ -> do_append_shift_pop(t1, t2)
  }
}

fn do_append_shift_pop(t1: FingerTree(u), t2: FingerTree(u)) -> FingerTree(u) {
  case shift(t2) {
    Ok(#(v, t2)) -> do_append_shift_pop(t1 |> push(v), t2)
    Error(Nil) -> t1
  }
}

pub fn new() {
  Empty
}
