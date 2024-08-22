import gleam/list
import gleam/option.{None, Some}

pub opaque type FingerTree(u) {
  Empty
  Single(u)
  Deep(size: Int, Finger(u), FingerTree(Node(u)), Finger(u))
}

type Finger(u) {
  One(u)
  Two(u, u)
  Three(u, u, u)
  Four(u, u, u, u)
}

type Node(u) {
  Node1(u)
  Node2(u, u)
  Node3(u, u, u)
}

// unshift(A,empty)                      ->  {single,A};
// unshift(A,{single,B})                 ->  deep([A],empty,[B]);
// unshift(A,{deep,_,[B,C,D,E],M,SF})    ->  deep([A,B],unshift(node3(C,D,E),M),SF);
// unshift(A,{deep,_,PR,M,SF})           ->  deep([A|PR],M,SF).
// 

pub fn unshift(tree: FingerTree(u), a: u) -> FingerTree(u) {
  case tree {
    Empty -> Single(a)
    Single(b) -> Deep(2, One(a), Empty, One(b))
    Deep(s, One(b), m, sf) -> Deep(s + 1, Two(a, b), m, sf)
    Deep(s, Two(b, c), m, sf) -> Deep(s + 1, Three(a, b, c), m, sf)
    Deep(s, Three(b, c, d), m, sf) -> Deep(s + 1, Four(a, b, c, d), m, sf)
    Deep(s, Four(b, c, d, e), Empty, sf) ->
      Deep(s + 1, Two(a, b), Single(Node3(c, d, e)), sf)
    Deep(s, Four(b, c, d, e), Single(v), sf) ->
      Deep(s + 1, Two(a, b), Deep(3, One(Node3(c, d, e)), Empty, One(v)), sf)
    Deep(s, Four(b, c, d, e), v, sf) ->
      Deep(s + 1, Two(a, b), unshift_node(v, Node3(c, d, e)), sf)
  }
}

fn unshift_node(tree: FingerTree(a), node: a) -> FingerTree(a) {
  tree |> unshift(node)
}

// push(empty,A)                      ->  {single,A};
// push({single,B},A)                 ->  deep([B],empty,[A]);
// push({deep,_,PR,M,[E,D,C,B]},A)    ->  deep(PR,push(M,node3(E,D,C)),[B,A]);
// push({deep,_,PR,M,SF},A)           ->  deep(PR,M,SF++[A]).

pub fn push(tree: FingerTree(u), a: u) -> FingerTree(u) {
  case tree {
    Empty -> Single(a)
    Single(b) -> Deep(2, One(a), Empty, One(b))
    Deep(s, pr, m, One(b)) -> Deep(s + 1, pr, m, Two(b, a))
    Deep(s, pr, m, Two(c, b)) -> Deep(s + 1, pr, m, Three(c, b, a))
    Deep(s, pr, m, Three(d, c, b)) -> Deep(s + 1, pr, m, Four(d, c, b, a))
    Deep(s, pr, m, Four(e, d, c, b)) ->
      Deep(s + 1, pr, push_node(m, Node3(e, d, c)), One(b))
  }
}

fn push_node(tree: FingerTree(a), node: a) -> FingerTree(a) {
  tree |> push(node)
}

pub fn reducer(tree: FingerTree(u), acc: v, r_fn: fn(v, u) -> v) -> v {
  case tree {
    Empty -> acc
    Single(u) -> r_fn(acc, u)
    Deep(_, pr, m, sf) -> {
      let acc = reducer_finger(pr, acc, r_fn)
      let acc = reducer_node(m, acc, r_fn)
      reducer_finger(sf, acc, r_fn)
    }
  }
}

fn reducer_finger(finger: Finger(u), acc: v, r_fn: fn(v, u) -> v) -> v {
  case finger {
    One(u) -> r_fn(acc, u)
    Two(u, v) -> r_fn(r_fn(acc, u), v)
    Three(u, v, w) -> r_fn(r_fn(r_fn(acc, u), v), w)
    Four(u, v, w, x) -> r_fn(r_fn(r_fn(r_fn(acc, u), v), w), x)
  }
}

fn reducer_node(node: FingerTree(Node(u)), acc: v, r_fn: fn(v, u) -> v) -> v {
  use acc, node <- reducer(node, acc)
  case node {
    Node1(u) -> r_fn(acc, u)
    Node2(u, v) -> r_fn(r_fn(acc, u), v)
    Node3(u, v, w) -> r_fn(r_fn(r_fn(acc, u), v), w)
  }
}

pub fn reducel(tree: FingerTree(u), acc: v, r_fn: fn(v, u) -> v) -> v {
  case tree {
    Empty -> acc
    Single(u) -> r_fn(acc, u)
    Deep(_, pr, m, sf) -> {
      let acc = reducel_finger(pr, acc, r_fn)
      let acc = reducel_node(m, acc, r_fn)
      reducel_finger(sf, acc, r_fn)
    }
  }
}

fn reducel_finger(finger: Finger(u), acc: v, r_fn: fn(v, u) -> v) -> v {
  case finger {
    One(u) -> r_fn(acc, u)
    Two(v, u) -> r_fn(r_fn(acc, u), v)
    Three(w, v, u) -> r_fn(r_fn(r_fn(acc, u), v), w)
    Four(x, w, v, u) -> r_fn(r_fn(r_fn(r_fn(acc, u), v), w), x)
  }
}

fn reducel_node(node: FingerTree(Node(u)), acc: v, r_fn: fn(v, u) -> v) -> v {
  use acc, node <- reducer(node, acc)
  case node {
    Node1(u) -> r_fn(acc, u)
    Node2(v, u) -> r_fn(r_fn(acc, u), v)
    Node3(w, v, u) -> r_fn(r_fn(r_fn(acc, u), v), w)
  }
}

pub fn from_list(l: List(e)) {
  list.fold_right(l, Empty, unshift)
}

pub fn to_list(tree: FingerTree(e)) {
  reducer(tree, [], fn(h, xs) { [xs, ..h] })
}

pub fn shift(tree: FingerTree(e)) -> Result(#(e, FingerTree(e)), Nil) {
  case tree {
    Empty -> Error(Nil)
    Single(e) -> Ok(#(e, Empty))
    Deep(s, pr, m, sf) -> {
      // a is the first element in the tree
      case shift_finger(pr) {
        #(popped_a, Some(new_pr)) -> Ok(#(popped_a, Deep(s - 1, new_pr, m, sf)))
        #(popped_a, None) -> {
          // the first element is now in the middle of the tree
          // the next element is the first element in the tree

          // pop the first node out of the tree
          case shift_node_tree(m) {
            Ok(#(Node3(a, b, c), rest_nodes)) ->
              Ok(#(popped_a, Deep(s - 1, Three(a, b, c), rest_nodes, sf)))
            Ok(#(Node2(a, b), rest_nodes)) ->
              Ok(#(popped_a, Deep(s - 1, Two(a, b), rest_nodes, sf)))
            Ok(#(Node1(a), rest_nodes)) ->
              Ok(#(popped_a, Deep(s - 1, One(a), rest_nodes, sf)))
            Error(Nil) -> {
              // the tree itself is now empty, and the next element must be in the suffix
              case shift_finger(sf) {
                #(new_a, Some(new_sf)) ->
                  Ok(#(popped_a, Deep(s - 1, One(new_a), Empty, new_sf)))
                #(new_a, None) -> Ok(#(popped_a, Single(new_a)))
              }
            }
          }
        }
      }
    }
  }
}

fn shift_node_tree(nodes: FingerTree(Node(u))) {
  shift(nodes)
}

fn shift_finger(finger: Finger(u)) {
  case finger {
    One(u) -> #(u, None)
    Two(u, v) -> #(u, Some(One(v)))
    Three(u, v, w) -> #(u, Some(Two(v, w)))
    Four(u, v, w, x) -> #(u, Some(Three(v, w, x)))
  }
}

pub fn pop(tree: FingerTree(e)) -> Result(#(e, FingerTree(e)), Nil) {
  case tree {
    Empty -> Error(Nil)
    Single(e) -> Ok(#(e, Empty))
    Deep(s, pr, m, sf) -> {
      // a is the first element in the tree
      case pop_finger(sf) {
        #(popped_a, Some(new_sf)) -> Ok(#(popped_a, Deep(s - 1, pr, m, new_sf)))
        #(popped_a, None) -> {
          // the last element is now in the middle of the tree
          // the next element is the last element in the tree 

          // pop the first node out of the tree
          case pop_node_tree(m) {
            Ok(#(Node3(a, b, c), rest_nodes)) ->
              Ok(#(popped_a, Deep(s - 1, pr, rest_nodes, Three(a, b, c))))
            Ok(#(Node2(a, b), rest_nodes)) ->
              Ok(#(popped_a, Deep(s - 1, pr, rest_nodes, Two(a, b))))
            Ok(#(Node1(a), rest_nodes)) ->
              Ok(#(popped_a, Deep(s - 1, pr, rest_nodes, One(a))))
            Error(Nil) -> {
              // the tree itself is now empty, and the next element must be in the suffix
              case pop_finger(pr) {
                #(new_a, Some(new_pr)) ->
                  Ok(#(popped_a, Deep(s - 1, new_pr, Empty, One(new_a))))
                #(new_a, None) -> Ok(#(popped_a, Single(new_a)))
              }
            }
          }
        }
      }
    }
  }
}

fn pop_node_tree(nodes: FingerTree(Node(u))) {
  pop(nodes)
}

fn pop_finger(finger: Finger(u)) {
  case finger {
    One(u) -> #(u, None)
    Two(u, v) -> #(v, Some(One(u)))
    Three(u, v, w) -> #(w, Some(Two(u, v)))
    Four(u, v, w, x) -> #(x, Some(Three(u, v, w)))
  }
}

pub fn filter(tree: FingerTree(e), f: fn(e) -> Bool) -> FingerTree(e) {
  reducel(tree, Empty, fn(acc, e) {
    case f(e) {
      True -> acc |> push(e)
      False -> acc
    }
  })
}

pub fn new() {
  Empty
}
