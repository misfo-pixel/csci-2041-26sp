(*You must first define an OCaml type 'key bst that describes a binary search tree whose keys have the type 'key. Here’s how to do that:*)

type 'key bst = BstEmpty | BstNode of 'key *'key bst *'key bst ;;

(*The type 'key may be any OCaml type; all keys in a BST have the same type. The constructor BstEmpty returns an empty BST. The constructor BstNode(k, l, r) returns a non-empty BST with a node at its root. Here k is the root’s key, of type 'key; l is the root’s left subtree, of type 'key bst; and r is the root’s right subtree, also of type 'key bst. (A ‘‘real’’ binary search tree would also have a value in each node, but we don’t care about values here.)

Using BstEmpty and BstNode, you must define an OCaml function bstDelete tree key, where tree has the type 'a bst, and key has the type 'a. The function itself has the type 'a bst -> 'a -> 'a bst. You may assume that tree satisfies the BST property. Your function must return a new BST that is like tree, and that satisfies the BST property, but in which the node containing key has been deleted. The node containing key may appear in any subtree of tree. There are at least five different cases that bstDelete must handle correctly:

Deleting from an empty BST.

Deleting from a BST with empty left and right subtrees.

Deleting from a BST with an empty left subtree and a non-empty right subtree.

Deleting from a BST with a non-empty left subtree and an empty right subtree.

Deleting from a BST with a non-empty left subtree and a non-empty right subtree.
*)

let rec find_max tree =
  match tree with
    | BstEmpty -> raise Not_found
    | BstNode (k, _, BstEmpty) -> k
    | BstNode (_, _, r) -> find_max r

let rec bstDelete t key =
  match t with
    | BstEmpty -> BstEmpty
    | BstNode (k, l, r) ->
        if key < k then
          BstNode (k, bstDelete l key, r)
        else if key > k then
          BstNode (k, l, bstDelete r key)
        else
          match (l, r) with
            | (BstEmpty, BstEmpty) -> BstEmpty
            | (BstEmpty, _) -> r
            | (_, BstEmpty) -> l
            | (_, _) ->
                let max_val = find_max l in
                let new_l = bstDelete l max_val in
                BstNode (max_val, new_l, r)
(*
Here are some hints.

Since bstDelete cannot use mutable data structures, it must return a copy of tree in which the node containing key does not appear. For efficiency, it must not copy all the nodes of tree! It must copy only the nodes that are necessary. If tree is well-balanced and has n > 0 nodes, then bstDelete must work in O(log₂ n) time.

It’s not an error if a node containing key never appears in tree! If that happens, then bstDelete must return either tree, or else a partial copy of tree—as described in the previous hint.

The cases shown above can be implemented using OCaml’s match–with mechanism. Each case will have an expression involving BstEmpty and/or BstNode on the left of the arrow ‘->’. Most cases may have an if–then–else on the right of the arrow.

Many of the cases will involve recursive calls. They need not all be tail recursions. Maybe none of them will be! It is impossible (or at least hard) to write bstDelete in a completely tail-recursive way.

For the last case, you must write a helper function bstMaxKey tree that returns the maximum key in tree, whose type is 'a bst. It must therefore have the type 'a bst -> 'a. To find the maximum key, bstMaxKey must move right down through tree as deeply as it can, until it finds the rightmost node. It must then return the key in that node. It must raise the exception BadEmptyBst if tree is empty, but that should never happen.
*)