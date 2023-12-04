// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

#![feature(doc_cfg)]
#![warn(clippy::missing_const_for_fn)]

//! Trees based on indexes to a generational arena.
//!
//! # Features
//! - `default`
//!   - `split`, `unified`, `deque`
//! - `split`
//!   - Enables
#![cfg_attr(not(feature = "split"), doc = "     split")]
#![cfg_attr(feature = "split", doc = "     [split]")]
//!     nodes, where each [node] is split into
#![cfg_attr(not(feature = "split"), doc = "     ['branches'](BranchNode),")]
#![cfg_attr(feature = "split", doc = "     ['branches'](split::Branch),")]
//!     which may have [children], and
#![cfg_attr(not(feature = "split"), doc = "     'leaves',")]
#![cfg_attr(feature = "split", doc = "     ['leaves'](split::Leaf),")]
//!     which may not have [children].
//! - `unified`
//!   - Enables
#![cfg_attr(not(feature = "unified"), doc = "     unified")]
#![cfg_attr(feature = "unified", doc = "     [unified]")]
//!     nodes, where every [node] may have [children].
//! - `deque`
//!   - Enables [nodes] which have [`VecDeque`] [children], rather than siblings forming a [linked]
//!     list, allowing operations by index.
//! - `serde`
//!   - Enables implementations of [serde]'s [`Serialize`] and [`Deserialize`] traits.
//!
//! [serde]: https://serde.rs/
//! [`Serialize`]: https://docs.rs/serde/1.0.193/serde/trait.Serialize.html
//! [`Deserialize`]: https://docs.rs/serde/1.0.193/serde/trait.Deserialize.html
//!
//! [node]: Node
//! [nodes]: Node
//! [linked]: LinkedNode
//! [children]: BranchNode::children

pub mod iter;

#[cfg(feature = "split")]
#[doc(cfg(feature = "split"))]
pub mod split;
#[cfg(feature = "unified")]
#[doc(cfg(feature = "unified"))]
pub mod unified;

macro_rules! token_to_node {
    ($(ref $($mut:ident)?,)? $N:ty: $token:expr, $arena:expr) => {
		<$(&$($mut)?)? $N as TryFrom<$(&$($mut)?)? <$N as Node>::Base>>::try_from(
			$(&$($mut)?)? ($arena).0[($token).idx()]
		)
		.expect("We know this is the correct node type.")
	};
}

use std::{
	collections::VecDeque,
	fmt::{Debug, Formatter},
	hash::{Hash, Hasher},
	iter::FusedIterator,
	marker::PhantomData,
	ops::Index,
};

use cfg_attrs::cfg_attrs;
#[cfg(feature = "serde")]
use serde::{Deserialize, Deserializer, Serialize, Serializer};
pub(crate) use token_to_node;

pub struct Token<N: Node> {
	idx: generational_arena::Index,
	_marker: PhantomData<N>,
}

#[cfg(feature = "serde")]
impl<N: Node> Serialize for Token<N> {
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: Serializer,
	{
		self.idx.serialize(serializer)
	}
}

#[cfg(feature = "serde")]
impl<'a, N: Node> Deserialize<'a> for Token<N> {
	fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
	where
		D: Deserializer<'a>,
	{
		Ok(Self {
			idx: generational_arena::Index::deserialize(deserializer)?,
			_marker: PhantomData,
		})
	}
}

impl<N: Node> Debug for Token<N> {
	#[inline(always)]
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "Token({:?})", self.idx)
	}
}

impl<N: Node, I: Idx> PartialEq<I> for Token<N> {
	#[inline(always)]
	fn eq(&self, other: &I) -> bool {
		self.idx() == other.idx()
	}
}

impl<N: Node> Eq for Token<N> {}

impl<N: Node> Hash for Token<N> {
	#[inline(always)]
	fn hash<H: Hasher>(&self, state: &mut H) {
		self.idx().hash(state)
	}
}

impl<N: Node> Clone for Token<N> {
	#[inline(always)]
	fn clone(&self) -> Self {
		*self
	}
}

impl<N: Node> Copy for Token<N> {}

mod sealed {
	/// Seals a trait, restricting external crates from implementing it.
	pub trait Sealed {}

	/// Provides a [`NodeToken`]'s [arena index] and seals the trait.
	///
	/// [`NodeToken`]: NodeToken
	/// [arena index]: generational_arena::Index
	pub trait Idx {
		/// Return's this [token]'s [arena index].
		///
		/// [token]: NodeToken
		/// [arena index]: generational_arena::Index;
		fn idx(&self) -> generational_arena::Index;
	}
}

pub(crate) use sealed::*;

pub trait NodeToken<N: Node>: Idx + Copy + PartialEq + Debug {
	/// Returns this [node]'s parent's token.
	///
	/// If this token refers to a root node, that node will have no parent, so [`None`] is returned.
	///
	/// [node]: Node
	#[inline(always)]
	fn parent(&self, arena: &Arena<N::Base>) -> Option<<<N::Base as BaseNode>::Branch as Node>::Token> {
		arena.0[self.idx()].parent()
	}

	/// Returns an iterator over the tokens of this [node]'s ancestors.
	///
	/// This iterator begins with the [node]'s [parent] and ends with the [root] node (i.e. the
	/// ancestor with no [parent]).
	///
	/// [node]: Node
	/// [parent]: Self::parent
	/// [root]: Self::root
	#[inline(always)]
	fn ancestors<'arena>(&self, arena: &'arena Arena<N::Base>) -> iter::Ancestors<'arena, N::Base> {
		arena.0[self.idx()].ancestors(arena)
	}

	/// Returns this [node]'s root node.
	///
	/// The root node is the most distant [ancestor]; it is the only [node] in a tree that does not
	/// have a [parent].
	///
	/// If this [node] is the root node, [`RootToken::This`] is returned. Otherwise,
	/// [`RootToken::Ancestor`] is returned with the root node's token.
	///
	/// Internally, this method iterates over the [node]'s [ancestors] to find the last one, so it
	/// is `O(n)` best, average, and worst case.
	///
	/// [node]: Node
	/// [ancestor]: Self::ancestors
	/// [parent]: Self::parent
	/// [ancestors]: Self::ancestors
	#[inline]
	fn root<'arena>(&self, arena: &'arena Arena<N::Base>) -> RootToken<N>
	where
		for<'base> &'base N: TryFrom<&'base N::Base>,
		for<'base> <&'base N as TryFrom<&'base N::Base>>::Error: Debug,
	{
		token_to_node!(ref, N: self, arena).root(arena)
	}

	/// Returns a reference to the [data] associated with this [node].
	///
	/// [node]: Node
	/// [data]: N::Data
	#[inline]
	fn data<'arena>(&self, arena: &'arena Arena<N::Base>) -> N::DataRef<'arena>
	where
		for<'base> &'base N: TryFrom<&'base N::Base>,
		for<'base> <&'base N as TryFrom<&'base N::Base>>::Error: Debug,
	{
		token_to_node!(ref, N: self, arena).data()
	}

	/// Returns a mutable reference to the [data] associated with this [node].
	///
	/// [node]: DataNode
	/// [data]: N::Data
	#[inline]
	fn data_mut<'arena>(&self, arena: &'arena mut Arena<N::Base>) -> N::DataRefMut<'arena>
	where
		for<'base> &'base mut N: TryFrom<&'base mut N::Base>,
		for<'base> <&'base mut N as TryFrom<&'base mut N::Base>>::Error: Debug,
	{
		token_to_node!(ref mut, N: self, arena).data_mut()
	}

	/// Returns the token of this [node]'s previous sibling.
	///
	/// If this [node] is the first child of its [parent], that means there is no previous sibling
	/// and thus [`None`] is returned.
	///
	/// [node]: LinkedNode
	/// [parent]: Self::parent
	#[inline(always)]
	fn prev(&self, arena: &Arena<N::Base>) -> Option<<N::Base as Node>::Token>
	where
		N::Base: LinkedNode,
	{
		arena.0[self.idx()].prev()
	}
	/// Returns the token of this [node]'s next sibling.
	///
	/// If this [node] is the last child of its [parent], that means there is no next sibling and
	/// thus [`None`] is returned.
	///
	/// [node]: LinkedNode
	/// [parent]: Self::parent
	#[inline(always)]
	fn next(&self, arena: &Arena<N::Base>) -> Option<<N::Base as Node>::Token>
	where
		N::Base: LinkedNode,
	{
		arena.0[self.idx()].next()
	}

	/// Returns an iterator over the tokens of this [node]'s preceding siblings.
	///
	/// This iterator begins with the [previous] node and ends with the [first child] of the
	/// [parent] node.
	///
	/// If this is the [first child], the iterator will be empty.
	///
	/// [node]: Node
	/// [previous]: Self::prev
	/// [first child]: BranchNode::first
	/// [parent]: Self::parent
	#[inline(always)]
	fn preceding_siblings<'arena>(&self, arena: &'arena Arena<N::Base>) -> iter::PrecedingSiblings<'arena, N>
	where
		N::Base: LinkedNode,
	{
		iter::PrecedingSiblings::new(arena, self.prev(arena))
	}

	/// Returns an iterator over the tokens of this [node]'s following siblings.
	///
	/// This iterator begins with the [next] node and ends with the [last child] of the [parent]
	/// node.
	///
	/// If this is the [last child], the iterator will be empty.
	///
	/// [node]: Node
	/// [next]: Self::next
	/// [last child]: BranchNode::last
	/// [parent]: Self::parent
	#[inline(always)]
	fn following_siblings<'arena>(&self, arena: &'arena Arena<N::Base>) -> iter::FollowingSiblings<'arena, N>
	where
		N::Base: LinkedNode,
	{
		iter::FollowingSiblings::new(arena, self.next(arena))
	}

	/// Returns the token of this [branch node]'s first child.
	///
	/// If this [branch node] has no children, [`None`] is returned.
	///
	/// [branch node]: BranchNode
	#[inline]
	fn first(&self, arena: &Arena<N::Base>) -> Option<<N::Base as Node>::Token>
	where
		N: BranchNode,
		for<'base> &'base N: TryFrom<&'base N::Base>,
		for<'base> <&'base N as TryFrom<&'base N::Base>>::Error: Debug,
	{
		token_to_node!(ref, N: self, arena).first()
	}
	/// Returns the token of this [branch node]'s last child.
	///
	/// If this [branch node] has no children, [`None`] is returned.
	///
	/// [branch node]: BranchNode
	#[inline]
	fn last(&self, arena: &Arena<N::Base>) -> Option<<N::Base as Node>::Token>
	where
		N: BranchNode,
		for<'base> &'base N: TryFrom<&'base N::Base>,
		for<'base> <&'base N as TryFrom<&'base N::Base>>::Error: Debug,
	{
		token_to_node!(ref, N: self, arena).last()
	}

	/// Returns an iterator over the tokens of this [branch node]'s children.
	///
	/// [branch node]: BranchNode
	#[inline]
	fn children<'arena>(&self, arena: &'arena Arena<N::Base>) -> N::ChildrenIter<'arena>
	where
		N: BranchNode,
		for<'base> &'base N: TryFrom<&'base N::Base>,
		for<'base> <&'base N as TryFrom<&'base N::Base>>::Error: Debug,
	{
		token_to_node!(ref, N: self, arena).children(arena)
	}

	/// Returns an iterator over the tokens of this [branch node]'s descendants.
	///
	/// This iterator is a breadth-first traversal of the branch node's descendants: its [children]
	/// are done first, then those [children]'s [children] in the same order, and so on and so on.
	///
	/// [branch node]: BranchNode
	/// [children]: Self::children
	#[inline]
	fn descendants<'arena>(&self, arena: &'arena Arena<N::Base>) -> iter::Descendants<'arena, N>
	where
		N: BranchNode,
		for<'base> &'base N: TryFrom<&'base N::Base>,
		for<'base> <&'base N as TryFrom<&'base N::Base>>::Error: Debug,
	{
		iter::Descendants::new(token_to_node!(ref, N: self, arena), arena)
	}

	#[cfg(feature = "deque")]
	/// Returns the number of children this [branch node] has.
	///
	/// [branch node]: BranchNodeDeque
	#[doc(cfg(feature = "deque"))]
	#[inline]
	fn len(&self, arena: &Arena<N::Base>) -> usize
	where
		N: BranchNodeDeque,
		for<'base> &'base N: TryFrom<&'base N::Base>,
		for<'base> <&'base N as TryFrom<&'base N::Base>>::Error: Debug,
	{
		token_to_node!(ref, N: self, arena).len()
	}

	/// Returns whether this [branch node] has no children.
	///
	/// [branch node]: BranchNode
	#[inline]
	fn is_empty(&self, arena: &Arena<N::Base>) -> bool
	where
		N: BranchNode,
		for<'base> &'base N: TryFrom<&'base N::Base>,
		for<'base> <&'base N as TryFrom<&'base N::Base>>::Error: Debug,
	{
		token_to_node!(ref, N: self, arena).is_empty()
	}

	#[cfg(feature = "deque")]
	/// Returns the token of the child at the given `index`.
	///
	/// # Panics
	/// This method will panic if `index` is out of bounds.
	///
	/// # See also
	/// For a fallible version, see [`get`].
	///
	/// [`get`]: Self::get
	#[doc(cfg(feature = "deque"))]
	#[inline]
	fn get_unchecked(&self, arena: &Arena<N::Base>, index: usize) -> <N::Base as Node>::Token
	where
		N: BranchNodeDeque,
		for<'base> &'base N: TryFrom<&'base N::Base>,
		for<'base> <&'base N as TryFrom<&'base N::Base>>::Error: Debug,
	{
		token_to_node!(ref, N: self, arena)[index]
	}

	#[cfg(feature = "deque")]
	/// Returns the token of the child at the given `index`.
	///
	/// If `index` is out of bounds, [`None`] is returned.
	///
	/// # See also
	/// For a version without bounds checks, see [`get_unchecked`].
	///
	/// [`get_unchecked`]: Self::get_unchecked
	#[doc(cfg(feature = "deque"))]
	#[inline]
	fn get(&self, arena: &Arena<N::Base>, index: usize) -> Option<<N::Base as Node>::Token>
	where
		N: BranchNodeDeque,
		for<'base> &'base N: TryFrom<&'base N::Base>,
		for<'base> <&'base N as TryFrom<&'base N::Base>>::Error: Debug,
	{
		token_to_node!(ref, N: self, arena).get(index)
	}

	#[cfg_attrs]
	/// Detaches this branch node's [first child], returning its token.
	///
	/// If this branch node is [empty] then there is no child to detach, so [`None`] is
	/// returned. Otherwise, the child is removed from this branch node's [children], but
	/// remains in the [arena].
	///
	/// This function is useful if you want to move a [node] from one [parent] to another.
	///
	/// # See also
	/// The [last child] may also be detached using [`detach_back`].
	#[configure(
		feature = "deque",
		/// If this is a [`BranchNodeDeque`], children may also be detached by index using
		/// [`detach`].
		///
		/// [`detach`]: BranchNodeDeque::detach
	)]
	/// If you want to remove a child and its descendants from the [arena] altogether, see
	#[configure(
		feature = "deque",
		/// [`pop_front`], [`pop_back`], or, if this is a [`BranchNodeDeque`], [`remove`].
		///
		/// [`remove`]: BranchNodeDeque::remove
	)]
	#[configure(
		not(feature = "deque"),
		/// [`pop_front`] and [`pop_back`].
	)]
	/// [`detach_back`]: Self::detach_back
	///
	/// [`pop_front`]: Self::pop_front
	/// [`pop_back`]: Self::pop_back
	///
	/// [first child]: Self::first
	/// [last child]: Self::last
	/// [empty]: Self::is_empty
	/// [children]: Self::children
	/// [arena]: Arena
	/// [node]: Node
	/// [parent]: Node::parent
	#[inline]
	fn detach_front(&self, arena: &mut Arena<N::Base>) -> Option<<N::Base as Node>::Token>
	where
		N: BranchNode<Token = Self>,
		for<'base> &'base mut N: TryFrom<&'base mut N::Base>,
		for<'base> <&'base mut N as TryFrom<&'base mut N::Base>>::Error: Debug,
	{
		N::detach_front(*self, arena)
	}
	#[cfg_attrs]
	/// Detaches this branch node's [last child], returning its token.
	///
	/// If this branch node is [empty] then there is no child to detach, so [`None`] is
	/// returned. Otherwise, the child is removed from this branch node's [children], but
	/// remains in the [arena].
	///
	/// This function is useful if you want to move a [node] from one [parent] to another.
	///
	/// # See also
	/// The [first child] may also be detached using [`detach_front`].
	#[configure(
		feature = "deque",
		/// If this is a [`BranchNodeDeque`], children may also be detached by index using
		/// [`detach`].
		///
		/// [`detach`]: BranchNodeDeque::detach
	)]
	/// If you want to remove a child and its descendants from the [arena] altogether, see
	#[configure(
		feature = "deque",
		/// [`pop_front`], [`pop_back`], or, if this is a [`BranchNodeDeque`], [`remove`].
		///
		/// [`remove`]: BranchNodeDeque::remove
	)]
	#[configure(
		not(feature = "deque"),
		/// [`pop_front`] and [`pop_back`].
	)]
	/// [`detach_front`]: Self::detach_front
	///
	/// [`pop_front`]: Self::pop_front
	/// [`pop_back`]: Self::pop_back
	/// [`remove`]: BranchNodeDeque::remove
	///
	/// [first child]: Self::first
	/// [last child]: Self::last
	/// [empty]: Self::is_empty
	/// [children]: Self::children
	/// [arena]: Arena
	/// [node]: Node
	/// [parent]: Node::parent
	#[inline]
	fn detach_back(&self, arena: &mut Arena<N::Base>) -> Option<<N::Base as Node>::Token>
	where
		N: BranchNode<Token = Self>,
		for<'base> &'base mut N: TryFrom<&'base mut N::Base>,
		for<'base> <&'base mut N as TryFrom<&'base mut N::Base>>::Error: Debug,
	{
		N::detach_back(*self, arena)
	}

	#[cfg_attrs]
	/// Removes this branch node's [first child].
	///
	/// If this branch node is [empty] then there is no child to remove, so [`None`] is
	/// returned. Otherwise, the removed [node] is returned.
	///
	/// # See also
	/// The [last child] may also be removed using [`pop_back`].
	#[configure(
		feature = "deque",
		/// If this is a [`BranchNodeDeque`], children may also be removed by index using
		/// [`remove`].
		///
		/// [`remove`]: BranchNodeDeque::remove
	)]
	/// If you don't want to remove a child from the [arena], but merely make it a [root node]
	/// or move it to another [parent], see
	#[configure(
		feature = "deque",
		/// [`detach_front`], [`detach_back`], or, if this is a [`BranchNodeDeque`], [`detach`].
		///
		/// [`detach`]: BranchNodeDeque::detach
	)]
	#[configure(
		not(feature = "deque"),
		/// [`detach_front`] and [`detach_back`].
	)]
	/// [`pop_back`]: Self::pop_back
	///
	/// [`detach_front`]: Self::detach_front
	/// [`detach_back`]: Self::detach_back
	///
	/// [first child]: Self::first
	/// [last child]: Self::last
	/// [node]: BaseNode
	/// [empty]: Self::is_empty
	/// [arena]: Arena
	/// [root node]: Self::root
	/// [parent]: Self::parent
	#[inline]
	fn pop_front(&self, arena: &mut Arena<N::Base>) -> Option<<N::Base as BaseNode>::Representation>
	where
		N: BranchNode<Token = Self>,
		for<'base> &'base mut N: TryFrom<&'base mut N::Base>,
		for<'base> <&'base mut N as TryFrom<&'base mut N::Base>>::Error: Debug,
	{
		N::pop_front(*self, arena)
	}
	#[cfg_attrs]
	/// Removes this branch node's [last child].
	///
	/// If this branch node is [empty] then there is no child to remove, so [`None`] is
	/// returned. Otherwise, the removed [node] is returned.
	///
	/// # See also
	/// The [first child] may also be removed using [`pop_front`].
	#[configure(
		feature = "deque",
		/// If this is a [`BranchNodeDeque`], children may also be removed by index using
		/// [`remove`].
		///
		/// [`remove`]: BranchNodeDeque::remove
	)]
	/// If you don't want to remove a child from the [arena], but merely make it a [root node]
	/// or move it to another [parent], see
	#[configure(
		feature = "deque",
		/// [`detach_front`], [`detach_back`], or, if this is a [`BranchNodeDeque`], [`detach`].
		///
		/// [`detach`]: BranchNodeDeque::detach
	)]
	#[configure(
		not(feature = "deque"),
		/// [`detach_front`] and [`detach_back`].
	)]
	/// [`pop_front`]: Self::pop_front
	///
	/// [`detach_front`]: Self::detach_front
	/// [`detach_back`]: Self::detach_back
	///
	/// [first child]: Self::first
	/// [last child]: Self::last
	/// [node]: BaseNode
	/// [empty]: Self::is_empty
	/// [arena]: Arena
	/// [root node]: Self::root
	/// [parent]: Self::parent
	#[inline]
	fn pop_back(&self, arena: &mut Arena<N::Base>) -> Option<<N::Base as BaseNode>::Representation>
	where
		N: BranchNode<Token = Self>,
		for<'base> &'base mut N: TryFrom<&'base mut N::Base>,
		for<'base> <&'base mut N as TryFrom<&'base mut N::Base>>::Error: Debug,
	{
		N::pop_front(*self, arena)
	}

	#[cfg_attrs]
	/// Pushes the given `new` token's [node] to the beginning of this branch node's
	/// [children].
	///
	/// # Panics
	/// This method will panic if the given `new` token refers to either:
	/// - this branch node's [root]; or
	/// - a [node] that already has a [parent].
	///
	/// # See also
	/// A child may also be pushed to the end with [`push_back`].
	#[configure(
		feature = "deque",
		/// If this is a [`BranchNodeDeque`], [children] may also be inserted by index using
		/// [`insert`].
		///
		/// [`insert`]: BranchNodeDeque::insert
	)]
	/// [`push_back`]: Self::push_back
	///
	/// [node]: BaseNode
	/// [children]: Self::children
	/// [root]: Self::root
	/// [parent]: Node::parent
	#[inline]
	fn push_front(&self, arena: &mut Arena<N::Base>, new: <N::Base as Node>::Token)
	where
		N: BranchNode<Token = Self>,
		for<'base> &'base mut N: TryFrom<&'base mut N::Base>,
		for<'base> <&'base mut N as TryFrom<&'base mut N::Base>>::Error: Debug,
	{
		N::push_front(*self, arena, new);
	}
	#[cfg_attrs]
	/// Pushes the given `new` token's [node] to the end of this branch node's [children].
	///
	/// # Panics
	/// This method will panic if the given `new` token refers to either:
	/// - this branch node's [root]; or
	/// - a [node] that already has a [parent].
	///
	/// # See also
	/// A child may also be pushed to the beginning with [`push_front`]. If this is a
	/// [`BranchNodeDeque`], children may also be inserted by index using [`insert`].
	#[configure(
		feature = "deque",
		/// If this is a [`BranchNodeDeque`], [children] may also be inserted by index using
		/// [`insert`].
		///
		/// [`insert`]: BranchNodeDeque::insert
	)]
	/// [`push_front`]: Self::push_front
	///
	/// [node]: BaseNode
	/// [children]: Self::children
	/// [root]: Self::root
	/// [parent]: Node::parent
	#[inline]
	fn push_back(&self, arena: &mut Arena<N::Base>, new: <N::Base as Node>::Token)
	where
		N: BranchNode<Token = Self>,
		for<'base> &'base mut N: TryFrom<&'base mut N::Base>,
		for<'base> <&'base mut N as TryFrom<&'base mut N::Base>>::Error: Debug,
	{
		N::push_back(*self, arena, new);
	}

	#[cfg(feature = "deque")]
	/// Detaches the child at the given `index`, returning its token.
	///
	/// This function is useful if you want to move a [node] from one [parent] to another.
	///
	/// # Panics
	/// This method will panic if the given `index` is out of bounds.
	///
	/// # See also
	/// The [first child] or [last child] may be detached with [`detach_front`] and [`detach_back`]
	/// respectively.
	///
	/// If you want to remove a child and its descendents from the [arena] altogether, see
	/// [`remove`], [`pop_front`], or [`pop_back`].
	///
	/// [`detach_front`]: Self::detach_front
	/// [`detach_back`]: Self::detach_back
	///
	/// [`remove`]: Self::remove
	/// [`pop_front`]: Self::pop_front
	/// [`pop_back`]: Self::pop_back
	///
	/// [node]: Node
	/// [arena]: Arena
	/// [parent]: Node::parent
	/// [first child]: Self::first
	/// [last child]: Self::last
	#[doc(cfg(feature = "deque"))]
	#[inline]
	fn detach(&self, arena: &mut Arena<N::Base>, index: usize) -> <N::Base as Node>::Token
	where
		N: BranchNodeDeque<Token = Self>,
		for<'base> &'base mut N: TryFrom<&'base mut N::Base>,
		for<'base> <&'base mut N as TryFrom<&'base mut N::Base>>::Error: Debug,
	{
		N::detach(*self, arena, index)
	}

	#[cfg(feature = "deque")]
	/// Removes the child at the given `index`.
	///
	/// # Panics
	/// This method will panic if the given `index` is out of bounds.
	///
	/// # See also
	/// The [first child] or [last child] may be removed with [`pop_front`] and [`pop_back`]
	/// respectively.
	///
	/// If you don't want to remove a child from the [arena], but merely make it a [root node] or
	/// move it to another [parent], see [`detach`], [`detach_front`], or [`detach_back`].
	///
	/// [`pop_front`]: Self::pop_front
	/// [`pop_back`]: Self::pop_back
	///
	/// [`detach`]: Self::detach
	/// [`detach_front`]: Self::detach_front
	/// [`detach_back`]: Self::detach_back
	///
	/// [arena]: Arena
	/// [root node]: Self::root
	/// [first child]: Self::first
	/// [last child]: Self::last
	/// [parent]: Node::parent
	#[doc(cfg(feature = "deque"))]
	#[inline]
	fn remove(&self, arena: &mut Arena<N::Base>, index: usize) -> <N::Base as BaseNode>::Representation
	where
		N: BranchNodeDeque<Token = Self>,
		for<'base> &'base mut N: TryFrom<&'base mut N::Base>,
		for<'base> <&'base mut N as TryFrom<&'base mut N::Base>>::Error: Debug,
	{
		N::remove(*self, arena, index)
	}

	#[cfg(feature = "deque")]
	/// Inserts the given `new` token's [node] at the given `index`.
	///
	/// # Panics
	/// This method will panic if:
	/// - the given `index` is greater than the [`len()`]; or
	/// - the given `new` [token] refers to either:
	///   - this branch node's [root]; or
	///   - a [node] that already has a [parent].
	///
	/// # See also
	/// A child can also be pushed to the beginning or end of this [branch node]'s [children] with
	/// [`push_front`] and [`push_back`] respectively.
	///
	/// [branch node]: BranchNodeDeque
	/// [children]: Self::children
	/// [`push_front`]: Self::push_front
	/// [`push_back`]: Self::push_back
	///
	/// [node]: BaseNode
	/// [token]: Self::Token
	/// [root]: Self::root
	/// [parent]: Node::parent
	///
	/// [`len()`]: Self::len
	#[doc(cfg(feature = "deque"))]
	#[inline]
	fn insert(&self, arena: &mut Arena<N::Base>, index: usize, new: <N::Base as Node>::Token)
	where
		N: BranchNodeDeque<Token = Self>,
		for<'base> &'base mut N: TryFrom<&'base mut N::Base>,
		for<'base> <&'base mut N as TryFrom<&'base mut N::Base>>::Error: Debug,
	{
		N::insert(*self, arena, index, new)
	}
}

impl<N: Node> Token<N> {
	/// Creates a new token wrapping the given [arena index].
	///
	/// [arena index]: generational_arena::Index
	#[inline(always)]
	pub(crate) const fn new(idx: generational_arena::Index) -> Self {
		Self {
			idx,
			_marker: PhantomData,
		}
	}
}

impl<N: Node> Idx for Token<N> {
	#[inline(always)]
	fn idx(&self) -> generational_arena::Index {
		self.idx
	}
}

impl<N: Node<Token = Self>> NodeToken<N> for Token<N> {}

/// A node in a tree.
pub trait Node: Debug + Sealed {
	/// The 'base node' used in the [arena].
	///
	/// For typed nodes where there is a typed node type in addition to separate branch and leaf
	/// types, all of those types will use the typed node type as the `ArenaNode`.
	///
	/// [arena]: Arena
	type Base: BaseNode;
	/// The [token] associated with this type of node.
	///
	/// [token]: NodeToken
	type Token: NodeToken<Self>
	where
		Self: Sized;

	/// The custom data associated with this node.
	type Data;
	/// A type acting as a reference to the node's [data].
	///
	/// This is the type returned by [`data`].
	///
	/// [data]: Self::Data
	/// [`data`]: Self::data
	type DataRef<'data>
	where
		Self: 'data;
	/// A type acting as a mutable reference to the node's [data].
	///
	/// This is the type returned by [`data_mut`].
	///
	/// [data]: Self::Data
	/// [`data_mut`]: Self::data_mut
	type DataRefMut<'data>
	where
		Self: 'data;

	/// Creates a new node allocated in the given `arena` using the given `data`.
	///
	/// The node's [token] is returned.
	///
	/// [token]: Token
	fn new(arena: &mut Arena<Self::Base>, data: Self::Data) -> Self::Token
	where
		Self: Sized;

	/// Returns this node's [token].
	///
	/// [token]: Self::Token
	fn token(&self) -> Self::Token
	where
		Self: Sized;

	/// Returns the [token] of this node's parent.
	///
	/// If this node is the root node of its tree, that means it has no parent and thus [`None`] is
	/// returned.
	///
	/// [token]: Token
	fn parent(&self) -> Option<<<Self::Base as BaseNode>::Branch as Node>::Token>;

	/// Returns an iterator over the [tokens] of this node's ancestors.
	///
	/// This iterator begins with the node's [parent] and ends with the [root] node (i.e. the
	/// ancestor with no [parent]).
	///
	/// [tokens]: Self::Token
	/// [parent]: Self::parent
	/// [root]: Self::root
	#[inline(always)]
	fn ancestors<'node>(&'node self, arena: &'node Arena<Self::Base>) -> iter::Ancestors<'node, Self::Base>
	where
		Self: Sized,
	{
		iter::Ancestors::new(arena, self.parent())
	}

	/// Returns this node's root node.
	///
	/// The root node is the most distant [ancestor]; it is the only node in a tree that does not
	/// have a [parent].
	///
	/// If this node is the root node, [`RootToken::This`] is returned. Otherwise,
	/// [`RootToken::Ancestor`] is returned with the root node's token.
	///
	/// Internally, this method iterates over the node's [ancestors] to find the last one, so it is
	/// `O(n)` best, average, and worst case.
	///
	/// [ancestor]: Self::ancestors
	/// [parent]: Self::parent
	/// [ancestors]: Self::ancestors
	#[inline(always)]
	fn root<'node>(&'node self, arena: &'node Arena<Self::Base>) -> RootToken<Self>
	where
		Self: Sized,
	{
		match self.ancestors(arena).last() {
			Some(root) => RootToken::Ancestor(root),
			None => RootToken::This(self.token()),
		}
	}

	/// Returns a reference to the [data] associated with this node.
	///
	/// [data]: Self::Data
	fn data(&self) -> Self::DataRef<'_>;

	/// Returns a mutable reference to the [data] associated with this node.
	///
	/// [data]: Self::Data
	fn data_mut(&mut self) -> Self::DataRefMut<'_>;
}

#[derive(Debug)]
pub enum RootToken<N: Node> {
	Ancestor(<<N::Base as BaseNode>::Branch as Node>::Token),
	This(N::Token),
}

impl<N: Node> Idx for RootToken<N> {
	#[inline(always)]
	fn idx(&self) -> generational_arena::Index {
		match self {
			Self::Ancestor(token) => token.idx(),
			Self::This(token) => token.idx(),
		}
	}
}

impl<N: Node, I: Idx> PartialEq<I> for RootToken<N> {
	fn eq(&self, other: &I) -> bool {
		self.idx() == other.idx()
	}
}

impl<N: Node> Eq for RootToken<N> {}

impl<N: Node> Hash for RootToken<N> {
	#[inline(always)]
	fn hash<H: Hasher>(&self, state: &mut H) {
		self.idx().hash(state);
	}
}

impl<N: Node> Copy for RootToken<N> {}

impl<N: Node> Clone for RootToken<N> {
	fn clone(&self) -> Self {
		*self
	}
}

impl<N: Node> NodeToken<N::Base> for RootToken<N> {}

pub trait BaseNode: Node<Base = Self> {
	/// The representation of the node after it is removed from the [arena].
	///
	/// If this is a [branch node], this representation should include its children.
	///
	/// [arena]: Arena
	/// [branch node]: BranchNode
	type Representation;

	/// The type used for branch nodes.
	///
	/// This may be the base node itself, or a separate node type.
	type Branch: BranchNode;
	/// The type used for leaf nodes.
	///
	/// This may be the base node itself, or a separate node type.
	type Leaf: Node;

	/// Converts this node into its [representation].
	///
	/// If this is a [branch node], this node's children should be removed from the `arena` and
	/// included in the [representation].
	///
	/// [representation]: Self::Representation
	/// [branch node]: BranchNode
	fn into_representation(self, arena: &mut Arena<Self>) -> Self::Representation
	where
		Self: Sized;
}

#[cfg(feature = "deque")]
/// Removes the given children from the [branch node], returning their [representations].
///
/// [branch node]: BranchNodeDeque
/// [representations]: BaseNode::Representation
#[doc(cfg(feature = "deque"))]
pub(crate) fn remove_children_deque<Branch: BranchNodeDeque>(
	branch: &Branch,
	arena: &mut Arena<Branch::Base>,
) -> VecDeque<<Branch::Base as BaseNode>::Representation> {
	let mut children = VecDeque::new();

	for i in 0..branch.len() {
		children.push_back(
			arena
				.0
				.remove(branch[i].idx())
				.expect("tried to remove child but there was no such node in the `arena`")
				.into_representation(arena),
		);
	}

	children
}

/// Removes the given children from the [branch node], returning their [representations].
///
/// [branch node]: BranchNode
/// [representations]: BaseNode::Representation
pub(crate) fn remove_children_linked<Branch: BranchNode>(
	branch: &Branch,
	arena: &mut Arena<Branch::Base>,
) -> VecDeque<<Branch::Base as BaseNode>::Representation>
where
	Branch::Base: LinkedNode,
{
	let mut children = VecDeque::new();
	let mut child = branch.first();

	while let Some(token) = &child {
		let removed = arena
			.0
			.remove(token.idx())
			.expect("tried to remove child but there was no such node in the `arena`");

		child = removed.next();
		children.push_back(removed.into_representation(arena));
	}

	children
}

/// A [node] that is linked to its [previous] and [next] siblings.
///
/// [node]: Node
/// [previous]: Self::prev
/// [next]: Self::next
pub trait LinkedNode: Node
where
	Self::Base: LinkedNode,
{
	/// Returns the [token] of this node's previous sibling.
	///
	/// If this node is the first child of its [parent], that means there is no previous sibling and
	/// thus [`None`] is returned.
	///
	/// [token]: Self::Token
	/// [parent]: Self::parent
	fn prev(&self) -> Option<<Self::Base as Node>::Token>;
	/// Returns the [token] of this node's next sibling.
	///
	/// If this node is the last child of its [parent], that means there is no next sibling and thus
	/// [`None`] is returned.
	///
	/// [token]: Self::Token
	/// [parent]: Self::parent
	fn next(&self) -> Option<<Self::Base as Node>::Token>;

	/// Returns an iterator over the [tokens] of this node's preceding siblings.
	///
	/// This iterator begins with the [previous] node and ends with the [first child] of the
	/// [parent] node.
	///
	/// If this is the [first child], the iterator will be empty.
	///
	/// [tokens]: Self::Token
	/// [previous]: Self::prev
	/// [first child]: BranchNode::first
	/// [parent]: Self::parent
	fn preceding_siblings<'node>(&'node self, arena: &'node Arena<Self::Base>) -> iter::PrecedingSiblings<'node, Self>
	where
		Self: Sized,
	{
		iter::PrecedingSiblings::new(arena, self.prev())
	}
	/// Returns an iterator over the [tokens] of this node's following siblings.
	///
	/// This iterator begins with the [next] node and ends with the [last child] of the [parent]
	/// node.
	///
	/// If this is the [last child], the iterator will be empty.
	///
	/// [tokens]: Self::Token
	/// [next]: Self::next
	/// [last child]: BranchNode::last
	/// [parent]: Self::parent
	#[inline(always)]
	fn following_siblings<'node>(&'node self, arena: &'node Arena<Self::Base>) -> iter::FollowingSiblings<'node, Self>
	where
		Self: Sized,
	{
		iter::FollowingSiblings::new(arena, self.next())
	}
}

#[cfg_attrs]
/// A [node] that can have [children].
///
/// [node]: Node
/// [children]: Self::children
pub trait BranchNode: Node {
	/// The iterator returned by [`children`].
	///
	/// [`children`]: Self::children
	type ChildrenIter<'branch>: DoubleEndedIterator<Item = <Self::Base as Node>::Token> + FusedIterator
	where
		Self: 'branch;

	/// Returns the [token] of this branch node's first child.
	///
	/// If this branch node has no children, [`None`] is returned.
	///
	/// [token]: Self::Token
	fn first(&self) -> Option<<Self::Base as Node>::Token>;
	/// Returns the [token] of this branch node's last child.
	///
	/// If this branch node has no children, [`None`] is returned.
	///
	/// [token]: Self::Token
	fn last(&self) -> Option<<Self::Base as Node>::Token>;

	// TODO: Should there be a separate version for linked branch nodes and `BranchNodeDeque`s? For
	//     : `BranchNodeDeque`s, `arena` is not necessary.
	/// Returns an iterator over the [tokens] of this branch node's children.
	///
	/// [tokens]: Self::Token
	fn children<'branch>(&'branch self, arena: &'branch Arena<Self::Base>) -> Self::ChildrenIter<'branch>;

	/// Returns an iterator over the [tokens] of this branch node's descendants.
	///
	/// This iterator is a breadth-first traversal of the branch node's descendants: its [children]
	/// are done first, then those [children]'s [children] in the same order, and so on and so on.
	///
	/// [tokens]: Node::Token
	/// [children]: Self::children
	#[inline(always)]
	fn descendants<'branch>(&'branch self, arena: &'branch Arena<Self::Base>) -> iter::Descendants<'branch, Self>
	where
		Self: Sized,
		for<'base> &'base Self: TryFrom<&'base Self::Base>,
	{
		iter::Descendants::new(self, arena)
	}

	/// Returns whether this branch node has no children.
	fn is_empty(&self) -> bool;

	/// Detaches this branch node's [first child], returning its [token].
	///
	/// If this branch node is [empty] then there is no child to detach, so [`None`] is
	/// returned. Otherwise, the child is removed from this branch node's [children], but
	/// remains in the [arena].
	///
	/// This function is useful if you want to move a [node] from one [parent] to another.
	///
	/// This function takes its [token] as a parameter instead of `&mut self` as a receiver to
	/// avoid having multiple mutable references into the arena at a time.
	///
	/// # See also
	/// The [last child] may also be detached using [`detach_back`].
	#[configure(
		feature = "deque",
		/// If this is a [`BranchNodeDeque`], children may also be detached by index using
		/// [`detach`].
		///
		/// [`detach`]: BranchNodeDeque::detach
	)]
	/// If you want to remove a child and its descendants from the [arena] altogether, see
	#[configure(
		feature = "deque",
		/// [`pop_front`], [`pop_back`], or, if this is a [`BranchNodeDeque`], [`remove`].
		///
		/// [`remove`]: BranchNodeDeque::remove
	)]
	#[configure(
		not(feature = "deque"),
		/// [`pop_front`] and [`pop_back`].
	)]
	/// [`detach_back`]: Self::detach_back
	///
	/// [`pop_front`]: Self::pop_front
	/// [`pop_back`]: Self::pop_back
	///
	/// [first child]: Self::first
	/// [last child]: Self::last
	/// [token]: Node::Token
	/// [empty]: Self::is_empty
	/// [children]: Self::children
	/// [arena]: Arena
	/// [node]: Node
	/// [parent]: Node::parent
	fn detach_front(token: Self::Token, arena: &mut Arena<Self::Base>) -> Option<<Self::Base as Node>::Token>
	where
		Self: Sized,
		for<'base> &'base mut Self: TryFrom<&'base mut Self::Base>,
		for<'base> <&'base mut Self as TryFrom<&'base mut Self::Base>>::Error: Debug;
	/// Detaches this branch node's [last child], returning its [token].
	///
	/// If this branch node is [empty] then there is no child to detach, so [`None`] is
	/// returned. Otherwise, the child is removed from this branch node's [children], but
	/// remains in the [arena].
	///
	/// This function is useful if you want to move a [node] from one [parent] to another.
	///
	/// This function takes its [token] as a parameter instead of `&mut self` as a receiver to
	/// avoid having multiple mutable references into the arena at a time.
	///
	/// # See also
	/// The [first child] may also be detached using [`detach_front`].
	#[configure(
		feature = "deque",
		/// If this is a [`BranchNodeDeque`], children may also be detached by index using
		/// [`detach`].
		///
		/// [`detach`]: BranchNodeDeque::detach
	)]
	/// If you want to remove a child and its descendants from the [arena] altogether, see
	#[configure(
		feature = "deque",
		/// [`pop_front`], [`pop_back`], or, if this is a [`BranchNodeDeque`], [`remove`].
		///
		/// [`remove`]: BranchNodeDeque::remove
	)]
	#[configure(
		not(feature = "deque"),
		/// [`pop_front`] and [`pop_back`].
	)]
	/// [`detach_front`]: Self::detach_front
	///
	/// [`pop_front`]: Self::pop_front
	/// [`pop_back`]: Self::pop_back
	/// [`remove`]: BranchNodeDeque::remove
	///
	/// [first child]: Self::first
	/// [last child]: Self::last
	/// [token]: Node::Token
	/// [empty]: Self::is_empty
	/// [children]: Self::children
	/// [arena]: Arena
	/// [node]: Node
	/// [parent]: Node::parent
	fn detach_back(token: Self::Token, arena: &mut Arena<Self::Base>) -> Option<<Self::Base as Node>::Token>
	where
		Self: Sized,
		for<'base> &'base mut Self: TryFrom<&'base mut Self::Base>,
		for<'base> <&'base mut Self as TryFrom<&'base mut Self::Base>>::Error: Debug;

	/// Removes this branch node's [first child].
	///
	/// If this branch node is [empty] then there is no child to remove, so [`None`] is
	/// returned. Otherwise, the removed [node] is returned.
	///
	/// This function takes its [token] as a parameter instead of `&mut self` as a receiver to
	/// avoid having multiple mutable references into the arena at a time.
	///
	/// # See also
	/// The [last child] may also be removed using [`pop_back`].
	#[configure(
		feature = "deque",
		/// If this is a [`BranchNodeDeque`], children may also be removed by index using
		/// [`remove`].
		///
		/// [`remove`]: BranchNodeDeque::remove
	)]
	/// If you don't want to remove a child from the [arena], but merely make it a [root node]
	/// or move it to another [parent], see
	#[configure(
		feature = "deque",
		/// [`detach_front`], [`detach_back`], or, if this is a [`BranchNodeDeque`], [`detach`].
		///
		/// [`detach`]: BranchNodeDeque::detach
	)]
	#[configure(
		not(feature = "deque"),
		/// [`detach_front`] and [`detach_back`].
	)]
	/// [`pop_back`]: Self::pop_back
	///
	/// [`detach_front`]: Self::detach_front
	/// [`detach_back`]: Self::detach_back
	///
	/// [first child]: Self::first
	/// [last child]: Self::last
	/// [node]: BaseNode
	/// [empty]: Self::is_empty
	/// [token]: Self::Token
	/// [arena]: Arena
	/// [root node]: Self::root
	/// [parent]: Self::parent
	fn pop_front(token: Self::Token, arena: &mut Arena<Self::Base>) -> Option<<Self::Base as BaseNode>::Representation>
	where
		Self: Sized,
		for<'base> &'base mut Self: TryFrom<&'base mut Self::Base>,
		for<'base> <&'base mut Self as TryFrom<&'base mut Self::Base>>::Error: Debug;
	/// Removes this branch node's [last child].
	///
	/// If this branch node is [empty] then there is no child to remove, so [`None`] is
	/// returned. Otherwise, the removed [node] is returned.
	///
	/// This function takes its [token] as a parameter instead of `&mut self` as a receiver
	/// avoid having multiple mutable references into the arena at a time.
	///
	/// # See also
	/// The [first child] may also be removed using [`pop_front`].
	#[configure(
		feature = "deque",
		/// If this is a [`BranchNodeDeque`], children may also be removed by index using
		/// [`remove`].
		///
		/// [`remove`]: BranchNodeDeque::remove
	)]
	/// If you don't want to remove a child from the [arena], but merely make it a [root node]
	/// or move it to another [parent], see
	#[configure(
		feature = "deque",
		/// [`detach_front`], [`detach_back`], or, if this is a [`BranchNodeDeque`], [`detach`].
		///
		/// [`detach`]: BranchNodeDeque::detach
	)]
	#[configure(
		not(feature = "deque"),
		/// [`detach_front`] and [`detach_back`].
	)]
	/// [`pop_front`]: Self::pop_front
	///
	/// [`detach_front`]: Self::detach_front
	/// [`detach_back`]: Self::detach_back
	///
	/// [first child]: Self::first
	/// [last child]: Self::last
	/// [node]: BaseNode
	/// [empty]: Self::is_empty
	/// [token]: Self::Token
	/// [arena]: Arena
	/// [root node]: Self::root
	/// [parent]: Self::parent
	fn pop_back(token: Self::Token, arena: &mut Arena<Self::Base>) -> Option<<Self::Base as BaseNode>::Representation>
	where
		Self: Sized,
		for<'base> &'base mut Self: TryFrom<&'base mut Self::Base>,
		for<'base> <&'base mut Self as TryFrom<&'base mut Self::Base>>::Error: Debug;

	/// Pushes the given `new` [token]'s [node] to the beginning of this branch node's
	/// [children].
	///
	/// This function takes its [token] as a parameter instead of `&mut self` as a receiver to
	/// avoid having multiple mutable references into the arena at a time.
	///
	/// # Panics
	/// This method will panic if the given `new` [token] refers to either:
	/// - this branch node's [root]; or
	/// - a [node] that already has a [parent].
	///
	/// # See also
	/// A child may also be pushed to the end with [`push_back`].
	#[configure(
		feature = "deque",
		/// If this is a [`BranchNodeDeque`], [children] may also be inserted by index using
		/// [`insert`].
		///
		/// [`insert`]: BranchNodeDeque::insert
	)]
	/// [`push_back`]: Self::push_back
	///
	/// [token]: Self::Token
	/// [node]: BaseNode
	/// [children]: Self::children
	/// [root]: Self::root
	/// [parent]: Node::parent
	fn push_front(token: Self::Token, arena: &mut Arena<Self::Base>, new: <Self::Base as Node>::Token)
	where
		Self: Sized,
		for<'base> &'base mut Self: TryFrom<&'base mut Self::Base>,
		for<'base> <&'base mut Self as TryFrom<&'base mut Self::Base>>::Error: Debug;
	/// Pushes the given `new` [token]'s [node] to the end of this branch node's [children].
	///
	/// This function takes its [token] as a parameter instead of `&mut self` as a receiver to
	/// avoid having multiple mutable references into the arena at a time.
	///
	/// # Panics
	/// This method will panic if the given `new` [token] refers to either:
	/// - this branch node's [root]; or
	/// - a [node] that already has a [parent].
	///
	/// # See also
	/// A child may also be pushed to the beginning with [`push_front`]. If this is a
	/// [`BranchNodeDeque`], children may also be inserted by index using [`insert`].
	#[configure(
		feature = "deque",
		/// If this is a [`BranchNodeDeque`], [children] may also be inserted by index using
		/// [`insert`].
		///
		/// [`insert`]: BranchNodeDeque::insert
	)]
	/// [`push_front`]: Self::push_front
	///
	/// [token]: Self::Token
	/// [node]: BaseNode
	/// [children]: Self::children
	/// [root]: Self::root
	/// [parent]: Node::parent
	fn push_back(token: Self::Token, arena: &mut Arena<Self::Base>, new: <Self::Base as Node>::Token)
	where
		Self: Sized,
		for<'base> &'base mut Self: TryFrom<&'base mut Self::Base>,
		for<'base> <&'base mut Self as TryFrom<&'base mut Self::Base>>::Error: Debug;
}

#[cfg(feature = "deque")]
/// A [node] that stores its [children] as a [`VecDeque`], allowing them to be [indexed].
///
/// [node]: Node
/// [children]: Self::children
///
/// [indexed]: Index
#[doc(cfg(feature = "deque"))]
pub trait BranchNodeDeque: BranchNode
where
	Self: Index<usize, Output = <Self::Base as Node>::Token> + Sized,
{
	/// Returns the number of children this branch node has.
	fn len(&self) -> usize;

	/// Returns the [token] of the child at the given `index`.
	///
	/// If `index` is out of bounds, [`None`] is returned.
	///
	/// [token]: Node::Token
	#[inline]
	fn get(&self, index: usize) -> Option<<Self::Base as Node>::Token> {
		if index < self.len() {
			Some(self[index])
		} else {
			None
		}
	}

	/// Detaches the child at the given `index`, returning its [token].
	///
	/// This function is useful if you want to move a [node] from one [parent] to another.
	///
	/// This function takes its [token] as a parameter instead of `&mut self` as a receiver to avoid
	/// having multiple mutable references into the [arena] at a time.
	///
	/// # Panics
	/// This method will panic if the given `index` is out of bounds.
	///
	/// # See also
	/// The [first child] or [last child] may be detached with [`detach_front`] and [`detach_back`]
	/// respectively.
	///
	/// If you want to remove a child and its descendents from the [arena] altogether, see
	/// [`remove`], [`pop_front`], or [`pop_back`].
	///
	/// [`detach_front`]: Self::detach_front
	/// [`detach_back`]: Self::detach_back
	///
	/// [`remove`]: Self::remove
	/// [`pop_front`]: Self::pop_front
	/// [`pop_back`]: Self::pop_back
	///
	/// [token]: Node::Token
	/// [node]: Node
	/// [arena]: Arena
	/// [parent]: Node::parent
	/// [first child]: Self::first
	/// [last child]: Self::last
	fn detach(token: Self::Token, arena: &mut Arena<Self::Base>, index: usize) -> <Self::Base as Node>::Token
	where
		Self: Sized,
		for<'base> &'base mut Self: TryFrom<&'base mut Self::Base>,
		for<'base> <&'base mut Self as TryFrom<&'base mut Self::Base>>::Error: Debug;

	/// Removes the child at the given `index`.
	///
	/// This function takes its [token] as a parameter instead of `&mut self` as a receiver to avoid
	/// having multiple mutable references into the [arena] at a time.
	///
	/// # Panics
	/// This method will panic if the given `index` is out of bounds.
	///
	/// # See also
	/// The [first child] or [last child] may be removed with [`pop_front`] and [`pop_back`]
	/// respectively.
	///
	/// If you don't want to remove a child from the [arena], but merely make it a [root node] or
	/// move it to another [parent], see [`detach`], [`detach_front`], or [`detach_back`].
	///
	/// [`pop_front`]: Self::pop_front
	/// [`pop_back`]: Self::pop_back
	///
	/// [`detach`]: Self::detach
	/// [`detach_front`]: Self::detach_front
	/// [`detach_back`]: Self::detach_back
	///
	/// [token]: Self::Token
	/// [arena]: Arena
	/// [root node]: Self::root
	/// [first child]: Self::first
	/// [last child]: Self::last
	/// [parent]: Node::parent
	fn remove(
		token: Self::Token,
		arena: &mut Arena<Self::Base>,
		index: usize,
	) -> <Self::Base as BaseNode>::Representation
	where
		Self: Sized,
		for<'base> &'base mut Self: TryFrom<&'base mut Self::Base>,
		for<'base> <&'base mut Self as TryFrom<&'base mut Self::Base>>::Error: Debug;

	/// Inserts the given `new` [token]'s [node] at the given `index`.
	///
	/// This function takes its [token] as a parameter instead of `&mut self` as a receiver to avoid
	/// having multiple mutable references into the arena at a time.
	///
	/// # Panics
	/// This method will panic if:
	/// - the given `index` is greater than the [`len()`]; or
	/// - the given `new` [token] refers to either:
	///   - this branch node's [root]; or
	///   - a [node] that already has a [parent].
	///
	/// # See also
	/// A child can also be pushed to the beginning or end of this branch node's [children] with
	/// [`push_front`] and [`push_back`] respectively.
	///
	/// [children]: Self::children
	/// [`push_front`]: Self::push_front
	/// [`push_back`]: Self::push_back
	///
	/// [node]: BaseNode
	/// [token]: Self::Token
	/// [root]: Self::root
	/// [parent]: Node::parent
	///
	/// [`len()`]: Self::len
	fn insert(token: Self::Token, arena: &mut Arena<Self::Base>, index: usize, new: <Self::Base as Node>::Token)
	where
		Self: Sized,
		for<'base> &'base mut Self: TryFrom<&'base mut Self::Base>,
		for<'base> <&'base mut Self as TryFrom<&'base mut Self::Base>>::Error: Debug;
}

/// An arena in which nodes are allocated.
///
/// # Examples
/// TODO
#[derive(Debug, Default)]
pub struct Arena<N: Node>(pub(crate) generational_arena::Arena<N>);

impl<N: Node> Arena<N> {
	/// Creates a new, empty arena.
	pub fn new() -> Self {
		Self(generational_arena::Arena::new())
	}

	/// Creates a new, empty arena with the given initial `capacity`.
	///
	/// A number of nodes equal to the `capacity` may be allocated in the arena without allocating
	/// further memory for the arena itself.
	pub fn with_capacity(capacity: usize) -> Self {
		Self(generational_arena::Arena::with_capacity(capacity))
	}
}

#[cfg(feature = "serde")]
impl<N: Node + Serialize> Serialize for Arena<N> {
	#[inline(always)]
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: Serializer,
	{
		self.0.serialize(serializer)
	}
}

#[cfg(feature = "serde")]
impl<'de, N: Node + Deserialize<'de>> Deserialize<'de> for Arena<N> {
	#[inline(always)]
	fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
	where
		D: Deserializer<'de>,
	{
		Ok(Self(generational_arena::Arena::deserialize(deserializer)?))
	}
}
