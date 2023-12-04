// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::{
	collections::{vec_deque, VecDeque},
	fmt::Debug,
	hash::{Hash, Hasher},
	iter::Copied,
	ops::Index,
};

use cfg_attrs::cfg_attrs;

use super::*;
use crate::{
	remove_children_deque,
	sealed::{Idx, Sealed},
	Arena,
	BaseNode,
	BranchNode,
	BranchNodeDeque,
	Node,
	Token,
};

#[cfg_attrs]
/// A [node] that is _not_ split into separate [branch] and leaf [nodes].
///
/// This is the [deque] version, where [children] are represented as a [`VecDeque`]. In this
/// version, a [node]'s [previous sibling][prev][(s)][preceding] and
/// [next sibling][next][(s)][following] are not available, but [nodes] can be [directly indexed],
/// and [children] can be [detached], [removed], or [inserted] by index.
///
/// `Data` represents the [custom data] associated with the node.
///
/// # See also
/// For the non-[deque] version, see [`UnifiedNode`].
#[configure(
	feature = "split",
	/// For a [node] that _is_ split into separate [branch] and leaf [nodes], see [`SplitNode`]
	/// and [`SplitNodeDeque`].
	///
	/// [`SplitNode`]: crate::split::SplitNode
	/// [`SplitNodeDeque`]: crate::split::SplitNodeDeque
	///
)]
/// [node]: Node
/// [nodes]: Node
/// [branch]: BranchNode
/// [custom data]: UnifiedNodeDeque::Data
///
/// [deque]: BranchNodeDeque
/// [children]: UnifiedNodeDeque::children
/// [directly indexed]: UnifiedNodeDeque::index
/// [detached]: UnifiedNodeDeque::detach
/// [removed]: UnifiedNodeDeque::remove
/// [inserted]: UnifiedNodeDeque::insert
///
/// [prev]: crate::LinkedNode::prev
/// [preceding]: crate::LinkedNode::preceding_siblings
/// [next]: crate::LinkedNode::next
/// [following]: crate::LinkedNode::following_siblings
#[derive(Debug)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct UnifiedNodeDeque<Data: Debug> {
	token: Token<Self>,

	parent: Option<Token<Self>>,
	children: VecDeque<Token<Self>>,

	data: Data,
}

impl<Data: Debug> PartialEq for UnifiedNodeDeque<Data> {
	#[inline(always)]
	fn eq(&self, other: &Self) -> bool {
		self.token == other.token
	}
}

impl<Data: Debug> Eq for UnifiedNodeDeque<Data> {}

impl<Data: Debug> Hash for UnifiedNodeDeque<Data> {
	#[inline(always)]
	fn hash<H: Hasher>(&self, state: &mut H) {
		self.token.hash(state);
	}
}

impl<Data: Debug> Sealed for UnifiedNodeDeque<Data> {}

impl<Data: Debug> Node for UnifiedNodeDeque<Data> {
	type Base = Self;
	type Token = Token<Self>;

	type Data = Data;
	type DataRef<'data> = &'data Data
	where
		Self: 'data;
	type DataRefMut<'data> = &'data mut Data
	where
		Self: 'data;

	fn new(arena: &mut Arena<Self::Base>, data: Self::Data) -> Self::Token
	where
		Self: Sized,
	{
		Token::new(arena.0.insert_with(|idx| Self {
			token: Token::new(idx),

			parent: None,
			children: VecDeque::new(),

			data,
		}))
	}

	#[inline(always)]
	fn token(&self) -> Self::Token {
		self.token
	}

	#[inline(always)]
	fn parent(&self) -> Option<Token<Self>> {
		self.parent
	}

	#[inline(always)]
	fn data(&self) -> &Self::Data {
		&self.data
	}

	#[inline(always)]
	fn data_mut(&mut self) -> &mut Self::Data {
		&mut self.data
	}
}

impl<Data: Debug> BaseNode for UnifiedNodeDeque<Data> {
	type Representation = UnifiedNodeRepresentation<Data>;

	type Branch = Self;
	type Leaf = Self;

	fn into_representation(self, arena: &mut Arena<Self>) -> Self::Representation {
		UnifiedNodeRepresentation {
			children: remove_children_deque(&self, arena),
			data: self.data,
		}
	}
}

impl<Data: Debug> BranchNode for UnifiedNodeDeque<Data> {
	type ChildrenIter<'branch> = Copied<vec_deque::Iter<'branch, Token<Self>>>
	where
		Self: 'branch;

	#[inline]
	fn first(&self) -> Option<Token<Self>> {
		match self.children.len() {
			0 => None,
			_ => Some(self.children[0]),
		}
	}

	#[inline]
	fn last(&self) -> Option<Token<Self>> {
		match self.children.len() {
			0 => None,
			len => Some(self.children[len - 1]),
		}
	}

	#[inline(always)]
	fn children<'branch>(&'branch self, _arena: &'branch Arena<Self::Base>) -> Self::ChildrenIter<'branch> {
		self.children.iter().copied()
	}

	#[inline(always)]
	fn is_empty(&self) -> bool {
		self.children.is_empty()
	}

	fn detach_front(token: Self::Token, arena: &mut Arena<Self::Base>) -> Option<Token<Self>> {
		let child = arena.0[token.idx()].children.pop_front();

		if let Some(child) = &child {
			let node = &mut arena.0[child.idx()];

			// Detach the node's parent.
			node.parent = None;
		}

		child
	}

	fn detach_back(token: Self::Token, arena: &mut Arena<Self::Base>) -> Option<Token<Self>> {
		let child = arena.0[token.idx()].children.pop_back();

		if let Some(child) = &child {
			let node = &mut arena.0[child.idx()];

			// Detach the node's parent.
			node.parent = None;
		}

		child
	}

	fn pop_front(token: Self::Token, arena: &mut Arena<Self::Base>) -> Option<UnifiedNodeRepresentation<Data>> {
		let child = arena.0[token.idx()].children.pop_front();

		child.map(|child| {
			arena
				.0
				.remove(child.idx())
				.expect("tried to remove child but there was no such node in the `arena`")
				.into_representation(arena)
		})
	}

	fn pop_back(token: Self::Token, arena: &mut Arena<Self::Base>) -> Option<UnifiedNodeRepresentation<Data>> {
		let child = arena.0[token.idx()].children.pop_back();

		child.map(|child| {
			arena
				.0
				.remove(child.idx())
				.expect("tried to remove child but there was no such node in the `arena`")
				.into_representation(arena)
		})
	}

	fn push_front(token: Self::Token, arena: &mut Arena<Self::Base>, new: Token<Self>) {
		// We're not pushing our own root...
		assert_ne!(
			arena.0[token.idx()].root(arena),
			new,
			"tried to push this branch's own root as a child"
		);
		// And we're not pushing a child that already has a parent...
		assert!(
			arena.0[new.idx()].parent.is_none(),
			"tried to push a child that already has a parent"
		);

		// Set the child's parent.
		arena.0[new.idx()].parent = Some(token);

		// Push the child's token.
		arena.0[token.idx()].children.push_front(new);
	}

	fn push_back(token: Self::Token, arena: &mut Arena<Self::Base>, new: Token<Self>) {
		// We're not pushing our own root...
		assert_ne!(
			arena.0[token.idx()].root(arena),
			new,
			"tried to push this branch's own root as a child"
		);
		// And we're not pushing a child that already has a parent...
		assert!(
			arena.0[new.idx()].parent.is_none(),
			"tried to push a child that already has a parent"
		);

		// Set the child's parent.
		arena.0[new.idx()].parent = Some(token);

		// Push the child's token.
		arena.0[token.idx()].children.push_back(new);
	}
}

impl<Data: Debug> Index<usize> for UnifiedNodeDeque<Data> {
	type Output = Token<Self>;

	#[inline(always)]
	fn index(&self, index: usize) -> &Self::Output {
		&self.children[index]
	}
}

impl<Data: Debug> BranchNodeDeque for UnifiedNodeDeque<Data> {
	#[inline(always)]
	fn len(&self) -> usize {
		self.children.len()
	}

	fn detach(token: Self::Token, arena: &mut Arena<Self>, index: usize) -> Token<Self> {
		let children = &mut arena.0[token.idx()].children;

		let child = children.remove(index).unwrap_or_else(|| {
			panic!(
				"the given `index` ({index}) was out of bounds; there were only {} children",
				children.len()
			)
		});

		// Detach the child's parent.
		arena.0[child.idx()].parent = None;

		child
	}

	fn remove(token: Self::Token, arena: &mut Arena<Self>, index: usize) -> UnifiedNodeRepresentation<Data> {
		let children = &mut arena.0[token.idx()].children;

		let child = children.remove(index).unwrap_or_else(|| {
			panic!(
				"the given `index` ({index}) was out of bounds; there were only {} children",
				children.len()
			)
		});

		arena
			.0
			.remove(child.idx())
			.expect("tried to remove child but there was no such node in `arena`")
			.into_representation(arena)
	}

	fn insert(token: Self::Token, arena: &mut Arena<Self>, index: usize, new: Token<Self>) {
		// We're not inserting our own root...
		assert_ne!(
			arena.0[token.idx()].root(arena),
			new,
			"tried to insert this branch's own root as a child"
		);
		// And we're not inserting a child that already has a parent...
		assert!(
			arena.0[new.idx()].parent.is_none(),
			"tried to insert a child that already has a parent"
		);

		// Set the child's parent.
		arena.0[new.idx()].parent = Some(token);

		// Insert the child's token.
		arena.0[token.idx()].children.insert(index, new);
	}
}
