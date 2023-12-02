// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::{
	collections::VecDeque,
	fmt::Debug,
	hash::{Hash, Hasher},
};

use crate::{
	iter,
	remove_children_linked,
	sealed::{Idx, Sealed},
	Arena,
	BaseNode,
	BranchNode,
	LinkedNode,
	Node,
	NodeToken,
	Token,
};

mod deque;

/// A [node] that is _not_ split into separate [branch] and leaf [nodes].
///
/// [`Data`] represents the [custom data] associated with the the node.
///
/// For a [node] that _is_ split into separate [branch] and leaf [nodes], see [`SplitNode`].
///
/// [`SplitNode`]: crate::split::SplitNode
///
/// [node]: Node
/// [nodes]: Node
/// [branch]: BranchNode
/// [custom data]: UnifiedNode::Data
#[derive(Debug)]
pub struct UnifiedNode<Data: Debug> {
	token: Token<Self>,

	parent: Option<Token<Self>>,

	prev: Option<Token<Self>>,
	next: Option<Token<Self>>,

	first_child: Option<Token<Self>>,
	last_child: Option<Token<Self>>,

	data: Data,
}

/// The [representation] of a [unified node] after it has been removed from the [arena].
///
/// [representation]: BaseNode::Representation
/// [unified node]: UnifiedNode
/// [arena]: Arena
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct UnifiedNodeRepresentation<Data: Debug> {
	/// The [node]'s [children].
	///
	/// [node]: UnifiedNode
	/// [children]: UnifiedNode::children
	pub children: VecDeque<UnifiedNodeRepresentation<Data>>,
	/// The [data] associated with the [node].
	///
	/// [node]: UnifiedNode
	/// [data]: UnifiedNode::Data
	pub data: Data,
}

impl<Data: Debug> PartialEq for UnifiedNode<Data> {
	#[inline(always)]
	fn eq(&self, other: &Self) -> bool {
		self.token == other.token
	}
}

impl<Data: Debug> Eq for UnifiedNode<Data> {}

impl<Data: Debug> Hash for UnifiedNode<Data> {
	#[inline(always)]
	fn hash<H: Hasher>(&self, state: &mut H) {
		self.token.hash(state);
	}
}

impl<Data: Debug> Sealed for UnifiedNode<Data> {}

impl<Data: Debug> Node for UnifiedNode<Data> {
	type Base = Self;
	type Token = Token<Self>;

	type Data = Data;
	type DataRef<'data> = &'data Data
	where
		Self: 'data;
	type DataRefMut<'data> = &'data mut Data
	where
		Self: 'data;

	fn new(arena: &mut Arena<Self::Base>, data: Self::Data) -> Self::Token {
		Token::new(arena.0.insert_with(|idx| Self {
			token: Token::new(idx),

			parent: None,

			prev: None,
			next: None,

			first_child: None,
			last_child: None,

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

impl<Data: Debug> BaseNode for UnifiedNode<Data> {
	type Representation = UnifiedNodeRepresentation<Data>;

	type Branch = Self;
	type Leaf = Self;

	fn into_representation(self, arena: &mut Arena<Self>) -> Self::Representation {
		UnifiedNodeRepresentation {
			children: remove_children_linked(&self, arena),
			data: self.data,
		}
	}
}

impl<Data: Debug> LinkedNode for UnifiedNode<Data> {
	#[inline(always)]
	fn prev(&self) -> Option<Token<Self>> {
		self.prev
	}

	#[inline(always)]
	fn next(&self) -> Option<Token<Self>> {
		self.next
	}
}

impl<Data: Debug> BranchNode for UnifiedNode<Data> {
	type ChildrenIter<'branch> = iter::ChildrenLinked<'branch, Self> where Self: 'branch;

	#[inline(always)]
	fn first(&self) -> Option<Token<Self>> {
		self.first_child
	}

	#[inline(always)]
	fn last(&self) -> Option<Token<Self>> {
		self.last_child
	}

	#[inline(always)]
	fn children<'branch>(&'branch self, arena: &'branch Arena<Self>) -> Self::ChildrenIter<'branch> {
		iter::ChildrenLinked::new(arena, self.first_child, self.last_child)
	}

	#[inline(always)]
	fn is_empty(&self) -> bool {
		// Assert that if either the first child or the last child are `None`, both are, because the
		// if there are 1 or more children, there should always be both a first and last child.
		debug_assert!(!(self.first_child.is_none() ^ self.last_child.is_none()));

		self.first_child.is_none()
	}

	fn detach_front(token: Self::Token, arena: &mut Arena<Self>) -> Option<Token<Self>> {
		let this = &mut arena.0[token.idx()];

		match (this.first_child, this.last_child) {
			// Just a single child.
			(Some(first), Some(last)) if first == last => {
				(this.first_child, this.last_child) = (None, None);

				let node = &mut arena.0[first.idx()];

				// Detach the node's parent and siblings.
				node.parent = None;

				node.prev = None;
				node.next = None;

				Some(first)
			},

			// Multiple children.
			(Some(first), Some(_)) => {
				let next = first.next(arena).expect("There are multiple children");

				// Move the `next` node to the front.
				arena.0[token.idx()].first_child = Some(next);
				arena.0[next.idx()].prev = None;

				let node = &mut arena.0[first.idx()];

				// Detach the node's parent and siblings.
				node.parent = None;

				node.prev = None;
				node.next = None;

				Some(first)
			},

			// No children.
			(..) => {
				// If either the first child or last child is `None`, both must be `None`.
				debug_assert!(this.first_child.is_none() && this.last_child.is_none());

				None
			},
		}
	}

	fn detach_back(token: Self::Token, arena: &mut Arena<Self>) -> Option<Token<Self>> {
		let this = &mut arena.0[token.idx()];

		match (this.first_child, this.last_child) {
			// Just a single child.
			(Some(first), Some(last)) if first == last => {
				(this.first_child, this.last_child) = (None, None);

				let node = &mut arena.0[last.idx()];

				// Detach the node's parent and siblings.
				node.parent = None;

				node.prev = None;
				node.next = None;

				Some(last)
			},

			// Multiple children.
			(Some(_), Some(last)) => {
				let prev = last.prev(arena).expect("There are multiple children");

				// Move the `prev` node to the back.
				arena.0[token.idx()].last_child = Some(prev);
				arena.0[prev.idx()].next = None;

				let node = &mut arena.0[last.idx()];

				// Detach the node's parent and siblings.
				node.parent = None;

				node.prev = None;
				node.next = None;

				Some(last)
			},

			// No children.
			(..) => {
				// If either the first child or last child is `None`, both must be `None`.
				debug_assert!(this.first_child.is_none() && this.last_child.is_none());

				None
			},
		}
	}

	fn pop_front(token: Self::Token, arena: &mut Arena<Self>) -> Option<UnifiedNodeRepresentation<Data>> {
		let this = &mut arena.0[token.idx()];

		match (this.first_child, this.last_child) {
			// Just a single child.
			(Some(first), Some(last)) if first == last => {
				(this.first_child, this.last_child) = (None, None);

				Some(
					arena
						.0
						.remove(first.idx())
						.expect("tried to remove child but there was no such node in the `arena`")
						.into_representation(arena),
				)
			},

			// Multiple children.
			(Some(first), Some(_)) => {
				let next = first.next(arena).expect("There are multiple children.");

				// Move the `next` node to the front.
				arena.0[token.idx()].first_child = Some(next);
				arena.0[next.idx()].prev = None;

				Some(
					arena
						.0
						.remove(first.idx())
						.expect("tried to remove child but there was no such node in the `arena`")
						.into_representation(arena),
				)
			},

			// No children.
			(..) => {
				// If either the first child or last child is `None`, both must be `None`.
				debug_assert!(this.first_child.is_none() && this.last_child.is_none());

				None
			},
		}
	}

	fn pop_back(token: Self::Token, arena: &mut Arena<Self>) -> Option<UnifiedNodeRepresentation<Data>> {
		let this = &mut arena.0[token.idx()];

		match (this.first_child, this.last_child) {
			// Just a single child.
			(Some(first), Some(last)) if first == last => {
				(this.first_child, this.last_child) = (None, None);

				Some(
					arena
						.0
						.remove(last.idx())
						.expect("tried to remove child but there was no such node in the `arena`")
						.into_representation(arena),
				)
			},

			// Multiple children.
			(Some(_), Some(last)) => {
				let prev = last.prev(arena).expect("There are multiple children.");

				// Move the `next` node to the front.
				arena.0[token.idx()].last_child = Some(prev);
				arena.0[prev.idx()].next = None;

				Some(
					arena
						.0
						.remove(last.idx())
						.expect("tried to remove child but there was no such node in the `arena`")
						.into_representation(arena),
				)
			},

			// No children.
			(..) => {
				// If either the first child or last child is `None`, both must be `None`.
				debug_assert!(this.first_child.is_none() && this.last_child.is_none());

				None
			},
		}
	}

	fn push_front(token: Self::Token, arena: &mut Arena<Self>, new: Token<Self>) {
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

		let this = &mut arena.0[token.idx()];

		// Push the child's token.
		match (this.first_child, this.last_child) {
			// One or more existing children.
			(Some(first), Some(_)) => {
				// Move the existing first child forward.
				this.first_child = Some(new);
				arena.0[first.idx()].prev = Some(new);
			},

			// No existing children.
			(..) => {
				// If either the first child or last child is `None`, both must be `None`.
				debug_assert!(this.first_child.is_none() && this.last_child.is_none());

				// Update the first and last child.
				(this.first_child, this.last_child) = (Some(new), Some(new));
			},
		}
	}

	fn push_back(token: Self::Token, arena: &mut Arena<Self>, new: Token<Self>) {
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

		let this = &mut arena.0[token.idx()];

		// Push the child's token.
		match (this.first_child, this.last_child) {
			// One or more existing children.
			(Some(_), Some(last)) => {
				// Move the existing first child forward.
				this.last_child = Some(new);
				arena.0[last.idx()].next = Some(new);
			},

			// No existing children.
			(..) => {
				// If either the first child or last child is `None`, both must be `None`.
				debug_assert!(this.first_child.is_none() && this.last_child.is_none());

				// Update the first and last child.
				(this.first_child, this.last_child) = (Some(new), Some(new));
			},
		}
	}
}
