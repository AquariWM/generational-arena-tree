// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::{collections::VecDeque, iter::FusedIterator};

use crate::{sealed::Idx, Arena, BaseNode, BranchNode, LinkedNode, Node};

/// An iterator over the [tokens] of a [node]'s [ancestors].
///
/// This iterator is returned by [`Node::ancestors`].
///
/// [tokens]: Node::Token
/// [node]: Node
/// [ancestors]: Node::ancestors
///
/// [`Node::ancestors`]: Node::ancestors
#[must_use = "iterators are lazy and do nothing unless consumed"]
pub struct Ancestors<'arena, Base: BaseNode> {
	arena: &'arena Arena<Base>,

	parent: Option<<<Base as BaseNode>::Branch as Node>::Token>,
}

/// An iterator over the [tokens] of a [branch]'s [children].
///
/// This iterator is returned by [`Branch::children`].
///
/// [tokens]: Node::Token
/// [branch]: BranchNode
/// [children]: Branch::children
#[must_use = "iterators are lazy and do nothing unless consumed"]
pub struct ChildrenLinked<'arena, Branch: BranchNode>
where
	Branch::Base: LinkedNode,
{
	arena: &'arena Arena<Branch::Base>,

	child_front: Option<<Branch::Base as Node>::Token>,
	child_back: Option<<Branch::Base as Node>::Token>,
}

/// An iterator over the [tokens] of a [branch]'s [descendants].
///
/// This iterator is returned by [`Branch::descendants`].
///
/// [tokens]: Node::Token
/// [branch]: BranchNode
/// [descendants]: Branch::descendants
#[must_use = "iterators are lazy and do nothing unless consumed"]
pub struct Descendants<'arena, Branch: BranchNode + 'arena>
where
	for<'base> &'base Branch: TryFrom<&'base Branch::Base>,
{
	arena: &'arena Arena<Branch::Base>,

	current: Option<Branch::ChildrenIter<'arena>>,
	stack: VecDeque<Branch::ChildrenIter<'arena>>,
}

/// An iterator over the [tokens] of a [node]'s [preceding siblings].
///
/// This iterator is returned by [`LinkedNode::preceding_siblings`].
///
/// [tokens]: Node::Token
/// [node]: LinkedNode
/// [preceding siblings]: LinkedNode::preceding_siblings
#[must_use = "iterators are lazy and do nothing unless consumed"]
pub struct PrecedingSiblings<'arena, Linked: Node>
where
	Linked::Base: LinkedNode,
{
	arena: &'arena Arena<Linked::Base>,

	prev: Option<<Linked::Base as Node>::Token>,
}

/// An iterator over the [tokens] of a [node]'s [following siblings].
///
/// This iterator is returned by [`LinkedNode::following_siblings`].
///
/// [tokens]: Node::Token
/// [node]: LinkedNode
/// [following siblings]: LinkedNode::following_siblings
#[must_use = "iterators are lazy and do nothing unless consumed"]
pub struct FollowingSiblings<'arena, Linked: Node>
where
	Linked::Base: LinkedNode,
{
	arena: &'arena Arena<Linked::Base>,

	next: Option<<Linked::Base as Node>::Token>,
}

impl<'arena, Base: BaseNode> Ancestors<'arena, Base> {
	#[inline(always)]
	pub(super) const fn new(
		arena: &'arena Arena<Base>,
		parent: Option<<<Base as BaseNode>::Branch as Node>::Token>,
	) -> Self {
		Self { arena, parent }
	}
}

impl<'arena, Base: BaseNode> Iterator for Ancestors<'arena, Base> {
	type Item = <<Base as BaseNode>::Branch as Node>::Token;

	#[inline]
	fn next(&mut self) -> Option<Self::Item> {
		self.parent.map(|parent| {
			self.parent = self.arena.0[parent.idx()].parent();

			parent
		})
	}

	#[inline(always)]
	fn size_hint(&self) -> (usize, Option<usize>) {
		match &self.parent {
			None => (0, Some(0)),
			Some(_) => (1, None),
		}
	}
}

impl<'arena, Base: BaseNode> FusedIterator for Ancestors<'arena, Base> {}

impl<'arena, Branch> ChildrenLinked<'arena, Branch>
where
	Branch: BranchNode,
	Branch::Base: LinkedNode,
{
	#[inline(always)]
	pub(super) const fn new(
		arena: &'arena Arena<Branch::Base>,
		child_front: Option<<Branch::Base as Node>::Token>,
		child_back: Option<<Branch::Base as Node>::Token>,
	) -> Self {
		Self {
			arena,

			child_front,
			child_back,
		}
	}
}

impl<'arena, Branch> Descendants<'arena, Branch>
where
	Branch: BranchNode + 'arena,
	for<'base> &'base Branch: TryFrom<&'base Branch::Base>,
{
	#[inline(always)]
	pub(super) const fn new(branch: &Branch, arena: &'arena Arena<Branch::Base>) -> Self {
		Self {
			arena,

			current: Some(branch.children(arena)),
			stack: VecDeque::new(),
		}
	}
}

impl<'arena, Linked: Node> PrecedingSiblings<'arena, Linked>
where
	Linked::Base: LinkedNode,
{
	#[inline(always)]
	pub(super) const fn new(arena: &'arena Arena<Linked::Base>, prev: Option<<Linked::Base as Node>::Token>) -> Self {
		Self { arena, prev }
	}
}

impl<'arena, Linked: Node> FollowingSiblings<'arena, Linked>
where
	Linked::Base: LinkedNode,
{
	#[inline(always)]
	pub(super) const fn new(arena: &'arena Arena<Linked::Base>, next: Option<<Linked::Base as Node>::Token>) -> Self {
		Self { arena, next }
	}
}

impl<'arena, Branch> Iterator for ChildrenLinked<'arena, Branch>
where
	Branch: BranchNode,
	Branch::Base: LinkedNode,
{
	type Item = <Branch::Base as Node>::Token;

	fn next(&mut self) -> Option<Self::Item> {
		self.child_front.map(|child| {
			match self.child_back {
				// If the front and back children have met in the middle, end both iterators.
				Some(back) if child == back => {
					(self.child_front, self.child_back) = (None, None);
				},

				// Otherwise, advance the front child forward.
				_ => self.child_front = self.arena.0[child.idx()].next(),
			}

			child
		})
	}

	fn size_hint(&self) -> (usize, Option<usize>) {
		match (&self.child_front, &self.child_back) {
			// No more items.
			(None, None) => (0, Some(0)),
			// 1 more item.
			(Some(front), Some(back)) if front == back => (1, Some(1)),
			// 2 or more items.
			(Some(_), Some(_)) => (2, None),

			// This _should_ be illegal state, but if it somehow does occur, then we don't have a
			// proper size hint.
			(..) => (0, None),
		}
	}
}

impl<'arena, Branch> DoubleEndedIterator for ChildrenLinked<'arena, Branch>
where
	Branch: BranchNode,
	Branch::Base: LinkedNode,
{
	fn next_back(&mut self) -> Option<Self::Item> {
		self.child_back.map(|child| {
			match self.child_front {
				// If the front and back children have met in the middle, end both iterators.
				Some(front) if child == front => {
					(self.child_front, self.child_back) = (None, None);
				},

				// Otherwise, advance the back child backward.
				_ => self.child_back = self.arena.0[child.idx()].prev(),
			}

			child
		})
	}
}

impl<'arena, Branch> FusedIterator for ChildrenLinked<'arena, Branch>
where
	Branch: BranchNode,
	Branch::Base: LinkedNode,
{
}

impl<'arena, Branch> Iterator for Descendants<'arena, Branch>
where
	Branch: BranchNode + 'arena,
	for<'base> &'base Branch: TryFrom<&'base Branch::Base>,
{
	type Item = <Branch::Base as Node>::Token;

	fn next(&mut self) -> Option<Self::Item> {
		// Next descendant's token.
		let token = self.current.as_mut().and_then(|iter| match iter.next() {
			// If `current` has another token, use that.
			Some(token) => Some(token),

			// If `current` doesn't have another token, try to find a `ChildrenIter` in the stack
			// that does.
			None => {
				self.current = None;
				let mut token = None;

				// Find the first `ChildrenIter` in the stack that has at least one child.
				//
				// NOTE: Going through multiple options to try to find a `ChildrenIter` in the stack
				//     : that has at least one child, rather than trying next `next()` call, is
				//     : required to ensure that this is a `FusedIterator`.
				while let Some(mut iter) = self.stack.pop_front() {
					match iter.next() {
						// This `ChildrenIter` has at least one child: update `self.current` and use
						// that child.
						Some(child) => {
							self.current = Some(iter);
							token = Some(child);

							break;
						},

						// This `ChildrenIter` does not have at least one child: move on to the next
						// one.
						None => continue,
					}
				}

				token
			},
		});

		// If there is a next token, and that token refers to a branch, add that branch's
		// `ChildrenIter` to the stack.
		if let Some(branch) = token
			.as_ref()
			.and_then(|token| <&Branch as TryFrom<&Branch::Base>>::try_from(&self.arena.0[token.idx()]).ok())
		{
			self.stack.push_back(branch.children(self.arena));
		}

		token
	}

	fn size_hint(&self) -> (usize, Option<usize>) {
		match &self.current {
			// There are no more children iterators to iterate, and so no more descendants are
			// possible.
			None => (0, Some(0)),

			Some(iter) => {
				let (min, max) = iter.size_hint();

				match max {
					// If the upper bound is `0` and the stack is empty, then there can be no more
					// descendants.
					Some(0) if self.stack.is_empty() => (0, Some(0)),

					// Otherwise, if there could be 1 or more children in the current iterator, then
					// those children could add extra `ChildrenIter`s to the stack, so we don't know
					// the upper bound.
					_ => (min, None),
				}
			},
		}
	}
}

impl<'arena, Branch> FusedIterator for Descendants<'arena, Branch>
where
	Branch: BranchNode + 'arena,
	for<'base> &'base Branch: TryFrom<&'base Branch::Base>,
{
}

impl<'arena, Linked: Node> Iterator for PrecedingSiblings<'arena, Linked>
where
	Linked::Base: LinkedNode,
{
	type Item = <Linked::Base as Node>::Token;

	#[inline]
	fn next(&mut self) -> Option<Self::Item> {
		self.prev.map(|child| {
			self.prev = self.arena.0[child.idx()].prev();

			child
		})
	}

	#[inline]
	fn size_hint(&self) -> (usize, Option<usize>) {
		match &self.prev {
			None => (0, Some(0)),
			Some(_) => (1, None),
		}
	}
}

impl<'arena, Linked: Node> FusedIterator for PrecedingSiblings<'arena, Linked> where Linked::Base: LinkedNode {}

impl<'arena, Linked: Node> Iterator for FollowingSiblings<'arena, Linked>
where
	Linked::Base: LinkedNode,
{
	type Item = <Linked::Base as Node>::Token;

	#[inline]
	fn next(&mut self) -> Option<Self::Item> {
		self.next.map(|child| {
			self.next = self.arena.0[child.idx()].next();

			child
		})
	}

	#[inline]
	fn size_hint(&self) -> (usize, Option<usize>) {
		match &self.next {
			None => (0, Some(0)),
			Some(_) => (1, None),
		}
	}
}

impl<'arena, Linked: Node> FusedIterator for FollowingSiblings<'arena, Linked> where Linked::Base: LinkedNode {}
