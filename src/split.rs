// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::{
	collections::VecDeque,
	fmt::{Debug, Formatter},
	hash::{Hash, Hasher},
};

use generational_arena::Index;

use crate::{
	iter,
	sealed::{Idx, Sealed},
	token_to_node,
	Arena,
	BaseNode,
	BranchNode,
	LinkedNode,
	Node,
	NodeToken,
	Token,
};

type BranchToken<BranchData, LeafData> = Token<Branch<BranchData, LeafData>>;
type LeafToken<BranchData, LeafData> = Token<Leaf<BranchData, LeafData>>;

/// A [node] that is split into separate [branch] and [leaf] nodes.
///
/// [`BranchData`] represents the [custom data](Branch::Data) associated with [branches], while
/// [`LeafData`] represents the [custom data](Leaf::Data) associated with [leaves].
///
/// For a [node] that _isn't_ split into separate branch and leaf nodes, see [`UnifiedNode`].
///
/// [`UnifiedNode`]: crate::unified::UnifiedNode
///
/// [node]: Node
/// [branch]: Branch
/// [branches]: Branch
/// [leaf]: Leaf
/// [leaves]: Leaf
#[derive(Debug)]
pub enum SplitNode<BranchData: Debug, LeafData: Debug> {
	/// A [branch] node that may have children.
	///
	/// [branch]: Branch
	Branch(Branch<BranchData, LeafData>),
	/// A [leaf] node that may not have children.
	///
	/// [leaf]: Leaf
	Leaf(Leaf<BranchData, LeafData>),
}

/// The [custom data] associated with a [split node].
///
/// [custom data]: Node::Data
/// [split node]: SplitNode
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum SplitData<BranchData: Debug, LeafData: Debug> {
	/// The [data] associated with a [branch] node.
	///
	/// [data]: Branch::Data
	/// [branch]: Branch
	Branch(BranchData),
	/// The [data] associated with a [leaf] node.
	///
	/// [data]: Leaf::Data
	/// [leaf]: Leaf
	Leaf(LeafData),
}

/// The [representation] of a [split node] after it has been removed from the [arena].
///
/// [representation]: BaseNode::Representation
/// [split node]: SplitNode
/// [arena]: Arena
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum SplitNodeRepresentation<BranchData: Debug, LeafData: Debug> {
	/// The [representation] of a [branch] node.
	///
	/// [representation]: BaseNode::Representation
	/// [branch]: Branch
	Branch {
		/// The [branch] node's [children].
		///
		/// [branch]: Branch
		/// [children]: Branch::children
		children: VecDeque<SplitNodeRepresentation<BranchData, LeafData>>,
		/// The [data] associated with the [branch] node.
		///
		/// [branch]: Branch
		/// [data]: Branch::Data
		data: BranchData,
	},
	/// The [representation] of a [leaf] node.
	///
	/// [representation]: BaseNode::Representation
	/// [leaf]: Leaf
	Leaf {
		/// The [data] associated with the [leaf] node.
		///
		/// [leaf]: Leaf
		/// [data]: Leaf::Data
		data: LeafData,
	},
}

/// The [token] type referring to a [split node].
///
/// [token]: NodeToken
/// [split node]: SplitNode
pub enum SplitToken<BranchData: Debug, LeafData: Debug> {
	/// A [branch] node's [token].
	///
	/// [branch]: Branch
	/// [token]: NodeToken
	Branch(BranchToken<BranchData, LeafData>),
	/// A [leaf] node's [token].
	///
	/// [leaf]: Leaf
	/// [token]: NodeToken
	Leaf(LeafToken<BranchData, LeafData>),
}

/// The [node] representing [branches] in a [split node] tree.
///
/// [Branches] are [nodes] which may have [children], as opposed to [leaves], which may not have
/// [children].
///
/// [`BranchData`] represents the [custom data] associated with branch nodes.
///
/// [custom data]: Branch::Data
///
/// [node]: Node
/// [nodes]: Node
/// [Branches]: BranchNode
/// [branches]: BranchNode
/// [leaves]: Leaf
/// [children]: Branch::children
/// [split node]: SplitNode
#[derive(Debug)]
pub struct Branch<BranchData: Debug, LeafData: Debug> {
	token: Token<Self>,

	parent: Option<BranchToken<BranchData, LeafData>>,

	prev: Option<SplitToken<BranchData, LeafData>>,
	next: Option<SplitToken<BranchData, LeafData>>,

	first_child: Option<SplitToken<BranchData, LeafData>>,
	last_child: Option<SplitToken<BranchData, LeafData>>,

	data: BranchData,
}

/// The [node] representing leaves in a [split node] tree.
///
/// Leaves are [nodes] which may not have [children], as opposed to [branches], which may have
/// [children].
///
/// [`LeafData`] represents the [custom data] associated with leaf nodes.
///
/// [custom data]: Leaf::Data
///
/// [node]: Node
/// [nodes]: Node
/// [children]: Branch::children
/// [branches]: Branch
/// [split node]: SplitNode
#[derive(Debug)]
pub struct Leaf<BranchData: Debug, LeafData: Debug> {
	token: Token<Self>,

	parent: Option<BranchToken<BranchData, LeafData>>,

	prev: Option<SplitToken<BranchData, LeafData>>,
	next: Option<SplitToken<BranchData, LeafData>>,

	data: LeafData,
}

impl<BranchData, LeafData> Debug for SplitToken<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	#[inline(always)]
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Branch(branch) => branch.fmt(f),
			Self::Leaf(leaf) => leaf.fmt(f),
		}
	}
}

impl<BranchData, LeafData, I: Idx> PartialEq<I> for SplitToken<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	#[inline(always)]
	fn eq(&self, other: &I) -> bool {
		self.idx() == other.idx()
	}
}

impl<BranchData, LeafData> Eq for SplitToken<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
}

impl<BranchData, LeafData> Hash for SplitToken<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	#[inline(always)]
	fn hash<H: Hasher>(&self, state: &mut H) {
		match self {
			Self::Branch(branch) => branch.hash(state),
			Self::Leaf(leaf) => leaf.hash(state),
		}
	}
}

impl<BranchData, LeafData> Clone for SplitToken<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	fn clone(&self) -> Self {
		*self
	}
}

impl<BranchData, LeafData> Copy for SplitToken<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
}

impl<BranchData, LeafData> SplitToken<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	/// Creates a new `SplitToken` for a [branch] from the given `token`.
	///
	/// [branch]: Branch
	#[inline(always)]
	pub const fn new_branch(token: BranchToken<BranchData, LeafData>) -> Self {
		Self::Branch(token)
	}

	/// Creates a new `SplitToken` for a [leaf] from the given `token`.
	///
	/// [leaf]: Leaf
	#[inline(always)]
	pub const fn new_leaf(token: LeafToken<BranchData, LeafData>) -> Self {
		Self::Leaf(token)
	}
}

impl<BranchData, LeafData> Idx for SplitToken<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	#[inline(always)]
	fn idx(&self) -> Index {
		match self {
			Self::Branch(branch) => branch.idx(),
			Self::Leaf(leaf) => leaf.idx(),
		}
	}
}

impl<BranchData, LeafData> NodeToken<SplitNode<BranchData, LeafData>> for SplitToken<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
}

impl<BranchData, LeafData> PartialEq for Branch<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	#[inline(always)]
	fn eq(&self, other: &Self) -> bool {
		self.token == other.token
	}
}

impl<BranchData, LeafData> Eq for Branch<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
}

impl<BranchData, LeafData> Hash for Branch<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	#[inline(always)]
	fn hash<H: Hasher>(&self, state: &mut H) {
		self.token.hash(state);
	}
}

impl<BranchData, LeafData> PartialEq for Leaf<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	#[inline(always)]
	fn eq(&self, other: &Self) -> bool {
		self.token == other.token
	}
}

impl<BranchData, LeafData> Eq for Leaf<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
}

impl<BranchData, LeafData> Hash for Leaf<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	#[inline(always)]
	fn hash<H: Hasher>(&self, state: &mut H) {
		self.token.hash(state);
	}
}

impl<'node, BranchData, LeafData> TryFrom<&'node SplitNode<BranchData, LeafData>>
	for &'node Branch<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	type Error = &'node SplitNode<BranchData, LeafData>;

	#[inline(always)]
	fn try_from(node: &'node SplitNode<BranchData, LeafData>) -> Result<Self, Self::Error> {
		match node {
			SplitNode::Branch(branch) => Ok(branch),
			SplitNode::Leaf(_) => Err(node),
		}
	}
}

impl<'node, BranchData, LeafData> TryFrom<&'node mut SplitNode<BranchData, LeafData>>
	for &'node mut Branch<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	type Error = &'node mut SplitNode<BranchData, LeafData>;

	#[inline(always)]
	fn try_from(node: &'node mut SplitNode<BranchData, LeafData>) -> Result<Self, Self::Error> {
		match node {
			SplitNode::Branch(branch) => Ok(branch),
			SplitNode::Leaf(_) => Err(node),
		}
	}
}

impl<'node, BranchData, LeafData> TryFrom<&'node SplitNode<BranchData, LeafData>> for &'node Leaf<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	type Error = &'node SplitNode<BranchData, LeafData>;

	#[inline(always)]
	fn try_from(node: &'node SplitNode<BranchData, LeafData>) -> Result<Self, Self::Error> {
		match node {
			SplitNode::Branch(_) => Err(node),
			SplitNode::Leaf(leaf) => Ok(leaf),
		}
	}
}

impl<'node, BranchData, LeafData> TryFrom<&'node mut SplitNode<BranchData, LeafData>>
	for &'node mut Leaf<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	type Error = &'node mut SplitNode<BranchData, LeafData>;

	#[inline(always)]
	fn try_from(node: &'node mut SplitNode<BranchData, LeafData>) -> Result<Self, Self::Error> {
		match node {
			SplitNode::Branch(_) => Err(node),
			SplitNode::Leaf(leaf) => Ok(leaf),
		}
	}
}

impl<BranchData, LeafData> SplitNode<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	#[inline(always)]
	fn set_parent(&mut self, parent: Option<BranchToken<BranchData, LeafData>>) {
		match self {
			Self::Branch(branch) => branch.parent = parent,
			Self::Leaf(leaf) => leaf.parent = parent,
		}
	}

	#[inline(always)]
	fn set_prev(&mut self, prev: Option<SplitToken<BranchData, LeafData>>) {
		match self {
			Self::Branch(branch) => branch.prev = prev,
			Self::Leaf(leaf) => leaf.prev = prev,
		}
	}

	#[inline(always)]
	fn set_next(&mut self, next: Option<SplitToken<BranchData, LeafData>>) {
		match self {
			Self::Branch(branch) => branch.next = next,
			Self::Leaf(leaf) => leaf.next = next,
		}
	}
}

impl<BranchData, LeafData> Sealed for SplitNode<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
}

impl<BranchData, LeafData> Node for SplitNode<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	type Base = Self;
	type Token = SplitToken<BranchData, LeafData> where Self: Sized;

	type Data = SplitData<BranchData, LeafData>;
	type DataRef<'data> = SplitData<&'data BranchData, &'data LeafData>
	where
		Self: 'data;
	type DataRefMut<'data> = SplitData<&'data mut BranchData, &'data mut LeafData>
	where
		Self: 'data;

	#[inline(always)]
	fn new(arena: &mut Arena<Self::Base>, data: Self::Data) -> Self::Token
	where
		Self: Sized,
	{
		match data {
			SplitData::Branch(data) => SplitToken::Branch(Branch::new(arena, data)),
			SplitData::Leaf(data) => SplitToken::Leaf(Leaf::new(arena, data)),
		}
	}

	#[inline(always)]
	fn token(&self) -> Self::Token
	where
		Self: Sized,
	{
		match self {
			Self::Branch(branch) => SplitToken::Branch(branch.token()),
			Self::Leaf(leaf) => SplitToken::Leaf(leaf.token()),
		}
	}

	#[inline(always)]
	fn parent(&self) -> Option<Token<<Self as BaseNode>::Branch>> {
		match self {
			Self::Branch(branch) => branch.parent(),
			Self::Leaf(leaf) => leaf.parent(),
		}
	}

	#[inline(always)]
	fn data(&self) -> Self::DataRef<'_> {
		match self {
			Self::Branch(branch) => SplitData::Branch(branch.data()),
			Self::Leaf(leaf) => SplitData::Leaf(leaf.data()),
		}
	}

	#[inline(always)]
	fn data_mut(&mut self) -> Self::DataRefMut<'_> {
		match self {
			Self::Branch(branch) => SplitData::Branch(branch.data_mut()),
			Self::Leaf(leaf) => SplitData::Leaf(leaf.data_mut()),
		}
	}
}

impl<BranchData, LeafData> BaseNode for SplitNode<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	type Representation = SplitNodeRepresentation<BranchData, LeafData>;

	type Branch = Branch<BranchData, LeafData>;
	type Leaf = Leaf<BranchData, LeafData>;

	fn into_representation(self, arena: &mut Arena<Self::Base>) -> Self::Representation
	where
		Self: Sized,
	{
		match self {
			// Branch.
			Self::Branch(branch) => {
				let mut children = VecDeque::new();
				let mut child = branch.first_child;

				while let Some(token) = &child {
					children.push_back(
						arena
							.0
							.remove(token.idx())
							.expect("tried to remove child but there was no such node in the `arena`")
							.into_representation(arena),
					);

					child = Some(*token);
				}

				SplitNodeRepresentation::Branch {
					children,
					data: branch.data,
				}
			},

			// Leaf.
			Self::Leaf(leaf) => SplitNodeRepresentation::Leaf { data: leaf.data },
		}
	}
}

impl<BranchData, LeafData> LinkedNode for SplitNode<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	#[inline(always)]
	fn prev(&self) -> Option<Self::Token> {
		match self {
			Self::Branch(branch) => branch.prev(),
			Self::Leaf(leaf) => leaf.prev(),
		}
	}

	#[inline(always)]
	fn next(&self) -> Option<Self::Token> {
		match self {
			Self::Branch(branch) => branch.next(),
			Self::Leaf(leaf) => leaf.next(),
		}
	}
}

impl<BranchData, LeafData> Sealed for Branch<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
}

impl<BranchData, LeafData> Node for Branch<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	type Base = SplitNode<BranchData, LeafData>;
	type Token = Token<Self>
	where
		Self: Sized;

	type Data = BranchData;
	type DataRef<'data> = &'data BranchData
	where
		Self: 'data;
	type DataRefMut<'data> = &'data mut BranchData
	where
		Self: 'data;

	fn new(arena: &mut Arena<Self::Base>, data: Self::Data) -> Token<Self>
	where
		Self: Sized,
	{
		Token::new(arena.0.insert_with(|idx| {
			SplitNode::Branch(Self {
				token: Token::new(idx),

				parent: None,

				prev: None,
				next: None,

				first_child: None,
				last_child: None,

				data,
			})
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
	fn data(&self) -> Self::DataRef<'_> {
		&self.data
	}

	#[inline(always)]
	fn data_mut(&mut self) -> Self::DataRefMut<'_> {
		&mut self.data
	}
}

impl<BranchData, LeafData> LinkedNode for Branch<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	#[inline(always)]
	fn prev(&self) -> Option<<Self::Base as Node>::Token> {
		self.prev
	}

	#[inline(always)]
	fn next(&self) -> Option<<Self::Base as Node>::Token> {
		self.next
	}
}

impl<BranchData, LeafData> BranchNode for Branch<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	type ChildrenIter<'branch> = iter::ChildrenLinked<'branch, Self>
	where
		Self: 'branch;

	#[inline(always)]
	fn first(&self) -> Option<<Self::Base as Node>::Token> {
		self.first_child
	}

	#[inline(always)]
	fn last(&self) -> Option<<Self::Base as Node>::Token> {
		self.last_child
	}

	#[inline(always)]
	fn children<'branch>(&'branch self, arena: &'branch Arena<Self::Base>) -> Self::ChildrenIter<'branch> {
		iter::ChildrenLinked::new(arena, self.first_child, self.last_child)
	}

	#[inline(always)]
	fn is_empty(&self) -> bool {
		// Assert that if either the first child or the last child are `None`, both are, because the
		// if there are 1 or more children, there should always be both a first and last child.
		debug_assert!(!(self.first_child.is_none() ^ self.last_child.is_none()));

		self.first_child.is_none()
	}

	fn detach_front(token: Self::Token, arena: &mut Arena<Self::Base>) -> Option<<Self::Base as Node>::Token>
	where
		Self: Sized,
	{
		let this = token_to_node!(ref mut, Self: token, arena);

		match (this.first_child, this.last_child) {
			// Just a single child.
			(Some(first), Some(last)) if first == last => {
				(this.first_child, this.last_child) = (None, None);

				let node = &mut arena.0[first.idx()];

				// Detach the node's parent and siblings.
				node.set_parent(None);

				node.set_prev(None);
				node.set_next(None);

				Some(first)
			},

			// Multiple children.
			(Some(first), Some(_)) => {
				let next = first.next(arena).expect("There are multiple children.");

				let this = token_to_node!(ref mut, Self: token, arena);

				// Move the `next` node to the front.
				this.first_child = Some(next);
				arena.0[next.idx()].set_prev(None);

				let node = &mut arena.0[first.idx()];

				// Detach the node's parent and siblings.
				node.set_parent(None);

				node.set_prev(None);
				node.set_next(None);

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

	fn detach_back(token: Self::Token, arena: &mut Arena<Self::Base>) -> Option<<Self::Base as Node>::Token>
	where
		Self: Sized,
	{
		let this = token_to_node!(ref mut, Self: token, arena);

		match (this.first_child, this.last_child) {
			// Just a single child.
			(Some(first), Some(last)) if first == last => {
				(this.first_child, this.last_child) = (None, None);

				let node = &mut arena.0[last.idx()];

				// Detach the node's parent and siblings.
				node.set_parent(None);

				node.set_prev(None);
				node.set_next(None);

				Some(last)
			},

			// Multiple children.
			(Some(_), Some(last)) => {
				let prev = last.prev(arena).expect("There are multiple children.");

				let this = token_to_node!(ref mut, Self: token, arena);

				// Move the `prev` node to the back.
				this.last_child = Some(prev);
				arena.0[prev.idx()].set_next(None);

				let node = &mut arena.0[last.idx()];

				// Detach the node's parent and siblings.
				node.set_parent(None);

				node.set_prev(None);
				node.set_next(None);

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

	fn pop_front(token: Self::Token, arena: &mut Arena<Self::Base>) -> Option<<Self::Base as BaseNode>::Representation>
	where
		Self: Sized,
	{
		let this = token_to_node!(ref mut, Self: token, arena);

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

				let this = token_to_node!(ref mut, Self: token, arena);

				// Move the `next` node to the front.
				this.first_child = Some(next);
				arena.0[next.idx()].set_prev(None);

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

	fn pop_back(token: Self::Token, arena: &mut Arena<Self::Base>) -> Option<<Self::Base as BaseNode>::Representation>
	where
		Self: Sized,
	{
		let this = token_to_node!(ref mut, Self: token, arena);

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
			(Some(_), Some(last)) => {
				let prev = last.prev(arena).expect("There are multiple children.");

				let this = token_to_node!(ref mut, Self: token, arena);

				// Move the `prev` node to the back.
				this.last_child = Some(prev);
				arena.0[prev.idx()].set_next(None);

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

	fn push_front(token: Self::Token, arena: &mut Arena<Self::Base>, new: <Self::Base as Node>::Token)
	where
		Self: Sized,
	{
		// We're not pushing our own root...
		assert_ne!(
			token_to_node!(ref, Self: token, arena).root(arena),
			new,
			"tried to push this branch's root node as a child"
		);
		// And we're not pushing a child that already has a parent...
		assert!(
			arena.0[new.idx()].parent().is_none(),
			"tried to push a child that already has a parent"
		);

		// Set the child's parent.
		arena.0[new.idx()].set_parent(Some(token));

		let this = token_to_node!(ref mut, Self: token, arena);

		match (this.first_child, this.last_child) {
			// One or more existing children.
			(Some(first), Some(_)) => {
				// Move the existing first child forward.
				this.first_child = Some(new);
				arena.0[first.idx()].set_prev(Some(new));
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

	fn push_back(token: Self::Token, arena: &mut Arena<Self::Base>, new: <Self::Base as Node>::Token)
	where
		Self: Sized,
	{
		// We're not pushing our own root...
		assert_ne!(
			token_to_node!(ref, Self: token, arena).root(arena),
			new,
			"tried to push this branch's root node as a child"
		);
		// And we're not pushing a child that already has a parent...
		assert!(
			arena.0[new.idx()].parent().is_none(),
			"tried to push a child that already has a parent"
		);

		// Set the child's parent.
		arena.0[new.idx()].set_parent(Some(token));

		let this = token_to_node!(ref mut, Self: token, arena);

		match (this.first_child, this.last_child) {
			// One or more existing children.
			(Some(_), Some(last)) => {
				// Move the existing last child backward.
				this.last_child = Some(new);
				arena.0[last.idx()].set_prev(Some(new));
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

impl<BranchData, LeafData> Sealed for Leaf<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
}

impl<BranchData, LeafData> Node for Leaf<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	type Base = SplitNode<BranchData, LeafData>;
	type Token = Token<Self> where Self: Sized;

	type Data = LeafData;
	type DataRef<'data> = &'data LeafData
	where
		Self: 'data;
	type DataRefMut<'data> = &'data mut LeafData
	where
		Self: 'data;

	fn new(arena: &mut Arena<Self::Base>, data: Self::Data) -> Token<Self>
	where
		Self: Sized,
	{
		Token::new(arena.0.insert_with(|idx| {
			SplitNode::Leaf(Self {
				token: Token::new(idx),

				parent: None,

				prev: None,
				next: None,

				data,
			})
		}))
	}

	#[inline(always)]
	fn token(&self) -> Self::Token
	where
		Self: Sized,
	{
		self.token
	}

	#[inline(always)]
	fn parent(&self) -> Option<Token<<Self::Base as BaseNode>::Branch>> {
		self.parent
	}

	#[inline(always)]
	fn data(&self) -> Self::DataRef<'_> {
		&self.data
	}

	#[inline(always)]
	fn data_mut(&mut self) -> Self::DataRefMut<'_> {
		&mut self.data
	}
}

impl<BranchData, LeafData> LinkedNode for Leaf<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	#[inline(always)]
	fn prev(&self) -> Option<<Self::Base as Node>::Token> {
		self.prev
	}

	#[inline(always)]
	fn next(&self) -> Option<<Self::Base as Node>::Token> {
		self.next
	}
}
