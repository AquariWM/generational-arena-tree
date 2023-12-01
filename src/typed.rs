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

#[derive(Debug)]
pub enum TypedNode<BranchData: Debug, LeafData: Debug> {
	Branch(Branch<BranchData, LeafData>),
	Leaf(Leaf<BranchData, LeafData>),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum TypedData<BranchData: Debug, LeafData: Debug> {
	Branch(BranchData),
	Leaf(LeafData),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum TypedNodeRepresentation<BranchData: Debug, LeafData: Debug> {
	Branch {
		children: VecDeque<TypedNodeRepresentation<BranchData, LeafData>>,
		data: BranchData,
	},

	Leaf {
		data: LeafData,
	},
}

pub enum TypedToken<BranchData: Debug, LeafData: Debug> {
	Branch(BranchToken<BranchData, LeafData>),
	Leaf(LeafToken<BranchData, LeafData>),
}

#[derive(Debug)]
pub struct Branch<BranchData: Debug, LeafData: Debug> {
	token: Token<Self>,

	parent: Option<BranchToken<BranchData, LeafData>>,

	prev: Option<TypedToken<BranchData, LeafData>>,
	next: Option<TypedToken<BranchData, LeafData>>,

	first_child: Option<TypedToken<BranchData, LeafData>>,
	last_child: Option<TypedToken<BranchData, LeafData>>,

	data: BranchData,
}

#[derive(Debug)]
pub struct Leaf<BranchData: Debug, LeafData: Debug> {
	token: Token<Self>,

	parent: Option<BranchToken<BranchData, LeafData>>,

	prev: Option<TypedToken<BranchData, LeafData>>,
	next: Option<TypedToken<BranchData, LeafData>>,

	data: LeafData,
}

impl<BranchData, LeafData> Debug for TypedToken<BranchData, LeafData>
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

impl<BranchData, LeafData, I: Idx> PartialEq<I> for TypedToken<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	#[inline(always)]
	fn eq(&self, other: &I) -> bool {
		self.idx() == other.idx()
	}
}

impl<BranchData, LeafData> Eq for TypedToken<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
}

impl<BranchData, LeafData> Hash for TypedToken<BranchData, LeafData>
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

impl<BranchData, LeafData> Clone for TypedToken<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	fn clone(&self) -> Self {
		*self
	}
}

impl<BranchData, LeafData> Copy for TypedToken<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
}

impl<BranchData, LeafData> TypedToken<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	/// Creates a new `TypedToken` for a [branch] from the given `token`.
	///
	/// [branch]: Branch
	#[inline(always)]
	pub const fn new_branch(token: BranchToken<BranchData, LeafData>) -> Self {
		Self::Branch(token)
	}

	/// Creates a new `TypedToken` for a [leaf] from the given `token`.
	///
	/// [leaf]: Leaf
	#[inline(always)]
	pub const fn new_leaf(token: LeafToken<BranchData, LeafData>) -> Self {
		Self::Leaf(token)
	}
}

impl<BranchData, LeafData> Idx for TypedToken<BranchData, LeafData>
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

impl<BranchData, LeafData> NodeToken<TypedNode<BranchData, LeafData>> for TypedToken<BranchData, LeafData>
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

impl<'node, BranchData, LeafData> TryFrom<&'node TypedNode<BranchData, LeafData>>
	for &'node Branch<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	type Error = &'node TypedNode<BranchData, LeafData>;

	#[inline(always)]
	fn try_from(node: &'node TypedNode<BranchData, LeafData>) -> Result<Self, Self::Error> {
		match node {
			TypedNode::Branch(branch) => Ok(branch),
			TypedNode::Leaf(_) => Err(node),
		}
	}
}

impl<'node, BranchData, LeafData> TryFrom<&'node mut TypedNode<BranchData, LeafData>>
	for &'node mut Branch<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	type Error = &'node mut TypedNode<BranchData, LeafData>;

	#[inline(always)]
	fn try_from(node: &'node mut TypedNode<BranchData, LeafData>) -> Result<Self, Self::Error> {
		match node {
			TypedNode::Branch(branch) => Ok(branch),
			TypedNode::Leaf(_) => Err(node),
		}
	}
}

impl<'node, BranchData, LeafData> TryFrom<&'node TypedNode<BranchData, LeafData>> for &'node Leaf<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	type Error = &'node TypedNode<BranchData, LeafData>;

	#[inline(always)]
	fn try_from(node: &'node TypedNode<BranchData, LeafData>) -> Result<Self, Self::Error> {
		match node {
			TypedNode::Branch(_) => Err(node),
			TypedNode::Leaf(leaf) => Ok(leaf),
		}
	}
}

impl<'node, BranchData, LeafData> TryFrom<&'node mut TypedNode<BranchData, LeafData>>
	for &'node mut Leaf<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	type Error = &'node mut TypedNode<BranchData, LeafData>;

	#[inline(always)]
	fn try_from(node: &'node mut TypedNode<BranchData, LeafData>) -> Result<Self, Self::Error> {
		match node {
			TypedNode::Branch(_) => Err(node),
			TypedNode::Leaf(leaf) => Ok(leaf),
		}
	}
}

impl<BranchData, LeafData> TypedNode<BranchData, LeafData>
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
	fn set_prev(&mut self, prev: Option<TypedToken<BranchData, LeafData>>) {
		match self {
			Self::Branch(branch) => branch.prev = prev,
			Self::Leaf(leaf) => leaf.prev = prev,
		}
	}

	#[inline(always)]
	fn set_next(&mut self, next: Option<TypedToken<BranchData, LeafData>>) {
		match self {
			Self::Branch(branch) => branch.next = next,
			Self::Leaf(leaf) => leaf.next = next,
		}
	}
}

impl<BranchData, LeafData> Sealed for TypedNode<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
}

impl<BranchData, LeafData> Node for TypedNode<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	type Base = Self;
	type Token = TypedToken<BranchData, LeafData> where Self: Sized;

	type Data = TypedData<BranchData, LeafData>;
	type DataRef<'data> = TypedData<&'data BranchData, &'data LeafData>
	where
		Self: 'data;
	type DataRefMut<'data> = TypedData<&'data mut BranchData, &'data mut LeafData>
	where
		Self: 'data;

	#[inline(always)]
	fn new(arena: &mut Arena<Self::Base>, data: Self::Data) -> Self::Token
	where
		Self: Sized,
	{
		match data {
			TypedData::Branch(data) => TypedToken::Branch(Branch::new(arena, data)),
			TypedData::Leaf(data) => TypedToken::Leaf(Leaf::new(arena, data)),
		}
	}

	#[inline(always)]
	fn token(&self) -> Self::Token
	where
		Self: Sized,
	{
		match self {
			Self::Branch(branch) => TypedToken::Branch(branch.token()),
			Self::Leaf(leaf) => TypedToken::Leaf(leaf.token()),
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
			Self::Branch(branch) => TypedData::Branch(branch.data()),
			Self::Leaf(leaf) => TypedData::Leaf(leaf.data()),
		}
	}

	#[inline(always)]
	fn data_mut(&mut self) -> Self::DataRefMut<'_> {
		match self {
			Self::Branch(branch) => TypedData::Branch(branch.data_mut()),
			Self::Leaf(leaf) => TypedData::Leaf(leaf.data_mut()),
		}
	}
}

impl<BranchData, LeafData> BaseNode for TypedNode<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	type Representation = TypedNodeRepresentation<BranchData, LeafData>;

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
							.expect("tried to remove child but there was no such node in the `arena`, child: {token:?}")
							.into_representation(arena),
					);

					child = Some(*token);
				}

				TypedNodeRepresentation::Branch {
					children,
					data: branch.data,
				}
			},

			// Leaf.
			Self::Leaf(leaf) => TypedNodeRepresentation::Leaf { data: leaf.data },
		}
	}
}

impl<BranchData, LeafData> LinkedNode for TypedNode<BranchData, LeafData>
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
	type Base = TypedNode<BranchData, LeafData>;
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
			TypedNode::Branch(Self {
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
		let this = token_to_node!(ref, Self: token, arena);

		// We're not pushing our own root...
		assert_ne!(
			this.root(arena),
			new,
			"tried to push this branch's root node as a child"
		);
		// And we're not pushing a child that already has a parent...
		assert!(
			arena.0[token.idx()].parent().is_none(),
			"tried to push a child that already has a parent"
		);

		// Set the child's parent.
		arena.0[token.idx()].set_parent(Some(token));

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
		let this = token_to_node!(ref, Self: token, arena);

		// We're not pushing our own root...
		assert_ne!(
			this.root(arena),
			new,
			"tried to push this branch's root node as a child"
		);
		// And we're not pushing a child that already has a parent...
		assert!(
			arena.0[token.idx()].parent().is_none(),
			"tried to push a child that already has a parent"
		);

		// Set the child's parent.
		arena.0[token.idx()].set_parent(Some(token));

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
	type Base = TypedNode<BranchData, LeafData>;
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
			TypedNode::Leaf(Self {
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
