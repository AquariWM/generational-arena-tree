// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::{collections::vec_deque, fmt::Debug, iter::Copied, ops::Index};

use super::*;
use crate::{remove_children_deque, BranchNodeDeque};

type BranchTokenDeque<BranchData, LeafData> = Token<BranchDeque<BranchData, LeafData>>;
type LeafTokenDeque<BranchData, LeafData> = Token<LeafDeque<BranchData, LeafData>>;

#[derive(Debug)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum SplitNodeDeque<BranchData: Debug, LeafData: Debug> {
	Branch(BranchDeque<BranchData, LeafData>),
	Leaf(LeafDeque<BranchData, LeafData>),
}

#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum SplitTokenDeque<BranchData: Debug, LeafData: Debug> {
	Branch(BranchTokenDeque<BranchData, LeafData>),
	Leaf(LeafTokenDeque<BranchData, LeafData>),
}

#[derive(Debug)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct BranchDeque<BranchData: Debug, LeafData: Debug> {
	token: Token<Self>,

	parent: Option<BranchTokenDeque<BranchData, LeafData>>,
	children: VecDeque<SplitTokenDeque<BranchData, LeafData>>,

	data: BranchData,
}

#[derive(Debug)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct LeafDeque<BranchData: Debug, LeafData: Debug> {
	token: Token<Self>,

	parent: Option<BranchTokenDeque<BranchData, LeafData>>,

	data: LeafData,
}

impl<BranchData, LeafData> Debug for SplitTokenDeque<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	#[inline(always)]
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Branch(branch) => write!(f, "BranchDequeToken({:?})", branch.idx()),
			Self::Leaf(leaf) => write!(f, "LeafDequeToken({:?})", leaf.idx()),
		}
	}
}

impl<BranchData, LeafData, I: Idx> PartialEq<I> for SplitTokenDeque<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	#[inline(always)]
	fn eq(&self, other: &I) -> bool {
		self.idx() == other.idx()
	}
}

impl<BranchData, LeafData> Eq for SplitTokenDeque<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
}

impl<BranchData, LeafData> Hash for SplitTokenDeque<BranchData, LeafData>
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

impl<BranchData, LeafData> Clone for SplitTokenDeque<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	#[inline(always)]
	fn clone(&self) -> Self {
		*self
	}
}

impl<BranchData, LeafData> Copy for SplitTokenDeque<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
}

impl<BranchData, LeafData> SplitTokenDeque<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	/// Creates a new `SplitTokenDeque` for a [branch] from the given `token`.
	///
	/// [branch]: Branch
	#[inline(always)]
	pub const fn new_branch(token: BranchTokenDeque<BranchData, LeafData>) -> Self {
		Self::Branch(token)
	}

	/// Creates a new `SplitTokenDeque` for a [leaf] from the given `token`.
	///
	/// [leaf]: Leaf
	#[inline(always)]
	pub const fn new_leaf(token: LeafTokenDeque<BranchData, LeafData>) -> Self {
		Self::Leaf(token)
	}
}

impl<BranchData, LeafData> Idx for SplitTokenDeque<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	#[inline(always)]
	fn idx(&self) -> generational_arena::Index {
		match self {
			Self::Branch(branch) => branch.idx(),
			Self::Leaf(leaf) => leaf.idx(),
		}
	}
}

impl<BranchData, LeafData> NodeToken<SplitNodeDeque<BranchData, LeafData>> for SplitTokenDeque<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
}

impl<BranchData, LeafData, Other: Node> PartialEq<Other> for BranchDeque<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
	<Self as Node>::Token: PartialEq<Other::Token>,
{
	fn eq(&self, other: &Other) -> bool {
		self.token == other.token()
	}
}

impl<BranchData, LeafData> Eq for BranchDeque<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
}

impl<BranchData, LeafData> Hash for BranchDeque<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	#[inline(always)]
	fn hash<H: Hasher>(&self, state: &mut H) {
		self.token.hash(state);
	}
}

impl<BranchData, LeafData, Other: Node> PartialEq<Other> for LeafDeque<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
	<Self as Node>::Token: PartialEq<Other::Token>,
{
	fn eq(&self, other: &Other) -> bool {
		self.token == other.token()
	}
}

impl<BranchData, LeafData> Eq for LeafDeque<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
}

impl<BranchData, LeafData> Hash for LeafDeque<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	#[inline(always)]
	fn hash<H: Hasher>(&self, state: &mut H) {
		self.token.hash(state);
	}
}

impl<'node, BranchData, LeafData> TryFrom<&'node SplitNodeDeque<BranchData, LeafData>>
	for &'node BranchDeque<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	type Error = &'node SplitNodeDeque<BranchData, LeafData>;

	#[inline(always)]
	fn try_from(node: &'node SplitNodeDeque<BranchData, LeafData>) -> Result<Self, Self::Error> {
		match node {
			SplitNodeDeque::Branch(branch) => Ok(branch),
			SplitNodeDeque::Leaf(_) => Err(node),
		}
	}
}

impl<'node, BranchData, LeafData> TryFrom<&'node mut SplitNodeDeque<BranchData, LeafData>>
	for &'node mut BranchDeque<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	type Error = &'node mut SplitNodeDeque<BranchData, LeafData>;

	#[inline(always)]
	fn try_from(node: &'node mut SplitNodeDeque<BranchData, LeafData>) -> Result<Self, Self::Error> {
		match node {
			SplitNodeDeque::Branch(branch) => Ok(branch),
			SplitNodeDeque::Leaf(_) => Err(node),
		}
	}
}

impl<'node, BranchData, LeafData> TryFrom<&'node SplitNodeDeque<BranchData, LeafData>>
	for &'node LeafDeque<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	type Error = &'node SplitNodeDeque<BranchData, LeafData>;

	#[inline(always)]
	fn try_from(node: &'node SplitNodeDeque<BranchData, LeafData>) -> Result<Self, Self::Error> {
		match node {
			SplitNodeDeque::Branch(_) => Err(node),
			SplitNodeDeque::Leaf(leaf) => Ok(leaf),
		}
	}
}

impl<'node, BranchData, LeafData> TryFrom<&'node mut SplitNodeDeque<BranchData, LeafData>>
	for &'node mut LeafDeque<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	type Error = &'node mut SplitNodeDeque<BranchData, LeafData>;

	#[inline(always)]
	fn try_from(node: &'node mut SplitNodeDeque<BranchData, LeafData>) -> Result<Self, Self::Error> {
		match node {
			SplitNodeDeque::Branch(_) => Err(node),
			SplitNodeDeque::Leaf(leaf) => Ok(leaf),
		}
	}
}

impl<BranchData, LeafData> SplitNodeDeque<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	#[inline(always)]
	fn set_parent(&mut self, parent: Option<BranchTokenDeque<BranchData, LeafData>>) {
		match self {
			Self::Branch(branch) => branch.parent = parent,
			Self::Leaf(leaf) => leaf.parent = parent,
		}
	}
}

impl<BranchData, LeafData> Sealed for SplitNodeDeque<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
}

impl<BranchData, LeafData> Node for SplitNodeDeque<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	type Base = Self;
	type Token = SplitTokenDeque<BranchData, LeafData>;

	type Data = SplitData<BranchData, LeafData>;
	type DataRef<'data> = SplitData<&'data BranchData, &'data LeafData>
	where
		Self: 'data;
	type DataRefMut<'data> = SplitData<&'data mut BranchData, &'data mut LeafData>
	where
		Self: 'data;

	#[inline(always)]
	fn new(arena: &mut Arena<Self>, data: Self::Data) -> Self::Token {
		match data {
			SplitData::Branch(data) => SplitTokenDeque::Branch(BranchDeque::new(arena, data)),
			SplitData::Leaf(data) => SplitTokenDeque::Leaf(LeafDeque::new(arena, data)),
		}
	}

	#[inline(always)]
	fn token(&self) -> Self::Token {
		match self {
			Self::Branch(branch) => SplitTokenDeque::Branch(branch.token()),
			Self::Leaf(leaf) => SplitTokenDeque::Leaf(leaf.token()),
		}
	}

	#[inline(always)]
	fn parent(&self) -> Option<<<Self::Base as BaseNode>::Branch as Node>::Token> {
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

impl<BranchData, LeafData> BaseNode for SplitNodeDeque<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	type Representation = SplitNodeRepresentation<BranchData, LeafData>;

	type Branch = BranchDeque<BranchData, LeafData>;
	type Leaf = LeafDeque<BranchData, LeafData>;

	fn into_representation(self, arena: &mut Arena<Self>) -> Self::Representation
	where
		Self: Sized,
	{
		match self {
			Self::Branch(branch) => SplitNodeRepresentation::Branch {
				children: remove_children_deque(&branch, arena),
				data: branch.data,
			},

			Self::Leaf(leaf) => SplitNodeRepresentation::Leaf { data: leaf.data },
		}
	}
}

impl<BranchData, LeafData> Sealed for BranchDeque<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
}

impl<BranchData, LeafData> Node for BranchDeque<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	type Base = SplitNodeDeque<BranchData, LeafData>;
	type Token = Token<Self>;

	type Data = BranchData;
	type DataRef<'data> = &'data BranchData
	where
		Self: 'data;
	type DataRefMut<'data> = &'data mut BranchData
	where
		Self: 'data;

	fn new(arena: &mut Arena<Self::Base>, data: Self::Data) -> Token<Self> {
		Token::new(arena.0.insert_with(|idx| {
			SplitNodeDeque::Branch(Self {
				token: Token::new(idx),

				parent: None,
				children: VecDeque::new(),

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
	fn data(&self) -> &BranchData {
		&self.data
	}

	#[inline(always)]
	fn data_mut(&mut self) -> &mut BranchData {
		&mut self.data
	}
}

impl<BranchData, LeafData> BranchNode for BranchDeque<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	type ChildrenIter<'branch>
	= Copied<vec_deque::Iter<'branch, <Self::Base as Node>::Token>>
	where
		Self: 'branch;

	#[inline]
	fn first(&self) -> Option<<Self::Base as Node>::Token> {
		match self.children.len() {
			0 => None,
			_ => Some(self.children[0]),
		}
	}

	#[inline]
	fn last(&self) -> Option<<Self::Base as Node>::Token> {
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

	fn detach_front(token: Self::Token, arena: &mut Arena<Self::Base>) -> Option<<Self::Base as Node>::Token> {
		let child = token_to_node!(ref mut, Self: token, arena).children.pop_front();

		if let Some(child) = &child {
			let node = &mut arena.0[child.idx()];

			// Detach the node's parent.
			node.set_parent(None);
		}

		child
	}

	fn detach_back(token: Self::Token, arena: &mut Arena<Self::Base>) -> Option<<Self::Base as Node>::Token> {
		let child = token_to_node!(ref mut, Self: token, arena).children.pop_back();

		if let Some(child) = &child {
			let node = &mut arena.0[child.idx()];

			// Detach the node's parent.
			node.set_parent(None);
		}

		child
	}

	fn pop_front(
		token: Self::Token,
		arena: &mut Arena<Self::Base>,
	) -> Option<SplitNodeRepresentation<BranchData, LeafData>> {
		let child = token_to_node!(ref mut, Self: token, arena).children.pop_front();

		child.map(|child| {
			arena
				.0
				.remove(child.idx())
				.expect("tried to remove child but there was no such node in the `arena`")
				.into_representation(arena)
		})
	}

	fn pop_back(
		token: Self::Token,
		arena: &mut Arena<Self::Base>,
	) -> Option<SplitNodeRepresentation<BranchData, LeafData>> {
		let child = token_to_node!(ref mut, Self: token, arena).children.pop_back();

		child.map(|child| {
			arena
				.0
				.remove(child.idx())
				.expect("tried to remove child but there was no such node in the `arena`")
				.into_representation(arena)
		})
	}

	fn push_front(token: Self::Token, arena: &mut Arena<Self::Base>, new: <Self::Base as Node>::Token) {
		// We're not pushing our own root...
		assert_ne!(
			arena.0[token.idx()].root(arena),
			new,
			"tried to push this branch's own root as a child"
		);
		// And we're not pushing a child that already has a parent...
		assert!(
			arena.0[new.idx()].parent().is_none(),
			"tried to push a child that already has a parent"
		);

		// Set the child's parent.
		arena.0[new.idx()].set_parent(Some(token));

		// Push the child's token.
		token_to_node!(ref mut, Self: token, arena).children.push_front(new);
	}

	fn push_back(token: Self::Token, arena: &mut Arena<Self::Base>, new: <Self::Base as Node>::Token) {
		// We're not pushing our own root...
		assert_ne!(
			arena.0[token.idx()].root(arena),
			new,
			"tried to push this branch's own root as a child"
		);
		// And we're not pushing a child that already has a parent...
		assert!(
			arena.0[new.idx()].parent().is_none(),
			"tried to push a child that already has a parent"
		);

		// Set the child's parent.
		arena.0[new.idx()].set_parent(Some(token));

		// Push the child's token.
		token_to_node!(ref mut, Self: token, arena).children.push_back(new);
	}
}

impl<BranchData, LeafData> Index<usize> for BranchDeque<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	type Output = <<Self as Node>::Base as Node>::Token;

	#[inline(always)]
	fn index(&self, index: usize) -> &Self::Output {
		&self.children[index]
	}
}

impl<BranchData, LeafData> BranchNodeDeque for BranchDeque<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	#[inline(always)]
	fn len(&self) -> usize {
		self.children.len()
	}

	fn detach(token: Self::Token, arena: &mut Arena<Self::Base>, index: usize) -> <Self::Base as Node>::Token {
		let children = &mut token_to_node!(ref mut, Self: token, arena).children;

		let child = children.remove(index).unwrap_or_else(|| {
			panic!(
				"the given `index` ({index}) was out of bounds; there were only {} children",
				children.len()
			)
		});

		// Detach the child's parent.
		arena.0[child.idx()].set_parent(None);

		child
	}

	fn remove(
		token: Self::Token,
		arena: &mut Arena<Self::Base>,
		index: usize,
	) -> SplitNodeRepresentation<BranchData, LeafData> {
		let children = &mut token_to_node!(ref mut, Self: token, arena).children;

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

	fn insert(token: Self::Token, arena: &mut Arena<Self::Base>, index: usize, new: <Self::Base as Node>::Token) {
		// We're not inserting our own root...
		assert_ne!(
			arena.0[token.idx()].root(arena),
			new,
			"tried to insert this branch's own root as a child"
		);
		// And we're not inserting a child that already has a parent...
		assert!(
			arena.0[new.idx()].parent().is_none(),
			"tried to insert a child that already has a parent"
		);

		// Set the child's parent.
		arena.0[new.idx()].set_parent(Some(token));

		// Insert the child's token.
		token_to_node!(ref mut, Self: token, arena).children.insert(index, new);
	}
}

impl<BranchData, LeafData> Sealed for LeafDeque<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
}

impl<BranchData, LeafData> Node for LeafDeque<BranchData, LeafData>
where
	BranchData: Debug,
	LeafData: Debug,
{
	type Base = SplitNodeDeque<BranchData, LeafData>;
	type Token = Token<Self>;

	type Data = LeafData;
	type DataRef<'data> = &'data LeafData
	where
		Self: 'data;
	type DataRefMut<'data> = &'data mut LeafData
	where
		Self: 'data;

	fn new(arena: &mut Arena<Self::Base>, data: Self::Data) -> Self::Token {
		Token::new(arena.0.insert_with(|idx| {
			SplitNodeDeque::Leaf(Self {
				token: Token::new(idx),

				parent: None,

				data,
			})
		}))
	}

	#[inline(always)]
	fn token(&self) -> Self::Token {
		self.token
	}

	#[inline(always)]
	fn parent(&self) -> Option<Token<<Self::Base as BaseNode>::Branch>> {
		self.parent
	}

	#[inline(always)]
	fn data(&self) -> &LeafData {
		&self.data
	}

	#[inline(always)]
	fn data_mut(&mut self) -> &mut LeafData {
		&mut self.data
	}
}
