// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

mod deque;

#[cfg(feature = "deque")]
use std::collections::VecDeque;

pub struct UntypedNode<T> {
	idx: usize,

	parent: Option<usize>,
	children: Vec<usize>,

	data: T,
}

#[cfg(feature = "deque")]
pub struct UntypedNodeDeque<T> {
	idx: usize,

	parent: Option<usize>,
	children: VecDeque<usize>,

	data: T,
}
